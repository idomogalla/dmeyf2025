# Limpieza de memoria
rm(list = ls(all.names = TRUE))
gc(full = TRUE, verbose = FALSE)

# Instalación y carga de paquetes necesarios
packages <- c("data.table", "parallel", "R.utils", "primes", "utils", 
              "rlist", "yaml", "lightgbm", "DiceKriging", "mlrMBO", "logger")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Configuración del logger
log_file <- "analysis.log"
if (file.exists(log_file)) {
  file.remove(log_file)
}

log_appender(appender_tee(log_file))
log_info("Iniciando el script de análisis y modelado.")

# Parámetros de configuración
PARAM <- list(
  experimento = "794_002",
  semilla_primigenia = 102191,
  train = c(202101, 202102, 202103, 202104),
  train_final = c(202101, 202102, 202103, 202104),
  future = c(202106),
  semilla_kaggle = 314159,
  cortes = seq(9000, 13000, by = 500),
  trainingstrategy = list(undersampling = 0.5),
  hyperparametertuning = list(
    xval_folds = 5,
    iteraciones = 30
  ),
  lgbm = list(
    param_fijos = list(
      boosting = "gbdt",
      objective = "binary",
      metric = "auc",
      first_metric_only = FALSE,
      boost_from_average = TRUE,
      feature_pre_filter = FALSE,
      force_row_wise = TRUE,
      verbosity = -100,
      seed = 102191, # Usar PARAM$semilla_primigenia directamente
      max_depth = -1L,
      min_gain_to_split = 0,
      min_sum_hessian_in_leaf = 0.001,
      lambda_l1 = 0.0,
      lambda_l2 = 0.0,
      max_bin = 31L,
      bagging_fraction = 1.0,
      pos_bagging_fraction = 1.0,
      neg_bagging_fraction = 1.0,
      is_unbalance = FALSE,
      scale_pos_weight = 1.0,
      drop_rate = 0.1,
      max_drop = 50,
      skip_drop = 0.5,
      extra_trees = FALSE,
      num_iterations = 1200,
      learning_rate = 0.02,
      feature_fraction = 0.5,
      num_leaves = 750,
      min_data_in_leaf = 5000
    )
  )
)

PARAM$hyperparametertuning$hs <- makeParamSet(
  makeIntegerParam("num_iterations", lower = 8L, upper = 2048L),
  makeNumericParam("learning_rate", lower = 0.01, upper = 0.3),
  makeNumericParam("feature_fraction", lower = 0.1, upper = 1.0),
  makeIntegerParam("num_leaves", lower = 8L, upper = 2048L),
  makeIntegerParam("min_data_in_leaf", lower = 1L, upper = 8000L)
)

# Funciones auxiliares
particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed, "L'Ecuyer-CMRG")
  
  bloque <- unlist(mapply(
    function(x, y) { rep(y, x) }, division, seq(from = start, length.out = length(division))
  ))
  
  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N], by = agrupa]
  log_info(paste("Partición creada en la columna:", campo))
}

# Preparación de datos
preparar_datos <- function(path) {
  log_info("Cargando datos...")
  dataset <- fread(path, stringsAsFactors = TRUE)
  
  log_info("Creando variables LAG y DELTA...")
  setorder(dataset, numero_de_cliente, foto_mes)
  
  cols_lagueables <- setdiff(colnames(dataset), c("numero_de_cliente", "foto_mes", "clase_ternaria"))
  
  dataset[, paste0(cols_lagueables, "_lag1") := shift(.SD, 1, NA, "lag"), by = numero_de_cliente, .SDcols = cols_lagueables]
  dataset[, paste0(cols_lagueables, "_lag2") := shift(.SD, 2, NA, "lag"), by = numero_de_cliente, .SDcols = cols_lagueables]
  
  for (vcol in cols_lagueables) {
    dataset[, paste0(vcol, "_delta1") := get(vcol) - get(paste0(vcol, "_lag1"))]
    dataset[, paste0(vcol, "_delta2") := get(vcol) - get(paste0(vcol, "_lag2"))]
  }
  
  log_info("Datos preparados.")
  return(dataset)
}

# Optimización Bayesiana
optimizar_hiperparametros <- function(dataset_train, campos_buenos) {
  log_info("Iniciando optimización bayesiana de hiperparámetros...")
  
  dtrain <- lgb.Dataset(
    data = data.matrix(dataset_train[training == 1L, campos_buenos, with = FALSE]),
    label = dataset_train[training == 1L, clase01],
    free_raw_data = FALSE
  )
  
  estimar_ganancia_auc <- function(x) {
    param_completo <- modifyList(PARAM$lgbm$param_fijos, x)
    
    modelocv <- lgb.cv(
      data = dtrain,
      nfold = PARAM$hyperparametertuning$xval_folds,
      stratified = TRUE,
      param = param_completo
    )
    
    auc <- modelocv$best_score
    log_info(paste("AUC en CV:", auc))
    
    rm(modelocv)
    gc(full = TRUE, verbose = FALSE)
    
    return(auc)
  }
  
  kbayesiana <- "bayesiana.RDATA"
  obj.fun <- makeSingleObjectiveFunction(
    fn = estimar_ganancia_auc,
    minimize = FALSE,
    noisy = TRUE,
    par.set = PARAM$hyperparametertuning$hs,
    has.simple.signature = FALSE
  )
  
  ctrl <- makeMBOControl(save.on.disk.at.time = 600, save.file.path = kbayesiana)
  ctrl <- setMBOControlTermination(ctrl, iters = PARAM$hyperparametertuning$iteraciones)
  ctrl <- setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())
  
  surr.km <- makeLearner("regr.km", predict.type = "se", covtype = "matern3_2", control = list(trace = TRUE))
  
  if (!file.exists(kbayesiana)) {
    bayesiana_salida <- mbo(obj.fun, learner = surr.km, control = ctrl)
  } else {
    bayesiana_salida <- mboContinue(kbayesiana)
  }
  
  log_info("Optimización bayesiana completada.")
  return(bayesiana_salida)
}

# Flujo principal
main <- function() {
  setwd("~/buckets/b1/exp/")
  experimento_folder <- paste0("HT", PARAM$experimento)
  dir.create(experimento_folder, showWarnings = FALSE)
  setwd(paste0("~/buckets/b1/exp/", experimento_folder))
  
  dataset <- preparar_datos("~/buckets/b1/datasets/competencia_01.csv.gz")
  
  dataset_train <- dataset[foto_mes %in% PARAM$train]
  dataset_train[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]
  
  set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")
  dataset_train[, azar := runif(nrow(dataset_train))]
  dataset_train[, training := 0L]
  dataset_train[
    foto_mes %in% PARAM$train & (azar <= PARAM$trainingstrategy$undersampling | clase_ternaria %in% c("BAJA+1", "BAJA+2")),
    training := 1L
  ]
  
  campos_buenos <- setdiff(colnames(dataset_train), c("clase_ternaria", "clase01", "azar", "training"))
  
  bayesiana_salida <- optimizar_hiperparametros(dataset_train, campos_buenos)
  
  tb_bayesiana <- as.data.table(bayesiana_salida$opt.path)
  tb_bayesiana[, iter := .I]
  setorder(tb_bayesiana, -y)
  
  fwrite(tb_bayesiana, file = "BO_log.txt", sep = "\t")
  
  mejores_hiperparametros <- tb_bayesiana[1, setdiff(colnames(tb_bayesiana), 
    c("y", "dob", "eol", "error.message", "exec.time", "ei", "error.model", 
      "train.time", "prop.type", "propose.time", "se", "mean", "iter")), with = FALSE]
  
  PARAM$out <- list(lgbm = list(mejores_hiperparametros = mejores_hiperparametros, y = tb_bayesiana[1, y]))
  write_yaml(PARAM, file = "PARAM.yml")
  
  dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+1", "BAJA+2"), 1L, 0L)]
  dataset_train_final <- dataset[foto_mes %in% PARAM$train_final]
  
  dtrain_final <- lgb.Dataset(
    data = data.matrix(dataset_train_final[, campos_buenos, with = FALSE]),
    label = dataset_train_final[, clase01]
  )
  
  param_final <- modifyList(PARAM$lgbm$param_fijos, mejores_hiperparametros)
  param_normalizado <- copy(param_final)
  param_normalizado$min_data_in_leaf <- round(param_final$min_data_in_leaf / PARAM$trainingstrategy$undersampling)
  
  log_info("Entrenando modelo final...")
  modelo_final <- lgb.train(data = dtrain_final, param = param_normalizado)
  log_info("Modelo final entrenado.")
  
  tb_importancia <- as.data.table(lgb.importance(modelo_final))
  fwrite(tb_importancia, file = "impo.txt", sep = "\t")
  
  log_info("Generando predicciones...")
  dfuture <- dataset[foto_mes %in% PARAM$future]
  prediccion <- predict(modelo_final, data.matrix(dfuture[, campos_buenos, with = FALSE]))
  
  tb_prediccion <- dfuture[, list(numero_de_cliente, foto_mes)]
  tb_prediccion[, prob := prediccion]
  fwrite(tb_prediccion, file = "prediccion.txt", sep = "\t")
  
  setorder(tb_prediccion, -prob)
  dir.create("kaggle")
  
  for (envios in PARAM$cortes) {
    tb_prediccion[, Predicted := 0L]
    tb_prediccion[1:envios, Predicted := 1L]
    
    archivo_kaggle <- paste0("./kaggle/KA", PARAM$experimento, "_", envios, ".csv")
    fwrite(tb_prediccion[, list(numero_de_cliente, Predicted)], file = archivo_kaggle, sep = ",")
    log_info(paste("Archivo de Kaggle generado:", archivo_kaggle))
  }
  
  write_yaml(PARAM, file = "PARAM.yml")
  log_info("Script finalizado.")
}

# Ejecutar el flujo principal
main()
