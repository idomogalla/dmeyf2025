#!/usr/bin/env Rscript

# Sección 1: Carga de Librerías
#------------------------------------------------------
if (!require("logger")) install.packages("logger")
library("logger")

suppressPackageStartupMessages({
  if (!require("data.table")) install.packages("data.table")
  library("data.table")
  if (!require("parallel")) install.packages("parallel")
  library("parallel")
  if (!require("R.utils")) install.packages("R.utils")
  library("R.utils")
  if (!require("primes")) install.packages("primes")
  library("primes")
  if (!require("utils")) install.packages("utils")
  library("utils")
  if (!require("rlist")) install.packages("rlist")
  library("rlist")
  if (!require("yaml")) install.packages("yaml")
  library("yaml")
  if (!require("lightgbm")) install.packages("lightgbm")
  library("lightgbm")
  if (!require("DiceKriging")) install.packages("DiceKriging")
  library("DiceKriging")
  if (!require("mlrMBO")) install.packages("mlrMBO")
  library("mlrMBO")
  if (!require("ggplot2")) install.packages("ggplot2")
  library("ggplot2")
})

# Sección 2: Configuración Inicial y Parámetros
#------------------------------------------------------
rm(list = ls(all.names = TRUE))
gc(full = TRUE, verbose = FALSE)

PARAM <- list()
PARAM$experimento <- "expC01_Prueba02_A"
PARAM$semilla_primigenia <- 200003
PARAM$train <- c(202101, 202102)
PARAM$train_final <- c(202101, 202102)
PARAM$future <- c(202104)
PARAM$train_final_kaggle <- c(202101, 202102, 202103, 202104)
PARAM$entrega_kaggle <- c(202106)
PARAM$semilla_kaggle <- 314159
PARAM$cortes <- seq(0, 20000, by = 100)
PARAM$trainingstrategy$undersampling <- 0.5
PARAM$hyperparametertuning$xval_folds <- 5
PARAM$lgbm$param_fijos <- list(
  boosting = "gbdt",
  objective = "binary",
  metric = "auc",
  first_metric_only = FALSE,
  boost_from_average = TRUE,
  feature_pre_filter = FALSE,
  force_row_wise = TRUE,
  verbosity = -100,
  seed = PARAM$semilla_primigenia,
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
# Bordes de hiperparámetros para BO
PARAM$hypeparametertuning$hs <- makeParamSet(
    makeNumericParam("min_sum_hessian_in_leaf", lower= 0.001, upper= 0.1),
    
    makeNumericParam("lambda_l1", lower= 0.0, upper= 100.0),
    makeNumericParam("lambda_l2", lower= 0.0, upper= 100.0),
    
    
    makeIntegerParam("num_leaves", lower= 10L, upper= 2048L),
    makeIntegerParam("max_depth", lower= -1L, upper= 14),


    makeIntegerParam("num_iterations", lower= 50L, upper= 3000L),
    makeNumericParam("learning_rate", lower= 0.01, upper= 0.3),

    makeNumericParam("feature_fraction", lower= 0.1, upper= 1.0),
    makeNumericParam("bagging_fraction", lower= 0.0, upper= 1.0),
    makeIntegerParam("bagging_freq", lower= 0L, upper= 10L),
    
    makeIntegerParam("min_data_in_leaf", lower= 10L, upper= 8000L),
    makeNumericParam("min_gain_to_split", lower= 0.0, upper= 15.0)
)
PARAM$hyperparametertuning$iteraciones <- 100

# ----- Configuración del Logger con Ruta Absoluta -----
# 1. Definir la ruta absoluta del directorio del experimento
dir_experimento <- paste0("~/buckets/b1/exp/", PARAM$experimento)
# 2. Crear el directorio si no existe (con recursive = TRUE por seguridad)
dir.create(dir_experimento, showWarnings = FALSE, recursive = TRUE)
# 3. Definir la ruta absoluta del archivo de log
log_file <- file.path(dir_experimento, paste0("log_", PARAM$experimento, ".log"))
# 4. Configurar el logger para que escriba en consola y en la ruta absoluta del archivo
log_appender(appender_tee(log_file))

log_info("------------------------------------------------------")
log_info(paste("Inicio del script. Experimento:", PARAM$experimento))
log_info(paste("El log se guardará en:", log_file))
log_info("------------------------------------------------------")

# Sección 3: Funciones Auxiliares
#------------------------------------------------------
particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed, "L'Ecuyer-CMRG")
  bloque <- unlist(mapply(function(x, y) { rep(y, x) }, division, seq(from = start, length.out = length(division))))
  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N], by = agrupa]
}

realidad_inicializar <- function(pfuture, pparam) {
  drealidad <- pfuture[, list(numero_de_cliente, foto_mes, clase_ternaria)]
  particionar(drealidad, division = c(3, 7), agrupa = "clase_ternaria", seed = pparam$semilla_kaggle)
  return(drealidad)
}

realidad_evaluar <- function(prealidad, pprediccion) {
  prealidad[pprediccion, on = c("numero_de_cliente", "foto_mes"), predicted := i.Predicted]
  tbl <- prealidad[, list("qty" = .N), list(fold, predicted, clase_ternaria)]
  res <- list()
  res$public <- tbl[fold == 1 & predicted == 1L, sum(qty * ifelse(clase_ternaria == "BAJA+2", 780000, -20000))] / 0.3
  res$private <- tbl[fold == 2 & predicted == 1L, sum(qty * ifelse(clase_ternaria == "BAJA+2", 780000, -20000))] / 0.7
  res$total <- tbl[predicted == 1L, sum(qty * ifelse(clase_ternaria == "BAJA+2", 780000, -20000))]
  prealidad[, predicted := NULL]
  return(res)
}

#------------------------------------------------------
tryCatch({
  # Sección 4: Preparación de Datos
  #------------------------------------------------------
  log_info("Iniciando Sección 4: Preparación de Datos.")
  setwd("~/buckets/b1/datasets")
  log_info(paste("Cambiando directorio a:", getwd()))
  dataset <- fread("./competencia_01.csv.gz", stringsAsFactors = TRUE)
  log_info("Dataset cargado correctamente.")

  # Genero columnas Lags y Delta Lags de orden 1
  cols_a_excluir <- c("numero_de_cliente", "foto_mes", "clase_ternaria")
  cols_con_lag <- setdiff(names(dataset), cols_a_excluir)
  nombres_nuevas_cols_lag <- paste0(cols_con_lag, "_lag1")
  dataset[, (nombres_nuevas_cols_lag) := shift(.SD, 1, NA, "lag"), by = numero_de_cliente, .SDcols = cols_con_lag]
  nombres_nuevas_cols_delta <- paste0(cols_con_lag, "_delta1")
  dataset[, (nombres_nuevas_cols_delta) := .SD - mget(nombres_nuevas_cols_lag), .SDcols = cols_con_lag]
  log_info("Features de lag y delta generadas.")

  dataset_train <- dataset[foto_mes %in% PARAM$train]
  dataset_train[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]
  log_info("Clase convertida a formato binario.")

  set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")
  dataset_train[, azar := runif(nrow(dataset_train))]
  dataset_train[, training := 0L]
  dataset_train[foto_mes %in% PARAM$train & (azar <= PARAM$trainingstrategy$undersampling | clase_ternaria %in% c("BAJA+1", "BAJA+2")), training := 1L]
  log_info(paste("Undersampling aplicado con una tasa de:", PARAM$trainingstrategy$undersampling))

  campos_buenos <- setdiff(colnames(dataset_train), c("clase_ternaria", "clase01", "azar", "training"))
  dtrain <- lgb.Dataset(
    data = data.matrix(dataset_train[training == 1L, campos_buenos, with = FALSE]),
    label = dataset_train[training == 1L, clase01],
    free_raw_data = FALSE
  )
  log_info("Dataset de entrenamiento para LightGBM creado.")
  log_info(paste("Dimensiones de dtrain -> Filas:", nrow(dtrain), "| Columnas:", ncol(dtrain)))

  setwd(dir_experimento)
  log_info(paste("Cambiando directorio de trabajo a:", getwd()))

  # Sección 5: Optimización Bayesiana
  #------------------------------------------------------
  log_info("Iniciando Sección 5: Optimización Bayesiana de Hiperparámetros.")
  EstimarGanancia_AUC_lightgbm <- function(x) {
    # x pisa (o agrega) a param_fijos
    param_completo <- modifyList(PARAM$lgbm$param_fijos, x)
    # Entreno LightGBM
    modelocv <- lgb.cv(data = dtrain, nfold = PARAM$hyperparametertuning$xval_folds, stratified = TRUE, param = param_completo)
    # obtengo la ganancia
    AUC <- modelocv$best_score
    # hago espacio en la memoria
    rm(modelocv)
    gc(full = TRUE, verbose = FALSE)
    log_info(paste("Iteración BO -> AUC:", format(AUC, digits = 6), "|", format(Sys.time(), "%a %b %d %X %Y")))
    return(AUC)
  }

  dir.create("Archivos Bayesiana", showWarnings = FALSE)
  kbayesiana <- "./Archivos Bayesiana/bayesiana.RDATA"
  funcion_optimizar <- EstimarGanancia_AUC_lightgbm # la funcion que voy a maximizar
  configureMlr(show.learner.output = FALSE)
  obj.fun <- makeSingleObjectiveFunction(fn = funcion_optimizar, minimize = FALSE, noisy = TRUE, par.set = PARAM$hypeparametertuning$hs, has.simple.signature = FALSE)
  ctrl <- makeMBOControl(save.on.disk.at.time = 600, save.file.path = kbayesiana)
  ctrl <- setMBOControlTermination(ctrl, iters = PARAM$hyperparametertuning$iteraciones)
  ctrl <- setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())
  surr.km <- makeLearner("regr.km", predict.type = "se", covtype = "matern3_2", control = list(trace = FALSE))

  if (!file.exists(kbayesiana)) {
    log_info(paste("Iniciando nueva búsqueda Bayesiana de", PARAM$hyperparametertuning$iteraciones, "iteraciones."))
    bayesiana_salida <- mbo(obj.fun, learner = surr.km, control = ctrl)
  } else {
    log_info("Continuando búsqueda Bayesiana desde archivo existente.")
    bayesiana_salida <- mboContinue(kbayesiana)
  }
  log_info("Optimización Bayesiana finalizada.")
  
  tb_bayesiana <- as.data.table(bayesiana_salida$opt.path)
  tb_bayesiana[, iter := .I]
  setorder(tb_bayesiana, -y)
  fwrite(tb_bayesiana, file = "./Archivos Bayesiana/BO_log.txt", sep = "\t")
  PARAM$out$lgbm$mejores_hiperparametros <- tb_bayesiana[1, setdiff(colnames(tb_bayesiana), c("y", "dob", "eol", "error.message", "exec.time", "ei", "error.model", "train.time", "prop.type", "propose.time", "se", "mean", "iter")), with = FALSE]
  PARAM$out$lgbm$y <- tb_bayesiana[1, y]
  write_yaml(PARAM, file = "./Archivos Bayesiana/PARAM.yml")
  
  log_info("Mejores hiperparámetros encontrados:")
  log_info(paste(capture.output(print(PARAM$out$lgbm$mejores_hiperparametros)), collapse = "\n"))
  log_info(paste("Mejor AUC (y):", PARAM$out$lgbm$y))
  
  # Sección 6: Entrenamiento y Predicción (Modelo Único)
  #------------------------------------------------------
  log_info("Iniciando Sección 6: Entrenamiento del Modelo Único.")
  dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+1", "BAJA+2"), 1L, 0L)]
  dataset_train <- dataset[foto_mes %in% PARAM$train_final]
  dtrain_final <- lgb.Dataset(data = data.matrix(dataset_train[, campos_buenos, with = FALSE]), label = dataset_train[, clase01])

  param_final <- modifyList(PARAM$lgbm$param_fijos, PARAM$out$lgbm$mejores_hiperparametros)
  param_normalizado <- copy(param_final)
  param_normalizado$min_data_in_leaf <- round(param_final$min_data_in_leaf / PARAM$trainingstrategy$undersampling)
  
  modelo_final <- lgb.train(data = dtrain_final, param = param_normalizado)
  log_info("Entrenamiento del modelo final completado.")
  lgb.save(modelo_final, "./Archivos Bayesiana/modelo.txt")
  tb_importancia <- as.data.table(lgb.importance(modelo_final))
  fwrite(tb_importancia, file = "./Archivos Bayesiana/impo.txt", sep = "\t")

  log_info("Generando predicciones y envíos para Kaggle (Modelo Único)...")
  dfuture <- dataset[foto_mes %in% PARAM$future]
  prediccion <- predict(modelo_final, data.matrix(dfuture[, campos_buenos, with = FALSE]))
  # Dataset realidad
  drealidad <- realidad_inicializar(dfuture, PARAM)
  # Tabla de predicciones
  tb_prediccion <- dfuture[, list(numero_de_cliente, foto_mes)]
  tb_prediccion[, prob := prediccion]
  setorder(tb_prediccion, -prob)
  
  dir.create("kaggle", showWarnings = FALSE)
  resultados <- data.table()
  
  for (envios in PARAM$cortes) {
    tb_prediccion[, Predicted := 0L] # seteo inicial a 0
    tb_prediccion[1:envios, Predicted := 1L] # marco los primeros n envios
    fwrite(tb_prediccion[, list(numero_de_cliente, Predicted)], file = paste0("./kaggle/", PARAM$experimento, "_", envios, ".csv"), sep = ",")
    res <- realidad_evaluar(drealidad, tb_prediccion)
    resultados <- rbind(resultados, data.table(clientes = envios, ganancia_total = res$total, ganancia_public = res$public, ganancia_private = res$private))
    options(scipen = 999)
    log_info(sprintf("Envios=%-5d | TOTAL=%11.0f | Public=%11.0f | Private=%11.0f", envios, res$total, res$public, res$private))
  }

  # pasar a formato largo
  resultados_long <- melt(
    resultados,
    id.vars = "clientes",
    measure.vars = c("ganancia_total", "ganancia_public", "ganancia_private"),
    variable.name = "tipo",
    value.name = "ganancia"
  )

  # calcular máximos por tipo
  maximos <- resultados_long[, .SD[which.max(ganancia)], by = tipo]

  # etiquetas personalizadas
  etiquetas <- paste0(
    maximos$tipo,
    " (envíos = ", maximos$clientes, ", máx = ", format(maximos$ganancia, big.mark = ","), ")"
  )
  names(etiquetas) <- maximos$tipo

  # gráfico
  p <- ggplot(resultados_long, aes(x = clientes, y = ganancia, color = tipo)) +
    geom_line(linewidth = 1) +
    geom_point(data = maximos, aes(x = clientes, y = ganancia, color = tipo), size = 3) +
    labs(
      title = paste0("Curvas de Ganancia (", PARAM$experimento, ")"),
      x = "Clientes",
      y = "Ganancia",
      color = "Máximos"
    ) +
    scale_color_manual(
      values = c(
        "ganancia_total" = "steelblue",
        "ganancia_public" = "forestgreen",
        "ganancia_private" = "firebrick"
      ),
      labels = etiquetas
    ) +
    theme_minimal() +
    theme(
      plot.margin = margin(10, 10, 10, 10),
      legend.position = "bottom"
    ) +
    guides(color = guide_legend(nrow = 3, byrow = TRUE))

  # guardar imagen
  ggsave(paste0("curvas_", PARAM$experimento, ".png"), plot = p, width = 10, height = 6)

  log_info("Gráfico de curvas de ganancia (modelo único) guardado.")

  # Sección 7: Entrenamiento y Predicción (Ensemble)
  #------------------------------------------------------
  log_info("Iniciando Sección 7: Entrenamiento del Ensamble de Modelos.")
  semillas <- c(200003, 300007, 400009, 500009, 600011)
  lista_predicciones <- list()

  for (semilla_actual in semillas) {
    log_info(paste("Entrenando modelo del ensamble con semilla:", semilla_actual))
    param_normalizado$seed <- semilla_actual
    modelo_final <- lgb.train(data = dtrain_final, param = param_normalizado)
    prediccion_individual <- predict(modelo_final, data.matrix(dfuture[, campos_buenos, with = FALSE]))
    tb_prediccion_individual <- dfuture[, list(numero_de_cliente, foto_mes)]
    tb_prediccion_individual[, prob := prediccion_individual]
    lista_predicciones[[as.character(semilla_actual)]] <- tb_prediccion_individual
  }

  log_info("Creando el ensamble final promediando probabilidades.")
  predicciones_todas <- rbindlist(lista_predicciones)
  tb_prediccion <- predicciones_todas[, .(prob = mean(prob)), by = .(numero_de_cliente, foto_mes)]
  setorder(tb_prediccion, -prob)
  dir.create("kaggle_promedidado", showWarnings = FALSE)
  resultados <- data.table()

  log_info("Generando predicciones y envíos para Kaggle (Ensemble)...")
  for (envios in PARAM$cortes) {
    tb_prediccion[, Predicted := 0L][1:envios, Predicted := 1L]
    fwrite(tb_prediccion[, list(numero_de_cliente, Predicted)], file = paste0("./kaggle_promedidado/", PARAM$experimento, "_", envios, ".csv"), sep = ",")
    res <- realidad_evaluar(drealidad, tb_prediccion)
    resultados <- rbind(resultados, data.table(clientes = envios, ganancia_total = res$total, ganancia_public = res$public, ganancia_private = res$private))
    options(scipen = 999)
    log_info(sprintf("Envios=%-5d | TOTAL=%11.0f | Public=%11.0f | Private=%11.0f", envios, res$total, res$public, res$private))
  }

    # pasar a formato largo
  resultados_long <- melt(
    resultados,
    id.vars = "clientes",
    measure.vars = c("ganancia_total", "ganancia_public", "ganancia_private"),
    variable.name = "tipo",
    value.name = "ganancia"
  )

  # calcular máximos por tipo
  maximos <- resultados_long[, .SD[which.max(ganancia)], by = tipo]

  # etiquetas personalizadas
  etiquetas <- paste0(
    maximos$tipo,
    " (envíos = ", maximos$clientes, ", máx = ", format(maximos$ganancia, big.mark = ","), ")"
  )
  names(etiquetas) <- maximos$tipo

  # gráfico
  p <- ggplot(resultados_long, aes(x = clientes, y = ganancia, color = tipo)) +
    geom_line(linewidth = 1) +
    geom_point(data = maximos, aes(x = clientes, y = ganancia, color = tipo), size = 3) +
    labs(
      title = paste0("Curvas de Ganancia (", PARAM$experimento, ")"),
      x = "Clientes",
      y = "Ganancia",
      color = "Máximos"
    ) +
    scale_color_manual(
      values = c(
        "ganancia_total" = "steelblue",
        "ganancia_public" = "forestgreen",
        "ganancia_private" = "firebrick"
      ),
      labels = etiquetas
    ) +
    theme_minimal() +
    theme(
      plot.margin = margin(10, 10, 10, 10),
      legend.position = "bottom"
    ) +
    guides(color = guide_legend(nrow = 3, byrow = TRUE))

  # guardar imagen
  ggsave(paste0("curvas_ensamble_", PARAM$experimento, ".png"), plot = p, width = 10, height = 6)
  log_info("Gráfico de curvas de ganancia (ensemble) guardado.")

}, error = function(e) {
  # Mensaje de error mejorado
  log_error("######################################################")
  log_error("Se ha producido un error fatal en la ejecución.")
  log_error("Revisa el último mensaje 'INFO' en el log para identificar la sección donde ocurrió el fallo.")
  log_error(paste("Mensaje de R:", e$message))
  log_error("######################################################")
  quit(status = 1) # Detiene el script con un código de error
})

# Sección 8: Finalización
#------------------------------------------------------
log_info("------------------------------------------------------")
log_info("Script finalizado exitosamente.")
log_info(paste("Fecha y hora de finalización:", format(Sys.time(), "%a %b %d %X %Y")))
log_info("------------------------------------------------------")