#!/usr/bin/env Rscript
#------------------------------------------------------
# Sección 1: Carga de Librerías
#------------------------------------------------------
suppressPackageStartupMessages({
  if (!require("logger"))
    install.packages("logger")
  library("logger")
  if (!require("data.table"))
    install.packages("data.table")
  library("data.table")
  if (!require("parallel"))
    install.packages("parallel")
  library("parallel")
  if (!require("R.utils"))
    install.packages("R.utils")
  library("R.utils")
  if (!require("primes"))
    install.packages("primes")
  library("primes")
  if (!require("utils"))
    install.packages("utils")
  library("utils")
  if (!require("rlist"))
    install.packages("rlist")
  library("rlist")
  if (!require("yaml"))
    install.packages("yaml")
  library("yaml")
  if (!require("lightgbm"))
    install.packages("lightgbm")
  library("lightgbm")
  if (!require("DiceKriging"))
    install.packages("DiceKriging")
  library("DiceKriging")
  if (!require("mlrMBO"))
    install.packages("mlrMBO")
  library("mlrMBO")
  if (!require("ggplot2"))
    install.packages("ggplot2")
  library("ggplot2")
  if (!require("ggrepel"))
    install.packages("ggrepel")
  library("ggrepel")
  if (!require("scales"))
    install.packages("scales")
  library("scales")
})
#------------------------------------------------------
# Sección 2: Configuración Inicial y Parámetros
#------------------------------------------------------
rm(list = ls(all.names = TRUE))
gc(full = TRUE, verbose = FALSE)

PARAM <- list()
PARAM$experimento <- "expC01_Prueba13"
PARAM$dir_experimento <- paste0("~/buckets/b1/exp/", PARAM$experimento)
PARAM$dir_dataset <- "~/buckets/b1/datasets/"
PARAM$carpeta_logs <- "logs/"
PARAM$carpeta_bayesiana <- "Archivos Bayesiana/"
PARAM$carpeta_kaggle <- "Kaggle/"
PARAM$carpeta_kaggle_ensamble <- "Kaggle_Promediado/"
PARAM$carpeta_graficos <- "Plots/"
PARAM$carpeta_entregables <- "Entregables/"
PARAM$archivo_summary_log <- "summary.txt"


PARAM$semilla_primigenia <- 200003
PARAM$semillas_ensemble <- c(200003, 300007, 400009, 500009, 600011, 314159, 102191, 111109, 230101, 100129)
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
  boosting= "gbdt", # puede ir  dart  , ni pruebe random_forest
  objective= "binary",
  metric= "auc",
  first_metric_only= FALSE,
  boost_from_average= TRUE,
  feature_pre_filter= FALSE,
  force_row_wise= TRUE, # para reducir warnings
  verbosity= -100,

  seed= PARAM$semilla_primigenia,

  max_depth= -1L, # -1 significa no limitar,  por ahora lo dejo fijo
  min_gain_to_split= 0, # min_gain_to_split >= 0
  min_sum_hessian_in_leaf= 0.001, #  min_sum_hessian_in_leaf >= 0.0
  lambda_l1= 0.0, # lambda_l1 >= 0.0
  lambda_l2= 0.0, # lambda_l2 >= 0.0
  max_bin= 31L, # lo debo dejar fijo, no participa de la BO

  bagging_fraction= 1.0, # 0.0 < bagging_fraction <= 1.0
  pos_bagging_fraction= 1.0, # 0.0 < pos_bagging_fraction <= 1.0
  neg_bagging_fraction= 1.0, # 0.0 < neg_bagging_fraction <= 1.0
  is_unbalance= FALSE, #
  scale_pos_weight= 1.0, # scale_pos_weight > 0.0

  drop_rate= 0.1, # 0.0 < neg_bagging_fraction <= 1.0
  max_drop= 50, # <=0 means no limit
  skip_drop= 0.5, # 0.0 <= skip_drop <= 1.0

  extra_trees= FALSE,

  num_iterations= 1200,
  learning_rate= 0.02,
  feature_fraction= 0.5,
  num_leaves= 750,
  min_data_in_leaf= 5000,
  early_stopping_round = 100
)
# Bordes de hiperparámetros para BO
PARAM$hyperparametertuning$hs <- makeParamSet(
  makeIntegerParam("num_leaves", lower = 10L, upper = 2048L),
  
  makeIntegerParam("num_iterations", lower = 50L, upper = 3000L),
  makeNumericParam("learning_rate", lower = 0.01, upper = 0.5),
  
  makeNumericParam("feature_fraction", lower = 0.1, upper = 1.0),
  
  makeIntegerParam("min_data_in_leaf", lower = 1L, upper = 10000L)

)
PARAM$hyperparametertuning$iteraciones <- 100

# ----- Configuración del Logger -----
# Creo la carpeta del experimento
dir.create(PARAM$dir_experimento,
           showWarnings = FALSE,
           recursive = TRUE)
setwd(PARAM$dir_experimento)
# Creo la carpeta para los logs
dir.create(PARAM$carpeta_logs,
           showWarnings = FALSE,
           recursive = TRUE)
# Definir la ruta del archivo log
log_file <- file.path(PARAM$carpeta_logs, paste0("log_", PARAM$experimento, ".log"))
# Configurar el logger para que escriba en consola y en la ruta absoluta del archivo
log_appender(appender_tee(log_file))

# Limpio el archivo de resumen al inicio de la ejecución
summary_log_file_path <- file.path(PARAM$carpeta_logs, PARAM$archivo_summary_log)
cat(paste0("Resumen del Experimento: ", PARAM$experimento, "\n"),
    file = summary_log_file_path,
    append = FALSE)
cat(paste0("Fecha: ", Sys.time(), "\n\n"),
    file = summary_log_file_path,
    append = TRUE)


log_info("------------------------------------------------------")
log_info(paste("Inicio del script. Experimento:", PARAM$experimento))
log_info(paste("El log se guardará en:", log_file))
log_info(paste("El resumen se guardará en:", summary_log_file_path))
log_info("------------------------------------------------------")

#------------------------------------------------------
# Sección 3: Funciones Auxiliares
#------------------------------------------------------
log_info("Iniciando Sección 3: Cargando funciones auxiliares")
source("funciones_auxiliares.R")
log_info("Funciones auxiliares cargadas desde 'funciones_auxiliares.R'")

tryCatch({
  #------------------------------------------------------
  # Sección 4: Preparación de Datos
  #------------------------------------------------------
  log_info("Iniciando Sección 4: Preparación de Datos.")
  log_info(paste("Leyendo dataset desde:", PARAM$dir_dataset))
  dataset <- fread(file.path(PARAM$dir_dataset, "competencia_01.csv.gz"),
                   stringsAsFactors = TRUE)
  log_info("Dataset cargado correctamente.")
  
  log_info("Inicio de Feature Engineering")
  setkey(dataset, numero_de_cliente, foto_mes)
  
  # Columnas a las que se les aplicará el ranking
  cols_a_rankear <- c(
    "mcomisiones_mantenimiento", "Master_Fvencimiento", "Visa_fultimo_cierre", "Master_fultimo_cierre", "mpayroll", "cpayroll_trx"
  )
  
  nuevas_cols_rank <- paste0(cols_a_rankear, "_rank")
  
  dataset[, (nuevas_cols_rank) := lapply(.SD, rank_con_cero_fijo), by = foto_mes, .SDcols = cols_a_rankear]
  dataset[, (cols_a_rankear) := NULL]
  
  log_info("Inicio de Feature Lags")
  cols_a_excluir <- c("numero_de_cliente", "foto_mes", "clase_ternaria")
  cols_con_lag <- setdiff(names(dataset), cols_a_excluir)

  nombres_nuevas_cols_lag <- paste0(cols_con_lag, "_lag1")
  dataset[, (nombres_nuevas_cols_lag) := shift(.SD, 1, NA, "lag"), by = numero_de_cliente, .SDcols = cols_con_lag]

  nombres_nuevas_cols_delta <- paste0(cols_con_lag, "_delta1")
  dataset[, (nombres_nuevas_cols_delta) :=  Map(function(col, col_lag) get(col) - get(col_lag), cols_con_lag, nombres_nuevas_cols_lag)]

  nombres_nuevas_cols_delta_pct <- paste0(cols_con_lag, "_delta_pct1")
  dataset[, (nombres_nuevas_cols_delta_pct) := Map(
    function(col, col_lag) {
      lag_val <- get(col_lag)
      curr_val <- get(col)
      delta_pct <- ifelse(is.na(lag_val) | lag_val == 0, NA, (curr_val - lag_val) / abs(lag_val))
      return(delta_pct)
    },
    cols_con_lag, nombres_nuevas_cols_lag
  )]

  log_info("Features de lag y delta generadas.")
  dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]
  
  log_info("Generando dataset de entrenamiento.")
  dataset_train <- dataset[foto_mes %in% PARAM$train]
  log_info("Clase convertida a formato binario.")
  
  set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")
  dataset_train[, azar := runif(nrow(dataset_train))]
  dataset_train[, training := 0L]
  dataset_train[
    foto_mes %in%  PARAM$train &
      (azar <= PARAM$trainingstrategy$undersampling | clase_ternaria %in% c("BAJA+1", "BAJA+2")),
    training := 1L
  ]
  log_info(paste0("Undersampling aplicado con una tasa de:",PARAM$trainingstrategy$undersampling))
  
  campos_buenos <- setdiff(colnames(dataset_train),
                           c("clase_ternaria", "clase01", "azar", "training"))
  dtrain <- lgb.Dataset(
    data = data.matrix(dataset_train[training == 1L, campos_buenos, with = FALSE]),
    label = dataset_train[training == 1L, clase01],
    free_raw_data = FALSE
  )
  log_info("Dataset de entrenamiento para LightGBM creado.")
  log_info(paste(
    "Dimensiones de dtrain -> Filas:",
    nrow(dtrain),
    "| Columnas:",
    ncol(dtrain)
  ))
  
}, error = function(e) {
  log_error("######################################################")
  log_error("Se ha producido un error fatal en la ejecución.")
  log_error("Revisa el último mensaje 'INFO' en el log para identificar la sección donde ocurrió el fallo.")
  log_error(paste("Mensaje de R:", e$message))
  log_error("######################################################")
  quit(status = 1)
})

tryCatch({
  #------------------------------------------------------
  # Sección 5: Optimización Bayesiana
  #------------------------------------------------------
  log_info("Iniciando Sección 5: Optimización Bayesiana de Hiperparámetros.")  
  
  dir.create(PARAM$carpeta_bayesiana, showWarnings = FALSE)
  kbayesiana <- paste0(PARAM$carpeta_bayesiana, "bayesiana.RDATA")
  funcion_optimizar <- EstimarGanancia_AUC_lightgbm
  configureMlr(show.learner.output = FALSE)
  obj.fun <- makeSingleObjectiveFunction(
    fn = funcion_optimizar,
    minimize = FALSE,
    noisy = TRUE,
    par.set = PARAM$hyperparametertuning$hs,
    has.simple.signature = FALSE
  )
  ctrl <- makeMBOControl(save.on.disk.at.time = 600,
                         save.file.path = kbayesiana)
  ctrl <- setMBOControlTermination(ctrl, iters = PARAM$hyperparametertuning$iteraciones)
  ctrl <- setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())
  surr.km <- makeLearner(
    "regr.km",
    predict.type = "se",
    covtype = "matern3_2",
    control = list(trace = FALSE)
  )
  
  if (!file.exists(kbayesiana)) {
    log_info(
      paste(
        "Iniciando nueva búsqueda Bayesiana de",
        PARAM$hyperparametertuning$iteraciones,
        "iteraciones."
      )
    )
    bayesiana_salida <- mbo(obj.fun, learner = surr.km, control = ctrl)
  } else {
    log_info("Continuando búsqueda Bayesiana desde archivo existente.")
    bayesiana_salida <- mboContinue(kbayesiana)
  }
  log_info("Optimización Bayesiana finalizada.")
  
  tb_bayesiana <- as.data.table(bayesiana_salida$opt.path)
  tb_bayesiana[, iter := .I]
  setorder(tb_bayesiana, -y)
  fwrite(
    tb_bayesiana,
    file = paste0(PARAM$carpeta_bayesiana, "BO_log.txt"),
    sep = "\t"
  )
  PARAM$out$lgbm$mejores_hiperparametros <- tb_bayesiana[1, setdiff(
    colnames(tb_bayesiana),
    c(
      "y", "dob", "eol", "error.message", "exec.time",
      "ei", "error.model", "train.time", "prop.type",
      "propose.time", "se", "mean", "iter"
    )
  ), with = FALSE]
  PARAM$out$lgbm$y <- tb_bayesiana[1, y]
  write_yaml(PARAM, file = paste0(PARAM$carpeta_bayesiana, "PARAM.yml"))
  
  log_info("Mejores hiperparámetros encontrados:")
  log_info(paste(capture.output(
    print(PARAM$out$lgbm$mejores_hiperparametros)
  ), collapse = "\n"))
  log_info(paste("Mejor AUC (y):", PARAM$out$lgbm$y))
  
  # Escribir resultados de la BO en el log de resumen
  log_summary("--- Resultados de la Optimización Bayesiana ---")
  log_summary("Hiperparámetros Óptimos:")
  hiperparametros_texto <- capture.output(print(PARAM$out$lgbm$mejores_hiperparametros))
  sapply(hiperparametros_texto, log_summary)
  log_summary(paste("\nAUC:", format(PARAM$out$lgbm$y, digits = 6)))
  log_summary("---------------------------------------------\n")

}, error = function(e) {
  log_error("######################################################")
  log_error("Se ha producido un error fatal en la Sección 5: Optimización Bayesiana.")
  log_error(paste("Mensaje de R:", e$message))
  log_error("######################################################")
  quit(status = 1)
})

tryCatch({
  #------------------------------------------------------
  # Sección 6: Entrenamiento y Predicción (Modelo Único)
  #------------------------------------------------------
  log_info("Iniciando Sección 6: Entrenamiento del Modelo Único.")
  dataset_train <- dataset[foto_mes %in% PARAM$train_final]
  dtrain_final <- lgb.Dataset(data = data.matrix(dataset_train[, campos_buenos, with = FALSE]), label = dataset_train[, clase01])
  
  param_final <- modifyList(PARAM$lgbm$param_fijos,
                            PARAM$out$lgbm$mejores_hiperparametros)
  param_normalizado <- copy(param_final)
  param_normalizado$min_data_in_leaf <- round(param_final$min_data_in_leaf / PARAM$trainingstrategy$undersampling)
  param_normalizado$early_stopping <- NULL
  param_normalizado$early_stopping_round <- NULL
  param_normalizado$early_stopping_rounds <- NULL
  
  modelo_final <- lgb.train(data = dtrain_final, param = param_normalizado)
  log_info("Entrenamiento del modelo final completado.")
  
  lgb.save(modelo_final, paste0(PARAM$carpeta_bayesiana, "modelo.txt"))
  tb_importancia <- as.data.table(lgb.importance(modelo_final))
  fwrite(
    tb_importancia,
    file = paste0(PARAM$carpeta_bayesiana, "importancia.txt"),
    sep = "\t"
  )
  
  log_info("Generando predicciones para el Modelo Único...")
  dfuture <- dataset[foto_mes %in% PARAM$future]
  prediccion_unica <- predict(modelo_final, data.matrix(dfuture[, campos_buenos, with = FALSE]))
  tb_prediccion_unico <- dfuture[, list(numero_de_cliente, foto_mes)]
  tb_prediccion_unico[, prob := prediccion_unica]
  
  drealidad <- realidad_inicializar(dfuture, PARAM)
  
  resultados_unico <- EvaluarYGraficar(
    tb_prediccion = tb_prediccion_unico,
    drealidad = drealidad,
    PARAM = PARAM,
    tipo_modelo = "unico",
    carpeta_salida_kaggle = PARAM$carpeta_kaggle
  )
  log_info(paste0("Envíos óptimos del modelo único: ",paste(resultados_unico$envios_optimos, collapse = ", ")))
  
  # Escribir resultados del modelo único en el log de resumen
  log_summary("--- Resultados del Modelo Único ---")
  log_summary(paste("Envíos con ganancia máxima:", paste(resultados_unico$envios_optimos, collapse = ", ")))
  log_summary(paste("Máxima ganancia:", format(resultados_unico$max_ganancia, big.mark = ".", decimal.mark = ",")))
  log_summary("-----------------------------------\n")

  log_info("Generando los archivos de entrega para el modelo único.")
  dataset_train_final <- dataset[foto_mes %in% PARAM$train_final_kaggle]
  dtrain_final_kaggle <- lgb.Dataset(data = data.matrix(dataset_train_final[, campos_buenos, with = FALSE]), 
                                    label = dataset_train_final[, clase01])

  # Nueva Búsqueda Bayesiana para el dataset final de Kaggle
  log_info("Iniciando nueva Búsqueda Bayesiana para el dataset final de Kaggle.")
  
  # Configuración de la nueva BO
  kbayesiana_kaggle <- paste0(PARAM$carpeta_bayesiana, "bayesiana_kaggle.RDATA")
  funcion_optimizar_kaggle <- EstimarGanancia_AUC_lightgbm_Kaggle
  
  obj.fun_kaggle <- makeSingleObjectiveFunction(
    fn = funcion_optimizar_kaggle,
    minimize = FALSE,
    noisy = TRUE,
    par.set = PARAM$hyperparametertuning$hs,
    has.simple.signature = FALSE
  )
  
  ctrl_kaggle <- makeMBOControl(save.on.disk.at.time = 600,
                                save.file.path = kbayesiana_kaggle)
  ctrl_kaggle <- setMBOControlTermination(ctrl_kaggle, iters = PARAM$hyperparametertuning$iteraciones)
  ctrl_kaggle <- setMBOControlInfill(ctrl_kaggle, crit = makeMBOInfillCritEI())
  
  if (!file.exists(kbayesiana_kaggle)) {
    log_info(
      paste(
        "Iniciando nueva búsqueda Bayesiana (Kaggle) de",
        PARAM$hyperparametertuning$iteraciones,
        "iteraciones."
      )
    )
    bayesiana_salida_kaggle <- mbo(obj.fun_kaggle, learner = surr.km, control = ctrl_kaggle)
  } else {
    log_info("Continuando búsqueda Bayesiana (Kaggle) desde archivo existente.")
    bayesiana_salida_kaggle <- mboContinue(kbayesiana_kaggle)
  }
  log_info("Optimización Bayesiana (Kaggle) finalizada.")
  
  tb_bayesiana_kaggle <- as.data.table(bayesiana_salida_kaggle$opt.path)
  mejores_hiperparametros_kaggle <- tb_bayesiana_kaggle[which.max(y), .SD, .SDcols = names(PARAM$hyperparametertuning$hs$pars)]

  log_info("Mejores hiperparámetros encontrados para Kaggle:")
  log_info(paste(capture.output(print(mejores_hiperparametros_kaggle)), collapse = "\n"))

  param_kaggle_BO <- modifyList(PARAM$lgbm$param_fijos, mejores_hiperparametros_kaggle)
  param_kaggle_BO$early_stopping <- NULL
  param_kaggle_BO$early_stopping_round <- NULL
  param_kaggle_BO$early_stopping_rounds <- NULL

  modelo_final_kaggle <- lgb.train(data = dtrain_final_kaggle, param = param_kaggle_BO)
  log_info("Modelo final para Kaggle re-entrenado con todos los datos y nuevos hiperparámetros.")

  dfuture_entrega <- dataset[foto_mes %in% PARAM$entrega_kaggle]
  prediccion_final <- predict(modelo_final_kaggle, data.matrix(dfuture_entrega[, campos_buenos, with = FALSE]))

  tb_prediccion_final <- dfuture_entrega[, list(numero_de_cliente, foto_mes)]
  tb_prediccion_final[, prob := prediccion_final]

  GenerarEnviosKaggle(
    tb_prediccion = tb_prediccion_final,
    envios_optimos = resultados_unico$envios_optimos,
    tipo_modelo = "final_unico",
    carpeta_salida = PARAM$carpeta_entregables,
    experimento_id = PARAM$experimento
  )
}, error = function(e) {
  log_error("######################################################")
  log_error("Se ha producido un error fatal en las Sección 6.")
  log_error(paste("Mensaje de R:", e$message))
  log_error("######################################################")
  quit(status = 1)
})

tryCatch({
  #------------------------------------------------------
  # Sección 7: Entrenamiento y Predicción (Ensemble)
  #------------------------------------------------------
  log_info("Iniciando Sección 7: Entrenamiento del Ensamble de Modelos.")
  lista_predicciones <- list()
  envios_optimos_individuales <- c()
  lista_resultados_individuales <- list()

  for (semilla_actual in PARAM$semillas_ensemble) {
    log_info(paste0("Entrenando modelo del ensamble con semilla: ", semilla_actual))
    param_normalizado$seed <- semilla_actual

    modelo <- lgb.train(data = dtrain_final, param = param_normalizado)
    prediccion_individual <- predict(modelo, data.matrix(dfuture[, campos_buenos, with = FALSE]))

    tb_pred_individual <- dfuture[, list(numero_de_cliente, foto_mes)]
    tb_pred_individual[, prob := prediccion_individual]
    lista_predicciones[[as.character(semilla_actual)]] <- tb_pred_individual

    resultados_individual <- data.table()
    setorder(tb_pred_individual, -prob)
    
    for (envios in PARAM$cortes) {
      tb_pred_individual[, Predicted := 0L]
      tb_pred_individual[1:envios, Predicted := 1L]
      res_ind <- realidad_evaluar(drealidad, tb_pred_individual)
      resultados_individual <- rbind(
        resultados_individual,
        data.table(clientes = envios, ganancia_total = res_ind$total)
      )
    }
    
    lista_resultados_individuales[[as.character(semilla_actual)]] <- resultados_individual
    
    max_ganancia_ind <- max(resultados_individual$ganancia_total, na.rm = TRUE)
    envio_optimo_individual <- max(resultados_individual[ganancia_total == max_ganancia_ind, clientes])

    log_info(paste0("--> Envío óptimo (más alto) para semilla ", semilla_actual, ": ", envio_optimo_individual))
    envios_optimos_individuales <- c(envios_optimos_individuales, envio_optimo_individual)
  }

  GraficarCurvasEnsemble(lista_resultados_individuales, PARAM)
  
  envio_promedio <- mean(envios_optimos_individuales)
  envio_promedio_redondeado <- round(envio_promedio / 100) * 100
  log_info(paste0("Envíos óptimos individuales: ", paste(envios_optimos_individuales, collapse = ", ")))
  log_info(paste0("Promedio de envíos individuales (redondeado a 100): ", envio_promedio_redondeado))

  log_info("Creando el ensamble final promediando probabilidades.")
  predicciones_todas <- rbindlist(lista_predicciones)
  tb_prediccion_ensamble <- predicciones_todas[, .(prob = mean(prob)), by = .(numero_de_cliente, foto_mes)]

  resultados_ensamble <- EvaluarYGraficar(
    tb_prediccion = tb_prediccion_ensamble,
    drealidad = drealidad,
    PARAM = PARAM,
    tipo_modelo = "ensamble",
    carpeta_salida_kaggle = PARAM$carpeta_kaggle_ensamble
  )
  log_info(paste0("Envíos óptimos del ensamble promediado:", paste(resultados_ensamble$envios_optimos, collapse = ", ")))
  
  # Escribir resultados del modelo ensamble en el log de resumen
  log_summary("--- Resultados del Modelo Ensamble ---")
  log_summary(paste("Envíos con ganancia máxima:", paste(resultados_ensamble$envios_optimos, collapse = ", ")))
  log_summary(paste("Máxima ganancia:", format(resultados_ensamble$max_ganancia, big.mark = ".", decimal.mark = ",")))
  log_summary(paste("Promedio de envíos individuales (redondeado):", envio_promedio_redondeado))
  log_summary("--------------------------------------\n")


  log_info("Generando los archivos finales para entregar, para el ensamble.")
  dataset_train_final_ens <- dataset[foto_mes %in% PARAM$train_final_kaggle]
  dtrain_final_kaggle_ens <- lgb.Dataset(
    data = data.matrix(dataset_train_final_ens[, campos_buenos, with = FALSE]),
    label = dataset_train_final_ens[, clase01]
  )
  log_info("Dataset final de Kaggle para el ensamble preparado.")

  lista_predicciones_final <- list()
  dfuture_entrega <- dataset[foto_mes %in% PARAM$entrega_kaggle]

  for (semilla_actual in PARAM$semillas_ensemble) {
    log_info(paste0("Re-entrenando modelo final del ensamble con semilla: ", semilla_actual))
    param_kaggle_BO$seed <- semilla_actual
    modelo_final_ens <- lgb.train(data = dtrain_final_kaggle_ens, param = param_kaggle_BO)
    prediccion_final_individual <- predict(modelo_final_ens, data.matrix(dfuture_entrega[, campos_buenos, with = FALSE]))

    tb_pred_final_individual <- dfuture_entrega[, list(numero_de_cliente, foto_mes)]
    tb_pred_final_individual[, prob := prediccion_final_individual]
    lista_predicciones_final[[as.character(semilla_actual)]] <- tb_pred_final_individual
  }

  log_info("Creando el ensamble final para la entrega promediando probabilidades.")
  predicciones_finales_todas <- rbindlist(lista_predicciones_final)
  tb_prediccion_final_ensamble <- predicciones_finales_todas[, .(prob = mean(prob)), by = .(numero_de_cliente, foto_mes)]

  envios_finales_ensamble <- unique(c(
    resultados_ensamble$envios_optimos,
    envio_promedio_redondeado
  ))
  log_info(paste0("Envíos finales a generar para el ensamble: ", paste(sort(envios_finales_ensamble), collapse = ", ")))

  GenerarEnviosKaggle(
    tb_prediccion = tb_prediccion_final_ensamble,
    envios_optimos = envios_finales_ensamble,
    tipo_modelo = "final_ensamble",
    carpeta_salida = PARAM$carpeta_entregables,
    experimento_id = PARAM$experimento
  )

  log_info("Archivos de entrega para el ensamble generados correctamente.")

}, error = function(e) {
  log_error("######################################################")
  log_error("Se ha producido un error fatal en las Sección 7.")
  log_error(paste("Mensaje de R:", e$message))
  log_error("######################################################")
  quit(status = 1)
})
#------------------------------------------------------
# Sección 8: Finalización
#------------------------------------------------------
log_info("------------------------------------------------------")
log_info("Script finalizado exitosamente.")
log_info(paste("Fecha y hora de finalización:", format(Sys.time(), "%a %b %d %X %Y")))
log_info("------------------------------------------------------")
