#!/usr/bin/env Rscript
#------------------------------------------------------
# Sección 1: Carga de Librerías
#------------------------------------------------------
suppressPackageStartupMessages({
  if (!require("logger")) install.packages("logger")
  library("logger")
  if (!require("data.table")) install.packages("data.table")
  library("data.table")
  if (!require("lightgbm")) install.packages("lightgbm")
  library("lightgbm")
  if (!require("mlrMBO")) install.packages("mlrMBO")
  library("mlrMBO")
  if (!require("ggplot2")) install.packages("ggplot2")
  library("ggplot2")
  if (!require("ggrepel")) install.packages("ggrepel")
  library("ggrepel")
  if (!require("scales")) install.packages("scales")
  library("scales")
  if (!require("yaml")) install.packages("yaml")
  library("yaml")
  if (!require("DiceKriging")) install.packages("DiceKriging")
  library("DiceKriging")
})

#------------------------------------------------------
# Sección 2: Configuración Inicial y Parámetros
#------------------------------------------------------
rm(list = ls(all.names = TRUE))
gc(full = TRUE, verbose = FALSE)

PARAM <- list()
PARAM$experimento <- "expC03_Prueba18"
PARAM$dir_experimento <- paste0("~/buckets/b1/exp/", PARAM$experimento)
PARAM$dir_dataset <- "~/buckets/b1/datasets/"
PARAM$carpeta_logs <- "logs/"
PARAM$carpeta_bayesiana <- "Archivos_Bayesiana/"
PARAM$carpeta_graficos <- "Plots/"
PARAM$carpeta_entregables <- "Entregables/"
PARAM$archivo_summary_log <- "summary.txt"

# PASO 1 y 2: Meses para optimización y búsqueda de envíos
PARAM$bo_training <- c(202101, 202102, 202103)
PARAM$bo_validation <- c(202104)

# PASO 3: Meses para entrenamiento final
PARAM$final_training <- c(202101, 202102, 202103, 202104)
PARAM$final_delivery <- c(202106)

PARAM$semillas_ensemble <- c(200003, 314159, 102191, 111109, 230101, 100129)
PARAM$semilla_kaggle_partition <- 314159
PARAM$cortes <- seq(5000, 15000, by = 100)

# Parámetros fijos para LightGBM
PARAM$lgbm$param_fijos <- list(
  boosting = "gbdt", objective = "binary", metric = "auc",
  first_metric_only = TRUE, boost_from_average = TRUE,
  feature_pre_filter = FALSE, force_row_wise = TRUE, verbosity = -100,
  max_depth = -1L, min_gain_to_split = 0.0, min_sum_hessian_in_leaf = 0.001,
  lambda_l1 = 0.0, lambda_l2 = 0.0, max_bin = 31L, bagging_fraction = 1.0,
  num_iterations = 5000, early_stopping_round = 200
)

# Rangos de Hiperparámetros para la Búsqueda Bayesiana
PARAM$hyperparametertuning$hs <- makeParamSet(
    makeNumericParam("learning_rate", lower = 0.05, upper = 0.3),
    makeNumericParam("feature_fraction", lower = 0.4, upper = 0.8),
    makeIntegerParam("min_data_in_leaf", lower = 20000L, upper = 80000L),
    makeIntegerParam("num_leaves", lower = 2048L, upper = 8192L),
    makeIntegerParam("num_iterations", lower= 200L,   upper= 3000L)
)

PARAM$hyperparametertuning$iteraciones <- 60

# ----- Configuración del Logger -----
dir.create(PARAM$dir_experimento, showWarnings = FALSE, recursive = TRUE)
setwd(PARAM$dir_experimento)
dir.create(PARAM$carpeta_logs, showWarnings = FALSE, recursive = TRUE)
log_file <- file.path(PARAM$carpeta_logs, paste0("log_", PARAM$experimento, ".log"))
log_appender(appender_tee(log_file))
summary_log_file_path <- file.path(PARAM$carpeta_logs, PARAM$archivo_summary_log)
cat(paste0("Resumen del Experimento: ", PARAM$experimento, "\n"), file = summary_log_file_path, append = FALSE)
cat(paste0("Fecha: ", Sys.time(), "\n\n"), file = summary_log_file_path, append = TRUE)
log_info(paste("Inicio del script. Experimento:", PARAM$experimento))

#------------------------------------------------------
# Sección 3: Funciones Auxiliares
#------------------------------------------------------
log_info("Cargando funciones auxiliares")

log_summary <- function(message) {
  cat(paste0(message, "\n"), file = summary_log_file_path, append = TRUE)
}

generar_lags_avanzados <- function(dataset, lags_a_crear, id_col) {
  cols_a_excluir <- c(id_col, "foto_mes", "clase_ternaria", "clase01")
  cols_con_lag <- setdiff(names(dataset), cols_a_excluir)
  for (k in lags_a_crear) {
    nombres_lag <- paste0(cols_con_lag, "_lag", k)
    nombres_delta <- paste0(cols_con_lag, "_delta", k)
    nombres_delta_pct <- paste0(cols_con_lag, "_delta_pct", k)
    dataset[, (nombres_lag) := shift(.SD, k, NA, "lag"), by = id_col, .SDcols = cols_con_lag]
    dataset[, (nombres_delta) := Map(function(col, col_lag) get(col) - get(col_lag), cols_con_lag, nombres_lag)]
    dataset[, (nombres_delta_pct) := Map(
        function(col, col_lag) {
            lag_val <- get(col_lag)
            curr_val <- get(col)
            delta_pct <- ifelse(is.na(lag_val) | lag_val == 0, NA, (curr_val - lag_val) / abs(lag_val))
            return(delta_pct)
        },
        cols_con_lag, nombres_lag
    )]
  }
  return(dataset)
}

crear_dataset_cuantico <- function(dataset_original) {
  positivos <- dataset_original[clase01 == 1L]
  clones_negativos <- copy(positivos)
  clones_negativos[, clase01 := 0L]
  return(rbind(dataset_original, clones_negativos))
}

realidad_inicializar <- function(pfuture, pparam) {
  drealidad <- pfuture[, list(numero_de_cliente, foto_mes, clase_ternaria)]
  set.seed(pparam$semilla_kaggle_partition)
  drealidad[, fold := sample(c(1, 2), .N, replace = TRUE, prob = c(0.3, 0.7))]
  return(drealidad)
}

realidad_evaluar <- function(prealidad, pprediccion) {
  prealidad[pprediccion, on = c("numero_de_cliente", "foto_mes"), predicted := i.Predicted]
  tbl <- prealidad[, list("qty" = .N), list(fold, predicted, clase_ternaria)]
  res <- list()
  res$public <- tbl[fold == 1 & predicted == 1L, sum(qty * ifelse(clase_ternaria == "BAJA+2", 780000, -20000), na.rm = TRUE)] / 0.3
  res$private <- tbl[fold == 2 & predicted == 1L, sum(qty * ifelse(clase_ternaria == "BAJA+2", 780000, -20000), na.rm = TRUE)] / 0.7
  res$total <- tbl[predicted == 1L, sum(qty * ifelse(clase_ternaria == "BAJA+2", 780000, -20000), na.rm = TRUE)]
  prealidad[, predicted := NULL]
  return(res)
}

EvaluarYGraficar <- function(tb_prediccion, drealidad, PARAM, tipo_modelo) {
  log_info(paste0("Iniciando evaluación y graficación para: ", tipo_modelo))
  resultados <- data.table()
  setorder(tb_prediccion, -prob)
  
  for (envios in PARAM$cortes) {
    tb_prediccion[, Predicted := 0L]
    tb_prediccion[1:envios, Predicted := 1L]
    res <- realidad_evaluar(drealidad, tb_prediccion)
    resultados <- rbind(resultados, data.table(clientes = envios, ganancia_total = res$total, ganancia_public = res$public, ganancia_private = res$private))
  }
  
  max_ganancia_valor <- max(resultados$ganancia_total)
  envios_max_total <- resultados[ganancia_total == max_ganancia_valor, clientes]
  
  resultados_long <- melt(resultados, id.vars = "clientes", measure.vars = c("ganancia_total", "ganancia_public", "ganancia_private"), variable.name = "tipo", value.name = "ganancia")
  maximos <- resultados_long[, .SD[ganancia == max(ganancia)], by = tipo]
  
  etiquetas <- paste0(tools::toTitleCase(gsub("_", " ", maximos$tipo)), " (máx = ", format(maximos$ganancia, big.mark = "."), " en ", maximos$clientes, ")")
  names(etiquetas) <- maximos$tipo
  
  dir.create(PARAM$carpeta_graficos, showWarnings = FALSE)
  p <- ggplot(resultados_long, aes(x = clientes, y = ganancia, color = tipo)) +
    geom_line(linewidth = 1) +
    geom_point(data = maximos, aes(x = clientes, y = ganancia, color = tipo), size = 3) +
    labs(title = paste0("Curvas de Ganancia (", tipo_modelo, " - ", PARAM$experimento, ")"), x = "Clientes Contactados (Envíos)", y = "Ganancia") +
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(values = c("ganancia_total" = "steelblue", "ganancia_public" = "forestgreen", "ganancia_private" = "firebrick"), labels = etiquetas) +
    theme_minimal() + theme(legend.position = "bottom")
  
  ggsave(paste0(PARAM$carpeta_graficos, "curvas_", tipo_modelo, "_", PARAM$experimento, ".png"), plot = p, width = 11, height = 7)
  log_info(paste0("Gráfico de curvas de ganancia (", tipo_modelo, ") guardado."))
  
  return(list(envios_optimos = envios_max_total, max_ganancia = max_ganancia_valor))
}

GenerarEnviosKaggle <- function(tb_prediccion, envios_optimos, tipo_modelo, carpeta_salida, experimento_id) {
  log_info(paste0("Iniciando generación de envíos para Kaggle del modelo: '", tipo_modelo, "'"))
  envios_a_generar <- unique(envios_optimos)
  dir.create(carpeta_salida, showWarnings = FALSE)
  setorder(tb_prediccion, -prob)

  for (envios in envios_a_generar) {
    tb_prediccion[, Predicted := 0L]
    tb_prediccion[1:envios, Predicted := 1L]
    archivo_kaggle <- paste0(carpeta_salida, "/", experimento_id, "_", tipo_modelo, "_", envios, ".csv")
    fwrite(tb_prediccion[, list(numero_de_cliente, Predicted)], file = archivo_kaggle, sep = ",")
    log_info(paste0("Archivo generado: ", archivo_kaggle))
  }
  log_info(paste0("Generación de envíos para '", tipo_modelo, "' finalizada."))
}

#------------------------------------------------------
# Sección 4: Preparación Global de Datos
#------------------------------------------------------
tryCatch({
    log_info("Iniciando Sección 4: Preparación Global de Datos.")
    dataset <- fread(file.path(PARAM$dir_dataset, "competencia_01.csv.gz"), stringsAsFactors = TRUE)

    setkey(dataset, numero_de_cliente, foto_mes)

    cols_a_rankear <- c("mcomisiones_mantenimiento", "Master_Fvencimiento", "Visa_fultimo_cierre", "Master_fultimo_cierre", "mpayroll", "cpayroll_trx")
    nuevas_cols_rank <- paste0(cols_a_rankear, "_rank")
    rank_con_cero_fijo <- function(x) {
    resultado <- numeric(length(x))
    idx_pos <- which(x > 0); idx_neg <- which(x < 0); idx_cero <- which(x == 0)
    if (length(idx_pos) > 0) resultado[idx_pos] <- frankv(x[idx_pos], ties.method = "average") / length(idx_pos)
    if (length(idx_neg) > 0) resultado[idx_neg] <- (frankv(-x[idx_neg], ties.method = "average") / length(idx_neg)) * -1
    if (length(idx_cero) > 0) resultado[idx_cero] <- 0
    return(resultado)
    }
    dataset[, (nuevas_cols_rank) := lapply(.SD, rank_con_cero_fijo), by = foto_mes, .SDcols = cols_a_rankear]
    dataset[, (cols_a_rankear) := NULL]

    log_info("Generando Lags y Deltas (orden 1 y 2)...")
    dataset <- generar_lags_avanzados(dataset, lags_a_crear = c(1, 2), id_col = "numero_de_cliente")

    dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

    campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01", "numero_de_cliente", "foto_mes"))
  
}, error = function(e) {
  log_error("Error fatal en Sección 4. Mensaje: ", e$message); quit(status = 1)
})

#------------------------------------------------------
# PASO 1: Optimización Bayesiana
#------------------------------------------------------
PARAM$out_ensemble <- list()
tryCatch({
  log_info("--- INICIO PASO 1: Optimización Bayesiana de Hiperparámetros ---")
  
  dataset_bo_original <- dataset[foto_mes %in% PARAM$bo_training]
  dataset_cuantico_bo <- crear_dataset_cuantico(dataset_bo_original)
  
  dtrain_bo <- lgb.Dataset(
    data = data.matrix(dataset_cuantico_bo[, campos_buenos, with = FALSE]),
    label = dataset_cuantico_bo[, clase01]
  )
  
  dvalid_bo <- lgb.Dataset(
    data = data.matrix(dataset[foto_mes %in% PARAM$bo_validation, campos_buenos, with = FALSE]),
    label = dataset[foto_mes %in% PARAM$bo_validation, clase01],
    free_raw_data=FALSE
  )
  
  log_info(paste("BO Train (cuántico):", nrow(dtrain_bo), "filas | BO Valid:", nrow(dvalid_bo), "filas"))
  
  EstimarGanancia_AUC_lightgbm <- function(x) {
    param_completo <- modifyList(PARAM$lgbm$param_fijos, x)
    modelo_bo <- lgb.train(data = dtrain_bo, valids = list(valid = dvalid_bo), param = param_completo, verbose = -1)
    AUC <- modelo_bo$best_score
    assign("nrounds_optimo", modelo_bo$best_iter, envir = .GlobalEnv)
    return(AUC)
  }
  
  for (semilla_actual in PARAM$semillas_ensemble) {
    log_info(paste0("--- Iniciando BO para la semilla: ", semilla_actual, " ---"))
    PARAM$lgbm$param_fijos$seed <- semilla_actual
    
    dir_semilla <- file.path(PARAM$carpeta_bayesiana, paste0("semilla_", semilla_actual))
    dir.create(dir_semilla, showWarnings = FALSE, recursive = TRUE)
    kbayesiana <- file.path(dir_semilla, "bayesiana.RDATA")
    
    configureMlr(show.learner.output = FALSE)
    obj.fun <- makeSingleObjectiveFunction(fn = EstimarGanancia_AUC_lightgbm, minimize = FALSE, noisy = TRUE, par.set = PARAM$hyperparametertuning$hs, has.simple.signature = FALSE)
    ctrl <- makeMBOControl(save.on.disk.at.time = 600, save.file.path = kbayesiana)
    ctrl <- setMBOControlTermination(ctrl, iters = PARAM$hyperparametertuning$iteraciones)
    ctrl <- setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())
    surr.km <- makeLearner("regr.km", predict.type = "se", covtype = "matern3_2", control = list(trace = FALSE))

    if (!file.exists(kbayesiana)) {
      bayesiana_salida <- mbo(obj.fun, learner = surr.km, control = ctrl)
    } else {
      bayesiana_salida <- mboContinue(kbayesiana)
    }
    
    tb_bayesiana <- as.data.table(bayesiana_salida$opt.path)
    setorder(tb_bayesiana, -y)
    
    mejores_hiperparametros <- as.list(tb_bayesiana[1, names(PARAM$hyperparametertuning$hs$pars), with = FALSE])
    EstimarGanancia_AUC_lightgbm(mejores_hiperparametros)
    mejores_hiperparametros$num_iterations <- nrounds_optimo
    
    PARAM$out_ensemble[[as.character(semilla_actual)]] <- list(
      mejores_hiperparametros = mejores_hiperparametros,
      y = tb_bayesiana[1, y]
    )
    
    log_info(paste("Mejores hiperparámetros para semilla", semilla_actual, "(n_iter:", nrounds_optimo, "):"))
    log_info(paste(capture.output(print(mejores_hiperparametros)), collapse = "\n"))
    rm(bayesiana_salida, tb_bayesiana, ctrl, surr.km, obj.fun)
    gc(full = TRUE, verbose = FALSE)
  }
  
  log_info("--- FIN PASO 1: Optimización Bayesiana completada para todas las semillas. ---")
  
}, error = function(e) {
  log_error("Error fatal en PASO 1. Mensaje: ", e$message); quit(status = 1)
})

#------------------------------------------------------
# PASO 2: Encontrar Envíos Óptimos
#------------------------------------------------------
tryCatch({
  log_info("--- INICIO PASO 2: Encontrando Envíos Óptimos con el Ensemble ---")
  
  lista_predicciones_eval <- list()
  
  dataset_bo_original_p2 <- dataset[foto_mes %in% PARAM$bo_training]
  dataset_cuantico_bo_p2 <- crear_dataset_cuantico(dataset_bo_original_p2)
  dtrain_p2 <- lgb.Dataset(
    data = data.matrix(dataset_cuantico_bo_p2[, campos_buenos, with = FALSE]),
    label = dataset_cuantico_bo_p2[, clase01]
  )
  
  for (semilla_actual in PARAM$semillas_ensemble) {
    log_info(paste0("Entrenando (1-3) y prediciendo (4) para semilla: ", semilla_actual))
    
    mejores_params <- PARAM$out_ensemble[[as.character(semilla_actual)]]$mejores_hiperparametros
    param_final_eval <- modifyList(PARAM$lgbm$param_fijos, mejores_params)
    param_final_eval$seed <- semilla_actual
    param_final_eval$early_stopping_round <- NULL
    
    modelo_eval <- lgb.train(data = dtrain_p2, param = param_final_eval)
    
    prediccion_eval <- predict(
      modelo_eval,
      data.matrix(dataset[foto_mes %in% PARAM$bo_validation, campos_buenos, with = FALSE])
    )
    
    tb_prediccion <- dataset[foto_mes %in% PARAM$bo_validation, list(numero_de_cliente, foto_mes)]
    tb_prediccion[, prob := prediccion_eval]
    lista_predicciones_eval[[as.character(semilla_actual)]] <- tb_prediccion
  }
  
  tb_predicciones_todas <- rbindlist(lista_predicciones_eval)
  tb_prediccion_ensemble_eval <- tb_predicciones_todas[, .(prob = mean(prob)), by = .(numero_de_cliente, foto_mes)]
  
  drealidad_eval <- realidad_inicializar(dataset[foto_mes %in% PARAM$bo_validation, ], PARAM)
  
  resultados_eval <- EvaluarYGraficar(
    tb_prediccion = tb_prediccion_ensemble_eval,
    drealidad = drealidad_eval,
    PARAM = PARAM,
    tipo_modelo = "ensemble_evaluacion"
  )
  
  envios_optimos <- resultados_eval$envios_optimos
  max_ganancia_eval <- resultados_eval$max_ganancia
  
  log_info(paste0("GANANCIA MÁXIMA ENCONTRADA (en mes 4): ", format(max_ganancia_eval, big.mark = ",")))
  log_info(paste0("ENVÍOS ÓPTIMOS: ", paste(envios_optimos, collapse = ", ")))
  
  log_summary("--- Resultados de la Búsqueda del Corte (Paso 2) ---")
  log_summary(paste0("Máxima ganancia simulada en 202104: ", format(max_ganancia_eval, big.mark = ",")))
  log_summary(paste0("Número de envíos que la producen: ", paste(envios_optimos, collapse = ", ")))
  
  log_info("--- FIN PASO 2: Envíos óptimos determinados. ---")
  
}, error = function(e) {
  log_error("Error fatal en PASO 2. Mensaje: ", e$message)
  log_error(paste("Se usarán envíos por defecto:", paste(envios_optimos, collapse = ", ")))
})

#------------------------------------------------------
# PASO 3: Entrenamiento Final y Generación de Entregables
#------------------------------------------------------
tryCatch({
  log_info("--- INICIO PASO 3: Entrenamiento Final y Generación para Kaggle ---")
  
  dataset_final_original <- dataset[foto_mes %in% PARAM$final_training]
  dataset_cuantico_final <- crear_dataset_cuantico(dataset_final_original)
  
  dtrain_final <- lgb.Dataset(
    data = data.matrix(dataset_cuantico_final[, campos_buenos, with = FALSE]),
    label = dataset_cuantico_final[, clase01]
  )
  log_info(paste("Entrenamiento final (cuántico, meses 1-4) con", nrow(dtrain_final), "filas."))
  
  lista_predicciones_final <- list()
  
  for (semilla_actual in PARAM$semillas_ensemble) {
    log_info(paste0("Entrenando modelo final (1-4) para semilla: ", semilla_actual))
    
    mejores_params <- PARAM$out_ensemble[[as.character(semilla_actual)]]$mejores_hiperparametros
    param_final_kaggle <- modifyList(PARAM$lgbm$param_fijos, mejores_params)
    param_final_kaggle$seed <- semilla_actual
    param_final_kaggle$early_stopping_round <- NULL
    
    modelo_final <- lgb.train(data = dtrain_final, param = param_final_kaggle)
    
    prediccion_final <- predict(
      modelo_final,
      data.matrix(dataset[foto_mes %in% PARAM$final_delivery, campos_buenos, with = FALSE])
    )
    
    tb_prediccion_final <- dataset[foto_mes %in% PARAM$final_delivery, list(numero_de_cliente)]
    tb_prediccion_final[, prob := prediccion_final]
    lista_predicciones_final[[as.character(semilla_actual)]] <- tb_prediccion_final
  }
  
  tb_predicciones_finales_todas <- rbindlist(lista_predicciones_final)
  tb_prediccion_ensemble_final <- tb_predicciones_finales_todas[, .(prob = mean(prob)), by = numero_de_cliente]
  
  log_info(paste0("Generando archivos de entrega para Kaggle con los envíos óptimos: ", paste(envios_optimos, collapse = ", ")))
  
  GenerarEnviosKaggle(
    tb_prediccion = tb_prediccion_ensemble_final,
    envios_optimos = envios_optimos,
    tipo_modelo = "final_ensemble_cuantico",
    carpeta_salida = PARAM$carpeta_entregables,
    experimento_id = PARAM$experimento
  )
  
  log_info("--- FIN PASO 3: Archivos para Kaggle generados. ---")

}, error = function(e) {
  log_error("Error fatal en PASO 3. Mensaje: ", e$message); quit(status = 1)
})

#------------------------------------------------------
# Sección Final: Finalización
#------------------------------------------------------
log_info("------------------------------------------------------")
log_info("Script finalizado exitosamente.")
log_info(paste("Fecha y hora de finalización:", format(Sys.time(), "%a %b %d %X %Y")))
log_info("------------------------------------------------------")