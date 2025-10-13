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
PARAM$experimento <- "expC01_Prueba17_Ensemble"
PARAM$dir_experimento <- paste0("~/buckets/b1/exp/", PARAM$experimento)
PARAM$dir_dataset <- "~/buckets/b1/datasets/"
PARAM$carpeta_logs <- "logs/"
PARAM$carpeta_bayesiana <- "Archivos_Bayesiana/"
PARAM$carpeta_kaggle <- "Kaggle/"
PARAM$carpeta_kaggle_ensamble <- "Kaggle_Promediado/"
PARAM$carpeta_graficos <- "Plots/"
PARAM$carpeta_entregables <- "Entregables/"
PARAM$archivo_summary_log <- "summary.txt"


PARAM$semillas_ensemble <- c(200003, 314159, 102191, 111109, 230101, 100129, 378399, 100992)
PARAM$train <- c(202101, 202102)
PARAM$train_final <- c(202101, 202102)
PARAM$future <- c(202104)
PARAM$train_final_kaggle <- c(202101, 202102, 202103, 202104)
PARAM$entrega_kaggle <- c(202106)
# La semilla de kaggle se usa para la partición public/private, se deja fija
PARAM$semilla_kaggle <- 314159
PARAM$cortes <- seq(0, 20000, by = 100)

PARAM$trainingstrategy$undersampling <- 0.2

PARAM$hyperparametertuning$xval_folds <- 5
PARAM$lgbm$param_fijos <- list(
  boosting= "gbdt",
  objective= "binary",
  metric= "auc",
  first_metric_only= FALSE,
  boost_from_average= TRUE,
  feature_pre_filter= FALSE,
  force_row_wise= TRUE,
  verbosity= -100,
  max_depth= -1L,
  min_gain_to_split= 0,
  min_sum_hessian_in_leaf= 0.001,
  lambda_l1= 0.0,
  lambda_l2= 0.0,
  max_bin= 31L,
  bagging_fraction= 1.0,
  pos_bagging_fraction= 1.0,
  neg_bagging_fraction= 1.0,
  is_unbalance= FALSE,
  scale_pos_weight= 1.0,
  drop_rate= 0.1,
  max_drop= 50,
  skip_drop= 0.5,
  extra_trees= FALSE,
  num_iterations= 1200,
  learning_rate= 0.02,
  feature_fraction= 0.5,
  num_leaves= 750,
  min_data_in_leaf= 5000,
  early_stopping_round = 100
)
PARAM$hyperparametertuning$hs <- makeParamSet(
  makeIntegerParam("num_leaves", lower = 10L, upper = 2048L),
  makeIntegerParam("num_iterations", lower = 50L, upper = 3000L),
  makeNumericParam("learning_rate", lower = 0.01, upper = 0.5),
  makeNumericParam("feature_fraction", lower = 0.1, upper = 1.0),
  makeIntegerParam("min_data_in_leaf", lower = 1L, upper = 10000L)
)
PARAM$hyperparametertuning$iteraciones <- 80

# ----- Configuración del Logger -----
dir.create(PARAM$dir_experimento, showWarnings = FALSE, recursive = TRUE)
setwd(PARAM$dir_experimento)
dir.create(PARAM$carpeta_logs, showWarnings = FALSE, recursive = TRUE)
log_file <- file.path(PARAM$carpeta_logs, paste0("log_", PARAM$experimento, ".log"))
log_appender(appender_tee(log_file))

summary_log_file_path <- file.path(PARAM$carpeta_logs, PARAM$archivo_summary_log)
cat(paste0("Resumen del Experimento: ", PARAM$experimento, "\n"), file = summary_log_file_path, append = FALSE)
cat(paste0("Fecha: ", Sys.time(), "\n\n"), file = summary_log_file_path, append = TRUE)

log_info("------------------------------------------------------")
log_info(paste("Inicio del script. Experimento:", PARAM$experimento))
log_info(paste("El log se guardará en:", log_file))
log_info(paste("El resumen se guardará en:", summary_log_file_path))
log_info("------------------------------------------------------")

#------------------------------------------------------
# Sección 3: Funciones Auxiliares
#------------------------------------------------------
log_info("Iniciando Sección 3: Cargando funciones auxiliares")

log_summary <- function(message) {
  cat(paste0(message, "\n"), file = summary_log_file_path, append = TRUE)
}

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

# La función de FE se deja como en el original
generar_lags_avanzados <- function(dataset, lags_a_crear, cols_a_excluir, id_col) {
  log_info("Iniciando la generación de features de lags, deltas y deltas pct...")
  cols_con_lag <- setdiff(names(dataset), cols_a_excluir)
  log_info(paste0("Se crearán features para ", length(cols_con_lag), " columnas."))
  for (k in lags_a_crear) {
    log_info(paste0("--- Generando lag ", k, " y sus derivados ---"))
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
  log_info("Generación de lags finalizada.")
  return(dataset)
}

#------------------------------------------------------
# Sección 4: Preparación de Datos
#------------------------------------------------------
tryCatch({
  log_info("Iniciando Sección 4: Preparación de Datos.")
  log_info(paste("Leyendo dataset desde:", PARAM$dir_dataset))
  dataset <- fread(file.path(PARAM$dir_dataset, "competencia_01.csv.gz"), stringsAsFactors = TRUE)
  log_info("Dataset cargado correctamente.")

  log_info("Inicio de Feature Engineering")
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

  lags_deseados <- c(1)
  columnas_a_ignorar <- c("numero_de_cliente", "foto_mes", "clase_ternaria")
  id_cliente <- "numero_de_cliente"

  # Genero lags
  dataset <- generar_lags_avanzados(
    dataset = dataset,
    lags_a_crear = lags_deseados,
    cols_a_excluir = columnas_a_ignorar,
    id_col = id_cliente
  )

  dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]
  log_info("Generando dataset de entrenamiento para la Optimización Bayesiana.")
  dataset_train <- dataset[foto_mes %in% PARAM$train]
  
  # Se aplica undersampling una sola vez para la BO, pero la semilla se setea dentro del bucle de BO para reproducibilidad
  dataset_train[, azar := runif(nrow(dataset_train))]
  dataset_train[, training := 0L]
  dataset_train[foto_mes %in% PARAM$train & (azar <= PARAM$trainingstrategy$undersampling | clase_ternaria %in% c("BAJA+1", "BAJA+2")), training := 1L]
  log_info(paste0("Undersampling aplicado con una tasa de:", PARAM$trainingstrategy$undersampling))

  campos_buenos <- setdiff(colnames(dataset_train), c("clase_ternaria", "clase01", "azar", "training"))
  
  dtrain <- lgb.Dataset(
    data = data.matrix(dataset_train[training == 1L, campos_buenos, with = FALSE]),
    label = dataset_train[training == 1L, clase01],
    free_raw_data = FALSE
  )
  log_info("Dataset de entrenamiento para LightGBM (BO) creado.")
  log_info(paste("Dimensiones de dtrain -> Filas:", nrow(dtrain), "| Columnas:", ncol(dtrain)))

}, error = function(e) {
  log_error("Error fatal en Sección 4: Preparación de Datos. Mensaje: ", e$message)
  quit(status = 1)
})

#------------------------------------------------------
# Sección 5: Optimización Bayesiana (Por Semilla)
#------------------------------------------------------
PARAM$out_ensemble <- list() # Lista para guardar los resultados de cada semilla

tryCatch({
  log_info("Iniciando Sección 5: Optimización Bayesiana de Hiperparámetros para cada semilla.")
  
  for (semilla_actual in PARAM$semillas_ensemble) {
    log_info(paste0("--- Iniciando Optimización Bayesiana para la semilla: ", semilla_actual, " ---"))

    # Seteamos la semilla para la BO
    PARAM$lgbm$param_fijos$seed <- semilla_actual
    
    EstimarGanancia_AUC_lightgbm <- function(x) {
      param_completo <- modifyList(PARAM$lgbm$param_fijos, x)
      modelocv <- lgb.cv(data = dtrain, nfold = PARAM$hyperparametertuning$xval_folds, stratified = TRUE, param = param_completo)
      AUC <- modelocv$best_score
      rm(modelocv)
      gc(full = TRUE, verbose = FALSE)
      log_info(paste("Iteración BO [Semilla:", semilla_actual, "] -> AUC:", format(AUC, digits = 6), "|", format(Sys.time(), "%X")))
      return(AUC)
    }

    # Directorio y archivo específicos para esta semilla
    dir_semilla <- file.path(PARAM$carpeta_bayesiana, paste0("semilla_", semilla_actual))
    dir.create(dir_semilla, showWarnings = FALSE, recursive = TRUE)
    kbayesiana <- file.path(dir_semilla, "bayesiana.RDATA")
    
    funcion_optimizar <- EstimarGanancia_AUC_lightgbm
    configureMlr(show.learner.output = FALSE)
    obj.fun <- makeSingleObjectiveFunction(fn = funcion_optimizar, minimize = FALSE, noisy = TRUE, par.set = PARAM$hyperparametertuning$hs, has.simple.signature = FALSE)
    ctrl <- makeMBOControl(save.on.disk.at.time = 600, save.file.path = kbayesiana)
    ctrl <- setMBOControlTermination(ctrl, iters = PARAM$hyperparametertuning$iteraciones)
    ctrl <- setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())
    surr.km <- makeLearner("regr.km", predict.type = "se", covtype = "matern3_2", control = list(trace = FALSE))

    if (!file.exists(kbayesiana)) {
      log_info(paste("Iniciando nueva búsqueda Bayesiana de", PARAM$hyperparametertuning$iteraciones, "iteraciones para semilla", semilla_actual))
      bayesiana_salida <- mbo(obj.fun, learner = surr.km, control = ctrl)
    } else {
      log_info(paste("Continuando búsqueda Bayesiana desde archivo existente para semilla", semilla_actual))
      bayesiana_salida <- mboContinue(kbayesiana)
    }
    
    log_info(paste("Optimización Bayesiana finalizada para la semilla:", semilla_actual))

    tb_bayesiana <- as.data.table(bayesiana_salida$opt.path)
    tb_bayesiana[, iter := .I]
    setorder(tb_bayesiana, -y)
    fwrite(tb_bayesiana, file = file.path(dir_semilla, "BO_log.txt"), sep = "\t")

    mejores_hiperparametros <- tb_bayesiana[1, setdiff(colnames(tb_bayesiana), c("y", "dob", "eol", "error.message", "exec.time", "ei", "error.model", "train.time", "prop.type", "propose.time", "se", "mean", "iter")), with = FALSE]
    mejor_y <- tb_bayesiana[1, y]
    
    # Guardamos los resultados en la lista
    PARAM$out_ensemble[[as.character(semilla_actual)]] <- list(
      mejores_hiperparametros = mejores_hiperparametros,
      y = mejor_y
    )
    
    write_yaml(list(mejores_hiperparametros=mejores_hiperparametros, y=mejor_y), file = file.path(dir_semilla, "PARAM.yml"))
    
    log_info(paste("Mejores hiperparámetros para semilla", semilla_actual, ":"))
    log_info(paste(capture.output(print(mejores_hiperparametros)), collapse = "\n"))
    log_info(paste("Mejor AUC (y) para semilla", semilla_actual, ":", mejor_y))

    log_summary(paste0("--- Resultados BO para Semilla: ", semilla_actual, " ---"))
    log_summary("Hiperparámetros Óptimos:")
    sapply(capture.output(print(mejores_hiperparametros)), log_summary)
    log_summary(paste("\nAUC:", format(mejor_y, digits = 6)))
    log_summary("---------------------------------------------\n")
  }
}, error = function(e) {
  log_error("Error fatal en Sección 5: Optimización Bayesiana. Mensaje: ", e$message)
  quit(status = 1)
})


#------------------------------------------------------
# Funciones para secciones 6 y 7
#------------------------------------------------------
EvaluarYGraficar <- function(tb_prediccion, drealidad, PARAM, tipo_modelo, carpeta_salida_kaggle) {
  log_info(paste0("Iniciando evaluación y graficación para el modelo: ", tipo_modelo))
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
  log_info(paste0("Envíos óptimos para [", tipo_modelo, "]: ", paste(envios_max_total, collapse = ", "), " | Máxima Ganancia: ", max_ganancia_valor))
  
  resultados_long <- melt(resultados, id.vars = "clientes", measure.vars = c("ganancia_total", "ganancia_public", "ganancia_private"), variable.name = "tipo", value.name = "ganancia")
  maximos <- resultados_long[, .SD[ganancia == max(ganancia)], by = tipo]
  
  etiquetas <- paste0(tools::toTitleCase(gsub("_", " ", maximos$tipo)), " (máx = ", format(maximos$ganancia, big.mark = ".", decimal.mark = ","), " en ", maximos$clientes, ")")
  names(etiquetas) <- maximos$tipo
  
  dir.create(PARAM$carpeta_graficos, showWarnings = FALSE)
  p <- ggplot(resultados_long, aes(x = clientes, y = ganancia, color = tipo)) +
    geom_line(linewidth = 1) +
    geom_point(data = maximos, aes(x = clientes, y = ganancia, color = tipo), size = 3) +
    labs(title = paste0("Curvas de Ganancia (Modelo ", tools::toTitleCase(gsub("_", " ", tipo_modelo))," - ", PARAM$experimento, ")"), x = "Clientes Contactados (Envíos)", y = "Ganancia", color = "Máximos") +
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(values = c("ganancia_total" = "steelblue", "ganancia_public" = "forestgreen", "ganancia_private" = "firebrick"), labels = etiquetas) +
    theme_minimal() + theme(legend.position = "bottom")
  
  ggsave(paste0(PARAM$carpeta_graficos, "curvas_", tipo_modelo, "_", PARAM$experimento, ".png"), plot = p, width = 11, height = 7)
  log_info(paste0("Gráfico de curvas de ganancia (", tipo_modelo, ") guardado."))
  
  # Retornamos la tabla de resultados junto con los envíos óptimos
  return(list(envios_optimos = envios_max_total, max_ganancia = max_ganancia_valor, tabla_resultados = resultados))
}

GenerarEnviosKaggle <- function(tb_prediccion, envios_optimos, tipo_modelo, carpeta_salida, experimento_id) {
  log_info(paste0("Iniciando generación de envíos para Kaggle del modelo: '", tipo_modelo, "'"))
  envios_a_generar <- unique(c(envios_optimos, 10500, envios_optimos + 100, envios_optimos - 100))
  log_info(paste0("Se generarán archivos para los siguientes envíos: ", paste(envios_a_generar, collapse = ", ")))
  dir.create(carpeta_salida, showWarnings = FALSE)
  setorder(tb_prediccion, -prob)

  for (envios in envios_a_generar) {
    if(envios <= 0) next
    tb_prediccion[, Predicted := 0L]
    tb_prediccion[1:envios, Predicted := 1L]
    archivo_kaggle <- paste0(carpeta_salida, experimento_id, "_", tipo_modelo, "_", envios, ".csv")
    fwrite(tb_prediccion[, list(numero_de_cliente, Predicted)], file = archivo_kaggle, sep = ",")
    log_info(paste0("Archivo generado: ", archivo_kaggle))
  }
  log_info(paste0("Generación de envíos para '", tipo_modelo, "' finalizada."))
}

GraficarCurvasEnsemble <- function(lista_resultados_tablas, PARAM) {
  log_info("Iniciando la graficación de la superposición de curvas del ensemble.")
  tb_todas <- rbindlist(lapply(names(lista_resultados_tablas), function(sem) {
    lista_resultados_tablas[[sem]][, semilla := as.character(sem)]
  }))

  tb_promedio <- tb_todas[, .(ganancia_total = mean(ganancia_total)), by = clientes]
  maximos_individuales <- tb_todas[, .SD[ganancia_total == max(ganancia_total)][1], by = semilla]
  maximo_promedio <- tb_promedio[ganancia_total == max(ganancia_total)][1]
  
  labels_plot <- c(sapply(maximos_individuales$semilla, function(sem) {
    max_info <- maximos_individuales[semilla == sem]
    paste0("S ", sem, ": G ", format(max_info$ganancia_total, big.mark = "."), " (E ", max_info$clientes, ")")
  }),
    paste0("Promedio: G ", format(maximo_promedio$ganancia_total, big.mark = "."), " (E ", maximo_promedio$clientes, ")")
  )
  names(labels_plot) <- c(maximos_individuales$semilla, "Promedio")

  colores_plot <- c(scales::hue_pal()(length(PARAM$semillas_ensemble)), "black")
  names(colores_plot) <- c(names(lista_resultados_tablas), "Promedio")

  dir.create(PARAM$carpeta_graficos, showWarnings = FALSE)
  p <- ggplot() +
    geom_line(data = tb_todas, aes(x = clientes, y = ganancia_total, group = semilla, color = semilla), alpha = 0.5, linewidth = 1) +
    geom_line(data = tb_promedio, aes(x = clientes, y = ganancia_total, color = "Promedio"), linewidth = 1.2) +
    geom_point(data = maximos_individuales, aes(x = clientes, y = ganancia_total, color = semilla), size = 3, alpha = 0.7) +
    geom_point(data = maximo_promedio, aes(x = clientes, y = ganancia_total, color = "Promedio"), size = 4, shape = 18) +
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(name = "Curvas y sus Máximos", values = colores_plot, labels = labels_plot) +
    labs(title = "Curvas de Ganancia del Ensemble y su Promedio", subtitle = paste0("Experimento: ", PARAM$experimento), x = "Clientes Contactados (Envíos)", y = "Ganancia Total") +
    theme_minimal() + theme(legend.position = "bottom") + guides(color = guide_legend(ncol = 2))

  ggsave(paste0(PARAM$carpeta_graficos, "curvas_ensemble_superpuestas_", PARAM$experimento, ".png"), plot = p, width = 12, height = 8)
  log_info("Gráfico de superposición de curvas del ensemble guardado correctamente.")
}

#------------------------------------------------------
# Sección 6: Entrenamiento y Predicción (Modelos Individuales)
#------------------------------------------------------
lista_predicciones_future <- list()
lista_resultados_evaluacion <- list()

tryCatch({
  log_info("Iniciando Sección 6: Entrenamiento de Modelos Individuales y Evaluación.")
  
  dataset_train_final_modelos <- dataset[foto_mes %in% PARAM$train_final]
  dtrain_final <- lgb.Dataset(data = data.matrix(dataset_train_final_modelos[, campos_buenos, with = FALSE]), label = dataset_train_final_modelos[, clase01])
  
  dfuture <- dataset[foto_mes %in% PARAM$future]
  drealidad <- realidad_inicializar(dfuture, PARAM)

  for (semilla_actual in PARAM$semillas_ensemble) {
    log_info(paste0("--- Entrenando y Prediciendo con modelo de semilla: ", semilla_actual, " ---"))
    
    mejores_params <- PARAM$out_ensemble[[as.character(semilla_actual)]]$mejores_hiperparametros
    param_final <- modifyList(PARAM$lgbm$param_fijos, mejores_params)
    param_final$seed <- semilla_actual
    
    param_normalizado <- copy(param_final)
    param_normalizado$min_data_in_leaf <- round(param_final$min_data_in_leaf / PARAM$trainingstrategy$undersampling)
    param_normalizado$early_stopping_round <- NULL
    
    modelo_individual <- lgb.train(data = dtrain_final, param = param_normalizado)
    
    prediccion_individual <- predict(modelo_individual, data.matrix(dfuture[, campos_buenos, with = FALSE]))
    tb_prediccion_individual <- dfuture[, list(numero_de_cliente, foto_mes)]
    tb_prediccion_individual[, prob := prediccion_individual]
    
    # Guardamos la predicción para el ensamble
    lista_predicciones_future[[as.character(semilla_actual)]] <- tb_prediccion_individual
    
    # Evaluamos y graficamos el modelo individual
    resultados_individual <- EvaluarYGraficar(
      tb_prediccion = tb_prediccion_individual,
      drealidad = drealidad,
      PARAM = PARAM,
      tipo_modelo = paste0("individual_semilla_", semilla_actual),
      carpeta_salida_kaggle = PARAM$carpeta_kaggle
    )
    
    # Guardamos la tabla de resultados para el gráfico comparativo
    lista_resultados_evaluacion[[as.character(semilla_actual)]] <- resultados_individual$tabla_resultados
    
    log_summary(paste0("--- Resultados Modelo Individual (Semilla: ", semilla_actual, ") ---"))
    log_summary(paste0("Envíos con ganancia máxima: ", paste(resultados_individual$envios_optimos, collapse = ", ")))
    log_summary(paste0("Máxima ganancia: ", format(resultados_individual$max_ganancia, big.mark = ".", decimal.mark = ",")))
    log_summary("----------------------------------------------------\n")
  }
}, error = function(e) {
  log_error("Error fatal en Sección 6: Entrenamiento Individual. Mensaje: ", e$message)
  quit(status = 1)
})


#------------------------------------------------------
# Sección 7: Ensamblado, Evaluación y Generación Final
#------------------------------------------------------
tryCatch({
  log_info("Iniciando Sección 7: Ensamblado de Modelos y Generación Final.")
  
  # 1. Promediar predicciones en 'future'
  log_info("Promediando predicciones para la evaluación del ensamble...")
  tb_predicciones_todas <- rbindlist(lista_predicciones_future, idcol = "semilla")
  tb_prediccion_ensemble <- tb_predicciones_todas[, .(prob = mean(prob)), by = .(numero_de_cliente, foto_mes)]
  
  # 2. Evaluar y graficar el ensamble
  log_info("Evaluando el modelo de ensamble...")
  resultados_ensemble <- EvaluarYGraficar(
    tb_prediccion = tb_prediccion_ensemble,
    drealidad = drealidad,
    PARAM = PARAM,
    tipo_modelo = "ensemble",
    carpeta_salida_kaggle = PARAM$carpeta_kaggle_ensamble
  )
  
  log_summary("--- Resultados del Modelo Ensemble ---")
  log_summary(paste0("Envíos con ganancia máxima: ", paste(resultados_ensemble$envios_optimos, collapse = ", ")))
  log_summary(paste0("Máxima ganancia: ", format(resultados_ensemble$max_ganancia, big.mark = ".", decimal.mark = ",")))
  log_summary("---------------------------------------\n")
  
  # 3. Graficar la superposición de curvas
  GraficarCurvasEnsemble(lista_resultados_evaluacion, PARAM)
  
  # 4. Re-entrenar modelos con todos los datos para Kaggle y generar predicción final
  log_info("Re-entrenando modelos con todos los datos para la entrega final de Kaggle...")
  dataset_train_final_kaggle <- dataset[foto_mes %in% PARAM$train_final_kaggle]
  dtrain_kaggle <- lgb.Dataset(data = data.matrix(dataset_train_final_kaggle[, campos_buenos, with = FALSE]), label = dataset_train_final_kaggle[, clase01])
  dfuture_entrega <- dataset[foto_mes %in% PARAM$entrega_kaggle]
  
  lista_predicciones_kaggle <- list()
  
  for (semilla_actual in PARAM$semillas_ensemble) {
    log_info(paste0("--- Re-entrenando modelo de semilla: ", semilla_actual, " para Kaggle ---"))
    mejores_params <- PARAM$out_ensemble[[as.character(semilla_actual)]]$mejores_hiperparametros
    param_final <- modifyList(PARAM$lgbm$param_fijos, mejores_params)
    param_final$seed <- semilla_actual
    
    param_normalizado <- copy(param_final)
    param_normalizado$min_data_in_leaf <- round(param_final$min_data_in_leaf / PARAM$trainingstrategy$undersampling)
    param_normalizado$early_stopping_round <- NULL

    modelo_kaggle <- lgb.train(data = dtrain_kaggle, param = param_normalizado)
    prediccion_kaggle <- predict(modelo_kaggle, data.matrix(dfuture_entrega[, campos_buenos, with = FALSE]))
    
    tb_prediccion_kaggle_individual <- dfuture_entrega[, list(numero_de_cliente)]
    tb_prediccion_kaggle_individual[, prob := prediccion_kaggle]
    lista_predicciones_kaggle[[as.character(semilla_actual)]] <- tb_prediccion_kaggle_individual
  }
  
  # 5. Promediar predicciones de Kaggle y generar archivos de envío
  log_info("Promediando predicciones para la entrega final de Kaggle...")
  tb_predicciones_kaggle_todas <- rbindlist(lista_predicciones_kaggle)
  tb_prediccion_final_ensemble <- tb_predicciones_kaggle_todas[, .(prob = mean(prob)), by = numero_de_cliente]
  
  GenerarEnviosKaggle(
    tb_prediccion = tb_prediccion_final_ensemble,
    envios_optimos = resultados_ensemble$envios_optimos,
    tipo_modelo = "final_ensemble",
    carpeta_salida = PARAM$carpeta_entregables,
    experimento_id = PARAM$experimento
  )
  
}, error = function(e) {
  log_error("Error fatal en Sección 7: Ensamblado y Generación Final. Mensaje: ", e$message)
  quit(status = 1)
})

#------------------------------------------------------
# Sección 8: Finalización
#------------------------------------------------------
log_info("------------------------------------------------------")
log_info("Script finalizado exitosamente.")
log_info(paste("Fecha y hora de finalización:", format(Sys.time(), "%a %b %d %X %Y")))
log_info("------------------------------------------------------")