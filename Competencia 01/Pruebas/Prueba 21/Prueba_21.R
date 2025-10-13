#!/usr/bin/env Rscript
#------------------------------------------------------
# Script Final: Ensamble de Prueba_13 y Prueba_16
#------------------------------------------------------

#------------------------------------------------------
# Sección 1: Carga de Librerías
#------------------------------------------------------
suppressPackageStartupMessages({
  if (!require("logger")) install.packages("logger")
  library("logger")
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
  if (!require("ggrepel")) install.packages("ggrepel")
  library("ggrepel")
  if (!require("scales")) install.packages("scales")
  library("scales")
})

#------------------------------------------------------
# Sección 2: Configuración Inicial y Parámetros
#------------------------------------------------------
rm(list = ls(all.names = TRUE))
gc(full = TRUE, verbose = FALSE)

PARAM <- list()
PARAM$experimento <- "expC01_Prueba21"
PARAM$dir_experimento <- paste0("~/buckets/b1/exp/", PARAM$experimento)
PARAM$dir_dataset <- "~/buckets/b1/datasets/"
PARAM$carpeta_logs <- "logs/"
PARAM$carpeta_bayesiana <- "Archivos_Bayesiana/"
PARAM$carpeta_entregables <- "Entregables/"
PARAM$carpeta_graficos <- "Plots/"
PARAM$archivo_summary_log <- "summary.txt"

# Parámetros comunes de ejecución
PARAM$semilla_primigenia <- 200003
PARAM$semillas_ensemble <- c(200003, 300007, 400009, 500009, 600011, 314159, 102191, 111109, 230101, 100129)
PARAM$future <- c(202104)
PARAM$train_final_kaggle <- c(202101, 202102, 202103, 202104)
PARAM$entrega_kaggle <- c(202106)
PARAM$semilla_kaggle <- 314159
PARAM$cortes <- seq(0, 20000, by = 100)
PARAM$hyperparametertuning$xval_folds <- 5
PARAM$hyperparametertuning$iteraciones <- 100

# Parámetros específicos para Prueba_13
PARAM$p13 <- list()
PARAM$p13$train <- c(202101, 202102)
PARAM$p13$train_final <- c(202101, 202102)
PARAM$p13$undersampling <- 0.5

# Parámetros específicos para Prueba_16
PARAM$p16 <- list()
PARAM$p16$train <- c(202101, 202102)
PARAM$p16$train_final <- c(202101, 202102, 202103)
PARAM$p16$undersampling <- 0.2

# Parámetros fijos de LightGBM (comunes)
PARAM$lgbm_fijos <- list(
  boosting = "gbdt", objective = "binary", metric = "auc",
  first_metric_only = FALSE, boost_from_average = TRUE, feature_pre_filter = FALSE,
  force_row_wise = TRUE, verbosity = -100, seed = PARAM$semilla_primigenia,
  max_depth = -1L, min_gain_to_split = 0, min_sum_hessian_in_leaf = 0.001,
  lambda_l1 = 0.0, lambda_l2 = 0.0, max_bin = 31L, bagging_fraction = 1.0,
  pos_bagging_fraction = 1.0, neg_bagging_fraction = 1.0, is_unbalance = FALSE,
  scale_pos_weight = 1.0, drop_rate = 0.1, max_drop = 50, skip_drop = 0.5,
  extra_trees = FALSE, num_iterations = 1200, learning_rate = 0.02,
  feature_fraction = 0.5, num_leaves = 750, min_data_in_leaf = 5000,
  early_stopping_round = 100
)

# Bordes para la Optimización Bayesiana (comunes)
PARAM$hyperparametertuning$hs <- makeParamSet(
  makeIntegerParam("num_leaves", lower = 10L, upper = 2048L),
  makeIntegerParam("num_iterations", lower = 50L, upper = 3000L),
  makeNumericParam("learning_rate", lower = 0.01, upper = 0.5),
  makeNumericParam("feature_fraction", lower = 0.1, upper = 1.0),
  makeIntegerParam("min_data_in_leaf", lower = 1L, upper = 10000L)
)

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
log_info("Cargando funciones auxiliares...")

log_summary <- function(message) {
  cat(paste0(message, "\n"), file = summary_log_file_path, append = TRUE)
}

particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed, "L'Ecuyer-CMRG")
  bloque <- unlist(mapply(function(x, y) rep(y, x), division, seq(from = start, length.out = length(division))))
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

generar_lags_avanzados <- function(dataset, lags_a_crear, cols_a_excluir, id_col) {
  cols_con_lag <- setdiff(names(dataset), cols_a_excluir)
  for (k in lags_a_crear) {
    nombres_lag <- paste0(cols_con_lag, "_lag", k)
    nombres_delta <- paste0(cols_con_lag, "_delta", k)
    dataset[, (nombres_lag) := shift(.SD, k, NA, "lag"), by = id_col, .SDcols = cols_con_lag]
    dataset[, (nombres_delta) := Map(function(col, col_lag) get(col) - get(col_lag), cols_con_lag, nombres_lag)]
  }
  return(dataset)
}

rank_con_cero_fijo <- function(x) {
  resultado <- numeric(length(x))
  idx_pos <- which(x > 0); idx_neg <- which(x < 0); idx_cero <- which(x == 0)
  if (length(idx_pos) > 0) resultado[idx_pos] <- frankv(x[idx_pos], ties.method = "average") / length(idx_pos)
  if (length(idx_neg) > 0) resultado[idx_neg] <- (frankv(-x[idx_neg], ties.method = "average") / length(idx_neg)) * -1
  if (length(idx_cero) > 0) resultado[idx_cero] <- 0
  return(resultado)
}

# Funciones de evaluación y graficación (se usarán en la sección 6)
EvaluarYGraficar <- function(tb_prediccion, drealidad, PARAM, tipo_modelo) {
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
    log_info(paste0("Envíos óptimos para [", tipo_modelo, "]: ", paste(envios_max_total, collapse = ", ")))
    log_info(paste0("Máxima ganancia para [", tipo_modelo, "]: ", format(max_ganancia_valor, big.mark = ".", decimal.mark = ",")))

    log_summary(paste0("--- Resultados del Modelo: ", tipo_modelo, " ---"))
    log_summary(paste0("Envíos con ganancia máxima: ", paste(envios_max_total, collapse = ", ")))
    log_summary(paste0("Máxima ganancia: ", format(max_ganancia_valor, big.mark = ".", decimal.mark = ",")))
    log_summary("--------------------------------------------------\n")

    # Graficación... (código omitido por brevedad, es idéntico al original)
    return(list(envios_optimos = envios_max_total, max_ganancia = max_ganancia_valor))
}

GenerarEnviosKaggle <- function(tb_prediccion, envios_optimos, tipo_modelo, carpeta_salida, experimento_id) {
    log_info(paste0("Generando envíos para Kaggle del modelo: '", tipo_modelo, "'"))
    dir.create(carpeta_salida, showWarnings = FALSE)
    setorder(tb_prediccion, -prob)
    for (envios in envios_optimos) {
        tb_prediccion[, Predicted := 0L]
        tb_prediccion[1:envios, Predicted := 1L]
        archivo_kaggle <- paste0(carpeta_salida, experimento_id, "_", tipo_modelo, "_", envios, ".csv")
        fwrite(tb_prediccion[, list(numero_de_cliente, Predicted)], file = archivo_kaggle, sep = ",")
        log_info(paste0("Archivo generado: ", archivo_kaggle))
    }
}


#------------------------------------------------------------------------------
#--------- PROCESAMIENTO PRUEBA 13 --------------------------------------------
#------------------------------------------------------------------------------
log_info("======================================================")
log_info("INICIANDO PROCESAMIENTO DE PRUEBA 13")
log_info("======================================================")

#--- Sección 4.1: Preparación de Datos (Prueba 13) ---
log_info("Cargando dataset original...")
dataset_original <- fread(file.path(PARAM$dir_dataset, "competencia_01.csv.gz"), stringsAsFactors = TRUE)
dataset_p13 <- copy(dataset_original)

log_info("Inicio de Feature Engineering (Prueba 13)")
setkey(dataset_p13, numero_de_cliente, foto_mes)
cols_a_rankear_p13 <- c("mcomisiones_mantenimiento", "Master_Fvencimiento", "Visa_fultimo_cierre", "Master_fultimo_cierre", "mpayroll", "cpayroll_trx")
nuevas_cols_rank_p13 <- paste0(cols_a_rankear_p13, "_rank")
dataset_p13[, (nuevas_cols_rank_p13) := lapply(.SD, rank_con_cero_fijo), by = foto_mes, .SDcols = cols_a_rankear_p13]
dataset_p13[, (cols_a_rankear_p13) := NULL]

cols_a_excluir_p13 <- c("numero_de_cliente", "foto_mes", "clase_ternaria")
cols_con_lag_p13 <- setdiff(names(dataset_p13), cols_a_excluir_p13)
dataset_p13 <- generar_lags_avanzados(dataset_p13, lags_a_crear = c(1), cols_a_excluir = cols_a_excluir_p13, id_col = "numero_de_cliente")

dataset_p13[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]
dataset_train_p13 <- dataset_p13[foto_mes %in% PARAM$p13$train]
set.seed(PARAM$semilla_primigenia)
dataset_train_p13[, azar := runif(.N)]
dataset_train_p13[, training := ifelse(azar <= PARAM$p13$undersampling | clase01 == 1L, 1L, 0L)]

campos_buenos_p13 <- setdiff(colnames(dataset_train_p13), c("clase_ternaria", "clase01", "azar", "training"))
dtrain_p13 <- lgb.Dataset(
    data = data.matrix(dataset_train_p13[training == 1L, campos_buenos_p13, with = FALSE]),
    label = dataset_train_p13[training == 1L, clase01]
)

#--- Sección 4.2: Optimización Bayesiana (Prueba 13) ---
log_info("Iniciando Optimización Bayesiana para Prueba 13")
EstimarGanancia_AUC_p13 <- function(x) {
    param_completo <- modifyList(PARAM$lgbm_fijos, x)
    modelocv <- lgb.cv(data = dtrain_p13, nfold = PARAM$hyperparametertuning$xval_folds, stratified = TRUE, param = param_completo)
    return(modelocv$best_score)
}
dir.create(PARAM$carpeta_bayesiana, showWarnings = FALSE)
kbayesiana_p13 <- file.path(PARAM$carpeta_bayesiana, "bayesiana_p13.RDATA")
obj.fun_p13 <- makeSingleObjectiveFunction(fn = EstimarGanancia_AUC_p13, minimize = FALSE, noisy = TRUE, par.set = PARAM$hyperparametertuning$hs, has.simple.signature = FALSE)
ctrl_p13 <- makeMBOControl(save.on.disk.at.time = 600, save.file.path = kbayesiana_p13)
ctrl_p13 <- setMBOControlTermination(ctrl_p13, iters = PARAM$hyperparametertuning$iteraciones)
ctrl_p13 <- setMBOControlInfill(ctrl_p13, crit = makeMBOInfillCritEI())
surr.km_p13 <- makeLearner("regr.km", predict.type = "se", covtype = "matern3_2", control = list(trace = FALSE))

if (!file.exists(kbayesiana_p13)) {
    bayesiana_salida_p13 <- mbo(obj.fun_p13, learner = surr.km_p13, control = ctrl_p13)
} else {
    bayesiana_salida_p13 <- mboContinue(kbayesiana_p13)
}
mejores_hiperparametros_p13 <- as.data.table(bayesiana_salida_p13$x)
log_info("Mejores hiperparámetros para Prueba 13 encontrados.")

#--- Sección 4.3: Entrenamiento de Modelos de Validación (Prueba 13) ---
log_info("Entrenando modelos de validación para Prueba 13.")
dataset_train_final_p13 <- dataset_p13[foto_mes %in% PARAM$p13$train_final]
dtrain_final_p13 <- lgb.Dataset(data = data.matrix(dataset_train_final_p13[, campos_buenos_p13, with = FALSE]), label = dataset_train_final_p13[, clase01])

param_final_p13 <- modifyList(PARAM$lgbm_fijos, mejores_hiperparametros_p13)
param_final_p13$min_data_in_leaf <- round(param_final_p13$min_data_in_leaf / PARAM$p13$undersampling)
param_final_p13$early_stopping_round <- NULL

dfuture <- dataset_p13[foto_mes %in% PARAM$future]

# Modelo Único P13
log_info("...Modelo Único P13")
modelo_unico_p13 <- lgb.train(data = dtrain_final_p13, param = param_final_p13)
prediccion_unica_p13 <- predict(modelo_unico_p13, data.matrix(dfuture[, campos_buenos_p13, with = FALSE]))
tb_prediccion_unico_p13 <- dfuture[, list(numero_de_cliente, foto_mes)]
tb_prediccion_unico_p13[, prob := prediccion_unica_p13]

# Ensamble P13
log_info("...Ensamble P13")
lista_predicciones_p13 <- list()
for (semilla in PARAM$semillas_ensemble) {
    param_final_p13$seed <- semilla
    modelo <- lgb.train(data = dtrain_final_p13, param = param_final_p13)
    pred <- predict(modelo, data.matrix(dfuture[, campos_buenos_p13, with = FALSE]))
    tb_pred <- dfuture[, list(numero_de_cliente, foto_mes)]; tb_pred[, prob := pred]
    lista_predicciones_p13[[as.character(semilla)]] <- tb_pred
}
tb_prediccion_ensamble_p13 <- rbindlist(lista_predicciones_p13)[, .(prob = mean(prob)), by = .(numero_de_cliente, foto_mes)]
log_info("Finalizado procesamiento de Prueba 13.")


#------------------------------------------------------------------------------
#--------- PROCESAMIENTO PRUEBA 16 --------------------------------------------
#------------------------------------------------------------------------------
log_info("======================================================")
log_info("INICIANDO PROCESAMIENTO DE PRUEBA 16")
log_info("======================================================")
rm(dataset_p13, dtrain_p13, dataset_train_p13, dtrain_final_p13); gc()
dataset_p16 <- copy(dataset_original)

#--- Sección 5.1: Preparación de Datos (Prueba 16) ---
log_info("Inicio de Feature Engineering (Prueba 16)")
setkey(dataset_p16, numero_de_cliente, foto_mes)
cols_a_rankear_p16 <- c("mrentabilidad", "mrentabilidad_annual", "mcomisiones", "mactivos_margen", "mpasivos_margen", "mcuenta_corriente_adicional", "mcuenta_corriente", "mcaja_ahorro", "mcaja_ahorro_adicional", "mcaja_ahorro_dolares", "mcuentas_saldo", "mautoservicio", "mtarjeta_visa_consumo", "mtarjeta_master_consumo", "mprestamos_personales", "mprestamos_prendarios", "mprestamos_hipotecarios", "mplazo_fijo_dolares", "mplazo_fijo_pesos", "minversion1_pesos", "minversion1_dolares", "minversion2", "mpayroll", "mpayroll2", "mcuenta_debitos_automaticos", "mttarjeta_visa_debitos_automaticos", "mttarjeta_master_debitos_automaticos", "mpagodeservicios", "mpagomiscuentas", "mcajeros_propios_descuentos", "mtarjeta_visa_descuentos", "mtarjeta_master_descuentos", "mcomisiones_mantenimiento", "mcomisiones_otras", "mforex_buy", "mforex_sell", "mtransferencias_recibidas", "mtransferencias_emitidas", "mextraccion_autoservicio", "mcheques_depositados", "mcheques_emitidos", "mcheques_depositados_rechazados", "mcheques_emitidos_rechazados", "matm", "matm_other", "Master_mfinanciacion_limite", "Master_msaldototal", "Master_msaldopesos", "Master_msaldodolares", "Master_mconsumospesos", "Master_mconsumosdolares", "Master_mlimitecompra", "Master_madelantopesos", "Master_madelantodolares", "Master_mpagado", "Master_mpagospesos", "Master_mpagosdolares", "Master_mconsumototal", "Master_mpagominimo", "Visa_mfinanciacion_limite", "Visa_msaldototal", "Visa_msaldopesos", "Visa_msaldodolares", "Visa_mconsumospesos", "Visa_mconsumosdolares", "Visa_mlimitecompra", "Visa_madelantopesos", "Visa_madelantodolares", "Visa_mpagado", "Visa_mpagospesos", "Visa_mpagosdolares", "Visa_mconsumototal", "Visa_mpagominimo")
nuevas_cols_rank_p16 <- paste0(cols_a_rankear_p16, "_rank")
dataset_p16[, (nuevas_cols_rank_p16) := lapply(.SD, rank_con_cero_fijo), by = foto_mes, .SDcols = cols_a_rankear_p16]
dataset_p16[, (cols_a_rankear_p16) := NULL]

cols_a_excluir_p16 <- c("numero_de_cliente", "foto_mes", "clase_ternaria")
dataset_p16 <- generar_lags_avanzados(dataset_p16, lags_a_crear = c(1, 2), cols_a_excluir = cols_a_excluir_p16, id_col = "numero_de_cliente")

dataset_p16[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]
dataset_train_p16 <- dataset_p16[foto_mes %in% PARAM$p16$train]
set.seed(PARAM$semilla_primigenia)
dataset_train_p16[, azar := runif(.N)]
dataset_train_p16[, training := ifelse(azar <= PARAM$p16$undersampling | clase01 == 1L, 1L, 0L)]
campos_buenos_p16 <- setdiff(colnames(dataset_train_p16), c("clase_ternaria", "clase01", "azar", "training"))
dtrain_p16 <- lgb.Dataset(
    data = data.matrix(dataset_train_p16[training == 1L, campos_buenos_p16, with = FALSE]),
    label = dataset_train_p16[training == 1L, clase01]
)

#--- Sección 5.2: Optimización Bayesiana (Prueba 16) ---
log_info("Iniciando Optimización Bayesiana para Prueba 16")
EstimarGanancia_AUC_p16 <- function(x) {
    param_completo <- modifyList(PARAM$lgbm_fijos, x)
    modelocv <- lgb.cv(data = dtrain_p16, nfold = PARAM$hyperparametertuning$xval_folds, stratified = TRUE, param = param_completo)
    return(modelocv$best_score)
}
kbayesiana_p16 <- file.path(PARAM$carpeta_bayesiana, "bayesiana_p16.RDATA")
obj.fun_p16 <- makeSingleObjectiveFunction(fn = EstimarGanancia_AUC_p16, minimize = FALSE, noisy = TRUE, par.set = PARAM$hyperparametertuning$hs, has.simple.signature = FALSE)
ctrl_p16 <- makeMBOControl(save.on.disk.at.time = 600, save.file.path = kbayesiana_p16)
ctrl_p16 <- setMBOControlTermination(ctrl_p16, iters = PARAM$hyperparametertuning$iteraciones)
ctrl_p16 <- setMBOControlInfill(ctrl_p16, crit = makeMBOInfillCritEI())
surr.km_p16 <- makeLearner("regr.km", predict.type = "se", covtype = "matern3_2", control = list(trace = FALSE))

if (!file.exists(kbayesiana_p16)) {
    bayesiana_salida_p16 <- mbo(obj.fun_p16, learner = surr.km_p16, control = ctrl_p16)
} else {
    bayesiana_salida_p16 <- mboContinue(kbayesiana_p16)
}
mejores_hiperparametros_p16 <- as.data.table(bayesiana_salida_p16$x)
log_info("Mejores hiperparámetros para Prueba 16 encontrados.")

#--- Sección 5.3: Entrenamiento de Modelos de Validación (Prueba 16) ---
log_info("Entrenando modelos de validación para Prueba 16.")
dataset_train_final_p16 <- dataset_p16[foto_mes %in% PARAM$p16$train_final]
dtrain_final_p16 <- lgb.Dataset(data = data.matrix(dataset_train_final_p16[, campos_buenos_p16, with = FALSE]), label = dataset_train_final_p16[, clase01])

param_final_p16 <- modifyList(PARAM$lgbm_fijos, mejores_hiperparametros_p16)
param_final_p16$min_data_in_leaf <- round(param_final_p16$min_data_in_leaf / PARAM$p16$undersampling)
param_final_p16$early_stopping_round <- NULL

dfuture <- dataset_p16[foto_mes %in% PARAM$future]

# Modelo Único P16
log_info("...Modelo Único P16")
modelo_unico_p16 <- lgb.train(data = dtrain_final_p16, param = param_final_p16)
prediccion_unica_p16 <- predict(modelo_unico_p16, data.matrix(dfuture[, campos_buenos_p16, with = FALSE]))
tb_prediccion_unico_p16 <- dfuture[, list(numero_de_cliente, foto_mes)]
tb_prediccion_unico_p16[, prob := prediccion_unica_p16]

# Ensamble P16
log_info("...Ensamble P16")
lista_predicciones_p16 <- list()
for (semilla in PARAM$semillas_ensemble) {
    param_final_p16$seed <- semilla
    modelo <- lgb.train(data = dtrain_final_p16, param = param_final_p16)
    pred <- predict(modelo, data.matrix(dfuture[, campos_buenos_p16, with = FALSE]))
    tb_pred <- dfuture[, list(numero_de_cliente, foto_mes)]; tb_pred[, prob := pred]
    lista_predicciones_p16[[as.character(semilla)]] <- tb_pred
}
tb_prediccion_ensamble_p16 <- rbindlist(lista_predicciones_p16)[, .(prob = mean(prob)), by = .(numero_de_cliente, foto_mes)]
log_info("Finalizado procesamiento de Prueba 16.")
rm(dataset_p16, dtrain_p16, dataset_train_p16, dtrain_final_p16); gc()

#------------------------------------------------------------------------------
#--------- SECCIÓN 6: ENSAMBLE FINAL Y VALIDACIÓN -----------------------------
#------------------------------------------------------------------------------
log_info("======================================================")
log_info("INICIANDO ENSAMBLE Y VALIDACIÓN FINAL")
log_info("======================================================")

drealidad <- realidad_inicializar(dfuture, PARAM)

#--- 6.1: Ensamble de los dos Modelos Únicos ---
log_info("Promediando y evaluando los Modelos Únicos.")
tb_prediccion_ensamble_de_unicos <- rbindlist(list(tb_prediccion_unico_p13, tb_prediccion_unico_p16))
tb_prediccion_ensamble_de_unicos <- tb_prediccion_ensamble_de_unicos[, .(prob = mean(prob)), by = .(numero_de_cliente, foto_mes)]
resultados_ensamble_unicos <- EvaluarYGraficar(tb_prediccion_ensamble_de_unicos, drealidad, PARAM, "ensamble_de_unicos")

#--- 6.2: Ensamble de los dos Ensambles ---
log_info("Promediando y evaluando los modelos Ensamble.")
tb_prediccion_ensamble_de_ensambles <- rbindlist(list(tb_prediccion_ensamble_p13, tb_prediccion_ensamble_p16))
tb_prediccion_ensamble_de_ensambles <- tb_prediccion_ensamble_de_ensambles[, .(prob = mean(prob)), by = .(numero_de_cliente, foto_mes)]
resultados_ensamble_ensambles <- EvaluarYGraficar(tb_prediccion_ensamble_de_ensambles, drealidad, PARAM, "ensamble_de_ensambles")

#------------------------------------------------------------------------------
#--------- SECCIÓN 7: GENERACIÓN DE ENTREGAS FINALES --------------------------
#------------------------------------------------------------------------------
log_info("======================================================")
log_info("INICIANDO GENERACIÓN DE ARCHIVOS FINALES PARA KAGGLE")
log_info("======================================================")

#--- 7.1 Re-entrenamiento y Predicción Final para Prueba 13 ---
log_info("Re-entrenando modelos de P13 con todos los datos...")
dataset_p13 <- copy(dataset_original) # Recargamos datos
# (Repetimos FE de P13)
setkey(dataset_p13, numero_de_cliente, foto_mes)
dataset_p13[, (nuevas_cols_rank_p13) := lapply(.SD, rank_con_cero_fijo), by = foto_mes, .SDcols = cols_a_rankear_p13]
dataset_p13[, (cols_a_rankear_p13) := NULL]
dataset_p13 <- generar_lags_avanzados(dataset_p13, lags_a_crear = c(1), cols_a_excluir = cols_a_excluir_p13, id_col = "numero_de_cliente")
dataset_p13[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]
dtrain_kaggle_p13 <- lgb.Dataset(data = data.matrix(dataset_p13[foto_mes %in% PARAM$train_final_kaggle, campos_buenos_p13, with = FALSE]), label = dataset_p13[foto_mes %in% PARAM$train_final_kaggle, clase01])
dfuture_entrega <- dataset_p13[foto_mes %in% PARAM$entrega_kaggle]

# Predicción final Único P13
modelo_final_unico_p13 <- lgb.train(data = dtrain_kaggle_p13, param = param_final_p13)
pred_final_unico_p13 <- predict(modelo_final_unico_p13, data.matrix(dfuture_entrega[, campos_buenos_p13, with = FALSE]))
tb_pred_final_unico_p13 <- dfuture_entrega[, .(numero_de_cliente)]; tb_pred_final_unico_p13[, prob := pred_final_unico_p13]

# Predicción final Ensamble P13
lista_pred_final_p13 <- list()
for (semilla in PARAM$semillas_ensemble) {
    param_final_p13$seed <- semilla; modelo <- lgb.train(data = dtrain_kaggle_p13, param = param_final_p13)
    pred <- predict(modelo, data.matrix(dfuture_entrega[, campos_buenos_p13, with = FALSE]))
    lista_pred_final_p13[[as.character(semilla)]] <- data.table(numero_de_cliente = dfuture_entrega$numero_de_cliente, prob = pred)
}
tb_pred_final_ensamble_p13 <- rbindlist(lista_pred_final_p13)[, .(prob = mean(prob)), by = numero_de_cliente]
rm(dataset_p13, dtrain_kaggle_p13); gc()

#--- 7.2 Re-entrenamiento y Predicción Final para Prueba 16 ---
log_info("Re-entrenando modelos de P16 con todos los datos...")
dataset_p16 <- copy(dataset_original) # Recargamos datos
# (Repetimos FE de P16)
setkey(dataset_p16, numero_de_cliente, foto_mes)
dataset_p16[, (nuevas_cols_rank_p16) := lapply(.SD, rank_con_cero_fijo), by = foto_mes, .SDcols = cols_a_rankear_p16]
dataset_p16[, (cols_a_rankear_p16) := NULL]
dataset_p16 <- generar_lags_avanzados(dataset_p16, lags_a_crear = c(1, 2), cols_a_excluir = cols_a_excluir_p16, id_col = "numero_de_cliente")
dataset_p16[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]
dtrain_kaggle_p16 <- lgb.Dataset(data = data.matrix(dataset_p16[foto_mes %in% PARAM$train_final_kaggle, campos_buenos_p16, with = FALSE]), label = dataset_p16[foto_mes %in% PARAM$train_final_kaggle, clase01])
dfuture_entrega <- dataset_p16[foto_mes %in% PARAM$entrega_kaggle]

# Predicción final Único P16
modelo_final_unico_p16 <- lgb.train(data = dtrain_kaggle_p16, param = param_final_p16)
pred_final_unico_p16 <- predict(modelo_final_unico_p16, data.matrix(dfuture_entrega[, campos_buenos_p16, with = FALSE]))
tb_pred_final_unico_p16 <- dfuture_entrega[, .(numero_de_cliente)]; tb_pred_final_unico_p16[, prob := pred_final_unico_p16]

# Predicción final Ensamble P16
lista_pred_final_p16 <- list()
for (semilla in PARAM$semillas_ensemble) {
    param_final_p16$seed <- semilla; modelo <- lgb.train(data = dtrain_kaggle_p16, param = param_final_p16)
    pred <- predict(modelo, data.matrix(dfuture_entrega[, campos_buenos_p16, with = FALSE]))
    lista_pred_final_p16[[as.character(semilla)]] <- data.table(numero_de_cliente = dfuture_entrega$numero_de_cliente, prob = pred)
}
tb_pred_final_ensamble_p16 <- rbindlist(lista_pred_final_p16)[, .(prob = mean(prob)), by = numero_de_cliente]
rm(dataset_p16, dtrain_kaggle_p16); gc()

#--- 7.3: Promedio de Predicciones Finales y Generación de Archivos ---
log_info("Promediando predicciones finales y generando archivos de entrega.")

# Archivo final para el ensamble de modelos únicos
tb_entrega_final_unicos <- rbindlist(list(tb_pred_final_unico_p13, tb_pred_final_unico_p16))
tb_entrega_final_unicos <- tb_entrega_final_unicos[, .(prob = mean(prob)), by = numero_de_cliente]
GenerarEnviosKaggle(
    tb_prediccion = tb_entrega_final_unicos,
    envios_optimos = resultados_ensamble_unicos$envios_optimos,
    tipo_modelo = "final_ensamble_de_unicos",
    carpeta_salida = PARAM$carpeta_entregables,
    experimento_id = PARAM$experimento
)

# Archivo final para el ensamble de ensambles
tb_entrega_final_ensambles <- rbindlist(list(tb_pred_final_ensamble_p13, tb_pred_final_ensamble_p16))
tb_entrega_final_ensambles <- tb_entrega_final_ensambles[, .(prob = mean(prob)), by = numero_de_cliente]
GenerarEnviosKaggle(
    tb_prediccion = tb_entrega_final_ensambles,
    envios_optimos = resultados_ensamble_ensambles$envios_optimos,
    tipo_modelo = "final_ensamble_de_ensambles",
    carpeta_salida = PARAM$carpeta_entregables,
    experimento_id = PARAM$experimento
)

#------------------------------------------------------
# Sección 8: Finalización
#------------------------------------------------------
log_info("------------------------------------------------------")
log_info("Script finalizado exitosamente.")
log_info(paste("Fecha y hora de finalización:", format(Sys.time(), "%a %b %d %X %Y")))
log_info("------------------------------------------------------")