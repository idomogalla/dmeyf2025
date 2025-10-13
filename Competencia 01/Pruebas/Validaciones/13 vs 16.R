#------------------------------------------------------
# Sección 1: Carga de Librerías y Limpieza
#------------------------------------------------------
suppressPackageStartupMessages({
  library("data.table")
  library("lightgbm")
  library("primes")
  library("ggplot2")
})
rm(list = ls())
gc(full = TRUE)

#------------------------------------------------------
# Sección 2: Parámetros de la Comparación
#------------------------------------------------------
PARAM <- list()
PARAM$semilla_primigenia <- 200003
PARAM$qsemillas_tope <- 30
PARAM$training_pct <- 70L
PARAM$envios_cutoff <- 11000
PARAM$archivo_salida <- "resultados_wilcoxon_13_vs_16.txt"
PARAM$dataset <- "~/buckets/b1/datasets/"


# Modelo 1 (Prueba 13): Hiperparámetros de tu optimización
PARAM$lgbm1 <- list(
  boosting_type = "gbdt", objective = "binary", metric = "auc",
  max_depth = -1L, first_metric_only=FALSE, boost_from_average=TRUE, feature_pre_filter=FALSE,
  force_row_wise=TRUE, min_gain_to_split=0.0, min_sum_hessian_in_leaf = 0.001,
  lambda_l1=0.0, lambda_l2=0.0, max_bin = 31, bagging_fraction = 1.0, 
  pos_bagging_fraction = 1.0, neg_bagging_fraction = 1.0, is_unbalance = FALSE,
  scale_pos_weight = 1.0, drop_rate = 0.1, max_drop = 50.0, skip_drop = 0.5, extra_trees = FALSE,
  num_leaves = 904, num_iterations = 1527, learning_rate = 0.0105724, feature_fraction = 0.4846096,
  min_data_in_leaf = 40
)


# Modelo 2 (Prueba 16): Hiperparámetros de una alternativa
PARAM$lgbm2 <- list(
  boosting_type = "gbdt", objective = "binary", metric = "auc",
  max_depth = -1L, first_metric_only=FALSE, boost_from_average=TRUE, feature_pre_filter=FALSE,
  force_row_wise=TRUE, min_gain_to_split=0.0, min_sum_hessian_in_leaf = 0.001,
  lambda_l1=0.0, lambda_l2=0.0, max_bin = 31, bagging_fraction = 1.0, 
  pos_bagging_fraction = 1.0, neg_bagging_fraction = 1.0, is_unbalance = FALSE,
  scale_pos_weight = 1.0, drop_rate = 0.1, max_drop = 50.0, skip_drop = 0.5, extra_trees = FALSE,
  num_leaves = 904, num_iterations = 1797, learning_rate = 0.0156368, feature_fraction = 0.6575693,
  min_data_in_leaf = 50
)

#------------------------------------------------------
# Sección 3: Funciones de Feature Engineering
#------------------------------------------------------

# --- ESTRATEGIA DE FEATURE ENGINEERING PARA EL MODELO 1 (Prueba 13) ---
preparar_datos_modelo1 <- function(d) {
    datos <- copy(d)
    setkey(datos, numero_de_cliente, foto_mes)

    # Columnas a las que se les aplicará el ranking
    cols_a_rankear <- c(
      "mcomisiones_mantenimiento", "Master_Fvencimiento", "Visa_fultimo_cierre", 
      "Master_fultimo_cierre", "mpayroll", "cpayroll_trx"
    ) #

    nuevas_cols_rank <- paste0(cols_a_rankear, "_rank")

    rank_con_cero_fijo <- function(x) {
      resultado <- numeric(length(x))
      idx_pos <- which(x > 0)
      idx_neg <- which(x < 0)
      idx_cero <- which(x == 0)

      if (length(idx_pos) > 0) {
          resultado[idx_pos] <- frankv(x[idx_pos], ties.method = "average") / length(idx_pos)
      }
      if (length(idx_neg) > 0) {
          resultado[idx_neg] <- (frankv(-x[idx_neg], ties.method = "average") / length(idx_neg)) * -1
      }
      if (length(idx_cero) > 0) {
          resultado[idx_cero] <- 0
      }
      return(resultado)
    }

    datos[, (nuevas_cols_rank) := lapply(.SD, rank_con_cero_fijo), by = foto_mes, .SDcols = cols_a_rankear] #
    datos[, (cols_a_rankear) := NULL] #

    cols_a_excluir <- c("numero_de_cliente", "foto_mes", "clase_ternaria")
    cols_con_lag <- setdiff(names(datos), cols_a_excluir)

    # Creación de Lag 1, Delta 1 y Delta Pct 1
    nombres_nuevas_cols_lag <- paste0(cols_con_lag, "_lag1")
    datos[, (nombres_nuevas_cols_lag) := shift(.SD, 1, NA, "lag"), by = numero_de_cliente, .SDcols = cols_con_lag] #

    nombres_nuevas_cols_delta <- paste0(cols_con_lag, "_delta1")
    datos[, (nombres_nuevas_cols_delta) :=  Map(function(col, col_lag) get(col) - get(col_lag), cols_con_lag, nombres_nuevas_cols_lag)] #

    nombres_nuevas_cols_delta_pct <- paste0(cols_con_lag, "_delta_pct1")
    datos[, (nombres_nuevas_cols_delta_pct) := Map(
      function(col, col_lag) {
          lag_val <- get(col_lag)
          curr_val <- get(col)
          delta_pct <- ifelse(is.na(lag_val) | lag_val == 0, NA, (curr_val - lag_val) / abs(lag_val))
          return(delta_pct)
      },
      cols_con_lag, nombres_nuevas_cols_lag
    )] #

    return(datos)
}


# --- ESTRATEGIA DE FEATURE ENGINEERING PARA EL MODELO 2 (Prueba 16) ---

# Función auxiliar necesaria para el Modelo 2
generar_lags_avanzados <- function(dataset, lags_a_crear, cols_a_excluir, id_col) {
  cols_con_lag <- setdiff(names(dataset), cols_a_excluir)
  for (k in lags_a_crear) {
    nombres_lag <- paste0(cols_con_lag, "_lag", k)
    nombres_delta <- paste0(cols_con_lag, "_delta", k)
    dataset[, (nombres_lag) := shift(.SD, k, NA, "lag"), by = id_col, .SDcols = cols_con_lag]
    dataset[, (nombres_delta) := Map(function(col, col_lag) get(col) - get(col_lag), cols_con_lag, nombres_lag)]
    
  }
  return(dataset)
} #

# Función principal para el Modelo 2
preparar_datos_modelo2 <- function(d) {
    datos <- copy(d)
    setkey(datos, numero_de_cliente, foto_mes)
    
    # Columnas a las que se les aplicará el ranking (lista extendida de Prueba 16)
    cols_a_rankear <- c(
      "mrentabilidad", "mrentabilidad_annual", "mcomisiones", "mactivos_margen", "mpasivos_margen",
      "mcuenta_corriente_adicional", "mcuenta_corriente", "mcaja_ahorro", "mcaja_ahorro_adicional", "mcaja_ahorro_dolares",
      "mcuentas_saldo", "mautoservicio", "mtarjeta_visa_consumo", "mtarjeta_master_consumo", "mprestamos_personales", "mprestamos_prendarios",
      "mprestamos_hipotecarios", "mplazo_fijo_dolares", "mplazo_fijo_pesos", "minversion1_pesos", "minversion1_dolares", "minversion2", "mpayroll", "mpayroll2",
      "mcuenta_debitos_automaticos", "mttarjeta_visa_debitos_automaticos", "mttarjeta_master_debitos_automaticos", "mpagodeservicios", "mpagomiscuentas",
      "mcajeros_propios_descuentos", "mtarjeta_visa_descuentos", "mtarjeta_master_descuentos", "mcomisiones_mantenimiento", "mcomisiones_otras", "mforex_buy",
      "mforex_sell", "mtransferencias_recibidas", "mtransferencias_emitidas", "mextraccion_autoservicio", "mcheques_depositados", "mcheques_emitidos", 
      "mcheques_depositados_rechazados", "mcheques_emitidos_rechazados", "matm", "matm_other", "Master_mfinanciacion_limite",
      "Master_msaldototal", "Master_msaldopesos", "Master_msaldodolares", "Master_mconsumospesos", "Master_mconsumosdolares", "Master_mlimitecompra", "Master_madelantopesos", "Master_madelantodolares",
      "Master_mpagado", "Master_mpagospesos", "Master_mpagosdolares", "Master_mconsumototal", "Master_mpagominimo", "Visa_mfinanciacion_limite", 
      "Visa_msaldototal", "Visa_msaldopesos", "Visa_msaldodolares", "Visa_mconsumospesos", "Visa_mconsumosdolares", "Visa_mlimitecompra", "Visa_madelantopesos", "Visa_madelantodolares",
      "Visa_mpagado", "Visa_mpagospesos", "Visa_mpagosdolares", "Visa_mconsumototal", "Visa_mpagominimo"
    ) #
  
    nuevas_cols_rank <- paste0(cols_a_rankear, "_rank")
  
    rank_con_cero_fijo <- function(x) {
      resultado <- numeric(length(x))
      idx_pos <- which(x > 0)
      idx_neg <- which(x < 0)
      idx_cero <- which(x == 0)
    
      if (length(idx_pos) > 0) {
        resultado[idx_pos] <- frankv(x[idx_pos], ties.method = "average") / length(idx_pos)
      }
      if (length(idx_neg) > 0) {
        resultado[idx_neg] <- (frankv(-x[idx_neg], ties.method = "average") / length(idx_neg)) * -1
      }
      if (length(idx_cero) > 0) {
        resultado[idx_cero] <- 0
      }
      return(resultado)
    }
  
    datos[, (nuevas_cols_rank) := lapply(.SD, rank_con_cero_fijo), by = foto_mes, .SDcols = cols_a_rankear] #
    datos[, (cols_a_rankear) := NULL] #

    # Generación de Lags 1 y 2
    lags_deseados <- c(1, 2) #
    columnas_a_ignorar <- c("numero_de_cliente", "foto_mes", "clase_ternaria") #
    id_cliente <- "numero_de_cliente" #

    datos <- generar_lags_avanzados(
      dataset = datos,
      lags_a_crear = lags_deseados,
      cols_a_excluir = columnas_a_ignorar,
      id_col = id_cliente
    ) #
    
    return(datos)
}

#------------------------------------------------------
# Sección 4: Carga de Datos y Funciones de Comparación
#------------------------------------------------------
dataset_full <- fread(PARAM$dataset)
dataset_full[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]
MES_EVALUACION <- 202104
dataset <- dataset_full[foto_mes >= (MES_EVALUACION - 100) & foto_mes <= MES_EVALUACION]
rm(dataset_full); gc()

particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)
  bloque <- unlist(mapply(function(x, y) rep(y, x), division, seq(from = start, length.out = length(division))))
  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N], by = agrupa]
}

DosModelosLGBMEstimarGanancia <- function(semilla, training_pct, param_lgbm1, param_lgbm2) {
  dataset_mes <- dataset[foto_mes == MES_EVALUACION]
  particionar(dataset_mes, division = c(training_pct, 100L - training_pct), agrupa = "clase_ternaria", seed = semilla)
  clientes_train <- dataset_mes[fold == 1, numero_de_cliente]
  clientes_test <- dataset_mes[fold == 2, numero_de_cliente]
  
  # Modelo 1
  datos_m1_full <- preparar_datos_modelo1(dataset)
  train_m1 <- datos_m1_full[numero_de_cliente %in% clientes_train & foto_mes == MES_EVALUACION]
  test_m1  <- datos_m1_full[numero_de_cliente %in% clientes_test & foto_mes == MES_EVALUACION]
  campos_buenos_m1 <- setdiff(colnames(train_m1), c("clase_ternaria", "clase01", "fold"))
  dtrain1 <- lgb.Dataset(data = data.matrix(train_m1[, ..campos_buenos_m1]), label = train_m1$clase01)
  param_lgbm1$seed <- semilla
  modelo1 <- lgb.train(params = param_lgbm1, data = dtrain1, verbose = -1)
  prediccion1 <- predict(modelo1, data.matrix(test_m1[, ..campos_buenos_m1]))
  tb_pred1 <- test_m1[, .(clase_ternaria)]; tb_pred1[, prob := prediccion1]
  setorder(tb_pred1, -prob); tb_pred1[1:PARAM$envios_cutoff, Predicted := 1L]
  ganancia_test1 <- tb_pred1[Predicted == 1, sum(ifelse(clase_ternaria == "BAJA+2", 780000, -20000))]
  ganancia_test_normalizada1 <- ganancia_test1 / ((100 - training_pct) / 100)

  # Modelo 2
  datos_m2_full <- preparar_datos_modelo2(dataset)
  train_m2 <- datos_m2_full[numero_de_cliente %in% clientes_train & foto_mes == MES_EVALUACION]
  test_m2  <- datos_m2_full[numero_de_cliente %in% clientes_test & foto_mes == MES_EVALUACION]
  campos_buenos_m2 <- setdiff(colnames(train_m2), c("clase_ternaria", "clase01", "fold"))
  dtrain2 <- lgb.Dataset(data = data.matrix(train_m2[, ..campos_buenos_m2]), label = train_m2$clase01)
  param_lgbm2$seed <- semilla
  modelo2 <- lgb.train(params = param_lgbm2, data = dtrain2, verbose = -1)
  prediccion2 <- predict(modelo2, data.matrix(test_m2[, ..campos_buenos_m2]))
  tb_pred2 <- test_m2[, .(clase_ternaria)]; tb_pred2[, prob := prediccion2]
  setorder(tb_pred2, -prob); tb_pred2[1:PARAM$envios_cutoff, Predicted := 1L]
  ganancia_test2 <- tb_pred2[Predicted == 1, sum(ifelse(clase_ternaria == "BAJA+2", 780000, -20000))]
  ganancia_test_normalizada2 <- ganancia_test2 / ((100 - training_pct) / 100)
  
  return(list("ganancia1" = ganancia_test_normalizada1, "ganancia2" = ganancia_test_normalizada2))
}

CompararModelosLGBM <- function(qsemillas_tope, training_pct, param_lgbm1, param_lgbm2) {
  primos <- generate_primes(min = 100000, max = 1000000)
  set.seed(PARAM$semilla_primigenia)
  semillas <- sample(primos, qsemillas_tope)
  pvalue <- 1.0; isem <- 1; vgan1 <- c(); vgan2 <- c()
  while (isem <= qsemillas_tope & (is.na(pvalue) || pvalue > 0.05)) {
    res <- DosModelosLGBMEstimarGanancia(semillas[isem], training_pct, param_lgbm1, param_lgbm2)
    vgan1 <- c(vgan1, res$ganancia1); vgan2 <- c(vgan2, res$ganancia2)
    if (length(vgan1) > 1) { 
        # Añadido un chequeo para evitar que wilcox.test falle si las varianzas son cero
        if(sd(vgan1 - vgan2) > 0) {
            wt <- wilcox.test(vgan1, vgan2, paired = TRUE)
            pvalue <- wt$p.value 
        } else {
            pvalue <- 1.0
        }
    } else { 
      pvalue <- 1.0 
    }
    cat(sprintf("Iter: %03d | Semilla: %d | Ganancia M1: %12.0f | Ganancia M2: %12.0f | p-value: %.6f\n",
      isem, semillas[isem], res$ganancia1, res$ganancia2, pvalue))
    flush.console()
    isem <- isem + 1
  }
  out <- 0
  if (!is.na(pvalue) & pvalue < 0.05 & mean(vgan1) > mean(vgan2)) out <- 1
  if (!is.na(pvalue) & pvalue < 0.05 & mean(vgan1) < mean(vgan2)) out <- 2
  return(list("resultado" = out, "qsemillas" = length(vgan1), "p_value_final" = pvalue,
              "ganancia_media_m1" = mean(vgan1), "ganancia_media_m2" = mean(vgan2)))
}

#------------------------------------------------------
# Sección 5: Ejecución y Guardado de Resultados
#------------------------------------------------------

if (file.exists(PARAM$archivo_salida)) {
  file.remove(PARAM$archivo_salida)
}

sink(PARAM$archivo_salida, append = TRUE, split = TRUE)

cat("--- INICIO DEL EXPERIMENTO DE COMPARACIÓN ---\n")
cat("Fecha:", as.character(Sys.time()), "\n\n")

comparacion <- CompararModelosLGBM(
  PARAM$qsemillas_tope,
  PARAM$training_pct,
  PARAM$lgbm1,
  PARAM$lgbm2
)

cat("\n\n--- CONCLUSIONES DEL TEST DE WILCOXON ---\n")

resultado_texto <- if (comparacion$resultado == 1) {
  "La ESTRATEGIA 1 (Prueba 13) es estadísticamente MEJOR que la ESTRATEGIA 2 (Prueba 16)."
} else if (comparacion$resultado == 2) {
  "La ESTRATEGIA 2 (Prueba 16) es estadísticamente MEJOR que la ESTRATEGIA 1 (Prueba 13)."
} else {
  sprintf("NO se encontró una diferencia estadísticamente significativa con %d semillas.", comparacion$qsemillas)
}

cat(resultado_texto, "\n")
cat(sprintf("Semillas utilizadas: %d de %d\n", comparacion$qsemillas, PARAM$qsemillas_tope))
cat(sprintf("P-value final: %.6f\n", comparacion$p_value_final))
cat(sprintf("Ganancia promedio Estrategia 1 (Prueba 13): %12.0f\n", comparacion$ganancia_media_m1))
cat(sprintf("Ganancia promedio Estrategia 2 (Prueba 16): %12.0f\n", comparacion$ganancia_media_m2))

cat("-----------------------------------------\n")
cat("--- FIN DEL EXPERIMENTO ---\n")

sink()

message("El experimento ha finalizado. Los resultados completos se han guardado en: '", PARAM$archivo_salida, "'")