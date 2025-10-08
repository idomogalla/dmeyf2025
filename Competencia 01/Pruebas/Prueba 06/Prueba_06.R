#!/usr/bin/env Rscript
#------------------------------------------------------
# Sección 1: Carga de Librerías
#------------------------------------------------------
if (!require("logger"))
  install.packages("logger")
library("logger")

suppressPackageStartupMessages({
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
})
#------------------------------------------------------
# Sección 2: Configuración Inicial y Parámetros
#------------------------------------------------------
rm(list = ls(all.names = TRUE))
gc(full = TRUE, verbose = FALSE)

PARAM <- list()
PARAM$experimento <- "expC01_Prueba06"
PARAM$dir_experimento <- paste0("~/buckets/b1/exp/", PARAM$experimento)
PARAM$dir_dataset <- "~/buckets/b1/datasets/"
PARAM$carpeta_logs <- "logs/"
PARAM$carpeta_bayesiana <- "Archivos Bayesiana/"
PARAM$carpeta_kaggle <- "Kaggle/"
PARAM$carpeta_kaggle_ensamble <- "Kaggle_Promediado/"
PARAM$carpeta_graficos <- "Plots/"
PARAM$carpeta_entregables <- "Entregables/"

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
  min_data_in_leaf = 10000,
  early_stopping_round = 100
)
# Bordes de hiperparámetros para BO
PARAM$hypeparametertuning$hs <- makeParamSet(
  makeIntegerParam("num_leaves", lower = 10L, upper = 2048L),
  
  makeIntegerParam("num_iterations", lower = 50L, upper = 3000L),
  makeNumericParam("learning_rate", lower = 0.01, upper = 0.5),
  
  makeNumericParam("feature_fraction", lower = 0.1, upper = 1.0),
  
  makeIntegerParam("min_data_in_leaf", lower = 1L, upper = 10000L)
)
PARAM$hyperparametertuning$iteraciones <- 80

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

log_info("------------------------------------------------------")
log_info(paste("Inicio del script. Experimento:", PARAM$experimento))
log_info(paste("El log se guardará en:", log_file))
log_info("------------------------------------------------------")

#------------------------------------------------------
# Sección 3: Funciones Auxiliares
#------------------------------------------------------
log_info("Iniciando Sección 3: Cargando funciones auxiliares")
particionar <- function(data,
                        division,
                        agrupa = "",
                        campo = "fold",
                        start = 1,
                        seed = NA) {
  if (!is.na(seed))
    set.seed(seed, "L'Ecuyer-CMRG")
  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(
    from = start, length.out = length(division)
  )))
  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N], by = agrupa]
}

realidad_inicializar <- function(pfuture, pparam) {
  drealidad <- pfuture[, list(numero_de_cliente, foto_mes, clase_ternaria)]
  particionar(
    drealidad,
    division = c(3, 7),
    agrupa = "clase_ternaria",
    seed = pparam$semilla_kaggle
  )
  return(drealidad)
}

realidad_evaluar <- function(prealidad, pprediccion) {
  prealidad[pprediccion, on = c("numero_de_cliente", "foto_mes"), predicted := i.Predicted]
  tbl <- prealidad[, list("qty" = .N), list(fold, predicted, clase_ternaria)]
  res <- list()
  res$public <- tbl[fold == 1 &
                      predicted == 1L, sum(qty * ifelse(clase_ternaria == "BAJA+2", 780000, -20000))] / 0.3
  res$private <- tbl[fold == 2 &
                       predicted == 1L, sum(qty * ifelse(clase_ternaria == "BAJA+2", 780000, -20000))] / 0.7
  res$total <- tbl[predicted == 1L, sum(qty * ifelse(clase_ternaria == "BAJA+2", 780000, -20000))]
  prealidad[, predicted := NULL]
  return(res)
}

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
  # Agrego columnas
  dataset[, `:=`(
    # Suma de consumos de tarjetas
    mtarjetas_consumo = round(rowSums(.SD[, .(mtarjeta_visa_consumo, mtarjeta_master_consumo)], na.rm = TRUE), 1),
    # Suma de beneficios/descuentos
    mbeneficios = round(rowSums(.SD[, .(
      mcajeros_propios_descuentos,
      mtarjeta_visa_descuentos,
      mtarjeta_master_descuentos
    )], na.rm = TRUE), 1),
    # Suma de ingresos
    mingresos = round(rowSums(.SD[, .(mpayroll, mpayroll2, mtransferencias_recibidas)], na.rm = TRUE), 1),
    # Diferencia: límite menos consumo para MasterCard
    diff_master_compra = round(Master_mlimitecompra - Master_mconsumospesos, 2),
    # Diferencia: límite menos consumo para Visa
    diff_visa_compra = round(Visa_mlimitecompra - Visa_mconsumospesos, 2)
  )]
  
  dataset[, `:=`(
    # Diferencia: consumo total menos comisiones
    diff_comisiones_consumo = round(mtarjetas_consumo - mcomisiones_mantenimiento, 2),
    # Diferencia: beneficios totales menos comisiones
    diff_comisiones_beneficios = round(mbeneficios - mcomisiones_mantenimiento, 2)
  )]
  
  # Columnas a las que se les aplicará el ranking
  cols_a_rankear <- c(
    "mpasivos_margen",
    "Master_mlimitecompra",
    "Visa_mlimitecompra",
    "mpayroll",
    "mcuenta_corriente",
    "mcaja_ahorro",
    "mtarjetas_consumo",
    "mingresos",
    "Visa_mpagospesos"
  )
  
  # Nombres para las nuevas columnas de ranking
  nuevas_cols_rank <- paste0(cols_a_rankear, "_rank")
  
  # Funcion para ranking con cero fijo
  rank_con_cero_fijo <- function(x) {
    # Vector para guardar los resultados
    resultado <- numeric(length(x))
    
    # Índices para cada caso
    idx_pos <- which(x > 0)
    idx_neg <- which(x < 0)
    idx_cero <- which(x == 0)
    
    # 1. Ranking para valores positivos (> 0)
    if (length(idx_pos) > 0) {
      # Se divide por la cantidad de positivos para obtener el percentil (0 a 1)
      resultado[idx_pos] <- frankv(x[idx_pos], ties.method = "average") / length(idx_pos)
    }
    
    # 2. Ranking para valores negativos (< 0)
    if (length(idx_neg) > 0) {
      # Se calcula el percentil para los negativos y se multiplica por -1 (-1 a 0)
      resultado[idx_neg] <- (frankv(-x[idx_neg], ties.method = "average") / length(idx_neg)) * -1
    }
    
    # 3. Para los valores que son cero, el ranking es cero
    if (length(idx_cero) > 0) {
      resultado[idx_cero] <- 0
    }
    
    return(resultado)
  }
  
  # Aplicar la función a todas las columnas especificadas, agrupando por mes
  dataset[, (nuevas_cols_rank) := lapply(.SD, rank_con_cero_fijo), by = foto_mes, .SDcols = cols_a_rankear]
  # Elimino las columnas que rankee
  dataset[, (cols_a_rankear) := NULL]
  
  # Genero columnas Lags y Delta Lags de orden 1 y 2
  log_info("Inicio de Feature Lags")
  cols_a_excluir <- c("numero_de_cliente", "foto_mes", "clase_ternaria")
  cols_con_lag <- setdiff(names(dataset), cols_a_excluir)
  for (k in 1:2) {
    dataset[, paste0(cols_con_lag, "_lag", k) := lapply(.SD, shift, n = k), 
            by = numero_de_cliente, .SDcols = cols_con_lag]
    
    dataset[, paste0(cols_con_lag, "_delta", k) := Map(`-`, .SD, mget(paste0(cols_con_lag, "_lag", k))),
            .SDcols = cols_con_lag]
  }
  log_info("Features de lag y delta generadas.")

  dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

  # Armo el dataset de train
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
  # Mensaje de error mejorado
  log_error("######################################################")
  log_error("Se ha producido un error fatal en la ejecución.")
  log_error(
    "Revisa el último mensaje 'INFO' en el log para identificar la sección donde ocurrió el fallo."
  )
  log_error(paste("Mensaje de R:", e$message))
  log_error("######################################################")
  quit(status = 1) # Detiene el script con un código de error
})

tryCatch({
  #------------------------------------------------------
  # Sección 5: Optimización Bayesiana
  #------------------------------------------------------
  log_info("Iniciando Sección 5: Optimización Bayesiana de Hiperparámetros.")
  EstimarGanancia_AUC_lightgbm <- function(x) {
    # x pisa (o agrega) a param_fijos
    param_completo <- modifyList(PARAM$lgbm$param_fijos, x)
    # Entreno LightGBM
    modelocv <- lgb.cv(
      data = dtrain,
      nfold = PARAM$hyperparametertuning$xval_folds,
      stratified = TRUE,
      param = param_completo
    )
    # obtengo la ganancia
    AUC <- modelocv$best_score
    # hago espacio en la memoria
    rm(modelocv)
    gc(full = TRUE, verbose = FALSE)
    log_info(paste(
      "Iteración BO -> AUC:",
      format(AUC, digits = 6),
      "|",
      format(Sys.time(), "%a %b %d %X %Y")
    ))
    return(AUC)
  }
  
  dir.create(PARAM$carpeta_bayesiana, showWarnings = FALSE)
  kbayesiana <- paste0(PARAM$carpeta_bayesiana, "bayesiana.RDATA")
  funcion_optimizar <- EstimarGanancia_AUC_lightgbm # la funcion que voy a maximizar
  configureMlr(show.learner.output = FALSE)
  obj.fun <- makeSingleObjectiveFunction(
    fn = funcion_optimizar,
    minimize = FALSE,
    noisy = TRUE,
    par.set = PARAM$hypeparametertuning$hs,
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
      "y",
      "dob",
      "eol",
      "error.message",
      "exec.time",
      "ei",
      "error.model",
      "train.time",
      "prop.type",
      "propose.time",
      "se",
      "mean",
      "iter"
    )
  ), with = FALSE]
  PARAM$out$lgbm$y <- tb_bayesiana[1, y]
  write_yaml(PARAM, file = paste0(PARAM$carpeta_bayesiana, "PARAM.yml"))
  
  log_info("Mejores hiperparámetros encontrados:")
  log_info(paste(capture.output(
    print(PARAM$out$lgbm$mejores_hiperparametros)
  ), collapse = "\n"))
  log_info(paste("Mejor AUC (y):", PARAM$out$lgbm$y))
  
  #------------------------------------------------------
  # Funciones para secciones 6 y 7
  #------------------------------------------------------
  EvaluarYGraficar <- function(tb_prediccion,
                               drealidad,
                               PARAM,
                               tipo_modelo,
                               carpeta_salida_kaggle) {
    log_info(paste0("Iniciando evaluación y graficación para el modelo: ",tipo_modelo))
    dir.create(carpeta_salida_kaggle, showWarnings = FALSE)
    resultados <- data.table()
    
    # Ordenar las predicciones de mayor a menor probabilidad
    setorder(tb_prediccion, -prob)
    
    # --- Bucle de evaluación por cortes ---
    for (envios in PARAM$cortes) {
      tb_prediccion[, Predicted := 0L]
      tb_prediccion[1:envios, Predicted := 1L] # marco los envios
      
      fwrite(
        tb_prediccion[, list(numero_de_cliente, Predicted)],
        file = paste0(carpeta_salida_kaggle, PARAM$experimento,"_",envios,".csv"),
        sep = ","
      )
      
      res <- realidad_evaluar(drealidad, tb_prediccion)
      
      resultados <- rbind(
        resultados,
        data.table(
          clientes = envios,
          ganancia_total = res$total,
          ganancia_public = res$public,
          ganancia_private = res$private
        )
      )

      options(scipen = 999)
      log_info(
        sprintf(
          "Envios=%-5d | TOTAL=%11.0f | Public=%11.0f | Private=%11.0f",
          envios,
          res$total,
          res$public,
          res$private
        )
      )
    }
    
    # --- Guardar resultados en CSV ---
    archivo_resultados_csv <- paste0(PARAM$carpeta_bayesiana,"envios_",tipo_modelo,"_",PARAM$experimento,".csv")
    resultados_para_csv <- copy(resultados)
    setnames(
      resultados_para_csv,
      old = c(
        "clientes",
        "ganancia_total",
        "ganancia_public",
        "ganancia_private"
      ),
      new = c("ENVIOS", "TOTAL", "PUBLIC", "PRIVATE")
    )
    fwrite(resultados_para_csv, file = archivo_resultados_csv)
    log_info(paste0("Tabla de resultados para [",tipo_modelo,"] guardada en: ",archivo_resultados_csv))
    
    # --- Encontrar envíos con ganancia máxima ---
    max_ganancia_valor <- max(resultados$ganancia_total)
    envios_max_total <- resultados[ganancia_total == max_ganancia_valor, clientes]
    log_info(paste0("Envíos óptimos para [",tipo_modelo,"]: ",paste(envios_max_total, collapse = ", ")))
    
    # --- Preparar datos para el gráfico ---
    resultados_long <- melt(
      resultados,
      id.vars = "clientes",
      measure.vars = c("ganancia_total", "ganancia_public", "ganancia_private"),
      variable.name = "tipo",
      value.name = "ganancia"
    )
    
    maximos <- resultados_long[, .SD[which.max(ganancia)], by = tipo]
    
    etiquetas <- paste0(
      maximos$tipo,
      " (envíos = ",
      maximos$clientes,
      ", máx = ",
      format(maximos$ganancia, big.mark = ","),
      ")"
    )
    names(etiquetas) <- maximos$tipo
    
    # --- Crear y guardar gráfico ---
    p <- ggplot(resultados_long, aes(x = clientes, y = ganancia, color = tipo)) +
      geom_line(linewidth = 1) +
      geom_point(data = maximos,
                 aes(x = clientes, y = ganancia, color = tipo),
                 size = 3) +
      labs(
        title = paste0("Curvas de Ganancia (Modelo ",stringr::str_to_title(tipo_modelo)," - ",PARAM$experimento,")"),
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
      theme(plot.margin = margin(10, 10, 10, 10),
            legend.position = "bottom") +
      guides(color = guide_legend(nrow = 3, byrow = TRUE))
    
    ggsave(paste0(PARAM$carpeta_graficos,"curvas_",tipo_modelo,"_",PARAM$experimento,".png"),
      plot = p,
      width = 10,
      height = 6
    )
    log_info(paste0("Gráfico de curvas de ganancia (", tipo_modelo, ") guardado."))
    
    return(list(envios_optimos = envios_max_total))
  }

  GenerarEnviosKaggle <- function(tb_prediccion,
                                envios_optimos,
                                tipo_modelo,
                                carpeta_salida,
                                experimento_id) {

    log_info(paste0("Iniciando generación de envíos para Kaggle del modelo: '", tipo_modelo, "'"))

    # Se combinan los envíos óptimos con su versión +500 y se eliminan duplicados
    envios_a_generar <- unique(c(envios_optimos, envios_optimos + 500))

    log_info(paste0("Se generarán archivos para los siguientes envíos: ", paste(envios_a_generar, collapse = ", ")))

    # Asegurar que la tabla de predicción esté ordenada por probabilidad descendente
    setorder(tb_prediccion, -prob)

    # 3. Bucle para generar cada archivo
    for (envios in envios_a_generar) {
      # Marcar los N primeros clientes como 'Predicted = 1'
      tb_prediccion[, Predicted := 0L]
      tb_prediccion[1:envios, Predicted := 1L]

      # Definir el nombre del archivo de salida
      archivo_kaggle <- paste0(
        carpeta_salida,
        experimento_id, "_",
        tipo_modelo, "_",
        envios, ".csv"
      )

      # Grabar el archivo .csv
      fwrite(tb_prediccion[, list(numero_de_cliente, Predicted)],
            file = archivo_kaggle,
            sep = ",")

      log_info(paste0("Archivo generado: ", archivo_kaggle))
    }

    log_info(paste0("Generación de envíos para '", tipo_modelo, "' finalizada."))
  }
  
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
  
  # Entreno LGBM
  modelo_final <- lgb.train(data = dtrain_final, param = param_normalizado)
  log_info("Entrenamiento del modelo final completado.")

  log_info("Análisis de CV para el modelo final iniciado.")
  modelocv_unico <- lgb.cv(
    data = dtrain_final,
    nfold = PARAM$hyperparametertuning$xval_folds, # Reutilizamos los pliegues de la optimización
    stratified = TRUE,
    param = param_normalizado
  )
  auc_modelo_unico <- modelocv_unico$best_score
  log_info(paste("AUC (CV) del Modelo Único:", format(auc_modelo_unico, digits = 6)))
  
  lgb.save(modelo_final, paste0(PARAM$carpeta_bayesiana, "modelo.txt"))
  tb_importancia <- as.data.table(lgb.importance(modelo_final))
  fwrite(
    tb_importancia,
    file = paste0(PARAM$carpeta_bayesiana, "importancia.txt"),
    sep = "\t"
  )
  
  log_info("Generando predicciones para el Modelo Único...")
  dfuture <- dataset[foto_mes %in% PARAM$future]
  # Aplico el modelo a los datos nuevos (dfuture)
  prediccion_unica <- predict(modelo_final, data.matrix(dfuture[, campos_buenos, with = FALSE]))
  # Tabla de predicción
  tb_prediccion_unico <- dfuture[, list(numero_de_cliente, foto_mes)]
  tb_prediccion_unico[, prob := prediccion_unica]
  
  # Dataset realidad
  drealidad <- realidad_inicializar(dfuture, PARAM)
  
  # Usamos la función para generar los mejores envios
  resultados_unico <- EvaluarYGraficar(
    tb_prediccion = tb_prediccion_unico,
    drealidad = drealidad,
    PARAM = PARAM,
    tipo_modelo = "unico",
    carpeta_salida_kaggle = PARAM$carpeta_kaggle
  )
  log_info(paste0("Envíos óptimos del modelo único: ",paste(resultados_unico$envios_optimos, collapse = ", ")))
  
  # Creo los archivos para entrega
  log_info("Generando los archivos de entrega para el modelo único.")
  dataset_train_final <- dataset[foto_mes %in% PARAM$train_final_kaggle]
  dtrain_final_kaggle <- lgb.Dataset(data = data.matrix(dataset_train_final[, campos_buenos, with = FALSE]), 
                                    label = dataset_train_final[, clase01])

  # Usamos los mismos hiperparámetros ya optimizados
  modelo_final_kaggle <- lgb.train(data = dtrain_final_kaggle, param = param_normalizado)
  log_info("Modelo final para Kaggle re-entrenado con todos los datos.")

  # Generar las predicciones finales sobre el dataset de entrega
  dfuture_entrega <- dataset[foto_mes %in% PARAM$entrega_kaggle]
  prediccion_final <- predict(modelo_final_kaggle, data.matrix(dfuture_entrega[, campos_buenos, with = FALSE]))

  # Crear la tabla de predicción final
  tb_prediccion_final <- dfuture_entrega[, list(numero_de_cliente, foto_mes)]
  tb_prediccion_final[, prob := prediccion_final]

  # Se usa la PREDICCIÓN del modelo final, pero los CORTES ÓPTIMOS de la fase de evaluación
  GenerarEnviosKaggle(
    tb_prediccion = tb_prediccion_final,
    envios_optimos = envios_optimos_encontrados, # <--- Usamos el resultado de la Fase 1
    tipo_modelo = "final_unico",                  # <--- Un nombre de modelo descriptivo
    carpeta_salida = PARAM$carpeta_kaggle,
    experimento_id = PARAM$experimento
  )

  #------------------------------------------------------
  # Sección 7: Entrenamiento y Predicción (Ensemble)
  #------------------------------------------------------
  log_info("Iniciando Sección 7: Entrenamiento del Ensamble de Modelos.")
  semillas <- c(200003, 300007, 400009, 500009, 600011)
  lista_predicciones <- list()
  aucs_ensamble <- c()
  
  for (semilla_actual in semillas) {
    log_info(paste0("Entrenando modelo del ensamble con semilla: ",semilla_actual))
    param_normalizado$seed <- semilla_actual

    modelocv_ensamble <- lgb.cv(
        data = dtrain_final,
        nfold = PARAM$hyperparametertuning$xval_folds,
        stratified = TRUE,
        param = param_normalizado
      )
    auc_actual <- modelocv_ensamble$best_score
    aucs_ensamble <- c(aucs_ensamble, auc_actual) # Guardamos el AUC
    log_info(paste("-> AUC (CV) para semilla", semilla_actual, ":", format(auc_actual, digits = 6)))

    modelo <- lgb.train(data = dtrain_final, param = param_normalizado)
    prediccion_individual <- predict(modelo, data.matrix(dfuture[, campos_buenos, with = FALSE]))

    # Usamos una tabla temporal para cada predicción
    tb_pred_individual <- dfuture[, list(numero_de_cliente, foto_mes)]
    tb_pred_individual[, prob := prediccion_individual]
    lista_predicciones[[as.character(semilla_actual)]] <- tb_pred_individual
  }

  auc_promedio_ensamble <- mean(aucs_ensamble)
  log_info(paste("AUC Promedio (CV) del Ensemble:", format(auc_promedio_ensamble, digits = 6)))
  
  log_info("Creando el ensamble final promediando probabilidades.")
  predicciones_todas <- rbindlist(lista_predicciones)
  tb_prediccion_ensamble <- predicciones_todas[, .(prob = mean(prob)), by = .(numero_de_cliente, foto_mes)]
  
  # Usamos LA MISMA función para el ensamble
  resultados_ensamble <- EvaluarYGraficar(
    tb_prediccion = tb_prediccion_ensamble,
    drealidad = drealidad,
    PARAM = PARAM,
    tipo_modelo = "ensamble",
    carpeta_salida_kaggle = PARAM$carpeta_kaggle_ensamble
  )
  log_info(paste0("Envíos óptimos del ensamble:",paste(resultados_ensamble$envios_optimos, collapse = ", ")))

  log_info("Generando los archivos finales para entregar, para el ensamble.")
  # Definir el dataset de entrenamiento final para Kaggle (se usa para todos los modelos del ensamble)
  dataset_train_final_ens <- dataset[foto_mes %in% PARAM$train_final_kaggle]
  dtrain_final_kaggle_ens <- lgb.Dataset(
    data = data.matrix(dataset_train_final_ens[, campos_buenos, with = FALSE]),
    label = dataset_train_final_ens[, clase01]
  )
  log_info("Dataset final de Kaggle para el ensamble preparado.")

  # Re-entrenar cada modelo del ensamble con el dataset final y predecir
  lista_predicciones_final <- list()
  dfuture_entrega <- dataset[foto_mes %in% PARAM$entrega_kaggle] # Dataset de entrega final

  for (semilla_actual in semillas) {
    log_info(paste0("Re-entrenando modelo final del ensamble con semilla: ", semilla_actual))
    param_normalizado$seed <- semilla_actual

    # Entrenar el modelo con el dataset completo
    modelo_final_ens <- lgb.train(data = dtrain_final_kaggle_ens, param = param_normalizado)

    # Predecir sobre los datos de entrega
    prediccion_final_individual <- predict(modelo_final_ens, data.matrix(dfuture_entrega[, campos_buenos, with = FALSE]))

    # Guardar la predicción
    tb_pred_final_individual <- dfuture_entrega[, list(numero_de_cliente, foto_mes)]
    tb_pred_final_individual[, prob := prediccion_final_individual]
    lista_predicciones_final[[as.character(semilla_actual)]] <- tb_pred_final_individual
  }

  # Consolidar las predicciones del ensamble final promediando
  log_info("Creando el ensamble final para la entrega promediando probabilidades.")
  predicciones_finales_todas <- rbindlist(lista_predicciones_final)
  tb_prediccion_final_ensamble <- predicciones_finales_todas[, .(prob = mean(prob)), by = .(numero_de_cliente, foto_mes)]

  # Generar los archivos de envío para Kaggle con los cortes óptimos encontrados previamente
  GenerarEnviosKaggle(
    tb_prediccion = tb_prediccion_final_ensamble,
    envios_optimos = resultados_ensamble$envios_optimos, # <-- Usamos el resultado de la evaluación
    tipo_modelo = "final_ensamble",
    carpeta_salida = PARAM$carpeta_kaggle_ensamble,
    experimento_id = PARAM$experimento
  )

  log_info("Archivos de entrega para el ensamble generados correctamente.")

}, error = function(e) {
  # Mensaje de error mejorado
  log_error("######################################################")
  log_error("Se ha producido un error fatal en la ejecución.")
  log_error(
    "Revisa el último mensaje 'INFO' en el log para identificar la sección donde ocurrió el fallo."
  )
  log_error(paste("Mensaje de R:", e$message))
  log_error("######################################################")
  quit(status = 1) # Detiene el script con un código de error
})
#------------------------------------------------------
# Sección 8: Finalización
#------------------------------------------------------
log_info("------------------------------------------------------")
log_info("Script finalizado exitosamente.")
log_info(paste("Fecha y hora de finalización:", format(Sys.time(), "%a %b %d %X %Y")))
log_info("------------------------------------------------------")