tryCatch({  
  # se filtran los meses donde se entrena el modelo final
  log_info("Filtrando datos para el modelo final (según zLineaMuerte).")
  dataset_train_final <- dataset[foto_mes %in% PARAM$train_final$training]

  # Undersampling
  log_info("Haciendo undersampling para el modelo final.")
  set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")
  dataset_train_final[, azar := runif(nrow(dataset_train_final))]
  dataset_train_final[, training := 0L]

  dataset_train_final[
    (azar <= PARAM$train_final$undersampling | clase_ternaria %in% c("BAJA+1", "BAJA+2")),
    training := 1L
  ]

  dataset_train_final[, azar:= NULL] 

  # paso la clase a binaria
  log_info("Creando clase01 para el modelo final.")
  dataset_train_final[,
    clase01 := ifelse(clase_ternaria %in% c("BAJA+2","BAJA+1"), 1L, 0L)
  ]

  # Cargar Hiperparámetros Fijos  
  if (is.null(PARAM$lgbm_z)) {
    stop("No se encontraron los parámetros en PARAM$lgbm_z. Asegúrate de definirlos en main.R.")
  }
  PARAM$train_final$param_mejores <- PARAM$lgbm_z
  log_info("Parámetros finales (zLightGBM):")
  log_info(paste(capture.output(print(PARAM$train_final$param_mejores)), collapse = "\n"))

  # Agregar Canaritos al dataset de entrenamiento final
  log_info(paste("Agregando", PARAM$qcanaritos, "canaritos a dataset_train_final."))
  cols0 <- copy(colnames(dataset_train_final))
  filas <- nrow(dataset_train_final)
  
  for (i in seq_len(PARAM$qcanaritos)) {
    dataset_train_final[, paste0("canarito_", i) := runif(filas)]
  }
  
  cols_canaritos <- copy(setdiff(colnames(dataset_train_final), cols0))
  setcolorder(dataset_train_final, c(cols_canaritos, cols0))
  log_info("Canaritos agregados y reordenados.")

  # Redefinimos campos_buenos para que incluya los canarios
  campos_buenos_z <- setdiff(
    colnames(dataset_train_final),
    PARAM$trainingstrategy$campos_entrenar
  )

  # Semillas para el ensamble final
  set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")
  if(!exists("primos")) primos <- generate_primes(min = 100000, max = 1000000)
  PARAM$train_final$semillas <- sample(primos)[seq( PARAM$train_final$ksemillerio )]
  log_info(paste("Semillas a ser utilizadas en el modelo final para el ensamble:", paste(PARAM$train_final$semillas, collapse = ", ")))
  
  # dejo los datos en formato LightGBM
  log_info("Creando dtrain_final (con canaritos).")
  dtrain_final <- lgb.Dataset(
    data= data.matrix(dataset_train_final[training == 1L, campos_buenos_z, with= FALSE]),
    label= dataset_train_final[training == 1L, clase01],
    free_raw_data= FALSE
  )
  log_info(paste("dtrain_final filas:", nrow(dtrain_final), "columnas:", ncol(dtrain_final)))

  # libero memoria
  rm(dataset_train_final)
  gc()

  # genero los modelitos
  log_info("Generando modelos para hacer ensemble de semillas.")
  dir_modelitos <- file.path(PARAM$experimento_folder, "Modelitos")
  dir.create( dir_modelitos, showWarnings= FALSE)

  param_completo <- copy( PARAM$train_final$param_mejores)

  for( sem in PARAM$train_final$semillas ) {
    arch_modelo <- file.path(dir_modelitos, paste0("mod_", sem, ".txt"))
    if( !file.exists( arch_modelo ) )
    {
      log_info(paste("Entrenando modelo con semilla:", sem))
      param_completo$seed <- sem
      modelito <- lgb.train(
        data= dtrain_final,
        param= param_completo
      )
      lgb.save( modelito, filename= arch_modelo)
      rm(modelito)
      gc()
    }
  }
  log_info("Modelos generados.")
  rm(dtrain_final) # libero memoria
  gc()

  # Scoring
  log_info("Aplicando modelos a datos futuros.")
  dfuture <- dataset[foto_mes %in% PARAM$train_final$future ]
  
  # --- Agregar Canaritos al dataset de predicción ---
  log_info(paste("Agregando", PARAM$qcanaritos, "canaritos a dfuture."))
  filas_future <- nrow(dfuture)
  for (i in seq_len(PARAM$qcanaritos)) {
    dfuture[, paste0("canarito_", i) := runif(filas_future)]
  }

  mfuture <- data.matrix(dfuture[, campos_buenos_z, with= FALSE])

  vpred_acum <- rep(0.0, nrow(dfuture))
  qacumulados <- 0

  for( sem in PARAM$train_final$semillas ) {
    arch_modelo <- file.path(dir_modelitos, paste0("mod_", sem, ".txt"))
    if( file.exists( arch_modelo ) )
    {
      log_info(paste("Aplicando modelo con semilla:", sem))
      modelo_final <- lgb.load(arch_modelo) # leo del disco
      vpred_acum <- vpred_acum + predict(modelo_final, mfuture)
      qacumulados <- qacumulados + 1
    }
  }

  vpred_acum <- vpred_acum / qacumulados  
  log_info("Modelos aplicados.")
  
  # (Liberamos mfuture, ya no se necesita)
  rm(mfuture)
  gc()

  # tabla de prediccion
  log_info("Creando tabla de predicción.")
  # (Nos quedamos solo con las columnas necesarias de dfuture)
  tb_prediccion <- dfuture[, list(numero_de_cliente, foto_mes)]
  tb_prediccion[, prob := vpred_acum ]
  
  # (Liberamos dfuture, que ocupa mucha memoria con canaritos)
  rm(dfuture, vpred_acum)
  gc()

  file_prediccion <- file.path(PARAM$experimento_folder, "prediccion.txt")
  fwrite(tb_prediccion,
    file= file_prediccion,
    sep= "\t"
  )
  log_info(paste("Tabla de predicción guardada en:", file_prediccion))

  # Clasificación
  log_info("Generando archivo para entregar.")
  dir_kaggle <- file.path(PARAM$experimento_folder, PARAM$carpeta_entregables)
  dir.create(dir_kaggle, showWarnings=FALSE)

  setorder(tb_prediccion, -prob)

  # (Definir 'envios' basado en el modo Producción o Evaluación)
  if (isTRUE(PARAM$train_final$produccion)) {
    # MODO PRODUCCIÓN: Usar cortes hardcodeados
    log_info("MODO PRODUCCIÓN: Usando cortes hardcodeados.")
    if (is.null(PARAM$train_final$envios_a_generar) || length(PARAM$train_final$envios_a_generar) == 0) {
      stop("PARAM$train_final$produccion = TRUE pero 'PARAM$train_final$envios_a_generar' está vacío o no existe.")
    }
    envios <- PARAM$train_final$envios_a_generar
    
  } else {
    # MODO EVALUACIÓN: Cargar cortes desde el archivo .rds
    log_info("MODO EVALUACIÓN: Cargando cortes desde 'envios_optimos.rds'.")
    dir_evaluacion <- file.path(PARAM$experimento_folder, PARAM$carpeta_evaluacion)
    ruta_envios_rds <- file.path(dir_evaluacion, "envios_optimos.rds")

    if (!file.exists(ruta_envios_rds)) {
      stop(paste("No se encontró el archivo de envíos óptimos:", ruta_envios_rds,
                 "Asegúrate de que 10_Evaluacion_Ensamble.R se haya ejecutado correctamente."))
    }
    
    # Cargamos el vector de envios generado por el Script 10
    envios <- readRDS(ruta_envios_rds)
  }

  log_info(paste("Envíos finales a generar:", paste(envios, collapse = ", ")))

  for (envio in envios) {
    tb_prediccion[, Predicted := 0L]
    tb_prediccion[1:envio, Predicted := 1L]

    archivo_kaggle <- file.path(dir_kaggle, paste0("KA", PARAM$experimento, "_", envio, ".csv"))

    # grabo el archivo
    fwrite(tb_prediccion[, list(numero_de_cliente, Predicted)],
      file = archivo_kaggle,
      sep = ","
    )
    log_info(paste("Archivo para Kaggle guardado en:", archivo_kaggle))
  }
}, error = function(e) {
  log_error("######################################################")
  log_error("Se ha producido un error fatal en la Sección 11: Modelo Final.")
  log_error(paste("Mensaje de R:", e$message))
  log_error("######################################################")
})