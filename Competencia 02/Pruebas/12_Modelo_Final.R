tryCatch({  
  # se filtran los meses donde se entrena el modelo final
  log_info("Filtrando datos para el modelo final.")
  dataset_train_final <- dataset[foto_mes %in% PARAM$train_final$training]

  # Undersampling, van todos los "BAJA+1" y "BAJA+2" y solo algunos "CONTINIA"
  log_info("Haciendo undersampling para el modelo final.")
  set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")
  dataset_train_final[, azar := runif(nrow(dataset_train_final))]
  dataset_train_final[, training := 0L]

  dataset_train_final[
    (azar <= PARAM$train_final$undersampling | clase_ternaria %in% c("BAJA+1", "BAJA+2")),
    training := 1L
  ]

  dataset_train_final[, azar:= NULL] # elimino la columna azar

  # paso la clase a binaria que tome valores {0,1}  enteros
  #  BAJA+1 y BAJA+2  son  1,   CONTINUA es 0
  #  a partir de ahora ya NO puedo cortar  por prob(BAJA+2) > 1/40
  log_info("Creando clase01 para el modelo final.")
  dataset_train_final[,
    clase01 := ifelse(clase_ternaria %in% c("BAJA+2","BAJA+1"), 1L, 0L)
  ]

  # leo el archivo donde quedaron los hiperparametros optimos
  log_info("Leyendo mejores hiperparámetros de BO_log.txt")

  dir_bayesiana <- file.path(PARAM$experimento_folder, PARAM$carpeta_bayesiana)
  log_bo_file <- file.path(dir_bayesiana, "BO_log.txt")
  
  if (!file.exists(log_bo_file)) {
    log_warn(paste("No se encontró BO_log.txt en", dir_bayesiana, ". Probando en la carpeta raíz."))
    log_bo_file <- file.path(PARAM$experimento_folder, "BO_log.txt")
     if (!file.exists(log_bo_file)) {
        stop("No se encontró BO_log.txt en ninguna ubicación. Asegúrate de que 9_Optimizacion_Bayesiana.R se haya ejecutado.")
     }
  }
  
  tb_BO <-  fread(log_bo_file)
  setorder( tb_BO, -metrica)  # ordeno por metrica descendente
  
  log_info("Mejores hiperparámetros (fila 1 de BO_log.txt):")
  log_info(paste(capture.output(print(tb_BO[1])), collapse = "\n"))

  # en la tabla ademas de los parametros del LightGBM, hay campos de salida
  param_lgbm <- union( names(PARAM$lgbm$param_fijos),  names(PARAM$hipeparametertuning$hs$pars) )
  PARAM$train_final$param_mejores <- as.list( tb_BO[1, param_lgbm, with=FALSE])

  log_info("Ajustando los hiperparámetros al tamaño del dataset final.")
  nrow_dtrain_path <- file.path(PARAM$experimento_folder, PARAM$carpeta_bayesiana, "nrow_dtrain.rds")

  nrow_dtrain_original <- readRDS(nrow_dtrain_path)

  if (is.null(nrow_dtrain_original) || nrow_dtrain_original == 0) {
    stop("nrow_dtrain_original es 0 o NULL. No se puede reescalar min_data_in_leaf.")
  }
  
  original_min_data <- PARAM$train_final$param_mejores$min_data_in_leaf
  
  PARAM$train_final$param_mejores$min_data_in_leaf <- as.integer( round(
      original_min_data * nrow(dataset_train_final[training == 1L]) / nrow_dtrain_original
  ))

  log_info(paste("Original min_data_in_leaf:", tb_BO[1, min_data_in_leaf], "Ajustado min_data_in_leaf:", PARAM$train_final$param_mejores$min_data_in_leaf))
  log_info("Parámetros finales:")
  log_info(paste(capture.output(print(PARAM$train_final$param_mejores)), collapse = "\n"))

  set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")
  PARAM$train_final$semillas <- sample(primos)[seq( PARAM$train_final$ksemillerio )]
  log_info(paste("Semillas a ser utilizadas en el modelo final para el ensamble:", paste(PARAM$train_final$semillas, collapse = ", ")))
  
  # dejo los datos en formato LightGBM
  log_info("Creando dtrain_final.")
  dtrain_final <- lgb.Dataset(
    data= data.matrix(dataset_train_final[training == 1L, campos_buenos, with= FALSE]),
    label= dataset_train_final[training == 1L, clase01],
    free_raw_data= FALSE
  )

  log_info(paste("dtrain_final filas:", nrow(dtrain_final), "columnas:", ncol(dtrain_final)))
  # Libero memoria
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

  # Scoring
  # aplico el modelo a los datos sin clase
  log_info("Aplicando modelos a datos futuros.")
  dfuture <- dataset[foto_mes %in% PARAM$train_final$future ]
  mfuture <- data.matrix(dfuture[, campos_buenos, with= FALSE])

  vpred_acum <- rep(0.0, nrow(dfuture))
  qacumulados <- 0

  for( sem in PARAM$train_final$semillas ) {

    arch_modelo <- file.path(dir_modelitos, paste0("mod_", sem, ".txt"))
    if( file.exists( arch_modelo ) )
    {
      log_info(paste("Aplicando modelo con semilla:", sem))
      modelo_final <- lgb.load(arch_modelo) # leo del disco
      #hago el predict() y acumulo
      vpred_acum <- vpred_acum + predict(modelo_final, mfuture)
      qacumulados <- qacumulados + 1
    }
  }

  vpred_acum <- vpred_acum / qacumulados  # paso a probabildiad
  log_info("Modelos aplicados.")

  # tabla de prediccion, puede ser util para futuros ensembles
  #  ya que le modelo ganador va a ser un ensemble de LightGBMs
  log_info("Creando tabla de predicción.")
  tb_prediccion <- dfuture[, list(numero_de_cliente, foto_mes)]
  tb_prediccion[, prob := vpred_acum ]

  rm(mfuture, dfuture, vpred_acum)
  gc()

  # grabo las probabilidad del modelo
  file_prediccion <- file.path(PARAM$experimento_folder, "prediccion.txt")
  fwrite(tb_prediccion,
    file= file_prediccion,
    sep= "\t"
  )
  log_info(paste("Tabla de predicción guardada en:", file_prediccion))

  # Clasificación
  # genero archivos con los  "envios" mejores
  log_info("Generando archivo para entregar.")
  dir_kaggle <- file.path(PARAM$experimento_folder, PARAM$carpeta_entregables)
  dir.create(dir_kaggle, showWarnings=FALSE)

  # ordeno por probabilidad descendente
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
  log_error("Se ha producido un error fatal en la Sección 12: Modelo Final.")
  log_error(paste("Mensaje de R:", e$message))
  log_error("######################################################")
})