log_info("Inicio Modelo final")
trytryCatch({
  # se filtran los meses donde se entrena el modelo final
  log_info("Filtrando datos para el modelo final")
  dataset_train_final <- dataset[foto_mes %in% PARAM$train_final$training]

  # Undersampling, van todos los "BAJA+1" y "BAJA+2" y solo algunos "CONTINIA"
  log_info("Haciendo undersampling para el modelo final")
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
  log_info("Creando clase01 para el modelo final")
  dataset_train_final[,
    clase01 := ifelse(clase_ternaria %in% c("BAJA+2","BAJA+1"), 1L, 0L)
  ]

  # leo el archivo donde quedaron los hiperparametros optimos
  log_info("Leyendo mejores hiperparámetros de BO_log.txt")
  tb_BO <-  fread("BO_log.txt")
  setorder( tb_BO, -metrica)  # ordeno por metrica descendente
  log_info(paste("Mejores hiperparámetros:", tb_BO[1]))

  # en la tabla ademas de los parametros del LightGBM, hay campos de salida
  param_lgbm <- union( names(PARAM$lgbm$param_fijos),  names(PARAM$hipeparametertuning$hs$pars) )

  PARAM$train_final$param_mejores <- as.list( tb_BO[1, param_lgbm, with=FALSE])

  log_info("Ajustando min_data_in_leaf")
  PARAM$train_final$param_mejores$min_data_in_leaf <- as.integer( round(PARAM$train_final$param_mejores$min_data_in_leaf * nrow(dataset_train_final[training == 1L]) / nrow(dtrain)))

  log_info(paste("Original min_data_in_leaf:", tb_BO[1, min_data_in_leaf], "Ajustado min_data_in_leaf:", PARAM$train_final$param_mejores$min_data_in_leaf))
  log_info(paste("Parámetros finales:", PARAM$train_final$param_mejores))

  # dejo los datos en formato LightGBM
  log_info("Creando dtrain_final")
  dtrain_final <- lgb.Dataset(
    data= data.matrix(dataset_train_final[training == 1L, campos_buenos, with= FALSE]),
    label= dataset_train_final[training == 1L, clase01],
    free_raw_data= FALSE
  )

  log_info(paste("dtrain_final filas:", nrow(dtrain_final), "columnas:", ncol(dtrain_final)))

  # genero los modelitos
  log_info("Generando modelos para hacer ensemble de semillas.")
  dir.create( "modelitos", showWarnings= FALSE)

  param_completo <- copy( PARAM$train_final$param_mejores)

  for( sem in PARAM$train_final$semillas ) {

    arch_modelo <- paste0("./modelitos/mod_", sem, ".txt")
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
  log_info("Modelos generados")

  # aplico el modelo a los datos sin clase
  log_info("Aplicando modelos a datos futuros")
  dfuture <- dataset[foto_mes %in% PARAM$train_final$future ]
  mfuture <- data.matrix(dfuture[, campos_buenos, with= FALSE])

  vpred_acum <- rep(0.0, nrow(dfuture))
  qacumulados <- 0

  for( sem in PARAM$train_final$semillas ) {

    arch_modelo <- paste0("./modelitos/mod_", sem, ".txt")
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
  log_info("Modelos aplicados")


  # tabla de prediccion, puede ser util para futuros ensembles
  #  ya que le modelo ganador va a ser un ensemble de LightGBMs

  log_info("Creando tabla de predicción")
  tb_prediccion <- dfuture[, list(numero_de_cliente, foto_mes)]
  tb_prediccion[, prob := vpred_acum ]

  # grabo las probabilidad del modelo
  fwrite(tb_prediccion,
    file= "prediccion.txt",
    sep= "\t"
  )
  log_info("Tabla de predicción guardada en prediccion.txt")

  # genero archivos con los  "envios" mejores
  log_info("Generando archivo para Kaggle")
  dir.create("kaggle", showWarnings=FALSE)

  # ordeno por probabilidad descendente
  setorder(tb_prediccion, -prob)

  envios <- c(PARAM$eval_ensamble$envios_optimos_promedio, PARAM$eval_ensamble$envios_optimos_promedio + 1, 11000)

  for (envio in envios) {
    tb_prediccion[, Predicted := 0L]
    tb_prediccion[1:envio, Predicted := 1L]

    archivo_kaggle <- paste0("./kaggle/KA", PARAM$experimento, "_", envio, ".csv")

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
log_info("Fin Modelo Final")