tryCatch(
  {
    log_info("Iniciando la generación del Modelo Final (Predicción).")

    # Cargar Mejores Hiperparámetros
    log_info("Cargando mejores hiperparámetros de BO_log.txt")
    dir_bayesiana <- file.path(PARAM$experimento_folder, PARAM$carpeta_bayesiana)
    log_bo_file <- file.path(dir_bayesiana, "BO_log.txt")

    if (!file.exists(log_bo_file)) {
      log_bo_file <- file.path(PARAM$experimento_folder, "BO_log.txt")
      if (!file.exists(log_bo_file)) {
        stop("No se encontró el archivo BO_log.txt. Asegúrate de que 9_Optimizacion_Bayesiana.R se haya ejecutado.")
      }
    }

    tb_BO <- fread(log_bo_file)
    setorder(tb_BO, -metrica)

    # Definir manualmente los nombres de los hiperparámetros optimizados
    nombres_hiper_optimizados <- c(
      "num_iterations",
      "learning_rate",
      "feature_fraction",
      "min_data_in_leaf",
      "num_leaves"
    )

    param_lgbm <- union(names(PARAM$lgbm$param_fijos), nombres_hiper_optimizados)
    param_mejores <- as.list(tb_BO[1, param_lgbm, with = FALSE])

    log_info("Hiperparámetros óptimos cargados:")
    log_info(paste(capture.output(print(param_mejores)), collapse = "\n"))

    # Preparar Datos de Entrenamiento
    log_info("Filtrando datos para el modelo final (PARAM$train_final$training).")
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

    dataset_train_final[, azar := NULL] # elimino la columna azar

    # Crear clase binaria {0,1}
    log_info("Creando clase01 para el modelo final.")
    dataset_train_final[
      ,
      clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)
    ]

    log_info(paste("Modelo final creado. Número de filas a ser utilizadas: ", nrow(dataset_train_final[training == 1L])))

    # Escalado de Hiperparámetros
    log_info("Ajustando los hiperparámetros al tamaño del dataset final.")

    if (!exists("dtrain") || is.null(dtrain)) {
      # Intentamos leer el archivo si dtrain no está en memoria
      nrow_dtrain_path <- file.path(PARAM$experimento_folder, PARAM$carpeta_bayesiana, "nrow_dtrain.rds")
      if (file.exists(nrow_dtrain_path)) {
         nrow_dtrain_bayesiana <- readRDS(nrow_dtrain_path)
         log_info(paste("Se leyó nrow(dtrain) desde el archivo:", nrow_dtrain_bayesiana))
      } else {
         stop(paste("No se encontró 'nrow_dtrain.rds' en", dirname(nrow_dtrain_path)))
      }
    } else {
      nrow_dtrain_bayesiana <- nrow(dtrain)
      log_info(paste("Se usó nrow(dtrain) desde la memoria:", nrow_dtrain_bayesiana))
    }

    ratio <- nrow(dataset_train_final[training == 1L]) / nrow_dtrain_bayesiana
    param_mejores$min_data_in_leaf <- as.integer(round(param_mejores$min_data_in_leaf * ratio))

    if (is.na(param_mejores$min_data_in_leaf)) {
      stop("El cálculo final de min_data_in_leaf resultó en NA. Verifica los valores.")
    }

    log_info(paste("Original min_data_in_leaf:", tb_BO[1, min_data_in_leaf], "Ajustado min_data_in_leaf:", param_mejores$min_data_in_leaf))

    # Crear Dataset LightGBM
    campos_buenos <- copy(setdiff(
      colnames(dataset_train_final), PARAM$trainingstrategy$campos_entrenar
    ))

    dtrain_final <- lgb.Dataset(
      data = data.matrix(dataset_train_final[training == 1L, campos_buenos, with = FALSE]),
      label = dataset_train_final[training == 1L, clase01],
      free_raw_data = TRUE
    )

    log_info(paste("dtrain_final filas:", nrow(dtrain_final), "columnas:", ncol(dtrain_final)))

    rm(dataset_train_final)
    gc()

    # Preparar Datos Futuros (Predicción)
    log_info(paste("Preparando datos futuros para predicción usando PARAM$train_final$future:", paste(PARAM$train_final$future, collapse = ", ")))
    dfuture <- dataset[foto_mes %in% PARAM$train_final$future]

    if (nrow(dfuture) == 0) {
      stop(paste("No se encontraron datos para el período futuro:", paste(PARAM$train_final$future, collapse = ", ")))
    }

    mfuture <- data.matrix(dfuture[, campos_buenos, with = FALSE])
    
    # Crear directorios de salida
    dir_modelos <- file.path(PARAM$experimento_folder, PARAM$modelos_folder)
    dir.create(dir_modelos, recursive = TRUE, showWarnings = FALSE)
    
    dir_entregables <- file.path(PARAM$experimento_folder, PARAM$carpeta_entregables)
    dir.create(dir_entregables, recursive = TRUE, showWarnings = FALSE)

    # 6. Entrenamiento del Ensamble y Predicción
    total_semillas <- PARAM$train_final$ksemillerio * PARAM$train_final$iter
    
    set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")
    if (!exists("primos")) primos <- generate_primes(min = 100000, max = 1000000)
    PARAM$train_final$semillas <- sample(primos)[seq(total_semillas)]
    
    semillas_a_usar <- PARAM$train_final$semillas
    numero_de_semillas <- length(semillas_a_usar)

    log_info(paste0("Entrenando y prediciendo con ", numero_de_semillas, " semillas."))

    # Vector acumulador de probabilidades
    prob_acumulada <- rep(0, nrow(dfuture))

    param_entrenamiento <- copy(param_mejores)

    for (i in seq_along(semillas_a_usar)) {
      semilla_actual <- semillas_a_usar[i]
      log_info(paste0("--- Procesando semilla: ", semilla_actual, " (", i, "/", numero_de_semillas, ") ---"))

      # Verificar/Entrenar Modelo
      ruta_modelo <- file.path(dir_modelos, paste0("mod_final_", semilla_actual, ".txt"))

      if (file.exists(ruta_modelo)) {
        log_info(paste("Modelo existente encontrado. Cargando desde:", ruta_modelo))
        modelo <- lgb.load(filename = ruta_modelo)
      } else {
        log_info(paste("Entrenando nuevo modelo para semilla:", semilla_actual))
        param_entrenamiento$seed <- semilla_actual
        modelo <- lgb.train(data = dtrain_final, param = param_entrenamiento)
        
        lgb.save(modelo, filename = ruta_modelo)
        log_info(paste("Modelo guardado en:", ruta_modelo))
      }

      # Predicción
      prediccion_individual <- predict(modelo, mfuture)
      prob_acumulada <- prob_acumulada + prediccion_individual

      rm(modelo, prediccion_individual)
      gc()
    }

    # Promediar probabilidades
    prob_promedio <- prob_acumulada / numero_de_semillas

    # Generar Archivos de Salida
    log_info("Generando archivos de salida (Predicciones).")

    tb_prediccion <- dfuture[, list(numero_de_cliente, foto_mes)]
    tb_prediccion[, prob := prob_promedio]
    setorder(tb_prediccion, -prob)

    # Guardar probabilidades crudas
    file_probabilidades <- file.path(dir_entregables, paste0("prediccion_probabilidades_final_", PARAM$experimento, ".csv"))
    fwrite(tb_prediccion[, .(numero_de_cliente, prob)],
      file = file_probabilidades,
      sep = ",",
      col.names = FALSE
    )
    log_info(paste("Archivo de predicción con probabilidades guardado en:", file_probabilidades))

    # Generar entregas para los cortes solicitados
    envios_a_generar <- PARAM$train_final$envios_a_generar
    log_info(paste("Generando entregas para cortes:", paste(envios_a_generar, collapse = ", ")))

    for (envio in envios_a_generar) {
      # Asignación de la clase predicha
      tb_prediccion[, Predicted := 0L]
      tb_prediccion[1:envio, Predicted := 1L]

      archivo_kaggle <- file.path(dir_entregables, paste0("IDs_", PARAM$experimento, "_", envio, ".csv"))

      # Grabo el archivo
      fwrite(tb_prediccion[Predicted == 1L, .(numero_de_cliente)],
        file = archivo_kaggle,
        col.names = FALSE
      )

      log_info(paste("Archivo con los IDs seleccionados guardado en:", archivo_kaggle))
    }

    log_info("Generando un archivo de entrega tipo Kaggle...")

    for (envio in envios_a_generar) {
      tb_prediccion[, Predicted := 0L]
      tb_prediccion[1:envio, Predicted := 1L]

      archivo_kaggle <- file.path(dir_entregables, paste0("KA_", PARAM$experimento, "_", envio, ".csv"))

      # grabo el archivo
      fwrite(tb_prediccion[, list(numero_de_cliente, Predicted)],
        file = archivo_kaggle,
        sep = ","
      )
      log_info(paste("Archivo para Kaggle guardado en:", archivo_kaggle))
    }

    log_info("Proceso de Modelo Final finalizado correctamente.")
  },
  error = function(e) {
    log_error("######################################################")
    log_error("Se ha producido un error fatal en 12_Modelo_Final.R")
    log_error(paste("Mensaje de R:", e$message))
    log_error("######################################################")
  }
)
