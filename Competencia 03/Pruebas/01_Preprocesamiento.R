tryCatch({
  # Generación de la clase_ternaria
  if(PARAM$generar_ternaria){
      log_info("Iniciando generación de clase_ternaria con unificación de datasets.")
      # Definir paths de los dos archivos
      path_hist <- file.path(PARAM$dir_dataset, PARAM$dataset_hist_name)
      path_new  <- file.path(PARAM$dir_dataset, PARAM$dataset_name)
      # Cargar los datasets
      log_info(paste("Leyendo histórico:", path_hist))
      dataset_hist <- fread(path_hist)

      log_info(paste("Leyendo nuevo mes:", path_new))
      dataset_new <- fread(path_new)

      # Unificarlos (rbind)
      log_info("Unificando datasets...")
      dataset <- rbind(dataset_hist, dataset_new, fill=TRUE)

      # Liberar memoria de los individuales
      rm(dataset_hist, dataset_new)
      gc()

      log_info(paste("Periodos en dataset unificado:", paste(sort(unique(dataset$foto_mes)), collapse = ", ")))

      # calculo el periodo0 consecutivo
      dsimple <- dataset[, list(
                          "pos" = .I,
                          numero_de_cliente,
                          periodo0 = as.integer(foto_mes/100)*12 +  foto_mes%%100 )
                        ]

      # ordeno
      setorder( dsimple, numero_de_cliente, periodo0 )

      # calculo topes
      periodo_ultimo <- dsimple[, max(periodo0) ]
      periodo_anteultimo <- periodo_ultimo - 1

      # calculo los leads de orden 1 y 2
      dsimple[, c("periodo1", "periodo2") :=
                shift(periodo0, n=1:2, fill=NA, type="lead"),  numero_de_cliente
              ]

      # assign most common class values = "CONTINUA"
      dsimple[ periodo0 < periodo_anteultimo, clase_ternaria := "CONTINUA" ]

      # calculo BAJA+1
      dsimple[ periodo0 < periodo_ultimo &
                ( is.na(periodo1) | periodo0 + 1 < periodo1 ),
                clase_ternaria := "BAJA+1"
              ]

      # calculo BAJA+2
      dsimple[ periodo0 < periodo_anteultimo & (periodo0+1 == periodo1 )
                & ( is.na(periodo2) | periodo0 + 2 < periodo2 ),
                clase_ternaria := "BAJA+2"
              ]

      # pego el resultado en el dataset original
      setorder( dsimple, pos )
      dataset[, clase_ternaria := dsimple$clase_ternaria ]

      rm(dsimple)
      gc()

      # Resultado de la clase_ternaria
      setorder( dataset, foto_mes, clase_ternaria, numero_de_cliente)
      log_info("Distribución de clase ternaria:")
      print(dataset[, .N, list(foto_mes, clase_ternaria)])

      # Grabo en un archivo la cantidad de BAJA+1, BAJA+2 y CONTINUA
      fwrite(dataset[, .N, list(foto_mes, clase_ternaria)],
        file = file.path(PARAM$experimento_folder, "class_ternaria_summary.txt"),
        sep = "\t"
      )

      # Grabo el dataset FINAL unificado con la clase_ternaria calculada
      path_salida <- file.path(PARAM$dir_dataset, PARAM$dataset_ternaria_name)
      log_info(paste("Guardando dataset unificado en:", path_salida))
      fwrite( dataset,
          file = path_salida,
          sep = ","
      )
      log_info("clase_ternaria generada y guardada exitosamente.")
  } else {
    log_info("No se genera clase_ternaria, se lee el dataset directamente del input definido.")
    dataset <- fread(PARAM$input_dataset)
  }
}, error = function(e) {
  log_error("######################################################")
  log_error("Se ha producido un error fatal en la Sección 1: Preprocesamiento.")
  log_error(paste("Mensaje de R:", e$message))
  log_error("######################################################")
})