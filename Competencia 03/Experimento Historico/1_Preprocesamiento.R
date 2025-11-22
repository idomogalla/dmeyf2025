tryCatch(
  {
    # Generación de la clase_ternaria
    if (PARAM$generar_ternaria) {
      log_info("Generando clase_ternaria.")
      # leo el dataset
      dataset <- fread(PARAM$input_dataset)

      # calculo el periodo0 consecutivo
      dsimple <- dataset[, list(
        "pos" = .I,
        numero_de_cliente,
        periodo0 = as.integer(foto_mes / 100) * 12 + foto_mes %% 100
      )]

      # ordeno
      setorder(dsimple, numero_de_cliente, periodo0)

      # calculo topes
      periodo_ultimo <- dsimple[, max(periodo0)]
      periodo_anteultimo <- periodo_ultimo - 1

      # calculo los leads de orden 1 y 2
      dsimple[, c("periodo1", "periodo2") :=
        shift(periodo0, n = 1:2, fill = NA, type = "lead"), numero_de_cliente]

      # assign most common class values = "CONTINUA"
      dsimple[periodo0 < periodo_anteultimo, clase_ternaria := "CONTINUA"]

      # calculo BAJA+1
      dsimple[
        periodo0 < periodo_ultimo &
          (is.na(periodo1) | periodo0 + 1 < periodo1),
        clase_ternaria := "BAJA+1"
      ]

      # calculo BAJA+2
      dsimple[
        periodo0 < periodo_anteultimo & (periodo0 + 1 == periodo1) &
          (is.na(periodo2) | periodo0 + 2 < periodo2),
        clase_ternaria := "BAJA+2"
      ]

      # pego el resultado en el dataset original y grabo
      setorder(dsimple, pos)
      dataset[, clase_ternaria := dsimple$clase_ternaria]

      rm(dsimple)
      gc()

      # Resultado de la clase_ternaria
      setorder(dataset, foto_mes, clase_ternaria, numero_de_cliente)
      print(dataset[, .N, list(foto_mes, clase_ternaria)])

      # Grabo en un archivo la cantidad de BAJA+1, BAJA+2 y CONTINUA
      fwrite(dataset[, .N, list(foto_mes, clase_ternaria)],
        file = file.path(PARAM$experimento_folder, "class_ternaria_summary.txt"),
        sep = "\t"
      )

      # Grabo el dataset con la clase_ternaria
      fwrite(dataset,
        file = file.path(PARAM$dir_dataset, PARAM$dataset_ternaria_name),
        sep = ","
      )
      log_info("clase_ternaria generada y guardada.")
    } else {
      log_info("No se genera clase_ternaria, se lee el dataset directamente.")
      dataset <- fread(PARAM$input_dataset)
    }
  },
  error = function(e) {
    log_error("######################################################")
    log_error("Se ha producido un error fatal en la Sección 1: Preprocesamiento.")
    log_error(paste("Mensaje de R:", e$message))
    log_error("######################################################")
  }
)
