#--- Funciones de Evaluación ---
particionar <- function(data,
                        division,
                        agrupa = "",
                        campo = "fold",
                        start = 1,
                        seed = NA) {
  if (!is.na(seed)) {
    set.seed(seed, "L'Ecuyer-CMRG")
  }

  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(
    from = start, length.out = length(division)
  )))

  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N], by = agrupa]
}

realidad_inicializar <- function(pfuture, pparam) {
  drealidad <- pfuture[, list(numero_de_cliente, foto_mes, clase_ternaria)]

  semilla_a_usar <- ifelse(
    is.null(pparam$semilla_kaggle),
    pparam$semilla_primigenia,
    pparam$semilla_kaggle
  )

  particionar(
    drealidad,
    division = c(3, 7),
    agrupa = "clase_ternaria",
    seed = semilla_a_usar
  )

  return(drealidad)
}

realidad_evaluar <- function(prealidad, pprediccion) {
  prealidad_eval <- copy(prealidad)

  prealidad_eval[pprediccion, on = c("numero_de_cliente", "foto_mes"), predicted := i.Predicted]

  tbl <- prealidad_eval[, list("qty" = .N), list(fold, predicted, clase_ternaria)]

  res <- list()
  res$public <- tbl[fold == 1 &
    predicted == 1L, sum(qty * ifelse(clase_ternaria == "BAJA+2", 780000, -20000))] / 0.3
  res$private <- tbl[fold == 2 &
    predicted == 1L, sum(qty * ifelse(clase_ternaria == "BAJA+2", 780000, -20000))] / 0.7
  res$total <- tbl[predicted == 1L, sum(qty * ifelse(clase_ternaria == "BAJA+2", 780000, -20000))]

  return(res)
}

#--- Funciones de Gráficos ---
GraficarImportancia <- function(importancia, top_n = 50, ruta_grafico, subtitulo = "") {
  importancia_top <- importancia[order(-Gain)][1:min(top_n, nrow(importancia))]

  p <- ggplot(importancia_top, aes(x = reorder(Feature, Gain), y = Gain)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    coord_flip() +
    labs(
      title = paste("Top", top_n, "Feature Importance"),
      subtitle = subtitulo,
      x = "Features",
      y = "Gain"
    ) +
    theme_minimal()

  ggsave(ruta_grafico, plot = p, width = 10, height = 8)
  log_info(paste("Gráfico de importancia de features guardado en:", ruta_grafico))
}

GraficarCurvasEnsemble <- function(lista_resultados, tb_resultados_ensamble, PARAM_plot) {
  log_info("Iniciando la graficación de la superposición de curvas del ensemble.")

  tb_todas <- rbindlist(lapply(names(lista_resultados), function(sem) {
    lista_resultados[[sem]][, semilla := as.character(sem)]
  }))

  tb_promedio_visual <- tb_todas[, .(ganancia_total = mean(ganancia_total)), by = clientes]

  setnames(tb_resultados_ensamble, "ganancia_total", "ganancia_ensamble_real")

  if (!"ganancia_meseta" %in% colnames(tb_resultados_ensamble)) {
    log_warn("La columna 'ganancia_meseta' no se encontró. El gráfico puede fallar o estar incompleto.")
    tb_resultados_ensamble[, ganancia_meseta := NA_real_]
  }

  maximo_punto <- tb_resultados_ensamble[ganancia_ensamble_real == max(ganancia_ensamble_real, na.rm = TRUE)]
  maximo_punto <- head(maximo_punto, 1)

  maximo_meseta <- tb_resultados_ensamble[ganancia_meseta == max(ganancia_meseta, na.rm = TRUE)]
  maximo_meseta <- head(maximo_meseta, 1)

  semillas_unicas <- unique(tb_todas$semilla)
  label_ensamble_real <- "Ensamble Real (Negro)"
  label_promedio_visual <- "Promedio Visual (Azul)"
  label_meseta <- "Meseta Suavizada (Púrpura)"

  labels_plot <- c(semillas_unicas, label_ensamble_real, label_promedio_visual, label_meseta)
  names(labels_plot) <- c(semillas_unicas, "Ensamble Real", "Promedio Visual", "Meseta")

  colores_individuales <- scales::hue_pal()(length(semillas_unicas))
  names(colores_individuales) <- semillas_unicas
  colores_plot <- c(
    colores_individuales,
    "Ensamble Real" = "black",
    "Promedio Visual" = "blue",
    "Meseta" = "purple"
  )

  p <- ggplot() +
    geom_line(data = tb_todas, aes(x = clientes, y = ganancia_total, group = semilla, color = semilla), alpha = 0.45, linewidth = 0.5) +
    geom_line(data = tb_promedio_visual, aes(x = clientes, y = ganancia_total, color = "Promedio Visual"), linewidth = 0.8, linetype = "dashed") +
    geom_line(data = tb_resultados_ensamble, aes(x = clientes, y = ganancia_ensamble_real, color = "Ensamble Real"), linewidth = 1.0) +
    geom_line(data = tb_resultados_ensamble, aes(x = clientes, y = ganancia_meseta, color = "Meseta"), linewidth = 0.8, linetype = "dotdash") +
    geom_point(data = maximo_punto, aes(x = clientes, y = ganancia_ensamble_real), color = "red", size = 3, shape = 16) +
    geom_point(data = maximo_meseta, aes(x = clientes, y = ganancia_meseta), color = "purple", size = 3, shape = 17) +
    geom_label_repel(
      data = maximo_punto,
      aes(
        x = clientes, y = ganancia_ensamble_real,
        label = paste0(
          "Máximo\n",
          format(ganancia_ensamble_real,
            big.mark = ".",
            decimal.mark = ",", scientific = FALSE
          ),
          "\nEnvios\n",
          format(clientes, big.mark = ".", decimal.mark = ",")
        )
      ),
      fill = "white", color = "red", fontface = "bold",
      label.padding = unit(0.3, "lines"),
      nudge_y = 80000000,
      segment.color = "grey30",
      min.segment.length = 0,
      direction = "y"
    ) +
    geom_label_repel(
      data = maximo_meseta,
      aes(
        x = clientes, y = ganancia_meseta,
        label = paste0(
          "Meseta\n",
          format(ganancia_meseta,
            big.mark = ".",
            decimal.mark = ",", scientific = FALSE
          ),
          "\nEnvios\n",
          format(clientes, big.mark = ".", decimal.mark = ",")
        )
      ),
      fill = "white", color = "purple", fontface = "bold",
      label.padding = unit(0.3, "lines"),
      nudge_y = -90000000,
      segment.color = "grey30",
      min.segment.length = 0,
      direction = "y"
    ) +
    scale_y_continuous(
      labels = scales::comma,
      expand = expansion(mult = c(0.1, 0.25))
    ) +
    scale_x_continuous(labels = scales::comma) +
    scale_color_manual(
      name = "Modelo",
      values = colores_plot,
      labels = labels_plot
    ) +
    labs(
      title = paste0("Ganancia Acumulada (Semillas y Ensamble) - ", PARAM_plot$experimento),
      x = "Clientes Contactados (Envíos)",
      y = "Ganancia Acumulada"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 9, face = "bold"),
      legend.key.size = unit(0.5, "lines"),
      plot.margin = margin(10, 10, 10, 10)
    ) +
    guides(color = guide_legend(override.aes = list(
      alpha = 1,
      linewidth = 1.5,
      linetype = c(rep("solid", length(semillas_unicas) + 1), "dashed", "dotdash")
    )))

  ruta_grafico <- file.path(PARAM_plot$carpeta_graficos, "eval_curvas.png")
  ggsave(
    ruta_grafico,
    plot = p,
    width = 14,
    height = 8
  )

  log_info(paste0("Gráfico de superposición de curvas del ensemble guardado en: ", ruta_grafico))
}

#------------------------------------------------------
# Sección Principal: Evaluación del Ensamble
#------------------------------------------------------
tryCatch(
  {
    log_info("Iniciando la evaluación del ensamble en datos de testing.")

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

    # se filtran los meses donde se entrena el modelo final
    log_info("Filtrando datos para el modelo final (PARAM$train_final$training).")
    dataset_train_final <- dataset[foto_mes %in% PARAM$train_final$training]

    # Undersampling, van todos los "BAJA+1" y "BAJA+2" y solo algunos "CONTINUA"
    log_info("Haciendo undersampling para el modelo final.")
    set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")
    dataset_train_final[, azar := runif(nrow(dataset_train_final))]
    dataset_train_final[, training := 0L]

    dataset_train_final[
      (azar <= PARAM$train_final$undersampling | clase_ternaria %in% c("BAJA+1", "BAJA+2")),
      training := 1L
    ]

    dataset_train_final[, azar := NULL] # elimino la columna azar

    # paso la clase a binaria que tome valores {0,1}  enteros
    log_info("Creando clase01 para el modelo final.")
    dataset_train_final[
      ,
      clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)
    ]

    log_info(paste("Modelo final creado. Número de filas a ser utilizadas: ", nrow(dataset_train_final[training == 1L])))

    # Escalado de Hiperparámetros
    log_info("Ajustando los hiperparámetros al tamaño del dataset final.")

    if (!exists("dtrain") || is.null(dtrain)) {
      log_info("El objeto 'dtrain' (de la bayesiana) no existe en memoria. Leyendo 'nrow_dtrain.rds' desde el disco.")
      nrow_dtrain_path <- file.path(PARAM$experimento_folder, PARAM$carpeta_bayesiana, "nrow_dtrain.rds")

      if (!file.exists(nrow_dtrain_path)) {
        stop(paste(
          "No se encontró 'nrow_dtrain.rds' en", dirname(nrow_dtrain_path),
          "\nAsegúrate de que 10_Optimizacion_Bayesiana.R se haya ejecutado y guardado el archivo."
        ))
      }

      nrow_dtrain_bayesiana <- readRDS(nrow_dtrain_path)
      log_info(paste("Se leyó nrow(dtrain) desde el archivo:", nrow_dtrain_bayesiana))
    } else {
      nrow_dtrain_bayesiana <- nrow(dtrain) # Fallback si ya estaba cargado
      log_info(paste("Se usó nrow(dtrain) desde la memoria:", nrow_dtrain_bayesiana))
    }

    ratio <- nrow(dataset_train_final[training == 1L]) / nrow_dtrain_bayesiana

    param_mejores$min_data_in_leaf <- as.integer(round(param_mejores$min_data_in_leaf * ratio))

    # Chequeo de seguridad
    if (is.na(param_mejores$min_data_in_leaf)) {
      stop("El cálculo final de min_data_in_leaf resultó en NA. Verifica los valores.")
    }

    log_info(paste("Original min_data_in_leaf:", tb_BO[1, min_data_in_leaf], "Ajustado min_data_in_leaf:", param_mejores$min_data_in_leaf))
    log_info("Parámetros finales ajustados:")
    log_info(paste(capture.output(print(param_mejores)), collapse = "\n"))

    # Preparar Datos para LightGBM
    campos_buenos <- copy(setdiff(
      colnames(dataset_train_final), PARAM$trainingstrategy$campos_entrenar
    ))

    dtrain <- lgb.Dataset(
      data = data.matrix(dataset_train_final[training == 1L, campos_buenos, with = FALSE]),
      label = dataset_train_final[training == 1L, clase01],
      free_raw_data = TRUE
    )

    log_info(paste("dtrain nombre de las columnas: ", paste(colnames(dtrain), collapse = ", ")))
    log_info(paste("dtrain filas:", nrow(dtrain), "columnas:", ncol(dtrain)))

    # Libero memoria de dataset_train_final
    rm(dataset_train_final)
    gc()

    # Preparar datos de evaluación (testing) - USANDO PARAM$train_final$future
    log_info(paste("Preparando datos de evaluación (testing) usando PARAM$train_final$future:", paste(PARAM$train_final$future, collapse = ", ")))
    dfuture <- dataset[foto_mes %in% PARAM$train_final$future]

    if (nrow(dfuture) == 0) {
      stop(paste("No se encontraron datos para el período de testing:", paste(PARAM$train_final$future, collapse = ", ")))
    }

    mfuture <- data.matrix(dfuture[, campos_buenos, with = FALSE])
    drealidad <- realidad_inicializar(dfuture, PARAM)
    cortes_evaluacion <- PARAM$train_final$cortes_evaluacion

    # Crear directorios
    dir_graficos <- file.path(PARAM$experimento_folder, PARAM$carpeta_graficos)
    dir.create(dir_graficos, recursive = TRUE, showWarnings = FALSE)

    dir_evaluacion <- file.path(PARAM$experimento_folder, PARAM$carpeta_evaluacion)
    dir.create(dir_evaluacion, recursive = TRUE, showWarnings = FALSE)

    log_info(paste("Directorio de Gráficos creado en:", dir_graficos))
    log_info(paste("Directorio de Evaluación creado en:", dir_evaluacion))

    dir_modelos <- file.path(PARAM$experimento_folder, PARAM$modelos_folder)
    dir.create(dir_modelos, recursive = TRUE, showWarnings = FALSE)
    log_info(paste("Directorio de Modelos creado en:", dir_modelos))

    # Bucle de Entrenamiento y Evaluación del Ensamble
    total_semillas <- PARAM$train_final$ksemillerio * PARAM$train_final$iter

    log_info(paste0(
      "Total de semillas a entrenar: ", total_semillas,
      " (Iter: ", PARAM$train_final$iter,
      " x kSemillerio: ", PARAM$train_final$ksemillerio, ")"
    ))

    set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")
    if (!exists("primos")) primos <- generate_primes(min = 100000, max = 1000000)
    PARAM$train_final$semillas <- sample(primos)[seq(total_semillas)]

    semillas_a_evaluar <- PARAM$train_final$semillas
    numero_de_semillas <- length(semillas_a_evaluar)

    log_info(paste0("Evaluando ", numero_de_semillas, " semilla(s)."))
    log_info(paste("Semillas a evaluar: ", paste(semillas_a_evaluar, collapse = ", ")))

    # Guardar las semillas a evaluar para que el script APO las pueda usar
    ruta_semillas <- file.path(dir_evaluacion, "semillas_evaluadas.rds")
    saveRDS(semillas_a_evaluar, file = ruta_semillas)
    log_info(paste("Semillas guardadas en:", ruta_semillas))

    lista_predicciones <- list()
    lista_resultados_individuales <- list()
    lista_importancia <- list()

    resumen_ganancias <- data.table(
      semilla = character(),
      max_ganancia = numeric(),
      envios_optimos = character()
    )

    param_entrenamiento <- copy(param_mejores)

    log_info(paste0("Iniciando evaluación de ", numero_de_semillas, " semillas..."))

    for (i in seq_along(semillas_a_evaluar)) {
      semilla_actual <- semillas_a_evaluar[i]
      log_info(paste0("--- Procesando semilla: ", semilla_actual, " (", i, "/", numero_de_semillas, ") ---"))

      # Verificar si el modelo ya existe
      ruta_modelo <- file.path(dir_modelos, paste0("mod_", semilla_actual, ".txt"))

      if (file.exists(ruta_modelo)) {
        log_info(paste("Modelo existente encontrado. Cargando desde:", ruta_modelo))
        modelo <- lgb.load(filename = ruta_modelo)
      } else {
        log_info(paste("Modelo no encontrado. Entrenando nuevo modelo para semilla:", semilla_actual))
        param_entrenamiento$seed <- semilla_actual
        modelo <- lgb.train(data = dtrain, param = param_entrenamiento)

        # Guardar el modelo
        lgb.save(modelo, filename = ruta_modelo)
        log_info(paste("Modelo guardado en:", ruta_modelo))
      }

      imp <- lgb.importance(modelo, percentage = TRUE)
      lista_importancia[[as.character(semilla_actual)]] <- imp

      imp_ordenada_ind <- imp[order(-Gain)]

      ruta_csv_ind <- file.path(dir_evaluacion, paste0("fi_semilla_", semilla_actual, ".csv"))
      fwrite(imp_ordenada_ind, file = ruta_csv_ind)

      ruta_grafico_ind <- file.path(dir_graficos, paste0("fi_semilla_", semilla_actual, ".png"))
      GraficarImportancia(imp_ordenada_ind,
        top_n = PARAM$trainingstrategy$importancias,
        ruta_grafico = ruta_grafico_ind,
        subtitulo = paste(PARAM$experimento, "- Semilla:", semilla_actual)
      )

      rm(imp_ordenada_ind)

      prediccion_individual <- predict(modelo, mfuture)
      tb_pred_individual <- dfuture[, list(numero_de_cliente, foto_mes)]
      tb_pred_individual[, prob := prediccion_individual]
      tb_pred_individual[, semilla := as.character(semilla_actual)]

      lista_predicciones[[as.character(semilla_actual)]] <- tb_pred_individual

      resultados_individual <- data.table()
      setorder(tb_pred_individual, -prob)

      for (envios in cortes_evaluacion) {
        if (envios > 0 && envios <= nrow(tb_pred_individual)) {
          tb_pred_individual[, Predicted := 0L]
          tb_pred_individual[1:envios, Predicted := 1L]
          res_ind <- realidad_evaluar(drealidad, tb_pred_individual)
          resultados_individual <- rbind(
            resultados_individual,
            data.table(clientes = envios, ganancia_total = res_ind$total)
          )
        } else if (envios == 0) {
          resultados_individual <- rbind(
            resultados_individual,
            data.table(clientes = 0, ganancia_total = 0)
          )
        }
      }

      lista_resultados_individuales[[as.character(semilla_actual)]] <- resultados_individual

      max_ganancia_ind <- max(resultados_individual$ganancia_total, na.rm = TRUE)
      envios_optimos_ind <- resultados_individual[ganancia_total == max_ganancia_ind, clientes]
      envios_optimos_str <- paste(sort(unique(envios_optimos_ind)), collapse = ", ")

      log_info(paste0(
        "Semilla ", semilla_actual, ": Ganancia Máx = ",
        format(max_ganancia_ind, big.mark = ".", decimal.mark = ",", scientific = FALSE),
        " en envíos: [", envios_optimos_str, "]"
      ))

      resumen_ganancias <- rbind(
        resumen_ganancias,
        data.table(
          semilla = as.character(semilla_actual),
          max_ganancia = max_ganancia_ind,
          envios_optimos = envios_optimos_str
        )
      )

      rm(modelo, imp, prediccion_individual)
      gc()
    }

    log_info("Evaluación de semillas individuales completa.")

    rm(mfuture)
    gc()

    # Procesar Feature Importance Promediado
    log_info("Calculando Feature Importance promediada del ensamble.")

    imp_todas <- rbindlist(lista_importancia)
    imp_promediada <- imp_todas[, .(Gain = mean(Gain), Cover = mean(Cover), Frequency = mean(Frequency)), by = Feature]
    imp_ordenada <- imp_promediada[order(-Gain)]

    rm(lista_importancia, imp_todas)
    gc()

    ruta_csv_imp <- file.path(dir_evaluacion, "fi_ensemble.csv")
    fwrite(imp_ordenada, file = ruta_csv_imp)
    log_info(paste("Importancia de features (promediada) guardada en:", ruta_csv_imp))

    ruta_grafico_imp <- file.path(dir_graficos, "fi_ensemble.png")
    GraficarImportancia(imp_ordenada,
      top_n = PARAM$trainingstrategy$importancias,
      ruta_grafico = ruta_grafico_imp,
      subtitulo = paste(PARAM$experimento, "- Promedio de", length(semillas_a_evaluar), "semillas")
    )

    # Evaluación del Ensamble Promediado (ESTRATEGIA "GRAN ENSAMBLE")
    log_info("Evaluando el 'Gran Ensamble' (promedio de todas las semillas)...")

    predicciones_todas <- rbindlist(lista_predicciones)
    tb_prediccion_ensamble <- predicciones_todas[, .(prob = mean(prob)), by = .(numero_de_cliente, foto_mes)]

    rm(lista_predicciones, predicciones_todas)
    gc()

    resultados_ensamble <- data.table()
    setorder(tb_prediccion_ensamble, -prob)

    # Generar archivo con probabilidades (numero_de_cliente, prob)
    file_probabilidades <- file.path(dir_evaluacion, paste0("prediccion_probabilidades_evaluacion_", PARAM$experimento, ".csv"))
    fwrite(tb_prediccion_ensamble[, .(numero_de_cliente, prob)],
      file = file_probabilidades,
      sep = ",",
      col.names = FALSE
    )
    log_info(paste("Tabla de probabilidades guardada en:", file_probabilidades))

    for (envios in cortes_evaluacion) {
      if (envios > 0 && envios <= nrow(tb_prediccion_ensamble)) {
        tb_prediccion_ensamble[, Predicted := 0L]
        tb_prediccion_ensamble[1:envios, Predicted := 1L]
        res_ens <- realidad_evaluar(drealidad, tb_prediccion_ensamble)
        resultados_ensamble <- rbind(
          resultados_ensamble,
          data.table(clientes = envios, ganancia_total = res_ens$total)
        )
      } else if (envios == 0) {
        resultados_ensamble <- rbind(
          resultados_ensamble,
          data.table(clientes = 0, ganancia_total = 0)
        )
      }
    }

    # Cálculo y selección basada en meseta y pico
    log_info("Calculando ganancia_meseta para la selección de corte.")
    smoothing_window_clients <- 1000

    if (!exists("cortes_evaluacion") || length(cortes_evaluacion) < 3) {
      log_warn("No se encontró 'cortes_evaluacion', usando step=100 por defecto para la meseta.")
      step_size <- 100
    } else {
      step_size <- cortes_evaluacion[3] - cortes_evaluacion[2]
    }

    n_pasos_ventana <- as.integer(round(smoothing_window_clients / step_size))
    n_ventana_meseta <- (2 * n_pasos_ventana) + 1

    log_info(paste0(
      "Cálculo de meseta: step_size=", step_size,
      ", n_pasos_ventana=", n_pasos_ventana,
      ", n_ventana_meseta_final=", n_ventana_meseta
    ))

    resultados_ensamble[, ganancia_meseta := frollmean(
      x = ganancia_total,
      n = n_ventana_meseta,
      align = "center",
      na.rm = TRUE,
      hasNA = TRUE
    )]

    # Calcular y guardar resumen del promedio (BASADO EN MESETA)
    max_ganancia_meseta <- max(resultados_ensamble$ganancia_meseta, na.rm = TRUE)
    envios_optimos_meseta_vec <- sort(unique(resultados_ensamble[ganancia_meseta == max_ganancia_meseta, clientes]))
    envios_optimos_meseta_str <- paste(envios_optimos_meseta_vec, collapse = ", ")

    # Calcular y guardar resumen del promedio (BASADO EN PICO REAL)
    max_ganancia_ens <- max(resultados_ensamble$ganancia_total, na.rm = TRUE)
    envios_optimos_real_vec <- sort(unique(resultados_ensamble[ganancia_total == max_ganancia_ens, clientes]))
    envios_optimos_real_str <- paste(envios_optimos_real_vec, collapse = ", ")

    envios_manuales <- PARAM$train_final$envios_a_generar

    envios_optimos_vector <- sort(unique(c(envios_optimos_meseta_vec, envios_optimos_real_vec, envios_manuales)))

    PARAM$train_final$envios_optimos_promedio <- envios_optimos_vector

    ruta_envios_rds <- file.path(dir_evaluacion, "envios_optimos.rds")
    saveRDS(envios_optimos_vector, file = ruta_envios_rds)
    log_info(paste("Vector de envíos óptimos (Meseta + Pico + Manuales) guardado en:", ruta_envios_rds))

    log_info(paste0(
      "Gran Ensamble (Pico Real): Ganancia Máx = ",
      format(max_ganancia_ens, big.mark = ".", decimal.mark = ","),
      " en envíos: [", envios_optimos_real_str, "] (INCLUIDO EN RDS)"
    ))

    log_info(paste0(
      "Gran Ensamble (Meseta): Ganancia Máx = ",
      format(max_ganancia_meseta, big.mark = ".", decimal.mark = ","),
      " en envíos: [", envios_optimos_meseta_str, "] (INCLUIDO EN RDS)"
    ))

    resumen_ganancias <- rbind(
      resumen_ganancias,
      data.table(
        semilla = "GRAN_ENSAMBLE_PICO",
        max_ganancia = max_ganancia_ens,
        envios_optimos = envios_optimos_real_str
      ),
      data.table(
        semilla = "GRAN_ENSAMBLE_MESETA",
        max_ganancia = max_ganancia_meseta,
        envios_optimos = envios_optimos_meseta_str
      )
    )

    PARAM_plot <- list(
      carpeta_graficos = dir_graficos,
      experimento = PARAM$experimento
    )

    GraficarCurvasEnsemble(
      lista_resultados_individuales,
      resultados_ensamble,
      PARAM_plot
    )

    ruta_resumen_txt <- file.path(dir_evaluacion, "eval_resumen.txt")
    fwrite(resumen_ganancias,
      file = ruta_resumen_txt,
      sep = "\t",
      scipen = 999
    )
    log_info(paste0("Resumen de ganancias (Gran Ensamble) guardado en: ", ruta_resumen_txt))

    # Generar archivos de salida en formato csv
    log_info("Generando archivos de salida.")
    dir_kaggle <- file.path(PARAM$experimento_folder, PARAM$carpeta_entregables)
    dir.create(dir_kaggle, showWarnings = FALSE)

    for (envio in envios_optimos_vector) {
      tb_prediccion_ensamble[, Predicted := 0L]
      tb_prediccion_ensamble[1:envio, Predicted := 1L]

      archivo_kaggle <- file.path(dir_kaggle, paste0("Evaluacion_", PARAM$experimento, "_", envio, ".csv"))

      fwrite(tb_prediccion_ensamble[, list(numero_de_cliente, Predicted)],
        file = archivo_kaggle,
        sep = ","
      )
      log_info(paste("Archivo generado:", archivo_kaggle))
    }
    # (Limpiar objetos del Gran Ensamble)
    rm(resultados_ensamble,
        tb_prediccion_ensamble)
    gc()

    # APO logic removed (moved to 9_Evaluacion_APO.R)
  },
  error = function(e) {
    log_error("######################################################")
    log_error("Se ha producido un error fatal en la Sección 11: Evaluación del Ensamble.")
    log_error(paste("Mensaje de R:", e$message))
    log_error("######################################################")
  }
)
