#--- Funciones de Evaluación ---
particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed, "L'Ecuyer-CMRG")
  bloque <- unlist(mapply(function(x, y) { rep(y, x) }, division, seq(from = start, length.out = length(division))))
  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N], by = agrupa]
}
realidad_inicializar <- function(pfuture, pparam) {
  drealidad <- pfuture[, list(numero_de_cliente, foto_mes, clase_ternaria)]
  semilla_a_usar <- ifelse(is.null(pparam$semilla_kaggle), pparam$semilla_primigenia, pparam$semilla_kaggle)
  particionar(drealidad, division = c(3, 7), agrupa = "clase_ternaria", seed = semilla_a_usar)
  return(drealidad)
}
realidad_evaluar <- function(prealidad, pprediccion) {
  prealidad_eval <- copy(prealidad)
  prealidad_eval[pprediccion, on = c("numero_de_cliente", "foto_mes"), predicted := i.Predicted]
  tbl <- prealidad_eval[, list("qty" = .N), list(fold, predicted, clase_ternaria)]
  res <- list()
  res$public <- tbl[fold == 1 & predicted == 1L, sum(qty * ifelse(clase_ternaria == "BAJA+2", 780000, -20000))] / 0.3
  res$private <- tbl[fold == 2 & predicted == 1L, sum(qty * ifelse(clase_ternaria == "BAJA+2", 780000, -20000))] / 0.7
  res$total <- tbl[predicted == 1L, sum(qty * ifelse(clase_ternaria == "BAJA+2", 780000, -20000))]
  return(res)
}

#--- Funciones de Gráficos ---
GraficarImportancia <- function(importancia, top_n = 50, ruta_grafico, subtitulo = "") {
  importancia_top <- importancia[order(-Gain)][1:min(top_n, nrow(importancia))]
  p <- ggplot(importancia_top, aes(x = reorder(Feature, Gain), y = Gain)) +
    geom_bar(stat = "identity", fill = "skyblue") + coord_flip() +
    labs(title = paste("Top", top_n, "Feature Importance"), subtitle = subtitulo, x = "Features", y = "Gain") +
    theme_minimal()
  ggsave(ruta_grafico, plot = p, width = 10, height = 8)
  log_info(paste("Gráfico de importancia de features guardado en:", ruta_grafico))
}

# Función GraficarCurvasEnsemble
GraficarCurvasEnsemble <- function(lista_resultados, PARAM_plot) {
  log_info("Iniciando la graficación de la superposición de curvas del ensemble.")

  # Convertimos la lista de data.tables en un solo data.table
  tb_todas <- rbindlist(lapply(names(lista_resultados), function(sem) {
    lista_resultados[[sem]][, semilla := as.character(sem)]
  }))

  # Calculamos la ganancia promedio por cantidad de envíos
  tb_promedio <- tb_todas[, .(ganancia_total = mean(ganancia_total)), by = clientes]

  # Encontramos el máximo de la curva promedio
  maximo_promedio <- tb_promedio[ganancia_total == max(ganancia_total)]
  maximo_promedio <- head(maximo_promedio, 1) # Primer máximo si hay empate
  
  # Creación de Etiquetas para la Leyenda (Simples)
  semillas_unicas <- unique(tb_todas$semilla)
  labels_individuales <- semillas_unicas
  label_promedio <- "Promedio"
  labels_plot <- c(labels_individuales, label_promedio)
  names(labels_plot) <- c(semillas_unicas, "Promedio")

  # Configuración de Colores
  colores_individuales <- scales::hue_pal()(length(semillas_unicas))
  names(colores_individuales) <- semillas_unicas
  colores_plot <- c(colores_individuales, "Promedio" = "black")
  
  # Generación del Gráfico ggplot
  p <- ggplot() +
    # Líneas individuales (finas y algo transparentes)
    geom_line(data = tb_todas,
              aes(x = clientes, y = ganancia_total, group = semilla, color = semilla),
              alpha = 0.5, linewidth = 0.5) +
    # Línea promedio (gruesa y negra)
    geom_line(data = tb_promedio,
              aes(x = clientes, y = ganancia_total, color = "Promedio"),
              linewidth = 1.0) + 
    # Punto máximo promedio (Rojo)
    geom_point(data = maximo_promedio,
              aes(x = clientes, y = ganancia_total),
              color = "red", size = 3, shape = 16) +
    # Anotación de la GANANCIA MÁXIMA (ARRIBA del punto)
    geom_label(data = maximo_promedio,
                aes(x = clientes, y = ganancia_total, 
                    label = paste0("Máximo\n", 
                                    format(ganancia_total, big.mark = ".", decimal.mark = ","))),
                vjust = -0.5, # Posiciona justo ARRIBA del punto
                nudge_y = 10000000, # Empuja la etiqueta hacia ARRIBA
                fill = "white", color = "red", fontface = "bold",
                label.padding = unit(0.3, "lines")) +
    # Anotación de los ENVÍOS (con flecha, ABAJO)
    geom_text_repel(data = maximo_promedio,
                aes(x = clientes, y = ganancia_total,
                    label = format(clientes, big.mark = ".", decimal.mark = ",")),
                color = "black",
                fontface = "bold",
                size = 3.5,
                
                # --- Ajustes para la flecha y posición ---
                nudge_x = 5000,  # Empujar a la derecha
                nudge_y = -15000000, # Empujar hacia ABAJO
                
                segment.color = "grey30", 
                segment.size = 0.5,       
                min.segment.length = 0.5, 
                point.padding = 0.5,      
                box.padding = 0.5         
    ) +
    scale_y_continuous(labels = scales::comma, 
                       expand = expansion(mult = c(0.05, 0.15))) + 
    scale_x_continuous(labels = scales::comma) + 
    scale_color_manual(name = "Modelo",
                      values = colores_plot,
                      labels = labels_plot) +
    labs(
      title = paste0("Ganancia Acumulada (Semillas y Ensamble) - ", PARAM_plot$experimento),
      x = "Clientes Contactados (Envíos)",
      y = "Ganancia Acumulada"
    ) +
    theme_minimal() +
    # Mover leyenda a la derecha y con letra pequeña
    theme(
      legend.position = "right",
      legend.text = element_text(size = 8), 
      legend.title = element_text(size = 9, face = "bold"),
      legend.key.size = unit(0.5, "lines"), 
      plot.margin = margin(10, 10, 10, 10)
    ) +
    # Asegurar que las líneas en la leyenda sean visibles
    guides(color = guide_legend(override.aes = list(alpha = 1, linewidth = 1.5))) 

  # Nombre de archivo corto
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
tryCatch({
  log_info("Iniciando la evaluación del ensamble en datos de testing.")
  
  # Cargar Hiperparámetros Fijos
  log_info("Cargando hiperparámetros fijos.")
  
  if (is.null(PARAM$lgbm_z)) {
    stop("No se encontraron los parámetros en PARAM$lgbm_z. Asegúrate de definirlos en main.R.")
  }
  param_mejores <- PARAM$lgbm_z
  
  log_info("Hiperparámetros fijos cargados:")
  log_info(paste(capture.output(print(param_mejores)), collapse = "\n"))
  
  # Preparar Datos de Entrenamiento
  if (!exists("campos_buenos")) {
    stop("El objeto 'campos_buenos' no existe. Asegúrate de que 8_Modelado.R se haya ejecutado.")
  }

  log_info("Preparando dtrain para evaluación (con canaritos).")
  # Creamos una copia local para agregar canarios solo para este script
  dataset_train_eval <- dataset[training == 1L]
  
  cols0 <- copy(colnames(dataset_train_eval))
  filas <- nrow(dataset_train_eval)
  for (i in seq_len(PARAM$qcanaritos)) {
    dataset_train_eval[, paste0("canarito_", i) := runif(filas)]
  }
  # Las columnas canaritos mandatoriamente van al comienzo del dataset
  cols_canaritos <- copy(setdiff(colnames(dataset_train_eval), cols0))
  setcolorder(dataset_train_eval, c(cols_canaritos, cols0))
  
  # Definimos campos_buenos_z que incluye los canarios
  campos_buenos_z <- setdiff(colnames(dataset_train_eval), PARAM$trainingstrategy$campos_entrenar)
  
  log_info("Creando dtrain con canaritos.")
  dtrain <- lgb.Dataset(
    data = data.matrix(dataset_train_eval[, campos_buenos_z, with = FALSE]),
    label = dataset_train_eval[, clase01],
    free_raw_data = FALSE
  )
  rm(dataset_train_eval) # Liberar memoria
  gc()

  # Preparar Datos de Testing (con Canarios)
  log_info("Preparando datos de evaluación (testing) (con canaritos).")
  dfuture <- dataset[foto_mes %in% PARAM$eval_ensamble$mes_testing]
  
  if (nrow(dfuture) == 0) {
    stop(paste("No se encontraron datos para el período de testing:", PARAM$eval_ensamble$mes_testing))
  }
  
  # Agregamos canarios a dfuture
  filas_future <- nrow(dfuture)
  for (i in seq_len(PARAM$qcanaritos)) {
    dfuture[, paste0("canarito_", i) := runif(filas_future)]
  }
  
  mfuture <- data.matrix(dfuture[, campos_buenos_z, with = FALSE])
  drealidad <- realidad_inicializar(dfuture, PARAM)
  cortes_evaluacion <- seq(0, 20000, by = 100)
  
  # Crear directorios--
  dir_graficos <- file.path(PARAM$experimento_folder, PARAM$carpeta_graficos)
  dir.create(dir_graficos, recursive = TRUE, showWarnings = FALSE)
  dir_evaluacion <- file.path(PARAM$experimento_folder, PARAM$carpeta_evaluacion)
  dir.create(dir_evaluacion, recursive = TRUE, showWarnings = FALSE)
  
  log_info(paste("Directorio de Gráficos creado en:", dir_graficos))
  log_info(paste("Directorio de Evaluación creado en:", dir_evaluacion))

  # Feature Importance de la Semilla Primigenia 
  log_info(paste("Generando Feature Importance para la semilla primigenia:", PARAM$semilla_primigenia))
  
  param_primigenia <- copy(param_mejores)
  param_primigenia$seed <- PARAM$semilla_primigenia
  
  modelo_primigenia <- lgb.train(data = dtrain, param = param_primigenia)
  
  imp_primigenia <- lgb.importance(modelo_primigenia, percentage = TRUE)
  imp_ordenada_primigenia <- imp_primigenia[order(-Gain)]
  
  ruta_csv_imp_pri <- file.path(dir_evaluacion, "fi_primigenia.csv")
  fwrite(imp_ordenada_primigenia, file = ruta_csv_imp_pri)
  log_info(paste("Importancia de features (primigenia) guardada en:", ruta_csv_imp_pri))
  
  ruta_grafico_imp_pri <- file.path(dir_graficos, "fi_primigenia.png")
  GraficarImportancia(imp_ordenada_primigenia, 
                      top_n = PARAM$trainingstrategy$importancias, 
                      ruta_grafico = ruta_grafico_imp_pri,
                      subtitulo = paste(PARAM$experimento, "- Semilla Primigenia:", PARAM$semilla_primigenia))
  
  rm(modelo_primigenia, imp_primigenia, imp_ordenada_primigenia)
  gc()

  # Bucle de Entrenamiento y Evaluación del Ensamble
  set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")
  if(!exists("primos")) primos <- generate_primes(min = 100000, max = 1000000)
  PARAM$eval_ensamble$semillas <- sample(primos)[seq( PARAM$eval_ensamble$ksemillerio )]
    
  semillas_a_evaluar <- PARAM$eval_ensamble$semillas
  
  log_info(paste0("Evaluando ", length(semillas_a_evaluar), " semilla(s)."))
  log_info(paste("Semillas a evaluar:", paste(semillas_a_evaluar, collapse = ", ")))

  lista_predicciones <- list()
  lista_resultados_individuales <- list()
  lista_importancia <- list() 

  resumen_ganancias <- data.table(
    semilla = character(),
    max_ganancia = numeric(),
    envios_optimos = character() 
  )
  
  param_entrenamiento <- copy(param_mejores)
  
  log_info(paste0("Iniciando evaluación de ", length(semillas_a_evaluar), " semillas..."))
  
  for (semilla_actual in semillas_a_evaluar) {
    log_info(paste0("--- Procesando semilla: ", semilla_actual, " ---"))

    param_entrenamiento$seed <- semilla_actual
    modelo <- lgb.train(data = dtrain, param = param_entrenamiento)

    imp <- lgb.importance(modelo, percentage = TRUE)
    lista_importancia[[as.character(semilla_actual)]] <- imp

    imp_ordenada_ind <- imp[order(-Gain)]
    
    ruta_csv_ind <- file.path(dir_evaluacion, paste0("fi_semilla_", semilla_actual, ".csv"))
    fwrite(imp_ordenada_ind, file = ruta_csv_ind)
    
    ruta_grafico_ind <- file.path(dir_graficos, paste0("fi_semilla_", semilla_actual, ".png"))
    GraficarImportancia(imp_ordenada_ind, 
                        top_n = PARAM$trainingstrategy$importancias, 
                        ruta_grafico = ruta_grafico_ind,
                        subtitulo = paste(PARAM$experimento, "- Semilla:", semilla_actual))
    
    rm(imp_ordenada_ind) 

    prediccion_individual <- predict(modelo, mfuture)
    tb_pred_individual <- dfuture[, list(numero_de_cliente, foto_mes)]
    tb_pred_individual[, prob := prediccion_individual]
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
      format(max_ganancia_ind, big.mark = ".", decimal.mark = ","),
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
  }
  
  log_info("Evaluación de semillas individuales completa.")

  # Procesar Feature Importance Promediado
  log_info("Calculando Feature Importance promediada del ensamble.")
  
  imp_todas <- rbindlist(lista_importancia)
  imp_promediada <- imp_todas[, .(Gain = mean(Gain), Cover = mean(Cover), Frequency = mean(Frequency)), by = Feature]
  imp_ordenada <- imp_promediada[order(-Gain)]
  
  ruta_csv_imp <- file.path(dir_evaluacion, "fi_ensemble.csv")
  fwrite(imp_ordenada, file = ruta_csv_imp)
  log_info(paste("Importancia de features (promediada) guardada en:", ruta_csv_imp))
  
  ruta_grafico_imp <- file.path(dir_graficos, "fi_ensemble.png")
  GraficarImportancia(imp_ordenada, 
                      top_n = PARAM$trainingstrategy$importancias, 
                      ruta_grafico = ruta_grafico_imp,
                      subtitulo = paste(PARAM$experimento, "- Promedio de", length(semillas_a_evaluar), "semillas"))
  
  # 4. Evaluación del Ensamble Promediado
  log_info("Evaluando el ensamble promediado...")
  
  predicciones_todas <- rbindlist(lista_predicciones)
  tb_prediccion_ensamble <- predicciones_todas[, .(prob = mean(prob)), by = .(numero_de_cliente, foto_mes)]
  
  resultados_ensamble <- data.table()
  setorder(tb_prediccion_ensamble, -prob)
  
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
  
  max_ganancia_ens <- max(resultados_ensamble$ganancia_total, na.rm = TRUE)
  envios_optimos_ens <- resultados_ensamble[ganancia_total == max_ganancia_ens, clientes]
  envios_optimos_ens_str <- paste(sort(unique(envios_optimos_ens)), collapse = ", ")
  
  envios_optimos_ens_vec <- sort(unique(envios_optimos_ens))
  
  envios_manuales <- c(10500, 11000) # Dejamos los envíos manuales
  envios_optimos_vector <- sort(unique(c(envios_optimos_ens_vec, envios_manuales)))
  
  PARAM$eval_ensamble$envios_optimos_promedio <- envios_optimos_vector

  ruta_envios_rds <- file.path(dir_evaluacion, "envios_optimos.rds")
  saveRDS(envios_optimos_vector, file = ruta_envios_rds)
  log_info(paste("Vector de envíos óptimos guardado en:", ruta_envios_rds))

  log_info(paste0("Ensamble Promedio: Ganancia Máx = ", 
                 format(max_ganancia_ens, big.mark = ".", decimal.mark = ","), 
                 " en envíos: [", envios_optimos_ens_str, "]"))

  resumen_ganancias <- rbind(
    resumen_ganancias,
    data.table(
      semilla = "PROMEDIO",
      max_ganancia = max_ganancia_ens,
      envios_optimos = envios_optimos_ens_str
    )
  )

  # Generar Salidas
  PARAM_plot <- list(
    carpeta_graficos = dir_graficos,
    experimento = PARAM$experimento
  )

  GraficarCurvasEnsemble(lista_resultados_individuales, PARAM_plot)

  ruta_resumen_txt <- file.path(dir_evaluacion, "eval_resumen.txt")
  fwrite(resumen_ganancias, 
         file = ruta_resumen_txt, 
         sep = "\t",
         scipen = 999)
  log_info(paste0("Resumen de ganancias guardado en: ", ruta_resumen_txt))

}, error = function(e) {
  log_error("######################################################")
  log_error("Se ha producido un error fatal en la Sección 10: Evaluación del Ensamble.")
  log_error(paste("Mensaje de R:", e$message))
  log_error("######################################################")
})