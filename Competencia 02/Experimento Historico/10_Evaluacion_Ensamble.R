#--- Funciones de Evaluación ---
# Función particionar (requerida por realidad_inicializar)
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

# Función realidad_inicializar (Adaptada para usar semilla_primigenia de PARAM)
realidad_inicializar <- function(pfuture, pparam) {
  drealidad <- pfuture[, list(numero_de_cliente, foto_mes, clase_ternaria)]
  
  # Adaptación: Usamos semilla_primigenia si semilla_kaggle no está definida
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

# Función realidad_evaluar
realidad_evaluar <- function(prealidad, pprediccion) {
  # Asegurarse de que prealidad es una copia para no modificar el original
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
# Función para graficar y guardar la importancia de features
GraficarImportancia <- function(importancia, top_n = 50, ruta_grafico, subtitulo = "") {
  
  # Ordenar por ganancia y tomar top_n
  importancia_top <- importancia[order(-Gain)][1:min(top_n, nrow(importancia))]
  
  # Crear el gráfico
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
  
  # Guardar el gráfico
  ggsave(ruta_grafico, plot = p, width = 10, height = 8)
  log_info(paste("Gráfico de importancia de features guardado en:", ruta_grafico))
}

# Función GraficarCurvasEnsemble
GraficarCurvasEnsemble <- function(lista_resultados, tb_resultados_ensamble, PARAM_plot) {
  log_info("Iniciando la graficación de la superposición de curvas del ensemble.")

  # DATOS DE SEMILLAS INDIVIDUALES
  tb_todas <- rbindlist(lapply(names(lista_resultados), function(sem) {
    lista_resultados[[sem]][, semilla := as.character(sem)]
  }))

  # DATOS DEL PROMEDIO VISUAL (Promedio(Ganancias))
  tb_promedio_visual <- tb_todas[, .(ganancia_total = mean(ganancia_total)), by = clientes]

  # DATOS DEL ENSAMBLE REAL (Ganancia(Promedio(Prob)))
  # Esta es la tabla que se usa para eval_resumen.txt
  setnames(tb_resultados_ensamble, "ganancia_total", "ganancia_ensamble_real")

  # Encontramos el máximo de la curva del ENSAMBLE REAL
  maximo_promedio <- tb_resultados_ensamble[ganancia_ensamble_real == max(ganancia_ensamble_real)]
  maximo_promedio <- head(maximo_promedio, 1) # Primer máximo si hay empate
  
  # Creación de Etiquetas para la Leyenda
  semillas_unicas <- unique(tb_todas$semilla)
  
  # Definimos nombres para las nuevas líneas
  label_ensamble_real <- "Ensamble Real (Negro)"
  label_promedio_visual <- "Promedio Visual (Azul)"

  labels_plot <- c(semillas_unicas, label_ensamble_real, label_promedio_visual)
  names(labels_plot) <- c(semillas_unicas, "Ensamble Real", "Promedio Visual")

  # Configuración de Colores
  colores_individuales <- scales::hue_pal()(length(semillas_unicas))
  names(colores_individuales) <- semillas_unicas
  
  # Forzamos colores: Negro para el real, Azul para el visual
  colores_plot <- c(
    colores_individuales, 
    "Ensamble Real" = "black", 
    "Promedio Visual" = "blue"
  )
  
  # Generación del Gráfico ggplot
  p <- ggplot() +
    # Líneas individuales (finas y algo transparentes)
    geom_line(data = tb_todas,
              aes(x = clientes, y = ganancia_total, group = semilla, color = semilla),
              alpha = 0.5, linewidth = 0.5) +
    # Línea PROMEDIO VISUAL (Azul y Punteada)
    geom_line(data = tb_promedio_visual,
              aes(x = clientes, y = ganancia_total, color = "Promedio Visual"),
              linewidth = 0.8, linetype = "dashed") + 
    # Línea ENSAMBLE REAL (Negra y Gruesa)
    geom_line(data = tb_resultados_ensamble,
              aes(x = clientes, y = ganancia_ensamble_real, color = "Ensamble Real"),
              linewidth = 1.0) + 
    # Punto máximo promedio (Rojo) - BASADO EN EL ENSAMBLE REAL
    geom_point(data = maximo_promedio,
              aes(x = clientes, y = ganancia_ensamble_real),
              color = "red", size = 3, shape = 16) +
    # Anotación de la GANANCIA MÁXIMA (ARRIBA) - BASADO EN EL ENSAMBLE REAL
    geom_label(data = maximo_promedio,
                aes(x = clientes, y = ganancia_ensamble_real, 
                    label = paste0("Máximo\n", 
                                    format(ganancia_ensamble_real, big.mark = ".", decimal.mark = ","))),
                vjust = -0.5, 
                nudge_y = 10000000, 
                fill = "white", color = "red", fontface = "bold",
                label.padding = unit(0.3, "lines")) +
    # Anotación de los ENVÍOS (ABAJO) - BASADO EN EL ENSAMBLE REAL
    geom_text_repel(data = maximo_promedio,
                aes(x = clientes, y = ganancia_ensamble_real,
                    label = format(clientes, big.mark = ".", decimal.mark = ",")),
                color = "black",
                fontface = "bold",
                size = 3.5,
                nudge_y = -30000000, 
                direction = "y",
                ylim = c(NA, maximo_promedio$ganancia_ensamble_real - 10000), 
                segment.color = "grey30", 
                segment.size = 0.5,       
                min.segment.length = 0, 
                point.padding = 0.5,    
                box.padding = 0.5         
    ) +
    scale_y_continuous(labels = scales::comma, 
                       expand = expansion(mult = c(0.05, 0.15))) + 
    scale_x_continuous(labels = scales::comma) + 
    # Asignamos los colores y etiquetas actualizados
    scale_color_manual(name = "Modelo",
                      values = colores_plot,
                      labels = labels_plot) +
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
    # Asegurar que las líneas en la leyenda sean visibles
    guides(color = guide_legend(override.aes = list(
      alpha = 1, 
      linewidth = 1.5,
      # Hacemos que la línea del "Promedio Visual" aparezca punteada en la leyenda
      linetype = c(rep("solid", length(semillas_unicas) + 1), "dashed") 
    ))) 

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
  
  param_lgbm <- union(names(PARAM$lgbm$param_fijos), names(PARAM$hipeparametertuning$hs$pars))
  param_mejores <- as.list(tb_BO[1, param_lgbm, with = FALSE])
  
  log_info("Hiperparámetros óptimos cargados:")
  log_info(paste(capture.output(print(param_mejores)), collapse = "\n"))
  
  # Preparar Datos
  if (!exists("dtrain") || !exists("campos_buenos")) {
    stop("Los objetos 'dtrain' o 'campos_buenos' no existen. Asegúrate de que 8_Modelado.R se haya ejecutado.")
  }

  log_info("Preparando datos de evaluación (testing).")
  dfuture <- dataset[foto_mes %in% PARAM$eval_ensamble$mes_testing]
  
  if (nrow(dfuture) == 0) {
    stop(paste("No se encontraron datos para el período de testing:", PARAM$eval_ensamble$mes_testing))
  }
  
  mfuture <- data.matrix(dfuture[, campos_buenos, with = FALSE])
  drealidad <- realidad_inicializar(dfuture, PARAM)
  cortes_evaluacion <- seq(0, 20000, by = 100)
  
  # Crear directorios
  # Directorio para Gráficos (PNGs)
  dir_graficos <- file.path(PARAM$experimento_folder, PARAM$carpeta_graficos)
  dir.create(dir_graficos, recursive = TRUE, showWarnings = FALSE)
  
  # Directorio para Evaluación (CSVs, TXT)
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
  
  # Guardar CSV en /Evaluacion--
  ruta_csv_imp_pri <- file.path(dir_evaluacion, "fi_primigenia.csv")
  fwrite(imp_ordenada_primigenia, file = ruta_csv_imp_pri)
  log_info(paste("Importancia de features (primigenia) guardada en:", ruta_csv_imp_pri))
  
  # Guardar Gráfico (PNG) en /Plots
  ruta_grafico_imp_pri <- file.path(dir_graficos, "fi_primigenia.png")
  GraficarImportancia(imp_ordenada_primigenia, 
                      top_n = PARAM$trainingstrategy$importancias, 
                      ruta_grafico = ruta_grafico_imp_pri,
                      subtitulo = paste(PARAM$experimento, "- Semilla Primigenia:", PARAM$semilla_primigenia))
  
  # Limpiar modelo
  rm(modelo_primigenia, imp_primigenia, imp_ordenada_primigenia)
  gc()

  # Bucle de Entrenamiento y Evaluación del Ensamble
  set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")
  PARAM$eval_ensamble$semillas <- sample(primos)[seq( PARAM$eval_ensamble$ksemillerio )]
    
  semillas_a_evaluar <- PARAM$eval_ensamble$semillas
  
  log_info(paste0("Evaluando ", length(semillas_a_evaluar), 
                 " semilla(s)."))
  log_info(paste(
    "Semillas a evaluar:", 
    paste(semillas_a_evaluar, collapse = ", ")
  ))

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

    # Acumular Feature Importance
    imp <- lgb.importance(modelo, percentage = TRUE)
    lista_importancia[[as.character(semilla_actual)]] <- imp

    # Guardar FI individual de esta semilla
    imp_ordenada_ind <- imp[order(-Gain)]
    
    # Guardar CSV individual en /Evaluacion
    ruta_csv_ind <- file.path(dir_evaluacion, paste0("fi_semilla_", semilla_actual, ".csv"))
    fwrite(imp_ordenada_ind, file = ruta_csv_ind)
    
    # Guardar Gráfico individual en /Plots
    ruta_grafico_ind <- file.path(dir_graficos, paste0("fi_semilla_", semilla_actual, ".png"))
    GraficarImportancia(imp_ordenada_ind, 
                        top_n = PARAM$trainingstrategy$importancias, 
                        ruta_grafico = ruta_grafico_ind,
                        subtitulo = paste(PARAM$experimento, "- Semilla:", semilla_actual))
    
    rm(imp_ordenada_ind) # Limpiar

    # Predecir sobre datos de testing
    prediccion_individual <- predict(modelo, mfuture)
    tb_pred_individual <- dfuture[, list(numero_de_cliente, foto_mes)]
    tb_pred_individual[, prob := prediccion_individual]
    lista_predicciones[[as.character(semilla_actual)]] <- tb_pred_individual

    # Evaluar en todos los cortes
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

    # Calcular y guardar resumen de la semilla
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

    # Liberamos objetos grandes del bucle antes de la siguiente iteración
    # No borramos 'resultados_individual' porque se añade a la lista
    rm(modelo, imp, imp_ordenada_ind, prediccion_individual, tb_pred_individual)
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

  # Guardar CSV en /Evaluacion
  ruta_csv_imp <- file.path(dir_evaluacion, "fi_ensemble.csv")
  fwrite(imp_ordenada, file = ruta_csv_imp)
  log_info(paste("Importancia de features (promediada) guardada en:", ruta_csv_imp))
  
  # Guardar Gráfico (PNG) en /Plots
  ruta_grafico_imp <- file.path(dir_graficos, "fi_ensemble.png")
  GraficarImportancia(imp_ordenada, 
                      top_n = PARAM$trainingstrategy$importancias, 
                      ruta_grafico = ruta_grafico_imp,
                      subtitulo = paste(PARAM$experimento, "- Promedio de", length(semillas_a_evaluar), "semillas"))
  
  # 4. Evaluación del Ensamble Promediado
  log_info("Evaluando el ensamble promediado...")
  
  predicciones_todas <- rbindlist(lista_predicciones)
  tb_prediccion_ensamble <- predicciones_todas[, .(prob = mean(prob)), by = .(numero_de_cliente, foto_mes)]
  
  rm(lista_predicciones, predicciones_todas)
  gc()

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
  
  # Calcular y guardar resumen del promedio
  max_ganancia_ens <- max(resultados_ensamble$ganancia_total, na.rm = TRUE)
  envios_optimos_ens <- resultados_ensamble[ganancia_total == max_ganancia_ens, clientes]
  envios_optimos_ens_str <- paste(sort(unique(envios_optimos_ens)), collapse = ", ")
  
  # Obtiene el vector de envíos del ensamble
  envios_optimos_ens_vec <- sort(unique(envios_optimos_ens))
  
  # Envios hardcodeados
  envios_manuales <- c(10500, 11000) 

  # Combinar ambos vectores y obtener únicos
  envios_optimos_vector <- sort(unique(c(envios_optimos_ens_vec, envios_manuales)))
  
  # Guardar este vector en el PARAM local
  PARAM$eval_ensamble$envios_optimos_promedio <- envios_optimos_vector

  # Guardar el VECTOR en disco
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
    carpeta_graficos = dir_graficos, # Usar la variable ya creada
    experimento = PARAM$experimento
  )

  # Generar Gráfico
  GraficarCurvasEnsemble(lista_resultados_individuales, 
                         resultados_ensamble, 
                         PARAM_plot)

  # Guardar Resumen TXT en /Evaluacion
  ruta_resumen_txt <- file.path(dir_evaluacion, "eval_resumen.txt")
  fwrite(resumen_ganancias, 
         file = ruta_resumen_txt, 
         sep = "\t",
         scipen = 999)
  log_info(paste0("Resumen de ganancias (leíble para Wilcoxon) guardado en: ", ruta_resumen_txt))

}, error = function(e) {
  log_error("######################################################")
  log_error("Se ha producido un error fatal en la Sección 10: Evaluación del Ensamble.")
  log_error(paste("Mensaje de R:", e$message))
  log_error("######################################################")
})