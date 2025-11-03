log_info("Inicio de la evaluación del ensamble")
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

# Función GraficarCurvasEnsemble
GraficarCurvasEnsemble <- function(lista_resultados, PARAM_plot) {
  log_info("Iniciando la graficación de la superposición de curvas del ensemble.")

  # Convertimos la lista de data.tables en un solo data.table
  tb_todas <- rbindlist(lapply(names(lista_resultados), function(sem) {
    lista_resultados[[sem]][, semilla := as.character(sem)]
  }))

  # Calculamos la ganancia promedio por cantidad de envíos
  tb_promedio <- tb_todas[, .(ganancia_total = mean(ganancia_total)), by = clientes]

  # Encontramos los máximos para cada semilla individual
  maximos_individuales <- tb_todas[, .SD[ganancia_total == max(ganancia_total)], by = semilla]
  # Nos quedamos con el primer máximo si hay empates
  maximos_individuales <- maximos_individuales[, head(.SD, 1), by = semilla] 

  # Encontramos el máximo de la curva promedio
  maximo_promedio <- tb_promedio[ganancia_total == max(ganancia_total)]
  maximo_promedio <- head(maximo_promedio, 1) # Primer máximo si hay empate

  # --- Creación de Etiquetas para la Leyenda ---
  labels_individuales <- sapply(maximos_individuales$semilla, function(sem) {
    max_info <- maximos_individuales[semilla == sem]
    paste0("S ", sem, 
          ": G ", format(max_info$ganancia_total, big.mark = ".", decimal.mark = ","),
          " (E ", max_info$clientes, ")")
  })

  label_promedio <- paste0("Promedio: G ",
                          format(maximo_promedio$ganancia_total, big.mark = ".", decimal.mark = ","),
                          " (E ", maximo_promedio$clientes, ")")

  # --- Configuración de Colores y Leyendas para el Plot ---
  colores_individuales <- scales::hue_pal()(nrow(maximos_individuales))
  names(colores_individuales) <- maximos_individuales$semilla
  
  colores_plot <- c(colores_individuales, "Promedio" = "black")
  labels_plot <- c(labels_individuales, "Promedio" = label_promedio)
  names(labels_plot) <- c(names(colores_individuales), "Promedio")

  # Creación de la carpeta de gráficos
  dir.create(PARAM_plot$carpeta_graficos, showWarnings = FALSE)

  # --- Generación del Gráfico ggplot ---
  p <- ggplot() +
    # Líneas individuales
    geom_line(data = tb_todas,
              aes(x = clientes, y = ganancia_total, group = semilla, color = semilla),
              alpha = 0.5, linewidth = 1) +
    # Línea promedio
    geom_line(data = tb_promedio,
              aes(x = clientes, y = ganancia_total, color = "Promedio"),
              linewidth = 1) +
    # Puntos máximos individuales
    geom_point(data = maximos_individuales,
              aes(x = clientes, y = ganancia_total, color = semilla),
              size = 3, alpha = 0.7) +
    # Punto máximo promedio
    geom_point(data = maximo_promedio,
              aes(x = clientes, y = ganancia_total, color = "Promedio"),
              size = 4, shape = 18) +
    # Etiquetas de envíos (individuales)
    ggrepel::geom_text_repel(data = maximos_individuales,
                            aes(x = clientes, y = ganancia_total, label = clientes),
                            color = "black", size = 3.0,
                            box.padding = 0.3, point.padding = 0.3,
                            min.segment.length = 0) +
    # Etiqueta de envío (promedio)
    ggrepel::geom_text_repel(data = maximo_promedio,
                            aes(x = clientes, y = ganancia_total, label = clientes),
                            color = "black", size = 4.0, fontface = "bold",
                            box.padding = 0.5, point.padding = 0.5,
                            min.segment.length = 0) +
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(name = "Curvas y sus Máximos",
                      values = colores_plot,
                      labels = labels_plot) +
    labs(
      title = paste0("Curvas de Ganancia del Ensemble y su Promedio (Validación Interna)"),
      subtitle = paste0("Experimento: ", PARAM_plot$experimento),
      x = "Clientes Contactados (Envíos)",
      y = "Ganancia Total"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.margin = margin(10, 10, 10, 10)) +
    guides(color = guide_legend(ncol = 2)) # Ajusta el número de columnas de la leyenda

  # Guardar el gráfico
  ruta_grafico <- paste0(PARAM_plot$carpeta_graficos, "eval_ensemble_curvas_", PARAM_plot$experimento, ".png")
  ggsave(
    ruta_grafico,
    plot = p,
    width = 12,
    height = 8
  )

  log_info(paste0("Gráfico de superposición de curvas del ensemble guardado en: ", ruta_grafico))
}

#------------------------------------------------------
# Sección Principal: Evaluación del Ensamble
#------------------------------------------------------
tryCatch({
  log_info("Iniciando la evaluación del ensamble en datos de testing.")
  
  # --- 1. Cargar Mejores Hiperparámetros ---
  log_info("Cargando mejores hiperparámetros de BO_log.txt")
  if (!file.exists("BO_log.txt")) {
    stop("No se encontró el archivo BO_log.txt. Asegúrate de que 9_Optimizacion_Bayesiana.R se haya ejecutado.")
  }
  
  tb_BO <- fread("BO_log.txt")
  setorder(tb_BO, -metrica) # Ordenar por la métrica de la BO
  
  # Identificar los parámetros de LightGBM
  param_lgbm <- union(names(PARAM$lgbm$param_fijos), names(PARAM$hipeparametertuning$hs$pars))
  
  # Seleccionar los mejores parámetros
  param_mejores <- as.list(tb_BO[1, param_lgbm, with = FALSE])
  
  log_info("Hiperparámetros óptimos cargados:")
  log_info(paste(capture.output(print(param_mejores)), collapse = "\n"))
  
  # --- 2. Preparar Datos ---
  if (!exists("dtrain") || !exists("campos_buenos")) {
    stop("Los objetos 'dtrain' o 'campos_buenos' no existen. Asegúrate de que 8_Modelado.R se haya ejecutado.")
  }

  log_info("Preparando datos de evaluación (testing).")
  dfuture <- dataset[foto_mes %in% PARAM$trainingstrategy$testing]
  
  if (nrow(dfuture) == 0) {
    stop(paste("No se encontraron datos para el período de testing:", PARAM$trainingstrategy$testing))
  }
  
  # Matrix de predicción
  mfuture <- data.matrix(dfuture[, campos_buenos, with = FALSE])
  
  # Datos de realidad para evaluar
  drealidad <- realidad_inicializar(dfuture, PARAM)
  
  # Definir los cortes de evaluación (de Prueba_16.R)
  cortes_evaluacion <- seq(0, 20000, by = 100)
  
  # --- 3. Bucle de Entrenamiento y Evaluación del Ensamble ---
  semillas_a_evaluar <- PARAM$BO$semillas
  if (is.null(semillas_a_evaluar) || length(semillas_a_evaluar) == 0) {
    stop("No se encontraron semillas en PARAM$BO$semillas. Asegúrate de que 9_Optimizacion_Bayesiana.R se haya ejecutado.")
  }
  
  lista_predicciones <- list()
  lista_resultados_individuales <- list()
  resumen_ganancias <- data.table(
    semilla = character(),
    max_ganancia = numeric(),
    envios_optimos = character()
  )
  
  param_entrenamiento <- copy(param_mejores)
  
  log_info(paste0("Iniciando evaluación de ", length(semillas_a_evaluar), " semillas..."))
  
  for (semilla_actual in semillas_a_evaluar) {
    log_info(paste0("--- Procesando semilla: ", semilla_actual, " ---"))
    
    # Asignar semilla
    param_entrenamiento$seed <- semilla_actual
    
    # Entrenar modelo
    # 'dtrain' es el dataset de training definido en 8_Modelado.R
    modelo <- lgb.train(data = dtrain, param = param_entrenamiento)
    
    # Predecir sobre datos de testing
    prediccion_individual <- predict(modelo, mfuture)
    
    tb_pred_individual <- dfuture[, list(numero_de_cliente, foto_mes)]
    tb_pred_individual[, prob := prediccion_individual]
    
    # Guardar predicción
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
    
    # Guardar la curva de ganancia de esta semilla
    lista_resultados_individuales[[as.character(semilla_actual)]] <- resultados_individual
    
    # Calcular y guardar resumen de la semilla
    max_ganancia_ind <- max(resultados_individual$ganancia_total, na.rm = TRUE)
    envios_optimos_ind <- resultados_individual[ganancia_total == max_ganancia_ind, clientes]
    envios_optimos_str <- paste(sort(unique(envios_optimos_ind)), collapse = ", ")
    
    log_info(paste0("Semilla ", semilla_actual, ": Ganancia Máx = ", 
                   format(max_ganancia_ind, big.mark = ".", decimal.mark = ","), 
                   " en envíos: [", envios_optimos_str, "]"))
                   
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
  
  # --- 4. Evaluación del Ensamble Promediado ---
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
  
  # Añadir la curva promedio a la lista para el gráfico
  lista_resultados_individuales[["PROMEDIO"]] <- resultados_ensamble
  
  # Calcular y guardar resumen del promedio
  max_ganancia_ens <- max(resultados_ensamble$ganancia_total, na.rm = TRUE)
  envios_optimos_ens <- resultados_ensamble[ganancia_total == max_ganancia_ens, clientes]
  envios_optimos_ens_str <- paste(sort(unique(envios_optimos_ens)), collapse = ", ")
  # Guardar envíos óptimos en PARAM
  PARAM$eval_ensamble$envios_optimos_promedio <- sort(unique(envios_optimos_ens))

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

  # --- 5. Generar Salidas ---

  # Crear carpeta de gráficos
  dir.create("Plots", showWarnings = FALSE)
  PARAM_plot <- list(
    carpeta_graficos = "Plots/",
    experimento = PARAM$experimento
  )

  # Generar Gráfico
  # Modificamos la lista para que "PROMEDIO" no se grafique como individual
  lista_plot <- copy(lista_resultados_individuales)
  lista_plot[["PROMEDIO"]] <- NULL # La función Graficar... calcula su propio promedio

  GraficarCurvasEnsemble(lista_plot, PARAM_plot)

  # Guardar Resumen TXT
  ruta_resumen_txt <- "evaluacion_ensemble_resumen.txt"
  fwrite(resumen_ganancias, file = ruta_resumen_txt, sep = "\t")
  log_info(paste0("Resumen de ganancias guardado en: ", ruta_resumen_txt))

}, error = function(e) {
  log_error("######################################################")
  log_error("Se ha producido un error fatal en la Sección 10: Evaluación del Ensamble.")
  log_error(paste("Mensaje de R:", e$message))
  log_error("######################################################")
})

log_info("Fin de la evaluación del ensamble.")