log_info("Inicio Evaluación del Ensamble")

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
    paste0("S ", sem)
  })

  label_promedio <- paste0("Promedio: G ",
                          format(maximo_promedio$ganancia_total, big.mark = ".", decimal.mark = ","),
                          " (E ", maximo_promedio$clientes, ")")

  # --- Configuración de Colores y Leyendas para el Plot ---
  colores_individuales <- scales::hue_pal()(nrow(maximos_individuales))
  names(colores_individuales) <- maximos_individuales$semilla
  
  colores_plot <- c(colores_individuales, "Promedio" = "black")
  labels_plot <- c(labels_individuales, "Promedio" = "Promedio (Negro)")
  names(labels_plot) <- c(names(colores_individuales), "Promedio")

  # --- Generación del Gráfico ggplot ---
  p <- ggplot() +
    # Líneas individuales
    geom_line(data = tb_todas,
              aes(x = clientes, y = ganancia_total, group = semilla, color = semilla),
              alpha = 0.4, linewidth = 0.8) +
    # Línea promedio
    geom_line(data = tb_promedio,
              aes(x = clientes, y = ganancia_total, color = "Promedio"),
              linewidth = 1.2) + # Más gruesa
    # Punto máximo promedio
    geom_point(data = maximo_promedio,
              aes(x = clientes, y = ganancia_total, color = "Promedio"),
              size = 4, shape = 18) +
    
    # Añadir anotación del máximo promedio (como en la imagen de referencia)
    geom_label(data = maximo_promedio,
              aes(x = clientes, y = ganancia_total, 
                  label = paste0("Máximo\n", format(ganancia_total, big.mark = ".", decimal.mark = ","))),
              vjust = -0.5, # Mover etiqueta arriba del punto
              fill = "white", color = "red", fontface = "bold") +

    scale_y_continuous(labels = scales::comma) +
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
    theme(legend.position = "right",
          plot.margin = margin(10, 10, 10, 10)) +
    guides(color = guide_legend(ncol = 1, override.aes = list(alpha = 1, linewidth = 2)))

  # Nombre de archivo corto
  ruta_grafico <- file.path(PARAM_plot$carpeta_graficos, "eval_curvas.png")
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
  
  # --- Cargar Mejores Hiperparámetros ---
  log_info("Cargando mejores hiperparámetros de BO_log.txt")
  dir_bayesiana <- file.path(PARAM$experimento_folder, PARAM$carpeta_bayesiana)
  log_bo_file <- file.path(dir_bayesiana, "BO_log.txt")
  
  # Levanto el archivo BO_log.txt, o lo regenero desde bayesiana.RDATA si no existe
  if (!file.exists(log_bo_file)) {
    log_warn(paste("No se encontró BO_log.txt. Intentando REGENERAR desde bayesiana.RDATA..."))

    rdata_file <- file.path(dir_bayesiana, "bayesiana.RDATA")
    if (!file.exists(rdata_file)) {
      stop(paste("FALLO FATAL: No se encontró ni BO_log.txt ni", rdata_file, "para regenerarlo."))
    }

    log_info(paste("Cargando", rdata_file, "..."))

    # Cargamos el .RDATA en un entorno separado para evitar conflictos de nombres
    env_bo <- new.env()
    load(rdata_file, envir = env_bo)

    # Comprobamos que el objeto (guardado en Script 9) exista
    if (!exists("bayesiana_salida", envir = env_bo)) {
      stop(paste("El archivo", rdata_file, "no contiene el objeto 'bayesiana_salida'.",
                "Asegúrate de modificar el Script 9 para que guarde 'bayesiana_salida' al finalizar."))
    }

    mbo_result <- env_bo$bayesiana_salida

    # Extraemos el historial de optimización (el $opt.path)
    # mlrMBO guarda el historial en $opt.path$env$path
    if (is.null(mbo_result$opt.path$env$path)) {
      stop("No se pudo encontrar el historial ($opt.path$env$path) dentro de bayesiana.RDATA.")
    }

    tb_BO_regen <- as.data.table(mbo_result$opt.path$env$path)

    # mlrMBO guarda la métrica de resultado en la columna "y"
    if ("y" %in% names(tb_BO_regen)) {
      # Renombramos "y" a "metrica" para que coincida con tu loguear()
      setnames(tb_BO_regen, "y", "metrica")
    } else {
      stop("El historial de optimización no tiene columna 'y'. No se puede regenerar la métrica.")
    }

    # Añadimos columnas NA para las que tu loguear() crea pero mlrMBO no
    # (Script 10 solo ordena por 'metrica', así que esto es por completitud)
    if (!"metrica_mejor" %in% names(tb_BO_regen)) tb_BO_regen[, metrica_mejor := NA_real_]
    if (!"metrica_sd" %in% names(tb_BO_regen)) tb_BO_regen[, metrica_sd := NA_real_]

    # Guardamos el log regenerado
    fwrite(tb_BO_regen, file = log_bo_file)
    log_info(paste("BO_log.txt regenerado y guardado en:", log_bo_file))

    tb_BO <- tb_BO_regen
      
  } else {
    # Si el archivo existía (Intento 1 o 2), simplemente lo leemos
    log_info(paste("Cargando BO_log.txt encontrado en:", log_bo_file))
    tb_BO <- fread(log_bo_file)
  }
  
  tb_BO <- fread(log_bo_file)
  setorder(tb_BO, -metrica) 
  
  param_lgbm <- union(names(PARAM$lgbm$param_fijos), names(PARAM$hipeparametertuning$hs$pars))
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
  
  mfuture <- data.matrix(dfuture[, campos_buenos, with = FALSE])
  drealidad <- realidad_inicializar(dfuture, PARAM)
  cortes_evaluacion <- seq(0, 20000, by = 100)
  
  # --- Crear directorios ---
  # Directorio para Gráficos (PNGs)
  dir_graficos <- file.path(PARAM$experimento_folder, PARAM$carpeta_graficos)
  dir.create(dir_graficos, recursive = TRUE, showWarnings = FALSE)
  
  # Directorio para Evaluación (CSVs, TXT)
  dir_evaluacion <- file.path(PARAM$experimento_folder, PARAM$carpeta_evaluacion)
  dir.create(dir_evaluacion, recursive = TRUE, showWarnings = FALSE)
  
  log_info(paste("Directorio de Gráficos (PNGs) creado en:", dir_graficos))
  log_info(paste("Directorio de Evaluación (CSVs) creado en:", dir_evaluacion))

  # --- Feature Importance de la Semilla Primigenia ---
  log_info(paste("Generando Feature Importance para la semilla primigenia:", PARAM$semilla_primigenia))
  
  param_primigenia <- copy(param_mejores)
  param_primigenia$seed <- PARAM$semilla_primigenia
  
  modelo_primigenia <- lgb.train(data = dtrain, param = param_primigenia)
  
  imp_primigenia <- lgb.importance(modelo_primigenia, percentage = TRUE)
  imp_ordenada_primigenia <- imp_primigenia[order(-Gain)]
  
  # --- Guardar CSV en /Evaluacion ---
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

  # --- 3. Bucle de Entrenamiento y Evaluación del Ensamble ---
  
  # Usamos las semillas de la BO (Script 9)
  if (!exists("PARAM$BO$semillas") || length(PARAM$BO$semillas) == 0) {
    stop("PARAM$BO$semillas no está definido o está vacío. Asegúrate de que 9_Optimizacion_Bayesiana.R se haya ejecutado.")
  }
  
  semillas_a_evaluar <- PARAM$BO$semillas
  
  log_info(paste0("Evaluando ", length(semillas_a_evaluar), 
                 " semilla(s) de la Optimización Bayesiana (ksemillerio * repe)."))
  log_info(paste(
    "Semillas de la BO a evaluar:", 
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

    # --- Guardar FI individual de esta semilla ---
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
      format(max_ganG_ind, big.mark = ".", decimal.mark = ","),
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

  # --- Procesar Feature Importance Promediado ---
  log_info("Calculando Feature Importance promediada del ensamble.")
  
  imp_todas <- rbindlist(lista_importancia)
  imp_promediada <- imp_todas[, .(Gain = mean(Gain), Cover = mean(Cover), Frequency = mean(Frequency)), by = Feature]
  imp_ordenada <- imp_promediada[order(-Gain)]
  
  # --- Guardar CSV en /Evaluacion ---
  ruta_csv_imp <- file.path(dir_evaluacion, "fi_ensemble.csv")
  fwrite(imp_ordenada, file = ruta_csv_imp)
  log_info(paste("Importancia de features (promediada) guardada en:", ruta_csv_imp))
  
  # Guardar Gráfico (PNG) en /Plots
  ruta_grafico_imp <- file.path(dir_graficos, "fi_ensemble.png")
  GraficarImportancia(imp_ordenada, 
                      top_n = PARAM$trainingstrategy$importancias, 
                      ruta_grafico = ruta_grafico_imp,
                      subtitulo = paste(PARAM$experimento, "- Promedio de", length(semillas_a_evaluar), "semillas"))
  
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
  
  # Calcular y guardar resumen del promedio
  max_ganancia_ens <- max(resultados_ensamble$ganancia_total, na.rm = TRUE)
  envios_optimos_ens <- resultados_ensamble[ganancia_total == max_ganancia_ens, clientes]
  envios_optimos_ens_str <- paste(sort(unique(envios_optimos_ens)), collapse = ", ")
  
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
  PARAM_plot <- list(
    carpeta_graficos = dir_graficos, # Usar la variable ya creada
    experimento = PARAM$experimento
  )

  # Generar Gráfico
  GraficarCurvasEnsemble(lista_resultados_individuales, PARAM_plot)

  # --- Guardar Resumen TXT en /Evaluacion ---
  ruta_resumen_txt <- file.path(dir_evaluacion, "eval_resumen.txt")
  fwrite(resumen_ganancias, file = ruta_resumen_txt, sep = "\t")
  log_info(paste0("Resumen de ganancias (leíble para Wilcoxon) guardado en: ", ruta_resumen_txt))

}, error = function(e) {
  log_error("######################################################")
  log_error("Se ha producido un error fatal en la Sección 10: Evaluación del Ensamble.")
  log_error(paste("Mensaje de R:", e$message))
  log_error("######################################################")
})

log_info("Fin Evaluación del Ensamble.")