#!/usr/bin/env Rscript
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

  if( !"ganancia_meseta" %in% colnames(tb_resultados_ensamble) ) {
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
  geom_label_repel(data = maximo_punto,
        aes(x = clientes, y = ganancia_ensamble_real, 
            label = paste0("Máximo\n", 
                           format(ganancia_ensamble_real, big.mark = ".", 
                                  decimal.mark = ",", scientific = FALSE),
                           "\nEnvios\n",
                           format(clientes, big.mark = ".", decimal.mark = ","))),
        fill = "white", color = "red", fontface = "bold",
        label.padding = unit(0.3, "lines"),
        nudge_y = 80000000,
        segment.color = "grey30",
        min.segment.length = 0,
        direction = "y") +
  geom_label_repel(data = maximo_meseta,
        aes(x = clientes, y = ganancia_meseta, 
            label = paste0("Meseta\n", 
                           format(ganancia_meseta, big.mark = ".", 
                                  decimal.mark = ",", scientific = FALSE),
                           "\nEnvios\n",
                           format(clientes, big.mark = ".", decimal.mark = ","))),
        fill = "white", color = "purple", fontface = "bold",
        label.padding = unit(0.3, "lines"),
        nudge_y = -90000000,
        segment.color = "grey30",
        min.segment.length = 0,
        direction = "y") +
  scale_y_continuous(labels = scales::comma, 
                     expand = expansion(mult = c(0.1, 0.25))) +
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
  
  # Preparar Datos de Entrenamiento (con canaritos)
  if (!exists("campos_buenos")) {
    stop("El objeto 'campos_buenos' no existe. Asegúrate de que 8_Modelado.R se haya ejecutado.")
  }

  log_info("Preparando dtrain para evaluación (con canaritos).")
  dataset_train_eval <- dataset[training == 1L]
  
  # Las columnas canaritos mandatoriamente van al comienzo del dataset
  cols0 <- copy(colnames(dataset_train_eval))
  filas <- nrow(dataset_train_eval)
  for (i in seq_len(PARAM$qcanaritos)) {
    dataset_train_eval[, paste0("canarito_", i) := runif(filas)]
  }
  cols_canaritos <- copy(setdiff(colnames(dataset_train_eval), cols0))
  setcolorder(dataset_train_eval, c(cols_canaritos, cols0))
  
  # Campos a utilizar
  campos_buenos_z <- setdiff(colnames(dataset_train_eval), PARAM$trainingstrategy$campos_entrenar)
  
  log_info("Creando dtrain con canaritos.")
  dtrain <- lgb.Dataset(
    data = data.matrix(dataset_train_eval[, campos_buenos_z, with = FALSE]),
    label = dataset_train_eval[, clase01],
    free_raw_data = FALSE
  )
  rm(dataset_train_eval) 
  gc()

  # Preparar Datos de Testing (con canaritos)
  log_info("Preparando datos de evaluación (testing) (con canaritos).")
  dfuture <- dataset[foto_mes %in% PARAM$eval_ensamble$mes_testing]
  
  if (nrow(dfuture) == 0) {
    stop(paste("No se encontraron datos para el período de testing:", PARAM$eval_ensamble$mes_testing))
  }
  
  # Los canaritos tambien deben ir adelante en la predicción
  filas_future <- nrow(dfuture)
  for (i in seq_len(PARAM$qcanaritos)) {
    dfuture[, paste0("canarito_", i) := runif(filas_future)]
  }
  
  mfuture <- data.matrix(dfuture[, campos_buenos_z, with = FALSE])
  drealidad <- realidad_inicializar(dfuture, PARAM)
  cortes_evaluacion <- PARAM$eval_ensamble$cortes_evaluacion
  
  # Crear directorios
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
  total_semillas <- PARAM$eval_ensamble$ksemillerio * PARAM$eval_ensamble$iter
  
  log_info(paste0("Total de semillas a entrenar: ", total_semillas, 
               " (Iter: ", PARAM$eval_ensamble$iter, 
               " x kSemillerio: ", PARAM$eval_ensamble$ksemillerio, ")"))
  
  set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")
  if(!exists("primos")) primos <- generate_primes(min = 100000, max = 1000000)
  PARAM$eval_ensamble$semillas <- sample(primos)[seq( total_semillas )]
    
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
    
    rm(modelo, imp, prediccion_individual)
    gc()
  }
  
  log_info("Evaluación de semillas individuales completa.")

  rm(dtrain, mfuture)
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
                      subtitulo = paste(PARAM$experimento, "- Promedio de", length(semillas_a_evaluar), "semillas"))
  
  # Evaluación del Ensamble Promediado
  log_info("Evaluando el 'Gran Ensamble' (promedio de todas las semillas)...")
  
  predicciones_todas <- rbindlist(lista_predicciones)
  tb_prediccion_ensamble <- predicciones_todas[, .(prob = mean(prob)), by = .(numero_de_cliente, foto_mes)]
  
  # (Liberamos predicciones_todas solo si APO está apagado)
  if (PARAM$eval_ensamble$APO == FALSE) {
    rm(lista_predicciones, predicciones_todas)
    gc()
  }
  
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
  
  log_info(paste0("Cálculo de meseta: step_size=", step_size, 
                  ", n_pasos_ventana=", n_pasos_ventana, 
                  ", n_ventana_meseta_final=", n_ventana_meseta))
  
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
    
  envios_manuales <- c(10500, 11000) 

  envios_optimos_vector <- sort(unique(c(envios_optimos_meseta_vec, envios_optimos_real_vec, envios_manuales)))
  
  PARAM$eval_ensamble$envios_optimos_promedio <- envios_optimos_vector

  ruta_envios_rds <- file.path(dir_evaluacion, "envios_optimos.rds")
  saveRDS(envios_optimos_vector, file = ruta_envios_rds)
  log_info(paste("Vector de envíos óptimos (Meseta + Pico + Manuales) guardado en:", ruta_envios_rds))
  
  log_info(paste0("Gran Ensamble (Pico Real): Ganancia Máx = ", 
                 format(max_ganancia_ens, big.mark = ".", decimal.mark = ","), 
                 " en envíos: [", envios_optimos_real_str, "] (INCLUIDO EN RDS)"))
  
  log_info(paste0("Gran Ensamble (Meseta): Ganancia Máx = ", 
                 format(max_ganancia_meseta, big.mark = ".", decimal.mark = ","), 
                 " en envíos: [", envios_optimos_meseta_str, "] (INCLUIDO EN RDS)"))

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

  GraficarCurvasEnsemble(lista_resultados_individuales, 
                         resultados_ensamble, 
                         PARAM_plot)

  ruta_resumen_txt <- file.path(dir_evaluacion, "eval_resumen.txt")
  fwrite(resumen_ganancias, 
         file = ruta_resumen_txt, 
         sep = "\t",
         scipen = 999)
  log_info(paste0("Resumen de ganancias (Gran Ensamble) guardado en: ", ruta_resumen_txt))
  
  rm(resultados_ensamble, tb_prediccion_ensamble)
  gc()
  
  # APO by Gustavo  
  if (PARAM$eval_ensamble$APO == TRUE) {
    log_info("Iniciando Evaluación Comparativa: Estrategia APO")
    
    log_info("Preparando datos de 'future' (solo la 'verdad') para la evaluación APO.")
    dfuture_apo <- dataset[foto_mes %in% PARAM$eval_ensamble$mes_testing, 
                           list(numero_de_cliente, foto_mes, clase_ternaria)]
    
    dfuture_apo[, ganancia := ifelse(clase_ternaria=="BAJA+2", 780000, -20000)]
    
    cortes_fijos_apo <- c(10500, 11000, 11500, 12000) 
    
    mganancias <- matrix(nrow = PARAM$eval_ensamble$iter, 
                         ncol = length(cortes_fijos_apo))
    
    dir_kaggle <- file.path(PARAM$experimento_folder, PARAM$carpeta_entregables)
    dir.create(dir_kaggle, showWarnings = FALSE)
    log_info(paste("Directorio de Entregables (APO) creado en:", dir_kaggle))
    
    ruta_prediccion_apo <- file.path(dir_evaluacion, "prediccion_APO.txt")
    if (file.exists(ruta_prediccion_apo)) {
      file.remove(ruta_prediccion_apo)
    }
    log_info(paste("Archivo de predicciones APO se guardará en:", ruta_prediccion_apo))
    
    semillas_totales <- names(lista_predicciones)
    
    for (vapo in seq(PARAM$eval_ensamble$iter)) {
      
      desde <- 1 + (vapo - 1) * PARAM$eval_ensamble$ksemillerio
      hasta <- desde + PARAM$eval_ensamble$ksemillerio - 1
      semillas_subset <- semillas_totales[desde:hasta]
      
      log_info(paste0("--- Procesando Meta-Modelo APO: ", vapo, 
                   " (semillas ", desde, " a ", hasta, ") ---"))
      
      predicciones_subset_dt <- predicciones_todas[semilla %in% semillas_subset]
      
      tb_pred_metamodelo <- predicciones_subset_dt[, .(prob = mean(prob)), 
                                                   by = .(numero_de_cliente, foto_mes)]
      
      tb_eval_apo <- dfuture_apo[tb_pred_metamodelo, on = c("numero_de_cliente", "foto_mes")]

      setorder(tb_eval_apo, -prob)
      tb_eval_apo[, gan_acum := cumsum(ganancia)]
      tb_eval_apo[, meta_modelo := vapo]

      for (icor in seq_along(cortes_fijos_apo)) {
        mganancias[vapo, icor] <- tb_eval_apo[cortes_fijos_apo[icor], gan_acum]
      }

      fwrite(tb_eval_apo[, list(numero_de_cliente, foto_mes, prob, meta_modelo, gan_acum)],
             file = ruta_prediccion_apo,
             sep = "\t",
             append = TRUE
      )
      
      envios_fantasia <- 11000 
      tb_eval_apo[, Predicted := 0L]
      tb_eval_apo[1:envios_fantasia, Predicted := 1L]
      
      archivo_kaggle_fantasia <- file.path(dir_kaggle, paste0("KA_APO_", 
                                          PARAM$experimento, "_", vapo, "_", envios_fantasia, ".csv"))
      
      fwrite(tb_eval_apo[, list(numero_de_cliente, Predicted)],
             file = archivo_kaggle_fantasia,
             sep = ",")
      
      rm(tb_eval_apo, tb_pred_metamodelo, predicciones_subset_dt)
      gc()
    }

    log_info("--- Evaluación APO Completa. Generando el archivo para enviar según APO ---")

    colmedias <- colMeans(mganancias, na.rm = TRUE)
    mcorte_mejor <- max(colmedias, na.rm = TRUE)
    icorte_mejor <- which.max(colmedias)
    corte_mejor <- cortes_fijos_apo[icorte_mejor]

    log_info(paste0("Ganancia Máxima Promedio (APO): ", 
                 format(mcorte_mejor, big.mark = ".", decimal.mark = ","),
                 " en corte fijo: ", corte_mejor))

    colnames(mganancias) <- paste0("e", cortes_fijos_apo)
    tbl_local_apo <- as.data.table(mganancias)
    tbl_local_apo[, meta_modelo := 1:PARAM$eval_ensamble$iter]

    ruta_resumen_apo <- file.path(dir_evaluacion, "eval_resumen_APO.txt")
    fwrite(tbl_local_apo,
           file = ruta_resumen_apo,
           sep = "\t")
    log_info(paste("Resumen de ganancias por meta-modelo (APO) guardado en:", ruta_resumen_apo))

    log_info("Cargando predicciones_APO.txt para selección final...")
    tb_prediccion_apo_full <- fread(ruta_prediccion_apo)

    icerca <- which.min(abs(tb_prediccion_apo_full$gan_acum - mcorte_mejor))
    vmodelo <- tb_prediccion_apo_full[icerca, meta_modelo]
    ganancia_cercana <- tb_prediccion_apo_full[icerca, gan_acum]

    tb_pred_final_apo <- tb_prediccion_apo_full[meta_modelo == vmodelo]
    setorder(tb_pred_final_apo, -prob)
    corte_cercano <- tb_pred_final_apo[, .I[which.min(abs(gan_acum - mcorte_mejor))] ]

    log_info(paste0("Selección Final (APO): Meta-Modelo ", vmodelo, 
                 " en corte ", corte_cercano, 
                 " (Ganancia: ", format(tb_pred_final_apo[corte_cercano, gan_acum], big.mark = ".", decimal.mark = ","), ")"))

    tb_pred_final_apo[, Predicted := 0L]
    tb_pred_final_apo[1:corte_cercano, Predicted := 1L]

    archivo_pseudo_kaggle <- file.path(dir_kaggle, paste0("KA_APO_FINAL_", 
                                      PARAM$experimento, "_", corte_cercano, ".csv"))

    fwrite(tb_pred_final_apo[, list(numero_de_cliente, Predicted)],
           file = archivo_pseudo_kaggle,
           sep = ",")

    log_info(paste("Archivo de submission final (APO) guardado en:", archivo_pseudo_kaggle))

    rm(tb_prediccion_apo_full, tb_pred_final_apo, tbl_local_apo, mganancias, dfuture_apo, predicciones_todas)
    gc()
  }
}, error = function(e) {
  log_error("######################################################")
  log_error("Se ha producido un error fatal en la Sección 11: Evaluación del Ensamble.")
  log_error(paste("Mensaje de R:", e$message))
  log_error("######################################################")
})