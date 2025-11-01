#------------------------------------------------------
# Funciones Auxiliares para el Experimento
#------------------------------------------------------

# Función para escribir en el log de resumen
log_summary <- function(message) {
  cat(paste0(message, "\n"), file = summary_log_file_path, append = TRUE)
}

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

realidad_inicializar <- function(pfuture, pparam) {
  drealidad <- pfuture[, list(numero_de_cliente, foto_mes, clase_ternaria)]
  particionar(
    drealidad,
    division = c(3, 7),
    agrupa = "clase_ternaria",
    seed = pparam$semilla_kaggle
  )
  return(drealidad)
}

realidad_evaluar <- function(prealidad, pprediccion) {
  prealidad[pprediccion, on = c("numero_de_cliente", "foto_mes"), predicted := i.Predicted]
  tbl <- prealidad[, list("qty" = .N), list(fold, predicted, clase_ternaria)]
  res <- list()
  res$public <- tbl[fold == 1 &
                      predicted == 1L, sum(qty * ifelse(clase_ternaria == "BAJA+2", 780000, -20000))] / 0.3
  res$private <- tbl[fold == 2 &
                       predicted == 1L, sum(qty * ifelse(clase_ternaria == "BAJA+2", 780000, -20000))] / 0.7
  res$total <- tbl[predicted == 1L, sum(qty * ifelse(clase_ternaria == "BAJA+2", 780000, -20000))]
  prealidad[, predicted := NULL]
  return(res)
}

EvaluarYGraficar <- function(tb_prediccion,
                             drealidad,
                             PARAM,
                             tipo_modelo,
                             carpeta_salida_kaggle) {
  log_info(paste0(
    "Iniciando evaluación y graficación para el modelo: ",
    tipo_modelo
  ))
  dir.create(carpeta_salida_kaggle, showWarnings = FALSE)
  resultados <- data.table()
  
  setorder(tb_prediccion, -prob)
  
  for (envios in PARAM$cortes) {
    tb_prediccion[, Predicted := 0L]
    tb_prediccion[1:envios, Predicted := 1L]
    
    res <- realidad_evaluar(drealidad, tb_prediccion)
    
    resultados <- rbind(
      resultados,
      data.table(
        clientes = envios,
        ganancia_total = res$total,
        ganancia_public = res$public,
        ganancia_private = res$private
      )
    )
    
    options(scipen = 999)
    log_info(
      sprintf(
        "Envios=%%-5d | TOTAL=%%11.0f | Public=%%11.0f | Private=%%11.0f",
        envios,
        res$total,
        res$public,
        res$private
      )
    )
  }
  
  archivo_resultados_csv <- paste0(PARAM$carpeta_bayesiana,
                                   "envios_",
                                   tipo_modelo,
                                   "_",
                                   PARAM$experimento,
                                   ".csv")
  resultados_para_csv <- copy(resultados)
  setnames(
    resultados_para_csv,
    old = c(
      "clientes",
      "ganancia_total",
      "ganancia_public",
      "ganancia_private"
    ),
    new = c("ENVIOS", "TOTAL", "PUBLIC", "PRIVATE")
  )
  fwrite(resultados_para_csv, file = archivo_resultados_csv)
  log_info(
    paste0(
      "Tabla de resultados para [",
      tipo_modelo,
      "] guardada en: ",
      archivo_resultados_csv
    )
  )
  
  max_ganancia_valor <- max(resultados$ganancia_total)
  envios_max_total <- resultados[ganancia_total == max_ganancia_valor, clientes]
  log_info(paste0(
    "Envíos óptimos para [",
    tipo_modelo,
    "]: ",
    paste(envios_max_total, collapse = ", ")
  ))
  log_info(paste0(
    "Máxima ganancia para [",
    tipo_modelo,
    "]: ",
    max_ganancia_valor
  )) # Log a la consola
  
  # # Ganancia privada
  max_ganancia_private <- max(resultados$ganancia_private)
  envios_max_private <- resultados[ganancia_private == max_ganancia_private, clientes]
  log_info(paste0(
    "Envíos óptimos para Ganancia PRIVADA: ",
    paste(envios_max_private, collapse = ", ")
  ))
  
  # envios_optimos_combinados <- unique(c(envios_max_total, envios_max_private))
  envios_optimos_combinados <- unique(c(envios_max_total))
  log_info(paste0(
    "Envíos óptimos combinados a retornar: ",
    paste(sort(envios_optimos_combinados), collapse = ", ")
  ))
  
  resultados_long <- melt(
    resultados,
    id.vars = "clientes",
    measure.vars = c("ganancia_total", "ganancia_public", "ganancia_private"),
    variable.name = "tipo",
    value.name = "ganancia"
  )
  
  maximos <- resultados_long[, .SD[ganancia == max(ganancia)], by = tipo]
  
  maximos_leyenda <- maximos[, .(ganancia = first(ganancia)), by = tipo]
  etiquetas <- paste0(
    tools::toTitleCase(gsub("_", " ", maximos_leyenda$tipo)),
    " (máx = ",
    format(
      maximos_leyenda$ganancia,
      big.mark = ".",
      decimal.mark = ","
    ),
    ")"
  )
  names(etiquetas) <- maximos_leyenda$tipo
  
  dir.create(PARAM$carpeta_graficos, showWarnings = FALSE)
  p <- ggplot(resultados_long, aes(x = clientes, y = ganancia, color = tipo)) +
    geom_line(linewidth = 1) +
    geom_point(data = maximos,
               aes(x = clientes, y = ganancia, color = tipo),
               size = 3) +
    ggrepel::geom_text_repel(
      data = maximos,
      aes(label = clientes),
      color = "black",
      size = 3.5,
      box.padding = 0.5,
      point.padding = 0.5,
      min.segment.length = 0,
      segment.color = 'grey50'
    ) +
    labs(
      title = paste0(
        "Curvas de Ganancia (Modelo ",
        tools::toTitleCase(gsub("_", " ", tipo_modelo)),
        " - ",
        PARAM$experimento,
        ")"
      ),
      x = "Clientes Contactados (Envíos)",
      y = "Ganancia",
      color = "Máximos"
    ) +
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(
      values = c(
        "ganancia_total" = "steelblue",
        "ganancia_public" = "forestgreen",
        "ganancia_private" = "firebrick"
      ),
      labels = etiquetas
    ) +
    theme_minimal() +
    theme(plot.margin = margin(10, 10, 10, 10),
          legend.position = "bottom") +
    guides(color = guide_legend(nrow = 3, byrow = TRUE))
  
  ggsave(
    paste0(
      PARAM$carpeta_graficos,
      "curvas_",
      tipo_modelo,
      "_",
      PARAM$experimento,
      ".png"
    ),
    plot = p,
    width = 11,
    height = 7
  )
  log_info(paste0("Gráfico de curvas de ganancia (", tipo_modelo, ") guardado."))
  
  return(
    list(envios_optimos = envios_optimos_combinados, max_ganancia = max_ganancia_valor)
  )
}

GenerarEnviosKaggle <- function(tb_prediccion,
                                envios_optimos,
                                tipo_modelo,
                                carpeta_salida,
                                experimento_id) {
  log_info(paste0(
    "Iniciando generación de envíos para Kaggle del modelo: '",
    tipo_modelo,
    "'"
  ))
  
  envios_a_generar <- unique(c(envios_optimos, 10500))
  
  log_info(paste0(
    "Se generarán archivos para los siguientes envíos: ",
    paste(envios_a_generar, collapse = ", ")
  ))
  
  dir.create(carpeta_salida, showWarnings = FALSE)
  
  setorder(tb_prediccion, -prob)
  
  for (envios in envios_a_generar) {
    tb_prediccion[, Predicted := 0L]
    tb_prediccion[1:envios, Predicted := 1L]
    
    archivo_kaggle <- paste0(carpeta_salida,
                             experimento_id,
                             "_",
                             tipo_modelo,
                             "_",
                             envios,
                             ".csv")
    
    fwrite(tb_prediccion[, list(numero_de_cliente, Predicted)], file = archivo_kaggle, sep = ",")
    
    log_info(paste0("Archivo generado: ", archivo_kaggle))
  }
  
  log_info(paste0("Generación de envíos para '", tipo_modelo, "' finalizada."))
}

GraficarCurvasEnsemble <- function(lista_resultados, PARAM) {
  log_info("Iniciando la graficación de la superposición de curvas del ensemble.")
  
  tb_todas <- rbindlist(lapply(names(lista_resultados), function(sem) {
    lista_resultados[[sem]][, semilla := as.character(sem)]
  }))
  
  tb_promedio <- tb_todas[, .(ganancia_total = mean(ganancia_total)), by = clientes]
  
  maximos_individuales <- tb_todas[, .SD[ganancia_total == max(ganancia_total)], by = semilla]
  maximos_individuales <- maximos_individuales[, head(.SD, 1), by = semilla]
  
  maximo_promedio <- tb_promedio[ganancia_total == max(ganancia_total)]
  maximo_promedio <- head(maximo_promedio, 1)
  
  labels_individuales <- sapply(maximos_individuales$semilla, function(sem) {
    max_info <- maximos_individuales[semilla == sem]
    paste0(
      "S ",
      sem,
      ": G ",
      format(
        max_info$ganancia_total,
        big.mark = ".",
        decimal.mark = ","
      ),
      " (E ",
      max_info$clientes,
      ")"
    )
  })
  
  label_promedio <- paste0(
    "Promedio: G ",
    format(
      maximo_promedio$ganancia_total,
      big.mark = ".",
      decimal.mark = ","
    ),
    " (E ",
    maximo_promedio$clientes,
    ")"
  )
  
  colores_individuales <- scales::hue_pal()(nrow(maximos_individuales))
  names(colores_individuales) <- maximos_individuales$semilla
  
  colores_plot <- c(colores_individuales, "Promedio" = "black")
  labels_plot <- c(labels_individuales, "Promedio" = label_promedio)
  names(labels_plot) <- c(names(colores_individuales), "Promedio")
  
  dir.create(PARAM$carpeta_graficos, showWarnings = FALSE)
  
  p <- ggplot() +
    geom_line(
      data = tb_todas,
      aes(
        x = clientes,
        y = ganancia_total,
        group = semilla,
        color = semilla
      ),
      alpha = 0.5,
      linewidth = 1
    ) +
    geom_line(
      data = tb_promedio,
      aes(x = clientes, y = ganancia_total, color = "Promedio"),
      linewidth = 1
    ) +
    geom_point(
      data = maximos_individuales,
      aes(x = clientes, y = ganancia_total, color = semilla),
      size = 3,
      alpha = 0.7
    ) +
    geom_point(
      data = maximo_promedio,
      aes(x = clientes, y = ganancia_total, color = "Promedio"),
      size = 4,
      shape = 18
    ) +
    ggrepel::geom_text_repel(
      data = maximos_individuales,
      aes(x = clientes, y = ganancia_total, label = clientes),
      color = "black",
      size = 3.0,
      box.padding = 0.3,
      point.padding = 0.3,
      min.segment.length = 0
    ) +
    ggrepel::geom_text_repel(
      data = maximo_promedio,
      aes(x = clientes, y = ganancia_total, label = clientes),
      color = "black",
      size = 4.0,
      fontface = "bold",
      box.padding = 0.5,
      point.padding = 0.5,
      min.segment.length = 0
    ) +
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(name = "Curvas y sus Máximos",
                       values = colores_plot,
                       labels = labels_plot) +
    labs(
      title = paste0("Curvas de Ganancia del Ensemble y su Promedio"),
      subtitle = paste0("Experimento: ", PARAM$experimento),
      x = "Clientes Contactados (Envíos)",
      y = "Ganancia Total"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.margin = margin(10, 10, 10, 10)) +
    guides(color = guide_legend(ncol = 2))
  
  ggsave(
    paste0(
      PARAM$carpeta_graficos,
      "curvas_ensemble_superpuestas_",
      PARAM$experimento,
      ".png"
    ),
    plot = p,
    width = 12,
    height = 8
  )
  
  log_info("Gráfico de superposición de curvas del ensemble guardado correctamente.")
}

rank_con_cero_fijo <- function(x) {
  resultado <- numeric(length(x))
  idx_pos <- which(x > 0)
  idx_neg <- which(x < 0)
  idx_cero <- which(x == 0)
  
  if (length(idx_pos) > 0) {
    resultado[idx_pos] <- frankv(x[idx_pos], ties.method = "average") / length(idx_pos)
  }
  if (length(idx_neg) > 0) {
    resultado[idx_neg] <- (frankv(-x[idx_neg], ties.method = "average") / length(idx_neg)) * -1
  }
  if (length(idx_cero) > 0) {
    resultado[idx_cero] <- 0
  }
  return(resultado)
}

EstimarGanancia_AUC_lightgbm <- function(x) {
  param_completo <- modifyList(PARAM$lgbm$param_fijos, x)
  modelocv <- lgb.cv(
    data = dtrain,
    nfold = PARAM$hyperparametertuning$xval_folds,
    stratified = TRUE,
    param = param_completo
  )
  AUC <- modelocv$best_score
  rm(modelocv)
  gc(full = TRUE, verbose = FALSE)
  log_info(paste(
    "Iteración BO -> AUC:",
    format(AUC, digits = 6),
    "|",
    format(Sys.time(), "%a %b %d %X %Y")
  ))
  return(AUC)
}

EstimarGanancia_AUC_lightgbm_Kaggle <- function(x) {
  param_completo <- modifyList(PARAM$lgbm$param_fijos, x)
  modelocv <- lgb.cv(
    data = dtrain_final_kaggle,
    # Usando el dataset grande
    nfold = PARAM$hyperparametertuning$xval_folds,
    stratified = TRUE,
    param = param_completo
  )
  AUC <- modelocv$best_score
  rm(modelocv)
  gc(full = TRUE, verbose = FALSE)
  log_info(paste(
    "Iteración BO (Kaggle) -> AUC:",
    format(AUC, digits = 6),
    "|",
    format(Sys.time(), "%a %b %d %X %Y")
  ))
  return(AUC)
}
