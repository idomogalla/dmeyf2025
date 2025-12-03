# Data Drifting
# Se debe corregir el drifting natural que ocurre en los datos, en particular los datos monetarios que se vieron fuertemente afectados por una alta inflación
# Posibles métodos son:
# * No hacer absolutamente nada
# * Ajuste de valores monetarios por indices del tipo :
#    * IPC  Indice de Precios al Consumidor
#    * Dolar Oficial
#    * Dolar Blue
#    * UVA  Unidad de Valor Adquisitivo
setorder(dataset, numero_de_cliente, foto_mes)

# --- Definir la función de ranking ---
rank_cero_fijo <- function(x) {
    resultado <- numeric(length(x))
    is_na <- is.na(x)
    resultado[is_na] <- NA_real_

    idx_pos <- which(x > 0)
    idx_neg <- which(x < 0)

    n_pos <- length(idx_pos)
    n_neg <- length(idx_neg)

    if (n_pos > 0) {
        resultado[idx_pos] <- frank(x[idx_pos], ties.method = "random") / n_pos
    }
    if (n_neg > 0) {
        resultado[idx_neg] <- (frank(-x[idx_neg], ties.method = "random") / n_neg) * -1
    }

    return(resultado)
}

aplicar_rank_cero_fijo <- function(dataset) {
  # --- Automatizar la selección de columnas ---
  log_info("Identificando columnas a rankear...")
  patron_regex <- "^(m|Visa_m|Master_m|vm_m)"
  cols_a_rankear <- grep(patron_regex, colnames(dataset), value = TRUE)
  
  # Excluir la variable de agrupación
  cols_a_rankear <- setdiff(cols_a_rankear, "foto_mes")
  
  log_info(paste(
      "Se identificaron", length(cols_a_rankear),
      "columnas para rankear (basado en el patrón)."
  ))
  
  # --- Definir los nuevos nombres ---
  nuevas_cols_rank <- paste0(cols_a_rankear, "_rank")
  log_info(paste(
      "Se crearán", length(nuevas_cols_rank),
      "columnas nuevas (ej: '", nuevas_cols_rank[1], "')."
  ))
  
  # --- Aplicar la operación ---
  log_info("Iniciando operación de ranking agrupada por 'foto_mes'...")
  
  # Medir el tiempo de la operación es una buena práctica de log
  t_inicio <- Sys.time()
  dataset[, (nuevas_cols_rank) := lapply(.SD, rank_cero_fijo),
      by = foto_mes,
      .SDcols = cols_a_rankear
  ]
  t_fin <- Sys.time()
  
  log_info(paste(
      "Operación de ranking completada en",
      round(difftime(t_fin, t_inicio, units = "secs"), 2), "segundos."
  ))
  
  # --- Borrar las columnas originales ---
  log_info(paste("Eliminando las", length(cols_a_rankear), "columnas originales..."))
  dataset[, (cols_a_rankear) := NULL]
  
  # --- Resumen Final ---
  log_info(paste(
      "Dimensiones finales del dataset (filas x columnas):",
      paste(dim(dataset), collapse = " x ")
  ))
  log_info(paste(
      "Columnas de ranking agregadas:",
      paste(nuevas_cols_rank, collapse = ", "), "..."
  ))
}

aplicar_ipc <- function(dataset) {
  log_info("Iniciando ajuste por IPC...")
  
  # ================= IPC desde variaciones mensuales =================
  # Variaciones mensuales (%) en orden 201901, 201902, ..., 202109
  ipc_var <- c(
    2.9, 3.8, 4.7, 3.4, 3.1, 2.7, 2.2, 4.0, 5.9, 3.3, 4.3, 3.7,
    2.3, 2.0, 3.3, 1.5, 1.5, 2.2, 1.9, 2.7, 2.8, 3.8, 3.2, 4.0,
    4.0, 3.6, 4.8, 4.1, 3.3, 3.2, 3.0, 2.5, 3.5
  )
  
  # Genero la secuencia de foto_mes 201901..202109
  seq_fm <- function(from_fm, to_fm){
    dseq <- seq(as.Date(paste0(from_fm,"01"), "%Y%m%d"),
                as.Date(paste0(to_fm,"01"), "%Y%m%d"), by="month")
    as.integer(format(dseq, "%Y%m"))
  }
  fm_seq <- seq_fm(201901L, 202109L)
  
  if(length(ipc_var) != length(fm_seq)) {
    log_error("La longitud de ipc_var no coincide con la secuencia de meses generada.")
    stop("Error en secuencia IPC")
  }
  
  ipc <- data.table(
    foto_mes = fm_seq,
    variacion_mensual = ipc_var
  )
  
  # Índice acumulado (base libre). Pongo 100 en el primer mes y acumulo.
  ipc[, indice := 100 * cumprod(1 + variacion_mensual/100)]
  
  # ================= Deflactar a una base elegida =================
  base_mes <- 202109L            # sugerido: el último mes disponible
  if(!base_mes %in% ipc$foto_mes) {
    log_warn(paste("El mes base", base_mes, "no está en la tabla de IPC. Usando el último disponible."))
    base_mes <- tail(ipc$foto_mes, 1)
  }
  
  # Traigo el índice al dataset y calculo factor relativo a la base
  setkey(ipc, foto_mes)
  if(!"foto_mes" %in% names(dataset)) {
    log_error("La columna 'foto_mes' no está en el dataset.")
    stop("Falta foto_mes")
  }
  
  # Join para traer el indice 
  log_info("Uniendo índice IPC al dataset...")
  dataset[ipc, on="foto_mes", indice := i.indice]
  
  indice_base <- ipc[foto_mes == base_mes, indice][1]
  dataset[, defl_factor := indice / indice_base]  # >1 = precios más altos que la base
  
  # Detecto columnas de montos (m* y Master_m*/Visa_m*)
  montos_m_prefix <- grep("^m", names(dataset), value = TRUE)
  montos_card     <- grep("^(Master|Visa)_m", names(dataset), value = TRUE)
  montos_all <- unique(c(montos_m_prefix, montos_card))
  # Filtrar solo numéricas
  montos_all <- montos_all[vapply(dataset[, ..montos_all], is.numeric, logical(1))]
  
  log_info(sprintf("IPC aplicado (base %d). Columnas a deflactar: %d", base_mes, length(montos_all)))
  
  # Deflactar IN-PLACE a pesos de 'base_mes'
  for (cn in montos_all) {
    dataset[, (cn) := get(cn) / pmax(defl_factor, 1e-12)]
  }
  
  # (opcional) trazabilidad y limpieza
  dataset[, ipc_base := base_mes]
  dataset[, c("variacion_mensual", "indice","defl_factor") := NULL]
  
  log_info("Ajuste IPC finalizado.")
}

# --- Ejecución Principal ---
if (isTRUE(PARAM$drifting$rank_cero_fijo)) {
  log_info("Aplicando Data Drifting: Ranking Cero Fijo")
  aplicar_rank_cero_fijo(dataset)
} else if (isTRUE(PARAM$drifting$ipc)) {
  log_info("Aplicando Data Drifting: Ajuste IPC")
  aplicar_ipc(dataset)
} else {
  log_info("No se aplica ninguna corrección de Data Drifting.")
}
