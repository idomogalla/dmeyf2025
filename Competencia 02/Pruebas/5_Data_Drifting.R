#!/usr/bin/env Rscript
# Data Drifting
# Se debe corregir el drifting natural que ocurre en los datos, en particular los datos monetarios que se vieron fuertemente afectados por una alta inflación
# Posibles métodos son:
# * No hacer absolutamente nada
# * Ajuste de valores monetarios por indices del tipo :
#    * IPC  Indice de Precios al Consumidor
#    * Dolar Oficial
#    * Dolar Blue
#    * UVA  Unidad de Valor Adquisitivo
log_info("--- INICIO: Proceso de Ranking con Cero Fijo ---")

setorder(dataset, numero_de_cliente, foto_mes)

# --- 1. Definir la función de ranking ---
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

# --- Automatizar la selección de columnas ---
log_info("Identificando columnas a rankear...")
patron_regex <- "^(m|Visa_m|Master_m|vm_m)"
cols_a_rankear <- grep(patron_regex, colnames(dataset), value = TRUE)

# Excluir la variable de agrupación
cols_a_rankear <- setdiff(cols_a_rankear, "foto_mes")

log_info(paste("Se identificaron", length(cols_a_rankear), 
               "columnas para rankear (basado en el patrón)."))

# --- Definir los nuevos nombres ---
nuevas_cols_rank <- paste0(cols_a_rankear, "_rank")
log_info(paste("Se crearán", length(nuevas_cols_rank), 
               "columnas nuevas (ej: '", nuevas_cols_rank[1], "')."))

# --- Aplicar la operación ---
log_info("Iniciando operación de ranking agrupada por 'foto_mes'...")

# Medir el tiempo de la operación es una buena práctica de log
t_inicio <- Sys.time()
dataset[, (nuevas_cols_rank) := lapply(.SD, rank_cero_fijo), 
        by = foto_mes, 
        .SDcols = cols_a_rankear]
t_fin <- Sys.time()

log_info(paste("Operación de ranking completada en", 
               round(difftime(t_fin, t_inicio, units = "secs"), 2), "segundos."))

# --- Borrar las columnas originales ---
log_info(paste("Eliminando las", length(cols_a_rankear), "columnas originales..."))
dataset[, (cols_a_rankear) := NULL]

# --- Resumen Final ---
log_info(paste("Dimensiones finales del dataset (filas x columnas):", 
               paste(dim(dataset), collapse = " x ")))
log_info(paste("Columnas de ranking agregadas:",
               paste(nuevas_cols_rank, collapse = ", "), "..."))