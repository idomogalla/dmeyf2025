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
setorder(dataset, numero_de_cliente, foto_mes)

# ================= IPC desde variaciones mensuales =================
# Variaciones mensuales (%) en orden 201901, 201902, ..., 202108  (32 valores)
ipc_var <- c(
  2.9, 3.8, 4.7, 3.4, 3.1, 2.7, 2.2, 4.0, 5.9, 3.3, 4.3, 3.7,
  2.3, 2.0, 3.3, 1.5, 1.5, 2.2, 1.9, 2.7, 2.8, 3.8, 3.2, 4.0,
  4.0, 3.6, 4.8, 4.1, 3.3, 3.2, 3.0, 2.5
)

# Genero la secuencia de foto_mes 201901..202108
seq_fm <- function(from_fm, to_fm){
  dseq <- seq(as.Date(paste0(from_fm,"01"), "%Y%m%d"),
              as.Date(paste0(to_fm,"01"), "%Y%m%d"), by="month")
  as.integer(format(dseq, "%Y%m"))
}
fm_seq <- seq_fm(201901L, 202108L)

stopifnot(length(ipc_var) == length(fm_seq))

ipc <- data.table(
  foto_mes = fm_seq,
  variacion_mensual = ipc_var
)

# Índice acumulado (base libre). Pongo 100 en el primer mes y acumulo.
ipc[, indice := 100 * cumprod(1 + variacion_mensual/100)]

# ================= Deflactar a una base elegida =================
base_mes <- 202108L            # sugerido: el último mes disponible
stopifnot(base_mes %in% ipc$foto_mes)

# Traigo el índice al dataset y calculo factor relativo a la base
setkey(ipc, foto_mes)
stopifnot("foto_mes" %in% names(dataset))
dataset <- ipc[dataset, on="foto_mes"]

indice_base <- ipc[foto_mes == base_mes, indice][1]
dataset[, defl_factor := indice / indice_base]  # >1 = precios más altos que la base

# Detecto columnas de montos (m* y Master_m*/Visa_m*)
montos_m_prefix <- grep("^m", names(dataset), value = TRUE)
montos_card     <- grep("^(Master|Visa)_m", names(dataset), value = TRUE)
montos_all <- unique(c(montos_m_prefix, montos_card))
montos_all <- montos_all[vapply(dataset[, ..montos_all], is.numeric, logical(1))]

# Deflactar IN-PLACE a pesos de 'base_mes'
for (cn in montos_all) {
  dataset[, (cn) := get(cn) / pmax(defl_factor, 1e-12)]
}

# (opcional) trazabilidad y limpieza
dataset[, ipc_base := base_mes]
dataset[, c("variacion_mensual","indice","defl_factor") := NULL]