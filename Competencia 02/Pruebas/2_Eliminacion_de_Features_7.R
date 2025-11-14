#!/usr/bin/env Rscript
log_info("Eliminando las variables mprestamos_personales, cprestamos_personales")

dataset[, mprestamos_personales := NULL ]
dataset[, cprestamos_personales := NULL ]

#dataset[, internet := NULL ]
#dataset[, cprestamos_hipotecarios := NULL ]
#dataset[, tmobile_app := NULL ]
#dataset[, cmobile_app_trx := NULL ]

# 1) Limpieza: cualquier cosa que no sea 0/1 -> NA (en todos los meses)
dataset[, internet := fifelse(internet %in% 0:1, as.integer(internet), NA_integer_)]

# 2) Flip: solo para meses anteriores a 202010 y con valor válido
dataset[ foto_mes < 202010 & !is.na(internet), internet := 1L - internet ]