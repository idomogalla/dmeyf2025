#!/usr/bin/env Rscript
log_info("Eliminando las variables mprestamos_personales, cprestamos_personales")

dataset[, mprestamos_personales := NULL ]
dataset[, cprestamos_personales := NULL ]

# Flip internet
dataset[, internet := fifelse(internet %in% 0:1, as.integer(internet), NA_integer_)]
dataset[ foto_mes < 202010 & !is.na(internet), internet := 1L - internet ]