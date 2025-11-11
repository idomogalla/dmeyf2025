#!/usr/bin/env Rscript
log_info("Eliminando las variables mprestamos_personales, cprestamos_personales")

dataset[, mprestamos_personales := NULL ]
dataset[, cprestamos_personales := NULL ]

dataset[, internet := NULL ]
#dataset[, cprestamos_hipotecarios := NULL ]
dataset[, tmobile_app := NULL ]
dataset[, cmobile_app_trx := NULL ]