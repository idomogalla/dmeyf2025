log_info("Eliminando las variables mprestamos_personales, cprestamos_personales")

dataset[, mprestamos_personales := NULL ]
dataset[, cprestamos_personales := NULL ]