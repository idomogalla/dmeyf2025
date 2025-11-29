if (isTRUE(PARAM$eliminacion$prestamos_personales)) {
    log_info("Eliminando las variables mprestamos_personales, cprestamos_personales")
    dataset[, mprestamos_personales := NULL]
    dataset[, cprestamos_personales := NULL]
} else if (isTRUE(PARAM$eliminacion$flip_internet)) {
    log_info("Dando vuelta la variable internet")
    dataset[, internet := fifelse(internet %in% 0:1, as.integer(internet), NA_integer_)]
    dataset[foto_mes < 202010 & !is.na(internet), internet := 1L - internet]
} else if (isTRUE(PARAM$eliminacion$internet)) {
    log_info("Eliminando la variable internet")
    dataset[, internet := NULL]
} 
else {
    log_info("No se elimina ninguna variable.")
}