# Limpiamos la memoria
rm(list = ls())
gc()

require("data.table")
require("logger")

#------------------------------------------------------------------------------
# VARIABLES DE ENTRADA
#------------------------------------------------------------------------------
carpeta_experimento <- "~/buckets/b1/exp"
experimento <- "ensamble_final"

dir_dataset <- "~/buckets/b1/datasets/competencia_03_ternaria.csv.gz"
archivos_probabilidades <- c(
    "prob1.csv",
    "prob2.csv"
)
cortes <- c(10500, 11000) # Cortes a generar

carpeta_salida <- file.path(carpeta_experimento, experimento)
dir.create(carpeta_salida, showWarnings = FALSE, recursive = TRUE)

# Configurar Logger
log_file <- file.path(carpeta_salida, paste0("log_", experimento, ".txt"))
log_appender(appender_tee(log_file))

log_info(paste("Iniciando experimento:", experimento))
log_info(paste("Carpeta de salida:", carpeta_salida))

#------------------------------------------------------------------------------
# LÓGICA PRINCIPAL
#------------------------------------------------------------------------------

# Listas para guardar probabilidades
lista_predicciones <- list()

# Procesar cada archivo de probabilidades
log_info("Cargando archivos de probabilidades...")
for (i in seq_along(archivos_probabilidades)) {
    archivo <- archivos_probabilidades[i]
    nombre_modelo <- paste0("Modelo_", i)

    log_info(paste("Procesando:", archivo))

    if (!file.exists(archivo)) {
        log_warn(paste("Archivo no encontrado:", archivo))
        next
    }

    # Leer probabilidades (asumiendo formato: numero_de_cliente, prob)
    tb_pred_individual <- fread(archivo, header = FALSE, col.names = c("numero_de_cliente", "prob"))

    # Guardar para el ensamble
    lista_predicciones[[nombre_modelo]] <- copy(tb_pred_individual)
}

if (length(lista_predicciones) == 0) {
    log_error("No se cargaron predicciones.")
    stop("No se cargaron predicciones.")
}

# Ensamble Promediado
log_info("Calculando Ensamble Promediado...")
predicciones_todas <- rbindlist(lista_predicciones)
tb_prediccion_ensamble <- predicciones_todas[, .(prob = mean(prob)), by = .(numero_de_cliente)]

# Ordenar por probabilidad descendente
setorder(tb_prediccion_ensamble, -prob)

# Generar archivos de salida para cada corte
log_info("Generando archivos de salida...")
for (envio in cortes) {
    log_info(paste("Generando corte:", envio))

    tb_prediccion_ensamble[, Predicted := 0L]
    tb_prediccion_ensamble[1:envio, Predicted := 1L]

    archivo_kaggle <- file.path(carpeta_salida, paste0("IDs_Ensamble_", envio, ".csv"))

    # Grabo el archivo (Solo numero_de_cliente donde Predicted == 1)
    fwrite(tb_prediccion_ensamble[Predicted == 1L, .(numero_de_cliente)],
        file = archivo_kaggle,
        col.names = FALSE
    )

    log_info(paste("Archivo con los IDs seleccionados guardado en:", archivo_kaggle))
}

log_info("Generando un archivo de entrega tipo Kaggle...")

for (envio in cortes) {
    log_info(paste("Generando corte:", envio))

    tb_prediccion_ensamble[, Predicted := 0L]
    tb_prediccion_ensamble[1:envio, Predicted := 1L]

    archivo_kaggle <- file.path(carpeta_salida, paste0("KA_Ensamble_", envio, ".csv"))

    # Grabo el archivo (Solo numero_de_cliente donde Predicted == 1)
    fwrite(tb_prediccion_ensamble[, list(numero_de_cliente, Predicted)],
        file = archivo_kaggle,
        sep = ","
    )

    log_info(paste("Archivo para Kaggle guardado en:", archivo_kaggle))
}

log_info("Proceso finalizado.")
