suppressPackageStartupMessages({
  if (!require("logger"))
    install.packages("logger")
  library("logger")
  if (!require("data.table"))
    install.packages("data.table")
  library("data.table")
  if (!require("ggplot2"))
    install.packages("ggplot2")
  library("ggplot2")
  if (!require("rmarkdown"))
    install.packages("rmarkdown")
  library("rmarkdown")
})

#------------------------------------------------------
# Sección 1: Configuración Inicial y Parámetros
#------------------------------------------------------
rm(list = ls(all.names = TRUE))
gc(full = TRUE, verbose = FALSE)

PARAM <- list()
PARAM$experimento <- "Bases_Competencia_02" # nolint
PARAM$dir_experimento <- paste0("~/buckets/b1/exp/", PARAM$experimento)
PARAM$dir_dataset <- "~/buckets/b1/datasets/"
PARAM$carpeta_logs <- "logs/"
PARAM$carpeta_graficos <- "Plots/"

# ----- Configuración del Logger -----
# Creo la carpeta del experimento
dir.create(PARAM$dir_experimento,
           showWarnings = FALSE,
           recursive = TRUE)
setwd(PARAM$dir_experimento)
# Creo la carpeta para los logs
dir.create(PARAM$carpeta_logs,
           showWarnings = FALSE,
           recursive = TRUE)
# Definir la ruta del archivo log
log_file <- file.path(PARAM$carpeta_logs, paste0("log_", PARAM$experimento, ".log"))
# Configurar el logger para que escriba en consola y en la ruta absoluta del archivo
log_appender(appender_tee(log_file))

# Limpio el archivo de resumen al inicio de la ejecución
summary_log_file_path <- file.path(PARAM$carpeta_logs, PARAM$archivo_summary_log)
cat(
  paste0("Resumen del Experimento: ", PARAM$experimento, "\n"),
  file = summary_log_file_path,
  append = FALSE
)
cat(paste0("Fecha: ", Sys.time(), "\n\n"),
    file = summary_log_file_path,
    append = TRUE)


log_info("------------------------------------------------------")
log_info("Inicio del script.")
log_info(paste("El log se guardará en:", log_file))
log_info(paste("El resumen se guardará en:", summary_log_file_path))
log_info("------------------------------------------------------")

#------------------------------------------------------
# Sección 2: Lectura del Dataset
#------------------------------------------------------
dataset <- fread(file.path("competencia_02_crudo.csv.gz"), stringsAsFactors = TRUE)

log_info("------------------------------------------------------")
log_info(paste("Dataset leído con ",nrow(dataset)," filas y ", ncol(dataset)," columnas."))
log_info("------------------------------------------------------")


#------------------------------------------------------
# Sección 3: Generar clase ternaria
#------------------------------------------------------
log_info("Generando la clase ternaria...")
tryCatch({
  # calculo el periodo0 consecutivo
  dsimple <- dataset[, list(
      "pos" = .I,
      numero_de_cliente,
      periodo0 = as.integer(foto_mes/100)*12 +  foto_mes%%100 ) ]

  # ordeno
  setorder( dsimple, numero_de_cliente, periodo0 )

  # calculo topes
  periodo_ultimo <- dsimple[, max(periodo0) ]
  periodo_anteultimo <- periodo_ultimo - 1

  # calculo los leads de orden 1 y 2
  dsimple[, c("periodo1", "periodo2") :=
      shift(periodo0, n=1:2, fill=NA, type="lead"),  numero_de_cliente ]

  # assign most common class values = "CONTINUA"
  dsimple[ periodo0 < periodo_anteultimo, clase_ternaria := "CONTINUA" ]

  # calculo BAJA+1
  dsimple[ periodo0 < periodo_ultimo &
      ( is.na(periodo1) | periodo0 + 1 < periodo1 ),
      clase_ternaria := "BAJA+1" ]

  # calculo BAJA+2
  dsimple[ periodo0 < periodo_anteultimo & (periodo0+1 == periodo1 )
      & ( is.na(periodo2) | periodo0 + 2 < periodo2 ),
      clase_ternaria := "BAJA+2" ]

  # pego el resultado en el dataset original y grabo
  setorder( dsimple, pos )
  dataset[, clase_ternaria := dsimple$clase_ternaria ]

  fwrite(dataset,
      file =  paste0(PARAM$dir_dataset, "competencia_02.csv.gz"),
      sep = ","
  )
  log_info(paste0("Archivo competencia_02.csv.gz con la clase ternaria generado en ", PARAM$dir_dataset))
}, error = function(e) {
  log_error("Error al generar la clase ternaria: {e$message}")
})

#------------------------------------------------------
# Sección 4: Análisis
#------------------------------------------------------
# --- Distribución de la clase ternaria ---
log_info("--- Distribución de la clase objetivo a lo largo del tiempo ---")

# Calcular la distribución de la clase_ternaria por cada foto_mes
target_distribution <- dataset[, .(
  CONTINUA = sum(clase_ternaria == "CONTINUA", na.rm = TRUE),
  `BAJA+1` = sum(clase_ternaria == "BAJA+1", na.rm = TRUE),
  `BAJA+2` = sum(clase_ternaria == "BAJA+2", na.rm = TRUE)
), by = foto_mes]

# Ordeno por foto_mes
setorder(target_distribution, foto_mes)

# Muestro
log_info("Distribución de la clase objetivo:")
log_info(capture.output(print(target_distribution)))

# Guardo como CSV
fwrite(target_distribution, "distribucion_clase_ternaria.csv")
log_info("Distribución de la clase objetivo a lo largo del tiempo guardada en distribucion_clase_ternaria.csv")

# --- Análisis de retornos ---
log_info("--- Análisis de retorno de clientes ---")

# Me aseguro que la información esté ordenada por foto_mes
setorder(dataset, numero_de_cliente, foto_mes)

customer_periods <- dataset[, .(periodos = list(sort(unique(periodo0)))), by = numero_de_cliente]

absence_data <- data.table(
  numero_de_cliente = integer(),
  absence_start_periodo = integer(),
  absence_end_periodo = integer(),
  absence_duration_months = integer()
)

for (i in 1:nrow(customer_periods)) {
  cliente <- customer_periods$numero_de_cliente[i]
  periods <- unlist(customer_periods$periodos[i])

  if (length(periods) > 1) {
    for (j in 1:(length(periods) - 1)) {
      gap <- periods[j+1] - periods[j]
      if (gap > 1) { # There is an absence
        absence_duration <- gap - 1
        absence_data <- rbind(absence_data, list(
          cliente,
          periods[j] + 1,
          periods[j+1] - 1,
          absence_duration
        ))
      }
    }
  }
}

# Sumarizo las duraciones de ausencia
absence_summary <- absence_data[, .(
  cantidad_clientes_unicos = uniqueN(numero_de_cliente)
), by = absence_duration_months]

setorder(absence_summary, absence_duration_months)

# Agrego la columna "Tipo de ausencia"
absence_summary[, `Tipo de ausencia` := paste(absence_duration_months, "mes", ifelse(absence_duration_months > 1, "es", ""))]

# Selecciono y reordeno
absence_summary <- absence_summary[, .(`Tipo de ausencia`, cantidad_clientes_unicos)]

# Muestro en consola y log
log_info("Resumen del análisis de retorno de clientes:")
log_info(capture.output(print(absence_summary)))

# Guardo como CSV
fwrite(absence_summary, "retorno_clientes.csv")
log_info("Resumen del análisis de retorno de clientes guardada en retorno_clientes.csv")

# --- Evolución de las variables ---
log_info("--- Evolución de las variables ---")

# Excluyo numero_de_cliente, clase_ternaria, y cualquier columna temporal
features_to_plot <- setdiff(names(dataset), c("numero_de_cliente", "clase_ternaria", "periodo0", "diff_periodo"))

# Creo un directorio para los graficos
dir.create(PARAM$carpeta_graficos, showWarnings = FALSE)

# Genero gráficos para cada feature
plot_files <- c()
for (feature in features_to_plot) {
  # Me aseguro que sea numérica para hacer media/min/max
  if (is.numeric(dataset[[feature]])) {
    summary_data <- dataset[, .(
      mean_val = mean(get(feature), na.rm = TRUE),
      max_val = max(get(feature), na.rm = TRUE),
      min_val = min(get(feature), na.rm = TRUE)
    ), by = foto_mes]

    melted_summary <- melt(summary_data, id.vars = "foto_mes", variable.name = "metric", value.name = "value")

    p <- ggplot(melted_summary, aes(x = foto_mes, y = value, color = metric)) +
      geom_line() +
      geom_point() +
      labs(title = paste("Evolución de", feature),
           x = "Foto Mes",
           y = "Valor") +
      theme_minimal()

    plot_file <- file.path(PARAM$carpeta_graficos, paste0(feature, "_evolucion.png"))
    ggsave(plot_file, plot = p, width = 10, height = 6)
    plot_files <- c(plot_files, plot_file)
  }
}

# Creo un archivo R Markdown para el reporte en HTML
rmd_content <- "
---
title: \"Reporte de Evolución de Características\"
output: html_document
---

# Evolución de Características a lo Largo del Tiempo

"
for (plot_file in plot_files) {
  rmd_content <- paste0(rmd_content, "
## ", basename(plot_file), "
")
  rmd_content <- paste0(rmd_content, "![](", plot_file, ")

")
}

rmd_file <- "reporte_evolucion_features.Rmd"
writeLines(rmd_content, rmd_file)

# Renderizo el archivo R Markdown a HTML
render(rmd_file, output_file = "evolucion_features.html", quiet = TRUE)
log_info("Evolución histórica de las features guardada en evolucion_features.html")

log_info("--- Resumen de características con ceros/NaN ---")

# Excluyo numero_de_cliente, clase_ternaria, y columnas temporales
features_to_check <- setdiff(names(dataset), c("numero_de_cliente", "clase_ternaria", "periodo0", "diff_periodo"))

# Inicializo un data.table vacio para el summary
zero_nan_summary <- data.table(foto_mes = unique(dataset$foto_mes))
setorder(zero_nan_summary, foto_mes)

for (feature in features_to_check) {
  # Por cada foto_mes, me fijo si todos los valores son 0 o NA para la feature
  summary_col <- dataset[, .(
    all_zeros = all(get(feature) == 0, na.rm = TRUE),
    all_na = all(is.na(get(feature)))
  ), by = foto_mes]

  # Merge con la tabla summary
  zero_nan_summary <- merge(zero_nan_summary, summary_col, by = "foto_mes", all.x = TRUE)

  # Creo la nueva columna basada en condiciones: todo 0 o todo nulo
  zero_nan_summary[, (feature) := ""]
  zero_nan_summary[all_zeros == TRUE, (feature) := "0"]
  zero_nan_summary[all_na == TRUE, (feature) := "NaN"]

  # Remuevo columnas temporales
  zero_nan_summary[, c("all_zeros", "all_na") := NULL]
}

log_info("Resumen de características con ceros/NaN:")
log_info(capture.output(print(zero_nan_summary)))

fwrite(zero_nan_summary, "columnas_con_cero_o_nan.csv")
log_info("Resumen de características con ceros/NaN guardada en columnas_con_cero_o_nan.csv")