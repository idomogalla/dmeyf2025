suppressPackageStartupMessages({
  if (!require("logger")) install.packages("logger")
    library("logger")
  if (!require("data.table")) install.packages("data.table")
    library("data.table")
  if (!require("ggplot2"))  install.packages("ggplot2")
    library("ggplot2")
  if (!require("rmarkdown")) install.packages("rmarkdown")
    library("rmarkdown")
  if (!require("scales")) install.packages("scales")
    library("scales")
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
PARAM$archivo_summary_log <- "summary.txt"
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
log_info("Inicio de lectura del dataset...")
dataset <- fread(file.path(PARAM$dir_dataset,"competencia_02_crudo.csv.gz"), stringsAsFactors = TRUE)

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
  dataset[, periodo0 := dsimple$periodo0 ]
  log_info(paste0("Archivo competencia_02.csv.gz con la clase ternaria generado en ", PARAM$dir_dataset))
}, error = function(e) {
  log_error("Error al generar la clase ternaria: {e$message}")
})

#------------------------------------------------------
# Sección 4: Análisis
#------------------------------------------------------
# --- Distribución de la clase ternaria ---
log_info("------------------------------------------------------")
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
log_info(paste(capture.output(print(target_distribution)), collapse = "\n"))

# Guardo como CSV
fwrite(target_distribution, "distribucion_clase_ternaria.csv")
log_info("Distribución de la clase objetivo a lo largo del tiempo guardada en distribucion_clase_ternaria.csv")

# --- Análisis de retornos ---
log_info("------------------------------------------------------")
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
log_info(paste(capture.output(print(absence_summary)), collapse = "\n"))

# Guardo como CSV
fwrite(absence_summary, "retorno_clientes.csv")
log_info("Resumen del análisis de retorno de clientes guardada en retorno_clientes.csv")

# --- Evolución de las variables ---
log_info("------------------------------------------------------")
log_info("--- Evolución de las variables ---")

# Excluyo numero_de_cliente, clase_ternaria, y cualquier columna temporal
features_to_plot <- setdiff(names(dataset), c("numero_de_cliente", "foto_mes", "clase_ternaria", "periodo0", "diff_periodo"))

# Creo un directorio para los graficos
dir.create(PARAM$carpeta_graficos, showWarnings = FALSE)

# Genero gráficos para cada feature
plot_files <- c()
for (feature in features_to_plot) {
  log_info(paste("Generando gráfico de evolución para la feature:", feature))
  # Me aseguro que sea numérica
  if (is.numeric(dataset[[feature]])) {
    
    # --- Comprobación de variable dicotómica (0/1) ---
    feature_values <- na.omit(dataset[[feature]])
    unique_vals <- unique(feature_values)
    is_binary_01 <- all(unique_vals %in% c(0, 1))

    # --- Cálculo de sumario condicional ---
    summary_data <- dataset[, {
      vals <- get(feature)
      all_na <- all(is.na(vals))
      
      calc_list <- list()
      
      if (is_binary_01) {
        # ---- ES BINARIA: Calcular Proporción de 1s ----
        calc_list$mean_val <- if (all_na) NA_real_ else sum(vals == 1, na.rm = TRUE) / .N
      } else {
        # ---- NO ES BINARIA: Calcular Media estándar ----
        calc_list$mean_val <- if (all_na) NA_real_ else as.numeric(mean(vals, na.rm = TRUE))
      }
      
      calc_list$max_val  <- if (all_na) NA_real_ else as.numeric(max(vals, na.rm = TRUE))
      calc_list$min_val  <- if (all_na) NA_real_ else as.numeric(min(vals, na.rm = TRUE))
      
      calc_list
    }, by = foto_mes]

    # ... (conversión a Date) ...
    summary_data[, foto_mes_date := as.Date(paste0(foto_mes, "01"), format = "%Y%m%d")]

    melted_summary <- melt(summary_data,
                           id.vars = "foto_mes_date",
                           measure.vars = c("mean_val", "max_val", "min_val"),
                           variable.name = "metric",
                           value.name = "value")
    
    # --- Etiquetas Y FORMATO condicionales ---
    
    local_metric_levels <- c()
    local_metric_colors <- c()
    local_metric_labels <- c()
    local_facet_labels  <- c()
    local_label_formatter <- NULL
    
    if (is_binary_01) {
      # Si es binaria, renombramos 'mean_val' a 'prop_1s'
      melted_summary[metric == "mean_val", metric := "prop_1s"]
      
      local_metric_levels <- c("prop_1s", "max_val", "min_val")
      local_metric_colors <- c("prop_1s" = "blue", "max_val" = "darkgreen", "min_val" = "red")
      local_metric_labels <- c("prop_1s" = "Proporción 1s", "max_val" = "Máximo", "min_val" = "Mínimo")
      local_facet_labels  <- c("prop_1s" = "Proporción 1s", "max_val" = "Máximo", "min_val" = "Mínimo")
      
      # <-- ¡NUEVO! Usar formato decimal simple para proporciones
      local_label_formatter <- scales::label_number(accuracy = 0.001) 
      
    } else {
      # Si no, usamos las etiquetas estándar de "Media"
      local_metric_levels <- c("mean_val", "max_val", "min_val")
      local_metric_colors <- c("mean_val" = "blue", "max_val" = "darkgreen", "min_val" = "red")
      local_metric_labels <- c("mean_val" = "Media", "max_val" = "Máximo", "min_val" = "Mínimo")
      local_facet_labels  <- c("mean_val" = "Media", "max_val" = "Máximo", "min_val" = "Mínimo")
      
      # <-- ¡NUEVO! Usar formato SI (K, M) para números grandes
      local_label_formatter <- scales::label_number(
                                 accuracy = 0.1,
                                 scale_cut = scales::cut_si("")
                               )
    }
    
    # Aplicamos el orden de factores
    melted_summary[, metric := factor(metric, levels = local_metric_levels)]

    # --- Plotting (usando las variables locales) ---
    p <- ggplot(melted_summary, aes(x = foto_mes_date, y = value, color = metric)) +
      geom_line(show.legend = FALSE, na.rm = TRUE) + 
      geom_point(show.legend = FALSE, na.rm = TRUE) +
      
      # --- Usar la función de formato condicional ---
      geom_text(
        aes(label = local_label_formatter(value)),
        vjust = -0.8,  
        size = 2.5,  
        show.legend = FALSE,
        na.rm = TRUE 
      ) +
      
      labs(title = paste("Evolución de", feature),
           x = "Foto Mes",
           y = "Valor",
           color = "Métrica") + 
      theme_minimal() +
      
      scale_x_date(
         breaks = unique(melted_summary$foto_mes_date), 
         date_labels = "%Y-%m"                          
      ) + 
      
      scale_color_manual(values = local_metric_colors, 
                         labels = local_metric_labels,
                         na.value = "grey") + 
      
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8)) +
      
      facet_wrap(~ metric, scales = "free_y", ncol = 1, 
                 labeller = labeller(metric = local_facet_labels)) +
      
      coord_cartesian(clip = "off") 

    plot_file <- file.path(PARAM$carpeta_graficos, paste0(feature, "_evolucion.png"))
    ggsave(plot_file, plot = p, width = 12, height = 10) 
    plot_files <- c(plot_files, plot_file)
  }
  log_info(paste("Gráfico guardado en:", plot_file))
}

log_info("Todos los gráficos de evolución de features generados.")
log_info("------------------------------------------------------")
log_info("Generando reporte HTML con los gráficos...")
# Creo un archivo R Markdown para el reporte en HTML
rmd_content <- "
---
title: \"Reporte de Evolución de Características\"
output: html_document
---

<style>
body {
  /* Opcional: da un poco de margen a los lados */
  padding-left: 5%;
  padding-right: 5%;
}
.main-container {
  /* Esta es la línea clave: anula el ancho fijo */
  max-width: 95% !important;
}
</style>

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
log_info("------------------------------------------------------")
log_info("--- Resumen de características con ceros/NaN ---")

# Excluyo numero_de_cliente, clase_ternaria, y columnas temporales
features_to_check <- setdiff(names(dataset), c("numero_de_cliente", "clase_ternaria", "foto_mes", "periodo0", "diff_periodo"))

# 1. "Derretimos" (melt) el dataset a formato largo
melted_data <- melt(dataset, 
                    id.vars = "foto_mes", 
                    measure.vars = features_to_check,
                    variable.name = "feature", # Nombre de la columna de features
                    value.name = "value")      # Nombre de la columna de valores

# 2. Calculamos el "estado" para cada grupo (foto_mes, feature)
summary_calc <- melted_data[, {
  
  # Verificamos si todos los valores son NA
  all_na <- all(is.na(value))
  
  if (all_na) {
    status <- "NaN"
  } else {
    # Si no son todos NA, filtramos los NA para chequear los ceros
    vals_no_na <- na.omit(value)
    
    # Comprobamos si (1) hay al menos un valor no-NA y (2) todos son 0
    if (length(vals_no_na) > 0 && all(vals_no_na == 0)) {
      status <- "0"
    } else {
      # Si hay al menos un valor distinto de 0 (y no-NA), queda vacío
      status <- ""
    }
  }
  
  # Devolvemos una lista (data.table espera una lista en .SD)
  list(status = status)
  
}, by = .(foto_mes, feature)] # Agrupamos por mes y por feature

# 3. "Ensanchamos" (dcast) la tabla de vuelta al formato final
zero_nan_summary <- dcast(summary_calc, 
                          foto_mes ~ feature, 
                          value.var = "status")

# 4. Ordenamos por foto_mes
setorder(zero_nan_summary, foto_mes)

# --- Log y guardado ---
log_info("Resumen de características con ceros/NaN:")
log_info(paste(capture.output(print(zero_nan_summary)), collapse = "\n"))

fwrite(zero_nan_summary, "columnas_con_cero_o_nan.csv")
log_info("Resumen de características con ceros/NaN guardada en columnas_con_cero_o_nan.csv")