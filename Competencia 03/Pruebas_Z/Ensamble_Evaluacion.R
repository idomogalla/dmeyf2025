# Limpiamos la memoria
rm(list = ls())
gc()

require("data.table")
require("ggplot2")
require("ggrepel")
require("logger")

#------------------------------------------------------------------------------
# VARIABLES DE ENTRADA
#------------------------------------------------------------------------------
carpeta_experimento <- "~/buckets/b1/exp"
experimento <- "ensamble_final"

dir_dataset <- "~/buckets/b1/datasets/competencia_03_ternaria.csv.gz"
mes_evaluacion <- 202107
archivos_probabilidades <- c(
    "prob1.csv",
    "prob2.csv"
)
semilla_azar <- 102191

# Cortes para evaluar la ganancia
cortes_evaluacion <- seq(from = 500, to = 20000, by = 500)

carpeta_salida <- file.path(carpeta_experimento, experimento)
dir.create(carpeta_salida, showWarnings = FALSE, recursive = TRUE)

# Configurar Logger
log_file <- file.path(carpeta_salida, paste0("log_", experimento, ".txt"))
log_appender(appender_tee(log_file))

log_info(paste("Iniciando experimento:", experimento))
log_info(paste("Carpeta de salida:", carpeta_salida))

#------------------------------------------------------------------------------
# FUNCIONES HELPER
#------------------------------------------------------------------------------

particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
    if (!is.na(seed)) set.seed(seed, "L'Ecuyer-CMRG")
    bloque <- unlist(mapply(function(x, y) {
        rep(y, x)
    }, division, seq(from = start, length.out = length(division))))
    data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N], by = agrupa]
}

realidad_inicializar <- function(pfuture, semilla) {
    drealidad <- pfuture[, list(numero_de_cliente, foto_mes, clase_ternaria)]
    particionar(drealidad, division = c(3, 7), agrupa = "clase_ternaria", seed = semilla)
    return(drealidad)
}

realidad_evaluar <- function(prealidad, pprediccion) {
    prealidad_eval <- copy(prealidad)
    # Merge por numero_de_cliente y foto_mes
    prealidad_eval[pprediccion, on = c("numero_de_cliente", "foto_mes"), predicted := i.Predicted]

    # Si no matcheó, predicted es NA, lo ponemos en 0
    prealidad_eval[is.na(predicted), predicted := 0L]

    tbl <- prealidad_eval[, list("qty" = .N), list(fold, predicted, clase_ternaria)]
    res <- list()
    res$public <- tbl[fold == 1 & predicted == 1L, sum(qty * ifelse(clase_ternaria == "BAJA+2", 780000, -20000))] / 0.3
    res$private <- tbl[fold == 2 & predicted == 1L, sum(qty * ifelse(clase_ternaria == "BAJA+2", 780000, -20000))] / 0.7
    res$total <- tbl[predicted == 1L, sum(qty * ifelse(clase_ternaria == "BAJA+2", 780000, -20000))]
    return(res)
}

GraficarCurvasEnsemble <- function(lista_resultados, tb_resultados_ensamble, carpeta_graficos, nombre_experimento) {
    log_info("Iniciando la graficación de la superposición de curvas del ensemble.")

    tb_todas <- rbindlist(lapply(names(lista_resultados), function(sem) {
        lista_resultados[[sem]][, semilla := as.character(sem)]
    }))

    tb_promedio_visual <- tb_todas[, .(ganancia_total = mean(ganancia_total)), by = clientes]

    setnames(tb_resultados_ensamble, "ganancia_total", "ganancia_ensamble_real")

    if (!"ganancia_meseta" %in% colnames(tb_resultados_ensamble)) {
        tb_resultados_ensamble[, ganancia_meseta := NA_real_]
    }

    maximo_punto <- tb_resultados_ensamble[ganancia_ensamble_real == max(ganancia_ensamble_real, na.rm = TRUE)]
    maximo_punto <- head(maximo_punto, 1)

    maximo_meseta <- tb_resultados_ensamble[ganancia_meseta == max(ganancia_meseta, na.rm = TRUE)]
    maximo_meseta <- head(maximo_meseta, 1)

    semillas_unicas <- unique(tb_todas$semilla)
    label_ensamble_real <- "Ensamble Real (Negro)"
    label_promedio_visual <- "Promedio Visual (Azul)"
    label_meseta <- "Meseta Suavizada (Púrpura)"

    labels_plot <- c(semillas_unicas, label_ensamble_real, label_promedio_visual, label_meseta)
    names(labels_plot) <- c(semillas_unicas, "Ensamble Real", "Promedio Visual", "Meseta")

    colores_individuales <- scales::hue_pal()(length(semillas_unicas))
    names(colores_individuales) <- semillas_unicas
    colores_plot <- c(
        colores_individuales,
        "Ensamble Real" = "black",
        "Promedio Visual" = "blue",
        "Meseta" = "purple"
    )

    p <- ggplot() +
        geom_line(data = tb_todas, aes(x = clientes, y = ganancia_total, group = semilla, color = semilla), alpha = 0.45, linewidth = 0.5) +
        geom_line(data = tb_promedio_visual, aes(x = clientes, y = ganancia_total, color = "Promedio Visual"), linewidth = 0.8, linetype = "dashed") +
        geom_line(data = tb_resultados_ensamble, aes(x = clientes, y = ganancia_ensamble_real, color = "Ensamble Real"), linewidth = 1.0) +
        geom_line(data = tb_resultados_ensamble, aes(x = clientes, y = ganancia_meseta, color = "Meseta"), linewidth = 0.8, linetype = "dotdash") +
        geom_point(data = maximo_punto, aes(x = clientes, y = ganancia_ensamble_real), color = "red", size = 3, shape = 16) +
        geom_point(data = maximo_meseta, aes(x = clientes, y = ganancia_meseta), color = "purple", size = 3, shape = 17) +
        geom_label_repel(
            data = maximo_punto,
            aes(
                x = clientes, y = ganancia_ensamble_real,
                label = paste0(
                    "Máximo\n",
                    format(ganancia_ensamble_real,
                        big.mark = ".",
                        decimal.mark = ",", scientific = FALSE
                    ),
                    "\nEnvios\n",
                    format(clientes, big.mark = ".", decimal.mark = ",")
                )
            ),
            fill = "white", color = "red", fontface = "bold",
            label.padding = unit(0.3, "lines"),
            nudge_y = 80000000,
            segment.color = "grey30",
            min.segment.length = 0,
            direction = "y"
        ) +
        geom_label_repel(
            data = maximo_meseta,
            aes(
                x = clientes, y = ganancia_meseta,
                label = paste0(
                    "Meseta\n",
                    format(ganancia_meseta,
                        big.mark = ".",
                        decimal.mark = ",", scientific = FALSE
                    ),
                    "\nEnvios\n",
                    format(clientes, big.mark = ".", decimal.mark = ",")
                )
            ),
            fill = "white", color = "purple", fontface = "bold",
            label.padding = unit(0.3, "lines"),
            nudge_y = -90000000,
            segment.color = "grey30",
            min.segment.length = 0,
            direction = "y"
        ) +
        scale_y_continuous(
            labels = scales::comma,
            expand = expansion(mult = c(0.1, 0.25))
        ) +
        scale_x_continuous(labels = scales::comma) +
        scale_color_manual(
            name = "Modelo",
            values = colores_plot,
            labels = labels_plot
        ) +
        labs(
            title = paste0("Ganancia Acumulada (Semillas y Ensamble) - ", nombre_experimento),
            x = "Clientes Contactados (Envíos)",
            y = "Ganancia Acumulada"
        ) +
        theme_minimal() +
        theme(
            legend.position = "right",
            legend.text = element_text(size = 8),
            legend.title = element_text(size = 9, face = "bold"),
            legend.key.size = unit(0.5, "lines"),
            plot.margin = margin(10, 10, 10, 10)
        ) +
        guides(color = guide_legend(override.aes = list(
            alpha = 1,
            linewidth = 1.5,
            linetype = c(rep("solid", length(semillas_unicas) + 1), "dashed", "dotdash")
        )))

    ruta_grafico <- file.path(carpeta_graficos, "eval_curvas_ensamble.png")
    ggsave(
        ruta_grafico,
        plot = p,
        width = 14,
        height = 8
    )

    log_info(paste0("Gráfico de superposición de curvas del ensemble guardado en: ", ruta_grafico))
}

#------------------------------------------------------------------------------
# LÓGICA PRINCIPAL
#------------------------------------------------------------------------------

# Cargar dataset
log_info("Cargando dataset...")
dataset <- fread(dir_dataset)

# Filtrar por mes de evaluación
log_info(paste("Filtrando por mes:", mes_evaluacion))
dfuture <- dataset[foto_mes %in% mes_evaluacion]
if (nrow(dfuture) == 0) {
    log_error("No hay registros para el mes de evaluación especificado.")
    stop("No hay registros para el mes de evaluación especificado.")
}

# Inicializar realidad (ground truth)
log_info("Inicializando realidad...")
drealidad <- realidad_inicializar(dfuture, semilla_azar)

# Listas para guardar resultados
lista_resultados_individuales <- list()
lista_predicciones <- list()

# Procesar cada archivo de probabilidades
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

    # Agregar foto_mes para el merge (asumimos que corresponde al mes de evaluación)
    tb_pred_individual[, foto_mes := mes_evaluacion]

    # Guardar para el ensamble
    lista_predicciones[[nombre_modelo]] <- copy(tb_pred_individual)

    # Evaluar individualmente
    resultados_individual <- data.table()
    setorder(tb_pred_individual, -prob)

    for (envios in cortes_evaluacion) {
        if (envios > 0 && envios <= nrow(tb_pred_individual)) {
            tb_pred_individual[, Predicted := 0L]
            tb_pred_individual[1:envios, Predicted := 1L]
            res_ind <- realidad_evaluar(drealidad, tb_pred_individual)
            resultados_individual <- rbind(
                resultados_individual,
                data.table(clientes = envios, ganancia_total = res_ind$total)
            )
        }
    }

    lista_resultados_individuales[[nombre_modelo]] <- resultados_individual
}

# Ensamble Promediado
log_info("Calculando Ensamble Promediado...")
predicciones_todas <- rbindlist(lista_predicciones)
tb_prediccion_ensamble <- predicciones_todas[, .(prob = mean(prob)), by = .(numero_de_cliente, foto_mes)]

# Evaluar Ensamble
resultados_ensamble <- data.table()
setorder(tb_prediccion_ensamble, -prob)

for (envios in cortes_evaluacion) {
    if (envios > 0 && envios <= nrow(tb_prediccion_ensamble)) {
        tb_prediccion_ensamble[, Predicted := 0L]
        tb_prediccion_ensamble[1:envios, Predicted := 1L]
        res_ens <- realidad_evaluar(drealidad, tb_prediccion_ensamble)
        resultados_ensamble <- rbind(
            resultados_ensamble,
            data.table(clientes = envios, ganancia_total = res_ens$total)
        )
    }
}

# Calcular Meseta para el Ensamble
log_info("Calculando Meseta...")
smoothing_window_clients <- 1000
step_size <- cortes_evaluacion[2] - cortes_evaluacion[1]
n_pasos_ventana <- as.integer(round(smoothing_window_clients / step_size))
n_ventana_meseta <- (2 * n_pasos_ventana) + 1

resultados_ensamble[, ganancia_meseta := frollmean(
    x = ganancia_total,
    n = n_ventana_meseta,
    align = "center",
    na.rm = TRUE,
    hasNA = TRUE
)]

# Graficar
GraficarCurvasEnsemble(
    lista_resultados_individuales,
    resultados_ensamble,
    carpeta_salida,
    experimento
)

log_info("Proceso finalizado.")
