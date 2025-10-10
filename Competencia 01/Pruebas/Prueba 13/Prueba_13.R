#!/usr/bin/env Rscript
#------------------------------------------------------
# Sección 1: Carga de Librerías
#------------------------------------------------------
suppressPackageStartupMessages({
  if (!require("logger"))
    install.packages("logger")
  library("logger")
  if (!require("data.table"))
    install.packages("data.table")
  library("data.table")
  if (!require("parallel"))
    install.packages("parallel")
  library("parallel")
  if (!require("R.utils"))
    install.packages("R.utils")
  library("R.utils")
  if (!require("primes"))
    install.packages("primes")
  library("primes")
  if (!require("utils"))
    install.packages("utils")
  library("utils")
  if (!require("rlist"))
    install.packages("rlist")
  library("rlist")
  if (!require("yaml"))
    install.packages("yaml")
  library("yaml")
  if (!require("lightgbm"))
    install.packages("lightgbm")
  library("lightgbm")
  if (!require("DiceKriging"))
    install.packages("DiceKriging")
  library("DiceKriging")
  if (!require("mlrMBO"))
    install.packages("mlrMBO")
  library("mlrMBO")
  if (!require("ggplot2"))
    install.packages("ggplot2")
  library("ggplot2")
  if (!require("ggrepel"))
    install.packages("ggrepel")
  library("ggrepel")
  if (!require("scales"))
    install.packages("scales")
  library("scales")
})
#------------------------------------------------------
# Sección 2: Configuración Inicial y Parámetros
#------------------------------------------------------
rm(list = ls(all.names = TRUE))
gc(full = TRUE, verbose = FALSE)

PARAM <- list()
PARAM$experimento <- "expC01_Prueba13"
PARAM$dir_experimento <- paste0("~/buckets/b1/exp/", PARAM$experimento)
PARAM$dir_dataset <- "~/buckets/b1/datasets/"
PARAM$carpeta_logs <- "logs/"
PARAM$carpeta_bayesiana <- "Archivos Bayesiana/"
PARAM$carpeta_kaggle <- "Kaggle/"
PARAM$carpeta_kaggle_ensamble <- "Kaggle_Promediado/"
PARAM$carpeta_graficos <- "Plots/"
PARAM$carpeta_entregables <- "Entregables/"
PARAM$archivo_summary_log <- "summary.txt"


PARAM$semilla_primigenia <- 200003
PARAM$semillas_ensemble <- c(200003, 300007, 400009, 500009, 600011, 314159, 102191, 111109, 230101, 100129)
PARAM$train <- c(202101, 202102)
PARAM$train_final <- c(202101, 202102)
PARAM$future <- c(202104)
PARAM$train_final_kaggle <- c(202101, 202102, 202103, 202104)
PARAM$entrega_kaggle <- c(202106)
PARAM$semilla_kaggle <- 314159
PARAM$cortes <- seq(0, 20000, by = 100)

PARAM$trainingstrategy$undersampling <- 0.5

PARAM$hyperparametertuning$xval_folds <- 5
PARAM$lgbm$param_fijos <- list(
  boosting= "gbdt", # puede ir  dart  , ni pruebe random_forest
  objective= "binary",
  metric= "auc",
  first_metric_only= FALSE,
  boost_from_average= TRUE,
  feature_pre_filter= FALSE,
  force_row_wise= TRUE, # para reducir warnings
  verbosity= -100,

  seed= PARAM$semilla_primigenia,

  max_depth= -1L, # -1 significa no limitar,  por ahora lo dejo fijo
  min_gain_to_split= 0, # min_gain_to_split >= 0
  min_sum_hessian_in_leaf= 0.001, #  min_sum_hessian_in_leaf >= 0.0
  lambda_l1= 0.0, # lambda_l1 >= 0.0
  lambda_l2= 0.0, # lambda_l2 >= 0.0
  max_bin= 31L, # lo debo dejar fijo, no participa de la BO

  bagging_fraction= 1.0, # 0.0 < bagging_fraction <= 1.0
  pos_bagging_fraction= 1.0, # 0.0 < pos_bagging_fraction <= 1.0
  neg_bagging_fraction= 1.0, # 0.0 < neg_bagging_fraction <= 1.0
  is_unbalance= FALSE, #
  scale_pos_weight= 1.0, # scale_pos_weight > 0.0

  drop_rate= 0.1, # 0.0 < neg_bagging_fraction <= 1.0
  max_drop= 50, # <=0 means no limit
  skip_drop= 0.5, # 0.0 <= skip_drop <= 1.0

  extra_trees= FALSE,

  num_iterations= 1200,
  learning_rate= 0.02,
  feature_fraction= 0.5,
  num_leaves= 750,
  min_data_in_leaf= 5000,
  early_stopping_round = 100
)
# Bordes de hiperparámetros para BO
PARAM$hyperparametertuning$hs <- makeParamSet(
  makeIntegerParam("num_leaves", lower = 10L, upper = 2048L),
  
  makeIntegerParam("num_iterations", lower = 50L, upper = 3000L),
  makeNumericParam("learning_rate", lower = 0.01, upper = 0.5),
  
  makeNumericParam("feature_fraction", lower = 0.1, upper = 1.0),
  
  makeIntegerParam("min_data_in_leaf", lower = 1L, upper = 10000L)

)
PARAM$hyperparametertuning$iteraciones <- 100

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
cat(paste0("Resumen del Experimento: ", PARAM$experimento, "\n"),
    file = summary_log_file_path,
    append = FALSE)
cat(paste0("Fecha: ", Sys.time(), "\n\n"),
    file = summary_log_file_path,
    append = TRUE)


log_info("------------------------------------------------------")
log_info(paste("Inicio del script. Experimento:", PARAM$experimento))
log_info(paste("El log se guardará en:", log_file))
log_info(paste("El resumen se guardará en:", summary_log_file_path))
log_info("------------------------------------------------------")

#------------------------------------------------------
# Sección 3: Funciones Auxiliares
#------------------------------------------------------
log_info("Iniciando Sección 3: Cargando funciones auxiliares")

# Función para escribir en el log de resumen
log_summary <- function(message) {
  cat(paste0(message, "\n"),
      file = summary_log_file_path,
      append = TRUE)
}

particionar <- function(data,
                        division,
                        agrupa = "",
                        campo = "fold",
                        start = 1,
                        seed = NA) {
  if (!is.na(seed))
    set.seed(seed, "L'Ecuyer-CMRG")
  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(
    from = start, length.out = length(division)
  )))
  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N], by = agrupa]
}

realidad_inicializar <- function(pfuture, pparam) {
  drealidad <- pfuture[, list(numero_de_cliente, foto_mes, clase_ternaria)]
  particionar(
    drealidad,
    division = c(3, 7),
    agrupa = "clase_ternaria",
    seed = pparam$semilla_kaggle
  )
  return(drealidad)
}

realidad_evaluar <- function(prealidad, pprediccion) {
  prealidad[pprediccion, on = c("numero_de_cliente", "foto_mes"), predicted := i.Predicted]
  tbl <- prealidad[, list("qty" = .N), list(fold, predicted, clase_ternaria)]
  res <- list()
  res$public <- tbl[fold == 1 &
                      predicted == 1L, sum(qty * ifelse(clase_ternaria == "BAJA+2", 780000, -20000))] / 0.3
  res$private <- tbl[fold == 2 &
                       predicted == 1L, sum(qty * ifelse(clase_ternaria == "BAJA+2", 780000, -20000))] / 0.7
  res$total <- tbl[predicted == 1L, sum(qty * ifelse(clase_ternaria == "BAJA+2", 780000, -20000))]
  prealidad[, predicted := NULL]
  return(res)
}

tryCatch({
  #------------------------------------------------------
  # Sección 4: Preparación de Datos
  #------------------------------------------------------
  log_info("Iniciando Sección 4: Preparación de Datos.")
  log_info(paste("Leyendo dataset desde:", PARAM$dir_dataset))
  dataset <- fread(file.path(PARAM$dir_dataset, "competencia_01.csv.gz"),
                   stringsAsFactors = TRUE)
  log_info("Dataset cargado correctamente.")
  
  log_info("Inicio de Feature Engineering")
  setkey(dataset, numero_de_cliente, foto_mes)
  
  # Columnas a las que se les aplicará el ranking
  cols_a_rankear <- c(
    "mcomisiones_mantenimiento", "Master_Fvencimiento", "Visa_fultimo_cierre", "Master_fultimo_cierre", "mpayroll", "cpayroll_trx"
  )
  
  nuevas_cols_rank <- paste0(cols_a_rankear, "_rank")
  
  rank_con_cero_fijo <- function(x) {
    resultado <- numeric(length(x))
    idx_pos <- which(x > 0)
    idx_neg <- which(x < 0)
    idx_cero <- which(x == 0)
    
    if (length(idx_pos) > 0) {
      resultado[idx_pos] <- frankv(x[idx_pos], ties.method = "average") / length(idx_pos)
    }
    if (length(idx_neg) > 0) {
      resultado[idx_neg] <- (frankv(-x[idx_neg], ties.method = "average") / length(idx_neg)) * -1
    }
    if (length(idx_cero) > 0) {
      resultado[idx_cero] <- 0
    }
    return(resultado)
  }
  
  dataset[, (nuevas_cols_rank) := lapply(.SD, rank_con_cero_fijo), by = foto_mes, .SDcols = cols_a_rankear]
  dataset[, (cols_a_rankear) := NULL]
  
  log_info("Inicio de Feature Lags")
  cols_a_excluir <- c("numero_de_cliente", "foto_mes", "clase_ternaria")
  cols_con_lag <- setdiff(names(dataset), cols_a_excluir)

  nombres_nuevas_cols_lag <- paste0(cols_con_lag, "_lag1")
  dataset[, (nombres_nuevas_cols_lag) := shift(.SD, 1, NA, "lag"), by = numero_de_cliente, .SDcols = cols_con_lag]

  nombres_nuevas_cols_delta <- paste0(cols_con_lag, "_delta1")
  dataset[, (nombres_nuevas_cols_delta) :=  Map(function(col, col_lag) get(col) - get(col_lag), cols_con_lag, nombres_nuevas_cols_lag)]

  nombres_nuevas_cols_delta_pct <- paste0(cols_con_lag, "_delta_pct1")
  dataset[, (nombres_nuevas_cols_delta_pct) := Map(
    function(col, col_lag) {
      lag_val <- get(col_lag)
      curr_val <- get(col)
      delta_pct <- ifelse(is.na(lag_val) | lag_val == 0, NA, (curr_val - lag_val) / abs(lag_val))
      return(delta_pct)
    },
    cols_con_lag, nombres_nuevas_cols_lag
  )]

  log_info("Features de lag y delta generadas.")
  dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]
  
  log_info("Generando dataset de entrenamiento.")
  dataset_train <- dataset[foto_mes %in% PARAM$train]
  log_info("Clase convertida a formato binario.")
  
  set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")
  dataset_train[, azar := runif(nrow(dataset_train))]
  dataset_train[, training := 0L]
  dataset_train[
    foto_mes %in%  PARAM$train &
      (azar <= PARAM$trainingstrategy$undersampling | clase_ternaria %in% c("BAJA+1", "BAJA+2")),
    training := 1L
  ]
  log_info(paste0("Undersampling aplicado con una tasa de:",PARAM$trainingstrategy$undersampling))
  
  campos_buenos <- setdiff(colnames(dataset_train),
                           c("clase_ternaria", "clase01", "azar", "training"))
  dtrain <- lgb.Dataset(
    data = data.matrix(dataset_train[training == 1L, campos_buenos, with = FALSE]),
    label = dataset_train[training == 1L, clase01],
    free_raw_data = FALSE
  )
  log_info("Dataset de entrenamiento para LightGBM creado.")
  log_info(paste(
    "Dimensiones de dtrain -> Filas:",
    nrow(dtrain),
    "| Columnas:",
    ncol(dtrain)
  ))
  
}, error = function(e) {
  log_error("######################################################")
  log_error("Se ha producido un error fatal en la ejecución.")
  log_error("Revisa el último mensaje 'INFO' en el log para identificar la sección donde ocurrió el fallo.")
  log_error(paste("Mensaje de R:", e$message))
  log_error("######################################################")
  quit(status = 1)
})

tryCatch({
  #------------------------------------------------------
  # Sección 5: Optimización Bayesiana
  #------------------------------------------------------
  log_info("Iniciando Sección 5: Optimización Bayesiana de Hiperparámetros.")
  EstimarGanancia_AUC_lightgbm <- function(x) {
    param_completo <- modifyList(PARAM$lgbm$param_fijos, x)
    modelocv <- lgb.cv(
      data = dtrain,
      nfold = PARAM$hyperparametertuning$xval_folds,
      stratified = TRUE,
      param = param_completo
    )
    AUC <- modelocv$best_score
    rm(modelocv)
    gc(full = TRUE, verbose = FALSE)
    log_info(paste(
      "Iteración BO -> AUC:",
      format(AUC, digits = 6),
      "|",
      format(Sys.time(), "%a %b %d %X %Y")
    ))
    return(AUC)
  }
  
  dir.create(PARAM$carpeta_bayesiana, showWarnings = FALSE)
  kbayesiana <- paste0(PARAM$carpeta_bayesiana, "bayesiana.RDATA")
  funcion_optimizar <- EstimarGanancia_AUC_lightgbm
  configureMlr(show.learner.output = FALSE)
  obj.fun <- makeSingleObjectiveFunction(
    fn = funcion_optimizar,
    minimize = FALSE,
    noisy = TRUE,
    par.set = PARAM$hyperparametertuning$hs,
    has.simple.signature = FALSE
  )
  ctrl <- makeMBOControl(save.on.disk.at.time = 600,
                         save.file.path = kbayesiana)
  ctrl <- setMBOControlTermination(ctrl, iters = PARAM$hyperparametertuning$iteraciones)
  ctrl <- setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())
  surr.km <- makeLearner(
    "regr.km",
    predict.type = "se",
    covtype = "matern3_2",
    control = list(trace = FALSE)
  )
  
  if (!file.exists(kbayesiana)) {
    log_info(
      paste(
        "Iniciando nueva búsqueda Bayesiana de",
        PARAM$hyperparametertuning$iteraciones,
        "iteraciones."
      )
    )
    bayesiana_salida <- mbo(obj.fun, learner = surr.km, control = ctrl)
  } else {
    log_info("Continuando búsqueda Bayesiana desde archivo existente.")
    bayesiana_salida <- mboContinue(kbayesiana)
  }
  log_info("Optimización Bayesiana finalizada.")
  
  tb_bayesiana <- as.data.table(bayesiana_salida$opt.path)
  tb_bayesiana[, iter := .I]
  setorder(tb_bayesiana, -y)
  fwrite(
    tb_bayesiana,
    file = paste0(PARAM$carpeta_bayesiana, "BO_log.txt"),
    sep = "\t"
  )
  PARAM$out$lgbm$mejores_hiperparametros <- tb_bayesiana[1, setdiff(
    colnames(tb_bayesiana),
    c(
      "y", "dob", "eol", "error.message", "exec.time",
      "ei", "error.model", "train.time", "prop.type",
      "propose.time", "se", "mean", "iter"
    )
  ), with = FALSE]
  PARAM$out$lgbm$y <- tb_bayesiana[1, y]
  write_yaml(PARAM, file = paste0(PARAM$carpeta_bayesiana, "PARAM.yml"))
  
  log_info("Mejores hiperparámetros encontrados:")
  log_info(paste(capture.output(
    print(PARAM$out$lgbm$mejores_hiperparametros)
  ), collapse = "\n"))
  log_info(paste("Mejor AUC (y):", PARAM$out$lgbm$y))
  
  # Escribir resultados de la BO en el log de resumen
  log_summary("--- Resultados de la Optimización Bayesiana ---")
  log_summary("Hiperparámetros Óptimos:")
  hiperparametros_texto <- capture.output(print(PARAM$out$lgbm$mejores_hiperparametros))
  sapply(hiperparametros_texto, log_summary)
  log_summary(paste("\nAUC:", format(PARAM$out$lgbm$y, digits = 6)))
  log_summary("---------------------------------------------\n")

}, error = function(e) {
  log_error("######################################################")
  log_error("Se ha producido un error fatal en la Sección 5: Optimización Bayesiana.")
  log_error(paste("Mensaje de R:", e$message))
  log_error("######################################################")
  quit(status = 1)
})

#------------------------------------------------------
# Funciones para secciones 6 y 7
#------------------------------------------------------
EvaluarYGraficar <- function(tb_prediccion,
                              drealidad,
                              PARAM,
                              tipo_modelo,
                              carpeta_salida_kaggle) {
  log_info(paste0("Iniciando evaluación y graficación para el modelo: ",tipo_modelo))
  dir.create(carpeta_salida_kaggle, showWarnings = FALSE)
  resultados <- data.table()
  
  setorder(tb_prediccion, -prob)
  
  for (envios in PARAM$cortes) {
    tb_prediccion[, Predicted := 0L]
    tb_prediccion[1:envios, Predicted := 1L]
    
    res <- realidad_evaluar(drealidad, tb_prediccion)
    
    resultados <- rbind(
      resultados,
      data.table(
        clientes = envios,
        ganancia_total = res$total,
        ganancia_public = res$public,
        ganancia_private = res$private
      )
    )

    options(scipen = 999)
    log_info(
      sprintf(
        "Envios=%-5d | TOTAL=%11.0f | Public=%11.0f | Private=%11.0f",
        envios,
        res$total,
        res$public,
        res$private
      )
    )
  }
  
  archivo_resultados_csv <- paste0(PARAM$carpeta_bayesiana, "envios_", tipo_modelo, "_", PARAM$experimento, ".csv")
  resultados_para_csv <- copy(resultados)
  setnames(
    resultados_para_csv,
    old = c("clientes", "ganancia_total", "ganancia_public", "ganancia_private"),
    new = c("ENVIOS", "TOTAL", "PUBLIC", "PRIVATE")
  )
  fwrite(resultados_para_csv, file = archivo_resultados_csv)
  log_info(paste0("Tabla de resultados para [", tipo_modelo, "] guardada en: ", archivo_resultados_csv))
  
  max_ganancia_valor <- max(resultados$ganancia_total)
  envios_max_total <- resultados[ganancia_total == max_ganancia_valor, clientes]
  log_info(paste0("Envíos óptimos para [", tipo_modelo, "]: ", paste(envios_max_total, collapse = ", ")))
  log_info(paste0("Máxima ganancia para [", tipo_modelo, "]: ", max_ganancia_valor)) # Log a la consola
  
  # # Ganancia privada
  # max_ganancia_private <- max(resultados$ganancia_private)
  # envios_max_private <- resultados[ganancia_private == max_ganancia_private, clientes]
  # log_info(paste0("Envíos óptimos para Ganancia PRIVADA: ", paste(envios_max_private, collapse = ", ")))

  # envios_optimos_combinados <- unique(c(envios_max_total, envios_max_private))
  envios_optimos_combinados <- unique(c(envios_max_total))
  log_info(paste0("Envíos óptimos combinados a retornar: ", paste(sort(envios_optimos_combinados), collapse = ", ")))
  
  resultados_long <- melt(
    resultados,
    id.vars = "clientes",
    measure.vars = c("ganancia_total", "ganancia_public", "ganancia_private"),
    variable.name = "tipo",
    value.name = "ganancia"
  )
  
  maximos <- resultados_long[, .SD[ganancia == max(ganancia)], by = tipo]
  
  maximos_leyenda <- maximos[, .(ganancia = first(ganancia)), by = tipo]
  etiquetas <- paste0(
    tools::toTitleCase(gsub("_", " ", maximos_leyenda$tipo)),
    " (máx = ",
    format(maximos_leyenda$ganancia, big.mark = ".", decimal.mark = ","),
    ")"
  )
  names(etiquetas) <- maximos_leyenda$tipo
  
  dir.create(PARAM$carpeta_graficos, showWarnings = FALSE)
  p <- ggplot(resultados_long, aes(x = clientes, y = ganancia, color = tipo)) +
    geom_line(linewidth = 1) +
    geom_point(data = maximos,
              aes(x = clientes, y = ganancia, color = tipo),
              size = 3) +
    ggrepel::geom_text_repel(
      data = maximos,
      aes(label = clientes),
      color = "black",
      size = 3.5,
      box.padding = 0.5,
      point.padding = 0.5,
      min.segment.length = 0,
      segment.color = 'grey50'
    ) +
    labs(
      title = paste0("Curvas de Ganancia (Modelo ", tools::toTitleCase(gsub("_", " ", tipo_modelo))," - ",PARAM$experimento,")"),
      x = "Clientes Contactados (Envíos)",
      y = "Ganancia",
      color = "Máximos"
    ) +
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(
      values = c(
        "ganancia_total" = "steelblue",
        "ganancia_public" = "forestgreen",
        "ganancia_private" = "firebrick"
      ),
      labels = etiquetas
    ) +
    theme_minimal() +
    theme(plot.margin = margin(10, 10, 10, 10),
          legend.position = "bottom") +
    guides(color = guide_legend(nrow = 3, byrow = TRUE))
  
  ggsave(
    paste0(PARAM$carpeta_graficos, "curvas_", tipo_modelo, "_", PARAM$experimento, ".png"),
    plot = p,
    width = 11,
    height = 7
  )
  log_info(paste0("Gráfico de curvas de ganancia (", tipo_modelo, ") guardado."))
  
  return(list(envios_optimos = envios_optimos_combinados, max_ganancia = max_ganancia_valor))
}

GenerarEnviosKaggle <- function(tb_prediccion,
                              envios_optimos,
                              tipo_modelo,
                              carpeta_salida,
                              experimento_id) {

  log_info(paste0("Iniciando generación de envíos para Kaggle del modelo: '", tipo_modelo, "'"))

  envios_a_generar <- unique(c(envios_optimos, 10500))

  log_info(paste0("Se generarán archivos para los siguientes envíos: ", paste(envios_a_generar, collapse = ", ")))

  dir.create(carpeta_salida, showWarnings = FALSE)

  setorder(tb_prediccion, -prob)

  for (envios in envios_a_generar) {
    tb_prediccion[, Predicted := 0L]
    tb_prediccion[1:envios, Predicted := 1L]

    archivo_kaggle <- paste0(carpeta_salida,experimento_id, "_",tipo_modelo, "_",envios, ".csv")

    fwrite(tb_prediccion[, list(numero_de_cliente, Predicted)],
          file = archivo_kaggle,
          sep = ",")

    log_info(paste0("Archivo generado: ", archivo_kaggle))
  }

  log_info(paste0("Generación de envíos para '", tipo_modelo, "' finalizada."))
}

GraficarCurvasEnsemble <- function(lista_resultados, PARAM) {
  log_info("Iniciando la graficación de la superposición de curvas del ensemble.")

  tb_todas <- rbindlist(lapply(names(lista_resultados), function(sem) {
    lista_resultados[[sem]][, semilla := as.character(sem)]
  }))

  tb_promedio <- tb_todas[, .(ganancia_total = mean(ganancia_total)), by = clientes]

  maximos_individuales <- tb_todas[, .SD[ganancia_total == max(ganancia_total)], by = semilla]
  maximos_individuales <- maximos_individuales[, head(.SD, 1), by = semilla]

  maximo_promedio <- tb_promedio[ganancia_total == max(ganancia_total)]
  maximo_promedio <- head(maximo_promedio, 1)

  labels_individuales <- sapply(maximos_individuales$semilla, function(sem) {
    max_info <- maximos_individuales[semilla == sem]
    paste0("S ", sem, 
          ": G ", format(max_info$ganancia_total, big.mark = ".", decimal.mark = ","),
          " (E ", max_info$clientes, ")")
  })

  label_promedio <- paste0("Promedio: G ",
                          format(maximo_promedio$ganancia_total, big.mark = ".", decimal.mark = ","),
                          " (E ", maximo_promedio$clientes, ")")

  colores_individuales <- scales::hue_pal()(nrow(maximos_individuales))
  names(colores_individuales) <- maximos_individuales$semilla
  
  colores_plot <- c(colores_individuales, "Promedio" = "black")
  labels_plot <- c(labels_individuales, "Promedio" = label_promedio)
  names(labels_plot) <- c(names(colores_individuales), "Promedio")

  dir.create(PARAM$carpeta_graficos, showWarnings = FALSE)

  p <- ggplot() +
    geom_line(data = tb_todas,
              aes(x = clientes, y = ganancia_total, group = semilla, color = semilla),
              alpha = 0.5, linewidth = 1) +
    geom_line(data = tb_promedio,
              aes(x = clientes, y = ganancia_total, color = "Promedio"),
              linewidth = 1) +
    geom_point(data = maximos_individuales,
              aes(x = clientes, y = ganancia_total, color = semilla),
              size = 3, alpha = 0.7) +
    geom_point(data = maximo_promedio,
              aes(x = clientes, y = ganancia_total, color = "Promedio"),
              size = 4, shape = 18) +
    ggrepel::geom_text_repel(data = maximos_individuales,
                            aes(x = clientes, y = ganancia_total, label = clientes),
                            color = "black", size = 3.0,
                            box.padding = 0.3, point.padding = 0.3,
                            min.segment.length = 0) +
    ggrepel::geom_text_repel(data = maximo_promedio,
                            aes(x = clientes, y = ganancia_total, label = clientes),
                            color = "black", size = 4.0, fontface = "bold",
                            box.padding = 0.5, point.padding = 0.5,
                            min.segment.length = 0) +
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(name = "Curvas y sus Máximos",
                      values = colores_plot,
                      labels = labels_plot) +
    labs(
      title = paste0("Curvas de Ganancia del Ensemble y su Promedio"),
      subtitle = paste0("Experimento: ", PARAM$experimento),
      x = "Clientes Contactados (Envíos)",
      y = "Ganancia Total"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.margin = margin(10, 10, 10, 10)) +
    guides(color = guide_legend(ncol = 2))

  ggsave(
    paste0(PARAM$carpeta_graficos, "curvas_ensemble_superpuestas_", PARAM$experimento, ".png"),
    plot = p,
    width = 12,
    height = 8
  )

  log_info("Gráfico de superposición de curvas del ensemble guardado correctamente.")
}

tryCatch({  
  #------------------------------------------------------
  # Sección 6: Entrenamiento y Predicción (Modelo Único)
  #------------------------------------------------------
  log_info("Iniciando Sección 6: Entrenamiento del Modelo Único.")
  dataset_train <- dataset[foto_mes %in% PARAM$train_final]
  dtrain_final <- lgb.Dataset(data = data.matrix(dataset_train[, campos_buenos, with = FALSE]), label = dataset_train[, clase01])
  
  param_final <- modifyList(PARAM$lgbm$param_fijos,
                            PARAM$out$lgbm$mejores_hiperparametros)
  param_normalizado <- copy(param_final)
  param_normalizado$min_data_in_leaf <- round(param_final$min_data_in_leaf / PARAM$trainingstrategy$undersampling)
  param_normalizado$early_stopping <- NULL
  param_normalizado$early_stopping_round <- NULL
  param_normalizado$early_stopping_rounds <- NULL
  
  modelo_final <- lgb.train(data = dtrain_final, param = param_normalizado)
  log_info("Entrenamiento del modelo final completado.")
  
  lgb.save(modelo_final, paste0(PARAM$carpeta_bayesiana, "modelo.txt"))
  tb_importancia <- as.data.table(lgb.importance(modelo_final))
  fwrite(
    tb_importancia,
    file = paste0(PARAM$carpeta_bayesiana, "importancia.txt"),
    sep = "\t"
  )
  
  log_info("Generando predicciones para el Modelo Único...")
  dfuture <- dataset[foto_mes %in% PARAM$future]
  prediccion_unica <- predict(modelo_final, data.matrix(dfuture[, campos_buenos, with = FALSE]))
  tb_prediccion_unico <- dfuture[, list(numero_de_cliente, foto_mes)]
  tb_prediccion_unico[, prob := prediccion_unica]
  
  drealidad <- realidad_inicializar(dfuture, PARAM)
  
  resultados_unico <- EvaluarYGraficar(
    tb_prediccion = tb_prediccion_unico,
    drealidad = drealidad,
    PARAM = PARAM,
    tipo_modelo = "unico",
    carpeta_salida_kaggle = PARAM$carpeta_kaggle
  )
  log_info(paste0("Envíos óptimos del modelo único: ",paste(resultados_unico$envios_optimos, collapse = ", ")))
  
  # Escribir resultados del modelo único en el log de resumen
  log_summary("--- Resultados del Modelo Único ---")
  log_summary(paste("Envíos con ganancia máxima:", paste(resultados_unico$envios_optimos, collapse = ", ")))
  log_summary(paste("Máxima ganancia:", format(resultados_unico$max_ganancia, big.mark = ".", decimal.mark = ",")))
  log_summary("-----------------------------------\n")

  log_info("Generando los archivos de entrega para el modelo único.")
  dataset_train_final <- dataset[foto_mes %in% PARAM$train_final_kaggle]
  dtrain_final_kaggle <- lgb.Dataset(data = data.matrix(dataset_train_final[, campos_buenos, with = FALSE]), 
                                    label = dataset_train_final[, clase01])

  modelo_final_kaggle <- lgb.train(data = dtrain_final_kaggle, param = param_normalizado)
  log_info("Modelo final para Kaggle re-entrenado con todos los datos.")

  dfuture_entrega <- dataset[foto_mes %in% PARAM$entrega_kaggle]
  prediccion_final <- predict(modelo_final_kaggle, data.matrix(dfuture_entrega[, campos_buenos, with = FALSE]))

  tb_prediccion_final <- dfuture_entrega[, list(numero_de_cliente, foto_mes)]
  tb_prediccion_final[, prob := prediccion_final]

  GenerarEnviosKaggle(
    tb_prediccion = tb_prediccion_final,
    envios_optimos = resultados_unico$envios_optimos,
    tipo_modelo = "final_unico",
    carpeta_salida = PARAM$carpeta_entregables,
    experimento_id = PARAM$experimento
  )

  #------------------------------------------------------
  # Sección 7: Entrenamiento y Predicción (Ensemble)
  #------------------------------------------------------
  log_info("Iniciando Sección 7: Entrenamiento del Ensamble de Modelos.")
  lista_predicciones <- list()
  envios_optimos_individuales <- c()
  lista_resultados_individuales <- list()

  for (semilla_actual in PARAM$semillas_ensemble) {
    log_info(paste0("Entrenando modelo del ensamble con semilla: ", semilla_actual))
    param_normalizado$seed <- semilla_actual

    modelo <- lgb.train(data = dtrain_final, param = param_normalizado)
    prediccion_individual <- predict(modelo, data.matrix(dfuture[, campos_buenos, with = FALSE]))

    tb_pred_individual <- dfuture[, list(numero_de_cliente, foto_mes)]
    tb_pred_individual[, prob := prediccion_individual]
    lista_predicciones[[as.character(semilla_actual)]] <- tb_pred_individual

    resultados_individual <- data.table()
    setorder(tb_pred_individual, -prob)
    
    for (envios in PARAM$cortes) {
      tb_pred_individual[, Predicted := 0L]
      tb_pred_individual[1:envios, Predicted := 1L]
      res_ind <- realidad_evaluar(drealidad, tb_pred_individual)
      resultados_individual <- rbind(
        resultados_individual,
        data.table(clientes = envios, ganancia_total = res_ind$total)
      )
    }
    
    lista_resultados_individuales[[as.character(semilla_actual)]] <- resultados_individual
    
    max_ganancia_ind <- max(resultados_individual$ganancia_total, na.rm = TRUE)
    envio_optimo_individual <- max(resultados_individual[ganancia_total == max_ganancia_ind, clientes])

    log_info(paste0("--> Envío óptimo (más alto) para semilla ", semilla_actual, ": ", envio_optimo_individual))
    envios_optimos_individuales <- c(envios_optimos_individuales, envio_optimo_individual)
  }

  GraficarCurvasEnsemble(lista_resultados_individuales, PARAM)
  
  envio_promedio <- mean(envios_optimos_individuales)
  envio_promedio_redondeado <- round(envio_promedio / 100) * 100
  log_info(paste0("Envíos óptimos individuales: ", paste(envios_optimos_individuales, collapse = ", ")))
  log_info(paste0("Promedio de envíos individuales (redondeado a 100): ", envio_promedio_redondeado))

  log_info("Creando el ensamble final promediando probabilidades.")
  predicciones_todas <- rbindlist(lista_predicciones)
  tb_prediccion_ensamble <- predicciones_todas[, .(prob = mean(prob)), by = .(numero_de_cliente, foto_mes)]

  resultados_ensamble <- EvaluarYGraficar(
    tb_prediccion = tb_prediccion_ensamble,
    drealidad = drealidad,
    PARAM = PARAM,
    tipo_modelo = "ensamble",
    carpeta_salida_kaggle = PARAM$carpeta_kaggle_ensamble
  )
  log_info(paste0("Envíos óptimos del ensamble promediado:", paste(resultados_ensamble$envios_optimos, collapse = ", ")))
  
  # NUEVO: Escribir resultados del modelo ensamble en el log de resumen
  log_summary("--- Resultados del Modelo Ensamble ---")
  log_summary(paste("Envíos con ganancia máxima:", paste(resultados_ensamble$envios_optimos, collapse = ", ")))
  log_summary(paste("Máxima ganancia:", format(resultados_ensamble$max_ganancia, big.mark = ".", decimal.mark = ",")))
  log_summary(paste("Promedio de envíos individuales (redondeado):", envio_promedio_redondeado))
  log_summary("--------------------------------------\n")


  log_info("Generando los archivos finales para entregar, para el ensamble.")
  dataset_train_final_ens <- dataset[foto_mes %in% PARAM$train_final_kaggle]
  dtrain_final_kaggle_ens <- lgb.Dataset(
    data = data.matrix(dataset_train_final_ens[, campos_buenos, with = FALSE]),
    label = dataset_train_final_ens[, clase01]
  )
  log_info("Dataset final de Kaggle para el ensamble preparado.")

  lista_predicciones_final <- list()
  dfuture_entrega <- dataset[foto_mes %in% PARAM$entrega_kaggle]

  for (semilla_actual in PARAM$semillas_ensemble) {
    log_info(paste0("Re-entrenando modelo final del ensamble con semilla: ", semilla_actual))
    param_normalizado$seed <- semilla_actual
    modelo_final_ens <- lgb.train(data = dtrain_final_kaggle_ens, param = param_normalizado)
    prediccion_final_individual <- predict(modelo_final_ens, data.matrix(dfuture_entrega[, campos_buenos, with = FALSE]))

    tb_pred_final_individual <- dfuture_entrega[, list(numero_de_cliente, foto_mes)]
    tb_pred_final_individual[, prob := prediccion_final_individual]
    lista_predicciones_final[[as.character(semilla_actual)]] <- tb_pred_final_individual
  }

  log_info("Creando el ensamble final para la entrega promediando probabilidades.")
  predicciones_finales_todas <- rbindlist(lista_predicciones_final)
  tb_prediccion_final_ensamble <- predicciones_finales_todas[, .(prob = mean(prob)), by = .(numero_de_cliente, foto_mes)]

  envios_finales_ensamble <- unique(c(
    resultados_ensamble$envios_optimos,
    envio_promedio_redondeado
  ))
  log_info(paste0("Envíos finales a generar para el ensamble: ", paste(sort(envios_finales_ensamble), collapse = ", ")))

  GenerarEnviosKaggle(
    tb_prediccion = tb_prediccion_final_ensamble,
    envios_optimos = envios_finales_ensamble,
    tipo_modelo = "final_ensamble",
    carpeta_salida = PARAM$carpeta_entregables,
    experimento_id = PARAM$experimento
  )

  log_info("Archivos de entrega para el ensamble generados correctamente.")

}, error = function(e) {
  log_error("######################################################")
  log_error("Se ha producido un error fatal en las Secciones 6 o 7.")
  log_error(paste("Mensaje de R:", e$message))
  log_error("######################################################")
  quit(status = 1)
})
#------------------------------------------------------
# Sección 8: Finalización
#------------------------------------------------------
log_info("------------------------------------------------------")
log_info("Script finalizado exitosamente.")
log_info(paste("Fecha y hora de finalización:", format(Sys.time(), "%a %b %d %X %Y")))
log_info("------------------------------------------------------")