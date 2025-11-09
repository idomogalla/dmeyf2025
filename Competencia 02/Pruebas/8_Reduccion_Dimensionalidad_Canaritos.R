log_info("Inicio 8_Reduccion_Dimensionalidad_Canaritos.R")

# Variables globales para la métrica y el conteo
VPOS_CORTE <- c()
GVEZ <- 1 # Variable global para contar ejecuciones

# Métrica de ganancia personalizada
fganancia_lgbm_meseta <- function(probs, datos) {
  vlabels <- get_field(datos, "label")
  vpesos <- get_field(datos, "weight")

  tbl <- as.data.table(list(
    "prob" = probs,
    # Utiliza los parámetros de ganancia definidos en main.R
    "gan" = ifelse(vlabels == 1 & vpesos > 1,
                   PARAM$reduccion_canaritos$train$gan1,
                   PARAM$reduccion_canaritos$train$gan0)
  ))

  setorder(tbl, -prob)
  tbl[, posicion := .I]
  tbl[, gan_acum := cumsum(gan)]
  setorder(tbl, -gan_acum) # voy por la meseta

  gan <- mean(tbl[1:500, gan_acum]) # meseta de tamaño 500

  pos_meseta <- tbl[1:500, median(posicion)]
  VPOS_CORTE <<- c(VPOS_CORTE, pos_meseta)

  return(list(
    "name" = "ganancia",
    "value" = gan,
    "higher_better" = TRUE
  ))
}


# Función principal de Canaritos Asesinos
CanaritosAsesinos <- function(
  canaritos_ratio,
  canaritos_desvios,
  canaritos_semilla) {

  log_info("Inicio CanaritosAsesinos().")
  gc(verbose= FALSE)

  # Definir la clase01 usando parámetros de main.R
  log_info("Creando variable 'clase01' para el entrenamiento.")
  dataset[, clase01 := 0L ]
  dataset[ clase_ternaria %in% PARAM$reduccion_canaritos$train$clase01_valor1,
      clase01 := 1L ]

  # Calcular cantidad de canaritos
  qty_canaritos <- as.integer(ncol(dataset) * canaritos_ratio)

  # Validar qty_canaritos (para evitar el error 'argument of length 0')
  if (length(qty_canaritos) == 0 || is.na(qty_canaritos) || qty_canaritos <= 0) {
    log_info("Creando 0 canaritos (ratio=0 o parámetro no definido).")
    qty_canaritos <- 0 # Asegurar que sea 0 y no NULL o NA
  } else {
    log_info(paste("Creando", qty_canaritos, "canaritos."))
  }

  # El loop ahora solo se ejecuta SI qty_canaritos > 0
  if (qty_canaritos > 0) {
    set.seed(canaritos_semilla, kind = "L'Ecuyer-CMRG")
    for (i in seq_len(qty_canaritos)) {
      dataset[, paste0("canarito", i) := runif(nrow(dataset))]
    }
  }

  # Campos buenos (features) para entrenar
  campos_buenos <- setdiff(
    colnames(dataset),
    c( PARAM$reduccion_canaritos$campitos_no_entrenar, "clase01")
  )
  
  log_info(paste("El modelo de canaritos se entrenará con", length(campos_buenos), "variables (reales + canaritos)."))

  # Definir set de entrenamiento con undersampling (usando params de main.R)
  log_info("Definiendo set de entrenamiento (training) y validación (validation).")
  azar <- runif(nrow(dataset))
  dataset[, entrenamiento :=
    as.integer( foto_mes %in% PARAM$reduccion_canaritos$train$training &
      (clase01 == 1 | azar < PARAM$reduccion_canaritos$train$undersampling))]

  # Crear dtrain
  dtrain <- lgb.Dataset(
    data = data.matrix(dataset[entrenamiento == TRUE, campos_buenos, with = FALSE]),
    label = dataset[entrenamiento == TRUE, clase01],
    weight = dataset[
      entrenamiento == TRUE,
      ifelse(clase_ternaria %in% PARAM$reduccion_canaritos$train$positivos, 1.000001, 1.0)
    ],
    free_raw_data = FALSE
  )

  # Crear dvalid
  dvalid <- lgb.Dataset(
    data = data.matrix(dataset[foto_mes %in% PARAM$reduccion_canaritos$train$validation, campos_buenos, with = FALSE]),
    label = dataset[foto_mes %in% PARAM$reduccion_canaritos$train$validation, clase01],
    weight = dataset[
      foto_mes %in% PARAM$reduccion_canaritos$train$validation,
      ifelse( clase_ternaria %in% PARAM$reduccion_canaritos$train$positivos, 1.000001, 1.0)
    ],
    free_raw_data = FALSE
  )

  rm(azar)
  gc(verbose= FALSE)

  # Parámetros INTERNOS de LightGBM (se quedan en el script)
  param <- list(
    objective = "binary",
    metric = "custom",
    first_metric_only = TRUE,
    boost_from_average = TRUE,
    feature_pre_filter = FALSE,
    verbosity = -100,
    seed = canaritos_semilla, # Usa la semilla pasada como argumento
    max_depth = -1,
    min_gain_to_split = 0.0,
    lambda_l1 = 0.0,
    lambda_l2 = 0.0,
    max_bin = 31,
    num_iterations = 9999, # un numero grande, lo limita early_stopping_rounds
    force_row_wise = TRUE,
    learning_rate = 0.065,
    feature_fraction = 1.0,
    min_data_in_leaf = 260,
    num_leaves = 60,
    early_stopping_rounds = 200,
    num_threads = 1
  )

  # Entrenar el modelo
  log_info("Iniciando entrenamiento del modelo LightGBM para canaritos.")
  set.seed(canaritos_semilla, kind = "L'Ecuyer-CMRG")
  modelo <- lgb.train(
    data = dtrain,
    valids = list(valid = dvalid),
    eval = fganancia_lgbm_meseta,
    param = param, # Usa los parámetros definidos localmente
    verbose = -100
  )
  
  log_info(paste("Modelo entrenado. Mejor iteración:", modelo$best_iter))

  # Calcular importancia
  log_info("Calculando importancia de variables (reales y canaritos).")
  tb_importancia <- lgb.importance(model = modelo)
  tb_importancia[, pos := .I]

  # Guardar la tabla de importancia (usando la ruta del experimento)
  impo_file_path <- file.path(PARAM$experimento_folder, paste0("impo_", GVEZ, ".txt"))
  fwrite(tb_importancia,
    file = impo_file_path,
    sep = "\t"
  )
  log_info(paste("Tabla de importancia intermedia guardada en:", impo_file_path))
  GVEZ <<- GVEZ + 1

  # Calcular umbral de corte
  log_info("Calculando umbral de corte basado en la mediana y desvíos de los canaritos.")
  umbral <- tb_importancia[
    Feature %like% "canarito",
    median(pos) + canaritos_desvios * sd(pos) # Usa desvíos pasado como argumento
  ]
  log_info(paste("El umbral de corte (posición) es:", round(umbral, 2)))

  # Seleccionar columnas útiles
  col_utiles <- tb_importancia[
    pos < umbral & !(Feature %like% "canarito"),
    Feature
  ]
  
  col_originales <- length(campos_buenos) - qty_canaritos
  log_info(paste("Seleccionadas", length(col_utiles), "variables útiles de",
                 col_originales, "variables originales (excluyendo canaritos)."))

  # Asegurarse de no borrar campos clave
  col_utiles <- unique(c(
    col_utiles,
    c(PARAM$reduccion_canaritos$campitos_no_entrenar, "mes") # 'mes' se asume que existe
  ))

  # Identificar y eliminar columnas inútiles
  col_inutiles <- setdiff(colnames(dataset), col_utiles)
  
  if (length(col_inutiles) > 0) {
    log_info(paste("Eliminando", length(col_inutiles), "variables (incluyendo canaritos y variables débiles)."))
    dataset[, (col_inutiles) := NULL]
  } else {
    log_info("No se encontraron variables para eliminar.")
  }
 
  gc(verbose= FALSE)

  log_info("Fin CanaritosAsesinos().")
  return(tb_importancia)
}

# Llamada a la función usando los parámetros centralizados en main.R
log_info("Iniciando la ejecución de CanaritosAsesinos.")
tb_importancia_final <- CanaritosAsesinos(
  canaritos_ratio = PARAM$reduccion_canaritos$ratio,
  canaritos_desvios = PARAM$reduccion_canaritos$desvios,
  canaritos_semilla = PARAM$semilla_primigenia # Usa la semilla global
)

# Guardar la tabla de importancia final en la carpeta del experimento
canaritos_file_path <- file.path(PARAM$experimento_folder, "canaritos.txt")
fwrite( tb_importancia_final,
  file = canaritos_file_path,
  sep = "\t"
)
log_info(paste("Tabla de importancia final guardada en:", canaritos_file_path))

# Verificación final
log_info(paste("El dataset final ahora tiene", ncol(dataset), "columnas."))
log_info("Columnas finales (primeras 100):")
log_info(paste(head(colnames(dataset), 100), collapse = ", "))

# Limpiar variables del script
rm(tb_importancia_final, fganancia_lgbm_meseta, CanaritosAsesinos)
gc(verbose= FALSE)

log_info("Fin 8_Reduccion_Dimensionalidad_Canaritos.R")