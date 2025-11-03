#------------------------------------------------------------------------------
# 1. Initialization
#------------------------------------------------------------------------------

# Clear memory
rm(list = ls(all.names = TRUE))
gc(full = TRUE, verbose = FALSE)

# Load libraries
if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("data.table package is not installed.")
}
library(data.table)

# Load zlightgbm
if (!requireNamespace("zlightgbm", quietly = TRUE)) {
    message("Installing zlightgbm...") # 'message' es apropiado aquí
    install.packages("https://storage.googleapis.com/open-courses/dmeyf2025-e4a2/zlightgbm_4.6.0.99.tar.gz",
                   repos = NULL, type = "source")
}
library(zlightgbm)

# Custom log function
log_message <- function(message) {
  cat(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), message, "\n")
}

log_message("Initialization and libraries loaded.")

#------------------------------------------------------------------------------
# 1.5. Auxiliary Functions (Integradas)
#------------------------------------------------------------------------------

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
    division = c(3, 7), # Asumiendo 30% public, 70% private
    agrupa = "clase_ternaria",
    seed = pparam$semilla_kaggle
  )
  return(drealidad)
}

realidad_evaluar <- function(prealidad, pprediccion) {
  # Asegurarse de que 'predicted' no exista antes del join
  if ("predicted" %in% colnames(prealidad)) {
    prealidad[, predicted := NULL]
  }
  
  prealidad[pprediccion, on = c("numero_de_cliente", "foto_mes"), predicted := i.Predicted]
  
  # Manejar NAs en predicted (clientes no presentes en pprediccion, aunque no debería pasar)
  prealidad[is.na(predicted), predicted := 0L]
  
  tbl <- prealidad[, list("qty" = .N), list(fold, predicted, clase_ternaria)]
  
  res <- list()
  
  # Ganancia (780k si BAJA+2, -20k si CONTINUA o BAJA+1)
  ganancia_baja2 <- 780000
  costo_error <- -20000
  
  res$public <- tbl[fold == 1 &
                      predicted == 1L, sum(qty * ifelse(clase_ternaria == "BAJA+2", ganancia_baja2, costo_error))] / 0.3
  
  res$private <- tbl[fold == 2 &
                       predicted == 1L, sum(qty * ifelse(clase_ternaria == "BAJA+2", ganancia_baja2, costo_error))] / 0.7
  
  res$total <- tbl[predicted == 1L, sum(qty * ifelse(clase_ternaria == "BAJA+2", ganancia_baja2, costo_error))]
  
  # Limpiar la columna 'predicted' de prealidad para la prox iteración
  prealidad[, predicted := NULL]
  
  return(res)
}

log_message("Auxiliary functions (particionar, realidad_inicializar, realidad_evaluar) defined.")

#------------------------------------------------------------------------------
# 2. Configuration
#------------------------------------------------------------------------------

PARAM <- list()
PARAM$experimento <- "exp_zlgbm_1"
PARAM$semilla_primigenia <- 974411

# Paths
PARAM$path$root <- "~/buckets/b1"
PARAM$path$exp <- file.path(PARAM$path$root, "exp", PARAM$experimento)
PARAM$path$datasets <- file.path(PARAM$path$root, "datasets")
PARAM$path$dataset_crudo <- file.path(PARAM$path$datasets, "competencia_02.csv.gz")
PARAM$path$gridsearch_log <- file.path(PARAM$path$exp, "gridsearch_results_v2.csv")


# Definición de Train/Validation y Evaluación
PARAM$train$meses <- c(202101, 202102, 202103, 202104)
PARAM$train$undersampling <- 0.50
PARAM$validation$meses <- c(202106) # Testeamos contra 202104
PARAM$eval$cortes <- seq(8000, 13000, by = 100) # Rango de envíos para buscar el óptimo

# Grid de Hiperparámetros (Actualizado)
PARAM$gridsearch <- list(
  canaritos = c(50, 100, 150),
  gradient_bound = c(0.05, 0.1, 0.25, 0.5),
  min_data_in_leaf = c(10, 20, 30, 50, 100)
)


# LGBM parameters (Base)
PARAM$lgbm_base <- list(
  boosting = "gbdt",
  objective = "binary",
  metric = "custom",
  first_metric_only = FALSE,
  boost_from_average = TRUE,
  feature_pre_filter = FALSE,
  force_row_wise = TRUE,
  verbosity = -100,
  seed = PARAM$semilla_primigenia,
  max_bin = 31L,
  num_iterations = 9999L,
  num_leaves = 999L,
  learning_rate = 1.0,
  feature_fraction = 0.50
  # canaritos, gradient_bound, min_data_in_leaf se añadirán desde el grid
)

# Create experiment folder
dir.create(PARAM$path$exp, showWarnings = FALSE, recursive = TRUE)

log_message(paste("Configuration loaded. Experiment:", PARAM$experimento))
log_message(paste("Grid search log will be saved to:", PARAM$path$gridsearch_log))

#------------------------------------------------------------------------------
# 3. Preprocessing (Se hace UNA SOLA VEZ)
#------------------------------------------------------------------------------

log_message("Starting Preprocessing...")

# Load dataset
log_message(paste("Loading dataset from:", PARAM$path$dataset_crudo))
dataset <- fread(PARAM$path$dataset_crudo)
log_message("Dataset loaded.")

# Feature Elimination
log_message("Eliminating features: 'mprestamos_personales' and 'cprestamos_personales'.")
dataset[, mprestamos_personales := NULL]
dataset[, cprestamos_personales := NULL]
log_message("Features eliminated.")


# Historical Feature Engineering
log_message("Starting historical feature engineering...")
setorder(dataset, numero_de_cliente, foto_mes)
cols_lagueables <- copy(setdiff(
  colnames(dataset),
  c("numero_de_cliente", "foto_mes", "clase_ternaria")
))
# Lags
log_message("Creating lags (order 1 and 2)...")
for (lag_order in 1:2) {
  new_cols <- paste0(cols_lagueables, "_lag", lag_order)
  dataset[, (new_cols) := shift(.SD, n = lag_order, fill = NA, type = "lag"),
          by = numero_de_cliente, .SDcols = cols_lagueables]
}
# Delta Lags
log_message("Creating delta lags (order 1 and 2)...")
for (vcol in cols_lagueables) {
  dataset[, paste0(vcol, "_delta1") := get(vcol) - get(paste0(vcol, "_lag1"))]
  dataset[, paste0(vcol, "_delta2") := get(vcol) - get(paste0(vcol, "_lag2"))]
}
log_message("Historical feature engineering finished.")
log_message("Preprocessing finished.")


#------------------------------------------------------------------------------
# 4. Grid Search & Validation
#------------------------------------------------------------------------------

log_message("Starting Grid Search...")

# Preparar el grid
grid <- expand.grid(PARAM$gridsearch, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
log_message(paste("Total iterations in grid search:", nrow(grid)))

# Preparar drealidad (dataset de validación) UNA SOLA VEZ
log_message(paste("Preparing validation reality data for month:", PARAM$validation$meses))
dataset_validation_base <- dataset[foto_mes %in% PARAM$validation$meses]
drealidad_base <- realidad_inicializar(
  dataset_validation_base,
  pparam = list(semilla_kaggle = PARAM$semilla_primigenia)
)
log_message("Validation reality data prepared.")


# Inicializar archivo de log del Grid Search
log_file_path <- PARAM$path$gridsearch_log
if (file.exists(log_file_path)) {
  log_message(paste("Removing old log file:", log_file_path))
  file.remove(log_file_path)
}
# Escribir el header
header <- data.table(
  iter = integer(),
  canaritos = integer(),
  gradient_bound = numeric(),
  min_data_in_leaf = integer(),
  max_ganancia = numeric(),
  envios_optimos = character()
)
fwrite(header, file = log_file_path, append = FALSE, col.names = TRUE)
log_message(paste("Grid search log initialized at:", log_file_path))


# Bucle principal del Grid Search
for (i in 1:nrow(grid)) {
  
  # --- 4.1. Configuración de la Iteración ---
  params_iter <- grid[i, ]
  p_canaritos <- params_iter$canaritos
  p_gradient_bound <- params_iter$gradient_bound
  p_min_data_in_leaf <- params_iter$min_data_in_leaf
  
  log_message(paste0(
    "--- Iteration ", i, "/", nrow(grid), ": ",
    "canaritos=", p_canaritos, ", ",
    "gradient_bound=", p_gradient_bound, ", ",
    "min_data_in_leaf=", p_min_data_in_leaf, " ---"
  ))
  
  
  # --- 4.2. Preparar Datos de Entrenamiento (con canaritos dinámicos) ---
  log_message("Preparing training data...")
  
  # Copiamos para no modificar el 'dataset' original
  dataset_train <- dataset[foto_mes %in% PARAM$train$meses, .SD]
  
  # (Undersampling)
  set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")
  dataset_train[, azar := runif(.N)]
  dataset_train[, training := 0L]
  dataset_train[
    (azar <= PARAM$train$undersampling | clase_ternaria %in% c("BAJA+1", "BAJA+2")),
    training := 1L
  ]
  dataset_train[, azar := NULL]
  
  # (Target Engineering)
  dataset_train[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]
  
  # (Canaritos Dinámicos)
  cols_canaritos_new <- character(0)
  if (p_canaritos > 0) {
    log_message(paste("Adding", p_canaritos, "canaritos to training data..."))
    filas_train <- nrow(dataset_train)
    for (j in seq_len(p_canaritos)) {
      col_name <- paste0("canarito_", j)
      dataset_train[, (col_name) := runif(filas_train)]
      cols_canaritos_new <- c(cols_canaritos_new, col_name)
    }
  }
  
  # (Preparar dtrain para LightGBM)
  campos_buenos <- setdiff(
    colnames(dataset_train),
    c("clase_ternaria", "clase01", "training")
  )
  
  dtrain <- lgb.Dataset(
    data = data.matrix(dataset_train[training == 1L, campos_buenos, with = FALSE]),
    label = dataset_train[training == 1L, clase01],
    free_raw_data = FALSE
  )
  log_message("dtrain created.")
  
  
  # --- 4.3. Entrenar Modelo ---
  log_message("Training LightGBM model...")
  
  # Configurar parámetros de LGBM para esta iteración
  lgbm_params_iter <- PARAM$lgbm_base
  lgbm_params_iter$canaritos <- p_canaritos
  lgbm_params_iter$gradient_bound <- p_gradient_bound
  lgbm_params_iter$min_data_in_leaf <- p_min_data_in_leaf
  
  modelo <- lgb.train(
    data = dtrain,
    param = lgbm_params_iter
  )
  log_message("Model trained.")
  
  
  # --- 4.4. Preparar Datos de Validación (con canaritos dinámicos) ---
  log_message("Preparing validation data...")
  
  # Copiamos para no modificar 'dataset_validation_base'
  dataset_validation <- dataset_validation_base[, .SD]
  
  # (Añadir Canaritos) - Deben ser los mismos que en train
  if (p_canaritos > 0) {
    log_message(paste("Adding", p_canaritos, "canaritos to validation data..."))
    filas_val <- nrow(dataset_validation)
    for (col_name in cols_canaritos_new) {
      dataset_validation[, (col_name) := runif(filas_val)]
    }
  }
  
  
  # --- 4.5. Predecir ---
  log_message("Generating predictions on validation set...")
  prediccion <- predict(
    modelo,
    data.matrix(dataset_validation[, campos_buenos, with = FALSE])
  )
  
  # Crear tabla de predicción
  tb_prediccion <- dataset_validation[, list(numero_de_cliente, foto_mes)]
  tb_prediccion[, prob := prediccion]
  log_message("Predictions generated.")
  
  
  # --- 4.6. Evaluar Ganancia (Lógica de funciones_auxiliares.R) ---
  log_message("Evaluating gain...")
  
  # Copiamos drealidad_base para que 'realidad_evaluar' pueda modificarla
  drealidad_iter <- copy(drealidad_base)
  
  resultados_iter <- data.table()
  setorder(tb_prediccion, -prob)
  
  # Bucle sobre los cortes de envío para encontrar el óptimo
  for (envios in PARAM$eval$cortes) {
    tb_prediccion[, Predicted := 0L]
    tb_prediccion[1:envios, Predicted := 1L]
    
    # Usamos la función interna
    res <- realidad_evaluar(drealidad_iter, tb_prediccion)
    
    resultados_iter <- rbind(
      resultados_iter,
      data.table(
        clientes = envios,
        ganancia_total = res$total # Solo nos interesa la ganancia total
      )
    )
  }
  
  # Encontrar la ganancia máxima y los envíos óptimos
  max_ganancia_iter <- max(resultados_iter$ganancia_total, na.rm = TRUE)
  envios_optimos_iter <- resultados_iter[ganancia_total == max_ganancia_iter, clientes]
  envios_optimos_str <- paste(sort(unique(envios_optimos_iter)), collapse = ", ")
  
  log_message(paste(
    "Iteration", i, "Result: Max Gain=", 
    format(max_ganancia_iter, big.mark=","), 
    "at Envios=", envios_optimos_str
  ))
  
  
  # --- 4.7. Registrar Resultados ---
  log_entry <- data.table(
    iter = i,
    canaritos = p_canaritos,
    gradient_bound = p_gradient_bound,
    min_data_in_leaf = p_min_data_in_leaf,
    max_ganancia = max_ganancia_iter,
    envios_optimos = envios_optimos_str
  )
  
  # Escribir al log (append)
  fwrite(log_entry, file = log_file_path, append = TRUE, col.names = FALSE)
  
  
  # --- 4.8. Limpieza de la iteración ---
  log_message("Cleaning up iteration...")
  rm(modelo, dtrain, dataset_train, dataset_validation, tb_prediccion, resultados_iter, drealidad_iter)
  gc(full = TRUE, verbose = FALSE)
  
} # --- Fin del bucle Grid Search ---


#------------------------------------------------------------------------------
# 5. Finalización
#------------------------------------------------------------------------------

log_message("Grid Search finished.")
log_message(paste("Final results are saved in:", log_file_path))

# Mostrar resultados finales en consola
try({
  final_results <- fread(log_file_path)
  log_message("--- Final Grid Search Summary (ordered by max_ganancia) ---")
  setorder(final_results, -max_ganancia)
  # 'print' es la forma correcta de mostrar un data.table en la consola
  print(final_results)
})

log_message("Script finished.")