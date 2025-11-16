#!/usr/bin/env Rscript
# limpio la memoria
rm(list=ls(all.names=TRUE)) # remove all objects
gc(full=TRUE, verbose=FALSE) # garbage collection

# Instalo y cargo las librerías que se usan en el workflow
if (!require("data.table")) install.packages("data.table")
if (!require("Rcpp")) install.packages("Rcpp")
if (!require("zlightgbm")) {
  install.packages("https://storage.googleapis.com/open-courses/dmeyf2025-e4a2/zlightgbm_4.6.0.99.tar.gz",
                   repos = NULL, type = "source")
}
if (!require("DiceKriging")) install.packages("DiceKriging")
if (!require("mlrMBO")) install.packages("mlrMBO")
if (!require("primes")) install.packages("primes")
if (!require("rlist")) install.packages("rlist")
if (!require("logger")) install.packages("logger")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ggrepel")) install.packages("ggrepel")
if (!require("scales")) install.packages("scales")

require("data.table")
require("Rcpp")
require("zlightgbm")
require("DiceKriging")
require("mlrMBO")
require("primes")
require("rlist")
require("logger")
require("ggplot2")
require("ggrepel")
require("scales")

# Guardo el directorio de origen
home_dir <- getwd()

# Defino los parámetros del workflow
PARAM <- list()

# Parámetros generales
PARAM$experimento <- "Modelo_22"
PARAM$semilla_primigenia <- 102191 # Semilla de zLineaMuerte

# Parámetro de Canaritos
PARAM$qcanaritos <- 5L

# Path a los datos de entrada
PARAM$generar_ternaria <- FALSE
PARAM$dir_dataset <- "~/buckets/b1/datasets"
#PARAM$dataset_name <- "competencia_02_crudo.csv.gz"
PARAM$dataset_name <- "competencia_02.csv.gz"
PARAM$input_dataset <- file.path(PARAM$dir_dataset, PARAM$dataset_name)

# Path a la carpeta de salida del experimento
PARAM$output_folder <- "~/buckets/b1/exp"
PARAM$experimento_folder <- file.path(PARAM$output_folder, PARAM$experimento)
dir.create(PARAM$experimento_folder, showWarnings=FALSE)

PARAM$carpeta_bayesiana <- "Bayesiana" # Aunque no la usemos, los scripts la referencian
PARAM$carpeta_evaluacion <- "Evaluacion"
PARAM$carpeta_graficos <- "Plots"
PARAM$carpeta_entregables <- "Entregables"
PARAM$modelos_folder <- "Modelos"

# Parámetros de Feature Engineering Histórico
# Lags
PARAM$FE_hist$lags$run <- TRUE # Activar o desactivar lags
PARAM$FE_hist$lags$n_lags <- c(1, 2, 3, 6, 12) # Número de lags a crear
PARAM$FE_hist$lags$aceleracion <- FALSE # Activar o desactivar aceleración (derivada segunda)
# Tendencias
PARAM$FE_hist$Tendencias$run <- TRUE # Activar o desactivar Tendencias
PARAM$FE_hist$Tendencias$ventana <- c(6)
PARAM$FE_hist$Tendencias$tendencia <- TRUE
PARAM$FE_hist$Tendencias$minimo <- FALSE
PARAM$FE_hist$Tendencias$maximo <- FALSE
PARAM$FE_hist$Tendencias$promedio <- FALSE
PARAM$FE_hist$Tendencias$ratioavg <- FALSE
PARAM$FE_hist$Tendencias$ratiomax <- FALSE
# Media Moviles
PARAM$FE_hist$MovingAverages$run <- FALSE # Activar o desactivar Moving Averages
PARAM$FE_hist$MovingAverages$windows <- c(3, 6) # Ventanas de moving averages
PARAM$FE_hist$MovingAverages$delta_change <- TRUE # Cambio respecto a periodo anterior (delta entre periodos)
PARAM$FE_hist$MovingAverages$vs_actual <- TRUE #Media móvil vs valor actual

# Parámetros de Feature Engineering con Random Forest
PARAM$FE_rf <- list()
# Los siguientes parámetros se deben modificar
PARAM$FE_rf$arbolitos <- 20
PARAM$FE_rf$hojas_por_arbol <- 16
PARAM$FE_rf$datos_por_hoja <- 100
PARAM$FE_rf$mtry_ratio <- 0.2
# Parámetros quasi fijos
PARAM$FE_rf$train$training <- c(202101, 202102, 202103)
PARAM$FE_rf$lgb_param <- list(
  num_iterations = PARAM$FE_rf$arbolitos,
  num_leaves = PARAM$FE_rf$hojas_por_arbol,
  min_data_in_leaf = PARAM$FE_rf$datos_por_hoja,
  feature_fraction_bynode = PARAM$FE_rf$mtry_ratio,
  boosting = "rf",
  bagging_fraction = (1.0 - 1.0 / exp(1.0)),
  bagging_freq = 1.0,
  feature_fraction = 1.0,
  max_bin = 31L,
  objective = "binary",
  first_metric_only = TRUE,
  boost_from_average = TRUE,
  feature_pre_filter = FALSE,
  force_row_wise = TRUE,
  verbosity = -100,
  max_depth = -1L,
  min_gain_to_split = 0.0,
  min_sum_hessian_in_leaf = 0.001,
  lambda_l1 = 0.0,
  lambda_l2 = 0.0,
  pos_bagging_fraction = 1.0,
  neg_bagging_fraction = 1.0,
  is_unbalance = FALSE,
  scale_pos_weight = 1.0,
  drop_rate = 0.1,
  max_drop = 50,
  skip_drop = 0.5,
  extra_trees = FALSE,
  canaritos = 0, # Me aseguro que es un LGBM común
  gradient_bound = 0 # Me aseguro que es un LGBM común
)

# Parámetros de Reducción de Dimensionalidad (Canaritos Asesinos)
PARAM$reduccion_canaritos <- list()
PARAM$reduccion_canaritos$train <- list()

# Parámetros principales: ratio de canaritos y desvíos para el corte
PARAM$reduccion_canaritos$ratio <- 0.2
PARAM$reduccion_canaritos$desvios <- 2

# Lista de campos que NO deben usarse para entrenar (identificadores, target, etc.)
PARAM$reduccion_canaritos$campitos_no_entrenar <- c( "numero_de_cliente", "foto_mes", "clase_ternaria" )

# Parámetros de entrenamiento del modelo de canaritos
PARAM$reduccion_canaritos$train$clase01_valor1 <- c( "BAJA+2", "BAJA+1")
PARAM$reduccion_canaritos$train$positivos <- c( "BAJA+2")
PARAM$reduccion_canaritos$train$training <- c( 202101, 202102, 202103)
PARAM$reduccion_canaritos$train$validation <- c( 202105 )
PARAM$reduccion_canaritos$train$undersampling <- 0.1
PARAM$reduccion_canaritos$train$gan1 <- 780000
PARAM$reduccion_canaritos$train$gan0 <- -20000

# Parámetros de Training Strategy (para Script 11 - Evaluación)
PARAM$trainingstrategy <- list()
PARAM$train_final$training <- c(
  201901, 201902, 201903, 201904, 201906,
  201907, 201908, 201909, 201911, 201912,
  202001, 202002, 202003, 202004, 202005,
  202007, 202008, 202009, 202010, 202011, 202012,
  202101, 202102, 202103, 202104, 202105, 202106
)
PARAM$trainingstrategy$testing <- c(202108) # Mes para script 11
PARAM$trainingstrategy$undersampling <- 0.05
PARAM$trainingstrategy$positivos <- c("BAJA+1", "BAJA+2")
PARAM$trainingstrategy$campos_entrenar <- c("clase_ternaria", "clase01", "azar", "training")
PARAM$trainingstrategy$importancias <- 50

# Parámetros fijos de zLightGBM
PARAM$lgbm_z <- list(
  boosting= "gbdt",
  objective= "binary",
  metric= "custom",
  first_metric_only= FALSE,
  boost_from_average= TRUE,
  feature_pre_filter= FALSE,
  force_row_wise= TRUE,
  verbosity= -100,

  seed= PARAM$semilla_primigenia,

  max_bin= 31L,
  min_data_in_leaf= 20L,  #este ya es el valor default de LightGBM

  num_iterations= 9999L, # dejo libre la cantidad de arboles, zLightGBM se detiene solo
  num_leaves= 9999L, # dejo libre la cantidad de hojas, zLightGBM sabe cuando no hacer un split
  learning_rate= 1.0,  # se lo deja en 1.0 para que si el score esta por debajo de gradient_bound no se lo escale
    
  feature_fraction= 0.50, # un valor equilibrado, habra que probar alternativas ...
    
  canaritos= PARAM$qcanaritos, # fundamental en zLightGBM, aqui esta el control del overfitting
  gradient_bound= 0.1   # default de zLightGBM
)

# Parámetros para evaluación (Script 10)
PARAM$eval_ensamble <- list()
PARAM$eval_ensamble$APO <- TRUE # Realizo la comparativa con APO
PARAM$eval_ensamble$iter <- 5
PARAM$eval_ensamble$ksemillerio <- 1 # Se multiplica por iter
PARAM$eval_ensamble$cortes_evaluacion <- c(8000, 8500, 9000, 9500, 10000, 10500, 11000, 11500, 12000)

# Parámetros para el entrenamiento final y predicción (Script 12)
PARAM$train_final <- list()
PARAM$train_final$produccion <- TRUE # Se activa para generar un archivo final con clase desconocida
PARAM$train_final$envios_a_generar <- c(10500, 11000) # Se debe obtener a partir del análisis previo
PARAM$train_final$future <- c(202108) # Mes para predecir (ej: 202108)
PARAM$train_final$training <- c(
  201901, 201902, 201903, 201904, 201906,
  201907, 201908, 201909, 201911, 201912,
  202001, 202002, 202003, 202004, 202005,
  202007, 202008, 202009, 202010, 202011, 202012,
  202101, 202102, 202103, 202104, 202105, 202106
)
PARAM$train_final$undersampling <- 0.05 # Undersampling
PARAM$train_final$ksemillerio <- 5 # Semillerio para modelo final

#------------------------------------------------------------------------------
# Función wrapper para ejecutar y cronometrar scripts
#------------------------------------------------------------------------------
source_con_log <- function(script_path, script_name) {
  
  log_info(paste("--- Iniciando:", script_name, "---"))
  t_inicio <- Sys.time()
  
  tryCatch({
    source(script_path, local = FALSE) 
    
    t_fin <- Sys.time()
    duracion <- round(as.numeric(difftime(t_fin, t_inicio, units = "secs")), 2)
    
    log_info(paste("--- Fin:", script_name, ". Duración:", duracion, "segundos. ---"))
    
  }, error = function(e) {
    t_fin <- Sys.time()
    duracion <- round(as.numeric(difftime(t_fin, t_inicio, units = "secs")), 2)
    log_error(paste("--- ERROR en:", script_name, "tras", duracion, "segundos. ---"))
    log_error(paste("Mensaje de R:", e$message))
    
    stop("Error en el script: ", script_name, ". Deteniendo el workflow.") 
  })
  
  log_info("--------------------------------------------------") # Separador
}

#------------------------------------------------------------------------------
# INICIO DEL WORKFLOW
#------------------------------------------------------------------------------
dir.create(PARAM$experimento_folder, showWarnings = FALSE)

log_file <- file.path(PARAM$experimento_folder, paste0("log_", PARAM$experimento, ".txt"))
log_appender(appender_tee(log_file))
log_info(paste("La salida del experimento se guardará en:", PARAM$experimento_folder))

log_info("Inciando el workflow")
log_info("==================================================")
# Ejecuto los scripts del workflow usando el wrapper
source_con_log(file.path(home_dir, "1_Preprocesamiento.R"), "1_Preprocesamiento.R")
source_con_log(file.path(home_dir, "2_Eliminacion_de_Features_7.R"), "2_Eliminacion_de_Features_7")
source_con_log(file.path(home_dir, "3_Data_Quality.R"), "3_Data_Quality.R")
source_con_log(file.path(home_dir, "4_Feature_Engineering_Intra_Mes_20.R"), "4_Feature_Engineering_Intra_Mes.R")
source_con_log(file.path(home_dir, "5_Data_Drifting.R"), "5_Data_Drifting.R")
source_con_log(file.path(home_dir, "6_Feature_Engineering_Historico.R"), "6_Feature_Engineering_Historico.R")
source_con_log(file.path(home_dir, "7_Feature_Engineering_RF.R"), "7_Feature_Engineering_RF.R")
#source_con_log(file.path(home_dir, "8_Reduccion_Dimensionalidad_Canaritos.R"), "8_Reduccion_Dimensionalidad_Canaritos.R")
#source_con_log(file.path(home_dir, "9_Modelado.R"), "9_Modelado.R")
#source_con_log(file.path(home_dir, "10_Evaluacion_APO.R"), "10_Evaluacion_APO.R")
source_con_log(file.path(home_dir, "11_Modelo_Final.R"), "12_Modelo_Final.R")
log_info("==================================================")
log_info("Workflow finalizado")