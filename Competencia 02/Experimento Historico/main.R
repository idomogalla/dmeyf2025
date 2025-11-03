# limpio la memoria
rm(list=ls(all.names=TRUE)) # remove all objects
gc(full=TRUE, verbose=FALSE) # garbage collection

# Instalo y cargo las librerías que se usan en el workflow
if (!require("data.table")) install.packages("data.table")
if (!require("Rcpp")) install.packages("Rcpp")
if (!require("lightgbm")) install.packages("lightgbm")
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
require("lightgbm")
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
PARAM$experimento <- "colaborativo_001"
PARAM$semilla_primigenia <- 200003

# Path a los datos de entrada
PARAM$dir_dataset <- "~/buckets/b1/datasets/"
PARAM$dataset_name <- "competencia_02_crudo.csv.gz"
#PARAM$dataset_name <- "competencia_02.csv.gz"
PARAM$input_dataset <- paste0(PARAM$dir_dataset, PARAM$dataset_name)
PARAM$generar_ternaria <- TRUE

# Path a la carpeta de salida del experimento
# La carpeta se crea relativo al script main.R
PARAM$output_folder <- "~/buckets/b1/exp/"
PARAM$experimento_folder <- file.path(PARAM$output_folder, PARAM$experimento)
dir.create(PARAM$experimento_folder, showWarnings=FALSE)
setwd(PARAM$experimento_folder)

PARAM$carpeta_graficos <- "Plots/"
PARAM$carpeta_entregables <- "Entregables"

# Parámetros de Feature Engineering Histórico
PARAM$FE_hist <- list()
# Lags
PARAM$FE_hist$lags$run <- TRUE # Activar o desactivar lags
PARAM$FE_hist$lags$n_lags <- c(1) # Número de lags a crear
# Tendencias
PARAM$FE_hist$Tendencias$run <- FALSE # Activar o desactivar Tendencias
PARAM$FE_hist$Tendencias$ventana <- 6
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
  extra_trees = FALSE
)

# Parámetros de Training Strategy para la Optimización Bayesiana
PARAM$trainingstrategy <- list()
PARAM$trainingstrategy$training <- c(
  201901, 201902, 201903, 201904, 201905, 201906,
  201907, 201908, 201909, 201910, 201911, 201912,
  202001, 202002, 202003, 202004, 202005, 202006,
  202007, 202008, 202009, 202010, 202011, 202012,
  202101, 202102, 202103, 202104
)
PARAM$trainingstrategy$testing <- c(202106)
PARAM$trainingstrategy$undersampling <- 0.05
PARAM$trainingstrategy$positivos <- c("BAJA+1", "BAJA+2")
PARAM$trainingstrategy$campos_entrenar <- c("clase_ternaria","clase01","azar")
PARAM$trainingstrategy$importancias <- 50

# Parámetros de Optimización de Hiperparámetros (Bayesian Optimization)
PARAM$hipeparametertuning <- list()
PARAM$hipeparametertuning$BO_iteraciones <- 30 # 50 seria mas razonable

# El parámetro ksemillerio indica se se hace semillerio DENTRO de la bayesiana
# 1 no se hace Ensemble Semillerio, apenas se corre un solo LightGBM
# mayor a 1, se hace un  k-Ensemble Semillerio
PARAM$hipeparametertuning$ksemillerio <- 1L
# El parámetro repe indica si dentro de la bayesiana se toman varias medidas y luego se promedian
# Esto se hace ya sea que se llama a un solo LightGBM o se hace un Ensemble Semillerio de LightGBMs
# Tener en cuenta que repe multiplica linealmente el tiempo de corrida de la Bayesian Optimization
PARAM$hipeparametertuning$repe <- 1L

# Parámetros fijos de LightGBM para la BO
PARAM$lgbm <- list()
PARAM$lgbm$param_fijos <- list(
  objective = "binary",
  metric = "custom",
  first_metric_only = TRUE,
  boost_from_average = TRUE,
  feature_pre_filter = FALSE,
  verbosity = -100,
  force_row_wise = TRUE,
  seed = PARAM$semilla_primigenia,
  extra_trees = FALSE,
  max_depth = -1L,
  min_gain_to_split = 0.0,
  min_sum_hessian_in_leaf = 0.001,
  lambda_l1 = 0.0,
  lambda_l2 = 0.0,
  bagging_fraction = 1.0,
  pos_bagging_fraction = 1.0,
  neg_bagging_fraction = 1.0,
  is_unbalance = FALSE,
  scale_pos_weight = 1.0,
  drop_rate = 0.1,
  max_drop = 50,
  skip_drop = 0.5,
  max_bin = 31
)

PARAM$eval_ensamble <- list()


# Parámetros para el entrenamiento final y predicción
PARAM$train_final <- list()
PARAM$train_final$future <- c(202108)
PARAM$train_final$training <- c(
  201901, 201902, 201903, 201904, 201905, 201906,
  201907, 201908, 201909, 201910, 201911, 201912,
  202001, 202002, 202003, 202004, 202005, 202006,
  202007, 202008, 202009, 202010, 202011, 202012,
  202101, 202102, 202103, 202104, 202105, 202106
)
PARAM$train_final$undersampling <- 0.10
PARAM$train_final$ksemillerio <- 30

# Parámetros para la generación del archivo de Kaggle
PARAM$kaggle <- list()
PARAM$kaggle$envios <- 11000


#------------------------------------------------------------------------------
# INICIO DEL WORKFLOW
#------------------------------------------------------------------------------
# Creo la carpeta donde se guardará la salida del experimento
dir.create(PARAM$experimento_folder, showWarnings = FALSE)

# Configuro el logger
log_file <- file.path(PARAM$experimento_folder, paste0("log_", PARAM$experimento, ".txt"))
log_appender(appender_tee(log_file))
log_info(paste("La salida del experimento se guardará en:", PARAM$experimento_folder))

log_info("Iniciando el workflow")
log_info("---------------------------")
# Ejecuto los scripts del workflow
# Cada script es autocontenido y se ejecuta en el entorno global
source(file.path(home_dir, "1_Preprocesamiento.R"))
log_info("---------------------------")

source(file.path(home_dir, "2_Data_Quality.R"))
log_info("---------------------------")

source(file.path(home_dir, "3_Data_Drifting.R"))
log_info("---------------------------")

#source(file.path(home_dir, "4_Feature_Engineering_Intra_Mes.R"))
log_info("---------------------------")

source(file.path(home_dir, "5_Feature_Engineering_Historico.R"))
log_info("---------------------------")

#source(file.path(home_dir, "6_Feature_Engineering_RF.R"))
log_info("---------------------------")

source(file.path(home_dir, "7_Reduccion_Dimensionalidad_Canaritos.R"))
log_info("---------------------------")

source(file.path(home_dir, "8_Modelado.R"))
log_info("---------------------------")

source(file.path(home_dir, "9_Optimizacion_Bayesiana.R"))
log_info("---------------------------")

source(file.path(home_dir, "10_Evaluacion_Ensamble.R"))
log_info("---------------------------")

#source(file.path(home_dir, "11_Modelo_Final.R"))
log_info("---------------------------")
log_info("Workflow finalizado")