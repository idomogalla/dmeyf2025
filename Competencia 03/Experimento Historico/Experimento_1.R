#!/usr/bin/env Rscript
# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE, verbose = FALSE) # garbage collection

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
PARAM$experimento <- "experimento_01"
PARAM$semilla_primigenia <- 200003

# Path a los datos de entrada
PARAM$generar_ternaria <- TRUE
PARAM$dir_dataset <- "~/buckets/b1/datasets"
PARAM$dataset_name <- "competencia_03_crudo.csv.gz"
PARAM$dataset_hist_name <- "competencia_02_crudo.csv.gz"
PARAM$dataset_ternaria_name <- "competencia_03_ternaria.csv.gz"
# PARAM$dataset_name <- "competencia_03_ternaria.csv.gz"
PARAM$input_dataset <- file.path(PARAM$dir_dataset, PARAM$dataset_name)

# Path a la carpeta de salida del experimento
# La carpeta se crea relativo al script main.R
PARAM$output_folder <- "~/buckets/b1/exp"
PARAM$experimento_folder <- file.path(PARAM$output_folder, PARAM$experimento)
dir.create(PARAM$experimento_folder, showWarnings = FALSE)

PARAM$carpeta_bayesiana <- "Bayesiana"
PARAM$carpeta_evaluacion <- "Evaluacion"
PARAM$carpeta_graficos <- "Plots"
PARAM$carpeta_entregables <- "Entregables"
PARAM$modelos_folder <- "Modelos"

# Parámetros de Feature Engineering Histórico
PARAM$FE_hist <- list()
# Lags
PARAM$FE_hist$lags$run <- TRUE # Activar o desactivar lags
PARAM$FE_hist$lags$n_lags <- c(1, 2) # Número de lags a crear
PARAM$FE_hist$lags$aceleracion <- FALSE # Activar o desactivar aceleración (derivada segunda)
# Tendencias
PARAM$FE_hist$Tendencias$run <- FALSE # Activar o desactivar Tendencias
PARAM$FE_hist$Tendencias$ventana <- c(6)
PARAM$FE_hist$Tendencias$tendencia <- FALSE
PARAM$FE_hist$Tendencias$minimo <- FALSE
PARAM$FE_hist$Tendencias$maximo <- FALSE
PARAM$FE_hist$Tendencias$promedio <- FALSE
PARAM$FE_hist$Tendencias$ratioavg <- FALSE
PARAM$FE_hist$Tendencias$ratiomax <- FALSE
# Media Moviles
PARAM$FE_hist$MovingAverages$run <- FALSE # Activar o desactivar Moving Averages
PARAM$FE_hist$MovingAverages$windows <- c(3, 6) # Ventanas de moving averages
PARAM$FE_hist$MovingAverages$delta_change <- FALSE # Cambio respecto a periodo anterior (delta entre periodos)
PARAM$FE_hist$MovingAverages$vs_actual <- FALSE # Media móvil vs valor actual

# Parámetros de Training Strategy para la Optimización Bayesiana
PARAM$trainingstrategy <- list()
PARAM$trainingstrategy$training <- c(
  201901, 201902, 201903, 201904, 201905, 201906,
  201907, 201908, 201909, 201910, 201911, 201912,
  202001, 202002, 202003, 202004, 202005, 202006,
  202007, 202008, 202009, 202010, 202011, 202012,
  202101, 202102, 202103
)
PARAM$trainingstrategy$testing <- c(202105)
PARAM$trainingstrategy$undersampling <- 0.02
PARAM$trainingstrategy$positivos <- c("BAJA+1", "BAJA+2")
PARAM$trainingstrategy$campos_entrenar <- c("clase_ternaria", "clase01", "azar", "training")
PARAM$trainingstrategy$importancias <- 50

# Parámetros de Optimización de Hiperparámetros (Bayesian Optimization)
PARAM$hipeparametertuning <- list()
PARAM$hipeparametertuning$BO_iteraciones <- 30 # 50 seria mas razonable

# El parámetro ksemillerio indica se se hace semillerio DENTRO de la bayesiana
# 1 no se hace Ensemble Semillerio, apenas se corre un solo LightGBM
# mayor a 1, se hace un  k-Ensemble Semillerio
PARAM$hipeparametertuning$ksemillerio <- 30L
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
  force_row_wise = TRUE, # para evitar warning
  seed = PARAM$semilla_primigenia,
  extra_trees = FALSE,
  max_depth = -1L, # -1 significa no limitar,  por ahora lo dejo fijo
  min_gain_to_split = 0.0, # min_gain_to_split >= 0.0
  min_sum_hessian_in_leaf = 0.001, #  min_sum_hessian_in_leaf >= 0.0
  lambda_l1 = 0.0, # lambda_l1 >= 0.0
  lambda_l2 = 0.0, # lambda_l2 >= 0.0

  bagging_fraction = 1.0, # 0.0 < bagging_fraction <= 1.0
  pos_bagging_fraction = 1.0, # 0.0 < pos_bagging_fraction <= 1.0
  neg_bagging_fraction = 1.0, # 0.0 < neg_bagging_fraction <= 1.0
  is_unbalance = FALSE, #
  scale_pos_weight = 1.0, # scale_pos_weight > 0.0

  drop_rate = 0.1, # 0.0 < neg_bagging_fraction <= 1.0
  max_drop = 50, # <=0 means no limit
  skip_drop = 0.5, # 0.0 <= skip_drop <= 1.0

  max_bin = 31
)

PARAM$BO <- list()

# Parámetros para el entrenamiento final y predicción
PARAM$train_final <- list()
PARAM$train_final$envios_a_generar <- c(11000)
PARAM$train_final$future <- c(202107)
PARAM$train_final$training <- c(
  201901, 201902, 201903, 201904, 201905, 201906,
  201907, 201908, 201909, 201910, 201911, 201912,
  202001, 202002, 202003, 202004, 202005, 202006,
  202007, 202008, 202009, 202010, 202011, 202012,
  202101, 202102, 202103, 202104, 202105
)
PARAM$train_final$undersampling <- 0.10
PARAM$train_final$iter <- 10
PARAM$train_final$ksemillerio <- 10
PARAM$train_final$cortes_evaluacion <- seq(0, 20000, by = 500)

#------------------------------------------------------------------------------
# Función wrapper para ejecutar y cronometrar scripts
#------------------------------------------------------------------------------
source_con_log <- function(script_path, script_name) {
  log_info(paste("--- Iniciando:", script_name, "---"))
  t_inicio <- Sys.time()

  tryCatch(
    {
      # Ejecuta el script.
      # local=FALSE asegura que todo corra en el mismo entorno que main.R
      source(script_path, local = FALSE)

      t_fin <- Sys.time()
      duracion <- round(as.numeric(difftime(t_fin, t_inicio, units = "secs")), 2)

      log_info(paste("--- Fin:", script_name, ". Duración:", duracion, "segundos. ---"))
    },
    error = function(e) {
      # Si el script falla, igual registra el tiempo y el error
      t_fin <- Sys.time()
      duracion <- round(as.numeric(difftime(t_fin, t_inicio, units = "secs")), 2)
      log_error(paste("--- ERROR en:", script_name, "tras", duracion, "segundos. ---"))
      log_error(paste("Mensaje de R:", e$message))

      # Detiene la ejecución de todo el main.R si un script falla
      stop("Error en el script: ", script_name, ". Deteniendo el workflow.")
    }
  )

  log_info("--------------------------------------------------") # Separador
}

#------------------------------------------------------------------------------
# INICIO DEL WORKFLOW
#------------------------------------------------------------------------------
# Creo la carpeta donde se guardará la salida del experimento
dir.create(PARAM$experimento_folder, showWarnings = FALSE)

# Configuro el logger
log_file <- file.path(PARAM$experimento_folder, paste0("log_", PARAM$experimento, ".txt"))
log_appender(appender_tee(log_file))
log_info(paste("La salida del experimento se guardará en:", PARAM$experimento_folder))

log_info("Inciando el workflow")
log_info("==================================================")

# Ejecuto los scripts del workflow usando el wrapper
source_con_log(file.path(home_dir, "1_Preprocesamiento.R"), "1_Preprocesamiento.R")
source_con_log(file.path(home_dir, "2_Eliminacion_de_Features.R"), "2_Eliminacion_de_Features")
source_con_log(file.path(home_dir, "3_Data_Quality.R"), "3_Data_Quality.R")
source_con_log(file.path(home_dir, "4_Data_Drifting.R"), "4_Data_Drifting.R")
source_con_log(file.path(home_dir, "5_Feature_Engineering_Historico.R"), "5_Feature_Engineering_Historico.R")
source_con_log(file.path(home_dir, "6_Modelado.R"), "6_Modelado.R")
source_con_log(file.path(home_dir, "7_Optimizacion_Bayesiana.R"), "7_Optimizacion_Bayesiana.R")
source_con_log(file.path(home_dir, "8_Evaluacion.R"), "8_Evaluacion.R")
source_con_log(file.path(home_dir, "9_Evaluacion_APO.R"), "9_Evaluacion_APO.R")

log_info("==================================================")
log_info("Workflow finalizado")
