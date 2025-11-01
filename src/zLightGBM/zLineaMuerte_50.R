# Linea de Muerte Z
# 1. Big Picture
# * Preprocesamiento
#    * Se quitan del datataset los envenenados atributos mprestamos_personales y cprestamos_personales
#    * Se agregan lags y delta_lags de orden 1 y 2
# * Modelado no se optimizan hiperarámetros
# * Produccion
#    * Entrenamieento final
#       * Se entrena en {202101, 202102, 202103, 202104}
#       * Se hace un conservador undersampling de 0.50 de los "CONTINUA"
#       * POS = {"BAJA+1", "BAJA+2"}
#       * librería zLightGBM
#          * 100 canaritos se agregan al comienzo del dataset, porque ahora hay mas datos
#          * gradient_bound se deja en su default de 0.1
#    * Clasificacion
#       * Se corta en 11000 envios
#
# Resultados :
# * ganancia de 391.085 M en el Private Leaderboard ( 11.000 en el Public )
# * utiliza 17 GB de memoria RAM
# * corre en 30 minutos

## Inicializacion

# limpio la memoria
Sys.time()
rm(list = ls(all.names = TRUE))
gc(full = TRUE, verbose = FALSE)

PARAM <- list()
PARAM$experimento <- "zmuerte-50"
PARAM$semilla_primigenia <- 974411

setwd("/content/buckets/b1/exp")
experimento_folder <- PARAM$experimento
dir.create(experimento_folder, showWarnings = FALSE)
setwd(paste0("/content/buckets/b1/exp/", experimento_folder))

## Preprocesamiento

### Generacion de la clase_ternaria

Sys.time()
require("data.table")

# leo el dataset
dataset <- fread("~/buckets/b1/datasets/competencia_01_crudo.csv")

# calculo el periodo0 consecutivo
dsimple <- dataset[, list(
  "pos" = .I,
  numero_de_cliente,
  periodo0 = as.integer(foto_mes / 100) * 12 + foto_mes %% 100
)]

# ordeno
setorder(dsimple, numero_de_cliente, periodo0)

# calculo topes
periodo_ultimo <- dsimple[, max(periodo0)]
periodo_anteultimo <- periodo_ultimo - 1

# calculo los leads de orden 1 y 2
dsimple[, c("periodo1", "periodo2") :=
          shift(periodo0, n = 1:2, fill = NA, type = "lead"),
        numero_de_cliente
]

# assign most common class values = "CONTINUA"
dsimple[periodo0 < periodo_anteultimo, clase_ternaria := "CONTINUA"]

# calculo BAJA+1
dsimple[ periodo0 < periodo_ultimo &
         (is.na(periodo1) | periodo0 + 1 < periodo1),
       clase_ternaria := "BAJA+1"
]

# calculo BAJA+2
dsimple[ periodo0 < periodo_anteultimo & (periodo0 + 1 == periodo1) &
         (is.na(periodo2) | periodo0 + 2 < periodo2),
       clase_ternaria := "BAJA+2"
]

# pego el resultado en el dataset original y grabo
setorder(dsimple, pos)
dataset[, clase_ternaria := dsimple$clase_ternaria]

rm(dsimple)
gc()
Sys.time()

### Eliminacion de Features

# La auténtica salsa mágica de este script es la eliminación de estos dos atributos
# mprestamos_personales y cprestamos_personales
dataset[, mprestamos_personales := NULL]
dataset[, cprestamos_personales := NULL]

Sys.time()

### Feature Engineering Historico

# Creacion de LAGs
setorder(dataset, numero_de_cliente, foto_mes)

# todo es lagueable, menos la primary key y la clase
cols_lagueables <- copy(setdiff(
  colnames(dataset),
  c("numero_de_cliente", "foto_mes", "clase_ternaria")
))

# lags de orden 1
dataset[, paste0(cols_lagueables, "_lag1") := shift(.SD, 1, NA, "lag"),
        by = numero_de_cliente, .SDcols = cols_lagueables]

# lags de orden 2
dataset[, paste0(cols_lagueables, "_lag2") := shift(.SD, 2, NA, "lag"),
        by = numero_de_cliente, .SDcols = cols_lagueables]

# agrego los delta lags
for (vcol in cols_lagueables) {
  dataset[, paste0(vcol, "_delta1") := get(vcol) - get(paste0(vcol, "_lag1"))]
  dataset[, paste0(vcol, "_delta2") := get(vcol) - get(paste0(vcol, "_lag2"))]
}

Sys.time()

## Modelado

### Optimizacion de Hipeparámetros

# Este script no hace optimización de hiperparámetros
# No se llama a una Bayesian Optimization, ese paso se saltea.

## Produccion

# training y future
Sys.time()

PARAM$train_final$meses <- c(202101, 202102, 202103, 202104)
PARAM$train_final$undersampling <- 0.50

PARAM$future <- c(202106)

### Final Training Strategy

# se filtran los meses donde se entrena el modelo final
dataset_train_final <- dataset[foto_mes %in% PARAM$train_final$meses]

# Undersampling, van todos los "BAJA+1" y "BAJA+2" y solo algunos "CONTINUA"
set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")
dataset_train_final[, azar := runif(.N)]
dataset_train_final[, training := 0L]

dataset_train_final[
  (azar <= PARAM$train_final$undersampling | clase_ternaria %in% c("BAJA+1", "BAJA+2")),
  training := 1L
]

dataset_train_final[, azar := NULL]

### Target Engineering

# paso la clase a binaria que tome valores {0,1}  enteros
# BAJA+1 y BAJA+2 son 1, CONTINUA es 0
dataset_train_final[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

### Final Model

# utilizo zLightGBM la nueva libreria
if (!require("zlightgbm")) {
  install.packages("https://storage.googleapis.com/open-courses/dmeyf2025-e4a2/zlightgbm_4.6.0.99.tar.gz",
                   repos = NULL, type = "source")
}
require("zlightgbm")
Sys.time()

# canaritos
PARAM$qcanaritos <- 100

cols0 <- copy(colnames(dataset_train_final))
filas <- nrow(dataset_train_final)

for (i in seq_len(PARAM$qcanaritos)) {
  dataset_train_final[, paste0("canarito_", i) := runif(filas)]
}

# las columnas canaritos mandatoriamente van al comienzo del dataset
cols_canaritos <- copy(setdiff(colnames(dataset_train_final), cols0))
setcolorder(dataset_train_final, c(cols_canaritos, cols0))

Sys.time()

# los campos que se van a utilizar
campos_buenos <- setdiff(
  colnames(dataset_train_final),
  c("clase_ternaria", "clase01", "training")
)

# dejo los datos en el formato que necesita LightGBM
dtrain_final <- lgb.Dataset(
  data = data.matrix(dataset_train_final[training == 1L, campos_buenos, with = FALSE]),
  label = dataset_train_final[training == 1L, clase01],
  free_raw_data = FALSE
)

cat("filas", nrow(dtrain_final), "columnas", ncol(dtrain_final), "\n")
Sys.time()

# definicion de parametros, los viejos y los nuevos
PARAM$lgbm <- list(
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
  min_data_in_leaf = 20L,  # este ya es el valor default de LightGBM

  num_iterations = 9999L, # dejo libre la cantidad de arboles, zLightGBM se detiene solo
  num_leaves = 999L, # dejo libre la cantidad de hojas, zLightGBM sabe cuando no hacer un split
  learning_rate = 1.0,  # se lo deja en 1.0 para que si el score esta por debajo de gradient_bound no se lo escale

  feature_fraction = 0.50, # un valor equilibrado, habra que probar alternativas ...

  canaritos = PARAM$qcanaritos, # fundamental en zLightGBM, aqui esta el control del overfitting
  gradient_bound = 0.1  # default de zLightGBM
)

Sys.time()

#### Entrenamiento del modelo

# entreno el modelo
modelo_final <- lgb.train(
  data = dtrain_final,
  param = PARAM$lgbm
)

Sys.time()

# grabo el modelo generado, esto pude ser levantado por LightGBM en cualquier maquina
lgb.save(modelo_final, file = "zmodelo.txt")

# grabo un dataset que tiene el detalle de los arboles de LightGBM
tb_arboles <- lgb.model.dt.tree(modelo_final)
fwrite(tb_arboles, file = "tb_arboles.txt", sep = "\t")

cat("cantidad arbolitos=", tb_arboles[, max(tree_index) + 1], "\n")
cat("summary de las hojas de los arboles\n")
print(summary(tb_arboles[, list(hojas = max(leaf_index, na.rm = TRUE) + 1), tree_index][, hojas]))

Sys.time()

### Scoring

# aplico el modelo a los datos sin clase
dfuture <- dataset[foto_mes %in% PARAM$future]

# en la versión actual de zLightGBM los campos canaritos deben estar en el dataset donde se hace el predict()
filas <- nrow(dfuture)

for (i in seq_len(PARAM$qcanaritos)) {
  dfuture[, paste0("canarito_", i) := runif(filas)]
}

prediccion <- predict(
  modelo_final,
  data.matrix(dfuture[, campos_buenos, with = FALSE])
)

# tabla de prediccion, puede ser util para futuros ensembles
tb_prediccion <- dfuture[, list(numero_de_cliente, foto_mes)]
tb_prediccion[, prob := prediccion]

# grabo las probabilidad del modelo
fwrite(tb_prediccion,
       file = "prediccion.txt",
       sep = "\t")

### Clasificacion

# genero archivos con los "envios" mejores
dir.create("kaggle", showWarnings = FALSE)

# ordeno por probabilidad descendente
setorder(tb_prediccion, -prob)

envios <- 11000
tb_prediccion[, Predicted := 0L]
tb_prediccion[1:envios, Predicted := 1L]

archivo_kaggle <- paste0("./kaggle/KA", PARAM$experimento, "_", envios, ".csv")

# grabo el archivo
fwrite(tb_prediccion[, list(numero_de_cliente, Predicted)],
       file = archivo_kaggle,
       sep = ",")

### Subida a Kaggle

# subida automática a Kaggle, solo tiene sentido para la Primera Competencia
comando <- "kaggle competitions submit"
UBA_comp <- "-c dm-ey-f-2025-primera"
arch <- paste("-f", archivo_kaggle)

mensaje <- paste0("-m 'under=", PARAM$train_final$undersampling,
                  "  cana=", PARAM$qcanaritos,
                  "  gb=", PARAM$lgbm$gradient_bound,
                  "  ff=", PARAM$lgbm$feature_fraction,
                  "  mdil=", PARAM$lgbm$min_data_in_leaf,
                  "  lr=", PARAM$lgbm$learning_rate, "'"
)

linea <- paste(comando, UBA_comp, arch, mensaje)
salida <- system(linea, intern = TRUE)
cat(salida, sep = "\n")

Sys.time()