#!/usr/bin/env Rscript
# se filtran los meses donde se entrena el modelo final
dataset_train_final <- dataset[foto_mes %in% PARAM$train_final$training]

cols0 <- copy(colnames(dataset_train_final))
filas <- nrow(dataset_train_final)

for( i in seq(PARAM$qcanaritos) ){
  dataset_train_final[, paste0("canarito_",i) := runif( filas) ]
}

# las columnas canaritos mandatoriamente van al comienzo del dataset
cols_canaritos <- copy( setdiff( colnames(dataset_train_final), cols0 ) )
setcolorder( dataset_train_final, c( cols_canaritos, cols0 ) )

set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")
dataset_train_final[, azar := runif(nrow(dataset_train_final))]
dataset_train_final[, training := 0L]

dataset_train_final[
  (azar <= PARAM$train_final$undersampling | clase_ternaria %in% c("BAJA+1", "BAJA+2", "BAJA+3")),
  training := 1L
]

dataset_train_final[, azar:= NULL] # elimino la columna azar

dataset_train_final[,
  clase01 := ifelse(clase_ternaria %in% c("BAJA+3","BAJA+2","BAJA+1"), 1L, 0L)
]