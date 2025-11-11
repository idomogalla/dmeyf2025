#!/usr/bin/env Rscript
# Target Engineering
# paso la clase a binaria que tome valores {0,1}  enteros
# BAJA+1 y BAJA+2 son  1, CONTINUA es 0
# a partir de ahora ya NO puedo cortar  por prob(BAJA+2) > 1/40
log_info("Creando clase01.")
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2","BAJA+1"), 1L, 0L)]

# Los campos en los que se entrena
log_info("Definiendo campos_buenos (sin canaritos, se agregarán luego).")
campos_buenos <- copy( setdiff(
                        colnames(dataset), PARAM$trainingstrategy$campos_entrenar)
                      )

# Cambio las proporciones de POS/NEG
log_info("Haciendo undersampling.")
set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")
dataset[, azar := runif(nrow(dataset))]
dataset[, training := 0L]

# Undersampling
dataset[  foto_mes %in%  PARAM$trainingstrategy$training &
          (azar <= PARAM$trainingstrategy$undersampling | clase_ternaria %in% c("BAJA+1", "BAJA+2")),
          training := 1L
        ]