log_info("Inicio 6_Feature_Engineering_RF.R")
# Feature Engineering a partir de hojas de Random Forest
columnas_originales <- copy(colnames(dataset))

AgregaVarRandomForest <- function() {

  log_info("inicio AgregaVarRandomForest()")
  gc(verbose= FALSE)
  dataset[, clase01 := 0L ]
  dataset[ clase_ternaria %in% c( "BAJA+2", "BAJA+1"),
      clase01 := 1L ]

  campos_buenos <- setdiff(
    colnames(dataset),
    c( "clase_ternaria", "clase01")
  )

  dataset[, entrenamiento :=
    as.integer( foto_mes %in% PARAM$FE_rf$train$training )]

  dtrain <- lgb.Dataset(
    data = data.matrix(dataset[entrenamiento == TRUE, campos_buenos, with = FALSE]),
    label = dataset[entrenamiento == TRUE, clase01],
    free_raw_data = FALSE
  )

  log_info("Entrenando Random Forest")
  modelo <- lgb.train(
     data = dtrain,
     param = PARAM$FE_rf$lgb_param,
     verbose = -100
  )

  log_info("Fin construccion RandomForest")
  # grabo el modelo, achivo .model
  lgb.save(modelo, file="modelo.model" )

  qarbolitos <- copy(PARAM$FE_rf$lgb_param$num_iterations)

  periodos <- dataset[ , unique( foto_mes ) ]

  for( periodo in  periodos )
  {
    log_info(paste("periodo = ", periodo))
    datamatrix <- data.matrix(dataset[ foto_mes== periodo, campos_buenos, with = FALSE])

    log_info("Inicio prediccion")
    prediccion <- predict(
        modelo,
        datamatrix,
        type = "leaf"
    )
    log_info("Fin prediccion")

    for( arbolito in 1:qarbolitos )
    {
       log_info(paste("arbolito = ", arbolito))
       hojas_arbol <- unique(prediccion[ , arbolito])

       for (pos in 1:length(hojas_arbol)) {
         # el numero de nodo de la hoja, estan salteados
         nodo_id <- hojas_arbol[pos]
         dataset[ foto_mes== periodo, paste0(
            "rf_", sprintf("%03d", arbolito),
             "_", sprintf("%03d", nodo_id)
          ) :=  as.integer( nodo_id == prediccion[ , arbolito]) ]

       }

       rm( hojas_arbol )
    }

    rm( prediccion )
    rm( datamatrix )
    gc(verbose= FALSE)
  }

  gc(verbose= FALSE)

  # borro clase01 , no debe ensuciar el dataset
  dataset[ , clase01 := NULL ]
  log_info("Fin AgregaVarRandomForest()")
}

# Feature Engineering agregando variables de Random Forest
#  aqui es donde se hace el trabajo
AgregaVarRandomForest()

log_info("Nuevas columnas creadas:")
log_info(paste0(columnas_nuevas, collapse = ", "))
rm(columnas_originales)
gc(verbose= FALSE)
log_info("Fin 6_Feature_Engineering_RF.R")