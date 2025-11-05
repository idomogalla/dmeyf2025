tryCatch({
  # Definir y crear la carpeta para los outputs de la BO
  dir_bayesiana <- file.path(PARAM$experimento_folder, PARAM$carpeta_bayesiana)
  dir.create(dir_bayesiana, showWarnings = FALSE)

  # Optimizacion de Hipeparámetros
  log_info("Creando dtrain.")
  dtrain <- lgb.Dataset(
    data= data.matrix(dataset[training == 1L, campos_buenos, with = FALSE]),
    label= dataset[training == 1L, clase01],
    free_raw_data= TRUE
  )

  log_info(paste("dtrain nombre de las columnas: ", paste(colnames(dataset), collapse = ", ")))
  log_info(paste("dtrain filas:", nrow(dtrain), "columnas:", ncol(dtrain)))

  # Notar que se recorren algunos hiperparametros en forma logaritmica
  # y que con forbidden se tiene en cuenta el juego que hay entre min_data_in_leaf y num_leaves
  PARAM$hipeparametertuning$hs <- makeParamSet(
    makeNumericParam("num_iterations", lower= 0.0, upper= 11.1, trafo= function(x) as.integer( round(2^x)) ),
    makeNumericParam("learning_rate", lower= -8.0, upper= -1.0, trafo= function(x) 2^x ),
    makeNumericParam("feature_fraction", lower= 0.05, upper= 1.0 ),
    makeNumericParam("min_data_in_leaf", lower= 0.0, upper= log2(nrow(dtrain)/2), trafo= function(x) as.integer(round(2^x)) ),
    makeNumericParam("num_leaves", lower= 1.0, upper= 10.0, trafo= function(x) as.integer(round(2^x)) ),
    forbidden = quote( (2^min_data_in_leaf)*(2^num_leaves) > nrow(dtrain) )
  )

  # defino los datos de testing
  log_info("Creando dataset_test.")
  dataset_test <- dataset[foto_mes %in% PARAM$trainingstrategy$testing]

  # precalculo el campo de la ganancia
  dataset_test[, gan := -20000.0 ]
  dataset_test[ clase_ternaria=="BAJA+2", gan := 780000]

  # precalculo la test_matrix
  test_matrix <- data.matrix(dataset_test[, campos_buenos, with= FALSE])

  log_info(paste("dataset_test filas:", nrow(dataset_test), "columnas:", ncol(dataset_test)))
  log_info(paste("test_matrix a ser usada en la bayesiana filas:", nrow(test_matrix), "columnas:", ncol(test_matrix)))

  primos <- generate_primes(min = 100000, max = 1000000)
  set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")
  # me quedo con PARAM$semillerio  primos al azar
  PARAM$BO$semillas <- sample(primos)[seq( PARAM$hipeparametertuning$ksemillerio * PARAM$hipeparametertuning$repe )]
  
  saveRDS(PARAM$BO$semillas, 
          file = file.path(dir_bayesiana, "BO_semillas.rds")) # Evito problemas de memoria al guardar el objeto semillas en otro lado

  log_info(paste("Semillas:", paste(PARAM$BO$semillas, collapse = ", ")))
  log_info(paste("PARAM object saved to:", file.path(dir_bayesiana, "param_state.RData")))

  # logueo al archivo BO_log.txt
  loguear  <- function( reg, arch=NA, verbose=TRUE )
  {
    t0 <- Sys.time()
    archivo <- arch
    if( is.na(arch) ) archivo <- paste0( folder, substitute( reg), ext )


    if( !file.exists( archivo ) )
    {
      # Escribo los titulos
      linea  <- paste0( "fecha\t", 
                        paste( list.names(reg), collapse="\t" ), "\n" )

      cat( linea, file=archivo )
    }

    # escribo el registro
    linea  <- paste0( format(t0, "%Y%m%d.%H%M%S"),  "\t",     # la fecha y hora
                      gsub( ", ", "\t", toString( reg ) ),  "\n" )

    cat( linea, file=archivo, append=TRUE )  # grabo al archivo

    if( verbose )  cat( linea )   # imprimo por pantalla
  }

  # esto esta en una funcion para que el garbage collector lo libere
  # entrena, aplica el modelo a testing, y devuelve el vector de probabilidades
  OneTrainPredict <- function(param_completo) {
    modelo <- lgb.train(
      data= dtrain,
      param= param_completo
    )
    gmodelo <<- modelo

    # aplico el modelo a los datos nuevos
    pred <- predict(
      modelo,
      test_matrix
    )

    return( pred )
  }

  # En el argumento x llegan los parmaetros de la bayesiana
  #  devuelve la ganancia en datos de testing

  # aqui se ira guardando la mejor iteracion de la bayesiana
  gmejor <- list()
  gmejor$iter <- 0
  gmejor$gan <- -Inf

  log_bo_file <- file.path(dir_bayesiana, "BO_log.txt")
  giter <- 0
  if( file.exists(log_bo_file) ){
    tb_BO <- fread(log_bo_file)
    giter <- nrow(tb_BO) -1 
  }

  EstimarGanancia_lightgbm <- function(x) {
    giter <<- giter + 1
    log_info(paste("BO iter:", giter))
    # x pisa (o agrega) a param_fijos
    param_completo <- modifyList(PARAM$lgbm$param_fijos, x)

    vgan_mesetas <- c()  # las ganancias, tengo repe de ellas

    # loop de las repeticionies
    for( repe in seq( PARAM$hipeparametertuning$repe ) )
    {
      desde <- (PARAM$hipeparametertuning$repe-1)*PARAM$hipeparametertuning$ksemillerio + 1
      hasta <- desde + PARAM$hipeparametertuning$ksemillerio -1
      rsemillas <- PARAM$BO$semillas[ desde:hasta ]

      # vector inicial de probabilidades
      vpred_acum <- rep( 0.0, nrow(dataset_test) )

      # loop del semillerio
      for( sem in rsemillas ) # itero semillerio
      {
          param_completo$seed <- sem  # asigno se semilla
          vpred_acum <- vpred_acum + OneTrainPredict( param_completo )
          
          gc(full= TRUE, verbose= FALSE)
      }

      # Calculo de ganancia suavizada de la meseta
      tb_prediccion <- dataset_test[, list(gan)]
      tb_prediccion[, prob := vpred_acum ]

      setorder(tb_prediccion, -prob)
      tb_prediccion[, gan_acum := cumsum(gan)]

      # la meseta es un punto, mil para la izquierda, otros mil para la derecha
      tb_prediccion[, gan_meseta :=
        frollmean(
          x= gan_acum, n= 2001, align= "center",
          na.rm= TRUE, hasNA= TRUE
        )
      ]

      vgan_mesetas <- c(vgan_mesetas, tb_prediccion[, max(gan_meseta, na.rm = TRUE)] )
    }

    gan_mesetas_prom <- mean( vgan_mesetas ) 

    if( gan_mesetas_prom > gmejor$gan ){
      gmejor$gan <<- gan_mesetas_prom
      gmejor$iter <<- giter

      # grabo importancia de variables
      fwrite( lgb.importance(gmodelo),
        file= file.path(dir_bayesiana, paste0("impo_", giter, ".txt")),
        sep= "\t"
      )
    }

    # datos que voy a loguear
    xx <- copy(param_completo)
    xx$iter <- giter
    xx$metrica_mejor <- gmejor$gan
    xx$metrica_sd <- sd(vgan_mesetas)
    xx$metrica <- gan_mesetas_prom

    loguear( xx, log_bo_file)
    set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")  # le reordeno a mlrMBO

    return( gan_mesetas_prom ) #tiempo_corrida
  }

  # Aqui comienza la configuracion de la Bayesian Optimization
  # en este archivo quedan la evolucion binaria de la BO
  kbayesiana <- file.path(dir_bayesiana, "bayesiana.RDATA")

  funcion_optimizar <- EstimarGanancia_lightgbm # la funcion que voy a maximizar

  configureMlr(show.learner.output= FALSE)

  # configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
  # por favor, no desesperarse por lo complejo

  obj.fun <- makeSingleObjectiveFunction(
    fn= funcion_optimizar, # la funcion que voy a maximizar
    minimize= FALSE, # estoy Maximizando la ganancia
    noisy= TRUE,
    par.set= PARAM$hipeparametertuning$hs, # definido al comienzo del programa
    has.simple.signature= FALSE # paso los parametros en una lista
  )

  # cada 600 segundos guardo el resultado intermedio
  ctrl <- makeMBOControl(
    save.on.disk.at.time= 600, # se graba cada 600 segundos
    save.file.path= kbayesiana
  ) # se graba cada 600 segundos

  # indico la cantidad de iteraciones que va a tener la Bayesian Optimization
  ctrl <- setMBOControlTermination(
    ctrl,
    iters= PARAM$hipeparametertuning$BO_iteraciones
  ) # cantidad de iteraciones

  # defino el método estandar para la creacion de los puntos iniciales,
  # los "No Inteligentes"
  ctrl <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI())

  # establezco la funcion que busca el maximo
  surr.km <- makeLearner(
    "regr.km",
    predict.type= "se",
    covtype= "matern3_2",
    control= list(trace= TRUE)
  )

  # inicio la optimizacion bayesiana, retomando si ya existe
  # es la celda mas lenta de todo el notebook
  log_info("Iniciando optimización bayesiana.")
  if (!file.exists(kbayesiana)) {
    log_info("No existe bayesiana.RDATA, comienzo nueva optimización.")
    bayesiana_salida <- mbo(obj.fun, learner= surr.km, control= ctrl)
  } else {
    log_info("Existe bayesiana.RDATA, retomo optimización.")
    bayesiana_salida <- mboContinue(kbayesiana) # retomo en caso que ya exista
  }
  log_info("Fin optimización bayesiana")
}, error = function(e) {
  log_error("######################################################")
  log_error("Se ha producido un error fatal en la Sección 9: Optimización Bayesiana.")
  log_error(paste("Mensaje de R:", e$message))
  log_error("######################################################")
})