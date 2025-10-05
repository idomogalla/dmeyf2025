# Función para manejar errores globales
manejar_error <- function() {
  if (exists("log_error")) {
    log_error("=================================================================")
    log_error("ERROR CRÍTICO: El script ha fallado")
    log_error("=================================================================")
    log_error("Revise el archivo de log para más detalles")
  }
}

# Configurar manejador de errores al salir
options(error = manejar_error)

# Función principal envuelta en tryCatch
main <- function() {
  
  tryCatch({
    
    # Cargar e instalar logger primero
    if(!require("logger")) install.packages("logger")
    library("logger")
    
    # Cargo las librerías necesarias
    if(!require("data.table")) install.packages("data.table")
    library("data.table")
    
    if(!require("parallel")) install.packages("parallel")
    library("parallel")
    
    if(!require("R.utils")) install.packages("R.utils")
    library("R.utils")
    
    if(!require("primes")) install.packages("primes")
    library("primes")
    
    if(!require("utils")) install.packages("utils")
    library("utils")
    
    if(!require("rlist")) install.packages("rlist")
    library("rlist")
    
    if(!require("yaml")) install.packages("yaml")
    library("yaml")
    
    if(!require("lightgbm")) install.packages("lightgbm")
    library("lightgbm")
    
    if(!require("DiceKriging")) install.packages("DiceKriging")
    library("DiceKriging")
    
    if(!require("mlrMBO")) install.packages("mlrMBO")
    library("mlrMBO")
    
    if(!require("ggplot2")) install.packages("ggplot2")
    library("ggplot2")
    
    # Limpio la memoria
    rm(list=ls(all.names=TRUE))
    gc(full=TRUE, verbose=FALSE)
    
    # ============================================================================
    # Parámetros
    # ============================================================================
    
    PARAM <- list()
    PARAM$experimento <- "expC01_Prueba02A"
    PARAM$semilla_primigenia <- 200003
    
    # Configurar logger
    log_file <- paste0("log_", PARAM$experimento, ".txt")
    log_appender(appender_tee(log_file))
    log_threshold(INFO)
    
    log_info("=================================================================")
    log_info("Inicio del experimento: {PARAM$experimento}")
    log_info("Fecha y hora: {format(Sys.time(), '%a %b %d %X %Y')}")
    log_info("=================================================================")
    
    # Training y future
    PARAM$train <- c(202101, 202102)
    PARAM$train_final <- c(202101, 202102)
    PARAM$future <- c(202104)
    PARAM$train_final_kaggle <- c(202101, 202102, 202103, 202104)
    PARAM$entrega_kaggle <- c(202106)
    PARAM$semilla_kaggle <- 314159
    PARAM$cortes <- seq(0, 20000, by= 100)
    
    log_info("Parámetros de training configurados:")
    log_info("  - train: {paste(PARAM$train, collapse=', ')}")
    log_info("  - train_final: {paste(PARAM$train_final, collapse=', ')}")
    log_info("  - future: {paste(PARAM$future, collapse=', ')}")
    
    PARAM$trainingstrategy$undersampling <- 0.5
    log_info("Undersampling: {PARAM$trainingstrategy$undersampling}")
    
    # Parametros LightGBM
    PARAM$hyperparametertuning$xval_folds <- 5
    log_info("Cross-validation folds: {PARAM$hyperparametertuning$xval_folds}")
    
    PARAM$lgbm$param_fijos <-  list(
      boosting= "gbdt",
      objective= "binary",
      metric= "auc",
      first_metric_only= FALSE,
      boost_from_average= TRUE,
      feature_pre_filter= FALSE,
      force_row_wise= TRUE,
      verbosity= -100,
      seed= PARAM$semilla_primigenia,
      max_depth= -1L,
      min_gain_to_split= 0,
      min_sum_hessian_in_leaf= 0.001,
      lambda_l1= 0.0,
      lambda_l2= 0.0,
      max_bin= 31L,
      bagging_fraction= 1.0,
      pos_bagging_fraction= 1.0,
      neg_bagging_fraction= 1.0,
      is_unbalance= FALSE,
      scale_pos_weight= 1.0,
      drop_rate= 0.1,
      max_drop= 50,
      skip_drop= 0.5,
      extra_trees= FALSE,
      num_iterations= 1200,
      learning_rate= 0.02,
      feature_fraction= 0.5,
      num_leaves= 750,
      min_data_in_leaf= 5000
    )
    
    log_info("Parámetros fijos de LightGBM configurados")
    
    # Bordes de hiperparámetros para BO
    PARAM$hypeparametertuning$hs <- makeParamSet(
        makeNumericParam("min_sum_hessian_in_leaf", lower= 0.001, upper= 0.1),
        
        makeNumericParam("lambda_l1", lower= 0.0, upper= 100.0),
        makeNumericParam("lambda_l2", lower= 0.0, upper= 100.0),
        
        
        makeIntegerParam("num_leaves", lower= 10L, upper= 2048L),
        makeIntegerParam("max_depth", lower= -1L, upper= 14),


        makeIntegerParam("num_iterations", lower= 50L, upper= 3000L),
        makeNumericParam("learning_rate", lower= 0.01, upper= 0.3),

        makeNumericParam("feature_fraction", lower= 0.1, upper= 1.0),
        makeNumericParam("bagging_fraction", lower= 0.0, upper= 1.0),
        makeIntegerParam("bagging_freq", lower= 0L, upper= 10L),
        
        makeIntegerParam("min_data_in_leaf", lower= 10L, upper= 8000L),
        makeNumericParam("min_gain_to_split", lower= 0.0, upper= 15.0)
    )
    PARAM$hyperparametertuning$iteraciones <- 100
    
    log_info("Iteraciones de optimización bayesiana: {PARAM$hyperparametertuning$iteraciones}")
    
    # ============================================================================
    # Funciones auxiliares
    # ============================================================================
    
    particionar <- function(data, division, agrupa= "", campo= "fold", start= 1, seed= NA) {
      if (!is.na(seed)) set.seed(seed, "L'Ecuyer-CMRG")
      
      bloque <- unlist(mapply(
        function(x, y) {rep(y, x)},division, seq(from= start, length.out= length(division))))
      
      data[, (campo) := sample(rep(bloque,ceiling(.N / length(bloque))))[1:.N],by= agrupa]
    }
    
    realidad_inicializar <- function( pfuture, pparam) {
      drealidad <- pfuture[, list(numero_de_cliente, foto_mes, clase_ternaria)]
      
      particionar(drealidad,
        division= c(3, 7),
        agrupa= "clase_ternaria",
        seed= PARAM$semilla_kaggle
      )
      
      return( drealidad )
    }
    
    realidad_evaluar <- function( prealidad, pprediccion) {
      prealidad[ pprediccion,
        on= c("numero_de_cliente", "foto_mes"),
        predicted:= i.Predicted
      ]
      
      tbl <- prealidad[, list("qty"=.N), list(fold, predicted, clase_ternaria)]
      
      res <- list()
      res$public  <- tbl[fold==1 & predicted==1L, sum(qty*ifelse(clase_ternaria=="BAJA+2", 780000, -20000))]/0.3
      res$private <- tbl[fold==2 & predicted==1L, sum(qty*ifelse(clase_ternaria=="BAJA+2", 780000, -20000))]/0.7
      res$total <- tbl[predicted==1L, sum(qty*ifelse(clase_ternaria=="BAJA+2", 780000, -20000))]
      
      prealidad[, predicted:=NULL]
      return( res )
    }
    
    # ============================================================================
    # Lectura del dataset
    # ============================================================================
    
    log_info("Iniciando lectura del dataset...")
    
    tryCatch({
      setwd("~/buckets/b1/datasets")
      dataset <- fread("./competencia_01.csv.gz", stringsAsFactors= TRUE)
      log_info("Dataset cargado exitosamente. Dimensiones: {nrow(dataset)} filas x {ncol(dataset)} columnas")
    }, error = function(e) {
      log_error("ERROR al cargar el dataset: {e$message}")
      stop(e)
    })
    
    # ============================================================================
    # Generación de lags
    # ============================================================================
    
    log_info("Generando lags de orden 1...")
    
    tryCatch({
      cols_a_excluir <- c("numero_de_cliente", "foto_mes", "clase_ternaria")
      cols_con_lag <- setdiff(names(dataset), cols_a_excluir)
      nombres_nuevas_cols_lag <- paste0(cols_con_lag, "_lag1")
      
      dataset[, (nombres_nuevas_cols_lag) := shift(.SD, 1, NA, "lag"), by = numero_de_cliente, .SDcols = cols_con_lag]
      log_info("Lags generados: {length(nombres_nuevas_cols_lag)} columnas")
      
      nombres_nuevas_cols_delta <- paste0(cols_con_lag, "_delta1")
      dataset[, (nombres_nuevas_cols_delta) := .SD - mget(nombres_nuevas_cols_lag), .SDcols = cols_con_lag]
      log_info("Delta lags generados: {length(nombres_nuevas_cols_delta)} columnas")
    }, error = function(e) {
      log_error("ERROR al generar lags: {e$message}")
      stop(e)
    })
    
    # ============================================================================
    # Preparación de datos de entrenamiento
    # ============================================================================
    
    log_info("Seleccionando datos de training...")
    
    tryCatch({
      dataset_train <- dataset[foto_mes %in% PARAM$train]
      log_info("Registros de training: {nrow(dataset_train)}")
      
      dataset_train[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2","BAJA+1"), 1L, 0L)]
      log_info("Clase binaria creada")
      
      # Undersampling
      set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")
      dataset_train[, azar := runif(nrow(dataset_train))]
      dataset_train[, training := 0L]
      
      dataset_train[
        foto_mes %in%  PARAM$train &
          (azar <= PARAM$trainingstrategy$undersampling | clase_ternaria %in% c("BAJA+1", "BAJA+2")),
        training := 1L
      ]
      
      registros_training <- dataset_train[training == 1L, .N]
      log_info("Registros para entrenamiento después de undersampling: {registros_training}")
      
      campos_buenos <- setdiff(
        colnames(dataset_train),
        c("clase_ternaria", "clase01", "azar", "training")
      )
      
      log_info("Campos para entrenamiento: {length(campos_buenos)}")
      
      # Crear dataset LightGBM
      dtrain <- lgb.Dataset(
        data= data.matrix(dataset_train[training == 1L, campos_buenos, with= FALSE]),
        label= dataset_train[training == 1L, clase01],
        free_raw_data= FALSE
      )
      
      log_info("Dataset LightGBM creado: {nrow(dtrain)} filas x {ncol(dtrain)} columnas")
      
    }, error = function(e) {
      log_error("ERROR en preparación de datos: {e$message}")
      stop(e)
    })
    
    # ============================================================================
    # Configuración de directorios
    # ============================================================================
    
    tryCatch({
      setwd("~/buckets/b1/exp/")
      dir.create(PARAM$experimento, showWarnings=FALSE)
      setwd(paste0("~/buckets/b1/exp/",PARAM$experimento))
      log_info("Directorio de trabajo: {getwd()}")
    }, error = function(e) {
      log_error("ERROR al configurar directorios: {e$message}")
      stop(e)
    })
    
    # ============================================================================
    # Función de optimización
    # ============================================================================
    
    EstimarGanancia_AUC_lightgbm <- function(x) {
      tryCatch({
        param_completo <- modifyList(PARAM$lgbm$param_fijos, x)
        
        modelocv <- lgb.cv(
          data= dtrain,
          nfold= PARAM$hyperparametertuning$xval_folds,
          stratified= TRUE,
          param= param_completo
        )
        
        AUC <- modelocv$best_score
        
        rm(modelocv)
        gc(full= TRUE, verbose= FALSE)
        
        msg <- paste0(format(Sys.time(), "%a %b %d %X %Y"), " AUC ", AUC)
        message(msg)
        log_info(msg)
        
        return(AUC)
      }, error = function(e) {
        log_error("ERROR en EstimarGanancia_AUC_lightgbm: {e$message}")
        log_error("Parámetros que causaron el error: {paste(names(x), x, sep='=', collapse=', ')}")
        return(-Inf)  # Retorna un valor muy bajo para que la optimización bayesiana lo descarte
      })
    }
    
    # ============================================================================
    # Optimización Bayesiana
    # ============================================================================
    
    log_info("=================================================================")
    log_info("Iniciando Optimización Bayesiana")
    log_info("=================================================================")
    
    tryCatch({
      dir.create("Archivos Bayesiana", showWarnings=FALSE)
      kbayesiana <- paste0("./Archivos Bayesiana/bayesiana.RDATA")
      
      funcion_optimizar <- EstimarGanancia_AUC_lightgbm
      
      configureMlr(show.learner.output= FALSE)
      
      obj.fun <- makeSingleObjectiveFunction(
        fn= funcion_optimizar,
        minimize= FALSE,
        noisy= TRUE,
        par.set= PARAM$hypeparametertuning$hs,
        has.simple.signature= FALSE
      )
      
      ctrl <- makeMBOControl(
        save.on.disk.at.time= 600,
        save.file.path= kbayesiana
      )
      
      ctrl <- setMBOControlTermination(
        ctrl,
        iters= PARAM$hyperparametertuning$iteraciones
      )
      
      ctrl <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI())
      
      surr.km <- makeLearner(
        "regr.km",
        predict.type= "se",
        covtype= "matern3_2",
        control= list(trace= TRUE)
      )
      
      if (!file.exists(kbayesiana)) {
        log_info("Iniciando nueva optimización bayesiana...")
        bayesiana_salida <- mbo(obj.fun, learner= surr.km, control= ctrl)
      } else {
        log_info("Retomando optimización bayesiana existente...")
        bayesiana_salida <- mboContinue(kbayesiana)
      }
      
      log_info("Optimización Bayesiana completada")
      
    }, error = function(e) {
      log_error("ERROR en Optimización Bayesiana: {e$message}")
      log_error("Stack trace: {paste(capture.output(traceback()), collapse='\n')}")
      stop(e)
    })
    
    # ============================================================================
    # Procesamiento de resultados BO
    # ============================================================================
    
    tryCatch({
      tb_bayesiana <- as.data.table(bayesiana_salida$opt.path)
      log_info("Columnas en tabla bayesiana:")
      log_info(paste(colnames(tb_bayesiana), collapse=", "))
      
      tb_bayesiana[, iter := .I]
      setorder(tb_bayesiana, -y)
      
      fwrite( tb_bayesiana,
        file= "./Archivos Bayesiana/BO_log.txt",
        sep= "\t"
      )
      log_info("Log de Bayesiana guardado")
      
      PARAM$out$lgbm$mejores_hiperparametros <- tb_bayesiana[
        1,
        setdiff(colnames(tb_bayesiana),
          c("y","dob","eol","error.message","exec.time","ei","error.model",
            "train.time","prop.type","propose.time","se","mean","iter")),
        with= FALSE
      ]
      
      PARAM$out$lgbm$y <- tb_bayesiana[1, y]
      
      write_yaml(PARAM, file="./Archivos Bayesiana/PARAM.yml")
      
      log_info("Mejores hiperparámetros encontrados:")
      log_info(paste(capture.output(print(PARAM$out$lgbm$mejores_hiperparametros)), collapse="\n"))
      log_info("Mejor AUC: {PARAM$out$lgbm$y}")
      
    }, error = function(e) {
      log_error("ERROR al procesar resultados de BO: {e$message}")
      stop(e)
    })
    
    # ============================================================================
    # Entrenamiento con mejores hiperparámetros
    # ============================================================================
    
    log_info("=================================================================")
    log_info("Entrenamiento con mejores hiperparámetros")
    log_info("=================================================================")
    
    tryCatch({
      dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+1", "BAJA+2"), 1L, 0L)]
      dataset_train <- dataset[foto_mes %in% PARAM$train_final]
      
      distribucion_clases <- dataset_train[,.N,clase_ternaria]
      log_info("Distribución de clases en train_final:")
      log_info(paste(capture.output(print(distribucion_clases)), collapse="\n"))
      
      dtrain_final <- lgb.Dataset(
        data= data.matrix(dataset_train[, campos_buenos, with= FALSE]),
        label= dataset_train[, clase01]
      )
      
      param_final <- modifyList(PARAM$lgbm$param_fijos,
        PARAM$out$lgbm$mejores_hiperparametros)
      
      log_info("Parámetros finales:")
      log_info(paste(capture.output(print(param_final)), collapse="\n"))
      
      param_normalizado <- copy(param_final)
      param_normalizado$min_data_in_leaf <-  round(param_final$min_data_in_leaf / PARAM$trainingstrategy$undersampling)
      
      log_info("Entrenando modelo final...")
      modelo_final <- lgb.train(
        data= dtrain_final,
        param= param_normalizado
      )
      log_info("Modelo final entrenado exitosamente")
      
      # Importancia de variables
      tb_importancia <- as.data.table(lgb.importance(modelo_final))
      archivo_importancia <- "./Archivos Bayesiana/impo.txt"
      
      fwrite(tb_importancia,
        file= archivo_importancia,
        sep= "\t"
      )
      log_info("Importancia de variables guardada")
      
      lgb.save(modelo_final, "./Archivos Bayesiana/modelo.txt")
      log_info("Modelo guardado en disco")
      
    }, error = function(e) {
      log_error("ERROR en entrenamiento final: {e$message}")
      stop(e)
    })
    
    # ============================================================================
    # Validación final
    # ============================================================================
    
    log_info("=================================================================")
    log_info("Validación final")
    log_info("=================================================================")
    
    tryCatch({
      dfuture <- dataset[foto_mes %in% PARAM$future]
      log_info("Registros en future: {nrow(dfuture)}")
      
      prediccion <- predict(
        modelo_final,
        data.matrix(dfuture[, campos_buenos, with= FALSE])
      )
      log_info("Predicciones generadas")
      
      drealidad <- realidad_inicializar( dfuture, PARAM)
      
      tb_prediccion <- dfuture[, list(numero_de_cliente, foto_mes)]
      tb_prediccion[, prob := prediccion ]
      
      fwrite(tb_prediccion,
        file= "./Archivos Bayesiana/prediccion.txt",
        sep= "\t"
      )
      log_info("Predicciones guardadas")
      
    }, error = function(e) {
      log_error("ERROR en validación final: {e$message}")
      stop(e)
    })
    
    # ============================================================================
    # Generación de archivos Kaggle
    # ============================================================================
    
    log_info("=================================================================")
    log_info("Generando archivos para Kaggle")
    log_info("=================================================================")
    
    tryCatch({
      setorder(tb_prediccion, -prob)
      dir.create("kaggle", showWarnings=FALSE)
      resultados <- data.table()
      
      for (envios in PARAM$cortes) {
        tb_prediccion[, Predicted := 0L]
        tb_prediccion[1:envios, Predicted := 1L]
        
        archivo_kaggle <- paste0("./kaggle/", PARAM$experimento, "_", envios, ".csv")
        
        fwrite(tb_prediccion[, list(numero_de_cliente, Predicted)],
          file= archivo_kaggle,
          sep= ","
        )
        
        res <- realidad_evaluar( drealidad, tb_prediccion)
        resultados <- rbind(
          resultados,
          data.table(
            clientes = envios,
            ganancia_total = res$total,
            ganancia_public = res$public,
            ganancia_private = res$private
          )
        )
        
        options(scipen = 999)
        msg <- sprintf("Envios=%d\t TOTAL=%d  Public=%d Private=%d",
                       envios, res$total, res$public, res$private)
        cat(msg, "\n")
        log_info(msg)
      }
      
      # Gráfico de ganancias
      resultados_long <- melt(
        resultados,
        id.vars = "clientes",
        measure.vars = c("ganancia_total", "ganancia_public", "ganancia_private"),
        variable.name = "tipo",
        value.name = "ganancia"
      )
      
      maximos <- resultados_long[, .SD[which.max(ganancia)], by = tipo]
      
      etiquetas <- paste0(
        maximos$tipo,
        " (envíos = ", maximos$clientes, ", máx = ", format(maximos$ganancia, big.mark = ","), ")"
      )
      names(etiquetas) <- maximos$tipo
      
      ggplot(resultados_long, aes(x = clientes, y = ganancia, color = tipo)) +
        geom_line(linewidth = 1) +
        geom_point(data = maximos, aes(x = clientes, y = ganancia, color = tipo), size = 3) +
        labs(
          title = "Curvas de Ganancia",
          x = "Clientes",
          y = "Ganancia",
          color = "Máximos"
        ) +
        scale_color_manual(values = c("ganancia_total" = "steelblue",
                                      "ganancia_public" = "forestgreen",
                                      "ganancia_private" = "firebrick"),
                           labels = etiquetas) +
        theme_minimal() +
        theme(
          plot.margin = margin(10, 10, 10, 10),
          legend.position = "bottom")+
        guides(color = guide_legend(nrow = 3, byrow = TRUE))
      
      ggsave("curvas.png", width = 10, height = 6)
      log_info("Gráfico de curvas guardado")
      
    }, error = function(e) {
      log_error("ERROR al generar archivos Kaggle: {e$message}")
      stop(e)
    })
    
    # ============================================================================
    # Ensemble con múltiples semillas
    # ============================================================================
    
    log_info("=================================================================")
    log_info("Validación final tipo Ensemble")
    log_info("=================================================================")
    
    tryCatch({
      semillas <- c(200003, 300007, 400009, 500009, 600011)
      log_info("Semillas para ensemble: {paste(semillas, collapse=', ')}")
      
      lista_predicciones <- list()
      
      dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+1", "BAJA+2"), 1L, 0L)]
      dataset_train <- dataset[foto_mes %in% PARAM$train_final]
      
      dtrain_final <- lgb.Dataset(
        data = data.matrix(dataset_train[, campos_buenos, with = FALSE]),
        label = dataset_train[, clase01]
      )
      
      param_final <- modifyList(PARAM$lgbm$param_fijos,
                                PARAM$out$lgbm$mejores_hiperparametros)
      
      param_normalizado <- copy(param_final)
      param_normalizado$min_data_in_leaf <- round(param_final$min_data_in_leaf / PARAM$trainingstrategy$undersampling)
      
      dfuture <- dataset[foto_mes %in% PARAM$future]
      
      log_info("Iniciando entrenamiento del ensamble con {length(semillas)} semillas...")
      
      for (semilla_actual in semillas) {
        tryCatch({
          log_info("Entrenando modelo con semilla: {semilla_actual}")
          
          param_normalizado$seed <- semilla_actual
          
          modelo_final <- lgb.train(
            data = dtrain_final,
            param = param_normalizado
          )
          
          prediccion_individual <- predict(
            modelo_final,
            data.matrix(dfuture[, campos_buenos, with = FALSE])
          )
          
          tb_prediccion_individual <- dfuture[, list(numero_de_cliente, foto_mes)] 
          tb_prediccion_individual[, prob := prediccion_individual]
          
          lista_predicciones[[as.character(semilla_actual)]] <- tb_prediccion_individual
          log_info("Modelo con semilla {semilla_actual} completado")
          
        }, error = function(e) {
          log_error("ERROR al entrenar modelo con semilla {semilla_actual}: {e$message}")
          # Continuar con la siguiente semilla
        })
      }
      
      if (length(lista_predicciones) == 0) {
        stop("No se pudo entrenar ningún modelo del ensemble")
      }
      
      log_info("Todas las semillas procesadas. Creando el ensamble final...")
      
      predicciones_todas <- rbindlist(lista_predicciones)
      tb_prediccion <- predicciones_todas[, .(prob = mean(prob)), by = .(numero_de_cliente, foto_mes)]
      
      log_info("Ensamble creado con éxito. Probabilidades promediadas.")
      
      drealidad <- realidad_inicializar(dfuture, PARAM)
      setorder(tb_prediccion, -prob)
      
      dir.create("kaggle_promedidado", showWarnings=FALSE)
      resultados <- data.table()
      
      for (envios in PARAM$cortes) {
        tb_prediccion[, Predicted := 0L]
        tb_prediccion[1:envios, Predicted := 1L]
        
        archivo_kaggle <- paste0("./kaggle_promedidado/", PARAM$experimento, "_", envios, ".csv")
        
        fwrite(tb_prediccion[, list(numero_de_cliente, Predicted)],
               file = archivo_kaggle,
               sep = ","
        )
        
        res <- realidad_evaluar(drealidad, tb_prediccion)
        resultados <- rbind(
          resultados,
          data.table(
            clientes = envios,
            ganancia_total = res$total,
            ganancia_public = res$public,
            ganancia_private = res$private
          )
        )
        
        options(scipen = 999)
        msg <- sprintf("Envios=%d\t TOTAL=%d  Public=%d  Private=%d",
                       envios, res$total, res$public, res$private)
        cat(msg, "\n")
        log_info(msg)
      }
      
      # Gráfico ensemble
      resultados_long <- melt(
        resultados,
        id.vars = "clientes",
        measure.vars = c("ganancia_total", "ganancia_public", "ganancia_private"),
        variable.name = "tipo",
        value.name = "ganancia"
      )
      
      maximos <- resultados_long[, .SD[which.max(ganancia)], by = tipo]
      
      etiquetas <- paste0(
        maximos$tipo,
        " (envíos = ", maximos$clientes, ", máx = ", format(maximos$ganancia, big.mark = ","), ")"
      )
      names(etiquetas) <- maximos$tipo
      
      ggplot(resultados_long, aes(x = clientes, y = ganancia, color = tipo)) +
        geom_line(linewidth  = 1) +
        geom_point(data = maximos, aes(x = clientes, y = ganancia, color = tipo), size  = 3) +
        labs(
          title = "Curvas de Ganancia",
          x = "Clientes",
          y = "Ganancia",
          color = "Máximos"
        ) +
        scale_color_manual(values = c("ganancia_total" = "steelblue",
                                      "ganancia_public" = "forestgreen",
                                      "ganancia_private" = "firebrick"),
                           labels = etiquetas) +
        theme_minimal() +
        theme(
          plot.margin = margin(10, 10, 10, 10),
          legend.position = "bottom")+
        guides(color = guide_legend(nrow = 3, byrow = TRUE))
      
      ggsave("curvas_ensamble.png", width = 10, height = 6)
      log_info("Gráfico de curvas ensemble guardado")
      
    }, error = function(e) {
      log_error("ERROR en ensemble: {e$message}")
      stop(e)
    })
    
    # ============================================================================
    # Finalización
    # ============================================================================
    
    tiempo_final <- format(Sys.time(), "%a %b %d %X %Y")
    log_info("=================================================================")
    log_info("Experimento finalizado EXITOSAMENTE: {tiempo_final}")
    log_info("=================================================================")
    
    return(invisible(TRUE))
    
  }, error = function(e) {
    # Captura de error global
    log_error("=================================================================")
    log_error("ERROR CRÍTICO EN LA EJECUCIÓN DEL SCRIPT")
    log_error("=================================================================")
    log_error("Mensaje de error: {e$message}")
    log_error("Hora del error: {format(Sys.time(), '%a %b %d %X %Y')}")
    
    # Intentar obtener más información del error
    tryCatch({
      log_error("Stack trace completo:")
      stack_trace <- capture.output(traceback())
      log_error(paste(stack_trace, collapse="\n"))
    }, error = function(e2) {
      log_error("No se pudo obtener el stack trace")
    })
    
    # Información del sistema
    tryCatch({
      log_error("Información del sistema:")
      log_error("  R version: {R.version.string}")
      log_error("  Platform: {R.version$platform}")
      log_error("  Working directory: {getwd()}")
      log_error("  Available memory: {gc()[2,2]} MB")
    }, error = function(e2) {
      log_error("No se pudo obtener información del sistema")
    })
    
    log_error("=================================================================")
    log_error("El script ha terminado con ERRORES. Revise el log completo.")
    log_error("=================================================================")
    
    # Terminar con código de error
    quit(status = 1, save = "no")
  }, warning = function(w) {
    # Captura de warnings
    log_warn("WARNING: {w$message}")
  })
}

# ============================================================================
# Ejecutar función principal
# ============================================================================

cat("Iniciando script...\n")
cat("Los logs se guardarán en el archivo de log correspondiente.\n")
cat("====================================================================\n\n")

# Ejecutar la función principal
main()

cat("\n====================================================================\n")
cat("Script finalizado. Revise el archivo de log para más detalles.\n")