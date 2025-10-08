#!/usr/bin/env Rscript

# Sección 1: Carga de Librerías
#------------------------------------------------------
if (!require("logger")) install.packages("logger")
library("logger")

suppressPackageStartupMessages({
  if (!require("data.table")) install.packages("data.table")
  library("data.table")
  if (!require("parallel")) install.packages("parallel")
  library("parallel")
  if (!require("R.utils")) install.packages("R.utils")
  library("R.utils")
  if (!require("primes")) install.packages("primes")
  library("primes")
  if (!require("utils")) install.packages("utils")
  library("utils")
  if (!require("rlist")) install.packages("rlist")
  library("rlist")
  if (!require("yaml")) install.packages("yaml")
  library("yaml")
  if (!require("lightgbm")) install.packages("lightgbm")
  library("lightgbm")
  if (!require("DiceKriging")) install.packages("DiceKriging")
  library("DiceKriging")
  if (!require("mlrMBO")) install.packages("mlrMBO")
  library("mlrMBO")
  if (!require("ggplot2")) install.packages("ggplot2")
  library("ggplot2")
})

# Sección 2: Configuración Inicial y Parámetros
#------------------------------------------------------
rm(list = ls(all.names = TRUE))
gc(full = TRUE, verbose = FALSE)

PARAM <- list()
PARAM$experimento <- "expC01_Prueba03"
PARAM$dir_experimento <- paste0("~/buckets/b1/exp/", PARAM$experimento)
PARAM$dir_dataset <- "~/buckets/b1/datasets/"
PARAM$carpeta_logs <- "logs/"
PARAM$carpeta_bayesiana <- "Archivos Bayesiana/"
PARAM$carpeta_kaggle <- "Kaggle/"
PARAM$carpeta_kaggle_ensamble <- "Kaggle_Promediado/"
PARAM$carpeta_graficos <- "Plots/"
PARAM$carpeta_entregables <- "Entregables/"

PARAM$semillas_primigenias <- c(200003, 300007, 400009, 500009, 600011)
PARAM$train <- c(202101, 202102)
PARAM$train_final <- c(202101, 202102)
PARAM$future <- c(202104)
PARAM$train_final_kaggle <- c(202101, 202102, 202103, 202104)
PARAM$entrega_kaggle <- c(202106)
PARAM$semilla_kaggle <- 314159
PARAM$cortes <- seq(0, 20000, by = 100)
PARAM$trainingstrategy$undersampling <- 0.5
PARAM$hyperparametertuning$xval_folds <- 5
PARAM$lgbm$param_fijos <- list(
  boosting = "gbdt",
  objective = "binary",
  metric = "auc",
  first_metric_only = FALSE,
  boost_from_average = TRUE,
  feature_pre_filter = FALSE,
  force_row_wise = TRUE,
  verbosity = -100,
  seed = 1,
  max_depth = -1L,
  min_gain_to_split = 0,
  min_sum_hessian_in_leaf = 0.001,
  lambda_l1 = 0.0,
  lambda_l2 = 0.0,
  max_bin = 31L,
  bagging_fraction = 1.0,
  pos_bagging_fraction = 1.0,
  neg_bagging_fraction = 1.0,
  is_unbalance = FALSE,
  scale_pos_weight = 1.0,
  drop_rate = 0.1,
  max_drop = 50,
  skip_drop = 0.5,
  extra_trees = FALSE,
  num_iterations = 1200,
  learning_rate = 0.02,
  feature_fraction = 0.5,
  num_leaves = 750,
  min_data_in_leaf = 5000
)
# Bordes de hiperparámetros para BO
PARAM$hypeparametertuning$hs <- makeParamSet(
  makeIntegerParam("num_iterations", lower= 50L, upper= 3000L),
  makeNumericParam("learning_rate", lower= 0.01, upper= 0.3),
  makeNumericParam("feature_fraction", lower= 0.1, upper= 1.0),
  makeIntegerParam("num_leaves", lower= 10L, upper= 2048L),
  makeIntegerParam("min_data_in_leaf", lower= 10L, upper= 8000L)
)
PARAM$hyperparametertuning$iteraciones <- 100

# ----- Configuración del Logger con Ruta Absoluta -----
# Creo la carpeta del experimento
dir.create(PARAM$dir_experimento,
           showWarnings = FALSE,
           recursive = TRUE)
setwd(PARAM$dir_experimento)
# Creo la carpeta para los logs
dir.create(PARAM$carpeta_logs,
           showWarnings = FALSE,
           recursive = TRUE)
# Definir la ruta del archivo log
log_file <- file.path(PARAM$carpeta_logs, paste0("log_", PARAM$experimento, ".log"))
# Configurar el logger para que escriba en consola y en la ruta absoluta del archivo
log_appender(appender_tee(log_file))

log_info("------------------------------------------------------")
log_info(paste("Inicio del script. Experimento:", PARAM$experimento))
log_info(paste("El log se guardará en:", log_file))
log_info("------------------------------------------------------")

# Sección 3: Funciones Auxiliares
#------------------------------------------------------
particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed, "L'Ecuyer-CMRG")
  bloque <- unlist(mapply(function(x, y) { rep(y, x) }, division, seq(from = start, length.out = length(division))))
  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N], by = agrupa]
}

realidad_inicializar <- function(pfuture, pparam) {
  drealidad <- pfuture[, list(numero_de_cliente, foto_mes, clase_ternaria)]
  particionar(drealidad, division = c(3, 7), agrupa = "clase_ternaria", seed = pparam$semilla_kaggle)
  return(drealidad)
}

realidad_evaluar <- function(prealidad, pprediccion) {
  prealidad[pprediccion, on = c("numero_de_cliente", "foto_mes"), predicted := i.Predicted]
  tbl <- prealidad[, list("qty" = .N), list(fold, predicted, clase_ternaria)]
  res <- list()
  res$public <- tbl[fold == 1 & predicted == 1L, sum(qty * ifelse(clase_ternaria == "BAJA+2", 780000, -20000))] / 0.3
  res$private <- tbl[fold == 2 & predicted == 1L, sum(qty * ifelse(clase_ternaria == "BAJA+2", 780000, -20000))] / 0.7
  res$total <- tbl[predicted == 1L, sum(qty * ifelse(clase_ternaria == "BAJA+2", 780000, -20000))]
  prealidad[, predicted := NULL]
  return(res)
}

#------------------------------------------------------
tryCatch({
  
    # Sección 4: Preparación de Datos
    #------------------------------------------------------
    log_info("Iniciando Sección 4: Preparación de Datos.")
    log_info(paste("Leyendo dataset desde:", PARAM$dir_dataset))
    dataset <- fread(file.path(PARAM$dir_dataset, "competencia_01.csv.gz"),
                    stringsAsFactors = TRUE)
    log_info("Dataset cargado correctamente.")

    cols_a_excluir <- c("numero_de_cliente", "foto_mes", "clase_ternaria")
    cols_con_lag <- setdiff(names(dataset), cols_a_excluir)
    nombres_nuevas_cols_lag <- paste0(cols_con_lag, "_lag1")
    dataset[, (nombres_nuevas_cols_lag) := shift(.SD, 1, NA, "lag"), by = numero_de_cliente, .SDcols = cols_con_lag]
    nombres_nuevas_cols_delta <- paste0(cols_con_lag, "_delta1")
    dataset[, (nombres_nuevas_cols_delta) := .SD - mget(nombres_nuevas_cols_lag), .SDcols = cols_con_lag]
    log_info("Features de lag y delta generadas.")

    dataset_train <- dataset[foto_mes %in% PARAM$train]
    dataset_train[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]
    log_info("Clase convertida a formato binario.")

    set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")
    dataset_train[, azar := runif(nrow(dataset_train))]
    dataset_train[, training := 0L]
    dataset_train[foto_mes %in% PARAM$train & (azar <= PARAM$trainingstrategy$undersampling | clase_ternaria %in% c("BAJA+1", "BAJA+2")), training := 1L]
    log_info(paste("Undersampling aplicado con una tasa de:", PARAM$trainingstrategy$undersampling))

    campos_buenos <- setdiff(colnames(dataset_train), c("clase_ternaria", "clase01", "azar", "training"))
    dtrain <- lgb.Dataset(
    data = data.matrix(dataset_train[training == 1L, campos_buenos, with = FALSE]),
    label = dataset_train[training == 1L, clase01],
    free_raw_data = FALSE
    )
    log_info("Dataset de entrenamiento para LightGBM creado.")
    log_info(paste("Dimensiones de dtrain -> Filas:", nrow(dtrain), "| Columnas:", ncol(dtrain)))

    # Sección 5: Optimización Bayesiana
    #------------------------------------------------------
    log_info("Iniciando Sección 5: Optimización Bayesiana de Hiperparámetros.")
    EstimarGanancia_AUC_lightgbm <- function(x) {
        param_completo <- modifyList(PARAM$lgbm$param_fijos, x)
        modelocv <- lgb.cv(data = dtrain, nfold = PARAM$hyperparametertuning$xval_folds, stratified = TRUE, param = param_completo)
        AUC <- modelocv$best_score
        rm(modelocv)
        gc(full = TRUE, verbose = FALSE)
        log_info(paste("Iteración BO -> AUC:", format(AUC, digits = 6), "|", format(Sys.time(), "%a %b %d %X %Y")))
        return(AUC)
    }

    # Lista para almacenar la tabla de predicción de cada modelo
    predicciones_ensemble <- list()

    dir.create(PARAM$carpeta_bayesiana, showWarnings=FALSE)
    # Recorro cada semilla
    for (semilla_actual in PARAM$semillas_primigenias) {
        log_info("---------------------------------------------------")
        log_info(paste0("Procesando semilla: ", semilla_actual))
        log_info("---------------------------------------------------\n")

        # Asigno la semilla actual al parámetro que usa el resto del script
        PARAM$semilla_primigenia <- semilla_actual
        PARAM$lgbm$param_fijos$seed <- semilla_actual
        
        #--- Preparación de datos (undersampling) con la semilla actual ---
        dataset_train <- dataset[foto_mes %in% PARAM$train]

        dataset_train[,
            clase01 := ifelse(clase_ternaria %in% c("BAJA+2","BAJA+1"), 1L, 0L)
        ]
        
        set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")
        dataset_train[, azar := runif(nrow(dataset_train))]
        dataset_train[, training := 0L]
        dataset_train[foto_mes %in% PARAM$train & (azar <= PARAM$trainingstrategy$undersampling | clase_ternaria %in% c("BAJA+1", "BAJA+2")),
                        training := 1L]
        
        campos_buenos <- setdiff(colnames(dataset_train), c("clase_ternaria", "clase01", "azar", "training"))
        
        dtrain <- lgb.Dataset(data= data.matrix(dataset_train[training == 1L, campos_buenos, with= FALSE]),
                                label= dataset_train[training == 1L, clase01], free_raw_data= FALSE)
        
        #--- Optimización Bayesiana ---
        kbayesiana <- paste0(PARAM$carpeta_bayesiana,"bayesiana_", semilla_actual, ".RDATA")
        funcion_optimizar <- EstimarGanancia_AUC_lightgbm # la funcion que voy a maximizar

        configureMlr(show.learner.output= FALSE)
        obj.fun <- makeSingleObjectiveFunction(fn= funcion_optimizar, minimize= FALSE, noisy= TRUE,
                                                par.set= PARAM$hypeparametertuning$hs, has.simple.signature= FALSE)
        ctrl <- makeMBOControl(save.on.disk.at.time= 600, save.file.path= kbayesiana)
        ctrl <- setMBOControlTermination(ctrl, iters= PARAM$hyperparametertuning$iteraciones)
        ctrl <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI())
        surr.km <- makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))

        if (!file.exists(kbayesiana)) {
            log_info(paste("Iniciando nueva búsqueda Bayesiana de ", PARAM$hyperparametertuning$iteraciones, " iteraciones."))
            bayesiana_salida <- mbo(obj.fun, learner= surr.km, control= ctrl)
        } else {
            log_info("Continuando búsqueda Bayesiana desde archivo existente.")
            bayesiana_salida <- mboContinue(kbayesiana)
        }

        log_info("Optimización Bayesiana finalizada.")
      
        tb_bayesiana <- as.data.table(bayesiana_salida$opt.path)
        tb_bayesiana[, iter := .I]
        setorder(tb_bayesiana, -y)
        
        fwrite(tb_bayesiana, file= paste0(PARAM$carpeta_bayesiana,"BO_log_", semilla_actual, ".txt"), sep= "\t")
        
        mejores_hiperparametros <- tb_bayesiana[1, setdiff(colnames(tb_bayesiana),
            c("y","dob","eol","error.message","exec.time","ei","error.model", "train.time",
            "prop.type","propose.time","se","mean","iter")), with= FALSE]

        PARAM$out$lgbm$mejores_hiperparametros <- mejores_hiperparametros
        PARAM$out$lgbm$y <- tb_bayesiana[1, y]
        write_yaml(PARAM, file= paste0(PARAM$carpeta_bayesiana,"PARAM_", semilla_actual, ".yml"))

        log_info("Mejores hiperparámetros encontrados:")
        log_info(paste(capture.output(print(PARAM$out$lgbm$mejores_hiperparametros)), collapse = "\n"))
        log_info(paste("Mejor AUC (y):", PARAM$out$lgbm$y))

        #--- Entrenamiento del modelo final para la semilla actual ---
        log_info(paste0("Iniciando entrenamiento del modelo final para la semilla ", semilla_actual, "."))
        dataset_train_final <- dataset[foto_mes %in% PARAM$train_final]
        dataset_train_final[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]
        dtrain_final <- lgb.Dataset(
            data = data.matrix(dataset_train_final[, campos_buenos, with=FALSE]),
            label = dataset_train_final[, clase01]
        )
        
        param_final <- modifyList(PARAM$lgbm$param_fijos, mejores_hiperparametros)
        param_normalizado <- copy(param_final)
        param_normalizado$min_data_in_leaf <- round(param_final$min_data_in_leaf / PARAM$trainingstrategy$undersampling)
        
        modelo_final <- lgb.train(data= dtrain_final, param= param_normalizado)
        
        tb_importancia <- as.data.table(lgb.importance(modelo_final))
        fwrite(tb_importancia, file= paste0(PARAM$carpeta_bayesiana,"impo_", semilla_actual, ".txt"), sep= "\t")
        lgb.save(modelo_final, paste0(PARAM$carpeta_bayesiana,"modelo_", semilla_actual, ".txt"))
        
        #--- Predicción y guardado para el ensemble ---
        dfuture <- dataset[foto_mes %in% PARAM$future]
        prediccion <- predict(modelo_final, data.matrix(dfuture[, campos_buenos, with= FALSE]))

        tb_prediccion_individual <- dfuture[, list(numero_de_cliente, foto_mes)]
        tb_prediccion_individual[, prob := prediccion]

        fwrite(tb_prediccion_individual, file= paste0(PARAM$carpeta_bayesiana,"prediccion_", semilla_actual, ".txt"), sep= "\t")
        predicciones_ensemble[[as.character(semilla_actual)]] <- tb_prediccion_individual

        log_info(paste0("Modelo con semilla ", semilla_actual, " entrenado y predicción guardada."))
    }

    # Sección 6: Entrenamiento y Predicción (Modelo Único)
    #------------------------------------------------------
    log_info("Creando el ensamble final.")
    # Combino todas las tablas de predicción en una sola
    tb_ensemble_completa <- rbindlist(predicciones_ensemble)

    # Calculo el promedio de las probabilidades para cada cliente y mes
    tb_prediccion <- tb_ensemble_completa[, .(prob = mean(prob)), by = .(numero_de_cliente, foto_mes)]

    # Guardo la predicción final del ensemble
    fwrite(tb_prediccion, file= paste0(PARAM$carpeta_bayesiana,"prediccion_ensemble.txt"), sep= "\t")

    log_info(paste0("Ensemble creado promediando las predicciones de los ", length(PARAM$semillas_primigenias), " modelos."))

    log_info("Generando predicciones y envíos para Kaggle...")
    drealidad <- realidad_inicializar(dataset[foto_mes %in% PARAM$future], PARAM)

    setorder(tb_prediccion, -prob)
    dir.create("kaggle", showWarnings = FALSE)
    resultados <- data.table()

    for (envios in PARAM$cortes) {
        tb_prediccion[, Predicted := 0L]
        tb_prediccion[1:envios, Predicted := 1L]

        archivo_kaggle <- paste0("./kaggle/", PARAM$experimento, "_ensemble_", envios, ".csv")
        fwrite(tb_prediccion[, list(numero_de_cliente, Predicted)], file= archivo_kaggle, sep= ",")
        
        res <- realidad_evaluar(drealidad, tb_prediccion)
        resultados <- rbind(resultados, data.table(
            clientes = envios, ganancia_total = res$total,
            ganancia_public = res$public, ganancia_private = res$private))
        
        options(scipen = 999)
        cat("Envios=", envios, "\t", " TOTAL=", format(res$total, big.mark=","), "  Public=", format(res$public, big.mark=","),
            " Private=", format(res$private, big.mark=","), "\n", sep= "")
    }

    max_ganancia_valor <- max(resultados$ganancia_total)
    envios_max_total <- resultados[ganancia_total == max_ganancia_valor, clientes]

    # pasar a formato largo
    resultados_long <- melt(
        resultados,
        id.vars = "clientes",
        measure.vars = c("ganancia_total", "ganancia_public", "ganancia_private"),
        variable.name = "tipo",
        value.name = "ganancia"
    )

    # calcular máximos por tipo
    maximos <- resultados_long[, .SD[which.max(ganancia)], by = tipo]

    # etiquetas personalizadas
    etiquetas <- paste0(
        maximos$tipo,
        " (envíos = ", maximos$clientes, ", máx = ", format(maximos$ganancia, big.mark = ","), ")"
    )
    names(etiquetas) <- maximos$tipo

    # gráfico
    p <- ggplot(resultados_long, aes(x = clientes, y = ganancia, color = tipo)) +
        geom_line(linewidth = 1) +
        geom_point(data = maximos, aes(x = clientes, y = ganancia, color = tipo), size = 3) +
        labs(
            title = paste0("Curvas de Ganancia (", PARAM$experimento, ")"),
            x = "Clientes",
            y = "Ganancia",
            color = "Máximos"
        ) +
        scale_color_manual(
            values = c(
            "ganancia_total" = "steelblue",
            "ganancia_public" = "forestgreen",
            "ganancia_private" = "firebrick"
            ),
            labels = etiquetas
        ) +
        theme_minimal() +
        theme(
            plot.margin = margin(10, 10, 10, 10),
            legend.position = "bottom"
        ) +
        guides(color = guide_legend(nrow = 3, byrow = TRUE))

    # guardar imagen
    ggsave(paste0("curvas_", PARAM$experimento, ".png"), plot = p, width = 10, height = 6)

    log_info("Gráfico de curvas de ganancia guardado.")

    # Sección 7: Generación de Entregables para Kaggle
    #------------------------------------------------------
    log_info("Iniciando Sección 7: Generación de Entregables para Kaggle.")
    
    # 1. Preparar los datasets finales para re-entrenamiento y predicción
    # Se usarán todos los datos disponibles para entrenar los modelos finales
    dataset_train_final_kaggle <- dataset[foto_mes %in% PARAM$train_final_kaggle]
    dataset_train_final_kaggle[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]
    
    # El dataset sobre el que se va a predecir para la entrega final
    dfuture_entrega <- dataset[foto_mes %in% PARAM$entrega_kaggle]
    
    log_info("Datasets finales de entrenamiento y entrega preparados.")
    
    # 2. Crear el lgb.Dataset final una sola vez para eficiencia
    dtrain_final_kaggle <- lgb.Dataset(
      data = data.matrix(dataset_train_final_kaggle[, campos_buenos, with = FALSE]),
      label = dataset_train_final_kaggle[, clase01]
    )
    
    # 3. Bucle para re-entrenar cada modelo con sus HP óptimos y predecir
    lista_predicciones_final <- list()
    
    for (semilla_actual in PARAM$semillas_primigenias) {
      log_info(paste0("Re-entrenando modelo final para semilla: ", semilla_actual))
      
      # Cargar los mejores hiperparámetros específicos para esta semilla
      param_semilla <- read_yaml(paste0(PARAM$carpeta_bayesiana,"PARAM_", semilla_actual, ".yml"))
      mejores_hp_semilla <- param_semilla$out$lgbm$mejores_hiperparametros
      
      # Combinar parámetros fijos con los optimizados para esta semilla
      param_final_kaggle <- modifyList(PARAM$lgbm$param_fijos, mejores_hp_semilla)
      param_final_kaggle$seed <- semilla_actual # Asegurar la semilla correcta
      
      # Normalizar min_data_in_leaf
      param_normalizado_kaggle <- copy(param_final_kaggle)
      param_normalizado_kaggle$min_data_in_leaf <- round(
        param_final_kaggle$min_data_in_leaf / PARAM$trainingstrategy$undersampling
      )
      
      # Entrenar el modelo final
      modelo_kaggle <- lgb.train(data = dtrain_final_kaggle, param = param_normalizado_kaggle)
      
      # Predecir sobre el dataset de entrega
      prediccion_kaggle <- predict(modelo_kaggle, data.matrix(dfuture_entrega[, campos_buenos, with = FALSE]))
      
      # Guardar la predicción en una tabla temporal
      tb_pred_final <- dfuture_entrega[, list(numero_de_cliente, foto_mes)]
      tb_pred_final[, prob := prediccion_kaggle]
      lista_predicciones_final[[as.character(semilla_actual)]] <- tb_pred_final
    }
    
    # 4. Crear el ensemble final promediando las predicciones de entrega
    log_info("Promediando predicciones finales para el ensemble de entrega.")
    predicciones_finales_todas <- rbindlist(lista_predicciones_final)
    tb_prediccion_final_ensamble <- predicciones_finales_todas[, .(prob = mean(prob)), by = .(numero_de_cliente, foto_mes)]
    
    # 5. Generar los archivos de entrega usando los envíos óptimos ya calculados
    log_info(paste0("Generando archivos de entrega para los envíos óptimos: ", paste(envios_max_total, collapse = ", ")))
    
    dir.create(PARAM$carpeta_entregables, showWarnings = FALSE)
    setorder(tb_prediccion_final_ensamble, -prob)
    
    for (envios in envios_max_total) {
      tb_prediccion_final_ensamble[, Predicted := 0L]
      tb_prediccion_final_ensamble[1:envios, Predicted := 1L]
      
      archivo_entrega <- paste0(PARAM$carpeta_entregables, PARAM$experimento, "_", envios, ".csv")
      
      fwrite(tb_prediccion_final_ensamble[, list(numero_de_cliente, Predicted)],
             file = archivo_entrega,
             sep = ",")
             
      log_info(paste0("Archivo de entrega generado: ", archivo_entrega))
    }


}, error = function(e) {
    # Mensaje de error mejorado
    log_error("######################################################")
    log_error("Se ha producido un error fatal en la ejecución.")
    log_error("Revisa el último mensaje 'INFO' en el log para identificar la sección donde ocurrió el fallo.")
    log_error(paste("Mensaje de R:", e$message))
    log_error("######################################################")
    quit(status = 1) # Detiene el script con un código de error
})

# Sección 8: Finalización
#------------------------------------------------------
log_info("------------------------------------------------------")
log_info("Script finalizado exitosamente.")
log_info(paste("Fecha y hora de finalización:", format(Sys.time(), "%a %b %d %X %Y")))
log_info("------------------------------------------------------")