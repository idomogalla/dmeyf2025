tryCatch(
    {
        log_info("--- Iniciando 9_Evaluacion_APO.R ---")

        # Directorios
        dir_evaluacion <- file.path(PARAM$experimento_folder, PARAM$carpeta_evaluacion)
        dir_modelos <- file.path(PARAM$experimento_folder, PARAM$modelos_folder)
        dir.create(dir_modelos, recursive = TRUE, showWarnings = FALSE)

        # Cargar semillas evaluadas en el paso anterior
        ruta_semillas <- file.path(dir_evaluacion, "semillas_evaluadas.rds")
        if (!file.exists(ruta_semillas)) {
            stop("No se encontró el archivo 'semillas_evaluadas.rds'. Ejecute 8_Evaluacion.R primero.")
        }
        semillas <- readRDS(ruta_semillas)
        log_info(paste("Se encontraron", length(semillas), "semillas para evaluar."))

        # Verificar modelos faltantes
        modelos_faltantes <- c()
        for (s in semillas) {
            if (!file.exists(file.path(dir_modelos, paste0("mod_", s, ".txt")))) {
                modelos_faltantes <- c(modelos_faltantes, s)
            }
        }

        # --- Bloque de Entrenamiento (Redundancia) ---
        if (length(modelos_faltantes) > 0) {
            log_info(paste("Faltan", length(modelos_faltantes), "modelos. Iniciando entrenamiento de redundancia..."))

            # Cargar Hiperparámetros (Lógica copiada de 8_Evaluacion.R)
            log_info("Cargando mejores hiperparámetros de BO_log.txt")
            dir_bayesiana <- file.path(PARAM$experimento_folder, PARAM$carpeta_bayesiana)
            log_bo_file <- file.path(dir_bayesiana, "BO_log.txt")

            if (!file.exists(log_bo_file)) {
                log_bo_file <- file.path(PARAM$experimento_folder, "BO_log.txt")
                if (!file.exists(log_bo_file)) {
                    stop("No se encontró el archivo BO_log.txt.")
                }
            }

            tb_BO <- fread(log_bo_file)
            setorder(tb_BO, -metrica)

            nombres_hiper_optimizados <- c("num_iterations", "learning_rate", "feature_fraction", "min_data_in_leaf", "num_leaves")
            param_lgbm <- union(names(PARAM$lgbm$param_fijos), nombres_hiper_optimizados)
            param_mejores <- as.list(tb_BO[1, param_lgbm, with = FALSE])

            # Preparar Datos de Entrenamiento
            log_info("Filtrando datos para el modelo final (PARAM$train_final$training).")
            dataset_train_final <- dataset[foto_mes %in% PARAM$train_final$training]

            log_info("Haciendo undersampling para el modelo final.")
            set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")
            dataset_train_final[, azar := runif(nrow(dataset_train_final))]
            dataset_train_final[, training := 0L]
            dataset_train_final[(azar <= PARAM$train_final$undersampling | clase_ternaria %in% c("BAJA+1", "BAJA+2")), training := 1L]
            dataset_train_final[, azar := NULL]
            dataset_train_final[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

            # Escalar Hiperparámetros
            if (!exists("dtrain") || is.null(dtrain)) {
                nrow_dtrain_path <- file.path(PARAM$experimento_folder, PARAM$carpeta_bayesiana, "nrow_dtrain.rds")
                if (file.exists(nrow_dtrain_path)) {
                    nrow_dtrain_bayesiana <- readRDS(nrow_dtrain_path)
                } else {
                    nrow_dtrain_bayesiana <- nrow(dataset_train_final[training == 1L]) # Fallback arriesgado pero necesario si falta el archivo
                    log_warn("No se encontró nrow_dtrain.rds, usando tamaño actual (ratio=1).")
                }
            } else {
                nrow_dtrain_bayesiana <- nrow(dtrain)
            }

            ratio <- nrow(dataset_train_final[training == 1L]) / nrow_dtrain_bayesiana
            param_mejores$min_data_in_leaf <- as.integer(round(param_mejores$min_data_in_leaf * ratio))

            # Crear lgb.Dataset
            campos_buenos <- setdiff(colnames(dataset_train_final), PARAM$trainingstrategy$campos_entrenar)
            dtrain <- lgb.Dataset(
                data = data.matrix(dataset_train_final[training == 1L, campos_buenos, with = FALSE]),
                label = dataset_train_final[training == 1L, clase01],
                free_raw_data = TRUE
            )
            rm(dataset_train_final)
            gc()

            # Entrenar Modelos Faltantes
            param_entrenamiento <- copy(param_mejores)
            for (semilla_actual in modelos_faltantes) {
                log_info(paste0("Entrenando modelo faltante para semilla: ", semilla_actual))
                param_entrenamiento$seed <- semilla_actual
                modelo <- lgb.train(data = dtrain, param = param_entrenamiento)

                ruta_modelo <- file.path(dir_modelos, paste0("mod_", semilla_actual, ".txt"))
                lgb.save(modelo, filename = ruta_modelo)
                log_info(paste("Modelo guardado en:", ruta_modelo))
                rm(modelo)
                gc()
            }
            rm(dtrain)
            gc()
        } else {
            log_info("Todos los modelos necesarios ya existen.")
        }

        # --- Bloque de Evaluación APO ---
        log_info("Iniciando Evaluación Comparativa: Estrategia APO")

        # Preparar datos de 'future'
        log_info("Preparando datos de 'future' para la evaluación APO.")
        dfuture_apo <- dataset[foto_mes %in% PARAM$train_final$future, list(numero_de_cliente, foto_mes, clase_ternaria)]
        dfuture_apo[, ganancia := ifelse(clase_ternaria == "BAJA+2", 780000, -20000)]

        # Preparar matriz de features para predicción
        campos_buenos <- setdiff(colnames(dataset), c(PARAM$trainingstrategy$campos_entrenar, "clase_ternaria", "clase01", "training", "azar"))

        mfuture <- data.matrix(dataset[foto_mes %in% PARAM$train_final$future, campos_buenos, with = FALSE])

        # Definir cortes fijos (APO)
        cortes_fijos_apo <- c(10000, 10500, 11000, 11500, 12000)

        mganancias <- matrix(nrow = PARAM$train_final$iter, ncol = length(cortes_fijos_apo))

        # Directorio de entregables
        dir_kaggle <- file.path(PARAM$experimento_folder, PARAM$carpeta_entregables)
        dir.create(dir_kaggle, showWarnings = FALSE)

        # Archivo de predicciones APO
        ruta_prediccion_apo <- file.path(dir_evaluacion, "prediccion_APO.txt")
        if (file.exists(ruta_prediccion_apo)) file.remove(ruta_prediccion_apo)

        # Bucle de Meta-Modelos (APO)
        # semillas es el vector de todas las semillas evaluadas
        # PARAM$train_final$ksemillerio es el tamaño del semillerio
        # PARAM$train_final$iter es la cantidad de iteraciones (meta-modelos)
        for (vapo in seq(PARAM$train_final$iter)) {
            desde <- 1 + (vapo - 1) * PARAM$train_final$ksemillerio
            hasta <- desde + PARAM$train_final$ksemillerio - 1

            if (hasta > length(semillas)) {
                log_warn(paste("No hay suficientes semillas para la iteración APO", vapo))
                next
            }

            semillas_subset <- semillas[desde:hasta]

            log_info(paste0("--- Procesando Meta-Modelo APO: ", vapo, " (semillas ", desde, " a ", hasta, ") ---"))

            # Acumulador de probabilidades
            prob_acumulada <- rep(0, nrow(mfuture))

            for (s in semillas_subset) {
                ruta_modelo <- file.path(dir_modelos, paste0("mod_", s, ".txt"))
                modelo <- lgb.load(ruta_modelo)
                pred <- predict(modelo, mfuture)
                prob_acumulada <- prob_acumulada + pred
                rm(modelo)
            }

            prob_promedio <- prob_acumulada / length(semillas_subset)

            # Crear tabla de evaluación local
            tb_eval_apo <- copy(dfuture_apo)
            tb_eval_apo[, prob := prob_promedio]
            setorder(tb_eval_apo, -prob)
            tb_eval_apo[, gan_acum := cumsum(ganancia)]
            tb_eval_apo[, meta_modelo := vapo]

            # Acumular ganancias en cortes fijos
            for (icor in seq_along(cortes_fijos_apo)) {
                mganancias[vapo, icor] <- tb_eval_apo[cortes_fijos_apo[icor], gan_acum]
            }

            # Guardar predicciones
            fwrite(tb_eval_apo[, list(numero_de_cliente, foto_mes, prob, meta_modelo, gan_acum)],
                file = ruta_prediccion_apo, sep = "\t", append = TRUE
            )

            rm(tb_eval_apo, prob_acumulada, prob_promedio)
            gc()
        }

        log_info("--- Evaluación APO Completa. Generando entregable ---")

        colmedias <- colMeans(mganancias, na.rm = TRUE)
        mcorte_mejor <- max(colmedias, na.rm = TRUE)
        icorte_mejor <- which.max(colmedias)
        corte_mejor <- cortes_fijos_apo[icorte_mejor]

        log_info(paste0("Ganancia Máxima Promedio (APO): ", format(mcorte_mejor, big.mark = ".", decimal.mark = ","), " en corte fijo: ", corte_mejor))

        # Guardar resumen
        colnames(mganancias) <- paste0("e", cortes_fijos_apo)
        tbl_local_apo <- as.data.table(mganancias)
        tbl_local_apo[, meta_modelo := 1:PARAM$train_final$iter]

        ruta_resumen_apo <- file.path(dir_evaluacion, "eval_resumen_APO.txt")
        fwrite(tbl_local_apo, file = ruta_resumen_apo, sep = "\t")

        # Selección Final
        log_info("Seleccionando mejor modelo individual...")
        tb_prediccion_apo_full <- fread(ruta_prediccion_apo)

        icerca <- which.min(abs(tb_prediccion_apo_full$gan_acum - mcorte_mejor))
        vmodelo <- tb_prediccion_apo_full[icerca, meta_modelo]

        tb_pred_final_apo <- tb_prediccion_apo_full[meta_modelo == vmodelo]
        setorder(tb_pred_final_apo, -prob)
        corte_cercano <- tb_pred_final_apo[, .I[which.min(abs(gan_acum - mcorte_mejor))]]

        log_info(paste0("Selección Final: Meta-Modelo ", vmodelo, " en corte ", corte_cercano))

        # Generar Submission
        tb_pred_final_apo[, Predicted := 0L]
        tb_pred_final_apo[1:corte_cercano, Predicted := 1L]

        archivo_pseudo_kaggle <- file.path(dir_kaggle, paste0("APO_FINAL_", PARAM$experimento, "_", corte_cercano, ".csv"))
        fwrite(tb_pred_final_apo[, list(numero_de_cliente, Predicted)], file = archivo_pseudo_kaggle, sep = ",")

        log_info(paste("Archivo generado:", archivo_pseudo_kaggle))

        rm(tb_prediccion_apo_full, tb_pred_final_apo, mfuture)
        gc()
    },
    error = function(e) {
        log_error("Error en 9_Evaluacion_APO.R")
        log_error(e$message)
        stop(e)
    }
)
