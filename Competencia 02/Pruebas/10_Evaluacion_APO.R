tryCatch({  
  # Defino los parámetros locales de APO basados en main_z.R
  APO_iter <- PARAM$eval_ensamble$iter
  ksemillerio <- PARAM$eval_ensamble$ksemillerio
  q_canaritos <- PARAM$qcanaritos
  cortes_evaluacion <- PARAM$eval_ensamble$cortes_evaluacion
  mes_testing <- PARAM$train_final$future
  
  # Carpeta para guardar los modelos
  modelos_path <- file.path(PARAM$experimento_folder, PARAM$modelos_folder)
  dir.create(modelos_path, showWarnings = FALSE)
  
  # Carpeta para guardar los resultados de evaluación
  evaluacion_path <- file.path(PARAM$experimento_folder, PARAM$carpeta_evaluacion)
  dir.create(evaluacion_path, showWarnings = FALSE)

  log_info("Parámetros de APO: Iteraciones={APO_iter}, Semillerio={ksemillerio}")

  # --- 1. Preparación de Datos de Entrenamiento ---
  
  log_info("Definiendo campos buenos para el entrenamiento...")
  # 'dataset_train_final' fue creado en el script 9_Modelado.R
  # Excluyo las columnas que no deben usarse para entrenar
  campos_buenos <- setdiff(
    colnames(dataset_train_final),
    c("clase_ternaria", "clase01", "training", "azar")
  )
  
  log_info("Creando lgb.Dataset (dtrain_final) para entrenamiento...")
  # Dejo los datos en formato LightGBM
  dtrain_final <- lgb.Dataset(
    data = data.matrix(dataset_train_final[training == 1L, campos_buenos, with = FALSE]),
    label = dataset_train_final[training == 1L, clase01],
    free_raw_data = FALSE
  )
  
  log_info(paste(
    "dtrain_final creado:",
    "filas=", nrow(dtrain_final),
    "columnas=", ncol(dtrain_final)
  ))

  # --- 2. Generación de Semillas y Modelos ---
  
  log_info("Generando semillas primas para el semillerio...")
  total_semillas <- APO_iter * ksemillerio 
  primos <- generate_primes(min = 100000, max = 1000000)
  set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")
  PARAM$eval_ensamble$semillas <- sample(primos)[seq( total_semillas )]
  
  log_info(paste("Total de semillas generadas:", total_semillas))
  
  # Copio los parámetros de zLightGBM
  param_completo <- copy(PARAM$lgbm_z)
  
  log_info("Iniciando entrenamiento de los {total_semillas} modelos...")
  
  for (sem in PARAM$eval_ensamble$semillas) {
    
    arch_modelo <- file.path(modelos_path, paste0("mod_", sem, ".txt"))
    
    if (!file.exists(arch_modelo)) {
      log_info(paste("Entrenando modelo para semilla:", sem))
      param_completo$seed <- sem
      
      modelito <- lgb.train(
        data = dtrain_final,
        param = param_completo
      )
      
      lgb.save(modelito, filename = arch_modelo)
      rm(modelito)
      gc()
      
    } else {
      log_info(paste("Modelo existente para semilla:", sem, "(saltando)"))
    }
  }
  
  log_info("--- Entrenamiento de todos los modelos finalizado ---")

  # --- 3. Preparación de Datos de Scoring (Future) ---
  
  log_info(paste("Preparando datos de 'future' para el mes:", mes_testing))
  
  # IMPORTANTE: 'dataset' es el global, con FE completo.
  dfuture <- dataset[foto_mes %in% mes_testing]
  
  log_info(paste("Agregando", q_canaritos, "columnas canarito a dfuture"))
  cols0_future <- copy(colnames(dfuture))
  filas_future <- nrow(dfuture)
  
  for (i in seq(q_canaritos)) {
    dfuture[, paste0("canarito_", i) := runif(filas_future)]
  }
  
  cols_canaritos_future <- copy(setdiff(colnames(dfuture), cols0_future))
  # Nos aseguramos que el orden de columnas sea idéntico al de entrenamiento
  setcolorder(dfuture, c(cols_canaritos_future, cols0_future))
  
  # Creamos la matriz para predicción
  mfuture <- data.matrix(dfuture[, campos_buenos, with = FALSE])
  
  # Agregamos la columna de ganancia
  dfuture[, ganancia := ifelse(clase_ternaria=="BAJA+2", 780000, -20000)]
  
  log_info("Datos de 'future' listos para scoring.")

  # --- 4. Bucle de Scoring APO ---
  
  log_info("Iniciando bucle de scoring APO...")
  
  # Matriz para guardar las ganancias de cada iteración de APO en cada corte
  mganancias <- matrix(
    nrow = APO_iter,
    ncol = length(cortes_evaluacion)
  )
  
  # Archivo de predicciones (lo borramos si existe)
  prediccion_file <- file.path(evaluacion_path, "prediccion_apo.txt")
  if (file.exists(prediccion_file)) {
    file.remove(prediccion_file)
  }
  
  for (vapo in seq(APO_iter)) {
    log_info(paste("--- Ejecutando APO Iteración:", vapo, "/", APO_iter, "---"))
    
    # Vector para acumular predicciones
    vpred_acum <- rep(0.0, nrow(dfuture))
    qacumulados <- 0
    
    # Selecciono las semillas para ESTA iteración de APO
    desde <- 1 + (vapo - 1) * ksemillerio
    hasta <- desde + ksemillerio - 1
    semillas <- PARAM$eval_ensamble$semillas[desde:hasta]
    
    log_info(paste("Usando", ksemillerio, "semillas para esta iteración."))
    
    for (sem in semillas) {
      arch_modelo <- file.path(modelos_path, paste0("mod_", sem, ".txt"))
      
      if (file.exists(arch_modelo)) {
        modelo_final <- lgb.load(arch_modelo)
        
        vpred_acum <- vpred_acum + predict(modelo_final, mfuture)
        qacumulados <- qacumulados + 1
        
        rm(modelo_final)
        gc()
      } else {
        log_warn(paste("No se encontró el modelo para la semilla:", sem))
      }
    } # Fin bucle semillas
    
    if (qacumulados > 0) {
      # Promedio las predicciones del semillerio
      vpred_acum <- vpred_acum / qacumulados
      
      # Creo la tabla de predicción para esta iteración de APO
      tb_prediccion <- dfuture[, list(numero_de_cliente, foto_mes, ganancia)]
      tb_prediccion[, meta_modelo := vapo] # ID de la iteración APO
      tb_prediccion[, prob := vpred_acum]
      
      setorder(tb_prediccion, -prob)
      
      # Calculo la ganancia acumulada
      tb_prediccion[, gan_acum := cumsum(ganancia)]
      tb_prediccion[, ganancia := NULL] # Ya no la necesito
      
      # acumulo las ganancias
      for( icor in seq(length(PARAM$train_final$cortes)) ){
        mganancias[ vapo, icor ] <- tb_prediccion[ PARAM$train_final$cortes[icor], gan_acum ]
      }
      
      # Grabo las probabilidades de esta iteración
      fwrite(tb_prediccion,
        file = prediccion_file,
        sep = "\t",
        append = TRUE
      )
      
      log_info(paste("Iteración APO", vapo, "completa."))
      rm(tb_prediccion)
      gc()
      
    } else {
      log_error(paste("No se encontraron modelos para la iteración APO", vapo))
    }
  } # Fin bucle APO
  
  log_info("--- Bucle de Scoring APO finalizado ---")

  # --- 5. Guardar Resultados de Evaluación ---
  log_info("Guardando resultados de la evaluación APO...")
  
  colnames(mganancias) <- paste0("e", cortes_evaluacion)
  tbl_local <- as.data.table(mganancias)
  
  archivo_apo_out <- file.path(evaluacion_path, "tb_apo_ganancias.txt")
  fwrite(
    tbl_local,
    file = archivo_apo_out,
    sep = "\t"
  )
  
  log_info(paste("Matriz de ganancias guardada en:", archivo_apo_out))
  
  # --- 6. Generación de Entregable "Pseudo Kaggle" ---
  log_info("--- Iniciando generación de entregable (Lógica APO) ---")
  
  # Calcular la mejor ganancia PROMEDIO
  colmedias <- colMeans(mganancias, na.rm = TRUE)
  mcorte_mejor <- max(colmedias, na.rm = TRUE)
  icorte_mejor <- which.max(colmedias)
  corte_mejor <- cortes_evaluacion[icorte_mejor]
  
  log_info(paste("Mejor ganancia media (mcorte_mejor):", format(mcorte_mejor, scientific = FALSE, big.mark = ",")))
  log_info(paste("Mejor corte promedio (corte_mejor):", corte_mejor))

  # Guardar las ganancias medias en el archivo general
  tbl_medias <- as.data.table(as.list(colmedias))
  colnames(tbl_medias) <- paste0("e", cortes_evaluacion)
  tbl_medias[, experimento := PARAM$experimento]
  
  # Directorio general de experimentos (un nivel arriba)
  exp_gral_path <- PARAM$output_folder 
  archivo_exp_gral <- file.path(exp_gral_path, "tb_experimentos_generales.txt")
  
  fwrite(tbl_medias,
    file = archivo_exp_gral,
    sep = "\t",
    append = TRUE
  )
  log_info(paste("Ganancias medias guardadas en:", archivo_exp_gral))
  
  # Encontrar la predicción INDIVIDUAL más cercana a la media
  log_info("Cargando predicciones completas para encontrar el mejor modelo...")
  tb_prediccion_full <- fread(prediccion_file)
  
  # icerca_global: la fila (en todo el archivo) más cercana a la ganancia media
  icerca_global <- which.min(abs(tb_prediccion_full$gan_acum - mcorte_mejor))
  
  # vmodelo: el ID de la iteración APO (meta_modelo) de esa fila
  vmodelo <- tb_prediccion_full[icerca_global, meta_modelo]
  
  log_info(paste("El modelo individual (vmodelo) más cercano a la media es:", vmodelo))
  log_info(paste("Ganancia de ese modelo en ese corte:", tb_prediccion_full[icerca_global, gan_acum]))
  
  # Generar el archivo de envío
  
  # Filtramos solo las predicciones de ese 'vmodelo'
  tb_pred_final <- tb_prediccion_full[meta_modelo == vmodelo]
  
  # icerca_local: el número de envíos (corte) dentro de ESE modelo
  icerca_local <- which.min(abs(tb_pred_final$gan_acum - mcorte_mejor))
  
  log_info(paste("Número de envíos (corte) para este modelo:", icerca_local))
  
  # Creamos la columna 'Predicted'
  tb_pred_final[, Predicted := 0L] # Seteo inicial a 0
  tb_pred_final[1:icerca_local, Predicted := 1L] # Marco los primeros
  
  # Definimos el nombre del archivo final
  archivo_pseudo_kaggle <- file.path(
    entregables_path,
    paste0("KA_", PARAM$experimento, "_", icerca_local, ".csv")
  )
  
  # Grabamos el archivo
  fwrite(tb_pred_final[, list(numero_de_cliente, Predicted)],
    file = archivo_pseudo_kaggle,
    sep = ","
  )
  
  log_info(paste("Archivo final de entregable generado en:", archivo_pseudo_kaggle))
  
  # Limpieza
  rm(tb_prediccion_full, tb_pred_final)
  gc()

}, error = function(e) {
  log_error("######################################################")
  log_error("Se ha producido un error fatal en la Sección 10: Evaluación APO.")
  log_error(paste("Mensaje de R:", e$message))
  log_error("Contexto del error:", e$call)
  log_error("######################################################")
  stop("Error fatal en 10_Evaluacion_APO.R")
})