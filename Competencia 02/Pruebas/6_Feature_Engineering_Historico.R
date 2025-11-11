#!/usr/bin/env Rscript
tryCatch({
  # Feature Engineering Historico
  columnas_originales <- copy(colnames(dataset))

  # se calculan para los 6 meses previos el minimo, maximo y
  #  tendencia calculada con cuadrados minimos
  # la formula de calculo de la tendencia puede verse en
  #  https://stats.libretexts.org/Bookshelves/Introductory_Statistics/Book%3A_Introductory_Statistics_(Shafer_and_Zhang)/10%3A_Correlation_and_Regression/10.04%3A_The_Least_Squares_Regression_Line
  # para la maxíma velocidad esta funcion esta escrita en lenguaje C,
  # y no en la porqueria de R o Python

  log_info("Compilando C++ function fhistC")
  cppFunction("NumericVector fhistC(NumericVector pcolumna, IntegerVector pdesde )
  {
    /* Aqui se cargan los valores para la regresion */
    double  x[100] ;
    double  y[100] ;

    int n = pcolumna.size();
    NumericVector out( 5*n );

    for(int i = 0; i < n; i++)
    {
      //lag
      if( pdesde[i]-1 < i )  out[ i + 4*n ]  =  pcolumna[i-1] ;
      else                   out[ i + 4*n ]  =  NA_REAL ;


      int  libre    = 0 ;
      int  xvalor   = 1 ;

      for( int j= pdesde[i]-1;  j<=i; j++ )
      {
        double a = pcolumna[j] ;

        if( !R_IsNA( a ) )
        {
            y[ libre ]= a ;
            x[ libre ]= xvalor ;
            libre++ ;
        }

        xvalor++ ;
      }

      /* Si hay al menos dos valores */
      if( libre > 1 )
      {
        double  xsum  = x[0] ;
        double  ysum  = y[0] ;
        double  xysum = xsum * ysum ;
        double  xxsum = xsum * xsum ;
        double  vmin  = y[0] ;
        double  vmax  = y[0] ;

        for( int h=1; h<libre; h++)
        {
          xsum  += x[h] ;
          ysum  += y[h] ;
          xysum += x[h]*y[h] ;
          xxsum += x[h]*x[h] ;

          if( y[h] < vmin )  vmin = y[h] ;
          if( y[h] > vmax )  vmax = y[h] ;
        }

        out[ i ]  =  (libre*xysum - xsum*ysum)/(libre*xxsum -xsum*xsum) ;
        out[ i + n ]    =  vmin ;
        out[ i + 2*n ]  =  vmax ;
        out[ i + 3*n ]  =  ysum / libre ;
      }
      else
      {
        out[ i       ]  =  NA_REAL ;
        out[ i + n   ]  =  NA_REAL ;
        out[ i + 2*n ]  =  NA_REAL ;
        out[ i + 3*n ]  =  NA_REAL ;
      }
    }

    return  out;
  }")
  log_info("Función C++ fhistC compilada.")

  # calcula la tendencia de las variables cols de los ultimos 6 meses
  # la tendencia es la pendiente de la recta que ajusta por cuadrados minimos
  # La funcionalidad de ratioavg es autoria de  Daiana Sparta,  UAustral  2021

  TendenciaYmuchomas <- function(
      dataset, cols, ventana = 6, tendencia = TRUE,
      minimo = TRUE, maximo = TRUE, promedio = TRUE,
      ratioavg = FALSE, ratiomax = FALSE) {
    gc(verbose= FALSE)
    # Esta es la cantidad de meses que utilizo para la historia
    ventana_regresion <- ventana

    last <- nrow(dataset)

    # creo el vector_desde que indica cada ventana
    # de esta forma se acelera el procesamiento ya que lo hago una sola vez
    vector_ids <- dataset[ , numero_de_cliente ]

    vector_desde <- seq(
      -ventana_regresion + 2,
      nrow(dataset) - ventana_regresion + 1
    )

    vector_desde[1:ventana_regresion] <- 1

    for (i in 2:last) {
      if (vector_ids[i - 1] != vector_ids[i]) {
        vector_desde[i] <- i
      }
    }
    for (i in 2:last) {
      if (vector_desde[i] < vector_desde[i - 1]) {
        vector_desde[i] <- vector_desde[i - 1]
      }
    }

    for (campo in cols) {
      nueva_col <- fhistC(dataset[, get(campo)], vector_desde)

      if (tendencia) {
        dataset[, paste0(campo, "_tend", ventana) :=
          nueva_col[(0 * last + 1):(1 * last)]]
      }

      if (minimo) {
        dataset[, paste0(campo, "_min", ventana) :=
          nueva_col[(1 * last + 1):(2 * last)]]
      }

      if (maximo) {
        dataset[, paste0(campo, "_max", ventana) :=
          nueva_col[(2 * last + 1):(3 * last)]]
      }

      if (promedio) {
        dataset[, paste0(campo, "_avg", ventana) :=
          nueva_col[(3 * last + 1):(4 * last)]]
      }

      if (ratioavg) {
        dataset[, paste0(campo, "_ratioavg", ventana) :=
          get(campo) / nueva_col[(3 * last + 1):(4 * last)]]
      }

      if (ratiomax) {
        dataset[, paste0(campo, "_ratiomax", ventana) :=
          get(campo) / nueva_col[(2 * last + 1):(3 * last)]]
      }
    }
  }

  MovingAverages <- function(dataset, cols, windows = c(3, 6), delta_change = TRUE, vs_actual = TRUE) {
    gc(verbose = FALSE)
    setorder(dataset, numero_de_cliente, foto_mes)

    for (col in cols) {
      for (window in windows) {  
        ma_col <- paste0(col, "_ma", window)
        dataset[, (ma_col) := frollmean(get(col), n = window, align = "right", fill = NA), 
                by = numero_de_cliente]

        if (delta_change) {       
          delta_ma_col <- paste0(col, "_ma", window, "_delta")
          lag_ma_col <- paste0(col, "_ma", window, "_lag1")

          dataset[, (lag_ma_col) := shift(get(ma_col), n = 1, fill = NA, type = "lag"), 
                  by = numero_de_cliente]

          dataset[, (delta_ma_col) := get(ma_col) - get(lag_ma_col)]
          
          dataset[, (lag_ma_col) := NULL]
        }

        if (vs_actual) {       
          vs_actual_col <- paste0(col, "_ma", window, "_vs_actual")
          dataset[, (vs_actual_col) := get(col) - get(ma_col)]
        }
      }
    }
  }

  # Ordeno todo para hacer el FE Histórico
  setorder(dataset, numero_de_cliente, foto_mes)

  # todo es lagueable, menos la primary key y la clase
  cols_lagueables <- copy( setdiff(
    colnames(dataset),
    c("numero_de_cliente", "foto_mes", "clase_ternaria")
  ))

  # https://rdrr.io/cran/data.table/man/shift.html

  if (PARAM$FE_hist$lags$run) {
    # Agrego los lags
    log_info(paste0("Creando lags para los valores: ", paste(PARAM$FE_hist$lags$n_lags, collapse = ", ")))
    for (i in PARAM$FE_hist$lags$n_lags) {
      dataset[,
        paste0(cols_lagueables, "_lag", i) := shift(.SD, i, NA, "lag"),
        by = numero_de_cliente,
        .SDcols = cols_lagueables
      ]
    }

    # Agrego los delta lags
    log_info(paste0("Creando delta lags para los valores: ", paste(PARAM$FE_hist$lags$n_lags, collapse = ", ")))
    for (i in PARAM$FE_hist$lags$n_lags) {
      for (vcol in cols_lagueables)
      {
        # La columna de lag podría no existir
        lag_col <- paste0(vcol, "_lag", i)
        if (lag_col %in% names(dataset)) {
          dataset[, paste0(vcol, "_delta", i) := get(vcol) - get(lag_col)]
        }
      }
    }

    # Derivada segunda (aceleración)
    if (PARAM$FE_hist$lags$aceleracion) {
      log_info("--- Iniciando cálculo de Aceleración (Derivada Segunda) ---")

      # Identificar lags para los que se puede calcular aceleración (k <= 6)
      k_aceleracion <- PARAM$FE_hist$lags$n_lags[PARAM$FE_hist$lags$n_lags <= 6]

      if (length(k_aceleracion) > 0) {
        # Identificar los lags dobles (k*2) que se necesitan
        k_dobles_necesarios <- unique(k_aceleracion * 2)

        # Identificar cuáles de esos lags dobles NO existen y deben crearse temporalmente
        k_a_crear_temporalmente <- setdiff(k_dobles_necesarios, PARAM$FE_hist$lags$n_lags)

        lags_temporales_creados <- c() # Para rastrear y borrar

        # Crear los lags temporales necesarios
        if (length(k_a_crear_temporalmente) > 0) {
          log_info(paste0("Creando lags temporales necesarios para aceleración: ", paste(k_a_crear_temporalmente, collapse = ", ")))
          for (i_temp in k_a_crear_temporalmente) {
            temp_lag_cols <- paste0(cols_lagueables, "_lag", i_temp)
            dataset[,
              (temp_lag_cols) := shift(.SD, i_temp, NA, "lag"),
              by = numero_de_cliente,
              .SDcols = cols_lagueables
            ]
            # Guardamos los nombres de todas las columnas creadas para borrarlas
            lags_temporales_creados <- c(lags_temporales_creados, temp_lag_cols)
          }
        }

        # Calcular las aceleraciones
        log_info(paste0("Calculando aceleraciones para k: ", paste(k_aceleracion, collapse = ", ")))
        for (k in k_aceleracion) {
          k1 <- k
          k2 <- k * 2

          for (vcol in cols_lagueables) {
            accel_col <- paste0(vcol, "_accel", k1)
            lag_k1_col <- paste0(vcol, "_lag", k1)
            lag_k2_col <- paste0(vcol, "_lag", k2)

            # Verificamos que ambos lags (k1 y k2) existan antes de calcular
            if (lag_k1_col %in% names(dataset) && lag_k2_col %in% names(dataset)) {
              # Fórmula: A_k = Xt - 2*X(t-k) + X(t-2k)
              dataset[, (accel_col) := get(vcol) - 2 * get(lag_k1_col) + get(lag_k2_col)]
            } else {
              log_warn(paste0("No se pudo crear ", accel_col, " (faltan ", lag_k1_col, " o ", lag_k2_col, ")."))
            }
          }
        }

        # Borrar los lags temporales que se crearon
        if (length(lags_temporales_creados) > 0) {
          log_info(paste0("Borrando ", length(lags_temporales_creados), " columnas de lags temporales..."))
          dataset[, (lags_temporales_creados) := NULL]
        }

      } else {
        log_info("No hay lags en n_lags <= 6. No se calculan aceleraciones.")
      }
      log_info("--- Fin cálculo de Aceleración ---")
    }
    # ########################################################
    # INICIO BLOQUE DE DEPURACIÓN (LOGS DE MUESTRA)
    # ########################################################
    log_info("--- Muestra de los datos creados en el proceso de Lags ---")
    tryCatch({
      # 1. Seleccionar la PRIMERA columna lagueable como ejemplo
      col_ejemplo <- cols_lagueables[6] # (o la que prefieras)
      log_info(paste("Mostrando ejemplos para la columna:", col_ejemplo))

      # 2. Seleccionar 5 IDs de cliente al azar
      ids_unicos <- unique(dataset$numero_de_cliente)
      n_sample <- min(5, length(ids_unicos)) # Tomar 5 (o menos, si hay menos de 5 IDs)
      ids_sample <- sample(ids_unicos, n_sample)

      log_info(paste("Mostrando IDs de muestra:", paste(ids_sample, collapse = ", ")))

      # 3. Definir todas las columnas que queremos mostrar
      lags_a_mostrar <- paste0(col_ejemplo, "_lag", PARAM$FE_hist$lags$n_lags)
      deltas_a_mostrar <- paste0(col_ejemplo, "_delta", PARAM$FE_hist$lags$n_lags)
      
      # Columnas de aceleración (solo si se calcularon)
      accel_a_mostrar <- c()
      k_aceleracion_log <- PARAM$FE_hist$lags$n_lags[PARAM$FE_hist$lags$n_lags <= 6]
      if (PARAM$FE_hist$lags$aceleracion && length(k_aceleracion_log) > 0) {
        accel_a_mostrar <- paste0(col_ejemplo, "_accel", k_aceleracion_log)
      }

      cols_a_mostrar <- c(
        "numero_de_cliente", "foto_mes", col_ejemplo,
        lags_a_mostrar, deltas_a_mostrar, accel_a_mostrar
      )

      # 4. Crear la tabla de muestra
      # Tomamos las últimas 6 filas por cada cliente
      dt_sample <- dataset[numero_de_cliente %in% ids_sample,
      tail(.SD, 6L),
      by = numero_de_cliente,
      .SDcols = intersect(cols_a_mostrar, colnames(dataset))] # Intersect previene errores si una col no se creó

      # 5. Capturar el 'print' de la tabla y mandarlo al log
      if (nrow(dt_sample) > 0) {
        log_info(
          paste(
              capture.output(print(dt_sample, nrows = 30)), # 30 = 5 IDs * 6 filas
              collapse = "\n"
            )
        )
      } else {
        log_info("No se pudo generar la tabla de muestra (dt_sample está vacía).")
      }
      
      # 6. Verificación de borrado de lags temporales
      if (PARAM$FE_hist$lags$aceleracion && length(k_aceleracion_log) > 0) {
        log_info("--- Verificación de borrado de lags temporales ---")
        k_dobles_necesarios_log <- unique(k_aceleracion_log * 2)
        k_temporales_log <- setdiff(k_dobles_necesarios_log, PARAM$FE_hist$lags$n_lags)
        
        if(length(k_temporales_log) > 0) {
          for (k_temp in k_temporales_log) {
            temp_col_name <- paste0(col_ejemplo, "_lag", k_temp)
            if (!temp_col_name %in% names(dataset)) {
              log_info(paste0("OK: ", temp_col_name, " no existe (borrado correctamente)."))
            } else {
              log_warn(paste0("ALERTA: ", temp_col_name, " aún existe (no debería)."))
            }
          }
        } else {
          log_info("No se crearon lags temporales (todos los k*2 ya estaban en n_lags).")
        }
      }

    }, error = function(e) {
      log_error(paste("Error al generar log de muestra:", e$message))
    })

    log_info("--- Fin Muestra ---")
    # ########################################################
    # FIN BLOQUE DE DEPURACIÓN
    # ########################################################
    log_info("Lags, Deltas y Aceleraciones creados.")
  } 

  if (PARAM$FE_hist$Tendencias$run) {
    # Tendencias
    cols_lagueables <- intersect(cols_lagueables, colnames(dataset))
    setorder(dataset, numero_de_cliente, foto_mes)
    log_info("Calculando tendencias.")
    for (v in PARAM$FE_hist$Tendencias$ventana) {
      TendenciaYmuchomas(dataset,
        cols = cols_lagueables,
        ventana = v, # Por defecto serán 6 meses
        tendencia = PARAM$FE_hist$Tendencias$tendencia,
        minimo = PARAM$FE_hist$Tendencias$minimo,
        maximo = PARAM$FE_hist$Tendencias$maximo,
        promedio = PARAM$FE_hist$Tendencias$promedio,
        ratioavg = PARAM$FE_hist$Tendencias$ratioavg,
        ratiomax = PARAM$FE_hist$Tendencias$ratiomax
      )
    }
    log_info("Tendencias calculadas.")
  }

  # Moving Averages
  if (PARAM$FE_hist$MovingAverages$run) {
    log_info("Calculando moving averages.")
    setorder(dataset, numero_de_cliente, foto_mes)
    MovingAverages(dataset,
      cols = cols_lagueables,
      windows = PARAM$FE_hist$MovingAverages$windows,
      delta_change = PARAM$FE_hist$MovingAverages$delta_change,
      vs_actual = PARAM$FE_hist$MovingAverages$vs_actual
    )
    log_info("Moving averages calculados.")
  }

  columnas_nuevas <- setdiff(colnames(dataset), columnas_originales)
  log_info("Nuevas columnas creadas:")
  log_info(paste0(columnas_nuevas, collapse = ", "))
}, error = function(e) {
  log_error("######################################################")
  log_error("Se ha producido un error fatal en la Sección 6: Feature Engineering Histórico.")
  log_error(paste("Mensaje de R:", e$message))
  log_error("######################################################")
})