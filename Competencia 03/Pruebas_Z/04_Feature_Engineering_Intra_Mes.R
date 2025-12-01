#------------------------------------------------------------------------------
# Funciones auxiliares (Helpers)
#------------------------------------------------------------------------------

# Verificar existencia estricta de columnas (Original)
atributos_presentes <- function(patributos) {
  atributos <- unique(patributos)
  comun <- intersect(atributos, colnames(dataset))
  return(length(atributos) == length(comun))
}

# Obtener el nombre correcto (original o rankeado)
get_col_name <- function(base_name, dataset_cols) {
  rank_name <- paste0(base_name, "_rank")
  if (rank_name %in% dataset_cols) {
    return(rank_name)
  } else if (base_name %in% dataset_cols) {
    return(base_name)
  } else {
    return(NULL)
  }
}

# Verificar si una columna existe (Helper para ranks)
col_exists <- function(base_name, dataset_cols) {
  rank_name <- paste0(base_name, "_rank")
  return((base_name %in% dataset_cols) || (rank_name %in% dataset_cols))
}

#------------------------------------------------------------------------------
# FUNCIÓN PRINCIPAL: Feature Engineering Intra-Mes
#------------------------------------------------------------------------------
AgregarVariables_IntraMes <- function(dataset,
                                      run_combinaciones_moreira = FALSE,
                                      run_combinaciones_monetarias = FALSE,
                                      run_ratios = FALSE,
                                      run_totales = FALSE,
                                      run_comportamiento = FALSE,
                                      run_riesgo = FALSE) {
  log_info("inicio AgregarVariables_IntraMes()")

  # Tracking de columnas
  columnas_originales <- copy(colnames(dataset))

  log_info("Agregando variables extraídas del workflow jueves.")

  dataset[, kmes := foto_mes %% 100]

  dataset[, ctrx_quarter_normalizado := as.numeric(ctrx_quarter)]
  dataset[cliente_antiguedad == 1, ctrx_quarter_normalizado := ctrx_quarter * 5.0]
  dataset[cliente_antiguedad == 2, ctrx_quarter_normalizado := ctrx_quarter * 2.0]
  dataset[cliente_antiguedad == 3, ctrx_quarter_normalizado := ctrx_quarter * 1.2]

  # Variable extraída de tesis de maestría - Usar versión rankeada si existe
  mpayroll_col <- get_col_name("mpayroll", colnames(dataset))
  if (!is.null(mpayroll_col)) {
    dataset[, mpayroll_sobre_edad := get(mpayroll_col) / cliente_edad]
  }

  if (run_combinaciones_moreira) {
    dataset[, x_trx_visa_por_cuenta := ctarjeta_visa_transacciones / pmax(ctarjeta_visa, 1)]
    dataset[, x_margen_total := mactivos_margen + mpasivos_margen] 
    dataset[, x_pct_margen_activos := mactivos_margen / pmax(x_margen_total, 1)]# No sigue
    dataset[, x_liquidity_stress := (Master_delinquency + Visa_delinquency + mcomisiones_mantenimiento + mcomisiones_otras) / pmax(mcuentas_saldo, 1)] # No sigue
    dataset[, x_lifecycle_stage := cliente_antiguedad / pmax(cliente_edad * 12, 1)] # No sigue
    dataset[, x_ticket_visa := mtarjeta_visa_consumo / pmax(ctarjeta_visa_transacciones, 1)]
    dataset[, x_air_bike := mtarjeta_visa_consumo / pmax((mcuentas_saldo), 1)]
    dataset[, x_air_bike_2 := Visa_msaldototal / pmax((mcuentas_saldo), 1)]
    dataset[, x_bonzo := (mtarjeta_visa_consumo * pmax(Visa_status, 1))/100]
  }

  # Combinaciones monetarias
  if (run_combinaciones_monetarias) {
    if( atributos_presentes( c("Master_status", "Visa_status") ))
      {
        dataset[, vm_status01 := pmax(Master_status, Visa_status, na.rm = TRUE)]
        dataset[, vm_status02 := Master_status + Visa_status]

        dataset[, vm_status03 := pmax(
          ifelse(is.na(Master_status), 10, Master_status),
          ifelse(is.na(Visa_status), 10, Visa_status)
        )]

        dataset[, vm_status04 := ifelse(is.na(Master_status), 10, Master_status)
          + ifelse(is.na(Visa_status), 10, Visa_status)]

        dataset[, vm_status05 := ifelse(is.na(Master_status), 10, Master_status)
          + 100 * ifelse(is.na(Visa_status), 10, Visa_status)]

        dataset[, vm_status06 := ifelse(is.na(Visa_status),
          ifelse(is.na(Master_status), 10, Master_status),
          Visa_status
        )]

        dataset[, mv_status07 := ifelse(is.na(Master_status),
          ifelse(is.na(Visa_status), 10, Visa_status),
          Master_status
        )]
      }

      # combino MasterCard y Visa
      if( atributos_presentes( c("Master_mfinanciacion_limite", "Visa_mfinanciacion_limite") ))
        dataset[, vm_mfinanciacion_limite := rowSums(cbind(Master_mfinanciacion_limite, Visa_mfinanciacion_limite), na.rm = TRUE)]

      if( atributos_presentes( c("Master_Fvencimiento", "Visa_Fvencimiento") ))
        dataset[, vm_Fvencimiento := pmin(Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE)]

      if( atributos_presentes( c("Master_Finiciomora", "Visa_Finiciomora") ))
        dataset[, vm_Finiciomora := pmin(Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE)]

      if( atributos_presentes( c("Master_msaldototal", "Visa_msaldototal") ))
        dataset[, vm_msaldototal := rowSums(cbind(Master_msaldototal, Visa_msaldototal), na.rm = TRUE)]

      if( atributos_presentes( c("Master_msaldopesos", "Visa_msaldopesos") ))
        dataset[, vm_msaldopesos := rowSums(cbind(Master_msaldopesos, Visa_msaldopesos), na.rm = TRUE)]

      if( atributos_presentes( c("Master_msaldodolares", "Visa_msaldodolares") ))
        dataset[, vm_msaldodolares := rowSums(cbind(Master_msaldodolares, Visa_msaldodolares), na.rm = TRUE)]

      if( atributos_presentes( c("Master_mconsumospesos", "Visa_mconsumospesos") ))
        dataset[, vm_mconsumospesos := rowSums(cbind(Master_mconsumospesos, Visa_mconsumospesos), na.rm = TRUE)]

      if( atributos_presentes( c("Master_mconsumosdolares", "Visa_mconsumosdolares") ))
        dataset[, vm_mconsumosdolares := rowSums(cbind(Master_mconsumosdolares, Visa_mconsumosdolares), na.rm = TRUE)]

      if( atributos_presentes( c("Master_mlimitecompra", "Visa_mlimitecompra") ))
        dataset[, vm_mlimitecompra := rowSums(cbind(Master_mlimitecompra, Visa_mlimitecompra), na.rm = TRUE)]

      if( atributos_presentes( c("Master_madelantopesos", "Visa_madelantopesos") ))
        dataset[, vm_madelantopesos := rowSums(cbind(Master_madelantopesos, Visa_madelantopesos), na.rm = TRUE)]

      if( atributos_presentes( c("Master_madelantodolares", "Visa_madelantodolares") ))
        dataset[, vm_madelantodolares := rowSums(cbind(Master_madelantodolares, Visa_madelantodolares), na.rm = TRUE)]

      if( atributos_presentes( c("Master_fultimo_cierre", "Visa_fultimo_cierre") ))
        dataset[, vm_fultimo_cierre := pmax(Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE)]

      if( atributos_presentes( c("Master_mpagado", "Visa_mpagado") ))
        dataset[, vm_mpagado := rowSums(cbind(Master_mpagado, Visa_mpagado), na.rm = TRUE)]

      if( atributos_presentes( c("Master_mpagospesos", "Visa_mpagospesos") ))
        dataset[, vm_mpagospesos := rowSums(cbind(Master_mpagospesos, Visa_mpagospesos), na.rm = TRUE)]

      if( atributos_presentes( c("Master_mpagosdolares", "Visa_mpagosdolares") ))
        dataset[, vm_mpagosdolares := rowSums(cbind(Master_mpagosdolares, Visa_mpagosdolares), na.rm = TRUE)]

      if( atributos_presentes( c("Master_fechaalta", "Visa_fechaalta") ))
        dataset[, vm_fechaalta := pmax(Master_fechaalta, Visa_fechaalta, na.rm = TRUE)]

      if( atributos_presentes( c("Master_mconsumototal", "Visa_mconsumototal") ))
        dataset[, vm_mconsumototal := rowSums(cbind(Master_mconsumototal, Visa_mconsumototal), na.rm = TRUE)]

      if( atributos_presentes( c("Master_cconsumos", "Visa_cconsumos") ))
        dataset[, vm_cconsumos := rowSums(cbind(Master_cconsumos, Visa_cconsumos), na.rm = TRUE)]

      if( atributos_presentes( c("Master_cadelantosefectivo", "Visa_cadelantosefectivo") ))
        dataset[, vm_cadelantosefectivo := rowSums(cbind(Master_cadelantosefectivo, Visa_cadelantosefectivo), na.rm = TRUE)]

      if( atributos_presentes( c("Master_mpagominimo", "Visa_mpagominimo") ))
        dataset[, vm_mpagominimo := rowSums(cbind(Master_mpagominimo, Visa_mpagominimo), na.rm = TRUE)]

      if( atributos_presentes( c("Master_mlimitecompra", "vm_mlimitecompra") ))
        dataset[, vmr_Master_mlimitecompra := Master_mlimitecompra / vm_mlimitecompra]

      if( atributos_presentes( c("Visa_mlimitecompra", "vm_mlimitecompra") ))
        dataset[, vmr_Visa_mlimitecompra := Visa_mlimitecompra / vm_mlimitecompra]

      if( atributos_presentes( c("vm_msaldototal", "vm_mlimitecompra") ))
        dataset[, vmr_msaldototal := vm_msaldototal / vm_mlimitecompra]

      if( atributos_presentes( c("vm_msaldopesos", "vm_mlimitecompra") ))
        dataset[, vmr_msaldopesos := vm_msaldopesos / vm_mlimitecompra]

      if( atributos_presentes( c("vm_msaldopesos", "vm_msaldototal") ))
        dataset[, vmr_msaldopesos2 := vm_msaldopesos / vm_msaldototal]

      if( atributos_presentes( c("vm_msaldodolares", "vm_mlimitecompra") ))
        dataset[, vmr_msaldodolares := vm_msaldodolares / vm_mlimitecompra]

      if( atributos_presentes( c("vm_msaldodolares", "vm_msaldototal") ))
        dataset[, vmr_msaldodolares2 := vm_msaldodolares / vm_msaldototal]

      if( atributos_presentes( c("vm_mconsumospesos", "vm_mlimitecompra") ))
        dataset[, vmr_mconsumospesos := vm_mconsumospesos / vm_mlimitecompra]

      if( atributos_presentes( c("vm_mconsumosdolares", "vm_mlimitecompra") ))
        dataset[, vmr_mconsumosdolares := vm_mconsumosdolares / vm_mlimitecompra]

      if( atributos_presentes( c("vm_madelantopesos", "vm_mlimitecompra") ))
        dataset[, vmr_madelantopesos := vm_madelantopesos / vm_mlimitecompra]

      if( atributos_presentes( c("vm_madelantodolares", "vm_mlimitecompra") ))
        dataset[, vmr_madelantodolares := vm_madelantodolares / vm_mlimitecompra]

      if( atributos_presentes( c("vm_mpagado", "vm_mlimitecompra") ))
        dataset[, vmr_mpagado := vm_mpagado / vm_mlimitecompra]

      if( atributos_presentes( c("vm_mpagospesos", "vm_mlimitecompra") ))
        dataset[, vmr_mpagospesos := vm_mpagospesos / vm_mlimitecompra]

      if( atributos_presentes( c("vm_mpagosdolares", "vm_mlimitecompra") ))
        dataset[, vmr_mpagosdolares := vm_mpagosdolares / vm_mlimitecompra]

      if( atributos_presentes( c("vm_mconsumototal", "vm_mlimitecompra") ))
        dataset[, vmr_mconsumototal := vm_mconsumototal / vm_mlimitecompra]

      if( atributos_presentes( c("vm_mpagominimo", "vm_mlimitecompra") ))
        dataset[, vmr_mpagominimo := vm_mpagominimo / vm_mlimitecompra]

      if( atributos_presentes( c("ctrx_quarter_normalizado", "ctarjeta_visa_transacciones") ))
        dataset[, attr1 := rowSums(cbind(ctrx_quarter_normalizado, ctarjeta_visa_transacciones), na.rm = TRUE)]

      if( atributos_presentes( c("ctrx_quarter_normalizado", "cpayroll_trx") ))
        dataset[, attr2 := rowSums(cbind(ctrx_quarter_normalizado, cpayroll_trx), na.rm = TRUE)]

       if( atributos_presentes( c("vm_status01", "cpayroll_trx") ))
        dataset[, attr3 := vm_status01 / cpayroll_trx]

      if( atributos_presentes( c("cpayroll_trx", "ctarjeta_visa_transacciones") ))
        dataset[, attr4 := cpayroll_trx * ctarjeta_visa_transacciones]

      if( atributos_presentes( c("cpayroll_trx", "ctarjeta_visa_transacciones") ))
        dataset[, attr5 := cpayroll_trx / ctarjeta_visa_transacciones]

      if( atributos_presentes( c("cpayroll_trx", "vmr_mpagominimo") ))
        dataset[, attr6 := cpayroll_trx / vmr_mpagominimo]

      if( atributos_presentes( c("cpayroll_trx", "vmr_mpagominimo") ))
        dataset[, attr7 := cpayroll_trx / vmr_mpagominimo]

      if( atributos_presentes( c("vm_status01", "tcallcenter") ))
        dataset[, attr8 := rowSums(cbind(vm_status01, tcallcenter), na.rm = TRUE)]

      if( atributos_presentes( c("ctrx_quarter_normalizado", "tcallcenter") ))
        dataset[, attr9 := ctrx_quarter_normalizado / tcallcenter]

      if( atributos_presentes( c("vm_status01", "tcallcenter") ))
       dataset[, attr10 := rowSums(cbind(vm_status01, ctarjeta_visa_transacciones), na.rm = TRUE)]
      
      if( atributos_presentes( c("ctrx_quarter_normalizado", "cliente_edad") ))
        dataset[, attr11 := ctrx_quarter_normalizado * cliente_edad]

      if( atributos_presentes( c("ctarjeta_visa_transacciones", "tcallcenter") ))
        dataset[, attr12 := ctarjeta_visa_transacciones / tcallcenter]

      if( atributos_presentes( c("ctrx_quarter_normalizado", "vmr_mpagominimo") ))
        dataset[, attr13 := rowSums(cbind(ctrx_quarter_normalizado, vmr_mpagominimo), na.rm = TRUE)]

      if( atributos_presentes( c("cpayroll_trx", "vm_status01") ))
        dataset[, attr14 := cpayroll_trx / vm_status01]

      if( atributos_presentes( c("tcallcenter", "vmr_mpagominimo") ))
        dataset[, attr15 := tcallcenter / vmr_mpagominimo]

      if( atributos_presentes( c("ctrx_quarter_normalizado", "vmr_mpagominimo") ))
        dataset[, attr16 := ctrx_quarter_normalizado / vmr_mpagominimo]

      if( atributos_presentes( c("vm_status01", "tcallcenter") ))
        dataset[, attr18 := vm_status01 / tcallcenter] 

      if( atributos_presentes( c("cpayroll_trx", "vmr_mpagominimo") ))
        dataset[, attr19 := rowSums(cbind(cpayroll_trx, vmr_mpagominimo), na.rm = TRUE)]

      if( atributos_presentes( c("ctrx_quarter_normalizado", "ctarjeta_visa_transacciones", "mpayroll_sobre_edad") ))
        dataset[, attr20 := rowSums(cbind(ctrx_quarter_normalizado, ctarjeta_visa_transacciones, mpayroll_sobre_edad), na.rm = TRUE)]

      if( atributos_presentes( c("ctrx_quarter", "ctarjeta_visa_transacciones", "cpayroll_trx", "vmr_mpagominimo") ))
        dataset[, attr21 := rowSums(cbind(ctrx_quarter, ctarjeta_visa_transacciones, cpayroll_trx / vmr_mpagominimo), na.rm = TRUE)]
  }

  if (run_ratios || run_totales || run_comportamiento || run_riesgo) {
    tryCatch(
      {
        cols_dataset <- colnames(dataset)

        mpayroll_col <- get_col_name("mpayroll", cols_dataset)
        mpayroll2_col <- get_col_name("mpayroll2", cols_dataset)

        visa_msaldototal_col <- get_col_name("Visa_msaldototal", cols_dataset)
        master_msaldototal_col <- get_col_name("Master_msaldototal", cols_dataset)
        visa_mlimitecompra_col <- get_col_name("Visa_mlimitecompra", cols_dataset)
        master_mlimitecompra_col <- get_col_name("Master_mlimitecompra", cols_dataset)

        visa_msaldopesos_col <- get_col_name("Visa_msaldopesos", cols_dataset)
        master_msaldopesos_col <- get_col_name("Master_msaldopesos", cols_dataset)

        visa_mconsumototal_col <- get_col_name("Visa_mconsumototal", cols_dataset)
        master_mconsumototal_col <- get_col_name("Master_mconsumototal", cols_dataset)

        mtarjeta_visa_consumo_col <- get_col_name("mtarjeta_visa_consumo", cols_dataset)
        mtarjeta_master_consumo_col <- get_col_name("mtarjeta_master_consumo", cols_dataset)

        mcuenta_corriente_col <- get_col_name("mcuenta_corriente", cols_dataset)
        mcaja_ahorro_col <- get_col_name("mcaja_ahorro", cols_dataset)
        mcaja_ahorro_dolares_col <- get_col_name("mcaja_ahorro_dolares", cols_dataset)

        mprestamos_prendarios_col <- get_col_name("mprestamos_prendarios", cols_dataset)
        mprestamos_hipotecarios_col <- get_col_name("mprestamos_hipotecarios", cols_dataset)

        mplazo_fijo_pesos_col <- get_col_name("mplazo_fijo_pesos", cols_dataset)
        mplazo_fijo_dolares_col <- get_col_name("mplazo_fijo_dolares", cols_dataset)
        minversion1_pesos_col <- get_col_name("minversion1_pesos", cols_dataset)
        minversion1_dolares_col <- get_col_name("minversion1_dolares", cols_dataset)
        minversion2_col <- get_col_name("minversion2", cols_dataset)

        mrentabilidad_col <- get_col_name("mrentabilidad", cols_dataset)
        mactivos_margen_col <- get_col_name("mactivos_margen", cols_dataset)
        mcomisiones_col <- get_col_name("mcomisiones", cols_dataset)

        # RATIOS
        if (run_ratios) {
          log_info("Ratios y Proporciones")

          # Ratios actividad
          dataset[, ratio_visa_master := ifelse(ctarjeta_master_transacciones > 0, ctarjeta_visa_transacciones / ctarjeta_master_transacciones, 0)]
          dataset[, ratio_debito_credito := ifelse((ctarjeta_visa_transacciones + ctarjeta_master_transacciones) > 0, ctarjeta_debito_transacciones / (ctarjeta_visa_transacciones + ctarjeta_master_transacciones), 0)]
          dataset[, ratio_transacciones_productos := ifelse(cproductos > 0, ctrx_quarter / cproductos, 0)]

          # Ratio consumo
          if (!is.null(mtarjeta_visa_consumo_col) && !is.null(mtarjeta_master_consumo_col)) {
            dataset[, ratio_consumo_visa_master := ifelse(get(mtarjeta_master_consumo_col) > 0, get(mtarjeta_visa_consumo_col) / get(mtarjeta_master_consumo_col), 0)]
          }

          # Rentabilidad y Comisiones
          if (!is.null(mrentabilidad_col)) dataset[, rentabilidad_por_producto := ifelse(cproductos > 0, get(mrentabilidad_col) / cproductos, 0)]

          if (!is.null(mrentabilidad_col) && !is.null(mactivos_margen_col)) {
            dataset[, margen_por_activo := ifelse(get(mactivos_margen_col) > 0, get(mrentabilidad_col) / get(mactivos_margen_col), 0)]
          }

          if (!is.null(mcomisiones_col) && !is.null(mrentabilidad_col)) {
            dataset[, comisiones_sobre_ingresos := ifelse(get(mrentabilidad_col) > 0, get(mcomisiones_col) / get(mrentabilidad_col), 0)]
          }

          if (!is.null(mcomisiones_col) && !is.null(mactivos_margen_col)) {
            dataset[, comisiones_sobre_activos := ifelse(get(mactivos_margen_col) > 0, get(mcomisiones_col) / get(mactivos_margen_col), 0)]
          }

          # Ratios canales
          dataset[, ratio_digital_tradicional := ifelse(ccajas_transacciones > 0, chomebanking_transacciones / ccajas_transacciones, 0)]
          dataset[, ratio_atm_cajas := ifelse(ccajas_transacciones > 0, catm_trx / ccajas_transacciones, 0)]

          # Ratios endeudamiento
          if (!is.null(mpayroll_col) && !is.null(visa_msaldototal_col) && !is.null(master_msaldototal_col)) {
            dataset[, ratio_deuda_ingreso := ifelse(get(mpayroll_col) > 0, (get(visa_msaldototal_col) + get(master_msaldototal_col)) / get(mpayroll_col), 0)]
          }
          if (!is.null(visa_mlimitecompra_col) && !is.null(visa_msaldototal_col)) {
            dataset[, utilizacion_visa := ifelse(get(visa_mlimitecompra_col) > 0, get(visa_msaldototal_col) / get(visa_mlimitecompra_col), 0)]
          }
          if (!is.null(master_mlimitecompra_col) && !is.null(master_msaldototal_col)) {
            dataset[, utilizacion_master := ifelse(get(master_mlimitecompra_col) > 0, get(master_msaldototal_col) / get(master_mlimitecompra_col), 0)]
          }
          if ("utilizacion_visa" %in% names(dataset) && "utilizacion_master" %in% names(dataset)) {
            dataset[, utilizacion_promedio := (utilizacion_visa + utilizacion_master) / 2]
          }

          # Ratios cuentas
          dataset[, ratio_cc_ca := ifelse(ccaja_ahorro > 0, ccuenta_corriente / ccaja_ahorro, 0)]

          if (!is.null(mcuenta_corriente_col) && !is.null(mcaja_ahorro_col)) {
            dataset[, ratio_saldo_cc_ca := ifelse(get(mcaja_ahorro_col) > 0, get(mcuenta_corriente_col) / get(mcaja_ahorro_col), 0)]
          }
        }

        # TOTALES (Agregaciones)
        if (run_totales) {
          log_info("Totales y Agregaciones")

          dataset[, total_productos_tarjetas := (ctarjeta_debito + ctarjeta_visa + ctarjeta_master)]
          dataset[, total_productos_cuentas := (ccuenta_corriente + ccaja_ahorro)]
          dataset[, total_productos_prestamos := (cprestamos_prendarios + cprestamos_hipotecarios)]
          dataset[, total_productos_inversiones := (cplazo_fijo + cinversion1 + cinversion2)]
          dataset[, total_productos_seguros := (cseguro_vida + cseguro_auto + cseguro_vivienda + cseguro_accidentes_personales)]

          dataset[, total_trx_digital := (chomebanking_transacciones + catm_trx)]
          dataset[, total_trx_presencial := (ccajas_transacciones + ccallcenter_transacciones)]
          dataset[, total_trx_tarjetas := (ctarjeta_visa_transacciones + ctarjeta_master_transacciones + ctarjeta_debito_transacciones)]

          if (!is.null(visa_msaldototal_col) && !is.null(master_msaldototal_col)) {
            dataset[, saldo_total_tarjetas := (get(visa_msaldototal_col) + get(master_msaldototal_col))]
          }

          if (!is.null(visa_mlimitecompra_col) && !is.null(master_mlimitecompra_col)) {
            dataset[, limite_total_tarjetas := (get(visa_mlimitecompra_col) + get(master_mlimitecompra_col))]
          }

          if (!is.null(visa_mconsumototal_col) && !is.null(master_mconsumototal_col)) {
            dataset[, consumo_total_tarjetas := (get(visa_mconsumototal_col) + get(master_mconsumototal_col))]
          }

          if (!is.null(mcuenta_corriente_col) && !is.null(mcaja_ahorro_col)) {
            dataset[, saldo_total_cuentas := (get(mcuenta_corriente_col) + get(mcaja_ahorro_col))]
          }

          if (!is.null(mcaja_ahorro_dolares_col) && !is.null(mplazo_fijo_dolares_col)) {
            dataset[, saldo_total_dolares := (get(mcaja_ahorro_dolares_col) + get(mplazo_fijo_dolares_col))]
          }

          if (!is.null(mprestamos_prendarios_col) && !is.null(mprestamos_hipotecarios_col)) {
            dataset[, monto_total_prestamos := (get(mprestamos_prendarios_col) + get(mprestamos_hipotecarios_col))]
          }

          if (!is.null(mplazo_fijo_pesos_col) && !is.null(mplazo_fijo_dolares_col) &&
            !is.null(minversion1_pesos_col) && !is.null(minversion1_dolares_col) && !is.null(minversion2_col)) {
            dataset[, monto_total_inversiones := (get(mplazo_fijo_pesos_col) + get(mplazo_fijo_dolares_col) +
              get(minversion1_pesos_col) + get(minversion1_dolares_col) + get(minversion2_col))]
          }

          if (!is.null(mpayroll_col) && !is.null(mpayroll2_col)) {
            dataset[, mpayroll_total := (get(mpayroll_col) + get(mpayroll2_col))]
          }
        }

        # COMPORTAMIENTO
        if (run_comportamiento) {
          log_info("Indicadores de Comportamiento")

          # REGLA DE SEGURIDAD: Si Totales está apagado, calculamos lo necesario localmente para que este bloque no falle.
          if (!"total_productos_tarjetas" %in% names(dataset)) dataset[, total_productos_tarjetas := (ctarjeta_debito + ctarjeta_visa + ctarjeta_master)]
          if (!"total_trx_tarjetas" %in% names(dataset)) dataset[, total_trx_tarjetas := (ctarjeta_visa_transacciones + ctarjeta_master_transacciones + ctarjeta_debito_transacciones)]
          if (!"total_productos_cuentas" %in% names(dataset)) dataset[, total_productos_cuentas := (ccuenta_corriente + ccaja_ahorro)]

          # Intensidad
          dataset[, intensidad_tarjetas := ifelse(total_productos_tarjetas > 0, total_trx_tarjetas / total_productos_tarjetas, 0)]
          dataset[, intensidad_cuentas := ifelse(total_productos_cuentas > 0, (chomebanking_transacciones + ccajas_transacciones) / total_productos_cuentas, 0)]

          # Diversificación (Checks de existencia para robustez)
          if (!"total_productos_prestamos" %in% names(dataset)) dataset[, total_productos_prestamos := (cprestamos_prendarios + cprestamos_hipotecarios)]
          if (!"total_productos_inversiones" %in% names(dataset)) dataset[, total_productos_inversiones := (cplazo_fijo + cinversion1 + cinversion2)]

          dataset[, diversificacion_productos := total_productos_tarjetas + total_productos_cuentas + total_productos_prestamos + total_productos_inversiones]

          dataset[, diversificacion_canales := (as.numeric(chomebanking_transacciones > 0) + as.numeric(ccajas_transacciones > 0) + as.numeric(catm_trx > 0) + as.numeric(ccallcenter_transacciones > 0))]

          # Concentración
          dataset[, concentracion_visa := ifelse(total_trx_tarjetas > 0, ctarjeta_visa_transacciones / total_trx_tarjetas, 0)]

          if (!"total_trx_digital" %in% names(dataset)) dataset[, total_trx_digital := (chomebanking_transacciones + catm_trx)]
          if (!"total_trx_presencial" %in% names(dataset)) dataset[, total_trx_presencial := (ccajas_transacciones + ccallcenter_transacciones)]

          dataset[, concentracion_digital := ifelse((total_trx_digital + total_trx_presencial) > 0, total_trx_digital / (total_trx_digital + total_trx_presencial), 0)]

          dataset[, ratio_productos_activos := ifelse(cproductos > 0, diversificacion_productos / cproductos, 0)]
        }

        # RIESGO
        if (run_riesgo) {
          log_info("Features de Riesgo")

          if (!is.null(visa_msaldopesos_col) && !is.null(master_msaldopesos_col)) {
            dataset[, flag_sobregiro := as.numeric(get(visa_msaldopesos_col) < 0 | get(master_msaldopesos_col) < 0)]
          }

          # Dependencia con Ratios (utilizacion_visa/master): Check de existencia
          if (col_exists("Visa_msaldototal", cols_dataset) && col_exists("Master_msaldototal", cols_dataset) &&
            "utilizacion_visa" %in% names(dataset) && "utilizacion_master" %in% names(dataset)) {
            dataset[, flag_limite_alto := as.numeric(utilizacion_visa > 0.8 | utilizacion_master > 0.8)]
          }

          dataset[, flag_cheques_rechazados := as.numeric(ccheques_emitidos_rechazados > 0)]
          dataset[, flag_mora_visa := as.numeric(!is.na(Visa_Finiciomora))]
          dataset[, flag_mora_master := as.numeric(!is.na(Master_Finiciomora))]
          dataset[, flag_baja_actividad := as.numeric(ctrx_quarter < 5)]

          # Dependencia con Totales (mpayroll_total): Fallback
          if ("mpayroll_total" %in% names(dataset)) {
            dataset[, flag_sin_payroll := as.numeric(mpayroll_total == 0 | is.na(mpayroll_total))]
          } else if (!is.null(mpayroll_col)) {
            dataset[, flag_sin_payroll := as.numeric(get(mpayroll_col) == 0 | is.na(get(mpayroll_col)))]
          }

          dataset[, flag_sin_tarjeta := as.numeric(ctarjeta_visa + ctarjeta_master == 0)]

          # Score simple (Seguro ante falta de flags)
          dataset[, score_riesgo := (ifelse("flag_sobregiro" %in% names(dataset), flag_sobregiro, 0) * 3) +
            (ifelse("flag_limite_alto" %in% names(dataset), flag_limite_alto, 0) * 2) +
            flag_cheques_rechazados * 2 +
            flag_mora_visa * 3 +
            flag_mora_master * 3 +
            flag_baja_actividad * 1]

          # Dependencia con Ratios (ratio_deuda_ingreso): Check de existencia
          if ("ratio_deuda_ingreso" %in% names(dataset)) {
            dataset[, nivel_endeudamiento := ifelse(ratio_deuda_ingreso < 0.3, 1L,
              ifelse(ratio_deuda_ingreso < 0.5, 2L,
                ifelse(ratio_deuda_ingreso < 0.7, 3L,
                  ifelse(ratio_deuda_ingreso < 1.0, 4L, 5L)
                )
              )
            )]
          }
        }
      },
      error = function(e) {
        cat("######################################################\n")
        cat("ERROR en Bloque Extendido de Variables Intra-Mes\n")
        cat("Mensaje:", e$message, "\n")
        cat("######################################################\n")
      }
    )
  }

  #----------------------------------------------------------------------------
  # RESUMEN
  #----------------------------------------------------------------------------
  nuevas_features <- setdiff(colnames(dataset), columnas_originales)
  log_info(paste("Total de nuevas features creadas:", length(nuevas_features)))

  # Liberar memoria
  rm(columnas_originales, nuevas_features)
  gc(verbose = FALSE)

  #----------------------------------------------------------------------------
  # LIMPIEZA FINAL (Infinitos y NaNs)
  #----------------------------------------------------------------------------
  infinitos <- lapply(names(dataset), function(.name) dataset[, sum(is.infinite(get(.name)))])
  nans <- lapply(names(dataset), function(.name) dataset[, sum(is.nan(get(.name)))])

  log_info("fin AgregarVariables_IntraMes()")
}

# Agrego variables
AgregarVariables_IntraMes(
  dataset,
  PARAM$intra_mes$run_combinaciones_moreira,
  PARAM$intra_mes$ejecutar_combinaciones_monetarias,
  PARAM$intra_mes$ejecutar_ratios,
  PARAM$intra_mes$ejecutar_totales,
  PARAM$intra_mes$ejecutar_comportamiento,
  PARAM$intra_mes$ejecutar_riesgo
)
