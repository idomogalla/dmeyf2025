#!/usr/bin/env Rscript
# Feature Engineering Intra-Mes
# Crear variables nuevas a partir de las existentes dentro del mismo registro, sin ir a buscar información histórica.
# El siguiente código es un mínimo ejemplo, agregar nuevos features a gusto

#------------------------------------------------------------------------------
# Función auxiliar para verificar la existencia de columnas
#------------------------------------------------------------------------------
atributos_presentes <- function( patributos )
{
  atributos <- unique( patributos )
  comun <- intersect( atributos, colnames(dataset) )

  return(  length( atributos ) == length( comun ) )
}

#------------------------------------------------------------------------------
# Esta es la parte que los alumnos deben desplegar todo su ingenio
# Agregar aqui sus PROPIAS VARIABLES manuales
#------------------------------------------------------------------------------
AgregarVariables_IntraMes <- function(dataset) {
  log_info( "inicio AgregarVariables_IntraMes()")
  gc(verbose= FALSE)

  #----------------------------------------------------------------------------
  # Agrego las variables originales del script
  #----------------------------------------------------------------------------

  # el mes 1,2, ..12 , podria servir para detectar estacionalidad
  log_info("Creando feature 'kmes'")
  dataset[, kmes := foto_mes %% 100]

  # creo un ctr_quarter que tenga en cuenta cuando
  # los clientes hace 3 menos meses que estan
  # ya que seria injusto considerar las transacciones medidas en menor tiempo
  log_info("Creando feature 'ctrx_quarter_normalizado'")
  dataset[, ctrx_quarter_normalizado := as.numeric(ctrx_quarter) ]
  dataset[cliente_antiguedad == 1, ctrx_quarter_normalizado := ctrx_quarter * 5.0]
  dataset[cliente_antiguedad == 2, ctrx_quarter_normalizado := ctrx_quarter * 2.0]
  dataset[cliente_antiguedad == 3, ctrx_quarter_normalizado := ctrx_quarter * 1.2]

  # variable extraida de una tesis de maestria de Irlanda, se perdió el link
  log_info("Creando feature 'mpayroll_sobre_edad'")
  dataset[, mpayroll_sobre_edad := mpayroll / cliente_edad]


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

  #----------------------------------------------------------------------------
  # valvula de seguridad para evitar valores infinitos
  # paso los infinitos a NULOS
  #----------------------------------------------------------------------------
  infinitos <- lapply(
    names(dataset),
    function(.name) dataset[, sum(is.infinite(get(.name)))]
  )

  #----------------------------------------------------------------------------
  # valvula de seguridad para evitar valores NaN  que es 0/0
  # paso los NaN a 0 , decision polemica si las hay
  # se invita a asignar un valor razonable segun la semantica del campo creado
  #----------------------------------------------------------------------------
  nans <- lapply(
    names(dataset),
    function(.name) dataset[, sum(is.nan(get(.name)))]
  )
  
  log_info( "fin AgregarVariables_IntraMes()")
}

#------------------------------------------------------------------------------
# EJECUTO LA FUNCION
#------------------------------------------------------------------------------
AgregarVariables_IntraMes(dataset)