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

  if( atributos_presentes( c("ctrx_quarter_normalizado", "ctarjeta_visa_transacciones") ))
    dataset[, iter_1_var_625 := rowSums(cbind(ctrx_quarter_normalizado, ctarjeta_visa_transacciones), na.rm = TRUE)]

  if( atributos_presentes( c("ctrx_quarter_normalizado", "cpayroll_trx") ))
    dataset[, iter_1_var_614 := rowSums(cbind(ctrx_quarter_normalizado, cpayroll_trx), na.rm = TRUE)]

  if( atributos_presentes( c("vm_status01", "cpayroll_trx") ))
    dataset[, iter_1_var_644 := vm_status01 / cpayroll_trx]

  if( atributos_presentes( c("cpayroll_trx", "ctarjeta_visa_transacciones") ))
    dataset[, iter_1_var_69 := cpayroll_trx * ctarjeta_visa_transacciones]

  if( atributos_presentes( c("cpayroll_trx", "ctarjeta_visa_transacciones") ))
    dataset[, iter_1_var_285 := cpayroll_trx / ctarjeta_visa_transacciones]

  if( atributos_presentes( c("cpayroll_trx", "vmr_mpagominimo") ))
    dataset[, iter_1_var_288 := cpayroll_trx / vmr_mpagominimo]

  if( atributos_presentes( c("cpayroll_trx", "vmr_mpagominimo") ))
    dataset[, iter_1_var_15 := cpayroll_trx / vmr_mpagominimo]

  if( atributos_presentes( c("vm_status01", "tcallcenter") ))
    dataset[, iter_1_var_796 := rowSums(cbind(vm_status01, tcallcenter), na.rm = TRUE)]

  if( atributos_presentes( c("ctrx_quarter_normalizado", "tcallcenter") ))
    dataset[, iter_1_var_227 := ctrx_quarter_normalizado / tcallcenter]

  if( atributos_presentes( c("vm_status01", "tcallcenter") ))
    dataset[, iter_1_var_796 := rowSums(cbind(vm_status01, ctarjeta_visa_transacciones), na.rm = TRUE)]
  
  if( atributos_presentes( c("ctrx_quarter_normalizado", "cliente_edad") ))
    dataset[, iter_1_var_20 := ctrx_quarter_normalizado * cliente_edad]

  if( atributos_presentes( c("ctarjeta_visa_transacciones", "tcallcenter") ))
    dataset[, iter_1_var_227 := ctarjeta_visa_transacciones / tcallcenter]

  if( atributos_presentes( c("ctrx_quarter_normalizado", "vmr_mpagominimo") ))
    dataset[, iter_1_var_796 := rowSums(cbind(ctrx_quarter_normalizado, vmr_mpagominimo), na.rm = TRUE)]

  if( atributos_presentes( c("cpayroll_trx", "vm_status01") ))
    dataset[, iter_1_var_227 := cpayroll_trx / vm_status01]

  if( atributos_presentes( c("tcallcenter", "vmr_mpagominimo") ))
    dataset[, iter_1_var_548 := tcallcenter / vmr_mpagominimo]

  if( atributos_presentes( c("ctrx_quarter_normalizado", "vmr_mpagominimo") ))
    dataset[, iter_1_var_18 := ctrx_quarter_normalizado / vmr_mpagominimo]

  if( atributos_presentes( c("vm_status01", "tcallcenter") ))
    dataset[, iter_1_var_487 := vm_status01 / tcallcenter]  

  if( atributos_presentes( c("cpayroll_trx", "vmr_mpagominimo") ))
    dataset[, iter_1_var_796 := rowSums(cbind(cpayroll_trx, vmr_mpagominimo), na.rm = TRUE)]

  if( atributos_presentes( c("ctrx_quarter_normalizado", "ctarjeta_visa_transacciones", "mpayroll_sobre_edad") ))
    dataset[, iter_2_var_617 := rowSums(cbind(ctrx_quarter_normalizado, ctarjeta_visa_transacciones, mpayroll_sobre_edad), na.rm = TRUE)]

  if( atributos_presentes( c("ctrx_quarter", "ctarjeta_visa_transacciones", "cpayroll_trx", "vmr_mpagominimo") ))
    dataset[, iter_2_var_667 := rowSums(cbind(ctrx_quarter, ctarjeta_visa_transacciones, cpayroll_trx / vmr_mpagominimo), na.rm = TRUE)]

  if( atributos_presentes( c("mprestamos_personales_rank", "mcaja_ahorro_rank") ))
    dataset[, iter_2_var_669 := rowSums(cbind(mprestamos_personales_rank, mcaja_ahorro_rank), na.rm = TRUE)]

  if( atributos_presentes( c("mprestamos_personales_rank", "mpasivos_margen_rank") ))
    dataset[, iter_2_var_674 := rowSums(cbind(mprestamos_personales_rank, mpasivos_margen_rank), na.rm = TRUE)]

  if( atributos_presentes( c("mtarjeta_visa_consumo_rank", "mcuenta_debitos_automaticos_rank") ))
    dataset[, iter_2_var_774 := rowSums(cbind(mtarjeta_visa_consumo_rank, mcuenta_debitos_automaticos_rank), na.rm = TRUE)]

  if( atributos_presentes( c("mprestamos_personales_rank", "mrentabilidad_annual_rank") ))
    dataset[, iter_2_var_673 := rowSums(cbind(mprestamos_personales_rank, mrentabilidad_annual_rank), na.rm = TRUE)]

  if( atributos_presentes( c("mcaja_ahorro_rank", "cpayroll_trx", "ctarjeta_visa_transacciones") ))
    dataset[, iter_2_var_83 := mcaja_ahorro_rank * cpayroll_trx * ctarjeta_visa_transacciones]

  if( atributos_presentes( c("mcuenta_corriente_rank", "mtarjeta_visa_consumo_rank") ))
    dataset[, iter_2_var_481 := mcuenta_corriente_rank / mtarjeta_visa_consumo_rank ]

  if( atributos_presentes( c("mcaja_ahorro_rank", "cpayroll_trx", "vmr_mpagominimo") ))
    dataset[, iter_2_var_78 := rowSums(cbind(cpayroll_trx, vmr_mpagominimo), na.rm = TRUE) * mcaja_ahorro_rank]

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