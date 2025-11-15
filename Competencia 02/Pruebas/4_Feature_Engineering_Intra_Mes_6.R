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
    dataset[, attr1 := rowSums(cbind(ctrx_quarter_normalizado, ctarjeta_visa_transacciones), na.rm = TRUE)]

  if( atributos_presentes( c("ctrx_quarter_normalizado", "cpayroll_trx") ))
    dataset[, attr2 := rowSums(cbind(ctrx_quarter_normalizado, cpayroll_trx), na.rm = TRUE)]

  if( atributos_presentes( c("cpayroll_trx", "ctarjeta_visa_transacciones") ))
    dataset[, attr3 := cpayroll_trx * ctarjeta_visa_transacciones]

  if( atributos_presentes( c("cpayroll_trx", "ctarjeta_visa_transacciones") ))
    dataset[, attr4 := cpayroll_trx / ctarjeta_visa_transacciones]

  if( atributos_presentes( c("ctrx_quarter_normalizado", "tcallcenter") ))
    dataset[, attr5 := ctrx_quarter_normalizado / tcallcenter]

  if( atributos_presentes( c("ctrx_quarter_normalizado", "cliente_edad") ))
    dataset[, attr6 := ctrx_quarter_normalizado * cliente_edad]

  if( atributos_presentes( c("ctarjeta_visa_transacciones", "tcallcenter") ))
    dataset[, attr7 := ctarjeta_visa_transacciones / tcallcenter]

  if( atributos_presentes( c("ctrx_quarter_normalizado", "ctarjeta_visa_transacciones", "mpayroll_sobre_edad") ))
    dataset[, attr8 := rowSums(cbind(ctrx_quarter_normalizado, ctarjeta_visa_transacciones, mpayroll_sobre_edad), na.rm = TRUE)]

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