log_info("Inicio 4_Feature_Engineering_Intra_Mes.R")
# Feature Engineering Intra-Mes
# Crear variables nuevas a partir de las existentes dentro del mismo registro, sin ir a buscar información histórica.
# El siguiente código es un mínimo ejemplo, agregar nuevos features a gusto

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
log_info("Fin 4_Feature_Engineering_Intra_Mes.R")