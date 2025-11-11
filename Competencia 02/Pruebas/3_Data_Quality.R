#!/usr/bin/env Rscript
# Data Quality
# Se deben reparar los atributos del dataset que para un cierto mes TODOS sus valores son cero.
# Relevar en forma muy minuciosa en el dataset cuales son los  <atributo,mes> que estan dañados.
# Algunas alternativas de solución son:
# * No hacer absolutamente nada, dejar el valor 0 tal cual está, a sabiendas que es incorrecto
# * Reemplazar esos valores dañados por  NA
# * Interpolar cada valor dañado por el valor del mes previo y el posterior
# * Calcularlo a partir de un modelo, libreria  MICE
AsignarNA <- function(pcampo, pmeses) {

  if( pcampo %in% colnames( dataset ) ) {

    dataset[ foto_mes %in% pmeses, paste0(pcampo) := NA ]
  }
}

AsignarNA("active_quarter", c(202006))
AsignarNA("internet", c(202006))

AsignarNA("mrentabilidad", c(201905, 201910, 202006))
AsignarNA("mrentabilidad_annual", c(201905, 201910, 202006))
AsignarNA("mcomisiones", c(201905, 201910, 202006))
AsignarNA("mactivos_margen", c(201905, 201910, 202006))
AsignarNA("mpasivos_margen", c(201905, 201910, 202006))

AsignarNA("mcuentas_saldo", c(202006))
AsignarNA("ctarjeta_debito_transacciones", c(202006))

AsignarNA("mautoservicio", c(202006))

AsignarNA("ctarjeta_visa_transacciones", c(202006))
AsignarNA("mtarjeta_visa_consumo", c(202006))

AsignarNA("ctarjeta_master_transacciones", c(202006))
AsignarNA("mtarjeta_master_consumo", c(202006))

AsignarNA("ctarjeta_visa_debitos_automaticos", c(201904))
AsignarNA("mttarjeta_visa_debitos_automaticos", c(201904))

AsignarNA("ccajeros_propios_descuentos",
c(201910, 202002, 202006, 202009, 202010, 202102))

AsignarNA("mcajeros_propios_descuentos",
c(201910, 202002, 202006, 202009, 202010, 202102))

AsignarNA("ctarjeta_visa_descuentos",
c(201910, 202002, 202006, 202009, 202010, 202102))

AsignarNA("mtarjeta_visa_descuentos",
c(201910, 202002, 202006, 202009, 202010, 202102))

AsignarNA("ctarjeta_master_descuentos",
c(201910, 202002, 202006, 202009, 202010, 202102))

AsignarNA("mtarjeta_master_descuentos",
c(201910, 202002, 202006, 202009, 202010, 202102))

AsignarNA("ccomisiones_otras", c(201905, 201910, 202006))
AsignarNA("mcomisiones_otras", c(201905, 201910, 202006))

AsignarNA("cextraccion_autoservicio", c(202006))
AsignarNA("mextraccion_autoservicio", c(202006))

AsignarNA("ccheques_depositados", c(202006))
AsignarNA("mcheques_depositados", c(202006))
AsignarNA("ccheques_emitidos", c(202006))
AsignarNA("mcheques_emitidos", c(202006))
AsignarNA("ccheques_depositados_rechazados", c(202006))
AsignarNA("mcheques_depositados_rechazados", c(202006))
AsignarNA("ccheques_emitidos_rechazados", c(202006))
AsignarNA("mcheques_emitidos_rechazados", c(202006))

AsignarNA("tcallcenter", c(202006))
AsignarNA("ccallcenter_transacciones", c(202006))

AsignarNA("thomebanking", c(202006))
AsignarNA("chomebanking_transacciones", c(201910, 202006))

AsignarNA("ccajas_transacciones", c(202006))
AsignarNA("ccajas_consultas", c(202006))

AsignarNA("ccajas_depositos", c(202006, 202105))

AsignarNA("ccajas_extracciones", c(202006))
AsignarNA("ccajas_otras", c(202006))

AsignarNA("catm_trx", c(202006))
AsignarNA("matm", c(202006))
AsignarNA("catm_trx_other", c(202006))
AsignarNA("matm_other", c(202006))

AsignarNA("tmobile_app", c(201901, 201902, 201903, 201904, 201905, 201906, 202006))
AsignarNA("cmobile_app_trx", c(201901, 201902, 201903, 201904, 201905, 201906, 202006))