# Instrucciones para la Entrega Final

Este documento detalla el procedimiento para ejecutar los scripts de la entrega final.

## Secuencia de Ejecución

1.  **Ejecutar los Modelos Individuales:**
    Primero se deben correr los scripts de los modelos. El orden de ejecución de estos archivos no es relevante.
    *   `Modelo_01.R`
    *   `Modelo_02.R`
    *   `Modelo_03.R`
    *   `Modelo_04.R`
    *   `Modelo_05.R`
    *   `Modelo_06.R`

    Cada uno de estos scripts generará sus respectivas predicciones/probabilidades.

2.  **Ejecutar el Ensamble Final:**
    Una vez finalizada la ejecución de todos los modelos individuales, se debe correr el script del ensamble:
    *   `Ensamble_Final.R`

## ⚠️ Importante: Ejecución en Múltiples Máquinas

Si los modelos individuales se ejecutaron en máquinas diferentes o las rutas de los archivos de salida varían respecto a la configuración local, es necesario realizar un ajuste manual antes de correr el ensamble.

**Debe ajustar las rutas en el archivo `Ensamble_Final.R`:**
Busque la variable `archivos_probabilidades` dentro del script y asegúrese de que las rutas apunten correctamente a la ubicación de los archivos de probabilidades generados por los modelos `Modelo_XX.R`.
