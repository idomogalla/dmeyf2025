# install.packages("data.table")
library(data.table)

analizar_wilcoxon_progresivo <- function(file1_path, file2_path, 
                                        max_iteraciones = 30, 
                                        alpha = 0.05,
                                        min_iteraciones = 5) {
  
  cat(paste("--- Iniciando Análisis Wilcoxon ---\n"))
  cat(paste("  max_iteraciones =", max_iteraciones, "\n"))
  cat(paste("  alpha (umbral)  =", alpha, "\n"))
  cat(paste("  min_iteraciones =", min_iteraciones, "\n"))
  cat("---------------------------------------\n")
  
  # --- Cargar los datos ---
  tryCatch({
    data1 <- data.table::fread(file1_path, sep = "\t", nrows = max_iteraciones)
    data2 <- data.table::fread(file2_path, sep = "\t", nrows = max_iteraciones)
  }, error = function(e) {
    stop(paste("Error al leer uno de los archivos:", e$message))
  })
  
  # --- Validar Datos ---
  if (!"max_ganancia" %in% colnames(data1) || !"max_ganancia" %in% colnames(data2)) {
    stop("La columna 'max_ganancia' no se encontró en uno o ambos archivos.")
  }
  
  vector1 <- data1$max_ganancia
  vector2 <- data2$max_ganancia
  
  # Determinar el límite real de iteraciones
  n_disponible <- min(length(vector1), length(vector2))
  
  if (n_disponible < min_iteraciones) {
    stop(paste("No hay suficientes datos. Se requieren al menos", min_iteraciones, "filas, pero solo hay", n_disponible))
  }
  
  # Ajustar max_iteraciones si los archivos son más cortos
  if (max_iteraciones > n_disponible) {
    cat(paste("Advertencia: max_iteraciones (", max_iteraciones, ") es mayor que las filas disponibles (", n_disponible, ").\n"))
    cat(paste("           Usando", n_disponible, "como máximo.\n"))
    max_iteraciones <- n_disponible
  }
  
  # --- Iteración Progresiva ---
  
  significance_found <- FALSE
  final_i <- max_iteraciones
  final_p_value <- NA
  
  for (i in min_iteraciones:max_iteraciones) {
    
    # Subconjuntos progresivos
    v1_sub <- vector1[1:i]
    v2_sub <- vector2[1:i]
    
    # Ejecutar el test (suprimir warnings por empates, que son comunes)
    test_result <- suppressWarnings(wilcox.test(v1_sub, v2_sub))
    current_p_value <- test_result$p.value
    
    final_p_value <- current_p_value # Guardar siempre el último p-valor
    
    # --- Verificación de corte de flujo ---
    if (!is.na(current_p_value) && current_p_value < alpha) {
      cat(paste("\n*** Corte de Flujo en Iteración", i, "***\n"))
      cat(paste("    P-Valor:", format(current_p_value, scientific = FALSE, digits = 5), "(<", alpha, ")\n"))
      significance_found <- TRUE
      final_i <- i
      break # Detener el bucle
    }
  }
  
  if (!significance_found) {
    cat(paste("\n-> Flujo completado: El test alcanzó", max_iteraciones, "iteraciones.\n"))
    cat(paste("   El p-valor nunca fue menor que", alpha, ".\n"))
  }
  
  # --- Reporte Final ---
  
  cat("\n--- Resultados Finales (en iteración", final_i, ") ---\n")
  cat(paste("  P-Valor:", format(final_p_value, scientific = FALSE, digits = 5), "\n"))
  
  if (significance_found) {
    cat("  Conclusión: Significativo.\n")
    cat("             -> Se encontró diferencia estadística.\n")
  } else {
    cat("  Conclusión: No Significativo.\n")
    cat(paste("             -> No hay diferencia estadística con", final_i, "iteraciones.\n"))
  }
  
  # Calcular promedios acumulados en el punto de corte
  avg1 <- mean(vector1[1:final_i])
  avg2 <- mean(vector2[1:final_i])
  
  cat("\n--- Promedio Acumulado (en iteración", final_i, ") ---\n")
  cat(paste("  Promedio Archivo 1:", format(avg1, big.mark = ",", scientific = FALSE, nsmall = 2), "\n"))
  cat(paste("  Promedio Archivo 2:", format(avg2, big.mark = ",", scientific = FALSE, nsmall = 2), "\n"))
  
  # Devolver resultados
  invisible(list(
    stopped_at_iteration = final_i,
    p_value = final_p_value,
    is_significant = significance_found,
    average_model_1 = avg1,
    average_model_2 = avg2
  ))
}


# Cambiar el máximo de iteraciones y el alpha:
analizar_wilcoxon_progresivo("c02_p4z.txt", "c02_p5z.txt", 
                              max_iteraciones = 30, 
                              alpha = 0.01)