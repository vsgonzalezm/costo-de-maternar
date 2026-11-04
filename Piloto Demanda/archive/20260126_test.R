# ==============================================================================
# PROYECTO FONDECYT: (DEMANDA)
# ==============================================================================

# Cargar librería para diseño algorítmico. Si no está, instalarla.
if(!require(AlgDesign)) install.packages("AlgDesign"); library(AlgDesign)

# Definir función auxiliar para redondear N a decenas (facilita la logística)
round10 <- function(x) {
  as.integer(ceiling(x / 10) * 10)
}

# Configurar contrastes suma-cero (recomendación de Wheeler para ortogonalidad)
options(contrasts = c("contr.sum", "contr.poly"))

# Fijar semilla para asegurar que los resultados sean replicables en el futuro
set.seed(12345)

# Definir repeticiones del algoritmo para evitar óptimos locales
nRepeats_default <- 50 # Número de repeticiones para optFederov

# Crear lista vacía para ir guardando los objetos de cada modelo
results_list <- list()

# ==============================================================================
# MODELO 1: SIN DESEMPEÑO (El más simple)
# ==============================================================================

# 1. Definir atributos y niveles teóricos
# Genero(2), Familia(7), Ocupacion(2), Exp(2) -> CORREGIDO: Exp es 2 niveles según tu instrucción
niveles_m1 <- c(2, 7, 2, 2)
nombres_m1 <- c("Genero", "Familia", "Ocupacion", "Exp")

# Generar el universo total de combinaciones posibles (Candidate Set)
cand_uni_m1 <- gen.factorial(levels = niveles_m1, varNames = nombres_m1, factors = "all")

# 2. Especificar la fórmula del modelo a estimar
# Incluye efectos principales e interacciones clave (GxF, GxO, etc.)
frm_teorica_m1 <- ~ Genero + Familia + Ocupacion + Exp +
  Genero:Familia + Genero:Ocupacion + Ocupacion:Familia + Genero:Ocupacion:Familia

# 3. Calcular tamaño muestral necesario
# Obtener número de parámetros (k) a estimar
matriz_X_m1 <- model.matrix(frm_teorica_m1, cand_uni_m1)
k_m1 <- ncol(matriz_X_m1)
cat("\n[M1] Parámetros a estimar (k):", k_m1, "\n")

# Definir N objetivo: Buscamos al menos el doble de observaciones que parámetros
N_recomendado_m1 <- ceiling(2 * k_m1)
N_final_m1 <- round10(N_recomendado_m1)

# Ajustes de seguridad por si el N calculado excede el universo disponible
if (N_final_m1 > nrow(cand_uni_m1)) {
  N_final_m1 <- nrow(cand_uni_m1)
}

cat("[M1] Tamaño muestral definido (N):", N_final_m1, "\n")

# 4. Ejecutar el algoritmo de optimización (optFederov)
set.seed(12345)
opt_m1 <- tryCatch({
  AlgDesign::optFederov(frml = frm_teorica_m1, # Fórmula del modelo
                        data = cand_uni_m1, # Datos del universo candidato
                        nTrials = N_final_m1, # Número de filas en el diseño final
                        criterion = "D",         # Maximizar D-Efficiency
                        nRepeats = nRepeats_default, # Número de reinicios para evitar óptimos locales
                        augment = FALSE,   # No agregar puntos, solo seleccionar
                        nullify = 1)             # Usar anulación para el diseño inicial
}, error = function(e) {
  warning("[M1] Error en optFederov. Se usará muestreo aleatorio como respaldo.")
  NULL
})

# Guardar el diseño resultante
if (!is.null(opt_m1)) {
  diseno_final_m1 <- as.data.frame(opt_m1$design, stringsAsFactors = TRUE)
} else {
  set.seed(12345 + 777)
  sel_m1 <- sample.int(nrow(cand_uni_m1), N_final_m1, replace = FALSE)
  diseno_final_m1 <- cand_uni_m1[sel_m1, , drop = FALSE]
}

# 5. Diagnosticar la calidad del diseño (Eficiencia D y G)
eval_final_m1 <- tryCatch({
  AlgDesign::eval.design(frm_teorica_m1, diseno_final_m1, X = cand_uni_m1)
}, error = function(e) { NULL })

# Extraer métricas clave
De_m1 <- NA; Ge_m1 <- NA        
if (!is.null(eval_final_m1)) {
  De_m1 <- round(as.numeric(eval_final_m1$De), 3)
  Ge_m1 <- round(as.numeric(eval_final_m1$Ge), 3)
}

cat(sprintf("[M1] D-Efficiency: %s | Ge: %s\n", De_m1, Ge_m1))  

# 6. Verificar ortogonalidad
matriz_correlacion_m1 <- tryCatch(model.matrix(frm_teorica_m1, diseno_final_m1), error = function(e) NULL)
max_cor_m1 <- NA; mean_cor_m1 <- NA

if (!is.null(matriz_correlacion_m1)) {
  mm_no_int_m1 <- matriz_correlacion_m1[, -1, drop = FALSE]
  if (ncol(mm_no_int_m1) >= 2) {
    cor_matrix_m1 <- suppressWarnings(cor(mm_no_int_m1))
    cor_values_m1 <- cor_matrix_m1[upper.tri(cor_matrix_m1)]
    max_cor_m1 <- round(max(abs(cor_values_m1), na.rm = TRUE), 3)
    mean_cor_m1 <- round(mean(abs(cor_values_m1), na.rm = TRUE), 3)
  }
}

print(matriz_correlacion_m1)
# Guardar M1
results_list$M1 <- list(cand_uni = cand_uni_m1, frm = frm_teorica_m1, k = k_m1, 
                        N_final = N_final_m1, opt = opt_m1, design = diseno_final_m1, 
                        De = De_m1, Ge = Ge_m1, MaxAbsCor = max_cor_m1)


# ==============================================================================
# MODELO 2: CON DESEMPEÑO (3 NIVELES) - EL MÁS COMPLEJO
# ==============================================================================

# 1. Definir niveles: Genero(2), Familia(7), Ocupacion(2), Exp(2), Desempeno(3)
niveles_m2 <- c(2, 7, 2, 2, 3)
nombres_m2 <- c("Genero", "Familia", "Ocupacion", "Exp", "Desempeno")
cand_uni_m2 <- gen.factorial(levels = niveles_m2, varNames = nombres_m2, factors = "all")

# 2. Definir fórmula compleja
frm_teorica_m2 <- ~ Genero + Familia + Ocupacion + Exp + Desempeno +
  Genero:Familia + Genero:Ocupacion + Ocupacion:Familia + Genero:Ocupacion:Familia +
  Genero:Desempeno + Familia:Desempeno + Genero:Desempeno:Familia

# 3. Calcular parámetros y N
matriz_X_m2 <- model.matrix(frm_teorica_m2, cand_uni_m2)
k_m2 <- ncol(matriz_X_m2)
cat("\n[M2] Parámetros a estimar (k):", k_m2, "\n")

N_recomendado_m2 <- ceiling(2 * k_m2)
N_final_m2 <- round10(N_recomendado_m2)

if (N_final_m2 > nrow(cand_uni_m2)) N_final_m2 <- nrow(cand_uni_m2)
cat("[M2] Tamaño muestral definido (N):", N_final_m2, "\n")

# 4. Optimización
set.seed(12346)
opt_m2 <- tryCatch({
  AlgDesign::optFederov(frml = frm_teorica_m2, data = cand_uni_m2, nTrials = N_final_m2,
                        criterion = "D", nRepeats = nRepeats_default, augment = FALSE, nullify = 1)
}, error = function(e) { NULL })

if (!is.null(opt_m2)) {
  diseno_final_m2 <- as.data.frame(opt_m2$design, stringsAsFactors = TRUE)
} else {
  set.seed(12346 + 777)
  sel_m2 <- sample.int(nrow(cand_uni_m2), N_final_m2, replace = FALSE)
  diseno_final_m2 <- cand_uni_m2[sel_m2, , drop = FALSE]
}

# 5. Diagnóstico
eval_final_m2 <- tryCatch({
  AlgDesign::eval.design(frm_teorica_m2, diseno_final_m2, X = cand_uni_m2)
}, error = function(e) { NULL })

De_m2 <- NA; Ge_m2 <- NA
if (!is.null(eval_final_m2)) {
  De_m2 <- round(as.numeric(eval_final_m2$De), 3)
  Ge_m2 <- round(as.numeric(eval_final_m2$Ge), 3)
}
cat(sprintf("[M2] D-Efficiency: %s | Ge: %s\n", De_m2, Ge_m2))

# 6. Check Ortogonalidad
matriz_correlacion_m2 <- tryCatch(model.matrix(frm_teorica_m2, diseno_final_m2), error = function(e) NULL)
max_cor_m2 <- NA; mean_cor_m2 <- NA
if (!is.null(matriz_correlacion_m2)) {
  mm_no_int_m2 <- matriz_correlacion_m2[, -1, drop = FALSE]
  if (ncol(mm_no_int_m2) >= 2) {
    cor_matrix_m2 <- suppressWarnings(cor(mm_no_int_m2))
    cor_values_m2 <- cor_matrix_m2[upper.tri(cor_matrix_m2)]
    max_cor_m2 <- round(max(abs(cor_values_m2), na.rm = TRUE), 3)
    mean_cor_m2 <- round(mean(abs(cor_values_m2), na.rm = TRUE), 3)
  }
}

results_list$M2 <- list(cand_uni = cand_uni_m2, frm = frm_teorica_m2, k = k_m2, 
                        N_final = N_final_m2, opt = opt_m2, design = diseno_final_m2, 
                        De = De_m2, Ge = Ge_m2, MaxAbsCor = max_cor_m2)


# ==============================================================================
# MODELO 3: CON DESEMPEÑO (2 NIVELES) - ALTERNATIVA INTERMEDIA
# ==============================================================================

# 1. Definir niveles: Genero(2), Familia(7), Ocupacion(2), Exp(2), Desempeno(2)
niveles_m3 <- c(2, 7, 2, 2, 2)
nombres_m3 <- c("Genero", "Familia", "Ocupacion", "Exp", "Desempeno")
cand_uni_m3 <- gen.factorial(levels = niveles_m3, varNames = nombres_m3, factors = "all")

# 2. Fórmula (Misma estructura que M2)
frm_teorica_m3 <- frm_teorica_m2

# 3. Calcular parámetros y N
matriz_X_m3 <- model.matrix(frm_teorica_m3, cand_uni_m3)
k_m3 <- ncol(matriz_X_m3)
cat("\n[M3] Parámetros a estimar (k):", k_m3, "\n")

N_recomendado_m3 <- ceiling(2 * k_m3)
N_final_m3 <- round10(N_recomendado_m3)
cat("[M3] Tamaño muestral definido (N):", N_final_m3, "\n")

# 4. Optimización
set.seed(12347)
opt_m3 <- tryCatch({
  AlgDesign::optFederov(frml = frm_teorica_m3, data = cand_uni_m3, nTrials = N_final_m3,
                        criterion = "D", nRepeats = nRepeats_default, augment = FALSE, nullify = 1)
}, error = function(e) { NULL })

if (!is.null(opt_m3)) {
  diseno_final_m3 <- as.data.frame(opt_m3$design, stringsAsFactors = TRUE)
} else {
  set.seed(12347 + 777)
  sel_m3 <- sample.int(nrow(cand_uni_m3), N_final_m3, replace = FALSE)
  diseno_final_m3 <- cand_uni_m3[sel_m3, , drop = FALSE]
}

# 5. Diagnóstico
eval_final_m3 <- tryCatch({
  AlgDesign::eval.design(frm_teorica_m3, diseno_final_m3, X = cand_uni_m3)
}, error = function(e) { NULL })

De_m3 <- NA; Ge_m3 <- NA
if (!is.null(eval_final_m3)) {
  De_m3 <- round(as.numeric(eval_final_m3$De), 3)
  Ge_m3 <- round(as.numeric(eval_final_m3$Ge), 3)
}
cat(sprintf("[M3] D-Efficiency: %s | Ge: %s\n", De_m3, Ge_m3))

# 6. Check Ortogonalidad
matriz_correlacion_m3 <- tryCatch(model.matrix(frm_teorica_m3, diseno_final_m3), error = function(e) NULL)
max_cor_m3 <- NA; mean_cor_m3 <- NA
if (!is.null(matriz_correlacion_m3)) {
  mm_no_int_m3 <- matriz_correlacion_m3[, -1, drop = FALSE]
  if (ncol(mm_no_int_m3) >= 2) {
    cor_matrix_m3 <- suppressWarnings(cor(mm_no_int_m3))
    cor_values_m3 <- cor_matrix_m3[upper.tri(cor_matrix_m3)]
    max_cor_m3 <- round(max(abs(cor_values_m3), na.rm = TRUE), 3)
    mean_cor_m3 <- round(mean(abs(cor_values_m3), na.rm = TRUE), 3)
  }
}

results_list$M3 <- list(cand_uni = cand_uni_m3, frm = frm_teorica_m3, k = k_m3, 
                        N_final = N_final_m3, opt = opt_m3, design = diseno_final_m3, 
                        De = De_m3, Ge = Ge_m3, MaxAbsCor = max_cor_m3)

# ==============================================================================
# TABLA RESUMEN PARA LA REUNIÓN
# ==============================================================================
summary_df <- data.frame(
  Modelo = c("M1", "M2", "M3"),
  Parametros = c(results_list$M1$k, results_list$M2$k, results_list$M3$k),
  N_Vinetas = c(results_list$M1$N_final, results_list$M2$N_final, results_list$M3$N_final),
  D_Eff = c(results_list$M1$De, results_list$M2$De, results_list$M3$De),
  G_Eff = c(results_list$M1$Ge, results_list$M2$Ge, results_list$M3$Ge),
  Max_Cor = c(results_list$M1$MaxAbsCor, results_list$M2$MaxAbsCor, results_list$M3$MaxAbsCor),
  stringsAsFactors = FALSE
)

# Tabla de resumen
print(summary_df, row.names = FALSE)

# 1. PARAMETROS (k): "El Costo"
#    - Indica la complejidad del modelo (grados de libertad consumidos).
#    - Incluye: Intercepto + Efectos Principales (Niveles-1) + Interacciones.

# 2. N_VINETAS: "El Menú Óptimo"
#    -  Es el conjunto de viñetas ÚNICAS seleccionadas por el algoritmo (Candidate Set reducido).
#    - Se recomienda que este número sea > 2*k o 3*k.
#    - En M2: 110 viñetas > 57 parámetros.
#    - Nota: Estas 110 viñetas se repiten aleatoriamente entre los 2,000 encuestadxs.

# 3. D_Eff (D-Efficiency): "Precisión de los Betas"
#    - Mide la calidad de la matriz para estimar los coeficientes.
#    - Rango: 0 a 1 (Donde 1 es un diseño ortogonal perfecto).
#    - Interpretación para Fondecyt:
#      * M1 (1.000): Perfecto, pero el modelo es demasiado simple (poco realista).
#      * M2 (0.754): MUY BUENO para un modelo con triples interacciones. >0.70 es robusto.
#      * M3 (0.643): Curiosamente menos eficiente que M2. Al reducir niveles de desempeño,
#        el algoritmo tuvo menos flexibilidad para acomodar las interacciones.

# 4. G_Eff (G-Efficiency): "Seguridad de Predicción"
#    - Mide el "peor caso" de varianza en la predicción a lo largo del diseño.
#    - Wheeler recomienda mirar esto para evitar "puntos ciegos".
#    - M2 (0.780) es superior a M3 (0.694), indicando que M2 es más estable globalmente.

# 5. Max_Cor (Correlación Máxima): "Ortogonalidad"
#    - Mide la independencia entre variables (0 es independencia total).
#    - Valor de ~0.50: Es NORMAL y ESPERADO cuando hay términos de interacción.
#      (Ej: La interacción "Mujer:Hijos" siempre correlaciona parcialmente con "Mujer").
#    - Lo vital es que los Efectos Principales (Genero, Familia, etc.) tengan correlación cercana a 0.
# ==============================================================================
# GENERACIÓN DE BLOQUES (DECKS)
# ==============================================================================
# Aquí tomamos el diseño elegido ( M2) y lo agrupamos en paquetes para encuestar.

# Usamos el diseño del M2 como base (que ya está optimizado)
diseno_base <- results_list$M2$design # Diseño final del M2
formula_base <- results_list$M2$frm   # Fórmula del M2

# Definir el tamaño del bloque: 2 viñetas por persona
# Si tenemos N viñetas en total, necesitamos N/2 bloques.
n_total_vinetas <- nrow(diseno_base)
n_bloques <- floor(n_total_vinetas / 2) # Número de bloques de 2 viñetas

# Ejecutar el bloqueo usando el criterio D
set.seed(12345)
bloqueo_optimo <- tryCatch({                                      # Usamos tryCatch para manejar errores
  AlgDesign::optBlock(frml = formula_base, 
                      withinData = diseno_base,
                      blocksizes = rep(2, n_bloques), 
                      criterion = "D") 
}, error = function(e) {
  warning("Error al generar bloques.")
  NULL
})

if(!is.null(bloqueo_optimo)){
  cat("Bloqueo completado exitosamente.\n")
  cat("Total de Decks generados:", length(bloqueo_optimo$Blocks), "\n")
  cat("Ejemplo del Deck 1 (lo que ve el primer encuestado):\n")
  print(bloqueo_optimo$Blocks$B1)
  
  # Revisar la calidad del bloqueo (Diagonality)
  cat(sprintf("\nEficiencia del Bloqueo (Diagonality): %s\n", round(bloqueo_optimo$diagonality, 3)))
}

# ==============================================================================
# SIMULACIÓN DE RESULTADOS 
# ==============================================================================

# 1. Expandir el diseño a la muestra total (4,000 obs aprox)
n_total <- 4000
filas_optimas <- results_list$M2$design
n_repeticiones <- ceiling(n_total / nrow(filas_optimas))
datos_simulados <- filas_optimas[rep(1:nrow(filas_optimas), times = n_repeticiones), ]
datos_simulados <- datos_simulados[1:n_total, ] # Recortar exacto a 4000

# 2. Definir los Coeficientes (Inventados para la simulacion)
# Intercepto base (Puntaje promedio)
beta_0 <- 7 

# Generar efectos condicionales
# Genero: Nivel 1 = Hombre (+0.5), Nivel 2 = Mujer (-0.5)
efecto_genero <- ifelse(datos_simulados$Genero == "1", 0.5, -0.5) 

# Familia: Niveles 5, 6, 7 (Muchos hijos) castigan (-1.0). El resto 0.
efecto_hijos  <- ifelse(as.numeric(datos_simulados$Familia) > 4, -1.0, 0.0)

# Desempeño: Nivel 3 (Alto) premia (+2.0), Nivel 1 (Bajo) castiga (-2.0)
efecto_desemp <- ifelse(datos_simulados$Desempeno == "3", 2.0, 
                        ifelse(datos_simulados$Desempeno == "1", -2.0, 0))

# 3. Generar la Respuesta (Y) agregando RUIDO aleatorio
set.seed(999)
ruido <- rnorm(n_total, mean = 0, sd = 1.5) # Error aleatorio normal

# Creamos la variable final en el data frame
datos_simulados$Evaluacion_Y <- beta_0 + efecto_genero + efecto_hijos + efecto_desemp + ruido

# 4. Correr el Modelo (Corrección: Usamos update para agregar la Y a la fórmula)
# update() toma la fórmula del diseño (X) y le agrega la respuesta (Y)
frm_completa <- update(results_list$M2$frm, Evaluacion_Y ~ .)
modelo_simulado <- lm(frm_completa, data = datos_simulados)

# 5. Mostrar Resultados
cat("\nRESULTADOS DE LA SIMULACIÓN (Recuperación de Parámetros):\n")
print(summary(modelo_simulado)$coefficients[1:12, ]) 

