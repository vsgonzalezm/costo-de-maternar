# ==============================================================================
# PROYECTO FONDECYT: (DEMANDA)
# ==============================================================================

# Cargar librería para diseño algorítmico. Si no está, instalarla.
if(!require(AlgDesign)) install.packages("AlgDesign"); library(AlgDesign)
if(!require(dplyr)) install.packages("dplyr"); library(dplyr) # manipulación de textos

# Definir función auxiliar para redondear N a decenas (facilita la logística)
round10 <- function(x) {
  as.integer(ceiling(x / 10) * 10)
}

# Configurar contrastes suma-cero (recomendación de Wheeler para ortogonalidad)
options(contrasts = c("contr.sum", "contr.poly"))

# Fijar semilla para asegurar que los resultados sean replicables en el futuro
set.seed(12345)
# Definir repeticiones del algoritmo para evitar óptimos locales
nRepeats_default <- 100 # Número de repeticiones para optFederov
# Crear lista vacía para ir guardando los objetos de cada modelo
results_list <- list()

# ==============================================================================
# MODELO: CON DESEMPEÑO (3 NIVELES) 
# ==============================================================================

# 1. Definir niveles: Genero(2), Familia(7), Ocupacion(2), Exp(3), Desempeno(3)
# Aquí "Ocupacion" representa el Bundle de Cargo + Educación
niveles_m2 <- c(2, 7, 2, 3, 3)
nombres_m2 <- c("Genero", "Familia", "Ocupacion", "Exp", "Desempeno")
cand_uni_m2 <- gen.factorial(levels = niveles_m2, varNames = nombres_m2, factors = "all")

# 2. Definir fórmula 
frm_teorica_m2 <- ~ Genero + Familia + Ocupacion + Exp + Desempeno + Genero:Familia 

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
  AlgDesign::optFederov(frml = frm_teorica_m2, # Fórmula teórica
                        data = cand_uni_m2,     # Datos candidatos
                        nTrials = N_final_m2,  # Tamaño del diseño
                        criterion = "D",   # Criterio D-optimalidad
                        nRepeats = nRepeats_default,   # Repeticiones para evitar óptimos locales
                        augment = FALSE, 
                        nullify = 1) # No incluir intercepto en la optimización
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

# los resultados considerando 40 viñetas son de 0.55 D-efficiency y 0.63 Ge, puede ser ajustado pero no estaría mal para el pretest. 



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
cat(sprintf("[M2] Max Abs Correlation: %s | Mean Abs Correlation: %s\n", max_cor_m2, mean_cor_m2))

# Hay que tener ojo en este punto porque el diseño puede no ser tan ortogonal, pero para un pretest puede ser aceptable, aunque por lo que entiendo es por los componentes de familia
# Ver la matriz de correlación completa redondeada a 2 decimales
round(cor(model.matrix(frm_teorica_m2, diseno_final_m2)[, -1]), 2)

# ==============================================================================
# 7. GENERACIÓN DE BLOQUES (BLOCKING) 
# ==============================================================================

# Definimos la función
crear_bloques <- function(diseno, formula) {
  set.seed(999)
  
  # Algoritmo de bloqueo
  bloques_opt <- optBlock(frml = formula, 
                          withinData = diseno, 
                          blocksizes = rep(2, 20), 
                          criterion = "D")
  
  # Extraemos el diseño ordenado
  diseno_salida <- bloques_opt$design
  

  # Creamos manualmente la columna "Block".
  # Como pedimos 20 bloques de 2, asignamos 1,1, 2,2, 3,3... hasta 20.
  diseno_salida$Block <- rep(1:20, each = 2)
  
  return(diseno_salida)
}

# Ejecución
df_con_bloques <- crear_bloques(diseno_final_m2, frm_teorica_m2)
  
  # ============================================================================
  # 8. MAPEO DE TEXTOS 
  # ============================================================================


procesar_textos <- function(df_input) {
  
  # Convertimos a data frame normal
  df_output <- as.data.frame(df_input)
  
  df_output <- df_output %>%
    mutate(
      # 1. Desempaquetar Ocupación/Educación (Bundle)
      Placeholder_Ocupacion = case_when(
        Ocupacion == "1" ~ "Analista contable",
        Ocupacion == "2" ~ "Asociado financiero"
      ),
      Placeholder_Educacion = case_when(
        Ocupacion == "1" ~ "Título técnico en Contabilidad",
        Ocupacion == "2" ~ "Título profesional en Ingeniería Comercial"
      ),
      
      # 2. Mapear Experiencia y Desempeño
      Placeholder_Exp = case_when(
        Exp == "1" ~ "6 años", 
        Exp == "2" ~ "7 años", 
        Exp == "3" ~ "8 años"
      ),
      Placeholder_Desempeno = case_when(
        Desempeno == "1" ~ "5.8", 
        Desempeno == "2" ~ "6.0", 
        Desempeno == "3" ~ "6.3"
      ),
      
      # 3. Asignar Género (Texto)
      Texto_Genero = if_else(Genero == "1", "Mujer", "Hombre")
    ) 
  
  # 4. Asignar Nombres Aleatorios
  n_filas <- nrow(df_output)
  set.seed(2025)
  nombres_mujer <- sample(c("Marcela", "Natalia"), n_filas, replace = TRUE)
  nombres_hombre <- sample(c("Claudio", "Patricio"), n_filas, replace = TRUE)
  
  df_output <- df_output %>%
    mutate(
      Placeholder_Nombre = if_else(Texto_Genero == "Mujer", nombres_mujer, nombres_hombre)
    )
  
  # 5. Mapear Familia (Textos largos exactos)
  textos_familia <- c(
    "1" = "se encuentra soltera/a, actualmente no tiene hijos y no tiene planes de tenerlos en los próximos años.",
    "2" = "se encuentra conviviendo con su pareja, no tienen hijos y le gustaría tener en los próximos años.",
    "3" = "se encuentra casado/a, no tienen hijos y le gustaría tener en los próximos años.",
    "4" = "se encuentra conviviendo con su pareja, tienen un hijo en edad preescolar y le gustaría tener otro hijo en los próximos años.",
    "5" = "se encuentra casado/a, tienen un hijo en edad preescolar y le gustaría tener otro hijo en los próximos años.",
    "6" = "se encuentra conviviendo con su pareja, tienen dos hijos en educación básica y no quiere tener más hijos.",
    "7" = "se encuentra casado/a, tienen dos hijos en educación básica y no quiere tener más hijos."
  )
  
  # Aplicamos el mapeo asegurando que Familia sea caracter para buscar
  df_output <- df_output %>%
    mutate(Familia = recode(as.character(Familia), !!!textos_familia))
  
  return(df_output)
}

# Ejecutar procesar textos
df_final <- procesar_textos(df_con_bloques)

  # ============================================================================
  # 9. EXPORTAR ARCHIVO DE VIÑETAS (
  # ============================================================================

# 1. Definimos qué columnas queremos
cols_finales <- c("Block", "Texto_Genero", "Placeholder_Nombre", "Familia", 
                  "Placeholder_Ocupacion", "Placeholder_Educacion", 
                  "Placeholder_Exp", "Placeholder_Desempeno")

# 2. Seleccionamos columnas 
archivo_vinetas <- df_final[, cols_finales]

# 3. Ordenamos por número de Bloque

archivo_vinetas <- archivo_vinetas[order(archivo_vinetas$Block), ]

# 4. GUARDAR EL ARCHIVO
write.csv(archivo_vinetas, "1_Base_Vinetas_Netquest.csv", row.names = FALSE)
#saveRDS(archivo_vinetas, "1_Base_Vinetas_Netquest.rds")


# 1. Creamos la asignación balanceada (200 personas, 20 bloques)
set.seed(777)
asignacion <- sample(rep(1:20, each = 10))

# 2. Creamos la tabla
lista_participantes <- data.frame(
  ID_Participante = 1:200,
  Bloque_Asignado = asignacion
)

# 3. GUARDAR EL ARCHIVO
write.csv(lista_participantes, "2_Asignacion_Participantes.csv", row.names = FALSE)
#saveRDS(lista_participantes, "2_Asignacion_Participantes.rds")
