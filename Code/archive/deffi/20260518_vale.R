if(!require(AlgDesign)) install.packages("AlgDesign"); library(AlgDesign)
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)

round10 <- function(x) { as.integer(ceiling(x / 10) * 10) }
options(contrasts = c("contr.sum", "contr.poly"))
set.seed(12345)

# ==============================================================================
# 1. DEFINIR LOS NIVELES (AJUSTADO)
# Ecivil bajó a 2. Ocupación SE ELIMINÓ de la matriz ortogonal (será Between-Subjects).
# ==============================================================================
niveles_nuevos <- c(2, 2, 3, 3, 3, 3) 
nombres_nuevos <- c("Genero", "Ecivil", "Eparental", "Planfamilia", "Exp", "Desempeno")
cand_uni <- gen.factorial(levels = niveles_nuevos, varNames = nombres_nuevos, factors = "all")

# ==============================================================================
# 2. DEFINICIÓN DEL MODELO
# ==============================================================================
frm_final <- ~ Genero * Ecivil * Eparental * Planfamilia + Exp + Desempeno

# ==============================================================================
# 3. CÁLCULO DE N Y OPTIMIZACIÓN
# ==============================================================================
k_final <- ncol(model.matrix(frm_final, cand_uni)) # Esto ahora dará 40 parámetros
N_final <- round10(3 * k_final) # Esto ahora dará 120 viñetas

cat("\n[REPORTE] Parámetros (k):", k_final, "| N sugerido:", N_final, "\n")

# Búsqueda del diseño óptimo
opt_diseno <- optFederov(frml = frm_final, data = cand_uni, nTrials = N_final, 
                         criterion = "D", nRepeats = 100, nullify = 1)

diseno_final <- as.data.frame(opt_diseno$design)
eval_final <- eval.design(frm_final, diseno_final, X = cand_uni)

cat(sprintf("[MÉTRICAS] D-Efficiency: %.3f | G-Efficiency: %.3f\n", eval_final$De, eval_final$Ge))

# ==============================================================================
# 4. GENERACIÓN DE BLOQUES Y POSTULANTE A/B
# ==============================================================================
set.seed(999)
n_por_bloque <- 2
total_bloques <- N_final / n_por_bloque # 120 / 2 = 60 Bloques

bloques_opt <- optBlock(frml = frm_final, withinData = diseno_final, 
                        blocksizes = rep(n_por_bloque, total_bloques), criterion = "D")

df_con_bloques <- as.data.frame(bloques_opt$design)
df_con_bloques$Block <- rep(1:total_bloques, each = n_por_bloque)
df_con_bloques$Postulante <- rep(c("A", "B"), total_bloques)

cat("\n[VALIDACIÓN DE BLOQUES]\n")
cat("Total de viñetas base:", nrow(df_con_bloques), "\n")
cat("Total de bloques (Decks):", length(unique(df_con_bloques$Block)), "\n\n")

# ==============================================================================
# 5. FUNCIÓN DE TEXTOS CON INYECCIÓN DE OCUPACIÓN (CORREGIDA)
# ==============================================================================
procesar_textos_split <- function(df_input, tipo_perfil) {
  df_output <- as.data.frame(df_input)
  
  df_output <- df_output %>%
    mutate(
      Texto_Genero = if_else(Genero == "1", "Mujer", "Hombre"),
      
      Texto_Civil = case_when(
        Texto_Genero == "Mujer" & Ecivil == "1" ~ "soltera",
        Texto_Genero == "Mujer" & Ecivil == "2" ~ "casada",
        Texto_Genero == "Hombre" & Ecivil == "1" ~ "soltero",
        Texto_Genero == "Hombre" & Ecivil == "2" ~ "casado"
      ),
      
      # SOLUCIÓN: case_when evalúa secuencialmente y adapta el tamaño automáticamente
      Placeholder_Ocupacion = case_when(
        tipo_perfil == "Profesional" & Texto_Genero == "Mujer" ~ "Asociada financiera",
        tipo_perfil == "Profesional" & Texto_Genero == "Hombre" ~ "Asociado financiero",
        TRUE ~ "Analista contable" # "Analista contable" es neutro, no cambia por género
      ),
      
      Placeholder_Educacion = if_else(tipo_perfil == "Profesional",
                                      "Título profesional en Ingeniería Comercial",
                                      "Título técnico en Contabilidad"),
      
      Texto_Situacion = case_when(
        Eparental == "1" & Planfamilia == "1" ~ "actualmente no tiene hijos y le gustaría tener en 2 años más.",
        Eparental == "1" & Planfamilia == "2" ~ "actualmente no tiene hijos y le gustaría tener en 5 años más.",
        Eparental == "1" & Planfamilia == "3" ~ "actualmente no tiene hijos y no tiene planes de tenerlos en el futuro.",
        Eparental == "2" & Planfamilia == "1" ~ "tiene un hijo y le gustaría tener otro en 2 años más.",
        Eparental == "2" & Planfamilia == "2" ~ "tiene un hijo y le gustaría tener otro en 5 años más.",
        Eparental == "2" & Planfamilia == "3" ~ "tiene un hijo y no quiere tener más.",
        Eparental == "3" & Planfamilia == "1" ~ "tiene dos hijos y le gustaría tener otro en 2 años más.",
        Eparental == "3" & Planfamilia == "2" ~ "tiene dos hijos y le gustaría tener otro en 5 años más.",
        Eparental == "3" & Planfamilia == "3" ~ "tiene dos hijos y no quiere tener más."
      ),
      
      Placeholder_Exp = case_when(Exp == "1" ~ "6 años", Exp == "2" ~ "7 años", Exp == "3" ~ "8 años"),
      Placeholder_Desempeno = case_when(Desempeno == "1" ~ "5.8", Desempeno == "2" ~ "6.0", Desempeno == "3" ~ "6.3")
    )
  
  nombres_mujer <- c("Marcela", "Natalia")
  nombres_hombre <- c("Claudio", "Patricio")
  
  df_output <- df_output %>%
    arrange(Block) %>%
    group_by(Block) %>%
    mutate(
      idx_bloque = row_number(),  
      Placeholder_Nombre = if_else(Texto_Genero == "Mujer", nombres_mujer[idx_bloque], nombres_hombre[idx_bloque])
    ) %>%
    select(-idx_bloque) %>%
    ungroup()
  
  df_output$Texto_Familia_Completo <- paste0("se encuentra ", df_output$Texto_Civil, ", ", df_output$Texto_Situacion)
  return(df_output)
}

# ==============================================================================
# 6. CREAR LAS DOS BASES DE DATOS (EL SPLIT-SAMPLE)
# ==============================================================================
# Ahora correrá impecable sin errores de tamaño
df_rama_profesional <- procesar_textos_split(df_con_bloques, tipo_perfil = "Profesional")
df_rama_tecnica <- procesar_textos_split(df_con_bloques, tipo_perfil = "Tecnico")

# Seleccionar columnas para exportar
cols_export <- c("Block", "Postulante", "Texto_Genero", "Placeholder_Nombre", "Texto_Familia_Completo", 
                 "Placeholder_Ocupacion", "Placeholder_Educacion", "Placeholder_Exp", "Placeholder_Desempeno")

archivo_profesional <- df_rama_profesional %>% select(all_of(cols_export)) %>% arrange(Block)
archivo_tecnico <- df_rama_tecnica %>% select(all_of(cols_export)) %>% arrange(Block)

# ==============================================================================
# 7. EXPORTACIÓN
# ==============================================================================
write.csv(archivo_profesional, "Base_Vinetas_Muestra_Profesional.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(archivo_tecnico, "Base_Vinetas_Muestra_Tecnica.csv", row.names = FALSE, fileEncoding = "UTF-8")

