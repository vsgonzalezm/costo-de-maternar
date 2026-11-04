
if(!require(AlgDesign)) install.packages("AlgDesign"); library(AlgDesign)
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)

round10 <- function(x) { as.integer(ceiling(x / 10) * 10) }
options(contrasts = c("contr.sum", "contr.poly"))
set.seed(12345)

# 1. DEFINIR LOS NIVELES 
niveles_nuevos <- c(2, 3, 3, 3, 2, 3, 3)
nombres_nuevos <- c("Genero", "Ecivil", "Eparental", "Planfamilia", "Ocupacion", "Exp", "Desempeno")
cand_uni <- gen.factorial(levels = niveles_nuevos, varNames = nombres_nuevos, factors = "all")

# 2. DEFINICIÓN DEL MODELO
# --- OPCIÓN C: COMPLEJA (k=59) ---
frm_final <- ~ Genero * Ecivil * Eparental * Planfamilia + Ocupacion + Exp + Desempeno

# 3. CÁLCULO DE N Y OPTIMIZACIÓN
k_final <- ncol(model.matrix(frm_final, cand_uni))
N_final <- round10(3 * k_final)

cat("\n[REPORTE] Parámetros (k):", k_final, "| N sugerido:", N_final, "\n")

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
total_bloques <- N_final / n_por_bloque 

bloques_opt <- optBlock(frml = frm_final, withinData = diseno_final, 
                        blocksizes = rep(n_por_bloque, total_bloques), criterion = "D")

df_con_bloques <- as.data.frame(bloques_opt$design)
df_con_bloques$Block <- rep(1:total_bloques, each = n_por_bloque)

# NUEVO: Añadir columna identificadora de postulante (A o B) por bloque
df_con_bloques$Postulante <- rep(c("A", "B"), total_bloques)

# VALIDACIÓN ADICIONAL
cat("\n[VALIDACIÓN DE BLOQUES]\n")
cat("Total de viñetas:", nrow(df_con_bloques), "\n")
cat("Total de bloques:", length(unique(df_con_bloques$Block)), "\n")
cat("Viñetas por bloque (Deben ser 2):\n")
print(table(table(df_con_bloques$Block)))

# 5. MAPEO DE TEXTOS CON CONCORDANCIA DE GÉNERO
procesar_textos_v3 <- function(df_input) {
  df_output <- as.data.frame(df_input)
  
  df_output <- df_output %>%
    mutate(
      Texto_Genero = if_else(Genero == "1", "Mujer", "Hombre"),
      
      Texto_Civil = case_when(
        Texto_Genero == "Mujer" & Ecivil == "1" ~ "soltera",
        Texto_Genero == "Mujer" & Ecivil == "2" ~ "casada",
        Texto_Genero == "Hombre" & Ecivil == "1" ~ "soltero",
        Texto_Genero == "Hombre" & Ecivil == "2" ~ "casado",
        TRUE ~ "conviviendo"
      ),
      
      Placeholder_Ocupacion = case_when(
        Texto_Genero == "Mujer" & Ocupacion == "1" ~ "Analista contable",
        Texto_Genero == "Mujer" & Ocupacion == "2" ~ "Asociada financiera",
        Texto_Genero == "Hombre" & Ocupacion == "1" ~ "Analista contable",
        Texto_Genero == "Hombre" & Ocupacion == "2" ~ "Asociado financiero"
      ),
      
      Placeholder_Educacion = if_else(Ocupacion == "1", 
                                      "Título técnico en Contabilidad", 
                                      "Título profesional en Ingeniería Comercial"),
      
      Texto_Situacion = case_when(
        Eparental == "1" & Planfamilia == "1" ~ "actualmente no tiene hijos y le gustaría tener en los próximos 2 años.",
        Eparental == "1" & Planfamilia == "2" ~ "actualmente no tiene hijos y le gustaría tener en los próximos 5 años.",
        Eparental == "1" & Planfamilia == "3" ~ "actualmente no tiene hijos y no tiene planes de tenerlos en el futuro.",
        Eparental == "2" & Planfamilia == "1" ~ "tiene un hijo y le gustaría tener otro en los próximos 2 años.",
        Eparental == "2" & Planfamilia == "2" ~ "tiene un hijo y le gustaría tener otro en los próximos 5 años.",
        Eparental == "2" & Planfamilia == "3" ~ "tiene un hijo y no quiere tener más.",
        Eparental == "3" & Planfamilia == "1" ~ "tiene dos hijos y le gustaría tener otro en los próximos 2 años.",
        Eparental == "3" & Planfamilia == "2" ~ "tiene dos hijos y le gustaría tener otro en los próximos 5 años.",
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
      Placeholder_Nombre = if_else(
        Texto_Genero == "Mujer",
        nombres_mujer[idx_bloque],  
        nombres_hombre[idx_bloque]  
      )
    ) %>%
    select(-idx_bloque) %>%
    ungroup()
  
  df_output$Texto_Familia_Completo <- paste0("se encuentra ", df_output$Texto_Civil, ", ", df_output$Texto_Situacion)
  return(df_output)
}

df_final <- procesar_textos_v3(df_con_bloques)

# 6. VALIDACIÓN DE NOMBRES Y POSTULANTE
print(df_final %>% 
        filter(Block %in% 1:5) %>% 
        select(Block, Postulante, Texto_Genero, Placeholder_Nombre, Texto_Civil))

# ==============================================================================
# 7. EXPORTACIÓN CON COLUMNA POSTULANTE
# ==============================================================================
archivo_vinetas <- df_final %>%
  select(Block, Postulante, Texto_Genero, Placeholder_Nombre, Texto_Familia_Completo, 
         Placeholder_Ocupacion, Placeholder_Educacion, Placeholder_Exp, Placeholder_Desempeno) %>%
  arrange(Block)

write.csv(archivo_vinetas, "Base_Vinetas_Netquest_Marzo.csv", row.names = FALSE, fileEncoding = "UTF-8")

