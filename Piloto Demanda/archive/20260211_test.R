# ==============================================================================
# PROYECTO FONDECYT: REDISEÑO DE NIVELES (DEMANDA)
# ==============================================================================

if(!require(AlgDesign)) install.packages("AlgDesign"); library(AlgDesign)
if(!require(dplyr)) install.packages("dplyr")

round10 <- function(x) { as.integer(ceiling(x / 10) * 10) }
options(contrasts = c("contr.sum", "contr.poly"))
set.seed(12345)

# ==============================================================================
# 1. DEFINIR LOS NIVELES 
# ==============================================================================

# Atributos: Genero(2), Ecivil(3), Eparental(3), Planfamilia(3), Ocupacion(2), Exp(3), Desempeno(3)
niveles_nuevos <- c(2, 3, 3, 3, 2, 3, 3)
nombres_nuevos <- c("Genero", "Ecivil", "Eparental", "Planfamilia", "Ocupacion", "Exp", "Desempeno")
cand_uni <- gen.factorial(levels = niveles_nuevos, varNames = nombres_nuevos, factors = "all")

# ==============================================================================
# 2. DEFINICIÓN DE PRUEBAS 
# ==============================================================================

# DESCOMENTA SOLO UNA DE ESTAS OPCIONES:

# --- OPCIÓN A: SIMPLE (k=15) ---
# frm_final <- ~ Genero + Ecivil + Eparental + Planfamilia + Ocupacion + Exp + Desempeno + Genero:Eparental

# --- OPCIÓN B: MEDIA (k=19) 
#frm_final <- ~ Genero + Ecivil + Eparental + Planfamilia + Ocupacion + Exp + Desempeno + Genero:(Ecivil + Eparental + Planfamilia)

# --- OPCIÓN C: COMPLEJA (k=59) ---
# frm_final <- ~ Genero * Ecivil * Eparental * Planfamilia + Ocupacion + Exp + Desempeno

# ==============================================================================
# 3. CÁLCULO DE N Y OPTIMIZACIÓN
# ==============================================================================

k_final <- ncol(model.matrix(frm_final, cand_uni))
N_final <- round10(3 * k_final) # Regla de seguridad 3k

cat("\n[REPORTE] Parámetros (k):", k_final, "| N sugerido:", N_final, "\n")

opt_diseno <- optFederov(frml = frm_final, data = cand_uni, nTrials = N_final, 
                         criterion = "D", nRepeats = 100, nullify = 1)

diseno_final <- as.data.frame(opt_diseno$design)
eval_final <- eval.design(frm_final, diseno_final, X = cand_uni)

cat(sprintf("[MÉTRICAS] D-Efficiency: %.3f | G-Efficiency: %.3f\n", eval_final$De, eval_final$Ge))

# ==============================================================================
# 4. GENERACIÓN DE BLOQUES (30 Bloques para 200 participantes)
# ==============================================================================

# Ajustamos bloques a 30 para mantener el balanceo en la muestra de 200
bloques_opt <- optBlock(frml = frm_final, withinData = diseno_final, 
                        blocksizes = rep(N_final/30, 30), criterion = "D")

df_con_bloques <- bloques_opt$design
df_con_bloques$Block <- rep(1:30, each = N_final/30)

# ==============================================================================
# 5. MAPEO DE TEXTOS CON LÓGICA CONDICIONAL 
# ==============================================================================

procesar_textos_v2 <- function(df_input) {
  df_output <- as.data.frame(df_input)
  
  df_output <- df_output %>%
    mutate(
      # Genero y Nombres
      Texto_Genero = if_else(Genero == "1", "Mujer", "Hombre"),
      # Estado Civil
      Texto_Civil = case_when(Ecivil == "1" ~ "soltero/a", 
                              Ecivil == "2" ~ "casado/a", 
                              Ecivil == "3" ~ "conviviendo"),
      # Bundle Ocupación
      Placeholder_Ocupacion = if_else(Ocupacion == "1", "Analista contable", "Asociado financiero"),
      Placeholder_Educacion = if_else(Ocupacion == "1", "Título técnico en Contabilidad", "Título profesional en Ingeniería Comercial"),
      
      # LÓGICA COMBINADA: Estado Parental + Plan de Familia
      Texto_Situacion = case_when(
        # Casos: SIN HIJOS
        Eparental == "1" & Planfamilia == "1" ~ "actualmente no tiene hijos y le gustaría tener en los próximos 3 años.",
        Eparental == "1" & Planfamilia == "2" ~ "actualmente no tiene hijos y le gustaría tener en los próximos 5 años.",
        Eparental == "1" & Planfamilia == "3" ~ "actualmente no tiene hijos y no tiene planes de tenerlos en el futuro.",
        
        # Casos: 1 HIJO PREESCOLAR
        Eparental == "2" & Planfamilia == "1" ~ "tiene un hijo en edad preescolar y le gustaría tener otro hijo en los próximos 3 años.",
        Eparental == "2" & Planfamilia == "2" ~ "tiene un hijo en edad preescolar y le gustaría tener otro hijo en los próximos 5 años.",
        Eparental == "2" & Planfamilia == "3" ~ "tiene un hijo en edad preescolar y no quiere tener más hijos.",
        
        # Casos: 2 HIJOS BÁSICA
        Eparental == "3" & Planfamilia == "1" ~ "tiene dos hijos en educación básica y le gustaría tener otro hijo en los próximos 3 años.",
        Eparental == "3" & Planfamilia == "2" ~ "tiene dos hijos en educación básica y le gustaría tener otro hijo en los próximos 5 años.",
        Eparental == "3" & Planfamilia == "3" ~ "tiene dos hijos en educación básica y no quiere tener más hijos."
      ),
      
      # Otros placeholders
      Placeholder_Exp = case_when(Exp == "1" ~ "6 años", Exp == "2" ~ "7 años", Exp == "3" ~ "8 años"),
      Placeholder_Desempeno = case_when(Desempeno == "1" ~ "5.8", Desempeno == "2" ~ "6.0", Desempeno == "3" ~ "6.3")
    )
  
  # Asignación de nombres (misma semilla para consistencia)
  set.seed(2025)
  df_output$Placeholder_Nombre <- if_else(df_output$Texto_Genero == "Mujer", 
                                          sample(c("Marcela", "Natalia"), nrow(df_output), replace = TRUE),
                                          sample(c("Claudio", "Patricio"), nrow(df_output), replace = TRUE))
  
  # Construcción del Párrafo Final de Familia
  df_output$Texto_Familia_Completo <- paste0("se encuentra ", df_output$Texto_Civil, ", ", df_output$Texto_Situacion)
  
  return(df_output)
}

df_final <- procesar_textos_v2(df_con_bloques)

# ==============================================================================
# 6. EXPORTACIÓN
# ==============================================================================

archivo_vinetas <- df_final %>%
  select(Block, Texto_Genero, Placeholder_Nombre, Texto_Familia_Completo, 
         Placeholder_Ocupacion, Placeholder_Educacion, Placeholder_Exp, Placeholder_Desempeno) %>%
  arrange(Block)

write.csv(archivo_vinetas, "Base_Vinetas_NuevosNiveles.csv", row.names = FALSE)