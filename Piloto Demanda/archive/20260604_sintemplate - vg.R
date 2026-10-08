if(!require(AlgDesign)) install.packages("AlgDesign"); library(AlgDesign)
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)

round10 <- function(x) { as.integer(ceiling(x / 10) * 10) }
options(contrasts = c("contr.sum", "contr.poly"))

# ==============================================================================
# 1. DEFINIR LOS NIVELES (SIN TEMPLATE)
# ==============================================================================
niveles_nuevos <- c(2, 2, 3, 3, 3, 3) 
nombres_nuevos <- c("Genero", "Ecivil", "Eparental", "Planfamilia", "Exp", "Desempeno")
cand_uni <- gen.factorial(levels = niveles_nuevos, varNames = nombres_nuevos, factors = "all")

# ==============================================================================
# 2. DEFINICIÓN DEL MODELO
# ==============================================================================
frm_final <- ~ Genero * Eparental * Planfamilia + Ecivil * Genero + Exp + Desempeno

# ==============================================================================
# 3. CÁLCULO DE N Y OPTIMIZACIÓN 
# ==============================================================================
set.seed(12345)
k_final <- ncol(model.matrix(frm_final, cand_uni))

N_teorico <- round10(3 * k_final) 
N_final <- max(N_teorico, 200) 

cat("\n[REPORTE] Parámetros (k):", k_final, "| N teórico:", N_teorico, "| N fijado:", N_final, "\n")

opt_diseno <- optFederov(frml = frm_final, data = cand_uni, nTrials = N_final, 
                         criterion = "D", nRepeats = 500, nullify = 1)

diseno_final <- as.data.frame(opt_diseno$design)
eval_final <- eval.design(frm_final, diseno_final, X = cand_uni)
cat(sprintf("[MÉTRICAS] D-Efficiency: %.3f | G-Efficiency: %.3f\n", eval_final$De, eval_final$Ge))

# ==============================================================================
# 4. GENERACIÓN DE BLOQUES BASE
# ==============================================================================
set.seed(999)
n_por_bloque <- 2
total_bloques <- N_final / n_por_bloque # 100 Bloques

bloques_opt <- optBlock(frml = frm_final, withinData = diseno_final, 
                        blocksizes = rep(n_por_bloque, total_bloques), criterion = "D")

df_con_bloques <- as.data.frame(bloques_opt$design)
df_con_bloques$Block <- rep(1:total_bloques, each = n_por_bloque)

# Identificamos cada fila dentro de su bloque (Fila 1 y Fila 2)
df_con_bloques <- df_con_bloques %>%
  group_by(Block) %>%
  mutate(Fila_Base = row_number()) %>%
  ungroup()

# ==============================================================================
# 5. EXPANSIÓN A 800 ENCUESTADOS Y PERMUTACIÓN PERFECTA (ORDEN Y TEMPLATE)
# ==============================================================================
encuestados_totales <- 800
repeticiones <- encuestados_totales / total_bloques # 8 repeticiones por bloque

df_expandido <- df_con_bloques %>%
  # Duplicamos la base de bloques 8 veces
  slice(rep(1:n(), times = repeticiones)) %>%
  # Asignamos ID a cada encuestado (del 1 al 800)
  mutate(Respondent_ID = rep(1:encuestados_totales, each = 2)) %>%
  group_by(Block, Fila_Base) %>%
  # Contamos qué repetición de este bloque es (del 1 al 8)
  mutate(Ciclo = row_number()) %>%
  ungroup() %>%
  # Creamos 4 patrones de permutación para distribuir matemáticamente
  mutate(Patron = ((Ciclo - 1) %% 4) + 1) %>%
  mutate(
    Orden_Aparicion = case_when(
      Patron == 1 & Fila_Base == 1 ~ 1, Patron == 1 & Fila_Base == 2 ~ 2,
      Patron == 2 & Fila_Base == 1 ~ 1, Patron == 2 & Fila_Base == 2 ~ 2,
      Patron == 3 & Fila_Base == 1 ~ 2, Patron == 3 & Fila_Base == 2 ~ 1,
      Patron == 4 & Fila_Base == 1 ~ 2, Patron == 4 & Fila_Base == 2 ~ 1
    ),
    Template = case_when(
      Patron == 1 & Fila_Base == 1 ~ "1", Patron == 1 & Fila_Base == 2 ~ "2",
      Patron == 2 & Fila_Base == 1 ~ "2", Patron == 2 & Fila_Base == 2 ~ "1",
      Patron == 3 & Fila_Base == 1 ~ "1", Patron == 3 & Fila_Base == 2 ~ "2",
      Patron == 4 & Fila_Base == 1 ~ "2", Patron == 4 & Fila_Base == 2 ~ "1"
    )
  ) %>%
  # Ordenamos para que la base quede secuencial lista para Netquest
  arrange(Respondent_ID, Orden_Aparicion)

# ==============================================================================
# 6. FUNCIÓN DE TEXTOS (AHORA AGRUPANDO POR ENCUESTADO)
# ==============================================================================
procesar_textos_split <- function(df_input) {
  df_output <- as.data.frame(df_input)
  
  df_output <- df_output %>%
    mutate(
      Texto_Genero = if_else(Genero == "1", "Mujer", "Hombre"),
      Texto_Civil = case_when(
        Texto_Genero == "Mujer" & Ecivil == "1" ~ "soltera",
        Texto_Genero == "Mujer" & Ecivil == "2" ~ "casada/conviviendo",
        Texto_Genero == "Hombre" & Ecivil == "1" ~ "soltero",
        Texto_Genero == "Hombre" & Ecivil == "2" ~ "casado/conviviendo"
      ),
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
      Placeholder_Desempeno = case_when(Desempeno == "1" ~ "5.8", Desempeno == "2" ~ "6.0", Desempeno == "3" ~ "6.3"),
      Texto_Template = if_else(Template == "1", "Redacción A", "Redacción B")
    )
  
  nombres_mujer <- c("Marcela", "Natalia")
  nombres_hombre <- c("Claudio", "Patricio")
  
  df_output <- df_output %>%
    group_by(Respondent_ID) %>% # El nombre varía por cada encuestado
    mutate(
      Placeholder_Nombre = if_else(Texto_Genero == "Mujer", nombres_mujer[Orden_Aparicion], nombres_hombre[Orden_Aparicion])
    ) %>%
    ungroup()
  
  df_output$Texto_Familia_Completo <- paste0("se encuentra ", df_output$Texto_Civil, ", ", df_output$Texto_Situacion)
  return(df_output)
}

# Procesar los textos base
df_base_procesada <- procesar_textos_split(df_expandido) 

# ==============================================================================
# 7. RAMIFICACIÓN Y LIMPIEZA FINAL
# ==============================================================================
df_base_limpia <- df_base_procesada %>%
  select(Respondent_ID, Block, Orden_Aparicion, Texto_Genero, Placeholder_Nombre, 
         Texto_Familia_Completo, Placeholder_Exp, Placeholder_Desempeno, Texto_Template)

# Ramificar inyectando Ocupación y Educación según mercado
df_long_profesional <- df_base_limpia %>%
  mutate(
    Placeholder_Ocupacion = if_else(Texto_Genero == "Mujer", "Asociada financiera", "Asociado financiero"),
    Placeholder_Educacion = "Título profesional en Ingeniería Comercial"
  ) %>%
  relocate(Placeholder_Ocupacion, Placeholder_Educacion, .after = Texto_Familia_Completo)

df_long_tecnico <- df_base_limpia %>%
  mutate(
    Placeholder_Ocupacion = "Analista contable",
    Placeholder_Educacion = "Título técnico en Contabilidad"
  ) %>%
  relocate(Placeholder_Ocupacion, Placeholder_Educacion, .after = Texto_Familia_Completo)




# ==============================================================================
# 8. CHEQUEOS DE BALANCE Y ROBUSTEZ (AUDITORÍA)
# ==============================================================================


dist_temp <- table(df_long_profesional$Texto_Template)
cat("1. Balance total de Templates (A vs B):", dist_temp[1], "vs", dist_temp[2], 
    ifelse(dist_temp[1]==dist_temp[2], "✓ APROBADO\n", "X ERROR\n"))

dist_orden <- table(df_long_profesional$Texto_Template, df_long_profesional$Orden_Aparicion)
cat("2. Balance de Orden (Template A en Posición 1 vs Posición 2):", dist_orden[1,1], "vs", dist_orden[1,2], 
    ifelse(dist_orden[1,1]==dist_orden[1,2], "✓ APROBADO\n", "X ERROR\n"))

check_encuestado <- df_long_profesional %>% group_by(Respondent_ID) %>% summarise(n_temp = n_distinct(Texto_Template)) %>% filter(n_temp < 2)
cat("3. Encuestados que ven el mismo Template 2 veces:", nrow(check_encuestado), 
    ifelse(nrow(check_encuestado)==0, "✓ APROBADO\n", "X ERROR\n"))
cat("=======================================================\n\n")

# ==============================================================================
# 9. EXPORTACIÓN
# ==============================================================================
write.csv(df_long_profesional, "Netquest_Long_Profesional.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(df_long_tecnico, "Netquest_Long_Tecnico.csv", row.names = FALSE, fileEncoding = "UTF-8")
