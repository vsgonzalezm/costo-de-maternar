if(!require(AlgDesign)) install.packages("AlgDesign"); library(AlgDesign)
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)

round10 <- function(x) { as.integer(ceiling(x / 10) * 10) }
options(contrasts = c("contr.sum", "contr.poly"))

# ==============================================================================
# 1. DEFINIR LOS NIVELES (TEMPLATE DENTRO DEL ALGORITMO)
# ==============================================================================
niveles_nuevos <- c(2, 2, 3, 3, 3, 3, 2) # Se agrega un factor más (Template)
nombres_nuevos <- c("Genero", "Ecivil", "Eparental", "Planfamilia", "Exp", "Desempeno", "Template")
cand_uni <- gen.factorial(levels = niveles_nuevos, varNames = nombres_nuevos, factors = "all")

# ==============================================================================
# 2. DEFINICIÓN DEL MODELO
# ==============================================================================
frm_final <- ~ Genero * Eparental * Planfamilia + Ecivil * Genero + Exp + Desempeno + Template

# ==============================================================================
# 3. CÁLCULO DE N Y OPTIMIZACIÓN 
# ==============================================================================
set.seed(12345)
k_final <- ncol(model.matrix(frm_final, cand_uni))
N_teorico <- round10(3 * k_final) 
N_final <- max(N_teorico, 200) 

opt_diseno <- optFederov(frml = frm_final, data = cand_uni, nTrials = N_final, 
                         criterion = "D", nRepeats = 500, nullify = 1)

diseno_final <- as.data.frame(opt_diseno$design)
eval_final <- eval.design(frm_final, diseno_final, X = cand_uni)
cat(sprintf("[MÉTRICAS] D-Efficiency: %.3f\n", eval_final$De))

# ==============================================================================
# 4. GENERACIÓN DE BLOQUES BASE
# ==============================================================================
set.seed(999)
total_bloques <- N_final / 2

bloques_opt <- optBlock(frml = frm_final, withinData = diseno_final, 
                        blocksizes = rep(2, total_bloques), criterion = "D")

df_con_bloques <- as.data.frame(bloques_opt$design)
df_con_bloques$Block <- rep(1:total_bloques, each = 2)

# ==============================================================================
# 5. EXPANSIÓN A 800 (SOLO PERMUTAMOS EL ORDEN DE APARICIÓN)
# ==============================================================================
df_expandido <- df_con_bloques %>%
  slice(rep(1:n(), times = 800 / total_bloques)) %>%
  mutate(Respondent_ID = rep(1:800, each = 2)) %>%
  group_by(Block, Respondent_ID) %>%
  # Permutamos aleatoriamente el orden de las dos filas del bloque
  mutate(Orden_Aparicion = sample(c(1, 2))) %>%
  ungroup() %>%
  arrange(Respondent_ID, Orden_Aparicion)

# ==============================================================================
# 6. FUNCIÓN DE TEXTOS
# ==============================================================================
procesar_textos_split <- function(df_input) {
  df_input %>%
    mutate(
      Texto_Genero = if_else(Genero == "1", "Mujer", "Hombre"),
      Texto_Civil = case_when(Genero == "1" & Ecivil == "1" ~ "soltera", Genero == "1" & Ecivil == "2" ~ "casada/conviviendo", Genero == "2" & Ecivil == "1" ~ "soltero", Genero == "2" & Ecivil == "2" ~ "casado/conviviendo"),
      Texto_Situacion = case_when(Eparental == "1" & Planfamilia == "1" ~ "no tiene hijos y le gustaría tener en 2 años más.", Eparental == "1" & Planfamilia == "2" ~ "no tiene hijos y le gustaría tener en 5 años más.", Eparental == "1" & Planfamilia == "3" ~ "no tiene hijos y no tiene planes de tenerlos.", Eparental == "2" & Planfamilia == "1" ~ "tiene un hijo y le gustaría tener otro en 2 años más.", Eparental == "2" & Planfamilia == "2" ~ "tiene un hijo y le gustaría tener otro en 5 años más.", Eparental == "2" & Planfamilia == "3" ~ "tiene un hijo y no quiere tener más.", Eparental == "3" & Planfamilia == "1" ~ "tiene dos hijos y le gustaría tener otro en 2 años más.", Eparental == "3" & Planfamilia == "2" ~ "tiene dos hijos y le gustaría tener otro en 5 años más.", Eparental == "3" & Planfamilia == "3" ~ "tiene dos hijos y no quiere tener más."),
      Placeholder_Exp = case_when(Exp == "1" ~ "6 años", Exp == "2" ~ "7 años", Exp == "3" ~ "8 años"),
      Placeholder_Desempeno = case_when(Desempeno == "1" ~ "5.8", Desempeno == "2" ~ "6.0", Desempeno == "3" ~ "6.3"),
      Texto_Template = if_else(Template == "1", "Redacción A", "Redacción B")
    ) %>%
    group_by(Respondent_ID) %>%
    mutate(Placeholder_Nombre = if_else(Genero == "1", c("Marcela", "Natalia")[Orden_Aparicion], c("Claudio", "Patricio")[Orden_Aparicion])) %>%
    ungroup() %>%
    mutate(Texto_Familia_Completo = paste0("se encuentra ", Texto_Civil, ", ", Texto_Situacion))
}

df_final <- procesar_textos_split(df_expandido)

# ==============================================================================
# 7. RAMIFICACIÓN Y EXPORTACIÓN
# ==============================================================================
func_exportar <- function(df, tipo_mercado) {
  df %>%
    mutate(Placeholder_Ocupacion = if_else(Genero == "1", 
                                           if(tipo_mercado=="Prof") "Asociada financiera" else "Analista contable",
                                           if(tipo_mercado=="Prof") "Asociado financiero" else "Analista contable"),
           Placeholder_Educacion = if(tipo_mercado=="Prof") "Título profesional en Ingeniería Comercial" else "Título técnico en Contabilidad") %>%
    select(Respondent_ID, Orden_Aparicion, Texto_Genero, Placeholder_Nombre, Texto_Familia_Completo, Placeholder_Exp, Placeholder_Desempeno, Placeholder_Ocupacion, Placeholder_Educacion, Texto_Template)
}

write.csv(func_exportar(df_final, "Prof"), "Netquest_Long_Profesional_contem.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(func_exportar(df_final, "Tec"), "Netquest_Long_Tecnico_contem.csv", row.names = FALSE, fileEncoding = "UTF-8")
# ==============================================================================
# 8. AUDITORÍA DE BALANCE Y ORTOGONALIDAD (TESTEO EXHAUSTIVO)
# ==============================================================================
cat("\n=======================================================\n")
cat("      TESTEO DE BALANCE Y ORTOGONALIDAD (TEMPLATE)     \n")
cat("=======================================================\n")

# 1. Balance Global
dist_temp <- table(df_final$Texto_Template)
cat("\n1. Balance Global del Template:\n")
print(dist_temp)

# 2. Balance de Orden de Aparición vs Template
# Para Netquest, esto debe ser perfectamente simétrico.
cat("\n2. Cruce Template x Orden de Aparición:\n")
print(table(df_final$Texto_Template, df_final$Orden_Aparicion))

# 3. Comportamiento Intra-Encuestado (Qué experimenta el usuario)
# ¿El algoritmo le muestra templates mezclados o el mismo?
check_encuestado <- df_final %>% 
  group_by(Respondent_ID) %>% 
  summarise(
    n_templates_vistos = n_distinct(Texto_Template),
    combinacion = paste(Texto_Template, collapse = " - ")
  )
cat("\n3. Distribución de Templates vistos por un mismo encuestado:\n")
print(table(check_encuestado$n_templates_vistos))
cat("(Nota: Si es 2, el encuestado ve un cambio de redacción entre la viñeta 1 y 2. Si es 1, ve el mismo formato en ambas. Evalúa si esto ensucia tu medición).\n")

# 4. Prueba de Ortogonalidad (Cramer's V o Chi-Cuadrado)
# Verifica si incluir el Template dentro de optFederov generó correlaciones espurias
# con las variables clave en esta fracción de N_final.
cat("\n4. Test de Independencia (Chi-cuadrado) Template vs Variables Sustantivas:\n")

vars_test <- c("Genero", "Ecivil", "Eparental", "Exp")
for(v in vars_test) {
  tbl <- table(df_final$Texto_Template, df_final[[v]])
  chi_test <- suppressWarnings(chisq.test(tbl))
  # Valores p > 0.05 indican que NO hay asociación (Ortogonalidad exitosa)
  cat(sprintf("   - Template vs %s: p-value = %.3f\n", v, chi_test$p.value))
}


