# ==============================================================================
# DISEÑO FCR — PRETEST OFERTA 
# N=50 Viñetas, 5 Viñetas por persona.
# Total de encuestados en esta base: 50 (Para replicar x2 en Netquest de acuerdo a mujeres con y sin hijos)
# ==============================================================================

if(!require(AlgDesign)) install.packages("AlgDesign"); library(AlgDesign)
if(!require(dplyr))     install.packages("dplyr");     library(dplyr)
if(!require(tidyr))     install.packages("tidyr");     library(tidyr) 

round10 <- function(x) { as.integer(ceiling(x / 10) * 10) }
options(contrasts = c("contr.sum", "contr.poly"))

# ==============================================================================
# 1. DEFINIR LOS NIVELES Y NOMBRES
# ==============================================================================
niveles_oferta <- c(2, 2, 2, 3, 2, 3) 
nombres_oferta <- c("Modalidad", "Flexibilidad", "Carga", "Ben_parentales", "Cuidado", "Liderazgo")
cand_uni <- gen.factorial(levels = niveles_oferta, varNames = nombres_oferta, factors = "all")

# ==============================================================================
# 2. DEFINICIÓN DEL MODELO (CON INTERACCIÓN TEÓRICA)
# ==============================================================================
frm_final <- ~ Modalidad * Flexibilidad + Carga + Ben_parentales + Cuidado + Liderazgo

# ==============================================================================
# 3. CÁLCULO DE N Y OPTIMIZACIÓN
# ==============================================================================
set.seed(12345)
k_final   <- ncol(model.matrix(frm_final, cand_uni)) # 10 parámetros
N_teorico <- round10(3 * k_final)                    # 30 Mínimo recomendado
N_final   <- 50                                      # Fijado en 50 viñetas

cat("\n[REPORTE] Parámetros (k):", k_final, "| N teórico:", N_teorico, "| N fijado:", N_final, "\n")

opt_diseno <- optFederov(frml = frm_final, data = cand_uni, nTrials = N_final, 
                         criterion = "D", nRepeats = 500, nullify = 1)

diseno_final <- as.data.frame(opt_diseno$design)
eval_final   <- eval.design(frm_final, diseno_final, X = cand_uni)
cat(sprintf("[MÉTRICAS] D-Efficiency: %.3f | G-Efficiency: %.3f\n", eval_final$De, eval_final$Ge)) 

# ==============================================================================
# 4. GENERACIÓN DE BLOQUES BASE (10 Bloques de 5 viñetas)
# ==============================================================================
set.seed(999)
n_por_bloque  <- 5  
total_bloques <- N_final / n_por_bloque # 50/5 = 10 Bloques exactos

bloques_opt <- optBlock(frml = frm_final, withinData = diseno_final, 
                        blocksizes = rep(n_por_bloque, total_bloques), criterion = "D")

df_con_bloques <- do.call(rbind, lapply(seq_along(bloques_opt$Blocks), function(b) {
  d <- as.data.frame(bloques_opt$Blocks[[b]])
  d$Block <- b
  d
}))

rownames(df_con_bloques) <- NULL

df_con_bloques <- df_con_bloques %>%
  group_by(Block) %>%
  mutate(Fila_Base = row_number()) %>%
  ungroup()

# ==============================================================================
# 5. 50 ENCUESTADOS 
# ==============================================================================
encuestados_totales <- 50 # REDUCIDO A 50 PARA ENTREGAR LA BASE MAESTRA
repeticiones <- encuestados_totales / total_bloques # 50/10 = 5 personas evaluando cada bloque

df_expandido <- df_con_bloques %>%
  slice(rep(1:n(), times = repeticiones)) %>%
  mutate(Respondent_ID = rep(1:encuestados_totales, each = n_por_bloque)) %>%
  group_by(Block, Respondent_ID) %>%
  mutate(Orden_Aparicion = sample(1:n_por_bloque)) %>%
  ungroup() %>%
  arrange(Respondent_ID, Orden_Aparicion)

# ==============================================================================
# 6. INYECCIÓN DE TEXTOS EXACTOS
# ==============================================================================
procesar_textos_oferta <- function(df_input) {
  df_input %>%
    mutate(
      Texto_Modalidad = if_else(Modalidad == "1", 
                                "Híbrido, algunos días remoto y otros presencial.", 
                                "Todos los días presencial."),
      Texto_Flexibilidad = if_else(Flexibilidad == "1", 
                                   "Fijo (de 9 a.m a 6 p.m).", 
                                   "Flexible (puedes modificar el horario según tu preferencia)."),
      Texto_Carga = if_else(Carga == "1", 
                            "Nunca deberás trabajar horas extra.", 
                            "Frecuentemente deberás trabajar horas extra."),
      Texto_Parentales = case_when(
        Ben_parentales == "1" ~ "El mínimo legal para madres (5 meses y medio) y padres (5 días).",
        Ben_parentales == "2" ~ "6 meses adicionales a lo legal (5 meses y medio) para las madres y 25 días adicionales a lo legal (5 días) para los padres.",
        Ben_parentales == "3" ~ "6 meses para las madres y 6 meses para los padres (intransferible)."
      ),
      Texto_Cuidado = if_else(Cuidado == "1", 
                              "Sala cuna para menores de 2 años para las madres.", 
                              "Sala cuna para menores de 2 años tanto para madres (requisito legal) como para padres."),
      Texto_Liderazgo = case_when(
        Liderazgo == "1" ~ "Tu jefe se enfoca en potenciar las fortalezas de tu equipo.",
        Liderazgo == "2" ~ "Tu jefe se enfoca en los resultados de sus trabajadores.",
        Liderazgo == "3" ~ "Tu jefe se enfoca en la organización de los procesos en el equipo."
      )
    ) %>%
    select(Respondent_ID, Block, Orden_Aparicion, Texto_Modalidad, Texto_Flexibilidad, 
           Texto_Carga, Texto_Parentales, Texto_Cuidado, Texto_Liderazgo)
}

df_long_final <- procesar_textos_oferta(df_expandido)

# ==============================================================================
# 7. AUDITORÍA DEL DISEÑO EXPERIMENTAL
# ==============================================================================


chk_duplicados <- df_long_final %>%
  group_by(Respondent_ID) %>%
  summarise(perfiles_unicos = n_distinct(paste(Texto_Modalidad, Texto_Flexibilidad, Texto_Carga, Texto_Parentales, Texto_Cuidado, Texto_Liderazgo))) %>%
  filter(perfiles_unicos < 5)

cat(sprintf("(a) Encuestados que ven ofertas repetidas: %d %s\n", 
            nrow(chk_duplicados), ifelse(nrow(chk_duplicados) == 0, "(✓ PERFECTO, cero duplicados)", "(! ERROR)")))

chk_eval <- df_long_final %>% count(Respondent_ID)
cat(sprintf("\n(b) Verificación final: %d encuestados evaluando exactamente %d viñetas cada uno.\n", 
            n_distinct(chk_eval$Respondent_ID), unique(chk_eval$n)))

# ==============================================================================
# 8. OUTPUT APILADO EN FORMATO ANCHO (50 filas) 
# ==============================================================================
cols_vineta <- c("Texto_Modalidad", "Texto_Flexibilidad", "Texto_Carga", 
                 "Texto_Parentales", "Texto_Cuidado", "Texto_Liderazgo")

df_wide_final <- df_long_final %>%
  pivot_wider(
    id_cols     = c(Respondent_ID, Block),
    names_from  = Orden_Aparicion,
    values_from = all_of(cols_vineta),
    names_glue  = "V{Orden_Aparicion}_{.value}"
  ) %>%
  arrange(Respondent_ID)



# ==============================================================================
# 9. EXPORTACIÓN DE ARCHIVOS
# ==============================================================================
write.csv(df_long_final, "Netquest_Oferta_Pretest_Long_BaseMaestra_50.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(df_wide_final, "Netquest_Oferta_Pretest_Wide_BaseMaestra_50.csv", row.names = FALSE, fileEncoding = "UTF-8")
