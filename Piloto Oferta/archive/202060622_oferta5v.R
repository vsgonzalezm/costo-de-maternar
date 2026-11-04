# ==============================================================================
# DISEÑO FCR — PRETEST OFERTA
# 200 Encuestados, 5 Viñetas por encuestado 
# ==============================================================================

if(!require(AlgDesign)) install.packages("AlgDesign"); library(AlgDesign)
if(!require(dplyr))     install.packages("dplyr");     library(dplyr)
if(!require(tidyr))     install.packages("tidyr");     library(tidyr) 

round10 <- function(x) { as.integer(ceiling(x / 10) * 10) }
options(contrasts = c("contr.sum", "contr.poly"))

# ==============================================================================
# 1. DEFINIR LOS NIVELES Y NOMBRES
# Modalidad(2), Flexibilidad(2), Carga(2), Ben_parentales(3), Cuidado(2), Liderazgo(3)
# ==============================================================================
niveles_oferta <- c(2, 2, 2, 3, 2, 3) 
nombres_oferta <- c("Modalidad", "Flexibilidad", "Carga", "Ben_parentales", "Cuidado", "Liderazgo")
cand_uni <- gen.factorial(levels = niveles_oferta, varNames = nombres_oferta, factors = "all")

# ==============================================================================
# 2. DEFINICIÓN DEL MODELO (CON INTERACCIÓN TEÓRICA)
# ==============================================================================
frm_final <- ~ Modalidad * Flexibilidad + Carga + Ben_parentales + Cuidado + Liderazgo

# ==============================================================================
# 3. CÁLCULO DE N Y OPTIMIZACIÓN (PRETEST)
# ==============================================================================
set.seed(12345)
k_final   <- ncol(model.matrix(frm_final, cand_uni)) # 10 parámetros
N_teorico <- round10(3 * k_final)                    # 30 Mínimo recomendado
N_final   <- 40                                      # Fijado en 40 para 5 viñetas/persona, se puede subir a 50 y mejor al D

cat("\n[REPORTE] Parámetros (k):", k_final, "| N teórico:", N_teorico, "| N fijado:", N_final, "\n")

opt_diseno <- optFederov(frml = frm_final, data = cand_uni, nTrials = N_final, 
                         criterion = "D", nRepeats = 500, nullify = 1)

diseno_final <- as.data.frame(opt_diseno$design)
eval_final   <- eval.design(frm_final, diseno_final, X = cand_uni)
cat(sprintf("[MÉTRICAS] D-Efficiency: %.3f | G-Efficiency: %.3f\n", eval_final$De, eval_final$Ge)) 

# ==============================================================================
# 4. GENERACIÓN DE BLOQUES BASE (Vía robusta con $Blocks)
# ==============================================================================
set.seed(999)
n_por_bloque  <- 5  # AHORA SON 5 VIÑETAS POR ENCUESTADO
total_bloques <- N_final / n_por_bloque # 10 Bloques exactos

bloques_opt <- optBlock(frml = frm_final, withinData = diseno_final, 
                        blocksizes = rep(n_por_bloque, total_bloques), criterion = "D")

# Reconstruimos desde $Blocks para no depender del orden general
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
# 5. EXPANSIÓN A 200 ENCUESTADOS Y PERMUTACIÓN DEL ORDEN
# ==============================================================================
encuestados_totales <- 200
repeticiones <- encuestados_totales / total_bloques 

df_expandido <- df_con_bloques %>%
  slice(rep(1:n(), times = repeticiones)) %>%
  mutate(Respondent_ID = rep(1:encuestados_totales, each = n_por_bloque)) %>%
  group_by(Block, Respondent_ID) %>%
  # Aleatorizamos el orden del 1 al 5 para cada encuestado
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
                                   "Fijo  (de 9 a.m a 6 p.m).", 
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

cat("\n=================== AUDITORÍA ===================\n")

# (a) Verificación estricta de NO duplicados por persona (buscamos 5 viñetas únicas)
chk_duplicados <- df_long_final %>%
  group_by(Respondent_ID) %>%
  summarise(perfiles_unicos = n_distinct(paste(Texto_Modalidad, Texto_Flexibilidad, Texto_Carga, Texto_Parentales, Texto_Cuidado, Texto_Liderazgo))) %>%
  filter(perfiles_unicos < 5)

cat(sprintf("(a) Encuestados que ven ofertas repetidas: %d %s\n", 
            nrow(chk_duplicados), ifelse(nrow(chk_duplicados) == 0, "(cero duplicados)", "(! ERROR)")))

# (b) Balance de niveles por dimensión
cat("\n(b) Balance de niveles por dimensión (Deberían ser casi simétricos):\n")
for (v in nombres_oferta) { cat("   ", v, ":", paste(table(diseno_final[[v]]), collapse = " / "), "\n") }

# (c) Calidad del bloqueo
cat(sprintf("\n(c) optBlock -> D: %.4f | Diagonality: %.4f (Cerca de 1 = decks ortogonales a los tratamientos)\n",
            bloques_opt$D, bloques_opt$diagonality))

# (d) Verificación de carga por encuestado
chk_eval <- df_long_final %>% count(Respondent_ID)
cat(sprintf("\n(d) Verificación final: %d encuestados evaluando exactamente %d viñetas cada uno.\n", 
            n_distinct(chk_eval$Respondent_ID), unique(chk_eval$n)))


# ==============================================================================
# 8. OUTPUT APILADO EN FORMATO ANCHO (200 filas) PARA PROGRAMACIÓN
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

cat("\n[WIDE] Filas exportadas:", nrow(df_wide_final), "(Esperado: 200) | Columnas 'V' hasta V5.\n")

# ==============================================================================
# 9. EXPORTACIÓN DE ARCHIVOS
# ==============================================================================
write.csv(df_long_final, "Netquest_Oferta_Pretest_Long_5V.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(df_wide_final, "Netquest_Oferta_Pretest_Wide_200_5V.csv", row.names = FALSE, fileEncoding = "UTF-8")
