if(!require(AlgDesign)) install.packages("AlgDesign"); library(AlgDesign)
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)

round10 <- function(x) { as.integer(ceiling(x / 10) * 10) }
options(contrasts = c("contr.sum", "contr.poly"))
set.seed(12345) 

# ==============================================================================
# 1. DEFINIR LOS NIVELES (CON TEMPLATE)
# ==============================================================================
niveles_nuevos <- c(2, 2, 3, 3, 3, 3, 2) 
nombres_nuevos <- c("Genero", "Ecivil", "Eparental", "Planfamilia", "Exp", "Desempeno", "Template")
cand_uni <- gen.factorial(levels = niveles_nuevos, varNames = nombres_nuevos, factors = "all")

# ==============================================================================
# 2. DEFINICIÓN DEL MODELO
# ==============================================================================
frm_final <- ~ Genero * Eparental * Planfamilia + Ecivil * Genero + Exp + Desempeno + Template

# ==============================================================================
# 3. CÁLCULO DE N Y OPTIMIZACIÓN
# ==============================================================================
k_final <- ncol(model.matrix(frm_final, cand_uni))
N_final <- round10(3 * k_final)

cat("\n[REPORTE] Parámetros (k):", k_final, "| N sugerido:", N_final, "\n")

opt_diseno <- optFederov(frml = frm_final, data = cand_uni, nTrials = N_final, 
                         criterion = "D", nRepeats = 100, nullify = 1)

diseno_final <- as.data.frame(opt_diseno$design)
eval_final <- eval.design(frm_final, diseno_final, X = cand_uni)
cat(sprintf("[MÉTRICAS] D-Efficiency: %.3f | G-Efficiency: %.3f\n", eval_final$De, eval_final$Ge))

# ==============================================================================
# 4. GENERACIÓN DE BLOQUES
# ==============================================================================
set.seed(999)

cat("[CHEQUEO] Distribución de Template en diseno_final:\n")
print(table(diseno_final$Template))

d_T1 <- diseno_final[diseno_final$Template == "1", ]
d_T2 <- diseno_final[diseno_final$Template == "2", ]

if (nrow(d_T1) != nrow(d_T2)) {
  stop("ERROR: diseno_final no tiene igual número de viñetas para Template 1 y 2.")
}

total_bloques <- nrow(d_T1)

d_T2_rand <- d_T2[sample(nrow(d_T2)), ]
d_T1$Block      <- 1:total_bloques
d_T2_rand$Block <- 1:total_bloques

df_con_bloques <- rbind(d_T1, d_T2_rand) |> dplyr::arrange(Block)

df_con_bloques <- df_con_bloques |>
  dplyr::group_by(Block) |>
  dplyr::slice(sample(dplyr::n())) |>
  dplyr::ungroup()

df_con_bloques$Postulante <- rep(c("A", "B"), total_bloques)

cat("\n[VALIDACIÓN DE BLOQUES]\n")
cat("Total de viñetas base:", nrow(df_con_bloques), "\n")
cat("Total de bloques:", total_bloques, "\n")

bloques_mismoT <- df_con_bloques |>
  dplyr::group_by(Block) |>
  dplyr::summarise(n_templates = dplyr::n_distinct(Template)) |>
  dplyr::filter(n_templates < 2)
cat("Bloques con mismo Template en ambas posiciones:", nrow(bloques_mismoT), 
    ifelse(nrow(bloques_mismoT) == 0, " ✓\n", " ← PROBLEMA\n"))

# ==============================================================================
# 5. EXPANSIÓN A 800 ENCUESTADOS  [NUEVO]
# Cada bloque se replica a 20 encuestados distintos
# ==============================================================================
encuestados_totales <- 800
reps_por_bloque     <- encuestados_totales / total_bloques  # = 20

cat(sprintf("[EXPANSIÓN] %d bloques × %d encuestados = %d encuestados totales\n",
            total_bloques, reps_por_bloque, encuestados_totales))

df_expandido <- df_con_bloques |>
  # Replicar cada fila reps_por_bloque veces
  slice(rep(1:n(), times = reps_por_bloque)) |>
  # Ordenar para que queden agrupados por bloque y luego por ciclo
  arrange(Block, Postulante) |>
  group_by(Block, Postulante) |>
  mutate(Ciclo = row_number()) |>       # ciclo 1..20 dentro de cada bloque × posición
  ungroup() |>
  # Asignar Respondent_ID: cada par (Bloque, Ciclo) → un encuestado
  # Bloque 1 Ciclo 1 → ID 1, Bloque 1 Ciclo 2 → ID 2 ... Bloque 40 Ciclo 20 → ID 800
  mutate(Respondent_ID = (Block - 1) * reps_por_bloque + Ciclo) |>
  arrange(Respondent_ID, Postulante)

cat("[CHEQUEO EXPANSIÓN]\n")
cat("Filas totales:", nrow(df_expandido), "(esperado:", encuestados_totales * 2, ")\n")
cat("Encuestados únicos:", n_distinct(df_expandido$Respondent_ID), "\n")

# Verificar que cada encuestado ve exactamente 2 filas (postulante A y B)
check_enc <- df_expandido |> 
  group_by(Respondent_ID) |> 
  summarise(n = n(), n_post = n_distinct(Postulante)) |> 
  filter(n != 2 | n_post != 2)
cat("Encuestados con estructura incorrecta:", nrow(check_enc),
    ifelse(nrow(check_enc) == 0, " ✓\n", " ← PROBLEMA\n"))

# Verificar que cada encuestado ve exactamente un Template 1 y un Template 2
check_templ <- df_expandido |>
  group_by(Respondent_ID) |>
  summarise(n_templ = n_distinct(Template)) |>
  filter(n_templ < 2)
cat("Encuestados que ven el mismo Template dos veces:", nrow(check_templ),
    ifelse(nrow(check_templ) == 0, " ✓\n", " ← PROBLEMA\n"))

# ==============================================================================
# 6. FUNCIÓN DE TEXTOS
# ==============================================================================
procesar_textos_split <- function(df_input, tipo_perfil) {
  df_output <- as.data.frame(df_input)
  
  df_output <- df_output %>%
    mutate(
      Texto_Genero = if_else(Genero == "1", "Mujer", "Hombre"),
      
      Texto_Civil = case_when(
        Texto_Genero == "Mujer"  & Ecivil == "1" ~ "soltera",
        Texto_Genero == "Mujer"  & Ecivil == "2" ~ "casada",
        Texto_Genero == "Hombre" & Ecivil == "1" ~ "soltero",
        Texto_Genero == "Hombre" & Ecivil == "2" ~ "casado"
      ),
      
      Placeholder_Ocupacion = case_when(
        tipo_perfil == "Profesional" & Texto_Genero == "Mujer"  ~ "Asociada financiera",
        tipo_perfil == "Profesional" & Texto_Genero == "Hombre" ~ "Asociado financiero",
        TRUE ~ "Analista contable"
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
      
      Placeholder_Exp       = case_when(Exp == "1" ~ "6 años", Exp == "2" ~ "7 años", Exp == "3" ~ "8 años"),
      Placeholder_Desempeno = case_when(Desempeno == "1" ~ "5.8", Desempeno == "2" ~ "6.0", Desempeno == "3" ~ "6.3"),
      Texto_Template        = if_else(Template == "1", "Redacción A", "Redacción B")
    )
  
  nombres_mujer  <- c("Marcela", "Natalia")
  nombres_hombre <- c("Claudio",  "Patricio")
  
  # El nombre varía por posición dentro del bloque (Postulante A o B)
  # Postulante A → índice 1, Postulante B → índice 2
  df_output <- df_output %>%
    mutate(
      idx_nombre = if_else(Postulante == "A", 1L, 2L),
      Placeholder_Nombre = if_else(Texto_Genero == "Mujer",
                                   nombres_mujer[idx_nombre],
                                   nombres_hombre[idx_nombre])
    ) %>%
    select(-idx_nombre)
  
  df_output$Texto_Familia_Completo <- paste0("se encuentra ", df_output$Texto_Civil, 
                                             ", ", df_output$Texto_Situacion)
  return(df_output)
}

# ==============================================================================
# 7. CREAR LAS DOS RAMAS
# ==============================================================================
df_rama_profesional <- procesar_textos_split(df_expandido, tipo_perfil = "Profesional")
df_rama_tecnica     <- procesar_textos_split(df_expandido, tipo_perfil = "Tecnico")

# ==============================================================================
# 8. SELECCIÓN DE COLUMNAS Y EXPORTACIÓN
# ==============================================================================
cols_export <- c("Respondent_ID", "Block", "Postulante",
                 "Texto_Genero", "Placeholder_Nombre", "Texto_Familia_Completo",
                 "Placeholder_Ocupacion", "Placeholder_Educacion",
                 "Placeholder_Exp", "Placeholder_Desempeno", "Texto_Template")

archivo_profesional <- df_rama_profesional %>% select(all_of(cols_export)) %>% arrange(Respondent_ID, Postulante)
archivo_tecnico     <- df_rama_tecnica     %>% select(all_of(cols_export)) %>% arrange(Respondent_ID, Postulante)

# ==============================================================================
# 9. AUDITORÍA FINAL
# ==============================================================================
cat("\n===== AUDITORÍA FINAL =====\n")

cat("Filas archivo profesional:", nrow(archivo_profesional), 
    ifelse(nrow(archivo_profesional) == 1600, " ✓\n", " ← ERROR\n"))
cat("Filas archivo técnico:    ", nrow(archivo_tecnico),
    ifelse(nrow(archivo_tecnico) == 1600, " ✓\n", " ← ERROR\n"))

dist_temp <- table(archivo_profesional$Texto_Template)
cat("Balance Template (A vs B):", dist_temp[1], "vs", dist_temp[2],
    ifelse(dist_temp[1] == dist_temp[2], " ✓\n", " ← ERROR\n"))

check_enc_final <- archivo_profesional %>%
  group_by(Respondent_ID) %>%
  summarise(n_templ = n_distinct(Texto_Template)) %>%
  filter(n_templ < 2)
cat("Encuestados con mismo Template dos veces:", nrow(check_enc_final),
    ifelse(nrow(check_enc_final) == 0, " ✓\n", " ← PROBLEMA\n"))

cat("===========================\n")

write.csv(archivo_profesional, "Netquest_Profesional_800.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(archivo_tecnico,     "Netquest_Tecnico_800.csv",     row.names = FALSE, fileEncoding = "UTF-8")