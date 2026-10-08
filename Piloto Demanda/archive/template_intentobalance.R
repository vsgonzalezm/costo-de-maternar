if(!require(AlgDesign)) install.packages("AlgDesign"); library(AlgDesign)
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)

round10 <- function(x) { as.integer(ceiling(x / 10) * 10) }
options(contrasts = c("contr.sum", "contr.poly"))

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
# 3. OPTIMIZACIÓN CON N AUMENTADO PARA FACILITAR BALANCE
# ==============================================================================
k_final <- ncol(model.matrix(frm_final, cand_uni))
N_base  <- round10(3 * k_final)  # 80

# Probamos N = 80, 100, 120, 140, 160 hasta encontrar balance
N_candidatos <- c(N_base, 100, 120, 140, 160, 200)

diseno_final <- NULL
N_usado      <- NULL

for (N_try in N_candidatos) {
  cat(sprintf("\n[BUSCANDO] Probando N = %d...\n", N_try))
  
  semilla    <- 12345
  encontrado <- FALSE
  
  for (intento in 1:30) {
    set.seed(semilla)
    
    opt_try <- tryCatch(
      optFederov(frml = frm_final, data = cand_uni, nTrials = N_try,
                 criterion = "D", nRepeats = 100, nullify = 1),
      error = function(e) NULL
    )
    
    if (!is.null(opt_try)) {
      d_try <- as.data.frame(opt_try$design)
      n_T1  <- sum(d_try$Template == "1")
      n_T2  <- sum(d_try$Template == "2")
      cat(sprintf("  Intento %d (semilla %d): T1=%d T2=%d\n", intento, semilla, n_T1, n_T2))
      
      if (n_T1 == n_T2 && (800 %% (N_try / 2)) == 0) {
        diseno_final <- d_try
        N_usado      <- N_try
        encontrado   <- TRUE
        cat(sprintf("[OK] N=%d balanceado en intento %d (semilla %d)\n", N_try, intento, semilla))
        break
      }
    }
    semilla <- semilla + 1
  }
  
  if (encontrado) break
}

if (is.null(diseno_final)) stop("No se encontró diseño válido. Contacta al equipo.")

eval_final <- eval.design(frm_final, diseno_final, X = cand_uni)
cat(sprintf("[MÉTRICAS] D-Efficiency: %.3f | G-Efficiency: %.3f | N usado: %d\n",
            eval_final$De, eval_final$Ge, N_usado))

# ==============================================================================
# 4. GENERACIÓN DE BLOQUES (igual que tu profesora)
# ==============================================================================
set.seed(999)

d_T1 <- diseno_final[diseno_final$Template == "1", ]
d_T2 <- diseno_final[diseno_final$Template == "2", ]

total_bloques <- nrow(d_T1)
cat("\nTotal de bloques:", total_bloques, "\n")

d_T2_rand       <- d_T2[sample(nrow(d_T2)), ]
d_T1$Block      <- 1:total_bloques
d_T2_rand$Block <- 1:total_bloques

df_con_bloques <- rbind(d_T1, d_T2_rand) |> dplyr::arrange(Block)

df_con_bloques <- df_con_bloques |>
  dplyr::group_by(Block) |>
  dplyr::slice(sample(dplyr::n())) |>
  dplyr::ungroup()

df_con_bloques$Postulante <- rep(c("A", "B"), total_bloques)

bloques_mismoT <- df_con_bloques |>
  dplyr::group_by(Block) |>
  dplyr::summarise(n_t = dplyr::n_distinct(Template)) |>
  dplyr::filter(n_t < 2)
cat("Bloques con mismo Template en ambas posiciones:", nrow(bloques_mismoT),
    ifelse(nrow(bloques_mismoT) == 0, " ✓\n", " ← PROBLEMA\n"))

# ==============================================================================
# 5. EXPANSIÓN A 800 ENCUESTADOS
# ==============================================================================
encuestados_totales <- 800
reps_por_bloque     <- encuestados_totales / total_bloques

cat(sprintf("[EXPANSIÓN] %d bloques × %d encuestados = %d totales\n",
            total_bloques, reps_por_bloque, encuestados_totales))

if (encuestados_totales %% total_bloques != 0) {
  stop(sprintf("ERROR: %d encuestados no es divisible entre %d bloques.", 
               encuestados_totales, total_bloques))
}

df_expandido <- df_con_bloques |>
  dplyr::slice(rep(1:dplyr::n(), times = reps_por_bloque)) |>
  dplyr::arrange(Block, Postulante) |>
  dplyr::group_by(Block, Postulante) |>
  dplyr::mutate(Ciclo = dplyr::row_number()) |>
  dplyr::ungroup() |>
  dplyr::mutate(Respondent_ID = (Block - 1) * reps_por_bloque + Ciclo) |>
  dplyr::arrange(Respondent_ID, Postulante)

cat("Filas totales:", nrow(df_expandido),
    ifelse(nrow(df_expandido) == encuestados_totales * 2, " ✓\n", " ← ERROR\n"))
cat("Encuestados únicos:", dplyr::n_distinct(df_expandido$Respondent_ID), "\n")

check_templ <- df_expandido |>
  dplyr::group_by(Respondent_ID) |>
  dplyr::summarise(n_t = dplyr::n_distinct(Template)) |>
  dplyr::filter(n_t < 2)
cat("Encuestados con mismo Template dos veces:", nrow(check_templ),
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
    ) %>%
    mutate(
      idx_nombre         = if_else(Postulante == "A", 1L, 2L),
      Placeholder_Nombre = if_else(Texto_Genero == "Mujer",
                                   c("Marcela", "Natalia")[idx_nombre],
                                   c("Claudio", "Patricio")[idx_nombre])
    ) %>%
    select(-idx_nombre)
  
  df_output$Texto_Familia_Completo <- paste0("se encuentra ", df_output$Texto_Civil,
                                             ", ", df_output$Texto_Situacion)
  return(df_output)
}

# ==============================================================================
# 7. CREAR LAS DOS RAMAS
# ==============================================================================
df_rama_profesional <- procesar_textos_split(df_expandido, "Profesional")
df_rama_tecnica     <- procesar_textos_split(df_expandido, "Tecnico")

# ==============================================================================
# 8. EXPORTACIÓN
# ==============================================================================
cols_export <- c("Respondent_ID", "Block", "Postulante",
                 "Texto_Genero", "Placeholder_Nombre", "Texto_Familia_Completo",
                 "Placeholder_Ocupacion", "Placeholder_Educacion",
                 "Placeholder_Exp", "Placeholder_Desempeno", "Texto_Template")

archivo_profesional <- df_rama_profesional %>%
  select(all_of(cols_export)) %>% arrange(Respondent_ID, Postulante)
archivo_tecnico <- df_rama_tecnica %>%
  select(all_of(cols_export)) %>% arrange(Respondent_ID, Postulante)

# ==============================================================================
# 9. AUDITORÍA FINAL
# ==============================================================================
cat("\n===== AUDITORÍA FINAL =====\n")
cat("Filas profesional:", nrow(archivo_profesional),
    ifelse(nrow(archivo_profesional) == 1600, " ✓\n", " ← ERROR\n"))
cat("Filas técnico:    ", nrow(archivo_tecnico),
    ifelse(nrow(archivo_tecnico) == 1600, " ✓\n", " ← ERROR\n"))

dist_temp <- table(archivo_profesional$Texto_Template)
cat("Balance Template (A vs B):", dist_temp[1], "vs", dist_temp[2],
    ifelse(dist_temp[1] == dist_temp[2], " ✓\n", " ← ERROR\n"))

check_final <- archivo_profesional %>%
  group_by(Respondent_ID) %>%
  summarise(n_t = n_distinct(Texto_Template)) %>%
  filter(n_t < 2)
cat("Encuestados con mismo Template dos veces:", nrow(check_final),
    ifelse(nrow(check_final) == 0, " ✓\n", " ← PROBLEMA\n"))
cat("===========================\n")

write.csv(archivo_profesional, "Netquest_Profesional_800.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(archivo_tecnico,     "Netquest_Tecnico_800.csv",     row.names = FALSE, fileEncoding = "UTF-8")

