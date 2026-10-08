# ==============================================================================
# DISEÑO FCR — Opción 1 (Template fuera de optFederov, cruzado por construcción)
# v2: + M4a en la optimización, + diagnósticos de auditoría, + IDs por brazo,
#     + output apilado en formato ANCHO (1600 filas) para la empresa de campo,
#     + orden de bloques robusto (vía $Blocks).
# Decisiones de diseño (niveles, textos, nombres) = FINALES, no se tocan.
# ==============================================================================

if(!require(AlgDesign)) install.packages("AlgDesign"); library(AlgDesign)
if(!require(dplyr))     install.packages("dplyr");     library(dplyr)
if(!require(tidyr))     install.packages("tidyr");     library(tidyr)   # <-- para pivot_wider

round10 <- function(x) { as.integer(ceiling(x / 10) * 10) }
options(contrasts = c("contr.sum", "contr.poly"))

# ==============================================================================
# 1. DEFINIR LOS NIVELES (SIN TEMPLATE)
# ==============================================================================
niveles_nuevos <- c(2, 2, 3, 3, 3, 3)
nombres_nuevos <- c("Genero", "Ecivil", "Eparental", "Planfamilia", "Exp", "Desempeno")
cand_uni <- gen.factorial(levels = niveles_nuevos, varNames = nombres_nuevos, factors = "all")

# ==============================================================================
# 2. DEFINICIÓN DEL MODELO  (<-- AHORA INCLUYE M4a: Genero x Ecivil x Eparental)
# ==============================================================================
# M2a/M2b/M4b cubiertos por Genero*Eparental*Planfamilia
# M3 + M4a cubiertos por Genero*Ecivil*Eparental
frm_final <- ~ Genero * Eparental * Planfamilia +
               Genero * Ecivil * Eparental +
               Exp + Desempeno

# ==============================================================================
# 3. CÁLCULO DE N Y OPTIMIZACIÓN
# ==============================================================================
set.seed(12345)
k_final  <- ncol(model.matrix(frm_final, cand_uni))   # = 28 con M4a
N_teorico <- round10(3 * k_final)                     # = 90
N_final   <- max(N_teorico, 200)                      # = 200 (sigue holgado)

cat("\n[REPORTE] Parámetros (k):", k_final, "| N teórico:", N_teorico, "| N fijado:", N_final, "\n")

opt_diseno <- optFederov(frml = frm_final, data = cand_uni, nTrials = N_final,
                         criterion = "D", nRepeats = 500, nullify = 1)

diseno_final <- as.data.frame(opt_diseno$design)
eval_final   <- eval.design(frm_final, diseno_final, X = cand_uni)
cat(sprintf("[MÉTRICAS] D-Efficiency: %.3f | G-Efficiency: %.3f\n", eval_final$De, eval_final$Ge))

# ==============================================================================
# 4. GENERACIÓN DE BLOQUES BASE  (<-- orden robusto desde $Blocks, no rep())
# ==============================================================================
set.seed(999)
n_por_bloque  <- 2
total_bloques <- N_final / n_por_bloque               # 100 bloques

bloques_opt <- optBlock(frml = frm_final, withinData = diseno_final,
                        blocksizes = rep(n_por_bloque, total_bloques), criterion = "D")

# Reconstruimos desde $Blocks (cada elemento = data.frame de un bloque).
# Así NO dependemos de que $design venga ordenado por bloque.
df_con_bloques <- do.call(rbind, lapply(seq_along(bloques_opt$Blocks), function(b) {
  d <- as.data.frame(bloques_opt$Blocks[[b]])
  d$Block <- b
  d
}))
rownames(df_con_bloques) <- NULL

# Identificamos cada fila dentro de su bloque (Fila 1 y Fila 2)
df_con_bloques <- df_con_bloques %>%
  group_by(Block) %>%
  mutate(Fila_Base = row_number()) %>%
  ungroup()

# ==============================================================================
# 5. EXPANSIÓN A 800 ENCUESTADOS/BRAZO Y PERMUTACIÓN (ORDEN Y TEMPLATE)
# ==============================================================================
encuestados_totales <- 800
repeticiones <- encuestados_totales / total_bloques   # 8 repeticiones por bloque

df_expandido <- df_con_bloques %>%
  slice(rep(1:n(), times = repeticiones)) %>%
  mutate(Respondent_ID = rep(1:encuestados_totales, each = 2)) %>%
  group_by(Block, Fila_Base) %>%
  mutate(Ciclo = row_number()) %>%
  ungroup() %>%
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
  arrange(Respondent_ID, Orden_Aparicion)

# ==============================================================================
# 6. FUNCIÓN DE TEXTOS (agrupando por encuestado)
# ==============================================================================
procesar_textos_split <- function(df_input) {
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
      Placeholder_Exp        = case_when(Exp == "1" ~ "6 años", Exp == "2" ~ "7 años", Exp == "3" ~ "8 años"),
      Placeholder_Desempeno  = case_when(Desempeno == "1" ~ "5.8", Desempeno == "2" ~ "6.0", Desempeno == "3" ~ "6.3"),
      Texto_Template         = if_else(Template == "1", "Redacción A", "Redacción B")
    )

  nombres_mujer  <- c("Marcela", "Natalia")
  nombres_hombre <- c("Claudio", "Patricio")

  df_output <- df_output %>%
    group_by(Respondent_ID) %>%
    mutate(
      Placeholder_Nombre = if_else(Texto_Genero == "Mujer",
                                   nombres_mujer[Orden_Aparicion],
                                   nombres_hombre[Orden_Aparicion])
    ) %>%
    ungroup()

  df_output$Texto_Familia_Completo <- paste0("se encuentra ", df_output$Texto_Civil, ", ", df_output$Texto_Situacion)
  return(df_output)
}

df_base_procesada <- procesar_textos_split(df_expandido)

# ==============================================================================
# 7. RAMIFICACIÓN, IDs POR BRAZO Y LIMPIEZA FINAL
# ==============================================================================
df_base_limpia <- df_base_procesada %>%
  select(Respondent_ID, Block, Orden_Aparicion, Texto_Genero, Placeholder_Nombre,
         Texto_Familia_Completo, Placeholder_Exp, Placeholder_Desempeno, Texto_Template)

# --- Brazo PROFESIONAL: IDs 1..800 ---
df_long_profesional <- df_base_limpia %>%
  mutate(
    Brazo                 = "Profesional",
    ID_Global             = Respondent_ID,                 # 1..800
    Placeholder_Ocupacion = if_else(Texto_Genero == "Mujer", "Asociada financiera", "Asociado financiero"),
    Placeholder_Educacion = "Título profesional en Ingeniería Comercial"
  ) %>%
  relocate(ID_Global, Brazo, .before = Respondent_ID) %>%
  relocate(Placeholder_Ocupacion, Placeholder_Educacion, .after = Texto_Familia_Completo)

# --- Brazo TÉCNICO: IDs 801..1600 (evita colisión con el profesional) ---
df_long_tecnico <- df_base_limpia %>%
  mutate(
    Brazo                 = "Tecnico",
    ID_Global             = Respondent_ID + encuestados_totales,  # 801..1600
    Placeholder_Ocupacion = "Analista contable",
    Placeholder_Educacion = "Título técnico en Contabilidad"
  ) %>%
  relocate(ID_Global, Brazo, .before = Respondent_ID) %>%
  relocate(Placeholder_Ocupacion, Placeholder_Educacion, .after = Texto_Familia_Completo)

 
# ==============================================================================
# 8. CHEQUEOS DE BALANCE Y ROBUSTEZ (AUDITORÍA AMPLIADA)
# ==============================================================================
cat("\n=================== AUDITORÍA DEL DISEÑO ===================\n")
 
# --- 8.0 Diagnósticos del DISEÑO EXPERIMENTAL (Auspurg & Hinz, cap. 3.2.4 / 3.6) ---
 
# (a) Correlaciones del model.matrix, separando:
#     - DENTRO de un mismo término: intrínsecas del coding sum (~0.5 para factores
#       de 3 niveles, por aritmética de los contrastes). Son BENIGNAS: no son
#       confusión entre dimensiones.
#     - ENTRE términos distintos: ESTO es lo que debe ser ~0 (umbral 0.05, Auspurg).
mm   <- model.matrix(frm_final, diseno_final)[, -1, drop = FALSE]   # sin intercepto
cmat <- cor(mm)
vars_de  <- function(cn) paste(sort(nombres_nuevos[sapply(nombres_nuevos, grepl, x = cn)]), collapse = "*")
terminos <- vapply(colnames(mm), vars_de, character(1))
dif_term <- outer(terminos, terminos, "!=")
r_dentro <- max(abs(cmat[lower.tri(cmat) & !dif_term]))
r_entre  <- max(abs(cmat[lower.tri(cmat) &  dif_term]))
cat(sprintf("(a) Max |r| DENTRO de términos (coding sum, benigno): %.4f\n", r_dentro))
cat(sprintf("    Max |r| ENTRE términos distintos (el que importa):  %.4f  %s\n",
            r_entre, ifelse(r_entre <= 0.05, "✓ (<=0.05)",
                     ifelse(r_entre <= 0.10, "~ aceptable", "! revisar"))))
 
# (b) Balance de niveles por dimensión (deberían ser casi uniformes)
cat("(b) Balance de niveles por dimensión:\n")
for (v in nombres_nuevos) { cat("   ", v, ":", paste(table(diseno_final[[v]]), collapse = " / "), "\n") }
 
# (c) Calidad del bloqueo. OJO: el bloqueo solo PARTICIONA las mismas 200 viñetas,
#     así que la D-eff marginal del set es invariante. Lo que importa es la
#     "diagonality": cuán ortogonales quedan los bloques (decks) a las dimensiones.
cat(sprintf("(c) optBlock -> D: %.4f | diagonality: %.4f (cerca de 1 = decks ortogonales a tratamientos)\n",
            bloques_opt$D, bloques_opt$diagonality))
cat(sprintf("    D-eff marginal del set de 200 viñetas (invariante al bloqueo): %.3f\n", eval_final$De))
 
# (d) Identificabilidad: ¿el modelo (incl. M4a) es de rango completo en el diseño?
rango <- qr(model.matrix(frm_final, diseno_final))$rank
cat(sprintf("(d) Rango del model.matrix: %d / %d parámetros  %s\n",
            rango, k_final, ifelse(rango == k_final, "✓ todos identificables", "X DEFICIENTE")))
 
# --- 8.1 Diagnósticos del TEMPLATE / ORDEN (sobre la base expandida) ---
 
# (e) Cruce Template x perfil base: cada (Block, Fila_Base) debe ser 4 A y 4 B
chk_cruce <- df_expandido %>% count(Block, Fila_Base, Template)
cat("(e) Cruce completo Template x perfil (4 y 4 por perfil):",
    ifelse(all(chk_cruce$n == repeticiones / 2), "✓ APROBADO\n", "X ERROR\n"))
 
# (f) corr(Template, dimensión) ~ 0 en la base final (ortogonalidad realizada)
cat("(f) corr(Template, dimensión) en la base expandida:\n")
for (v in nombres_nuevos) {
  r <- cor(as.numeric(df_expandido$Template),
           as.numeric(as.character(df_expandido[[v]])))
  cat(sprintf("    corr(Template, %-12s) = %+.4f\n", v, r))
}
 
# --- 8.2 Diagnósticos clásicos (sobre un brazo; idénticos por construcción) ---
dist_temp <- table(df_long_profesional$Texto_Template)
cat("(g) Balance total de Templates (A vs B):", dist_temp[1], "vs", dist_temp[2],
    ifelse(dist_temp[1] == dist_temp[2], "✓ APROBADO\n", "X ERROR\n"))
 
dist_orden <- table(df_long_profesional$Texto_Template, df_long_profesional$Orden_Aparicion)
cat("(h) Balance de Orden (Template A pos.1 vs pos.2):", dist_orden[1,1], "vs", dist_orden[1,2],
    ifelse(dist_orden[1,1] == dist_orden[1,2], "✓ APROBADO\n", "X ERROR\n"))
 
check_encuestado <- df_long_profesional %>%
  group_by(ID_Global) %>% summarise(n_temp = n_distinct(Texto_Template)) %>% filter(n_temp < 2)
cat("(i) Encuestados que ven el mismo Template 2 veces:", nrow(check_encuestado),
    ifelse(nrow(check_encuestado) == 0, "✓ APROBADO\n", "X ERROR\n"))
 
# (j) IDs únicos entre brazos
ids_dup <- intersect(df_long_profesional$ID_Global, df_long_tecnico$ID_Global)
cat("(j) IDs compartidos entre brazos:", length(ids_dup),
    ifelse(length(ids_dup) == 0, "✓ APROBADO\n", "X ERROR\n"))
cat("===========================================================\n\n")
 
# ==============================================================================
# 8.3 DIAGNÓSTICOS ADICIONALES — BALANCE Y ORTOGONALIDAD DEL TEMPLATE
# (sobre df_base_procesada: base pre-ramificación, donde viven Texto_Template,
#  Orden_Aparicion, Respondent_ID y los códigos de las dimensiones. Como la
#  asignación de Template/Orden es idéntica para ambos brazos, basta correrlo
#  una vez aquí; no es necesario repetirlo por brazo.)
# ==============================================================================
cat("\n=======================================================\n")
cat("      TESTEO DE BALANCE Y ORTOGONALIDAD (TEMPLATE)     \n")
cat("=======================================================\n")
 
df_final <- df_base_procesada  # alias para legibilidad de este bloque
 
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
 
# 4. Prueba de Ortogonalidad (Chi-Cuadrado)
# Verifica si la asignación del Template generó correlaciones espurias
# con las variables clave en esta fracción de N_final.
# (las 6 dimensiones del diseño; valores p > 0.05 = NO hay asociación = ortogonalidad exitosa)
cat("\n4. Test de Independencia (Chi-cuadrado) Template vs Variables Sustantivas:\n")
vars_test <- nombres_nuevos  # = c("Genero","Ecivil","Eparental","Planfamilia","Exp","Desempeno")
for (v in vars_test) {
  tbl <- table(df_final$Texto_Template, df_final[[v]])
  chi_test <- suppressWarnings(chisq.test(tbl))
  cat(sprintf("   - Template vs %-12s: p-value = %.3f\n", v, chi_test$p.value))
}
cat("=======================================================\n\n")
 
# ==============================================================================
# 9. OUTPUT APILADO EN FORMATO ANCHO (1600 filas) PARA LA EMPRESA DE CAMPO
# ==============================================================================
# Un cuestionario por fila: V1_* = viñeta mostrada 1°, V2_* = viñeta mostrada 2°.
# Equivale al "setup data" de Auspurg & Hinz (cap. 4, Fig. 4-3/4-4) para mail-merge.
df_long_todos <- bind_rows(df_long_profesional, df_long_tecnico)
 
cols_vineta <- c("Texto_Genero", "Placeholder_Nombre", "Texto_Familia_Completo",
                 "Placeholder_Ocupacion", "Placeholder_Educacion",
                 "Placeholder_Exp", "Placeholder_Desempeno", "Texto_Template")
 
df_wide <- df_long_todos %>%
  pivot_wider(
    id_cols     = c(ID_Global, Brazo, Block),
    names_from  = Orden_Aparicion,
    values_from = all_of(cols_vineta),
    names_glue  = "V{Orden_Aparicion}_{.value}"
  ) %>%
  arrange(ID_Global)
 
cat("[WIDE] Filas:", nrow(df_wide), "(esperado 1600) | IDs únicos:", n_distinct(df_wide$ID_Global), "\n")
stopifnot(nrow(df_wide) == 1600, n_distinct(df_wide$ID_Global) == 1600)
 
# ==============================================================================
# 10. EXPORTACIÓN
# ==============================================================================
write.csv(df_long_profesional, "Netquest_Long_Profesional.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(df_long_tecnico,     "Netquest_Long_Tecnico.csv",     row.names = FALSE, fileEncoding = "UTF-8")
write.csv(df_wide,             "Netquest_Wide_Apilado_1600.csv", row.names = FALSE, fileEncoding = "UTF-8")
 
cat("Exportados: Netquest_Long_Profesional.csv, Netquest_Long_Tecnico.csv, Netquest_Wide_Apilado_1600.csv\n")
 