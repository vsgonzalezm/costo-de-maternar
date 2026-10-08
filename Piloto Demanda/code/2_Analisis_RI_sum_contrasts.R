# ============================================================
# SCRIPT: 2_Analisis_RI_sum_contrasts.R
#
# Análisis con modelos de intercepto aleatorio (RI)
# y codificación de efectos (sum / deviation contrasts)
#
# ESTRUCTURA:
#   PARTE A — Modelos RI con los 16 outcomes individuales
#   PARTE B — Modelos RI con índices agregados (3 índices + 2 individuales)
#   PARTE C — Figuras de valores esperados para grupos sustantivos
#
# Referencia metodológica: Auspurg & Hinz (2017), Cap. 5
#   → RI por respondente (L2); dimensiones de viñeta en L1
#   → No se anida por viñeta: su varianza está explicada
#     por las dimensiones del modelo (perfil_genero, etc.)
# ============================================================


# ============================================================
# 0. LIBRERÍAS
# ============================================================

pkgs <- c("tidyverse", "haven", "lme4", "lmerTest",
          "modelsummary", "ggplot2", "emmeans", "psych")

for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

# broom.mixed para tidy() en objetos lmer
if (!requireNamespace("broom.mixed", quietly = TRUE)) install.packages("broom.mixed")
library(broom.mixed)


# ============================================================
# HELPER 1: tidy_lmer — extrae coeficientes de lmer
# Funciona aunque broom.mixed tenga problemas de versión
# ============================================================
# ============================================================
# CORRECCIÓN: tidy_lmer con conf.low/conf.high garantizados
# ============================================================

tidy_lmer <- function(modelo, effects = "fixed") {
  if (is.null(modelo)) return(NULL)
  
  resultado <- tryCatch({
    broom.mixed::tidy(modelo, effects = effects, conf.int = TRUE)
  }, error = function(e) {
    # Fallback manual
    coefs <- as.data.frame(coef(summary(modelo)))
    coefs %>%
      tibble::rownames_to_column("term") %>%
      rename(
        estimate  = Estimate,
        std.error = `Std. Error`,
        statistic = `t value`
      ) %>%
      mutate(
        df        = as.data.frame(coef(summary(modelo)))[["df"]],
        p.value   = 2 * pt(-abs(statistic), df = df),
        effect    = "fixed"
      )
  })
  
  # Garantizar que conf.low y conf.high siempre existen
  if (!"conf.low" %in% names(resultado)) {
    resultado <- resultado %>%
      mutate(
        conf.low  = estimate - 1.96 * std.error,
        conf.high = estimate + 1.96 * std.error
      )
  }
  
  resultado
}


# Verificar que funciona antes de continuar
test_tidy <- tidy_lmer(resultados[["SET1_principales"]][["ans_p1_global"]])
cat("Columnas disponibles:", paste(names(test_tidy), collapse = ", "), "\n")
cat("¿conf.low existe?", "conf.low" %in% names(test_tidy), "\n")
cat("¿conf.high existe?", "conf.high" %in% names(test_tidy), "\n")


# ============================================================
# HELPER 2: aplicar_contr_sum
# ============================================================

aplicar_contr_sum <- function(fac, ref_level = NULL) {
  if (!is.factor(fac)) fac <- as.factor(fac)
  lvls <- levels(fac)
  if (is.null(ref_level)) ref_level <- lvls[1]
  stopifnot(ref_level %in% lvls)
  lvls_nuevo <- c(setdiff(lvls, ref_level), ref_level)
  fac        <- factor(fac, levels = lvls_nuevo)
  n          <- length(lvls_nuevo)
  C          <- contr.sum(n)
  colnames(C) <- lvls_nuevo[-n]
  contrasts(fac) <- C
  fac
}


# ============================================================
# 1. DIRECTORIO Y CARPETAS DE SALIDA
# ============================================================

# Ajustar si es necesario
# setwd("~/Desktop/Diseno_demanda")

dir.create("tablas/RI_sum_contrasts",          recursive = TRUE, showWarnings = FALSE)
dir.create("tablas/RI_indices",                recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/RI_sum_contrasts",         recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/RI_indices",               recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/valores_esperados",        recursive = TRUE, showWarnings = FALSE)

out_tab_ri   <- "tablas/RI_sum_contrasts"
out_tab_idx  <- "tablas/RI_indices"
out_fig_ri   <- "figuras/RI_sum_contrasts"
out_fig_idx  <- "figuras/RI_indices"
out_fig_pred <- "figuras/valores_esperados"


# ============================================================
# 2. CARGA DE DATOS
# ============================================================

base_long_final <- read_sav("base_pretest_long_final.sav")


# ============================================================
# 3. PREPARACIÓN DE VARIABLES CON SUM CONTRASTS
# ============================================================

df <- base_long_final %>%
  mutate(
    perfil_genero = aplicar_contr_sum(
      as_factor(perfil_genero), ref_level = "Hombre"),

    perfil_estado_civil = aplicar_contr_sum(
      as_factor(perfil_estado_civil), ref_level = "soltero/a"),

    perfil_parental = aplicar_contr_sum(
      factor(perfil_parental,
             levels = c("actualmente no tiene hijos",
                        "tiene un hijo", "tiene dos hijos")),
      ref_level = "actualmente no tiene hijos"),

    perfil_plan_futuro = aplicar_contr_sum(
      as_factor(perfil_plan_futuro), ref_level = "no quiere"),

    perfil_ocupacion  = aplicar_contr_sum(as_factor(perfil_ocupacion)),
    perfil_exp        = aplicar_contr_sum(as_factor(perfil_exp)),
    perfil_desempeno  = aplicar_contr_sum(as_factor(perfil_desempeno)),
    pantalla_orden    = aplicar_contr_sum(as.factor(pantalla_orden)),
    template_contexto = aplicar_contr_sum(as.factor(template_contexto))
  )

cat("✓ Sum contrasts aplicados\n")


# ============================================================
# 4. DEFINICIÓN DE OUTCOMES Y ETIQUETAS
# ============================================================

outcomes_p1             <- "ans_p1_global"
outcomes_evaluacion     <- paste0("ans_p2_", 1:7)
outcomes_comportamiento <- paste0("ans_p3_", 1:8)
all_outcomes            <- c(outcomes_p1, outcomes_evaluacion,
                              outcomes_comportamiento)

labels_outcomes <- c(
  "ans_p1_global" = "Recomendación de contratación",
  "ans_p2_1"      = "Confiable",
  "ans_p2_2"      = "Capaz",
  "ans_p2_3"      = "Comprometido/a largo plazo",
  "ans_p2_4"      = "Prometedor/a",
  "ans_p2_5"      = "Ambicioso/a",
  "ans_p2_6"      = "Riesgoso/a para la empresa",
  "ans_p2_7"      = "Costoso/a para la empresa",
  "ans_p3_1"      = "Usará beneficios empresa",
  "ans_p3_2"      = "Solicitará facilidades/permisos",
  "ans_p3_3"      = "Llegará tarde o se ausentará",
  "ans_p3_4"      = "Renunciará en próximos años",
  "ans_p3_5"      = "Tomará mayores responsabilidades",
  "ans_p3_6"      = "Será ascendido/a o promovido/a",
  "ans_p3_7"      = "Generará costos económicos adicionales",
  "ans_p3_8"      = "Generará costos operativos/organizacionales"
)


# ============================================================
# 5. CREACIÓN DE ÍNDICES
# Basado en análisis de consistencia interna (alpha > 0.77)
# ============================================================

df <- df %>%
  rowwise() %>%
  mutate(
    # Alpha = 0.877 en muestra completa
    idx_evaluacion_pos = mean(c_across(all_of(
      c("ans_p2_1", "ans_p2_2", "ans_p2_3",
        "ans_p2_4", "ans_p3_5", "ans_p3_6"))),
      na.rm = TRUE),

    # Alpha = 0.779
    idx_riesgo_conductual = mean(c_across(all_of(
      c("ans_p3_2", "ans_p3_3", "ans_p3_4"))),
      na.rm = TRUE),

    # Alpha = 0.828
    idx_costo_percibido = mean(c_across(all_of(
      c("ans_p2_6", "ans_p2_7",
        "ans_p3_7", "ans_p3_8"))),
      na.rm = TRUE)
  ) %>%
  ungroup()

outcomes_indices <- c(
  "ans_p1_global",
  "idx_evaluacion_pos",
  "idx_riesgo_conductual",
  "idx_costo_percibido",
  "ans_p2_5"
)

labels_indices <- c(
  "ans_p1_global"         = "Recomendación de contratación",
  "idx_evaluacion_pos"    = "Evaluación positiva (índice)",
  "idx_riesgo_conductual" = "Riesgo conductual (índice)",
  "idx_costo_percibido"   = "Costo percibido (índice)",
  "ans_p2_5"              = "Ambicioso/a"
)

cat("✓ Índices creados\n")
cat(sprintf("  idx_evaluacion_pos:    M=%.2f, SD=%.2f\n",
            mean(df$idx_evaluacion_pos,    na.rm=TRUE),
            sd(df$idx_evaluacion_pos,      na.rm=TRUE)))
cat(sprintf("  idx_riesgo_conductual: M=%.2f, SD=%.2f\n",
            mean(df$idx_riesgo_conductual, na.rm=TRUE),
            sd(df$idx_riesgo_conductual,   na.rm=TRUE)))
cat(sprintf("  idx_costo_percibido:   M=%.2f, SD=%.2f\n",
            mean(df$idx_costo_percibido,   na.rm=TRUE),
            sd(df$idx_costo_percibido,     na.rm=TRUE)))


# ============================================================
# 6. FÓRMULAS POR SET
# ============================================================

controles <- "pantalla_orden + template_contexto"

sets_formulas <- list(

  "SET1_principales" = paste(
    "perfil_genero + perfil_estado_civil + perfil_parental +",
    "perfil_plan_futuro + perfil_ocupacion +",
    "perfil_exp + perfil_desempeno +", controles),

  "SET2a_gen_x_parental" = paste(
    "perfil_genero * perfil_parental +",
    "perfil_estado_civil + perfil_plan_futuro +",
    "perfil_ocupacion + perfil_exp + perfil_desempeno +", controles),

  "SET2b_gen_x_planfam" = paste(
    "perfil_genero * perfil_plan_futuro +",
    "perfil_estado_civil + perfil_parental +",
    "perfil_ocupacion + perfil_exp + perfil_desempeno +", controles),

  "SET3_gen_x_ecivil" = paste(
    "perfil_genero * perfil_estado_civil +",
    "perfil_parental + perfil_plan_futuro +",
    "perfil_ocupacion + perfil_exp + perfil_desempeno +", controles),

  "SET4a_gen_x_ecivil_x_parental" = paste(
    "perfil_genero * perfil_estado_civil * perfil_parental +",
    "perfil_plan_futuro +",
    "perfil_ocupacion + perfil_exp + perfil_desempeno +", controles),

  "SET4b_gen_x_parental_x_plan" = paste(
    "perfil_genero * perfil_parental * perfil_plan_futuro +",
    "perfil_estado_civil +",
    "perfil_ocupacion + perfil_exp + perfil_desempeno +", controles)
)


# ============================================================
# 7. FUNCIÓN: CORRER MODELOS RI
# Intercepto aleatorio por respondente (L2)
# Dimensiones de viñeta en L1 — NO se anida por viñeta
# (su varianza queda explicada por las dimensiones del modelo)
# ============================================================

correr_modelos_ri <- function(formula_str, outcomes, datos) {
  map(outcomes, function(y) {
    fml <- as.formula(paste(y, "~", formula_str, "+ (1 | numericalId)"))
    tryCatch(
      lmer(fml, data = datos, REML = TRUE),
      error   = function(e) {
        message(sprintf("  Error en %s: %s", y, e$message))
        NULL
      },
      warning = function(w) suppressWarnings(
        lmer(fml, data = datos, REML = TRUE))
    )
  }) %>% set_names(outcomes)
}


# ============================================================
# 8. CORRER MODELOS — PARTE A: 16 outcomes individuales
# ============================================================

cat("\n=== PARTE A: Corriendo modelos RI (16 outcomes) ===\n")

resultados <- map(sets_formulas, function(fml_str) {
  correr_modelos_ri(fml_str, all_outcomes, df)
})

cat("✓ Modelos RI corridos:",
    length(sets_formulas), "sets ×",
    length(all_outcomes), "outcomes =",
    length(sets_formulas) * length(all_outcomes), "modelos\n")


# ============================================================
# 9. EXPORTAR TABLAS — PARTE A
# ============================================================

exportar_tabla_ri <- function(lista_modelos, outcomes,
                               labels, nombre_archivo,
                               notas_extra = NULL) {
  # Filtrar modelos NULL
  modelos_sel <- lista_modelos[outcomes]
  modelos_sel <- modelos_sel[!sapply(modelos_sel, is.null)]
  if (length(modelos_sel) == 0) {
    cat("  ⚠ Sin modelos válidos para:", nombre_archivo, "\n")
    return(invisible(NULL))
  }
  names(modelos_sel) <- labels[names(modelos_sel)]

  notas_base <- c(
    "Modelos de intercepto aleatorio (lme4/lmerTest).",
    "Intercepto aleatorio por respondente (L2).",
    "Codificación de efectos (sum contrasts): cada coeficiente =",
    "desviación del nivel respecto al promedio del diseño experimental.",
    "p-values de Satterthwaite."
  )

  modelsummary(
    modelos_sel,
    stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    gof_map = list(
      list(raw = "nobs",   clean = "N observaciones", fmt = 0),
      list(raw = "icc",    clean = "ICC (ρ)",          fmt = 3),
      list(raw = "logLik", clean = "Log-likelihood",   fmt = 1),
      list(raw = "AIC",    clean = "AIC",               fmt = 1)
    ),
    notes  = c(notas_base, notas_extra),
    output = nombre_archivo
  )
  cat("  ✓", nombre_archivo, "\n")
}

cat("\nExportando tablas SET1...\n")
exportar_tabla_ri(resultados[["SET1_principales"]], outcomes_p1,
                   labels_outcomes,
                   file.path(out_tab_ri, "SET1_contratacion.docx"))
exportar_tabla_ri(resultados[["SET1_principales"]], outcomes_evaluacion,
                   labels_outcomes,
                   file.path(out_tab_ri, "SET1_evaluaciones.docx"))
exportar_tabla_ri(resultados[["SET1_principales"]], outcomes_comportamiento,
                   labels_outcomes,
                   file.path(out_tab_ri, "SET1_comportamiento.docx"))

cat("Exportando tablas SET2a...\n")
exportar_tabla_ri(resultados[["SET2a_gen_x_parental"]], outcomes_p1,
                   labels_outcomes,
                   file.path(out_tab_ri, "SET2a_contratacion.docx"))
exportar_tabla_ri(resultados[["SET2a_gen_x_parental"]], outcomes_evaluacion,
                   labels_outcomes,
                   file.path(out_tab_ri, "SET2a_evaluaciones.docx"))
exportar_tabla_ri(resultados[["SET2a_gen_x_parental"]], outcomes_comportamiento,
                   labels_outcomes,
                   file.path(out_tab_ri, "SET2a_comportamiento.docx"))

cat("Exportando tablas SET2b...\n")
exportar_tabla_ri(resultados[["SET2b_gen_x_planfam"]], outcomes_p1,
                   labels_outcomes,
                   file.path(out_tab_ri, "SET2b_contratacion.docx"))
exportar_tabla_ri(resultados[["SET2b_gen_x_planfam"]], outcomes_evaluacion,
                   labels_outcomes,
                   file.path(out_tab_ri, "SET2b_evaluaciones.docx"))
exportar_tabla_ri(resultados[["SET2b_gen_x_planfam"]], outcomes_comportamiento,
                   labels_outcomes,
                   file.path(out_tab_ri, "SET2b_comportamiento.docx"))

cat("Exportando tablas SET3...\n")
exportar_tabla_ri(resultados[["SET3_gen_x_ecivil"]], outcomes_p1,
                   labels_outcomes,
                   file.path(out_tab_ri, "SET3_contratacion.docx"))
exportar_tabla_ri(resultados[["SET3_gen_x_ecivil"]], outcomes_evaluacion,
                   labels_outcomes,
                   file.path(out_tab_ri, "SET3_evaluaciones.docx"))
exportar_tabla_ri(resultados[["SET3_gen_x_ecivil"]], outcomes_comportamiento,
                   labels_outcomes,
                   file.path(out_tab_ri, "SET3_comportamiento.docx"))

cat("Exportando tablas SET4a...\n")
exportar_tabla_ri(resultados[["SET4a_gen_x_ecivil_x_parental"]], outcomes_p1,
                   labels_outcomes,
                   file.path(out_tab_ri, "SET4a_contratacion.docx"))
exportar_tabla_ri(resultados[["SET4a_gen_x_ecivil_x_parental"]], outcomes_evaluacion,
                   labels_outcomes,
                   file.path(out_tab_ri, "SET4a_evaluaciones.docx"))
exportar_tabla_ri(resultados[["SET4a_gen_x_ecivil_x_parental"]], outcomes_comportamiento,
                   labels_outcomes,
                   file.path(out_tab_ri, "SET4a_comportamiento.docx"))

cat("Exportando tablas SET4b...\n")
exportar_tabla_ri(resultados[["SET4b_gen_x_parental_x_plan"]], outcomes_p1,
                   labels_outcomes,
                   file.path(out_tab_ri, "SET4b_contratacion.docx"))
exportar_tabla_ri(resultados[["SET4b_gen_x_parental_x_plan"]], outcomes_evaluacion,
                   labels_outcomes,
                   file.path(out_tab_ri, "SET4b_evaluaciones.docx"))
exportar_tabla_ri(resultados[["SET4b_gen_x_parental_x_plan"]], outcomes_comportamiento,
                   labels_outcomes,
                   file.path(out_tab_ri, "SET4b_comportamiento.docx"))

cat("✓ Todas las tablas PARTE A exportadas\n")


# ============================================================
# 10. EXTRAER RESULTADOS PARA FIGURAS — PARTE A
# ============================================================

extraer_resultados <- function(resultados_set, outcomes) {
  map_dfr(outcomes, function(y) {
    m <- resultados_set[[y]]
    if (is.null(m)) return(NULL)
    tidy_lmer(m) %>%
      filter(effect == "fixed") %>%
      mutate(outcome = y)
  })
}

resultados_set1  <- extraer_resultados(resultados[["SET1_principales"]],            all_outcomes)
resultados_set2a <- extraer_resultados(resultados[["SET2a_gen_x_parental"]],        all_outcomes)
resultados_set2b <- extraer_resultados(resultados[["SET2b_gen_x_planfam"]],         all_outcomes)
resultados_set3  <- extraer_resultados(resultados[["SET3_gen_x_ecivil"]],           all_outcomes)
resultados_set4a <- extraer_resultados(resultados[["SET4a_gen_x_ecivil_x_parental"]], all_outcomes)
resultados_set4b <- extraer_resultados(resultados[["SET4b_gen_x_parental_x_plan"]], all_outcomes)

cat("✓ Resultados extraídos para figuras\n")

# Verificar términos
cat("\n--- Términos SET1 ---\n")
resultados_set1 %>% distinct(term) %>% pull(term) %>% sort() %>% print()


# ============================================================
# 11. FUNCIÓN DE ETIQUETAS
# ============================================================

agregar_etiquetas <- function(df_res, nombre_set,
                               labs_out = labels_outcomes) {
  df_res %>%
    filter(!is.na(estimate)) %>%
    mutate(
      set = nombre_set,

      grupo_outcome = case_when(
        outcome == "ans_p1_global"               ~ "Contratación",
        outcome %in% outcomes_evaluacion         ~ "Evaluación",
        outcome %in% outcomes_comportamiento     ~ "Comportamiento futuro",
        outcome == "idx_evaluacion_pos"          ~ "Índices",
        outcome == "idx_riesgo_conductual"       ~ "Índices",
        outcome == "idx_costo_percibido"         ~ "Índices",
        TRUE                                      ~ "Otro"
      ),

      outcome_label = case_when(
        outcome == "ans_p1_global"         ~ "Recomendación contratación",
        outcome == "ans_p2_1"              ~ "Confiable",
        outcome == "ans_p2_2"              ~ "Capaz",
        outcome == "ans_p2_3"              ~ "Comprometido/a largo plazo",
        outcome == "ans_p2_4"              ~ "Prometedor/a",
        outcome == "ans_p2_5"              ~ "Ambicioso/a",
        outcome == "ans_p2_6"              ~ "Riesgoso/a para empresa",
        outcome == "ans_p2_7"              ~ "Costoso/a financieramente",
        outcome == "ans_p3_1"              ~ "Uso de beneficios",
        outcome == "ans_p3_2"             ~ "Requerirá facilidades/permisos",
        outcome == "ans_p3_3"              ~ "Llegará tarde / se ausentará",
        outcome == "ans_p3_4"              ~ "Renunciará en próximos años",
        outcome == "ans_p3_5"              ~ "Tomará mayores responsabilidades",
        outcome == "ans_p3_6"              ~ "Será ascendido/a",
        outcome == "ans_p3_7"              ~ "Generará costos económicos",
        outcome == "ans_p3_8"              ~ "Generará costos operativos",
        outcome == "idx_evaluacion_pos"    ~ "Evaluación positiva (índice)",
        outcome == "idx_riesgo_conductual" ~ "Riesgo conductual (índice)",
        outcome == "idx_costo_percibido"   ~ "Costo percibido (índice)",
        TRUE ~ outcome
      ),

      term_label = case_when(
        # Efectos principales
        term == "perfil_generoMujer"                             ~ "Género: Mujer",
        term == "perfil_estado_civilcasado/a"                    ~ "Estado civil: Casado/a",
        term == "perfil_estado_civilconviviendo"                 ~ "Estado civil: Conviviendo",
        term == "perfil_parentaltiene un hijo"                   ~ "Parental: Un hijo",
        term == "perfil_parentaltiene dos hijos"                 ~ "Parental: Dos hijos",
        term == "perfil_plan_futuroen 2 años"                    ~ "Plan: en 2 años",
        term == "perfil_plan_futuroen 5 años"                    ~ "Plan: en 5 años",
        term == "perfil_exp6 años"                               ~ "Experiencia: 6 años",
        term == "perfil_exp8 años"                               ~ "Experiencia: 8 años",
        term == "perfil_desempeno6.0"                            ~ "Desempeño: 6.0",
        term == "perfil_desempeno6.3"                            ~ "Desempeño: 6.3",
        term == "perfil_ocupacionasociado/a financiero/a"        ~ "Ocupación: financiero/a",
        term == "pantalla_orden2"                                ~ "Orden: 2do perfil",
        term == "template_contextoB"                             ~ "Template: B",
        # Interacciones triples SET4a
        term == "perfil_generoMujer:perfil_estado_civilcasado/a:perfil_parentaltiene dos hijos"    ~ "Mujer × Casado/a × Dos hijos",
        term == "perfil_generoMujer:perfil_estado_civilcasado/a:perfil_parentaltiene un hijo"      ~ "Mujer × Casado/a × Un hijo",
        term == "perfil_generoMujer:perfil_estado_civilconviviendo:perfil_parentaltiene dos hijos" ~ "Mujer × Conviviendo × Dos hijos",
        term == "perfil_generoMujer:perfil_estado_civilconviviendo:perfil_parentaltiene un hijo"   ~ "Mujer × Conviviendo × Un hijo",
        # Interacciones triples SET4b
        term == "perfil_generoMujer:perfil_parentaltiene dos hijos:perfil_plan_futuroen 2 años"    ~ "Mujer × Dos hijos × Plan en 2 años",
        term == "perfil_generoMujer:perfil_parentaltiene dos hijos:perfil_plan_futuroen 5 años"    ~ "Mujer × Dos hijos × Plan en 5 años",
        term == "perfil_generoMujer:perfil_parentaltiene un hijo:perfil_plan_futuroen 2 años"      ~ "Mujer × Un hijo × Plan en 2 años",
        term == "perfil_generoMujer:perfil_parentaltiene un hijo:perfil_plan_futuroen 5 años"      ~ "Mujer × Un hijo × Plan en 5 años",
        # Interacciones dos vías: género × estado civil
        term == "perfil_generoMujer:perfil_estado_civilcasado/a"    ~ "Mujer × Casado/a",
        term == "perfil_generoMujer:perfil_estado_civilconviviendo" ~ "Mujer × Conviviendo",
        # Interacciones dos vías: género × parental
        term == "perfil_generoMujer:perfil_parentaltiene un hijo"   ~ "Mujer × Un hijo",
        term == "perfil_generoMujer:perfil_parentaltiene dos hijos" ~ "Mujer × Dos hijos",
        # Interacciones dos vías: género × plan
        term == "perfil_generoMujer:perfil_plan_futuroen 2 años"    ~ "Mujer × Plan en 2 años",
        term == "perfil_generoMujer:perfil_plan_futuroen 5 años"    ~ "Mujer × Plan en 5 años",
        # Interacciones dos vías: estado civil × parental
        term == "perfil_estado_civilcasado/a:perfil_parentaltiene dos hijos"    ~ "Casado/a × Dos hijos",
        term == "perfil_estado_civilcasado/a:perfil_parentaltiene un hijo"      ~ "Casado/a × Un hijo",
        term == "perfil_estado_civilconviviendo:perfil_parentaltiene dos hijos" ~ "Conviviendo × Dos hijos",
        term == "perfil_estado_civilconviviendo:perfil_parentaltiene un hijo"   ~ "Conviviendo × Un hijo",
        # Interacciones dos vías: parental × plan
        term == "perfil_parentaltiene dos hijos:perfil_plan_futuroen 2 años"    ~ "Dos hijos × Plan en 2 años",
        term == "perfil_parentaltiene dos hijos:perfil_plan_futuroen 5 años"    ~ "Dos hijos × Plan en 5 años",
        term == "perfil_parentaltiene un hijo:perfil_plan_futuroen 2 años"      ~ "Un hijo × Plan en 2 años",
        term == "perfil_parentaltiene un hijo:perfil_plan_futuroen 5 años"      ~ "Un hijo × Plan en 5 años",
        TRUE ~ term
      ),

      term_grupo = case_when(
        term == "perfil_generoMujer"                            ~ "Género",
        str_detect(term, "estado_civil") & !str_detect(term, ":") ~ "Estado civil",
        str_detect(term, "parental")     & !str_detect(term, ":") ~ "Estado parental",
        str_detect(term, "plan_futuro")  & !str_detect(term, ":") ~ "Plan reproductivo",
        str_detect(term, "genero.*parental.*plan|parental.*plan.*genero") ~ "Interacción triple",
        str_detect(term, "genero.*ecivil.*parental|ecivil.*parental.*genero") ~ "Interacción triple",
        str_detect(term, ":") & str_count(term, ":") >= 2      ~ "Interacción triple",
        str_detect(term, "genero") & str_detect(term, ":")     ~ "Interacción: Género",
        str_detect(term, ":")                                   ~ "Interacción: Otras",
        str_detect(term, "exp|desempeno|ocupacion")             ~ "Controles perfil",
        str_detect(term, "pantalla|template")                   ~ "Controles diseño",
        TRUE                                                     ~ "Otros"
      ),

      sig = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        p.value < 0.1   ~ ".",
        TRUE            ~ ""
      )
    )
}


# ============================================================
# 12. FUNCIÓN PARA GRÁFICOS
# ============================================================

orden_term_label_global <- c(
  "Género: Mujer",
  "Estado civil: Casado/a", "Estado civil: Conviviendo",
  "Parental: Un hijo", "Parental: Dos hijos",
  "Plan: en 2 años", "Plan: en 5 años",
  "Mujer × Casado/a", "Mujer × Conviviendo",
  "Mujer × Un hijo", "Mujer × Dos hijos",
  "Mujer × Plan en 2 años", "Mujer × Plan en 5 años",
  "Casado/a × Un hijo", "Casado/a × Dos hijos",
  "Conviviendo × Un hijo", "Conviviendo × Dos hijos",
  "Un hijo × Plan en 2 años", "Un hijo × Plan en 5 años",
  "Dos hijos × Plan en 2 años", "Dos hijos × Plan en 5 años",
  "Mujer × Casado/a × Un hijo", "Mujer × Casado/a × Dos hijos",
  "Mujer × Conviviendo × Un hijo", "Mujer × Conviviendo × Dos hijos",
  "Mujer × Un hijo × Plan en 2 años", "Mujer × Un hijo × Plan en 5 años",
  "Mujer × Dos hijos × Plan en 2 años", "Mujer × Dos hijos × Plan en 5 años",
  "Experiencia: 6 años", "Experiencia: 8 años",
  "Desempeño: 6.0", "Desempeño: 6.3",
  "Ocupación: financiero/a",
  "Orden: 2do perfil", "Template: B"
)

colores_grupos <- c(
  "Género"               = "#e41a1c",
  "Estado civil"         = "#377eb8",
  "Estado parental"      = "#4daf4a",
  "Plan reproductivo"    = "#ff7f00",
  "Interacción: Género"  = "#a65628",
  "Interacción: Otras"   = "#984ea3",
  "Interacción triple"   = "#f781bf",
  "Controles perfil"     = "#808080",
  "Controles diseño"     = "#cccccc",
  "Otros"                = "#999999"
)

graficar_set <- function(resultados_etiquetados, nombre_set,
                          prefijo_archivo, out_dir,
                          modo = "todos") {

  datos_viz <- resultados_etiquetados %>%
    filter(!str_detect(term, "Intercept"), !is.na(estimate)) %>%
    { if (modo == "interacciones") filter(., str_detect(term, ":"))
      else if (modo == "principales") filter(., !str_detect(term, ":"))
      else . } %>%
    mutate(
      term_grupo = factor(term_grupo, levels = names(colores_grupos)),
      term_label = factor(
        term_label,
        levels = orden_term_label_global[
          orden_term_label_global %in% unique(term_label)])
    )

  if (nrow(datos_viz) == 0) {
    cat("  ⚠ Sin datos para graficar:", prefijo_archivo, "\n")
    return(invisible(NULL))
  }

  sufijo <- if (modo == "interacciones") "_interacciones"
  else if (modo == "principales")        "_principales"
  else                                   ""

  for (grupo in unique(datos_viz$grupo_outcome)) {

    datos_grupo <- datos_viz %>% filter(grupo_outcome == grupo)
    if (nrow(datos_grupo) == 0) next

    n_terms <- n_distinct(datos_grupo$term_label)
    alto    <- max(8, n_terms * 0.38)

    p <- ggplot(datos_grupo,
                aes(x = estimate, y = term_label, color = term_grupo)) +
      geom_vline(xintercept = 0, linetype = "dashed",
                 color = "gray50", linewidth = 0.5) +
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                     height = 0.2, linewidth = 0.7, alpha = 0.8) +
      geom_point(size = 3) +
      geom_text(data = datos_grupo %>% filter(p.value < 0.1),
                aes(label = sig, x = conf.high + 0.05),
                size = 4, fontface = "bold", color = "black") +
      geom_hline(
        data = datos_grupo %>%
          group_by(term_grupo) %>%
          summarise(y_max = max(as.numeric(term_label)), .groups = "drop") %>%
          filter(y_max < max(as.numeric(datos_grupo$term_label))),
        aes(yintercept = y_max + 0.5),
        color = "gray80", linewidth = 0.4, linetype = "dotted",
        inherit.aes = FALSE
      ) +
      facet_wrap(~ outcome_label, ncol = 2, scales = "free_x") +
      scale_color_manual(
        values = colores_grupos,
        breaks = levels(datos_grupo$term_grupo)[
          levels(datos_grupo$term_grupo) %in% unique(datos_grupo$term_grupo)]
      ) +
      labs(
        title    = paste0(nombre_set, ": ", grupo,
                          if (modo == "interacciones") " (Interacciones)"
                          else if (modo == "principales") " (Efectos principales)"
                          else ""),
        subtitle = paste0(
          "Coeficientes RI — Codificación de efectos (sum contrasts)\n",
          "Cada coeficiente = desviación respecto al promedio del diseño\n",
          "Intercepto aleatorio por respondente (Auspurg & Hinz, 2017, Cap. 5)"
        ),
        x       = "Coeficiente estimado (IC 95%)",
        y       = NULL,
        color   = "Tipo de variable",
        caption = ". p < 0.1   * p < 0.05   ** p < 0.01   *** p < 0.001"
      ) +
      theme_bw(base_size = 11) +
      theme(
        strip.background = element_rect(fill = "#f0f0f0"),
        strip.text       = element_text(face = "bold", size = 9),
        legend.position  = "bottom",
        legend.title     = element_text(face = "bold"),
        plot.title       = element_text(face = "bold", size = 13),
        plot.subtitle    = element_text(size = 8, color = "gray40"),
        axis.text.y      = element_text(size = 9)
      )

    nombre_fig <- file.path(
      out_dir,
      paste0(prefijo_archivo, sufijo, "_",
             tolower(str_replace_all(grupo, "[/ ]", "_")), ".png")
    )

    ggsave(nombre_fig, plot = p,
           width = 14, height = alto, dpi = 150, bg = "white")
    cat("  ✓", nombre_fig, "\n")
  }
}


# ============================================================
# 13. GENERAR FIGURAS — PARTE A
# ============================================================

cat("\n=== Generando figuras PARTE A ===\n")

resultados_set1 %>%
  agregar_etiquetas("SET1: Efectos principales") %>%
  graficar_set("SET1 — Efectos principales", "grafico_set1",
               out_fig_ri, modo = "todos")

resultados_set2a %>%
  agregar_etiquetas("SET2a: Género × Estado parental") %>%
  graficar_set("SET2a — Género × Estado parental", "grafico_set2a",
               out_fig_ri, modo = "todos")

resultados_set2b %>%
  agregar_etiquetas("SET2b: Género × Plan familia") %>%
  graficar_set("SET2b — Género × Plan familia", "grafico_set2b",
               out_fig_ri, modo = "todos")

resultados_set3 %>%
  agregar_etiquetas("SET3: Género × Estado civil") %>%
  graficar_set("SET3 — Género × Estado civil", "grafico_set3",
               out_fig_ri, modo = "todos")

resultados_set4a_etiq <- resultados_set4a %>%
  agregar_etiquetas("SET4a: Género × Estado civil × Parental")

resultados_set4a_etiq %>%
  graficar_set("SET4a — Género × E. civil × Parental", "grafico_set4a",
               out_fig_ri, modo = "principales")

resultados_set4a_etiq %>%
  graficar_set("SET4a — Género × E. civil × Parental", "grafico_set4a",
               out_fig_ri, modo = "interacciones")

resultados_set4b_etiq <- resultados_set4b %>%
  agregar_etiquetas("SET4b: Género × Parental × Plan familia")

resultados_set4b_etiq %>%
  graficar_set("SET4b — Género × Parental × Plan familia", "grafico_set4b",
               out_fig_ri, modo = "principales")

resultados_set4b_etiq %>%
  graficar_set("SET4b — Género × Parental × Plan familia", "grafico_set4b",
               out_fig_ri, modo = "interacciones")

cat("✓ Figuras PARTE A completadas\n")


# ============================================================
# 14. PARTE B — MODELOS RI CON ÍNDICES
# ============================================================

cat("\n=== PARTE B: Corriendo modelos RI (índices) ===\n")

resultados_idx <- map(sets_formulas, function(fml_str) {
  correr_modelos_ri(fml_str, outcomes_indices, df)
})

cat("✓ Modelos RI índices corridos:",
    length(sets_formulas), "sets ×",
    length(outcomes_indices), "outcomes =",
    length(sets_formulas) * length(outcomes_indices), "modelos\n")


# --- Tablas índices ---

cat("\nExportando tablas índices...\n")

exportar_tabla_idx <- function(nombre_set, outcomes, nombre_archivo) {
  exportar_tabla_ri(
    resultados_idx[[nombre_set]], outcomes, labels_indices,
    nombre_archivo,
    notas_extra = c(
      "Índice evaluación positiva: media de ans_p2_1,2,3,4 + ans_p3_5,6 (α=0.877).",
      "Índice riesgo conductual: media de ans_p3_2,3,4 (α=0.779).",
      "Índice costo percibido: media de ans_p2_6,7 + ans_p3_7,8 (α=0.828)."
    )
  )
}

for (set_nm in names(sets_formulas)) {
  prefijo <- str_remove(set_nm, "SET[0-9ab]+_")
  exportar_tabla_idx(set_nm, outcomes_indices,
                     file.path(out_tab_idx,
                               paste0(set_nm, "_indices.docx")))
}

cat("✓ Tablas índices exportadas\n")


# --- Extraer resultados índices para figuras ---

res_idx_set1  <- extraer_resultados(resultados_idx[["SET1_principales"]],              outcomes_indices)
res_idx_set2a <- extraer_resultados(resultados_idx[["SET2a_gen_x_parental"]],          outcomes_indices)
res_idx_set2b <- extraer_resultados(resultados_idx[["SET2b_gen_x_planfam"]],           outcomes_indices)
res_idx_set3  <- extraer_resultados(resultados_idx[["SET3_gen_x_ecivil"]],             outcomes_indices)
res_idx_set4a <- extraer_resultados(resultados_idx[["SET4a_gen_x_ecivil_x_parental"]], outcomes_indices)
res_idx_set4b <- extraer_resultados(resultados_idx[["SET4b_gen_x_parental_x_plan"]],   outcomes_indices)


# --- Función etiquetas para índices ---

agregar_etiquetas_idx <- function(df_res, nombre_set) {
  agregar_etiquetas(df_res, nombre_set, labs_out = labels_indices) %>%
    mutate(
      grupo_outcome = case_when(
        outcome == "ans_p1_global"         ~ "Contratación",
        outcome == "idx_evaluacion_pos"    ~ "Evaluación positiva",
        outcome == "idx_riesgo_conductual" ~ "Riesgo conductual",
        outcome == "idx_costo_percibido"   ~ "Costo percibido",
        outcome == "ans_p2_5"              ~ "Ambicioso/a",
        TRUE ~ grupo_outcome
      )
    )
}


# --- Figuras índices ---

cat("\n=== Generando figuras PARTE B (índices) ===\n")

res_idx_set1 %>%
  agregar_etiquetas_idx("SET1: Efectos principales") %>%
  graficar_set("SET1 — Efectos principales (índices)", "grafico_set1_idx",
               out_fig_idx, modo = "todos")

res_idx_set2a %>%
  agregar_etiquetas_idx("SET2a: Género × Parental") %>%
  graficar_set("SET2a — Género × Parental (índices)", "grafico_set2a_idx",
               out_fig_idx, modo = "todos")

res_idx_set2b %>%
  agregar_etiquetas_idx("SET2b: Género × Plan") %>%
  graficar_set("SET2b — Género × Plan (índices)", "grafico_set2b_idx",
               out_fig_idx, modo = "todos")

res_idx_set3 %>%
  agregar_etiquetas_idx("SET3: Género × E. civil") %>%
  graficar_set("SET3 — Género × E. civil (índices)", "grafico_set3_idx",
               out_fig_idx, modo = "todos")

res_idx_set4a_etiq <- res_idx_set4a %>%
  agregar_etiquetas_idx("SET4a: Género × E. civil × Parental")

res_idx_set4a_etiq %>%
  graficar_set("SET4a (índices)", "grafico_set4a_idx",
               out_fig_idx, modo = "principales")
res_idx_set4a_etiq %>%
  graficar_set("SET4a (índices)", "grafico_set4a_idx",
               out_fig_idx, modo = "interacciones")

res_idx_set4b_etiq <- res_idx_set4b %>%
  agregar_etiquetas_idx("SET4b: Género × Parental × Plan")

res_idx_set4b_etiq %>%
  graficar_set("SET4b (índices)", "grafico_set4b_idx",
               out_fig_idx, modo = "principales")
res_idx_set4b_etiq %>%
  graficar_set("SET4b (índices)", "grafico_set4b_idx",
               out_fig_idx, modo = "interacciones")

cat("✓ Figuras PARTE B completadas\n")


# ============================================================
# 15. PARTE C — VALORES ESPERADOS PARA GRUPOS SUSTANTIVOS
#
# Modelo base: SET2b (género × plan + parental como efecto principal)
# Porque captura los mecanismos FCR centrales
#
# Grupos de interés:
#   G1: Hombre/Mujer, soltero/a, sin hijos, sin planes
#   G2: Hombre/Mujer, casado/a, con hijos, plan 2 años
#   G3: Hombre/Mujer, casado/a, con hijos, plan 5 años
#   G4: Mujer, sin hijos → 2 hijos, sin planes → plan 5 años
#
# Resto de covariables: en el promedio del diseño (= 0 con sum contrasts)
# ============================================================

cat("\n=== PARTE C: Valores esperados para grupos sustantivos ===\n")

# Usar SET2b para predicciones (género × plan + parental)
modelos_pred <- resultados_idx[["SET2b_gen_x_planfam"]]

# Función para calcular valores esperados con IC
# usando emmeans (maneja sum contrasts correctamente)

calcular_valores_esperados <- function(modelo, outcome_nm,
                                        specs, at_vals) {
  if (is.null(modelo)) return(NULL)

  tryCatch({
    em <- emmeans(modelo, specs = specs, at = at_vals,
                  type = "response")
    as.data.frame(em) %>%
      mutate(outcome = outcome_nm,
             outcome_label = labels_indices[outcome_nm])
  }, error = function(e) {
    message(sprintf("  emmeans error en %s: %s", outcome_nm, e$message))
    NULL
  })
}


# ============================================================
# FIGURA C1: Hombres vs Mujeres
# Soltero/a, sin hijos, sin planes (perfil de menor riesgo FCR)
# vs Casado/a, con hijos, con planes (perfil de mayor riesgo FCR)
# ============================================================

cat("\n--- Figura C1: Grupos por género y perfil reproductivo ---\n")

grupos_c1 <- list(
  at = list(
    perfil_genero       = c("Mujer", "Hombre"),
    perfil_parental     = c("actualmente no tiene hijos", "tiene dos hijos"),
    perfil_plan_futuro  = c("no quiere", "en 2 años", "en 5 años"),
    perfil_estado_civil = c("soltero/a", "casado/a")
  )
)

pred_c1 <- map_dfr(outcomes_indices, function(y) {
  m <- modelos_pred[[y]]
  calcular_valores_esperados(
    m, y,
    specs   = ~ perfil_genero * perfil_plan_futuro * perfil_parental,
    at_vals = grupos_c1$at
  )
}) %>%
  filter(
    # Grupo 1: bajo riesgo FCR
    (perfil_parental == "actualmente no tiene hijos" &
       perfil_plan_futuro == "no quiere") |
      # Grupo 2: alto riesgo FCR — plan 2 años
      (perfil_parental == "tiene dos hijos" &
         perfil_plan_futuro == "en 2 años") |
      # Grupo 3: alto riesgo FCR — plan 5 años
      (perfil_parental == "tiene dos hijos" &
         perfil_plan_futuro == "en 5 años")
  ) %>%
  mutate(
    perfil_genero = factor(perfil_genero, levels = c("Hombre", "Mujer")),
    grupo_perfil  = case_when(
      perfil_parental == "actualmente no tiene hijos" &
        perfil_plan_futuro == "no quiere"  ~ "Sin hijos,\nsin planes",
      perfil_parental == "tiene dos hijos" &
        perfil_plan_futuro == "en 2 años"  ~ "2 hijos,\nplan en 2 años",
      perfil_parental == "tiene dos hijos" &
        perfil_plan_futuro == "en 5 años"  ~ "2 hijos,\nplan en 5 años"
    ),
    grupo_perfil = factor(grupo_perfil,
                           levels = c("Sin hijos,\nsin planes",
                                      "2 hijos,\nplan en 2 años",
                                      "2 hijos,\nplan en 5 años")),
    outcome_label = factor(outcome_label,
                           levels = labels_indices[outcomes_indices])
  )

if (nrow(pred_c1) > 0) {
  fig_c1 <- ggplot(pred_c1,
                   aes(x = grupo_perfil, y = emmean,
                       color = perfil_genero,
                       group = perfil_genero)) +
    geom_line(linewidth = 0.8, alpha = 0.7,
              position = position_dodge(width = 0.3)) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                  width = 0.15, linewidth = 0.7,
                  position = position_dodge(width = 0.3)) +
    geom_point(size = 3,
               position = position_dodge(width = 0.3)) +
    facet_wrap(~ outcome_label, ncol = 2, scales = "free_y") +
    scale_color_manual(values = c("Hombre" = "#377eb8",
                                   "Mujer"  = "#e41a1c")) +
    labs(
      title    = "Valores esperados por género y perfil reproductivo",
      subtitle = paste0(
        "Estado civil: soltero/a (sin hijos/planes) o casado/a (con hijos/planes)\n",
        "Resto de covariables en el promedio del diseño experimental\n",
        "Modelo: género × plan familiar + parental (SET2b, intercepto aleatorio)"
      ),
      x     = "Perfil reproductivo",
      y     = "Valor esperado (IC 95%)",
      color = "Género del perfil",
      caption = "Basado en emmeans; IC 95% ajustado por estructura anidada"
    ) +
    theme_bw(base_size = 11) +
    theme(
      strip.background = element_rect(fill = "#f0f0f0"),
      strip.text       = element_text(face = "bold", size = 9),
      legend.position  = "bottom",
      axis.text.x      = element_text(size = 9),
      plot.subtitle    = element_text(size = 8, color = "gray40")
    )

  ggsave(file.path(out_fig_pred, "C1_genero_x_perfil_reproductivo.png"),
         fig_c1, width = 12, height = 10, dpi = 150, bg = "white")
  cat("  ✓ C1 guardada\n")
} else {
  cat("  ⚠ Sin predicciones para C1\n")
}


# ============================================================
# FIGURA C2: Solo mujeres
# Cruce de estado parental (0, 1, 2 hijos) × plan familiar
# El corazón del mecanismo FCR
# ============================================================

cat("\n--- Figura C2: Mujeres — parental × plan familiar ---\n")

pred_c2 <- map_dfr(outcomes_indices, function(y) {
  m <- modelos_pred[[y]]
  calcular_valores_esperados(
    m, y,
    specs   = ~ perfil_genero * perfil_plan_futuro * perfil_parental,
    at_vals = list(
      perfil_genero      = "Mujer",
      perfil_parental    = c("actualmente no tiene hijos",
                              "tiene un hijo",
                              "tiene dos hijos"),
      perfil_plan_futuro = c("no quiere", "en 2 años", "en 5 años")
    )
  )
}) %>%
  mutate(
    parental_label = case_when(
      perfil_parental == "actualmente no tiene hijos" ~ "Sin hijos",
      perfil_parental == "tiene un hijo"              ~ "1 hijo",
      perfil_parental == "tiene dos hijos"            ~ "2 hijos"
    ),
    parental_label = factor(parental_label,
                             levels = c("Sin hijos", "1 hijo", "2 hijos")),
    plan_label = case_when(
      perfil_plan_futuro == "no quiere" ~ "Sin planes",
      perfil_plan_futuro == "en 2 años" ~ "Plan: 2 años",
      perfil_plan_futuro == "en 5 años" ~ "Plan: 5 años"
    ),
    plan_label = factor(plan_label,
                         levels = c("Sin planes", "Plan: 2 años", "Plan: 5 años")),
    outcome_label = factor(outcome_label,
                           levels = labels_indices[outcomes_indices])
  )

if (nrow(pred_c2) > 0) {
  fig_c2 <- ggplot(pred_c2,
                   aes(x = parental_label, y = emmean,
                       color = plan_label, group = plan_label)) +
    geom_line(linewidth = 0.8, alpha = 0.8,
              position = position_dodge(width = 0.3)) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                  width = 0.15, linewidth = 0.7,
                  position = position_dodge(width = 0.3)) +
    geom_point(size = 3,
               position = position_dodge(width = 0.3)) +
    facet_wrap(~ outcome_label, ncol = 2, scales = "free_y") +
    scale_color_manual(
      values = c(
        "Sin planes"   = "#4daf4a",
        "Plan: 2 años" = "#ff7f00",
        "Plan: 5 años" = "#e41a1c"
      )
    ) +
    labs(
      title    = "Valores esperados — Solo perfiles MUJER",
      subtitle = paste0(
        "Por estado parental y plan familiar\n",
        "Estado civil: soltero/a | Resto de covariables en el promedio\n",
        "Modelo: género × plan familiar + parental (SET2b, intercepto aleatorio)"
      ),
      x     = "Estado parental",
      y     = "Valor esperado (IC 95%)",
      color = "Plan familiar",
      caption = "Basado en emmeans; IC 95% ajustado por estructura anidada"
    ) +
    theme_bw(base_size = 11) +
    theme(
      strip.background = element_rect(fill = "#f0f0f0"),
      strip.text       = element_text(face = "bold", size = 9),
      legend.position  = "bottom",
      axis.text.x      = element_text(size = 9),
      plot.subtitle    = element_text(size = 8, color = "gray40")
    )

  ggsave(file.path(out_fig_pred, "C2_mujeres_parental_x_plan.png"),
         fig_c2, width = 12, height = 10, dpi = 150, bg = "white")
  cat("  ✓ C2 guardada\n")
} else {
  cat("  ⚠ Sin predicciones para C2\n")
}


# ============================================================
# FIGURA C3: Comparación Hombre vs Mujer
# Para cada combinación parental × plan
# (penalty gap: brecha entre géneros)
# ============================================================

cat("\n--- Figura C3: Brecha de género por combinación parental × plan ---\n")

pred_c3 <- map_dfr(outcomes_indices, function(y) {
  m <- modelos_pred[[y]]
  calcular_valores_esperados(
    m, y,
    specs   = ~ perfil_genero * perfil_plan_futuro * perfil_parental,
    at_vals = list(
      perfil_genero      = c("Hombre", "Mujer"),
      perfil_parental    = c("actualmente no tiene hijos",
                              "tiene un hijo",
                              "tiene dos hijos"),
      perfil_plan_futuro = c("no quiere", "en 2 años", "en 5 años")
    )
  )
})

if (nrow(pred_c3) > 0) {

  # Calcular brecha Mujer - Hombre
  brecha_c3 <- pred_c3 %>%
    select(outcome, outcome_label, perfil_genero,
           perfil_parental, perfil_plan_futuro, emmean, SE) %>%
    pivot_wider(names_from  = perfil_genero,
                values_from = c(emmean, SE)) %>%
    mutate(
      brecha    = emmean_Mujer - emmean_Hombre,
      se_brecha = sqrt(SE_Mujer^2 + SE_Hombre^2),
      lo        = brecha - 1.96 * se_brecha,
      hi        = brecha + 1.96 * se_brecha,
      parental_label = case_when(
        perfil_parental == "actualmente no tiene hijos" ~ "Sin hijos",
        perfil_parental == "tiene un hijo"              ~ "1 hijo",
        perfil_parental == "tiene dos hijos"            ~ "2 hijos"
      ),
      plan_label = case_when(
        perfil_plan_futuro == "no quiere"  ~ "Sin planes",
        perfil_plan_futuro == "en 2 años"  ~ "Plan: 2 años",
        perfil_plan_futuro == "en 5 años"  ~ "Plan: 5 años"
      ),
      parental_label = factor(parental_label,
                               levels = c("Sin hijos", "1 hijo", "2 hijos")),
      plan_label     = factor(plan_label,
                               levels = c("Sin planes",
                                          "Plan: 2 años", "Plan: 5 años")),
      combo          = paste0(parental_label, "\n", plan_label),
      outcome_label  = factor(outcome_label,
                               levels = labels_indices[outcomes_indices]),
      sig_brecha     = case_when(
        lo > 0 | hi < 0 ~ "*",
        TRUE            ~ ""
      )
    )

  fig_c3 <- ggplot(brecha_c3,
                   aes(x = parental_label, y = brecha,
                       color = plan_label, group = plan_label)) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "gray50", linewidth = 0.5) +
    geom_line(linewidth = 0.8, alpha = 0.8,
              position = position_dodge(width = 0.3)) +
    geom_errorbar(aes(ymin = lo, ymax = hi),
                  width = 0.15, linewidth = 0.7,
                  position = position_dodge(width = 0.3)) +
    geom_point(size = 3,
               position = position_dodge(width = 0.3)) +
    geom_text(
      data = brecha_c3 %>% filter(sig_brecha == "*"),
      aes(label = "*", y = hi + 0.05),
      size = 5, color = "black",
      position = position_dodge(width = 0.3)
    ) +
    facet_wrap(~ outcome_label, ncol = 2, scales = "free_y") +
    scale_color_manual(
      values = c(
        "Sin planes"   = "#4daf4a",
        "Plan: 2 años" = "#ff7f00",
        "Plan: 5 años" = "#e41a1c"
      )
    ) +
    labs(
      title    = "Brecha de género (Mujer − Hombre) por perfil reproductivo",
      subtitle = paste0(
        "Positivo = mujeres evaluadas más alto; Negativo = penalización de mujeres\n",
        "* = IC 95% no cruza cero | Estado civil: soltero/a\n",
        "Modelo: género × plan familiar + parental (SET2b, intercepto aleatorio)"
      ),
      x       = "Estado parental",
      y       = "Diferencia Mujer − Hombre (IC 95%)",
      color   = "Plan familiar",
      caption = "Brecha calculada de valores esperados de emmeans"
    ) +
    theme_bw(base_size = 11) +
    theme(
      strip.background = element_rect(fill = "#f0f0f0"),
      strip.text       = element_text(face = "bold", size = 9),
      legend.position  = "bottom",
      axis.text.x      = element_text(size = 9),
      plot.subtitle    = element_text(size = 8, color = "gray40")
    )

  ggsave(file.path(out_fig_pred, "C3_brecha_genero_parental_x_plan.png"),
         fig_c3, width = 12, height = 10, dpi = 150, bg = "white")
  cat("  ✓ C3 guardada\n")
} else {
  cat("  ⚠ Sin predicciones para C3\n")
}


# ============================================================
# FIGURA C4: Heatmap de valores esperados — Mujeres
# Parental (filas) × Plan (columnas), por outcome
# Permite ver la magnitud absoluta de cada combinación
# ============================================================

cat("\n--- Figura C4: Heatmap valores esperados — Mujeres ---\n")

if (exists("pred_c2") && nrow(pred_c2) > 0) {

  fig_c4 <- ggplot(pred_c2 %>%
                     filter(outcome != "ans_p2_5"),  # excluir Ambicioso/a
                   aes(x = plan_label, y = parental_label,
                       fill = emmean)) +
    geom_tile(color = "white", linewidth = 0.8) +
    geom_text(aes(label = round(emmean, 2)),
              size = 3.5, color = "black", fontface = "bold") +
    facet_wrap(~ outcome_label, ncol = 2) +
    scale_fill_gradient2(
      low      = "#d01c8b",
      mid      = "white",
      high     = "#4dac26",
      midpoint = 4,  # punto medio de escala 1-7
      limits   = c(1, 7),
      name     = "Valor\nesperado"
    ) +
    labs(
      title    = "Heatmap de valores esperados — Solo perfiles MUJER",
      subtitle = paste0(
        "Combinaciones de estado parental × plan familiar\n",
        "Estado civil: soltero/a | Escala 1–7"
      ),
      x = "Plan familiar",
      y = "Estado parental",
      caption = "Valores calculados con emmeans; resto de covariables en el promedio"
    ) +
    theme_bw(base_size = 11) +
    theme(
      strip.background = element_rect(fill = "#f0f0f0"),
      strip.text       = element_text(face = "bold", size = 9),
      axis.text.x      = element_text(angle = 15, hjust = 1),
      plot.subtitle    = element_text(size = 8, color = "gray40")
    )

  ggsave(file.path(out_fig_pred, "C4_heatmap_mujeres.png"),
         fig_c4, width = 12, height = 10, dpi = 150, bg = "white")
  cat("  ✓ C4 guardada\n")
}

cat("\n✓ PARTE C completada\n")


# ============================================================
# RESUMEN FINAL
# ============================================================

cat("\n")
cat("============================================================\n")
cat("  PROCESO COMPLETADO\n")
cat("============================================================\n\n")
cat("PARTE A — Modelos RI (16 outcomes individuales):\n")
cat("  Tablas en: tablas/RI_sum_contrasts/\n")
cat("  Figuras en: figuras/RI_sum_contrasts/\n\n")
cat("PARTE B — Modelos RI (índices agregados):\n")
cat("  Tablas en: tablas/RI_indices/\n")
cat("  Figuras en: figuras/RI_indices/\n\n")
cat("PARTE C — Valores esperados:\n")
cat("  C1: Género × perfil reproductivo (H vs M, sin/con riesgo FCR)\n")
cat("  C2: Mujeres — parental × plan familiar\n")
cat("  C3: Brecha de género (Mujer - Hombre) por combinación\n")
cat("  C4: Heatmap valores esperados — solo mujeres\n")
cat("  Figuras en: figuras/valores_esperados/\n\n")
cat("Referencia: Auspurg & Hinz (2017), Cap. 5\n")
cat("  → RI por respondente (L2)\n")
cat("  → No anidado por viñeta (varianza explicada por dimensiones del modelo)\n")
cat("  → Sum contrasts: coeficientes = desviación del promedio del diseño\n")
