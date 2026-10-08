# ============================================================================
# 07 - Reliability-based composite indices and cluster-robust models
# Author: Isi
# Modified: Tania Hutt - arreglo de errores, deduplicacion de sets, consistencia de rutas
# Date: agosto 2026
# Purpose: Builds Cronbach's-alpha-vetted composite indices from the Likert
#          evaluation/behavior items, fits the same cluster-robust model sets
#          as 04_analysis_cluster_robust_models.R using indices in place of
#          their component items, and saves each model list as an .rds file
#          for 08_Tables_indices.R / 09_Figures_indices.R.
# ============================================================================

library(tidyverse)
library(here)
library(psych)
library(estimatr)

project_dir <- if (dir.exists(here("Encuesta Demanda", "Data"))) {
  here("Encuesta Demanda")
} else {
  here()
}

# Misma base ya renombrada/etiquetada que usan 03_.../04_... (no los ans_p* crudos).
base_long_final <- read_csv(
  file.path(project_dir, "Data", "processed", "base_demanda_analysis_ready.csv"),
  show_col_types = FALSE
)

# long_term_commitment_score is reverse-direction relative to the rest of
# "riesgo": higher commitment means LOWER risk, the opposite polarity from
# risky_continuity_score/expected_late_absence/expected_quit_work, where
# higher = more risk. Reverse-scored (1-7 scale) before it enters that
# construct's alpha or index -- check.keys = TRUE in calcular_alpha() below
# only auto-corrects the printed alpha *statistic*, it does not reverse the
# raw column, so the rowMeans() index further down would silently cancel out
# the very risk signal the other three items measure without this.
base_long_final <- base_long_final %>%
  mutate(long_term_commitment_score_rev = 8 - long_term_commitment_score)

#MAPEO DE ITEMS A CONSTRUCTOS
constructos <- list(
  costo       = c("operational_costly_score", "expected_benefits_use", "expected_accommodations"),
  riesgo      = c("risky_continuity_score", "expected_late_absence", "expected_quit_work", "long_term_commitment_score_rev"),
  crecimiento = c("ambitious_score", "promising_score", "expected_new_challenges", "expected_promotion"),
  competencia = c("trustworthy_score", "competent_score"),
  investment  = c("recommendation_probability", "good_investment_score"),
  evaluation  = c("trustworthy_score", "competent_score", "recommendation_probability", "good_investment_score")
)

# salary_costly_score is intentionally in no construct: reverse-coding it
# (tested against good_investment_score/recommendation_probability) doesn't
# fix a direction problem -- it just isn't measuring the same latent thing as
# any of costo/investment/evaluation in this data (alpha stays <= .16-.41
# every way it was tried). It's reported as its own independent item below.

#CALCULAR ALPHA POR CONSTRUCTO
calcular_alpha <- function(datos, items, nombre) {
  a <- psych::alpha(as.data.frame(datos[, items]), check.keys = TRUE)
  raw_alpha <- a$total$raw_alpha
  cat(sprintf("  %-12s alpha = %.3f -> %s\n", nombre, raw_alpha,
              ifelse(raw_alpha >= .70, "SE AGREGA como índice",
                     "se analiza por ítem individual")))
  list(alpha_obj = a, raw_alpha = raw_alpha, agregar = raw_alpha >= .70)
}

cat("Confiabilidad por constructo:\n")
resultados_alpha <- imap(constructos, ~ calcular_alpha(base_long_final, .x, .y))

#EXPORTAR TABLA
tables_dir_reliability <- file.path(project_dir, "Outputs", "tables", "indices")
dir.create(tables_dir_reliability, recursive = TRUE, showWarnings = FALSE)

tabla_alpha <- imap_dfr(resultados_alpha, function(res, nombre) {
  tibble(
    constructo = nombre,
    items      = paste(constructos[[nombre]], collapse = ", "),
    n_items    = length(constructos[[nombre]]),
    raw_alpha  = round(res$raw_alpha, 3),
    decision   = ifelse(res$agregar, "Se agrega como índice", "Se analiza por ítem individual")
  )
})

write_csv(tabla_alpha, file.path(tables_dir_reliability, "alpha_por_constructo.csv"))
cat("  done:", file.path(tables_dir_reliability, "alpha_por_constructo.csv"), "\n")

#CHEQUEO EXPLORATORIO -> alpha de costo (3 items, sin salary) incluyendo salary_costly_score
#y "buena inversión": ninguna combinacion mejora la consistencia del constructo costo.
costo_con_inversion <- c("salary_costly_score", "operational_costly_score", "expected_benefits_use", "expected_accommodations", "good_investment_score")

alpha_exploratorio <- calcular_alpha(base_long_final, costo_con_inversion,
                                      "costo_con_inversion (EXPLORATORIO)")

tabla_alpha_exploratoria <- tibble(
  constructo = "costo_con_inversion",
  items      = paste(costo_con_inversion, collapse = ", "),
  n_items    = length(costo_con_inversion),
  raw_alpha  = round(alpha_exploratorio$raw_alpha, 3),
  decision   = "EXPLORATORIO -- no reemplaza la definición confirmatoria de 'costo' (3 ítems)"
)

write_csv(tabla_alpha_exploratoria, file.path(tables_dir_reliability, "alpha_exploratorio_costo_con_inversion.csv"))
cat("  done:", file.path(tables_dir_reliability, "alpha_exploratorio_costo_con_inversion.csv"), "\n")

#CREACION DE INDICES
# All five constructos below pass the .70 threshold (see the alpha table
# above) except costo (3 items, alpha = .504), which stays disaggregated --
# same rule as before, just re-evaluated for the current item composition.
base_con_indices <- base_long_final %>%
  mutate(
    idx_riesgo      = rowMeans(across(all_of(constructos$riesgo)), na.rm = TRUE),
    idx_crecimiento = rowMeans(across(all_of(constructos$crecimiento)), na.rm = TRUE),
    idx_competencia = rowMeans(across(all_of(constructos$competencia)), na.rm = TRUE),
    idx_investment  = rowMeans(across(all_of(constructos$investment)), na.rm = TRUE),
    idx_evaluation  = rowMeans(across(all_of(constructos$evaluation)), na.rm = TRUE)
  )

#OUTCOMES PARA EL ANALISIS: 5 indices + costo individual (3 items, no paso el
#umbral) + salary_costly_score (independiente, ver nota mas arriba) +
#contratacion.
#
#NOTA: recommendation_probability se mantiene TAMBIEN como outcome individual
#ademas de alimentar idx_investment/idx_evaluation -- a diferencia del resto
#de items que se retiran de la lista individual una vez que pasan a integrar
#un indice, "hiring recommendation" es el DV principal del proyecto (item 5
#del preregistro, el contraste "Anchor" en 05b/05c/08) y se reporta aparte en
#todos los demas scripts, asi que se preserva esa doble presencia aqui a
#proposito. good_investment_score, en cambio, SI se retira como individual
#(solo estaba suelto porque nunca antes paso el umbral de ningun constructo).
outcomes_indices <- c(
  "recommendation_probability",
  "idx_riesgo", "idx_crecimiento", "idx_competencia", "idx_investment", "idx_evaluation",
  constructos$costo,
  "salary_costly_score"
)

outcome_labels_indices <- c(
  "recommendation_probability" = "Hiring recommendation",
  "idx_riesgo"       = "Perceived risk (index)",
  "idx_crecimiento"  = "Growth potential (index)",
  "idx_competencia"  = "Competence (index)",
  "idx_investment"   = "Investment quality (index)",
  "idx_evaluation"   = "Overall evaluation (index)",
  "operational_costly_score" = "Costly (operational)",
  "expected_benefits_use"    = "Expected: use benefits",
  "expected_accommodations"  = "Expected: request accommodations",
  "salary_costly_score"      = "Costly (salary)"
)

outcome_groups_indices <- list(
  hiring  = "recommendation_probability",
  indices = c("idx_riesgo", "idx_crecimiento", "idx_competencia", "idx_investment", "idx_evaluation"),
  costo   = constructos$costo,
  salario = "salary_costly_score"
)

#SUM CONTRASTS
apply_sum_contrasts <- function(fac, ref_level = NULL) {
  if (!is.factor(fac)) fac <- as.factor(fac)
  lvls <- levels(fac)
  if (is.null(ref_level)) ref_level <- lvls[1]
  stopifnot(ref_level %in% lvls)
  lvls_reordered <- c(setdiff(lvls, ref_level), ref_level)
  fac <- factor(fac, levels = lvls_reordered)
  n <- length(lvls_reordered)
  C <- contr.sum(n)
  colnames(C) <- lvls_reordered[-n]
  contrasts(fac) <- C
  fac
}

df_idx <- base_con_indices %>%
  mutate(
    perfil_genero          = apply_sum_contrasts(factor(perfil_genero, levels = c("Hombre", "Mujer")), ref_level = "Hombre"),
    perfil_estado_civil    = apply_sum_contrasts(factor(perfil_estado_civil, levels = c("soltero/a", "casado/a")), ref_level = "soltero/a"),
    perfil_parental        = apply_sum_contrasts(factor(perfil_parental, levels = c("actualmente no tiene hijos", "tiene un hijo", "tiene dos hijos")), ref_level = "actualmente no tiene hijos"),
    # Sin tilde: ver revision de 04_analysis_cluster_robust_models.R (normalizar_texto()
    # en 01_... convierte "años" -> "anos"; con tilde aqui, plan_futuro quedaba NA).
    perfil_plan_futuro     = apply_sum_contrasts(factor(perfil_plan_futuro, levels = c("no quiere", "en 2 anos", "en 5 anos")), ref_level = "no quiere"),
    perfil_exp             = apply_sum_contrasts(factor(perfil_exp, levels = c("6 años", "7 años", "8 años"))),
    perfil_desempeno       = apply_sum_contrasts(as.factor(perfil_desempeno)),
    pantalla_orden         = apply_sum_contrasts(as.factor(pantalla_orden)),
    template_contexto      = apply_sum_contrasts(as.factor(template_contexto)),
    condicion_ocupacional  = apply_sum_contrasts(factor(VISITAS_1_BRAZO, levels = c("profesional", "tecnico")), ref_level = "profesional")
  )

controls <- "pantalla_orden + template_contexto"

run_models <- function(formula_str, outcomes, data, id_var = "numericalId") {
  map(outcomes, function(y) {
    fml <- as.formula(paste(y, "~", formula_str))
    tryCatch(
      lm_robust(fml, data = data, clusters = data[[id_var]], se_type = "CR2"),
      error = function(e) {
        message(sprintf("  Error on %s: %s", y, e$message))
        NULL
      }
    )
  }) %>% set_names(outcomes)
}

# Cuenta modelos que realmente se ajustaron (tryCatch devuelve NULL en error);
# antes se reportaba length(models), que es constante y esconde fallos silenciosos.
n_success <- function(models) sum(!map_lgl(models, is.null))

#SPLIT POR CONDICION OCUPACIONAL (para SET1-SET4b)
df_idx_prof <- df_idx %>% filter(VISITAS_1_BRAZO == "profesional")
df_idx_tec  <- df_idx %>% filter(VISITAS_1_BRAZO == "tecnico")

cat("Profesional (índices): N =", nrow(df_idx_prof), "| Técnico (índices): N =", nrow(df_idx_tec), "\n")

models_dir <- file.path(project_dir, "Outputs", "models")
dir.create(models_dir, recursive = TRUE, showWarnings = FALSE)

# Mismas formulas por set que 04_analysis_cluster_robust_models.R, aplicadas a
# outcomes_indices en vez de all_outcomes.
model_sets <- tribble(
  ~set_id, ~split_by_condition, ~formula_rhs,

  "set1", TRUE,
  paste(
    "perfil_genero + perfil_estado_civil + perfil_parental +",
    "perfil_plan_futuro + perfil_exp + perfil_desempeno +", controls
  ),

  "set2a", TRUE, #GENERO x ESTADO PARENTAL
  paste(
    "perfil_genero * perfil_parental +",
    "perfil_estado_civil + perfil_plan_futuro +",
    "perfil_exp + perfil_desempeno +", controls
  ),

  "set2b", TRUE, #GENERO x PLAN FUTURO
  paste(
    "perfil_genero * perfil_plan_futuro +",
    "perfil_estado_civil + perfil_parental +",
    "perfil_exp + perfil_desempeno +", controls
  ),

  "set3", TRUE, #GENERO x ESTADO CIVIL
  paste(
    "perfil_genero * perfil_estado_civil +",
    "perfil_parental + perfil_plan_futuro +",
    "perfil_exp + perfil_desempeno +", controls
  ),

  "set4a", TRUE, #GENERO x ESTADO CIVIL x ESTADO PARENTAL
  paste(
    "perfil_genero * perfil_estado_civil * perfil_parental +",
    "perfil_plan_futuro +",
    "perfil_exp + perfil_desempeno +", controls
  ),

  "set4b", TRUE, #GENERO x ESTADO PARENTAL x PLAN FUTURO
  paste(
    "perfil_genero * perfil_parental * perfil_plan_futuro +",
    "perfil_estado_civil +",
    "perfil_exp + perfil_desempeno +", controls
  ),

  "set5", FALSE, #+ CONDICION OCUPACIONAL
  paste(
    "perfil_genero + perfil_estado_civil + perfil_parental +",
    "perfil_plan_futuro + perfil_exp + perfil_desempeno +",
    "condicion_ocupacional +", controls
  ),

  "set5a", FALSE, #OCUPACION x GENERO
  paste(
    "perfil_genero * condicion_ocupacional +",
    "perfil_estado_civil + perfil_parental + perfil_plan_futuro +",
    "perfil_exp + perfil_desempeno +", controls
  ),

  "set5b", FALSE, #OCUPACION x ESTADO PARENTAL
  paste(
    "perfil_parental * condicion_ocupacional +",
    "perfil_genero + perfil_estado_civil + perfil_plan_futuro +",
    "perfil_exp + perfil_desempeno +", controls
  ),

  "set5c", FALSE, #OCUPACION x PLAN FUTURO
  paste(
    "perfil_plan_futuro * condicion_ocupacional +",
    "perfil_genero + perfil_estado_civil + perfil_parental +",
    "perfil_exp + perfil_desempeno +", controls
  ),

  "set5d", FALSE, #OCUPACION x GENERO x ESTADO PARENTAL
  paste(
    "perfil_genero * perfil_parental * condicion_ocupacional +",
    "perfil_estado_civil + perfil_plan_futuro +",
    "perfil_exp + perfil_desempeno +", controls
  ),

  "set5e", FALSE, #OCUPACION x GENERO x PLAN FUTURO
  paste(
    "perfil_genero * perfil_plan_futuro * condicion_ocupacional +",
    "perfil_estado_civil + perfil_parental +",
    "perfil_exp + perfil_desempeno +", controls
  ),

  # POOLED-WITH-CONTROL SETS: mirrors 04_analysis_cluster_robust_models.R's
  # set2a_pooled/set2b_pooled/set4a_pooled/set4b_pooled -- used by
  # 05c_marginal_means_pooled.R for the index outcomes.

  "set2a_pooled", FALSE, #GENERO x ESTADO PARENTAL, + CONDICION OCUPACIONAL
  paste(
    "perfil_genero * perfil_parental +",
    "perfil_estado_civil + perfil_plan_futuro +",
    "perfil_exp + perfil_desempeno + condicion_ocupacional +", controls
  ),

  "set2b_pooled", FALSE, #GENERO x PLAN FUTURO, + CONDICION OCUPACIONAL
  paste(
    "perfil_genero * perfil_plan_futuro +",
    "perfil_estado_civil + perfil_parental +",
    "perfil_exp + perfil_desempeno + condicion_ocupacional +", controls
  ),

  "set4a_pooled", FALSE, #GENERO x ESTADO CIVIL x ESTADO PARENTAL, + CONDICION OCUPACIONAL
  paste(
    "perfil_genero * perfil_estado_civil * perfil_parental +",
    "perfil_plan_futuro +",
    "perfil_exp + perfil_desempeno + condicion_ocupacional +", controls
  ),

  "set4b_pooled", FALSE, #GENERO x ESTADO PARENTAL x PLAN FUTURO, + CONDICION OCUPACIONAL
  paste(
    "perfil_genero * perfil_parental * perfil_plan_futuro +",
    "perfil_estado_civil +",
    "perfil_exp + perfil_desempeno + condicion_ocupacional +", controls
  )
)

run_and_save_set <- function(set_id, split_by_condition, formula_rhs) {
  label <- toupper(set_id)

  if (split_by_condition) {
    models_prof <- run_models(formula_rhs, outcomes_indices, df_idx_prof)
    models_tec  <- run_models(formula_rhs, outcomes_indices, df_idx_tec)

    cat(label, "índices (profesional):", n_success(models_prof), "of", length(outcomes_indices), "outcomes\n")
    cat(label, "índices (técnico):    ", n_success(models_tec),  "of", length(outcomes_indices), "outcomes\n")

    saveRDS(models_prof, file.path(models_dir, paste0(set_id, "_idx_models_prof.rds")))
    saveRDS(models_tec,  file.path(models_dir, paste0(set_id, "_idx_models_tec.rds")))
    cat("Saved", label, "índices models (both conditions)\n")
  } else {
    models <- run_models(formula_rhs, outcomes_indices, df_idx)

    cat(label, "índices (pooled):", n_success(models), "of", length(outcomes_indices), "outcomes\n")

    saveRDS(models, file.path(models_dir, paste0(set_id, "_idx_models.rds")))
    cat("Saved", label, "índices models (pooled)\n")
  }
}

pwalk(model_sets, run_and_save_set)
