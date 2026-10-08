# ============================================================================
# 04 - Cluster-robust regression models
# 1st version: Isi
# Modified: Tania Hutt - arreglo de errores, deduplicacion de sets, consistencia de rutas
# Date: agosto 2026
# Purpose: Fits cluster-robust OLS models (sum contrasts) for every outcome x
#          model-set x occupational-condition combination and saves each
#          model list as an .rds file for 05_Tables.R to turn into tables.
# ============================================================================

library(tidyverse)
library(here)
library(estimatr)

project_dir <- if (dir.exists(here("Encuesta Demanda", "Data"))) {
  here("Encuesta Demanda")
} else {
  here()
}

# Se usa la base ya renombrada/etiquetada de 02_... (mismos nombres de outcome
# que 03_univariate_descriptive_checks.R), en vez de recrear ese mapeo aqui.
base_long_final <- read_csv(
  file.path(project_dir, "Data", "processed", "base_demanda_analysis_ready.csv"),
  show_col_types = FALSE
)

outcome_hiring     <- "recommendation_probability"
outcome_evaluation <- c(
  "ambitious_score", "trustworthy_score", "competent_score",
  "risky_continuity_score", "salary_costly_score", "operational_costly_score",
  "long_term_commitment_score", "promising_score", "good_investment_score"
)
outcome_behavior <- c(
  "expected_late_absence", "expected_benefits_use", "expected_accommodations",
  "expected_quit_work", "expected_new_challenges", "expected_promotion"
)
all_outcomes <- c(outcome_hiring, outcome_evaluation, outcome_behavior)

outcome_groups <- list(
  hiring     = outcome_hiring,
  evaluation = outcome_evaluation,
  behavior   = outcome_behavior
)

# Etiquetas para tablas (se mantienen tambien en 05_Tables.R para que ese
# script pueda correr de forma independiente).
outcome_labels <- c(
  "recommendation_probability" = "Hiring recommendation",
  "ambitious_score" = "Ambitious", "trustworthy_score" = "Trustworthy",
  "competent_score" = "Competent", "risky_continuity_score" = "Risky (job continuity)",
  "salary_costly_score" = "Costly (salary)", "operational_costly_score" = "Costly (operational)",
  "long_term_commitment_score" = "Committed long-term", "promising_score" = "Promising",
  "good_investment_score" = "Good investment",
  "expected_late_absence" = "Expected: late/absent", "expected_benefits_use" = "Expected: use benefits",
  "expected_accommodations" = "Expected: request accommodations", "expected_quit_work" = "Expected: quit",
  "expected_new_challenges" = "Expected: take on new challenges", "expected_promotion" = "Expected: be promoted"
)

#DECISIÓN DE ANIDAMIENTO
# El diseño D-eficiente ya incluye cada atributo manipulado como un efecto fijo; Una vez
# controlada, no queda ninguna varianza sistemática residual que "pertenezca" a un perfil/bloque
# específico para anidar.
#Con nd = 2 perfiles por encuestado, se podría dar el  "bouncing beta problem":el anidamiento por cualquier variable que no sea el encuestado,
# con tan pocas observaciones por celda, es estadísticamente inestable.

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

df <- base_long_final %>%
  mutate(
    perfil_genero         = apply_sum_contrasts(factor(perfil_genero, levels = c("Hombre", "Mujer")), ref_level = "Hombre"),
    perfil_estado_civil    = apply_sum_contrasts(factor(perfil_estado_civil, levels = c("soltero/a", "casado/a")), ref_level = "soltero/a"),
    perfil_parental        = apply_sum_contrasts(factor(perfil_parental, levels = c("actualmente no tiene hijos", "tiene un hijo", "tiene dos hijos")), ref_level = "actualmente no tiene hijos"),
    # Sin tilde: perfil_plan_futuro pasa por normalizar_texto() en 01_..., que convierte
    # "años" -> "anos". Con tilde aqui, "en 2/5 años" no calzaba con ningun nivel y quedaba
    # NA para el 66% de las filas, tumbando practicamente todos los modelos (ver revision).
    perfil_plan_futuro     = apply_sum_contrasts(factor(perfil_plan_futuro, levels = c("no quiere", "en 2 anos", "en 5 anos")), ref_level = "no quiere"),
    perfil_exp             = apply_sum_contrasts(factor(perfil_exp, levels = c("6 años", "7 años", "8 años"))),
    perfil_desempeno       = apply_sum_contrasts(as.factor(perfil_desempeno)),
    pantalla_orden         = apply_sum_contrasts(as.factor(pantalla_orden)),
    template_contexto      = apply_sum_contrasts(as.factor(template_contexto)),
    condicion_ocupacional  = apply_sum_contrasts(factor(VISITAS_1_BRAZO, levels = c("profesional", "tecnico")), ref_level = "profesional")
  )

#CONTROLS COMUNES A TODOS LOS SETS
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

#SPLIT POR CONDICION OCUPACIONAL (entre sujetos -- confirmado: VISITAS_1_BRAZO
#es constante por respondente y coincide 1:1 con perfil_ocupacion)
df_prof <- df %>% filter(VISITAS_1_BRAZO == "profesional")
df_tec  <- df %>% filter(VISITAS_1_BRAZO == "tecnico")

cat("Profesional: N profiles =", nrow(df_prof), "| N respondents =", n_distinct(df_prof$numericalId), "\n")
cat("Técnico:     N profiles =", nrow(df_tec),  "| N respondents =", n_distinct(df_tec$numericalId), "\n")

models_dir <- file.path(project_dir, "Outputs", "models")
dir.create(models_dir, recursive = TRUE, showWarnings = FALSE)

# Cada fila define un set de modelos: formula (lado derecho) y si se corre por
# condicion ocupacional (profesional/tecnico, split_by_condition = TRUE) o pooled,
# con condicion_ocupacional como variable dentro del modelo (split_by_condition = FALSE).
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

  # POOLED-WITH-CONTROL SETS: same interaction structure as set2a/set2b/set4a/set4b,
  # but pooled across condicion_ocupacional with it entered as an additive control
  # (not interacted) -- used by 05c_marginal_means_pooled.R for H1-H3/H16 (and their
  # gender-DiD companions H4-H9) on the pooled sample. set1's pooled-with-control
  # analog is set5 above (no extra set needed there).

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
    models_prof <- run_models(formula_rhs, all_outcomes, df_prof)
    models_tec  <- run_models(formula_rhs, all_outcomes, df_tec)

    cat(label, "(profesional):", n_success(models_prof), "of", length(all_outcomes), "outcomes\n")
    cat(label, "(técnico):    ", n_success(models_tec),  "of", length(all_outcomes), "outcomes\n")

    saveRDS(models_prof, file.path(models_dir, paste0(set_id, "_models_prof.rds")))
    saveRDS(models_tec,  file.path(models_dir, paste0(set_id, "_models_tec.rds")))
    cat("Saved", label, "models (both conditions)\n")
  } else {
    models <- run_models(formula_rhs, all_outcomes, df)

    cat(label, "(pooled):", n_success(models), "of", length(all_outcomes), "outcomes\n")

    saveRDS(models, file.path(models_dir, paste0(set_id, "_models.rds")))
    cat("Saved", label, "models (pooled)\n")
  }
}

pwalk(model_sets, run_and_save_set)
