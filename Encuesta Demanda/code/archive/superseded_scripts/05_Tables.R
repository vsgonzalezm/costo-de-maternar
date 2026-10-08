# ============================================================================
# 05 - Regression model tables
# 1st version: Isi
# Modified: Tania Hutt 
# Date: agosto 2026
# Purpose: Create tables from regression models in 04.
# ============================================================================

library(tidyverse)
library(here)
library(modelsummary)
library(broom)

# Necesario para que broom::tidy() reconozca objetos lm_robust (registra
# tidy.lm_robust via S3); sin este load, tidy() cae a tidy.default() y falla.
library(estimatr)

project_dir <- if (dir.exists(here("Encuesta Demanda", "Data"))) {
  here("Encuesta Demanda")
} else {
  here()
}

models_dir <- file.path(project_dir, "Outputs", "models")
tables_dir <- file.path(project_dir, "Outputs", "tables")

# Outcome labels and groupings (kept here too, since this script should
# be able to run independently of 04). Names match base_demanda_analysis_ready.csv
# / 04_analysis_cluster_robust_models.R (already-renamed outcomes, not raw ans_p*).
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

outcome_groups <- list(
  hiring     = "recommendation_probability",
  evaluation = c(
    "ambitious_score", "trustworthy_score", "competent_score",
    "risky_continuity_score", "salary_costly_score", "operational_costly_score",
    "long_term_commitment_score", "promising_score", "good_investment_score"
  ),
  behavior = c(
    "expected_late_absence", "expected_benefits_use", "expected_accommodations",
    "expected_quit_work", "expected_new_challenges", "expected_promotion"
  )
)

# Converts each lm_robust model into pre-processed tidy/glance tables,
# so modelsummary doesn't run its internal (buggy) lm_robust handler.
to_modelsummary_list <- function(model) {
  out <- list(
    tidy   = broom::tidy(model, conf.int = TRUE),
    glance = broom::glance(model)
  )
  class(out) <- "modelsummary_list"
  out
}

export_table <- function(models_list, outcomes, labels, filename) {
  selected <- models_list[outcomes]
  selected <- selected[!sapply(selected, is.null)]
  if (length(selected) == 0) return(invisible(NULL))
  names(selected) <- labels[names(selected)]

  selected_ms <- lapply(selected, to_modelsummary_list)

  modelsummary(
    selected_ms,
    stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
    fmt = 3,
    notes = c(
      "OLS with cluster-robust standard errors (CR2, clustered by respondent).",
      "Sum contrasts: coefficients reflect deviation from the design mean, not a reference category.",
      "Controls for presentation order and template version."
    ),
    output = filename
  )
  cat("  done:", filename, "\n")
}

#SETS POR CONDICION (SET1-SET4b): 6 sets x 2 condiciones x 3 grupos = 36 tablas
sets_por_condicion <- c("set1", "set2a", "set2b", "set3", "set4a", "set4b")

for (set_name in sets_por_condicion) {

  tables_dir_set <- file.path(tables_dir, paste0(set_name, "_models"))
  dir.create(tables_dir_set, recursive = TRUE, showWarnings = FALSE)

  models_prof <- readRDS(file.path(models_dir, paste0(set_name, "_models_prof.rds")))
  models_tec  <- readRDS(file.path(models_dir, paste0(set_name, "_models_tec.rds")))

  condiciones <- list(profesional = models_prof, tecnico = models_tec)

  iwalk(condiciones, function(models_cond, nombre_cond) {
    iwalk(outcome_groups, function(outcomes_vec, group_name) {
      export_table(
        models_cond, outcomes_vec, outcome_labels,
        file.path(tables_dir_set,
                  paste0(toupper(set_name), "_", nombre_cond, "_", group_name, ".docx"))
      )
    })
  })

  cat("Tablas exportadas para", set_name, "\n")
}

#SETS POOLED (SET5-SET5e): 6 sets x 3 grupos = 18 tablas (sin split por condicion,
#porque condicion_ocupacional ya entra como variable dentro del modelo)
sets_pooled <- c("set5", "set5a", "set5b", "set5c", "set5d", "set5e")

for (set_name in sets_pooled) {

  tables_dir_set <- file.path(tables_dir, paste0(set_name, "_models"))
  dir.create(tables_dir_set, recursive = TRUE, showWarnings = FALSE)

  models_pooled <- readRDS(file.path(models_dir, paste0(set_name, "_models.rds")))

  iwalk(outcome_groups, function(outcomes_vec, group_name) {
    export_table(
      models_pooled, outcomes_vec, outcome_labels,
      file.path(tables_dir_set, paste0(toupper(set_name), "_pooled_", group_name, ".docx"))
    )
  })

  cat("Tablas exportadas para", set_name, "(pooled)\n")
}