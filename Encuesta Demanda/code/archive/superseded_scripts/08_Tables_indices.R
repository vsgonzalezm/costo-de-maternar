# ============================================================================
# 08 - Regression model tables (composite-index outcomes)
# Author: valentinagonzalez
# Modified: Tania Hutt - arreglo de error de sintaxis, deps faltantes, consistencia de rutas
# Date: agosto 2026
# Purpose: Create tables from the index-outcome regression models in 07_Indices.R.
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

#OUTCOME LABELS Y GRUPOS PARA INDICES (mismos nombres que 07_Indices.R;
#repetidos aqui para que este script pueda correr de forma independiente)
outcome_labels_indices <- c(
  "recommendation_probability" = "Hiring recommendation",
  "idx_riesgo"      = "Perceived risk (index)",
  "idx_crecimiento" = "Growth potential (index)",
  "idx_competencia" = "Competence (index)",
  "salary_costly_score"      = "Costly (salary)",
  "operational_costly_score" = "Costly (operational)",
  "expected_benefits_use"    = "Expected: use benefits",
  "expected_accommodations"  = "Expected: request accommodations",
  "good_investment_score"    = "Good investment"
)

outcome_groups_indices <- list(
  hiring    = "recommendation_probability",
  indices   = c("idx_riesgo", "idx_crecimiento", "idx_competencia"),
  costo     = c("salary_costly_score", "operational_costly_score", "expected_benefits_use", "expected_accommodations"),
  inversion = "good_investment_score"
)

#MISMAS FUNCIONES DEL SCRIPT 05 (para poder correr este script de forma independiente)
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
      "Controls for presentation order and template version.",
      "idx_riesgo, idx_crecimiento, idx_competencia are unweighted-mean composite indices (alpha >= .70)."
    ),
    output = filename
  )
  cat("  done:", filename, "\n")
}

#SETS POR CONDICION (SET1-SET4b con indices): 6 sets x 2 condiciones x 4 grupos = 48 tablas
sets_por_condicion <- c("set1", "set2a", "set2b", "set3", "set4a", "set4b")

for (set_name in sets_por_condicion) {

  tables_dir_set <- file.path(tables_dir, paste0(set_name, "_idx_models"))
  dir.create(tables_dir_set, recursive = TRUE, showWarnings = FALSE)

  models_prof <- readRDS(file.path(models_dir, paste0(set_name, "_idx_models_prof.rds")))
  models_tec  <- readRDS(file.path(models_dir, paste0(set_name, "_idx_models_tec.rds")))

  condiciones <- list(profesional = models_prof, tecnico = models_tec)

  iwalk(condiciones, function(models_cond, nombre_cond) {
    iwalk(outcome_groups_indices, function(outcomes_vec, group_name) {
      export_table(
        models_cond, outcomes_vec, outcome_labels_indices,
        file.path(tables_dir_set,
                  paste0(toupper(set_name), "_IDX_", nombre_cond, "_", group_name, ".docx"))
      )
    })
  })

  cat("Tablas (índices) exportadas para", set_name, "\n")
}

#SETS POOLED (SET5-SET5e con indices): 6 sets x 4 grupos = 24 tablas
sets_pooled <- c("set5", "set5a", "set5b", "set5c", "set5d", "set5e")

for (set_name in sets_pooled) {

  tables_dir_set <- file.path(tables_dir, paste0(set_name, "_idx_models"))
  dir.create(tables_dir_set, recursive = TRUE, showWarnings = FALSE)

  models_pooled <- readRDS(file.path(models_dir, paste0(set_name, "_idx_models.rds")))

  iwalk(outcome_groups_indices, function(outcomes_vec, group_name) {
    export_table(
      models_pooled, outcomes_vec, outcome_labels_indices,
      file.path(tables_dir_set, paste0(toupper(set_name), "_IDX_pooled_", group_name, ".docx"))
    )
  })

  cat("Tablas (índices) exportadas para", set_name, "(pooled)\n")
}
