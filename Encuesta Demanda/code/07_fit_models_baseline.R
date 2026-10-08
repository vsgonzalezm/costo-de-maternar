# ============================================================================
# 07 - Fit BASELINE models (pooled sample, occupation as control / moderator)
# Modified: Tania Hutt - octubre 2026 (reemplaza 04_analysis_cluster_robust_models.R
#           y la parte de ajuste de 07_Indices.R, restringidos a los sets pooled)
# Purpose: Fits every pooled model set in model_sets (00_setup.R, scope == "pooled")
#          on ALL outcomes (16 items + 5 composite indices), cluster-robust CR2
#          by respondent, sum contrasts. One .rds per set = named list of lm_robust.
#          Downstream (08-11) only READ these files; nothing refits a model.
# Outputs: outputs/07_fit_models_baseline/07_<set>.rds
# ============================================================================
proj <- if (dir.exists(here::here("Encuesta Demanda", "data"))) here::here("Encuesta Demanda") else here::here()
source(file.path(proj, "code", "00_setup.R"))
init_output("07_fit_models_baseline")

df <- load_analysis_data()
cat("N profiles =", nrow(df), "| N respondents =", n_distinct(df$numericalId), "\n")

pwalk(sets_baseline, function(set_id, formula_rhs, ...) {
  models <- run_models(formula_rhs, outcomes_models, df)
  cat(sprintf("%-14s %2d of %d outcomes\n", toupper(set_id), n_success(models), length(outcomes_models)))
  saveRDS(models, out_path(paste0(set_id, ".rds")))
})

# Especificaciones usadas, junto a los modelos (trazabilidad)
sets_baseline %>% select(set_id, prereg, label, formula_rhs) %>% write_csv(out_path("model_specifications.csv"))
finish_output()
