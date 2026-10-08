# ============================================================================
# 15 - Fit models SPLIT by occupational condition (profesional / tecnico)
# Modified: Tania Hutt - octubre 2026 (reemplaza la parte "split" de
#          04_analysis_cluster_robust_models.R y 07_Indices.R)
# Purpose: Pre-registered SET 1-4b estimated separately within each occupation
#          (VISITAS_1_BRAZO is between-subjects, constant per respondent). All
#          outcomes (16 items + 5 indices). These are NOT the baseline (see 07);
#          they answer "does the effect hold within each occupation?".
# Outputs: outputs/15_fit_models_split/15_<set>_<prof|tec>.rds
# ============================================================================
proj <- if (dir.exists(here::here("Encuesta Demanda", "data"))) here::here("Encuesta Demanda") else here::here()
source(file.path(proj, "code", "00_setup.R"))
init_output("15_fit_models_split")

df <- load_analysis_data()
samples <- list(prof = filter(df, VISITAS_1_BRAZO == "profesional"),
                tec  = filter(df, VISITAS_1_BRAZO == "tecnico"))
cat("Profesional: N profiles =", nrow(samples$prof), "| respondents =", n_distinct(samples$prof$numericalId), "\n")
cat("Tecnico:     N profiles =", nrow(samples$tec),  "| respondents =", n_distinct(samples$tec$numericalId), "\n")

pwalk(sets_split, function(set_id, formula_rhs, ...) {
  iwalk(samples, function(d, s) {
    models <- run_models(formula_rhs, outcomes_models, d)
    cat(sprintf("%-6s %-4s %2d of %d outcomes\n", toupper(set_id), s, n_success(models), length(outcomes_models)))
    saveRDS(models, out_path(paste0(set_id, "_", s, ".rds")))
  })
})

sets_split %>% select(set_id, prereg, label, formula_rhs) %>% write_csv(out_path("model_specifications.csv"))
finish_output()
