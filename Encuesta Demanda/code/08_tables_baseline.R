# ============================================================================
# 08 - Regression tables, BASELINE models (pooled, occupation in the model)
# 1st version: Isi | Modified: Tania Hutt - octubre 2026 (fusiona 05_Tables.R y
#          08_Tables_indices.R; lee los .rds de 07_fit_models_baseline.R)
# Purpose: One Word table per set x outcome group (hiring / evaluation /
#          behavior / indices).
# Outputs: outputs/08_tables_baseline/<set>/08_<SET>_pooled_<group>.docx
# ============================================================================
proj <- if (dir.exists(here::here("Encuesta Demanda", "data"))) here::here("Encuesta Demanda") else here::here()
source(file.path(proj, "code", "00_setup.R"))
init_output("08_tables_baseline")
source(file.path(code_dir, "helpers_tables_figures.R"))

for (set_id in sets_baseline$set_id) {
  models <- load_models(set_id, "pooled")
  iwalk(outcome_groups, function(outcomes_vec, group_name) {
    export_table(models, outcomes_vec, outcome_labels,
                 out_path(set_id, paste0(toupper(set_id), "_pooled_", group_name, ".docx")))
  })
}
finish_output()
