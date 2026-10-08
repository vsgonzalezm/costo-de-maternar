# ============================================================================
# 16 - Regression tables, models SPLIT by occupation
# 1st version: Isi | Modified: Tania Hutt - octubre 2026
# Outputs: outputs/16_tables_split/<set>/16_<SET>_<profesional|tecnico>_<group>.docx
# ============================================================================
proj <- if (dir.exists(here::here("Encuesta Demanda", "data"))) here::here("Encuesta Demanda") else here::here()
source(file.path(proj, "code", "00_setup.R"))
init_output("16_tables_split")
source(file.path(code_dir, "helpers_tables_figures.R"))

cond_names <- c(prof = "profesional", tec = "tecnico")
for (set_id in sets_split$set_id) {
  iwalk(cond_names, function(cond_label, s) {
    models <- load_models(set_id, s)
    iwalk(outcome_groups, function(outcomes_vec, group_name) {
      export_table(models, outcomes_vec, outcome_labels,
                   out_path(set_id, paste0(toupper(set_id), "_", cond_label, "_", group_name, ".docx")))
    })
  })
}
finish_output()
