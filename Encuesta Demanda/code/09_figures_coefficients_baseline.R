# ============================================================================
# 09 - Coefficient plots and predicted-value plots, BASELINE models (pooled)
# 1st version: Isi | Modified: Tania Hutt - octubre 2026 (fusiona 06_Figures.R y
#          09_Figures_indices.R; lee los .rds de 07_fit_models_baseline.R)
# Outputs: outputs/09_figures_coefficients_baseline/<set>/09_<SET>_pooled_<group>_coefficients.png
#          outputs/09_figures_coefficients_baseline/predicted/09_<SET>_pooled_predicted.png
# ============================================================================
proj <- if (dir.exists(here::here("Encuesta Demanda", "data"))) here::here("Encuesta Demanda") else here::here()
source(file.path(proj, "code", "00_setup.R"))
init_output("09_figures_coefficients_baseline")
source(file.path(code_dir, "helpers_tables_figures.R"))

# A. Coeficientes: interaccion triple -> dos figuras (principales / interacciones)
for (i in seq_len(nrow(sets_baseline))) {
  s <- sets_baseline[i, ]
  tidy_all <- extract_tidy_all(load_models(s$set_id, "pooled"))
  modos <- if (is_triple_set(s$formula_rhs)) c("principales", "interacciones") else "todos"
  iwalk(outcome_groups, function(outcomes_vec, group_name) {
    for (m in modos)
      plot_coefficients(tidy_all, outcomes_vec, outcome_labels, set_title(s$set_id),
                        toupper(s$set_id), "pooled", group_name, s$set_id, modo = m)
  })
}

# B. Valores predichos (escala 1-7) para los sets de interaccion doble
predicted_specs <- tribble(
  ~set_id,         ~x_var,                 ~color_var,
  "set2a_pooled",  "perfil_parental",      "perfil_genero",
  "set2b_pooled",  "perfil_plan_futuro",   "perfil_genero",
  "set3_pooled",   "perfil_estado_civil",  "perfil_genero",
  "set5a",         "perfil_genero",        "condicion_ocupacional",
  "set5b",         "perfil_parental",      "condicion_ocupacional",
  "set5c",         "perfil_plan_futuro",   "condicion_ocupacional"
)
pwalk(predicted_specs, function(set_id, x_var, color_var) {
  plot_predicted_values(load_models(set_id, "pooled"), outcomes_predichos, outcome_labels,
                        x_var, color_var, set_title(set_id), toupper(set_id), "pooled", "predicted")
})
finish_output()
