# ============================================================================
# 17 - Coefficient plots and predicted-value plots, models SPLIT by occupation
# 1st version: Isi | Modified: Tania Hutt - octubre 2026
# Outputs: outputs/17_figures_coefficients_split/<set>/17_<SET>_<cond>_<group>_coefficients.png
#          outputs/17_figures_coefficients_split/predicted/17_<SET>_<cond>_predicted.png
# ============================================================================
proj <- if (dir.exists(here::here("Encuesta Demanda", "data"))) here::here("Encuesta Demanda") else here::here()
source(file.path(proj, "code", "00_setup.R"))
init_output("17_figures_coefficients_split")
source(file.path(code_dir, "helpers_tables_figures.R"))

cond_names <- c(prof = "profesional", tec = "tecnico")

for (i in seq_len(nrow(sets_split))) {
  s <- sets_split[i, ]
  modos <- if (is_triple_set(s$formula_rhs)) c("principales", "interacciones") else "todos"
  iwalk(cond_names, function(cond_label, cond) {
    tidy_all <- extract_tidy_all(load_models(s$set_id, cond))
    iwalk(outcome_groups, function(outcomes_vec, group_name) {
      for (m in modos)
        plot_coefficients(tidy_all, outcomes_vec, outcome_labels, set_title(s$set_id),
                          toupper(s$set_id), cond_label, group_name, s$set_id, modo = m)
    })
  })
}

predicted_specs <- tribble(
  ~set_id, ~x_var,                 ~color_var,
  "set2a", "perfil_parental",      "perfil_genero",
  "set2b", "perfil_plan_futuro",   "perfil_genero",
  "set3",  "perfil_estado_civil",  "perfil_genero"
)
pwalk(predicted_specs, function(set_id, x_var, color_var) {
  iwalk(cond_names, function(cond_label, cond) {
    plot_predicted_values(load_models(set_id, cond), outcomes_predichos, outcome_labels,
                          x_var, color_var, set_title(set_id), toupper(set_id), cond_label, "predicted")
  })
})
finish_output()
