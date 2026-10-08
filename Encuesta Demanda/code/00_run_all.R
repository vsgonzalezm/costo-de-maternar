# ============================================================================
# 00 - Corre el pipeline completo, en orden (de datos crudos a figuras/tablas)
# Uso: Rscript code/00_run_all.R      (o source() desde RStudio)
# Cada script escribe en outputs/<NN_nombre_del_script>/ con archivos prefijados <NN>_.
# Los scripts 07 y 15 (ajuste de modelos) deben correr antes que los que leen modelos.
# ============================================================================
proj <- if (dir.exists(here::here("Encuesta Demanda", "data"))) here::here("Encuesta Demanda") else here::here()
steps <- c(
  # Datos y control de calidad
  "01_prepare_long_dataset", "02_labels_timing_quality_flags", "03_descriptives",
  "04_timing_checks", "05_response_time_metrics", "06_indices_reliability",
  # BASELINE: muestra pooled, ocupacion como control
  "07_fit_models_baseline", "08_tables_baseline", "09_figures_coefficients_baseline",
  "10_marginal_means_baseline", "11_figures_hypotheses_baseline",
  # Robustez
  "12_robustness_ceiling", "13_robustness_stochastic_dominance", "14_robustness_attention_check",
  # Analisis split por ocupacion
  "15_fit_models_split", "16_tables_split", "17_figures_coefficients_split", "18_marginal_means_split"
)
for (s in steps) {
  cat("\n######## ", s, " ########\n")
  status <- system2("Rscript", shQuote(file.path(proj, "code", paste0(s, ".R"))))
  if (status != 0) stop("Fallo ", s, call. = FALSE)
}
