# code/ — pipeline de análisis (Encuesta Demanda)

Corre en orden con `Rscript code/00_run_all.R`. Cada script escribe **solo** en
`outputs/<NN_nombre_del_script>/` y cada archivo lleva el prefijo `<NN>_`; cada carpeta
tiene un `<NN>_MANIFEST.csv`. Así, cualquier tabla/figura se rastrea a su script por el nombre.

| # | Script | Qué hace |
|---|---|---|
| 00 | `00_setup.R` | Rutas, datos, índices, factores (sum contrasts), outcomes, **`model_sets`** (tabla única de modelos), `out_path()` |
| 01–02 | `prepare_long_dataset`, `labels_timing_quality_flags` | Excel Netquest → base larga → base de análisis (`data/processed/`) |
| 03–05 | `descriptives`, `timing_checks`, `response_time_metrics` | Descriptivos y calidad de respuesta |
| 06 | `indices_reliability` | Alfa de Cronbach (regla preregistrada ≥ .70) |
| **BASELINE (pooled, ocupación en el modelo)** | | |
| 07 | `fit_models_baseline` | Ajusta los 11 sets pooled × 21 outcomes → `.rds` (único lugar donde se ajustan) |
| 08–09 | `tables_baseline`, `figures_coefficients_baseline` | Tablas Word; coeficientes y valores predichos |
| 10 | `marginal_means_baseline` | Anchor, H1–H16, contrastes de figuras, test de pooling |
| 11 | `figures_hypotheses_baseline` | Figuras por comparación y por outcome (para la charla) |
| **Robustez** | | |
| 12–14 | `robustness_ceiling`, `…stochastic_dominance`, `…attention_check` | Techo/piso, dominancia, sensibilidad al attention check |
| **Split por ocupación (profesional / técnico)** | | |
| 15–18 | `fit_models_split`, `tables_split`, `figures_coefficients_split`, `marginal_means_split` | SET 1–4b por condición |

Otros: `helpers_tables_figures.R` (funciones compartidas 08/09/16/17), `design/` (diseño de viñetas,
previo a la encuesta), `archive/` (versiones anteriores; `2026-10-02_pre_limpieza/` es copia íntegra
de los scripts antes de esta limpieza).
