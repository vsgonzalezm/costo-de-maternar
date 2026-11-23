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
| **Paper (noviembre 2026)** | | |
| 19 | `moderation` | Moderación por características del evaluador (exploratorio; no está en `00_run_all.R`) |
| 20 | `contrasts_corrected` | H1/H2 y H4–H7 desde SET 4b, H16 como diferencia de penalizaciones (SET 4a), "grupo más penalizado" (H3), medias por celda. **Reemplaza** las filas H1, H2, H4–H7 y H16 de `10`; la tabla confirmatoria del paper es `20_confirmatory_contrasts_corrected.csv` |
| 21 | `exploratory_conversion` | Exploratorio (suplementario): asociación entre acomodaciones esperadas y evaluación, por género del postulante |

Otros: `helpers_tables_figures.R` (funciones compartidas 08/09/16/17), `design/` (diseño de viñetas,
previo a la encuesta), `archive/` (versiones anteriores; `2026-10-02_pre_limpieza/` es copia íntegra
de los scripts antes de esta limpieza).

## Base lista para el análisis

`data/processed/base_demanda_analysis_ready.csv` (la escribe `02_labels_timing_quality_flags.R` y la lee
`00_setup.R`; todos los scripts de modelos parten de ella). Es la única base que hay que usar: 2.898 filas
(1.449 encuestados x 2 perfiles). `base_demanda_long_final.csv` y `..._with_labels_and_flags.csv` son pasos intermedios.

Cambios que hice en noviembre 2026 en 01 y 02 al validar la base de Netquest (guardé los scripts anteriores en `archive/2026-11-23_pre_validacion/`):

- Agregué `CARGO_DIRECCION_O_JEFATURA` (02): vale 1 si el cargo de P10 es dueño/a, gerente o director/a, subgerente o jefe/a-supervisor/a, y 0 si es administrativo/a, asistente u otro cargo. Lo hice para poder repetir los modelos solo con quienes cumplen el marco muestral; no elimina casos.
- Reemplacé `NETQUEST_EXTRA_BLOCK_FLAG` por `NETQUEST_EXTRA_SLOT_FLAG` (02). El anterior marcaba a las 68 personas de los 4 bloques con 17 encuestados, aunque la mayoría no era "extra". El nuevo marca solo a las 49 personas que llegaron después a una viñeta (`VISITAS_1`) ya usada por otra.
- Cambié los flags `UNDER_P10_*` a `<=` (02). Los tiempos de Netquest tienen un piso (10 s en CARGO y TAREA, 20 s en el perfil 2), así que con `<` el flag de TAREA no marcaba a nadie.
- Corregí las etiquetas con entidades HTML (`O&apos;Higgins` en `ZONA_label`), que venían así desde la hoja Labels de Netquest.
- Amplié las reglas de `abierta_costoso_simple` y `abierta_inversion_simple` (01) después de revisar una por una las respuestas que caían en "Otro". Arreglé el patrón `car`, que capturaba cargo/carrera/característica como si fueran "caro". En inversión agregué la categoría "Ausencias/familia".
- `UNDER_P10_ALL_3_TASKS` es un flag por perfil, no por persona: 64 filas TRUE (46 personas con algun perfil TRUE), pero los resumenes por encuestado (02, 03, 04) cuentan solo el primer perfil (31). Para filtrar personas usar `max()` por `key`.

Pendiente: la variable `CAMBIO20260729` de Netquest (159 casos del 29-30 jul) no se conserva en la base; falta confirmar con Tania qué cambió ese día.
