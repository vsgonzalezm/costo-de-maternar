# ============================================================================
# 00 - SETUP COMPARTIDO (se carga con source() al inicio de cada script 01-17)
# Date: octubre 2026
# Purpose: Unica fuente de verdad para:
#   - rutas del proyecto y convencion de nombres de outputs (out_path())
#   - carga de datos + indices compuestos + codificacion de factores (sum contrasts)
#   - outcomes, etiquetas y grupos
#   - TABLA UNICA de modelos (model_sets): baseline (pooled con control de
#     ocupacion) y split por ocupacion
#   - funciones para ajustar / guardar / cargar modelos
# NO se ejecuta sola: cada script la llama con la preamble de 2 lineas
#   proj <- if (dir.exists(here::here("Encuesta Demanda", "data"))) here::here("Encuesta Demanda") else here::here()
#   source(file.path(proj, "code", "00_setup.R"))
# ============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(estimatr)
})

# ----------------------------------------------------------------------------
# 1. RUTAS
# ----------------------------------------------------------------------------
project_dir <- if (dir.exists(here("Encuesta Demanda", "data"))) here("Encuesta Demanda") else here()
code_dir    <- file.path(project_dir, "code")
data_dir    <- file.path(project_dir, "data")
proc_dir    <- file.path(data_dir, "processed")
raw_dir     <- file.path(data_dir, "Raw")
outputs_dir <- file.path(project_dir, "outputs")

analysis_ready_path <- file.path(proc_dir, "base_demanda_analysis_ready.csv")

# ----------------------------------------------------------------------------
# 2. CONVENCION DE OUTPUTS: cada script escribe SOLO en outputs/<script>/ y
#    cada archivo lleva el numero del script como prefijo (08_tabla.docx).
#    Asi, al abrir cualquier archivo se sabe de inmediato que script lo produjo.
# ----------------------------------------------------------------------------
.out <- new.env()

# init_output("08_tables_baseline")  -> outputs/08_tables_baseline/
init_output <- function(script_stem) {
  .out$stem <- script_stem
  .out$num  <- str_extract(script_stem, "^[0-9]+")
  .out$dir  <- file.path(outputs_dir, script_stem)
  dir.create(.out$dir, recursive = TRUE, showWarnings = FALSE)
  cat("Outputs de", script_stem, "->", .out$dir, "\n")
  invisible(.out$dir)
}

# out_path("set2a", "tabla_hiring.docx") -> outputs/08_.../set2a/08_tabla_hiring.docx
# (subcarpetas opcionales; el prefijo numerico se agrega al nombre del archivo)
out_path <- function(...) {
  stopifnot(!is.null(.out$dir))
  parts <- c(...)
  fname <- parts[length(parts)]
  if (!str_detect(fname, paste0("^", .out$num, "_"))) fname <- paste0(.out$num, "_", fname)
  d <- do.call(file.path, as.list(c(.out$dir, parts[-length(parts)])))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  file.path(d, fname)
}

# Al final de cada script: deja un listado de lo producido (script, fecha, archivos)
finish_output <- function() {
  files <- list.files(.out$dir, recursive = TRUE)
  files <- files[!str_detect(basename(files), "MANIFEST")]
  tibble(script = paste0(.out$stem, ".R"), generated = format(Sys.time(), "%Y-%m-%d %H:%M"),
         file = files) %>%
    write_csv(file.path(.out$dir, paste0(.out$num, "_MANIFEST.csv")))
  cat("Listo:", .out$stem, "-", length(files), "archivos\n")
}

# ----------------------------------------------------------------------------
# 3. OUTCOMES, ETIQUETAS Y GRUPOS
# ----------------------------------------------------------------------------
outcome_hiring     <- "recommendation_probability"
outcome_evaluation <- c(
  "ambitious_score", "trustworthy_score", "competent_score",
  "risky_continuity_score", "salary_costly_score", "operational_costly_score",
  "long_term_commitment_score", "promising_score", "good_investment_score"
)
outcome_behavior <- c(
  "expected_late_absence", "expected_benefits_use", "expected_accommodations",
  "expected_quit_work", "expected_new_challenges", "expected_promotion"
)
outcomes_items   <- c(outcome_hiring, outcome_evaluation, outcome_behavior)           # 16
outcomes_indices <- c("idx_riesgo", "idx_crecimiento", "idx_competencia",
                      "idx_investment", "idx_evaluation")                              # 5
outcomes_models  <- c(outcomes_items, outcomes_indices)                                # se ajustan todos

# Los 8 outcomes que muestran las figuras de hipotesis (sin salary_costly_score ni idx_evaluation)
outcomes_headline <- c("recommendation_probability", "operational_costly_score",
                       "expected_benefits_use", "expected_accommodations",
                       "idx_riesgo", "idx_crecimiento", "idx_competencia", "idx_investment")

outcome_groups <- list(
  hiring     = outcome_hiring,
  evaluation = outcome_evaluation,
  behavior   = outcome_behavior,
  indices    = outcomes_indices
)

outcome_labels <- c(
  "recommendation_probability" = "Hiring recommendation",
  "ambitious_score" = "Ambitious", "trustworthy_score" = "Trustworthy",
  "competent_score" = "Competent", "risky_continuity_score" = "Risky (job continuity)",
  "salary_costly_score" = "Costly (salary)", "operational_costly_score" = "Costly (operational)",
  "long_term_commitment_score" = "Committed long-term", "promising_score" = "Promising",
  "good_investment_score" = "Good investment",
  "expected_late_absence" = "Expected: late/absent", "expected_benefits_use" = "Expected: use benefits",
  "expected_accommodations" = "Expected: request accommodations", "expected_quit_work" = "Expected: quit",
  "expected_new_challenges" = "Expected: take on new challenges", "expected_promotion" = "Expected: be promoted",
  "idx_riesgo"      = "Perceived risk (index)",
  "idx_crecimiento" = "Growth potential (index)",
  "idx_competencia" = "Competence (index)",
  "idx_investment"  = "Investment quality (index)",
  "idx_evaluation"  = "Overall evaluation (index)"
)

# Constructos para indices (regla preregistrada: alpha >= .70 -> se agrega;
# costo queda desagregado, alpha = .504). Ver 06_indices_reliability.R, que
# recalcula y documenta el alpha de cada uno.
constructos <- list(
  costo       = c("operational_costly_score", "expected_benefits_use", "expected_accommodations"),
  riesgo      = c("risky_continuity_score", "expected_late_absence", "expected_quit_work", "long_term_commitment_score_rev"),
  crecimiento = c("ambitious_score", "promising_score", "expected_new_challenges", "expected_promotion"),
  competencia = c("trustworthy_score", "competent_score"),
  investment  = c("recommendation_probability", "good_investment_score"),
  evaluation  = c("trustworthy_score", "competent_score", "recommendation_probability", "good_investment_score")
)

# ----------------------------------------------------------------------------
# 4. DATOS: lectura, indices y codificacion de factores
# ----------------------------------------------------------------------------
# long_term_commitment_score tiene polaridad inversa al resto de "riesgo"
# (mayor compromiso = menor riesgo): se invierte (escala 1-7) antes de entrar
# al alpha / indice.
add_indices <- function(d) {
  d %>%
    mutate(
      long_term_commitment_score_rev = 8 - long_term_commitment_score,
      idx_riesgo      = rowMeans(across(all_of(constructos$riesgo)), na.rm = TRUE),
      idx_crecimiento = rowMeans(across(all_of(constructos$crecimiento)), na.rm = TRUE),
      idx_competencia = rowMeans(across(all_of(constructos$competencia)), na.rm = TRUE),
      idx_investment  = rowMeans(across(all_of(constructos$investment)), na.rm = TRUE),
      idx_evaluation  = rowMeans(across(all_of(constructos$evaluation)), na.rm = TRUE)
    )
}

# Sum contrasts: el orden de niveles importa; un relevel silencioso cambia
# todos los numeros aguas abajo sin error.
apply_sum_contrasts <- function(fac, ref_level = NULL) {
  if (!is.factor(fac)) fac <- as.factor(fac)
  lvls <- levels(fac)
  if (is.null(ref_level)) ref_level <- lvls[1]
  stopifnot(ref_level %in% lvls)
  lvls_reordered <- c(setdiff(lvls, ref_level), ref_level)
  fac <- factor(fac, levels = lvls_reordered)
  n <- length(lvls_reordered)
  C <- contr.sum(n)
  colnames(C) <- lvls_reordered[-n]
  contrasts(fac) <- C
  fac
}

# perfil_plan_futuro pasa por normalizar_texto() en 01_..., que convierte
# "años" -> "anos"; con tilde aqui, "en 2/5 años" quedaba NA.
code_factors <- function(d) {
  d %>%
    mutate(
      perfil_genero         = apply_sum_contrasts(factor(perfil_genero, levels = c("Hombre", "Mujer")), ref_level = "Hombre"),
      perfil_estado_civil   = apply_sum_contrasts(factor(perfil_estado_civil, levels = c("soltero/a", "casado/a")), ref_level = "soltero/a"),
      perfil_parental       = apply_sum_contrasts(factor(perfil_parental, levels = c("actualmente no tiene hijos", "tiene un hijo", "tiene dos hijos")), ref_level = "actualmente no tiene hijos"),
      perfil_plan_futuro    = apply_sum_contrasts(factor(perfil_plan_futuro, levels = c("no quiere", "en 2 anos", "en 5 anos")), ref_level = "no quiere"),
      perfil_exp            = apply_sum_contrasts(factor(perfil_exp, levels = c("6 años", "7 años", "8 años"))),
      perfil_desempeno      = apply_sum_contrasts(as.factor(perfil_desempeno)),
      pantalla_orden        = apply_sum_contrasts(as.factor(pantalla_orden)),
      template_contexto     = apply_sum_contrasts(as.factor(template_contexto)),
      condicion_ocupacional = apply_sum_contrasts(factor(VISITAS_1_BRAZO, levels = c("profesional", "tecnico")), ref_level = "profesional")
    )
}

# Base de analisis: datos + indices + factores codificados (muestra completa).
load_analysis_data <- function(path = analysis_ready_path) {
  d <- read_csv(path, show_col_types = FALSE) %>% add_indices() %>% code_factors()
  stopifnot(
    identical(levels(d$perfil_plan_futuro), c("en 2 anos", "en 5 anos", "no quiere")),
    identical(levels(d$perfil_genero), c("Mujer", "Hombre")),
    !any(is.na(d$perfil_plan_futuro))
  )
  d
}

# ----------------------------------------------------------------------------
# 5. MODELOS: tabla unica de especificaciones
#    scope = "pooled": muestra completa, condicion_ocupacional DENTRO del modelo
#            -> BASELINE (scripts 07-11)
#    scope = "split" : un modelo por condicion ocupacional (profesional /
#            tecnico) -> analisis por ocupacion (scripts 14-17)
#    prereg = set del preregistro AsPredicted #297542 que replica
# ----------------------------------------------------------------------------
controls <- "pantalla_orden + template_contexto"
occ      <- "condicion_ocupacional"

model_sets <- tribble(
  ~set_id,         ~scope,   ~prereg, ~label,                                              ~formula_rhs,

  # --- BASELINE: pooled, ocupacion como control aditivo -------------------------
  "set5",          "pooled", "SET 5",  "Main effects + occupation",
  paste("perfil_genero + perfil_estado_civil + perfil_parental + perfil_plan_futuro +",
        "perfil_exp + perfil_desempeno +", occ, "+", controls),

  "set2a_pooled",  "pooled", "SET 2a", "Gender x Parental status (+ occupation)",
  paste("perfil_genero * perfil_parental + perfil_estado_civil + perfil_plan_futuro +",
        "perfil_exp + perfil_desempeno +", occ, "+", controls),

  "set2b_pooled",  "pooled", "SET 2b", "Gender x Family plans (+ occupation)",
  paste("perfil_genero * perfil_plan_futuro + perfil_estado_civil + perfil_parental +",
        "perfil_exp + perfil_desempeno +", occ, "+", controls),

  "set3_pooled",   "pooled", "SET 3",  "Gender x Marital status (+ occupation)",
  paste("perfil_genero * perfil_estado_civil + perfil_parental + perfil_plan_futuro +",
        "perfil_exp + perfil_desempeno +", occ, "+", controls),

  "set4a_pooled",  "pooled", "SET 4a", "Gender x Marital x Parental (+ occupation)",
  paste("perfil_genero * perfil_estado_civil * perfil_parental + perfil_plan_futuro +",
        "perfil_exp + perfil_desempeno +", occ, "+", controls),

  "set4b_pooled",  "pooled", "SET 4b", "Gender x Parental x Family plans (+ occupation)",
  paste("perfil_genero * perfil_parental * perfil_plan_futuro + perfil_estado_civil +",
        "perfil_exp + perfil_desempeno +", occ, "+", controls),

  "set5a",         "pooled", "SET 5a", "Occupation x Gender",
  paste("perfil_genero *", occ, "+ perfil_estado_civil + perfil_parental + perfil_plan_futuro +",
        "perfil_exp + perfil_desempeno +", controls),

  "set5b",         "pooled", "SET 5b", "Occupation x Parental status",
  paste("perfil_parental *", occ, "+ perfil_genero + perfil_estado_civil + perfil_plan_futuro +",
        "perfil_exp + perfil_desempeno +", controls),

  "set5c",         "pooled", "SET 5c", "Occupation x Family plans",
  paste("perfil_plan_futuro *", occ, "+ perfil_genero + perfil_estado_civil + perfil_parental +",
        "perfil_exp + perfil_desempeno +", controls),

  "set5d",         "pooled", "SET 5d", "Occupation x Gender x Parental status",
  paste("perfil_genero * perfil_parental *", occ, "+ perfil_estado_civil + perfil_plan_futuro +",
        "perfil_exp + perfil_desempeno +", controls),

  "set5e",         "pooled", "SET 5e", "Occupation x Gender x Family plans",
  paste("perfil_genero * perfil_plan_futuro *", occ, "+ perfil_estado_civil + perfil_parental +",
        "perfil_exp + perfil_desempeno +", controls),

  # --- SPLIT POR OCUPACION: sin variable de ocupacion, un ajuste por condicion --
  "set1",          "split",  "SET 1",  "Main effects",
  paste("perfil_genero + perfil_estado_civil + perfil_parental + perfil_plan_futuro +",
        "perfil_exp + perfil_desempeno +", controls),

  "set2a",         "split",  "SET 2a", "Gender x Parental status",
  paste("perfil_genero * perfil_parental + perfil_estado_civil + perfil_plan_futuro +",
        "perfil_exp + perfil_desempeno +", controls),

  "set2b",         "split",  "SET 2b", "Gender x Family plans",
  paste("perfil_genero * perfil_plan_futuro + perfil_estado_civil + perfil_parental +",
        "perfil_exp + perfil_desempeno +", controls),

  "set3",          "split",  "SET 3",  "Gender x Marital status",
  paste("perfil_genero * perfil_estado_civil + perfil_parental + perfil_plan_futuro +",
        "perfil_exp + perfil_desempeno +", controls),

  "set4a",         "split",  "SET 4a", "Gender x Marital x Parental",
  paste("perfil_genero * perfil_estado_civil * perfil_parental + perfil_plan_futuro +",
        "perfil_exp + perfil_desempeno +", controls),

  "set4b",         "split",  "SET 4b", "Gender x Parental x Family plans",
  paste("perfil_genero * perfil_parental * perfil_plan_futuro + perfil_estado_civil +",
        "perfil_exp + perfil_desempeno +", controls)
)

sets_baseline <- model_sets %>% filter(scope == "pooled")
sets_split    <- model_sets %>% filter(scope == "split")

# CR2 clusterizado por respondente. Con nd = 2 perfiles por encuestado no se
# anida por perfil/bloque (diseno D-eficiente ya incluye los atributos como
# efectos fijos; anidar daria el "bouncing beta problem").
run_models <- function(formula_str, outcomes, data, id_var = "numericalId") {
  map(outcomes, function(y) {
    tryCatch(
      lm_robust(as.formula(paste(y, "~", formula_str)), data = data,
                clusters = data[[id_var]], se_type = "CR2"),
      error = function(e) { message(sprintf("  Error on %s: %s", y, e$message)); NULL }
    )
  }) %>% set_names(outcomes)
}
n_success <- function(models) sum(!map_lgl(models, is.null))

# ----------------------------------------------------------------------------
# 6. DONDE SE GUARDAN / DE DONDE SE LEEN LOS MODELOS
#    baseline -> outputs/07_fit_models_baseline/07_<set>.rds
#    split    -> outputs/15_fit_models_split/14_<set>_<prof|tec>.rds
#    Cada .rds es una lista nombrada con TODOS los outcomes (items + indices).
# ----------------------------------------------------------------------------
models_stem <- c(pooled = "07_fit_models_baseline", split = "15_fit_models_split")

model_file <- function(set_id, sample = c("pooled", "prof", "tec")) {
  sample <- match.arg(sample)
  scope  <- if (sample == "pooled") "pooled" else "split"
  num    <- str_extract(models_stem[[scope]], "^[0-9]+")
  suffix <- if (sample == "pooled") "" else paste0("_", sample)
  file.path(outputs_dir, models_stem[[scope]], paste0(num, "_", set_id, suffix, ".rds"))
}

load_models <- function(set_id, sample = c("pooled", "prof", "tec")) {
  sample <- match.arg(sample)
  f <- model_file(set_id, sample)
  if (!file.exists(f)) stop("No existe ", f, ". Corre antes el script de ajuste de modelos (07 o 14).")
  readRDS(f)
}
