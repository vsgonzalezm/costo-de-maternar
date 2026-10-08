# ============================================================================
# 11 - Sensitivity analysis: attention-check full vs. restricted sample
# ============================================================================

library(tidyverse)
library(here)
library(estimatr)
library(broom)

project_dir <- if (dir.exists(here("Encuesta Demanda", "Data"))) {
  here("Encuesta Demanda")
} else {
  here()
}

#Mismo patron que 05_Tables.R / 05b: Outputs/tables/<subcarpeta> y Outputs/figures/<subcarpeta>
tables_dir  <- file.path(project_dir, "Outputs", "tables",  "sensitivity_ac")
figures_dir <- file.path(project_dir, "Outputs", "figures", "sensitivity_ac")
tables_by_set_dir <- file.path(tables_dir, "by_set")
dir.create(tables_dir,        recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir,       recursive = TRUE, showWarnings = FALSE)
dir.create(tables_by_set_dir, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# 1. DATOS
# ==============================================================================

base_long_final <- read_csv(
  file.path(project_dir, "Data", "processed", "base_demanda_analysis_ready.csv"),
  show_col_types = FALSE
)

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

df <- base_long_final %>%
  mutate(
    perfil_genero        = apply_sum_contrasts(factor(perfil_genero, levels = c("Hombre", "Mujer")), ref_level = "Hombre"),
    perfil_estado_civil  = apply_sum_contrasts(factor(perfil_estado_civil, levels = c("soltero/a", "casado/a")), ref_level = "soltero/a"),
    perfil_parental      = apply_sum_contrasts(factor(perfil_parental, levels = c("actualmente no tiene hijos", "tiene un hijo", "tiene dos hijos")), ref_level = "actualmente no tiene hijos"),
    #Sin tilde: normalizar_texto() en 01_... convierte "años" -> "anos"
    perfil_plan_futuro   = apply_sum_contrasts(factor(perfil_plan_futuro, levels = c("no quiere", "en 2 anos", "en 5 anos")), ref_level = "no quiere"),
    perfil_exp           = apply_sum_contrasts(factor(perfil_exp, levels = c("6 años", "7 años", "8 años"))),
    perfil_desempeno     = apply_sum_contrasts(as.factor(perfil_desempeno)),
    pantalla_orden       = apply_sum_contrasts(as.factor(pantalla_orden)),
    template_contexto    = apply_sum_contrasts(as.factor(template_contexto)),
    condicion_ocupacional = apply_sum_contrasts(factor(VISITAS_1_BRAZO, levels = c("profesional", "tecnico")), ref_level = "profesional")
  )

#SANITY CHECK: niveles identicos a 04_...
stopifnot(identical(levels(df$perfil_genero),       c("Mujer", "Hombre")))
stopifnot(identical(levels(df$perfil_estado_civil), c("casado/a", "soltero/a")))
stopifnot(identical(levels(df$perfil_parental),     c("tiene un hijo", "tiene dos hijos", "actualmente no tiene hijos")))
stopifnot(identical(levels(df$perfil_plan_futuro),  c("en 2 anos", "en 5 anos", "no quiere")))
stopifnot(identical(levels(df$condicion_ocupacional), c("tecnico", "profesional")))
cat("Sanity checks de niveles: OK\n")

# ==============================================================================
# 1b. DIAGNOSTICO DEL ATTENTION CHECK
#
# La pregunta P5
# FAILED_ATTENTION_CHECK se construye en 02
# ==============================================================================

cat("\n=== DIAGNOSTICO ATTENTION CHECK ===\n")

resp_level <- df %>% distinct(numericalId, .keep_all = TRUE)

ac_dist <- resp_level %>%
  count(check_atencion_memoria_label, FAILED_ATTENTION_CHECK) %>%
  arrange(FAILED_ATTENTION_CHECK, desc(n)) %>%
  mutate(pct = round(100 * n / sum(n), 1))

cat("\nRespuestas a P5 ('Que informacion NO estaba en los perfiles?'):\n")
print(ac_dist, n = Inf)

n_total  <- nrow(resp_level)
n_passed <- sum(resp_level$FAILED_ATTENTION_CHECK == 0, na.rm = TRUE)
n_failed <- sum(resp_level$FAILED_ATTENTION_CHECK == 1, na.rm = TRUE)

cat(sprintf("\n  Pasaron (respondieron RUT):   %d (%.1f%%)\n", n_passed, 100*n_passed/n_total))
cat(sprintf("  Fallaron (otra respuesta):    %d (%.1f%%)\n", n_failed, 100*n_failed/n_total))

cat("\n--- Perfil de quienes fallaron vs. pasaron ---\n")
#SEXO == 2 es "Femenino" 
ac_profile <- resp_level %>%
  mutate(ac_group = if_else(FAILED_ATTENTION_CHECK == 0, "Paso", "Fallo")) %>%
  group_by(ac_group) %>%
  summarise(
    n                = n(),
    pct_profesional  = round(100 * mean(VISITAS_1_BRAZO == "profesional", na.rm = TRUE), 1),
    pct_mujer        = round(100 * mean(SEXO == 2, na.rm = TRUE), 1),
    edad_media       = round(mean(as.numeric(EDAD), na.rm = TRUE), 1),
    duracion_mediana = round(median(DURATION_MINUTES, na.rm = TRUE), 1),
    pct_duration_lt6 = round(100 * mean(DURATION_LT_6MIN, na.rm = TRUE), 1),
    .groups = "drop"
  )
print(ac_profile, n = Inf)

cat("\n--- Attention check por condicion ocupacional ---\n")
ac_x_cond <- resp_level %>%
  count(VISITAS_1_BRAZO, FAILED_ATTENTION_CHECK) %>%
  group_by(VISITAS_1_BRAZO) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup()
print(ac_x_cond, n = Inf)

diagnostico_ac <- resp_level %>%
  select(numericalId, VISITAS_1_BRAZO, SEXO, EDAD, EDADR,
         check_atencion_memoria, check_atencion_memoria_label,
         FAILED_ATTENTION_CHECK, DURATION_MINUTES, DURATION_LT_6MIN,
         UNDER_P10_TIME_SCREEN_CARGO, UNDER_P10_TIME_SCREEN_PERFIL) %>%
  arrange(FAILED_ATTENTION_CHECK, numericalId)

write_csv(diagnostico_ac, file.path(tables_dir, "attention_check_diagnostic.csv"))
cat("\nDiagnostico exportado: Outputs/tables/sensitivity_ac/attention_check_diagnostic.csv\n")

# ==============================================================================
# 2. DEFINICION DE MUESTRAS
# ==============================================================================

samples <- list(
  full       = df,
  restricted = df %>% filter(FAILED_ATTENTION_CHECK == 0)
)

#Subsets: mismos que 04 (prof / tec para sets split; pooled para el resto)
get_data <- function(sample_df, subset) {
  switch(subset,
    prof   = sample_df %>% filter(VISITAS_1_BRAZO == "profesional"),
    tec    = sample_df %>% filter(VISITAS_1_BRAZO == "tecnico"),
    pooled = sample_df
  )
}

sample_sizes <- expand_grid(sample = names(samples), subset = c("prof", "tec", "pooled")) %>%
  mutate(
    n_respondents = map2_int(sample, subset, ~ n_distinct(get_data(samples[[.x]], .y)$numericalId)),
    n_profiles    = map2_int(sample, subset, ~ nrow(get_data(samples[[.x]], .y)))
  )

cat("\n=== Tamanos de muestra ===\n")
print(sample_sizes, n = Inf)
write_csv(sample_sizes, file.path(tables_dir, "sample_sizes.csv"))

n_full_resp  <- sample_sizes %>% filter(sample == "full",       subset == "pooled") %>% pull(n_respondents)
n_restr_resp <- sample_sizes %>% filter(sample == "restricted", subset == "pooled") %>% pull(n_respondents)

# ==============================================================================
# 3. OUTCOMES, ETIQUETAS Y TERMINOS
# ==============================================================================

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
all_outcomes <- c(outcome_hiring, outcome_evaluation, outcome_behavior)

outcome_groups <- list(
  hiring     = outcome_hiring,
  evaluation = outcome_evaluation,
  behavior   = outcome_behavior
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
  "expected_new_challenges" = "Expected: take on new challenges", "expected_promotion" = "Expected: be promoted"
)

#Diccionario de terminos
term_dictionary <- tribble(
  ~term,                             ~term_label,
  "perfil_generoMujer",              "Gender: Woman",
  "perfil_estado_civilcasado/a",     "Marital status: Married",
  "perfil_parentaltiene un hijo",    "Parental: 1 child",
  "perfil_parentaltiene dos hijos",  "Parental: 2 children",
  "perfil_plan_futuroen 2 anos",     "Family plans: 2 years",
  "perfil_plan_futuroen 5 anos",     "Family plans: 5 years",
  "condicion_ocupacionaltecnico",    "Occupation: Technical"
)

#Etiqueta 
resolve_terms <- function(term_vec) {
  map_dfr(unique(term_vec), function(term) {
    parts <- str_split(term, ":")[[1]]
    labs <- map_chr(parts, function(p) {
      m <- term_dictionary$term_label[term_dictionary$term == p]
      if (length(m) == 1) m else p
    })
    tibble(term = term, term_label = paste(labs, collapse = " × "))
  })
}

#Se excluyen intercepto y controles de diseno/presentacion. Se conservan efectos
#principales, condicion_ocupacional e interacciones (ninguna interaccion incluye controles).
control_regex <- "^\\(Intercept\\)$|^perfil_exp|^perfil_desempeno|^pantalla_orden|^template_contexto"

# ==============================================================================
# 4. SETS DE MODELOS
# ==============================================================================

controls <- "pantalla_orden + template_contexto"

model_sets <- tribble(
  ~set_id, ~split_by_condition, ~formula_rhs,

  "set1", TRUE,
  paste(
    "perfil_genero + perfil_estado_civil + perfil_parental +",
    "perfil_plan_futuro + perfil_exp + perfil_desempeno +", controls
  ),

  "set2a", TRUE, #GENERO x ESTADO PARENTAL
  paste(
    "perfil_genero * perfil_parental +",
    "perfil_estado_civil + perfil_plan_futuro +",
    "perfil_exp + perfil_desempeno +", controls
  ),

  "set2b", TRUE, #GENERO x PLAN FUTURO
  paste(
    "perfil_genero * perfil_plan_futuro +",
    "perfil_estado_civil + perfil_parental +",
    "perfil_exp + perfil_desempeno +", controls
  ),

  "set3", TRUE, #GENERO x ESTADO CIVIL
  paste(
    "perfil_genero * perfil_estado_civil +",
    "perfil_parental + perfil_plan_futuro +",
    "perfil_exp + perfil_desempeno +", controls
  ),

  "set4a", TRUE, #GENERO x ESTADO CIVIL x ESTADO PARENTAL
  paste(
    "perfil_genero * perfil_estado_civil * perfil_parental +",
    "perfil_plan_futuro +",
    "perfil_exp + perfil_desempeno +", controls
  ),

  "set4b", TRUE, #GENERO x ESTADO PARENTAL x PLAN FUTURO
  paste(
    "perfil_genero * perfil_parental * perfil_plan_futuro +",
    "perfil_estado_civil +",
    "perfil_exp + perfil_desempeno +", controls
  ),

  "set5", FALSE, #+ CONDICION OCUPACIONAL
  paste(
    "perfil_genero + perfil_estado_civil + perfil_parental +",
    "perfil_plan_futuro + perfil_exp + perfil_desempeno +",
    "condicion_ocupacional +", controls
  ),

  "set5a", FALSE, #OCUPACION x GENERO
  paste(
    "perfil_genero * condicion_ocupacional +",
    "perfil_estado_civil + perfil_parental + perfil_plan_futuro +",
    "perfil_exp + perfil_desempeno +", controls
  ),

  "set5b", FALSE, #OCUPACION x ESTADO PARENTAL
  paste(
    "perfil_parental * condicion_ocupacional +",
    "perfil_genero + perfil_estado_civil + perfil_plan_futuro +",
    "perfil_exp + perfil_desempeno +", controls
  ),

  "set5c", FALSE, #OCUPACION x PLAN FUTURO
  paste(
    "perfil_plan_futuro * condicion_ocupacional +",
    "perfil_genero + perfil_estado_civil + perfil_parental +",
    "perfil_exp + perfil_desempeno +", controls
  ),

  "set5d", FALSE, #OCUPACION x GENERO x ESTADO PARENTAL
  paste(
    "perfil_genero * perfil_parental * condicion_ocupacional +",
    "perfil_estado_civil + perfil_plan_futuro +",
    "perfil_exp + perfil_desempeno +", controls
  ),

  "set5e", FALSE, #OCUPACION x GENERO x PLAN FUTURO
  paste(
    "perfil_genero * perfil_plan_futuro * condicion_ocupacional +",
    "perfil_estado_civil + perfil_parental +",
    "perfil_exp + perfil_desempeno +", controls
  ),

  "set2a_pooled", FALSE, #GENERO x ESTADO PARENTAL, + CONDICION OCUPACIONAL
  paste(
    "perfil_genero * perfil_parental +",
    "perfil_estado_civil + perfil_plan_futuro +",
    "perfil_exp + perfil_desempeno + condicion_ocupacional +", controls
  ),

  "set2b_pooled", FALSE, #GENERO x PLAN FUTURO, + CONDICION OCUPACIONAL
  paste(
    "perfil_genero * perfil_plan_futuro +",
    "perfil_estado_civil + perfil_parental +",
    "perfil_exp + perfil_desempeno + condicion_ocupacional +", controls
  ),

  "set4a_pooled", FALSE, #GENERO x ESTADO CIVIL x ESTADO PARENTAL, + CONDICION OCUPACIONAL
  paste(
    "perfil_genero * perfil_estado_civil * perfil_parental +",
    "perfil_plan_futuro +",
    "perfil_exp + perfil_desempeno + condicion_ocupacional +", controls
  ),

  "set4b_pooled", FALSE, #GENERO x ESTADO PARENTAL x PLAN FUTURO, + CONDICION OCUPACIONAL
  paste(
    "perfil_genero * perfil_parental * perfil_plan_futuro +",
    "perfil_estado_civil +",
    "perfil_exp + perfil_desempeno + condicion_ocupacional +", controls
  )
)

#Una fila por set x subset: los sets split generan prof y tec; el resto, pooled
runs <- model_sets %>%
  mutate(subset = map(split_by_condition, ~ if (.x) c("prof", "tec") else "pooled")) %>%
  unnest(subset)

cat(sprintf("\n=== %d sets -> %d combinaciones set x subset x 2 muestras x %d outcomes ===\n",
            nrow(model_sets), nrow(runs), length(all_outcomes)))

# ==============================================================================
# 5. CORRER TODOS LOS MODELOS EN AMBAS MUESTRAS
# ==============================================================================

run_models <- function(formula_str, outcomes, data, id_var = "numericalId") {
  map(outcomes, function(y) {
    fml <- as.formula(paste(y, "~", formula_str))
    tryCatch(
      lm_robust(fml, data = data, clusters = data[[id_var]], se_type = "CR2"),
      error = function(e) {
        message(sprintf("  Error on %s: %s", y, e$message))
        NULL
      }
    )
  }) %>% set_names(outcomes)
}

tidy_models <- function(models, set_id, subset, sample_name) {
  map_dfr(names(models), function(y) {
    m <- models[[y]]
    if (is.null(m)) return(NULL)
    broom::tidy(m, conf.int = TRUE) %>%
      filter(!str_detect(term, control_regex)) %>%
      mutate(term_idx = row_number(),
             set_id = set_id, subset = subset, sample = sample_name, outcome = y)
  })
}

n_fit_log <- list()

tidy_all <- pmap_dfr(runs, function(set_id, split_by_condition, formula_rhs, subset) {
  map_dfr(names(samples), function(sample_name) {
    dat <- get_data(samples[[sample_name]], subset)
    cat(sprintf("  %-13s %-7s %-10s (%d perfiles)...\n",
                toupper(set_id), subset, sample_name, nrow(dat)))
    mods <- run_models(formula_rhs, all_outcomes, dat)
    n_ok <- sum(!map_lgl(mods, is.null))
    if (n_ok < length(all_outcomes)) {
      warning(sprintf("%s %s %s: solo %d de %d modelos ajustaron",
                      set_id, subset, sample_name, n_ok, length(all_outcomes)))
    }
    tidy_models(mods, set_id, subset, sample_name)
  })
})

labels_lookup <- resolve_terms(tidy_all$term)

tidy_all <- tidy_all %>%
  left_join(labels_lookup, by = "term") %>%
  mutate(
    outcome_label = outcome_labels[outcome],
    n_way = str_count(term, ":") + 1L,
    term_type = case_when(n_way == 1 ~ "main effect",
                          n_way == 2 ~ "2-way interaction",
                          TRUE       ~ "3-way interaction")
  )

# ==============================================================================
# 6. TABLAS
# ==============================================================================

#--- Tabla larga ---
tabla_larga <- tidy_all %>%
  select(set_id, subset, sample, outcome, outcome_label, term, term_label, term_type,
         estimate, std.error, statistic, p.value, conf.low, conf.high) %>%
  rename(est = estimate, se = std.error, t = statistic,
         p = p.value, ci_lo = conf.low, ci_hi = conf.high) %>%
  arrange(set_id, subset, outcome, term, sample)

write_csv(tabla_larga, file.path(tables_dir, "all_sets_ac_sensitivity_long.csv"))
cat("\nTabla larga guardada: all_sets_ac_sensitivity_long.csv\n")

#--- Tabla ancha: full vs restricted lado a lado ---
tabla_ancha <- tidy_all %>%
  select(set_id, subset, outcome, outcome_label, term, term_label, term_type, term_idx,
         sample, estimate, std.error, p.value, conf.low, conf.high) %>%
  rename(est = estimate, se = std.error, p = p.value,
         ci_lo = conf.low, ci_hi = conf.high) %>%
  pivot_wider(
    names_from  = sample,
    values_from = c(est, se, p, ci_lo, ci_hi)
  ) %>%
  mutate(
    delta_est    = est_restricted - est_full,
    pct_cambio   = round(100 * delta_est / abs(est_full), 1),
    #Cambio expresado en errores estandar de la muestra completa
    delta_en_se  = round(delta_est / se_full, 2),
    sig_full     = p_full       < 0.05,
    sig_restr    = p_restricted < 0.05,
    cambio_signo = sign(est_full) != sign(est_restricted),
    cambio_signi = sig_full != sig_restr,
    #El coef de restricted cae dentro del IC 95% de full? (y viceversa)
    restr_in_ci_full = est_restricted >= ci_lo_full       & est_restricted <= ci_hi_full,
    full_in_ci_restr = est_full       >= ci_lo_restricted & est_full       <= ci_hi_restricted
  ) %>%
  select(set_id, subset, outcome, outcome_label, term, term_label, term_type, term_idx,
         est_full, se_full, p_full, ci_lo_full, ci_hi_full,
         est_restricted, se_restricted, p_restricted, ci_lo_restricted, ci_hi_restricted,
         delta_est, pct_cambio, delta_en_se,
         sig_full, sig_restr, cambio_signo, cambio_signi,
         restr_in_ci_full, full_in_ci_restr) %>%
  arrange(set_id, subset, outcome, term_idx)

write_csv(tabla_ancha %>% select(-term_idx),
          file.path(tables_dir, "all_sets_ac_sensitivity_wide.csv"))
cat("Tabla ancha guardada: all_sets_ac_sensitivity_wide.csv\n")

#Una tabla ancha por set x subset, en by_set/
tabla_ancha %>%
  group_split(set_id, subset) %>%
  walk(function(d) {
    fname <- paste0(d$set_id[1], "_", d$subset[1], "_wide.csv")
    write_csv(d %>% select(-term_idx), file.path(tables_by_set_dir, fname))
  })
cat(sprintf("Tablas por set guardadas en: %s\n", tables_by_set_dir))

# ==============================================================================
# 7. RESUMEN: cuantos coeficientes cambian de conclusion
# ==============================================================================

resumen_por_set <- tabla_ancha %>%
  group_by(set_id, subset) %>%
  summarise(
    n_coefs          = n(),
    n_cambio_signo   = sum(cambio_signo,  na.rm = TRUE),
    n_cambio_signi   = sum(cambio_signi,  na.rm = TRUE),
    n_restr_fuera_ci = sum(!restr_in_ci_full, na.rm = TRUE),
    max_abs_delta_se = round(max(abs(delta_en_se), na.rm = TRUE), 2),
    .groups = "drop"
  )

write_csv(resumen_por_set, file.path(tables_dir, "all_sets_ac_sensitivity_summary.csv"))

cambios <- tabla_ancha %>%
  filter(cambio_signo | cambio_signi) %>%
  select(set_id, subset, outcome_label, term_label,
         est_full, p_full, est_restricted, p_restricted,
         delta_est, delta_en_se, cambio_signo, cambio_signi)

write_csv(cambios, file.path(tables_dir, "all_sets_ac_sensitivity_changes.csv"))

cat("\n=== RESUMEN SENSITIVITY POR SET (full vs. restricted) ===\n")
print(resumen_por_set, n = Inf)

cat(sprintf("\n  Coeficientes comparados en total:    %d\n", nrow(tabla_ancha)))
cat(sprintf("  Cambian de signo:                    %d\n", sum(tabla_ancha$cambio_signo, na.rm = TRUE)))
cat(sprintf("  Cambian de significancia (p<.05):    %d\n", sum(tabla_ancha$cambio_signi, na.rm = TRUE)))
cat(sprintf("  Restricted fuera del IC 95%% de full: %d\n", sum(!tabla_ancha$restr_in_ci_full, na.rm = TRUE)))
cat(sprintf("  Detalle de los que cambian: %s\n", file.path(tables_dir, "all_sets_ac_sensitivity_changes.csv")))

# ==============================================================================
# 8. FIGURAS: IC superpuestos full vs restricted
# ==============================================================================

sample_colors <- c(full = "#333333", restricted = "#E41A1C")
sample_labels <- c(
  full       = sprintf("Full sample (n=%d)", n_full_resp),
  restricted = sprintf("Restricted: no AC fail (n=%d)", n_restr_resp)
)

plot_set_group <- function(d_set, set_id, subset, group_name, outcome_vec) {
  d <- d_set %>% filter(outcome %in% outcome_vec)
  if (nrow(d) == 0) return(invisible(NULL))

  #Orden de terminos segun aparecen en el modelo (de arriba hacia abajo)
  term_levels <- d %>% distinct(term_idx, term_label) %>% arrange(term_idx) %>% pull(term_label) %>% unique()

  d <- d %>%
    mutate(
      term_label    = factor(term_label, levels = rev(term_levels)),
      sample        = factor(sample, levels = c("full", "restricted"), labels = sample_labels),
      outcome_label = factor(outcome_label, levels = outcome_labels[outcome_vec])
    )

  n_out   <- n_distinct(d$outcome)
  n_terms <- length(term_levels)
  n_col   <- min(3, n_out)
  n_row   <- ceiling(n_out / n_col)
  fig_w   <- 4.5 * n_col + 2.5
  fig_h   <- n_row * (0.28 * n_terms + 1.1) + 2.5

  p <- ggplot(d, aes(x = estimate, y = term_label, color = sample, shape = sample)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
    geom_pointrange(
      aes(xmin = conf.low, xmax = conf.high),
      position = position_dodge(width = 0.6),
      size = 0.3, linewidth = 0.5
    ) +
    scale_color_manual(values = sample_colors, labels = sample_labels, name = NULL) +
    scale_shape_manual(values = c(16, 17),     labels = sample_labels, name = NULL) +
    facet_wrap(~ outcome_label, ncol = n_col, scales = "free_x") +
    labs(
      title    = paste0(toupper(set_id), " (", subset, ") - Attention-check sensitivity: ", group_name),
      subtitle = "Sum-contrast OLS | 95% CR2 confidence intervals",
      x        = "Coefficient estimate",
      y        = NULL,
      caption  = paste0(
        "Restricted sample excludes respondents who failed the attention check (n=",
        n_failed, " excluded from ", n_total, ").\n",
        "P5 asked which information was NOT shown in the profiles; correct answer = RUT.\n",
        "Overlap of CIs indicates stable results across sample definitions."
      )
    ) +
    theme_bw(base_size = 10) +
    theme(
      legend.position  = "bottom",
      legend.direction = "horizontal",
      strip.text       = element_text(face = "bold", size = 8),
      panel.grid.minor = element_blank(),
      plot.caption     = element_text(size = 7, color = "grey40"),
      plot.title       = element_text(face = "bold", size = 11)
    )

  set_fig_dir <- file.path(figures_dir, paste0(set_id, "_", subset))
  dir.create(set_fig_dir, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(set_fig_dir, paste0(group_name, ".pdf")),
         plot = p, width = fig_w, height = fig_h, device = "pdf", limitsize = FALSE)
  invisible(p)
}

cat("\n=== Generando figuras ===\n")
tidy_all %>%
  group_split(set_id, subset) %>%
  walk(function(d_set) {
    sid <- d_set$set_id[1]; sub <- d_set$subset[1]
    cat(sprintf("  %s (%s)\n", toupper(sid), sub))
    walk2(names(outcome_groups), outcome_groups,
          ~ plot_set_group(d_set, sid, sub, .x, .y))
  })

cat("\n=== Script 11 completado ===\n")
cat(sprintf("Tablas:  %s\nFiguras: %s\n", tables_dir, figures_dir))