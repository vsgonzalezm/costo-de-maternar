# ============================================================================
# 14 - Sensitivity analysis: full sample vs. RESTRICTED samples (attention check + timing)
# Restricted samples (nested, each removes as few respondents as possible):
#   restr_ac_any         fails the attention check, any option (original criterion)
#   restr_ac_core        fails it through an option other than ac_weak_option ("Ultimo cargo")
#   restr_core_fast_q1   core failure AND fast (total duration or >= 2 screens), Q1 definition
#   restr_core_fast_p10  same, P10 definition
#   (optional) restr_lowq_q1 / restr_lowq_p10: any red flag of 05
# The time-based flags come from 05_response_time_metrics.R (run it first).
# ============================================================================

proj <- if (dir.exists(here::here("Encuesta Demanda", "data"))) here::here("Encuesta Demanda") else here::here()
source(file.path(proj, "code", "00_setup.R"))
library(broom)
init_output("14_robustness_attention_check")

# Datos + indices + factores (niveles verificados en load_analysis_data())
df <- load_analysis_data()


# ==============================================================================
# 2. DEFINICION DE MUESTRAS
# ==============================================================================

#Parameters. Defaults keep the original coverage (all model sets, all 21 outcomes);
#reduce sets_to_run / outcomes_to_run to run faster.
ac_weak_option  <- "Último cargo"   # wrong answer not counted as a clear failure (core failure = any other)
samples_to_run  <- c("restr_ac_any", "restr_ac_core", "restr_core_fast_q1", "restr_core_fast_p10")
sets_to_run     <- model_sets$set_id
outcomes_to_run <- outcomes_models

resp_level <- df %>% distinct(numericalId, .keep_all = TRUE)
n_total <- nrow(resp_level)

#Time-based flags computed in 05_response_time_metrics.R
flags_path <- file.path(outputs_dir, "05_response_time_metrics", "flags",
                        "05_quality_flags_by_respondent.csv")
if (!file.exists(flags_path)) {
  stop("No existe ", flags_path, ". Corre antes 05_response_time_metrics.R con flags_on <- TRUE.")
}
flags_05 <- read_csv(flags_path, show_col_types = FALSE)
flag_cols_needed <- c("fast_total_q1", "fast_screens_q1", "fast_total_p10",
                      "fast_screens_p10", "low_quality_q1", "low_quality_p10")
stopifnot(all(flag_cols_needed %in% names(flags_05)),
          n_distinct(flags_05$numericalId) == nrow(flags_05),
          setequal(flags_05$numericalId, resp_level$numericalId))
stopifnot(ac_weak_option %in% resp_level$check_atencion_memoria_label[resp_level$FAILED_ATTENTION_CHECK == 1])

resp_flags <- resp_level %>%
  select(numericalId, VISITAS_1_BRAZO, FAILED_ATTENTION_CHECK, check_atencion_memoria_label) %>%
  left_join(flags_05 %>% select(numericalId, all_of(flag_cols_needed)), by = "numericalId") %>%
  mutate(ac_any  = FAILED_ATTENTION_CHECK == 1,
         ac_core = ac_any & check_atencion_memoria_label != ac_weak_option)

ids_of <- function(flag) resp_flags$numericalId[flag %in% TRUE]

#Respondents EXCLUDED in each restricted sample (both profile rows leave)
sample_catalog <- list(
  restr_ac_any        = ids_of(resp_flags$ac_any),
  restr_ac_core       = ids_of(resp_flags$ac_core),
  restr_core_fast_q1  = ids_of(resp_flags$ac_core & (resp_flags$fast_total_q1  | resp_flags$fast_screens_q1)),
  restr_core_fast_p10 = ids_of(resp_flags$ac_core & (resp_flags$fast_total_p10 | resp_flags$fast_screens_p10)),
  restr_lowq_q1       = ids_of(resp_flags$low_quality_q1),
  restr_lowq_p10      = ids_of(resp_flags$low_quality_p10)
)
stopifnot(all(samples_to_run %in% names(sample_catalog)))

#Samples are made with filter() on df, as 15 splits by occupation, so contrast coding is unchanged
samples <- c(
  list(full = df),
  setNames(map(samples_to_run, function(s) df %>% filter(!numericalId %in% sample_catalog[[s]])),
           samples_to_run)
)
walk(samples, function(s) {
  stopifnot(identical(contrasts(s$perfil_genero),      contrasts(df$perfil_genero)),
            identical(contrasts(s$perfil_parental),    contrasts(df$perfil_parental)),
            identical(contrasts(s$perfil_plan_futuro), contrasts(df$perfil_plan_futuro)))
})

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
  ) %>%
  group_by(subset) %>%
  mutate(n_excluded_respondents = max(n_respondents[sample == "full"]) - n_respondents) %>%
  ungroup()

cat("\n=== Tamanos de muestra (n_excluded_respondents = respondentes que salen vs. full) ===\n")
print(sample_sizes, n = Inf)
write_csv(sample_sizes, out_path("sample_sizes.csv"))

# ==============================================================================
# 3. OUTCOMES, ETIQUETAS Y TERMINOS
# ==============================================================================

all_outcomes <- outcomes_to_run   # por defecto: 16 items + 5 indices (de 00_setup.R)

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

# model_sets viene de 00_setup.R (baseline pooled + split por ocupacion)

#Una fila por set x subset: los sets split generan prof y tec; el resto, pooled
runs <- model_sets %>%
  filter(set_id %in% sets_to_run) %>%
  mutate(subset = map(scope, ~ if (.x == "split") c("prof", "tec") else "pooled")) %>%
  unnest(subset)

cat(sprintf("\n=== %d sets -> %d combinaciones set x subset x %d muestras x %d outcomes ===\n",
            length(sets_to_run), nrow(runs), length(samples), length(all_outcomes)))

# ==============================================================================
# 5. CORRER TODOS LOS MODELOS EN TODAS LAS MUESTRAS
# ==============================================================================

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

tidy_all <- pmap_dfr(runs, function(set_id, formula_rhs, subset, ...) {
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

write_csv(tabla_larga, out_path("all_sets_ac_sensitivity_long.csv"))
cat("\nTabla larga guardada: all_sets_ac_sensitivity_long.csv\n")

#--- Tabla ancha: full vs cada muestra restringida, lado a lado (una fila por muestra restringida) ---
key_cols <- c("set_id", "subset", "outcome", "term")

full_tab <- tidy_all %>%
  filter(sample == "full") %>%
  select(all_of(key_cols), outcome_label, term_label, term_type, term_idx,
         est_full = estimate, se_full = std.error, p_full = p.value,
         ci_lo_full = conf.low, ci_hi_full = conf.high)

restr_tab <- tidy_all %>%
  filter(sample != "full") %>%
  select(all_of(key_cols), sample,
         est_restr = estimate, se_restr = std.error, p_restr = p.value,
         ci_lo_restr = conf.low, ci_hi_restr = conf.high)

tabla_ancha <- restr_tab %>%
  left_join(full_tab, by = key_cols) %>%
  mutate(
    delta_est    = est_restr - est_full,
    pct_cambio   = round(100 * delta_est / abs(est_full), 1),
    #Cambio expresado en errores estandar de la muestra completa
    delta_en_se  = round(delta_est / se_full, 2),
    #Precision perdida al sacar respondentes (> 1 = intervalo mas ancho)
    se_ratio     = round(se_restr / se_full, 3),
    sig_full     = p_full  < 0.05,
    sig_restr    = p_restr < 0.05,
    cambio_signo = sign(est_full) != sign(est_restr),
    cambio_signi = sig_full != sig_restr,
    #El coef de la muestra restringida cae dentro del IC 95% de full?
    restr_in_ci_full = est_restr >= ci_lo_full & est_restr <= ci_hi_full
  ) %>%
  select(set_id, subset, sample, outcome, outcome_label, term, term_label, term_type, term_idx,
         est_full, se_full, p_full, ci_lo_full, ci_hi_full,
         est_restr, se_restr, p_restr, ci_lo_restr, ci_hi_restr,
         delta_est, pct_cambio, delta_en_se, se_ratio,
         sig_full, sig_restr, cambio_signo, cambio_signi, restr_in_ci_full) %>%
  arrange(set_id, subset, sample, outcome, term_idx)

write_csv(tabla_ancha %>% select(-term_idx),
          out_path("all_sets_ac_sensitivity_wide.csv"))
cat("Tabla ancha guardada: all_sets_ac_sensitivity_wide.csv\n")

#Una tabla ancha por set x subset, en by_set/
tabla_ancha %>%
  group_split(set_id, subset) %>%
  walk(function(d) {
    fname <- paste0(d$set_id[1], "_", d$subset[1], "_wide.csv")
    write_csv(d %>% select(-term_idx), out_path("by_set", fname))
  })


# ==============================================================================
# 7. RESUMEN: cuantos coeficientes cambian de conclusion
# ==============================================================================

resumen_por_set <- tabla_ancha %>%
  group_by(sample, set_id, subset) %>%
  summarise(
    n_coefs          = n(),
    n_cambio_signo   = sum(cambio_signo,  na.rm = TRUE),
    n_cambio_signi   = sum(cambio_signi,  na.rm = TRUE),
    n_restr_fuera_ci = sum(!restr_in_ci_full, na.rm = TRUE),
    median_abs_delta_se = round(median(abs(delta_en_se), na.rm = TRUE), 2),
    max_abs_delta_se    = round(max(abs(delta_en_se), na.rm = TRUE), 2),
    .groups = "drop"
  )

write_csv(resumen_por_set, out_path("all_sets_ac_sensitivity_summary.csv"))

#Resumen por muestra (todos los sets, subsets y outcomes juntos)
resumen_por_muestra <- tabla_ancha %>%
  group_by(sample) %>%
  summarise(
    n_coefs          = n(),
    n_cambio_signo   = sum(cambio_signo, na.rm = TRUE),
    n_cambio_signi   = sum(cambio_signi, na.rm = TRUE),
    n_sig_solo_full  = sum(sig_full & !sig_restr, na.rm = TRUE),
    n_sig_solo_restr = sum(!sig_full & sig_restr, na.rm = TRUE),
    pct_restr_fuera_ci  = round(100 * mean(!restr_in_ci_full, na.rm = TRUE), 1),
    median_abs_delta_se = round(median(abs(delta_en_se), na.rm = TRUE), 2),
    max_abs_delta_se    = round(max(abs(delta_en_se), na.rm = TRUE), 2),
    median_se_ratio     = round(median(se_ratio, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  arrange(match(sample, samples_to_run))

write_csv(resumen_por_muestra, out_path("all_sets_ac_sensitivity_summary_by_sample.csv"))

cambios <- tabla_ancha %>%
  filter(cambio_signo | cambio_signi) %>%
  select(sample, set_id, subset, outcome_label, term_label,
         est_full, p_full, est_restr, p_restr,
         delta_est, delta_en_se, se_ratio, cambio_signo, cambio_signi)

write_csv(cambios, out_path("all_sets_ac_sensitivity_changes.csv"))

cat("\n=== RESUMEN SENSITIVITY POR MUESTRA (full vs. restringida) ===\n")
print(resumen_por_muestra, width = Inf)
cat("\nn_sig_solo_full = significativos en full y no en la restringida (puede reflejar perdida de\n",
    "potencia, ver median_se_ratio, y no un cambio del efecto). Detalle de los que cambian: ",
    out_path("all_sets_ac_sensitivity_changes.csv"), "\n", sep = "")

# ==============================================================================
# 8. FIGURAS: IC superpuestos full vs muestras restringidas
# ==============================================================================

sample_style <- tribble(
  ~sample,               ~short,                              ~color,    ~shape,
  "full",                "Full sample",                       "#333333", 16,
  "restr_ac_any",        "No AC fail (any option)",           "#E41A1C", 17,
  "restr_ac_core",       "No AC fail (core)",                 "#377EB8", 15,
  "restr_core_fast_q1",  "No core AC fail & fast (Q1)",       "#4DAF4A", 18,
  "restr_core_fast_p10", "No core AC fail & fast (P10)",      "#984EA3", 3,
  "restr_lowq_q1",       "No low quality (Q1)",               "#FF7F00", 4,
  "restr_lowq_p10",      "No low quality (P10)",              "#A65628", 8
) %>% filter(sample %in% names(samples)) %>% arrange(match(sample, names(samples)))

plot_set_group <- function(d_set, set_id, subset, group_name, outcome_vec) {
  d <- d_set %>% filter(outcome %in% outcome_vec)
  if (nrow(d) == 0) return(invisible(NULL))

  #Orden de terminos segun aparecen en el modelo (de arriba hacia abajo)
  term_levels <- d %>% distinct(term_idx, term_label) %>% arrange(term_idx) %>% pull(term_label) %>% unique()

  #Etiquetas de leyenda con el n de respondentes de cada muestra en este subset
  ns <- sample_sizes %>% filter(subset == !!subset) %>% select(sample, n_respondents)
  lab_vec <- setNames(paste0(sample_style$short, " (n=", ns$n_respondents[match(sample_style$sample, ns$sample)], ")"),
                      sample_style$sample)

  out_present <- intersect(outcome_vec, unique(d$outcome))
  d <- d %>%
    mutate(
      term_label    = factor(term_label, levels = rev(term_levels)),
      sample        = factor(sample, levels = sample_style$sample),
      outcome_label = factor(outcome_label, levels = outcome_labels[out_present])
    )

  n_out   <- n_distinct(d$outcome)
  n_terms <- length(term_levels)
  n_col   <- min(3, n_out)
  n_row   <- ceiling(n_out / n_col)
  fig_w   <- 4.5 * n_col + 3
  fig_h   <- n_row * (0.4 * n_terms + 1.1) + 3

  p <- ggplot(d, aes(x = estimate, y = term_label, color = sample, shape = sample)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
    geom_pointrange(
      aes(xmin = conf.low, xmax = conf.high),
      position = position_dodge(width = 0.8),
      size = 0.25, linewidth = 0.4
    ) +
    scale_color_manual(values = setNames(sample_style$color, sample_style$sample), labels = lab_vec, name = NULL) +
    scale_shape_manual(values = setNames(sample_style$shape, sample_style$sample), labels = lab_vec, name = NULL) +
    facet_wrap(~ outcome_label, ncol = n_col, scales = "free_x") +
    labs(
      title    = paste0(toupper(set_id), " (", subset, ") - Sensitivity to sample restrictions: ", group_name),
      subtitle = "Sum-contrast OLS | 95% CR2 confidence intervals",
      x        = "Coefficient estimate",
      y        = NULL,
      caption  = paste0(
        "AC = attention check (P5: which information was NOT in the profiles; correct answer = RUT). ",
        "'Core' failure = wrong answer other than '", ac_weak_option, "'.\n",
        "'Fast' = total duration or >= 2 of 4 screens at the bottom of their distribution (Q1 or P10; see 05)."
      )
    ) +
    guides(color = guide_legend(nrow = 2), shape = guide_legend(nrow = 2)) +
    theme_bw(base_size = 10) +
    theme(
      legend.position  = "bottom",
      strip.text       = element_text(face = "bold", size = 8),
      panel.grid.minor = element_blank(),
      plot.caption     = element_text(size = 7, color = "grey40"),
      plot.title       = element_text(face = "bold", size = 11)
    )

  ggsave(out_path("figures", paste0(set_id, "_", subset), paste0(group_name, ".pdf")),
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

finish_output()