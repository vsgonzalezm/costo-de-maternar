# ============================================================================
# 09 - CONFERENCE: exploratory figure set (full outcome panels, all variants)
# ----------------------------------------------------------------------------
# Produces the FULL exploratory set of figures for contrast families A-E
# (women-only / men-only / combined for each -> 15 figures) plus one
# dissociation figure (F), so every candidate figure for the talk can be
# reviewed before selecting. Nothing here is pre-selected.
#
# Factor coding (apply_sum_contrasts(), the df <- base_long_final %>%
# mutate(...) block) is copied VERBATIM from 04_analysis_cluster_robust_
# models.R -- sum contrasts are order-sensitive and a silent relevel breaks
# every downstream number without an error. Palette/theme are copied
# verbatim from 08_conference_models_and_figures.R.
#
# Outputs: Outputs/Tables/conference_figures/
# ============================================================================

library(tidyverse)
library(here)
library(estimatr)
library(marginaleffects)
library(patchwork)
library(psych)

project_dir <- if (dir.exists(here("Encuesta Demanda", "Data"))) here("Encuesta Demanda") else here()
out_dir <- file.path(project_dir, "Outputs", "Tables", "conference_figures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------
# Palette / theme -- verbatim from 08_conference_models_and_figures.R
# ---------------------------------------------------------------------------

PAL <- list(
  teal   = "#1f5f63",   # men / primary
  accent = "#b4552d",   # women / estimates that survive correction
  ink    = "#1c2b33",
  soft   = "#4a5c66",
  rule   = "#d8dee1",
  band   = "#e6efee"
)

theme_conf <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(colour = PAL$rule, linewidth = 0.3),
      panel.grid.major.x = element_line(colour = PAL$rule, linewidth = 0.3),
      axis.title         = element_text(colour = PAL$soft, size = base_size - 1),
      axis.text          = element_text(colour = PAL$ink),
      strip.text         = element_text(colour = PAL$teal, face = "bold", hjust = 0),
      plot.title         = element_text(colour = PAL$teal, face = "bold"),
      plot.subtitle      = element_text(colour = PAL$soft, size = base_size - 1),
      plot.caption       = element_text(colour = PAL$soft, size = base_size - 3, hjust = 0),
      legend.position    = "top",
      legend.title       = element_blank()
    )
}

# ===========================================================================
# 1. DATA -- factor coding copied verbatim from 04_analysis_cluster_robust_models.R
# ===========================================================================

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
    perfil_genero         = apply_sum_contrasts(factor(perfil_genero, levels = c("Hombre", "Mujer")), ref_level = "Hombre"),
    perfil_estado_civil    = apply_sum_contrasts(factor(perfil_estado_civil, levels = c("soltero/a", "casado/a")), ref_level = "soltero/a"),
    perfil_parental        = apply_sum_contrasts(factor(perfil_parental, levels = c("actualmente no tiene hijos", "tiene un hijo", "tiene dos hijos")), ref_level = "actualmente no tiene hijos"),
    perfil_plan_futuro     = apply_sum_contrasts(factor(perfil_plan_futuro, levels = c("no quiere", "en 2 anos", "en 5 anos")), ref_level = "no quiere"),
    perfil_exp             = apply_sum_contrasts(factor(perfil_exp, levels = c("6 años", "7 años", "8 años"))),
    perfil_desempeno       = apply_sum_contrasts(as.factor(perfil_desempeno)),
    pantalla_orden         = apply_sum_contrasts(as.factor(pantalla_orden)),
    template_contexto      = apply_sum_contrasts(as.factor(template_contexto)),
    condicion_ocupacional  = apply_sum_contrasts(factor(VISITAS_1_BRAZO, levels = c("profesional", "tecnico")), ref_level = "profesional")
  )

stopifnot(identical(levels(df$perfil_plan_futuro), c("en 2 anos", "en 5 anos", "no quiere")))
stopifnot(identical(levels(df$perfil_genero), c("Mujer", "Hombre")))
message("Factor-coding checks passed.")

# ===========================================================================
# 1b. OUTCOMES -- nine, built after the verbatim block above
# ===========================================================================
# Reverse-coding: long_term_commitment_score is favourable (higher = more
# committed = LESS risky), while the other three risk items are unfavourable
# (higher = riskier). Reversed here (8 - x, 1-7 scale) so idx_riesgo reads
# "higher = riskier" throughout, matching every other index's direction.
df <- df %>%
  mutate(
    long_term_commitment_score_rev = 8 - long_term_commitment_score,
    idx_competencia = rowMeans(across(c(competent_score, trustworthy_score)), na.rm = TRUE),
    idx_crecimiento = rowMeans(across(c(ambitious_score, promising_score,
                                        expected_new_challenges, expected_promotion)), na.rm = TRUE),
    idx_riesgo      = rowMeans(across(c(long_term_commitment_score_rev, expected_late_absence,
                                        expected_quit_work, risky_continuity_score)), na.rm = TRUE)
  )

outcomes_all <- c(
  "recommendation_probability", "good_investment_score", "idx_competencia",
  "idx_crecimiento", "idx_riesgo", "operational_costly_score",
  "salary_costly_score", "expected_accommodations", "expected_benefits_use"
)

LBL <- c(
  recommendation_probability = "Hiring recommendation",
  good_investment_score      = "Good investment",
  idx_competencia            = "Competence index",
  idx_crecimiento            = "Growth index",
  idx_riesgo                 = "Risk index",
  operational_costly_score   = "Operationally costly",
  salary_costly_score        = "Costly (salary)",
  expected_accommodations    = "Requests accommodations",
  expected_benefits_use      = "Uses company benefits"
)

# Fixed panel order for EVERY figure -- factor levels, never facet_wrap's
# default alphabetical/appearance order.
LBL_LEVELS <- LBL[outcomes_all]

outcome_family_map <- c(
  recommendation_probability = "hiring", good_investment_score = "hiring",
  operational_costly_score   = "cost",   salary_costly_score    = "cost",
  expected_accommodations    = "cost",   expected_benefits_use  = "cost",
  idx_riesgo                 = "risk",
  idx_crecimiento             = "growth",
  idx_competencia             = "competence",
  long_term_commitment_score = "risk"   # rides with idx_riesgo's family (Family C's raw-item add-on)
)

# For Figure F only: outcomes where higher = BETTER for the applicant need
# their women-minus-men shift sign flipped so that "rightward" consistently
# means "worse for the applicant" across every outcome on the shared axis.
reverse_for_worse <- c(
  recommendation_probability = TRUE, good_investment_score = TRUE,
  idx_competencia = TRUE, idx_crecimiento = TRUE,
  idx_riesgo = FALSE, operational_costly_score = FALSE,
  salary_costly_score = FALSE, expected_accommodations = FALSE,
  expected_benefits_use = FALSE
)

# --- Reliability checks -----------------------------------------------------
alpha_check <- function(data, items, name) {
  cat("\n=== psych::alpha() --", name, "===\n")
  a <- psych::alpha(as.data.frame(data[, items]), warnings = FALSE)
  print(a)
  raw_alpha <- a$total$raw_alpha
  if (raw_alpha < .70) {
    stop(sprintf("Index '%s' has alpha = %.3f (< .70) -- the index composition was revised; re-verify before trusting it.",
                 name, raw_alpha))
  }
  a
}

alpha_competencia <- alpha_check(df, c("competent_score", "trustworthy_score"), "idx_competencia")
alpha_crecimiento <- alpha_check(df, c("ambitious_score", "promising_score",
                                       "expected_new_challenges", "expected_promotion"), "idx_crecimiento")
alpha_riesgo <- alpha_check(df, c("long_term_commitment_score_rev", "expected_late_absence",
                                  "expected_quit_work", "risky_continuity_score"), "idx_riesgo")

# ===========================================================================
# 2. MODELS
# ===========================================================================

controls <- "pantalla_orden + template_contexto"
covars   <- "perfil_exp + perfil_desempeno + condicion_ocupacional"

rhs_set2a <- paste("perfil_genero * perfil_parental + perfil_estado_civil + perfil_plan_futuro +", covars, "+", controls)
rhs_set2b <- paste("perfil_genero * perfil_plan_futuro + perfil_estado_civil + perfil_parental +", covars, "+", controls)
rhs_setP  <- paste("perfil_genero * perfil_parental + perfil_genero * perfil_plan_futuro + perfil_estado_civil +", covars, "+", controls)

fit_set <- function(rhs, data, outcomes = outcomes_all) {
  map(outcomes, function(y) {
    tryCatch(
      lm_robust(as.formula(paste(y, "~", rhs)), data = data,
                clusters = data$numericalId, se_type = "CR2"),
      error = function(e) { message(sprintf("  failed: %s (%s)", y, e$message)); NULL }
    )
  }) %>% set_names(outcomes)
}

message("Fitting model sets (set2a, set2b, setP) x 9 outcomes...")
set2a <- fit_set(rhs_set2a, df); message("  set2a done")
set2b <- fit_set(rhs_set2b, df); message("  set2b done")
setP  <- fit_set(rhs_setP,  df); message("  setP done")

# --- Validation --------------------------------------------------------------
# |AME| == 2x|sum-contrast coefficient| holds EXACTLY only when the sample is
# perfectly balanced across every other factor gender interacts with. setP
# has perfil_genero interacting with BOTH perfil_parental and perfil_plan_
# futuro (unlike e.g. SET5 in 08_conference_models_and_figures.R, which has
# no gender interactions at all), so with this survey's real, unbalanced
# cell sizes the two quantities are only approximately equal -- empirically
# ~0.7% relative difference here, not the near-zero floating-point gap a
# 1e-6 absolute tolerance assumes. A relative check is used instead so this
# guards against an actual contrast-coding regression (which would produce a
# ~50%+ discrepancy, e.g. the marginal contrast collapsing to ~1x or flipping
# sign) without false-alarming on setP's expected interaction-driven drift.
m <- setP$recommendation_probability
b <- coef(m)[["perfil_generoMujer"]]
cc <- avg_comparisons(m, variables = "perfil_genero", newdata = df)$estimate[1]
rel_diff <- abs(abs(cc) - abs(2 * b)) / abs(2 * b)
stopifnot(rel_diff < 0.05)
message(sprintf("setP validation passed: |marginal contrast| ~= 2 x |coefficient| (relative diff = %.3f%%).", rel_diff * 100))

stopifnot(isTRUE(all.equal(vcov(m), marginaleffects::get_vcov(m))))
message("vcov check passed: marginaleffects reads the CR2 vcov straight off the lm_robust object.")

# ===========================================================================
# 3. ESTIMATION HELPERS
# ===========================================================================
# newdata is always a subset of OBSERVED rows (never datagrid()'s balanced
# grid) -- the design is D-efficient (200 of the possible combinations
# exist), so a balanced grid would average over profiles nobody ever saw.

print_subgroup_counts <- function(data, group_vars, title) {
  cat("\n", title, ":\n", sep = "")
  data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(n_obs = n(), n_clusters = n_distinct(numericalId), .groups = "drop") %>%
    print(n = Inf)
}

# marginaleffects does not return a `df` column at all -- and falls back to
# a normal-approximation `statistic` -- unless `df` is passed explicitly
# (confirmed empirically: avg_comparisons() on an lm_robust object returns
# no df column by default, for any of the simple / by-grouped / hypothesis
# call shapes used here). estimatr's CR2 SEs are themselves already
# Satterthwaite-approximated per coefficient (that's what `model$df` holds);
# marginaleffects can't extend that to an exact per-contrast Satterthwaite
# df for a compound linear combination (by-grouped, revpairwise), so every
# call below passes the conservative floor -- the minimum per-coefficient
# CR2/Satterthwaite df in the model -- as that contrast's df.
model_df <- function(model) min(model$df)

get_means <- function(models, by_vars, newdata) {
  imap_dfr(models, function(m, y) {
    tryCatch({
      p <- avg_predictions(m, by = by_vars, newdata = newdata, df = model_df(m))
      as_tibble(p) %>% mutate(outcome = y)
    }, error = function(e) NULL)
  })
}

# BH adjustment WITHIN outcome family (hiring has 2 members, cost has 4;
# risk/growth/competence are singletons post-index-restructuring, so their
# "correction" is a no-op -- that is intended, not a bug, given how few
# outcomes now share a family).
add_bh_family <- function(d) {
  d %>% group_by(outcome_family) %>% mutate(p.adj = p.adjust(p.value, method = "BH")) %>% ungroup()
}

get_contrast <- function(models, variables, newdata, by = NULL, hypothesis = NULL) {
  counts_pooled <- tibble(n_obs = nrow(newdata), n_clusters = n_distinct(newdata$numericalId))
  counts_by_gender <- newdata %>%
    group_by(perfil_genero) %>%
    summarise(n_obs = n(), n_clusters = n_distinct(numericalId), .groups = "drop") %>%
    mutate(perfil_genero = as.character(perfil_genero))

  out <- imap_dfr(models, function(m, y) {
    tryCatch({
      args <- list(model = m, variables = variables, newdata = newdata, df = model_df(m))
      if (!is.null(by)) args$by <- by
      if (!is.null(hypothesis)) args$hypothesis <- hypothesis
      res <- do.call(avg_comparisons, args)
      as_tibble(res) %>% mutate(outcome = y)
    }, error = function(e) { message(sprintf("  contrast failed: %s (%s)", y, e$message)); NULL })
  })

  if ("perfil_genero" %in% names(out)) {
    out <- out %>%
      mutate(perfil_genero = as.character(perfil_genero)) %>%
      left_join(counts_by_gender, by = "perfil_genero")
  } else {
    out <- out %>% bind_cols(counts_pooled[rep(1, nrow(out)), , drop = FALSE])
  }

  out %>%
    mutate(outcome_family = outcome_family_map[outcome],
           low_df_flag = df < 30) %>%
    add_bh_family()
}

# Accumulators for the final CSV / low-df flag.
contrast_log <- list()
log_contrast <- function(d, contrast_family, contrast_label, model_source) {
  gender_col <- if ("perfil_genero" %in% names(d)) d$perfil_genero else rep("Mujer-Hombre", nrow(d))
  d %>%
    mutate(contrast_family = contrast_family, contrast_label = contrast_label,
           model_source = model_source, gender = gender_col) %>%
    select(contrast_family, contrast_label, model_source, outcome, outcome_family, gender,
           estimate, std.error, conf.low, conf.high, df, p.value, p.adj, n_obs, n_clusters)
}
add_log <- function(d) contrast_log[[length(contrast_log) + 1]] <<- d

# Top-two-box / bottom-two-box shifts, computed directly from the data (not
# model-based). Used for topbox_shifts.csv (A-E, point estimates only) and,
# separately with a cluster bootstrap, for Figure F.
topbox_batch <- function(data, group_var, level_ref, level_cmp, contrast_label, gender_val) {
  d <- data
  if (!is.null(gender_val) && gender_val != "Mujer-Hombre") d <- d %>% filter(perfil_genero == gender_val)
  map_dfr(outcomes_all, function(y) {
    v1 <- d[[y]][d[[group_var]] == level_ref]
    v2 <- d[[y]][d[[group_var]] == level_cmp]
    tibble(
      contrast_label = contrast_label, gender = gender_val, outcome = y,
      ttb_ref = mean(v1 >= 6, na.rm = TRUE), ttb_cmp = mean(v2 >= 6, na.rm = TRUE),
      ttb_shift = mean(v2 >= 6, na.rm = TRUE) - mean(v1 >= 6, na.rm = TRUE),
      btb_ref = mean(v1 <= 2, na.rm = TRUE), btb_cmp = mean(v2 <= 2, na.rm = TRUE),
      btb_shift = mean(v2 <= 2, na.rm = TRUE) - mean(v1 <= 2, na.rm = TRUE)
    )
  })
}
topbox_log <- list()
add_topbox <- function(d) topbox_log[[length(topbox_log) + 1]] <<- d

# ===========================================================================
# 4. CONTRAST FAMILIES A-E
# ===========================================================================

# --- A. Pure gender penalty (setP) ------------------------------------------
# Childless AND no plans: women vs men. Two columns: the conditional
# prediction from setP (fitted on the full pooled data, averaged over this
# subset's observed rows) and a subset-refit robustness check (parental/plan
# terms dropped since they're constant in this subset, so keeping them would
# be collinear).
subset_A <- df %>% filter(perfil_parental == "actualmente no tiene hijos", perfil_plan_futuro == "no quiere")
print_subgroup_counts(subset_A, "perfil_genero", "Family A -- childless & no plans, by gender")

means_A <- get_means(setP, "perfil_genero", subset_A)

contrast_A <- get_contrast(setP, variables = list(perfil_genero = c("Hombre", "Mujer")), newdata = subset_A)
add_log(log_contrast(contrast_A, "A", "Pure gender penalty: women vs men (childless, no plans)", "setP"))

rhs_A_sub <- paste("perfil_genero + perfil_estado_civil +", covars, "+", controls)
setP_sub <- fit_set(rhs_A_sub, subset_A)
contrast_A_sub <- get_contrast(setP_sub, variables = list(perfil_genero = c("Hombre", "Mujer")), newdata = subset_A)
add_log(log_contrast(contrast_A_sub, "A", "Pure gender penalty (subset-refit robustness): women vs men (childless, no plans)", "setP_subset_robustness"))

add_topbox(topbox_batch(subset_A, "perfil_genero", "Hombre", "Mujer", "A: women vs men (childless, no plans)", "Mujer-Hombre"))

sig_A <- contrast_A %>% transmute(outcome, sig = p.adj < .05)
means_A_final <- means_A %>%
  mutate(perfil_genero = as.character(perfil_genero)) %>%
  left_join(sig_A, by = "outcome") %>%
  mutate(sig = if_else(perfil_genero == "Hombre", TRUE, sig),
         label = factor(LBL[outcome], levels = LBL_LEVELS),
         x_val = factor(if_else(perfil_genero == "Hombre", "Men", "Women"), levels = c("Men", "Women")),
         point_shape = factor(if_else(sig, "sig", "ns"), levels = c("sig", "ns")))

# --- B. Future motherhood penalty (set2b) -----------------------------------
# Childless: plans in 2 years vs no plans, by gender + DiD.
subset_B <- df %>% filter(perfil_parental == "actualmente no tiene hijos")
print_subgroup_counts(subset_B, c("perfil_genero", "perfil_plan_futuro"), "Family B/C(childless) -- childless, by gender x plan horizon")

means_B_full <- get_means(set2b, c("perfil_plan_futuro", "perfil_genero"), subset_B)

contrast_B <- get_contrast(set2b, variables = list(perfil_plan_futuro = c("no quiere", "en 2 anos")),
                            by = "perfil_genero", newdata = subset_B)
add_log(log_contrast(contrast_B, "B", "Future motherhood penalty: 2yr vs no plans, by gender (childless)", "set2b"))

contrast_B_DiD <- get_contrast(set2b, variables = list(perfil_plan_futuro = c("no quiere", "en 2 anos")),
                                by = "perfil_genero", hypothesis = ~revpairwise, newdata = subset_B)
add_log(log_contrast(contrast_B_DiD, "B", "Future motherhood penalty: 2yr vs no plans, women-minus-men DiD (childless)", "set2b"))

add_topbox(topbox_batch(subset_B %>% filter(perfil_genero == "Mujer"), "perfil_plan_futuro", "no quiere", "en 2 anos", "B: 2yr vs no plans (childless)", "Mujer"))
add_topbox(topbox_batch(subset_B %>% filter(perfil_genero == "Hombre"), "perfil_plan_futuro", "no quiere", "en 2 anos", "B: 2yr vs no plans (childless)", "Hombre"))

sig_B <- contrast_B %>% transmute(outcome, perfil_genero = as.character(perfil_genero), sig = p.adj < .05)
means_B_final <- means_B_full %>%
  filter(perfil_plan_futuro %in% c("no quiere", "en 2 anos")) %>%
  mutate(perfil_genero = as.character(perfil_genero)) %>%
  left_join(sig_B, by = c("outcome", "perfil_genero")) %>%
  mutate(sig = if_else(perfil_plan_futuro == "no quiere", TRUE, sig),
         label = factor(LBL[outcome], levels = LBL_LEVELS),
         x_val = factor(recode(as.character(perfil_plan_futuro), "no quiere" = "No plans", "en 2 anos" = "In 2 years"),
                        levels = c("No plans", "In 2 years")),
         point_shape = factor(if_else(sig, "sig", "ns"), levels = c("sig", "ns")))

# --- C. Five-year horizon (set2b) -------------------------------------------
# Figures cover the CHILDLESS population (direct "same structure as B" read,
# matching the figC_five_year_{...} filenames -- one trio, like B). The
# parents-population version of this same contrast is ALSO computed (per
# "for childless applicants and for parents separately") and written to
# all_contrasts.csv with its own contrast_label, but does not get a separate
# figure trio since none was named in the output figure list.
subset_D <- df %>% filter(perfil_parental %in% c("tiene un hijo", "tiene dos hijos"))
print_subgroup_counts(subset_D, c("perfil_genero", "perfil_plan_futuro"), "Family D/C(parents) -- parents, by gender x plan horizon")

means_D_full <- get_means(set2b, c("perfil_plan_futuro", "perfil_genero"), subset_D)

contrast_C_childless <- get_contrast(set2b, variables = list(perfil_plan_futuro = c("no quiere", "en 5 anos")),
                                      by = "perfil_genero", newdata = subset_B)
add_log(log_contrast(contrast_C_childless, "C", "Five-year horizon: 5yr vs no plans, by gender (childless)", "set2b"))

contrast_C_childless_DiD <- get_contrast(set2b, variables = list(perfil_plan_futuro = c("no quiere", "en 5 anos")),
                                          by = "perfil_genero", hypothesis = ~revpairwise, newdata = subset_B)
add_log(log_contrast(contrast_C_childless_DiD, "C", "Five-year horizon: 5yr vs no plans, women-minus-men DiD (childless)", "set2b"))

contrast_C_parents <- get_contrast(set2b, variables = list(perfil_plan_futuro = c("no quiere", "en 5 anos")),
                                    by = "perfil_genero", newdata = subset_D)
add_log(log_contrast(contrast_C_parents, "C", "Five-year horizon: 5yr vs no plans, by gender (parents)", "set2b"))

contrast_C_parents_DiD <- get_contrast(set2b, variables = list(perfil_plan_futuro = c("no quiere", "en 5 anos")),
                                        by = "perfil_genero", hypothesis = ~revpairwise, newdata = subset_D)
add_log(log_contrast(contrast_C_parents_DiD, "C", "Five-year horizon: 5yr vs no plans, women-minus-men DiD (parents)", "set2b"))

# Raw long_term_commitment_score item (not one of the 9 panel outcomes) --
# supports the claim that the 5yr horizon carries no penalty except here.
m_ltc <- lm_robust(as.formula(paste("long_term_commitment_score ~", rhs_set2b)), data = df,
                    clusters = df$numericalId, se_type = "CR2")
models_ltc <- list(long_term_commitment_score = m_ltc)

ltc_childless <- get_contrast(models_ltc, variables = list(perfil_plan_futuro = c("no quiere", "en 5 anos")),
                               by = "perfil_genero", newdata = subset_B)
add_log(log_contrast(ltc_childless, "C", "Five-year horizon, raw long_term_commitment_score: 5yr vs no plans, by gender (childless)", "set2b"))
ltc_childless_DiD <- get_contrast(models_ltc, variables = list(perfil_plan_futuro = c("no quiere", "en 5 anos")),
                                   by = "perfil_genero", hypothesis = ~revpairwise, newdata = subset_B)
add_log(log_contrast(ltc_childless_DiD, "C", "Five-year horizon, raw long_term_commitment_score: 5yr vs no plans, DiD (childless)", "set2b"))
ltc_parents <- get_contrast(models_ltc, variables = list(perfil_plan_futuro = c("no quiere", "en 5 anos")),
                             by = "perfil_genero", newdata = subset_D)
add_log(log_contrast(ltc_parents, "C", "Five-year horizon, raw long_term_commitment_score: 5yr vs no plans, by gender (parents)", "set2b"))
ltc_parents_DiD <- get_contrast(models_ltc, variables = list(perfil_plan_futuro = c("no quiere", "en 5 anos")),
                                 by = "perfil_genero", hypothesis = ~revpairwise, newdata = subset_D)
add_log(log_contrast(ltc_parents_DiD, "C", "Five-year horizon, raw long_term_commitment_score: 5yr vs no plans, DiD (parents)", "set2b"))

add_topbox(topbox_batch(subset_B %>% filter(perfil_genero == "Mujer"), "perfil_plan_futuro", "no quiere", "en 5 anos", "C: 5yr vs no plans (childless)", "Mujer"))
add_topbox(topbox_batch(subset_B %>% filter(perfil_genero == "Hombre"), "perfil_plan_futuro", "no quiere", "en 5 anos", "C: 5yr vs no plans (childless)", "Hombre"))
add_topbox(topbox_batch(subset_D %>% filter(perfil_genero == "Mujer"), "perfil_plan_futuro", "no quiere", "en 5 anos", "C: 5yr vs no plans (parents)", "Mujer"))
add_topbox(topbox_batch(subset_D %>% filter(perfil_genero == "Hombre"), "perfil_plan_futuro", "no quiere", "en 5 anos", "C: 5yr vs no plans (parents)", "Hombre"))

sig_C <- contrast_C_childless %>% transmute(outcome, perfil_genero = as.character(perfil_genero), sig = p.adj < .05)
means_C_final <- means_B_full %>%
  filter(perfil_plan_futuro %in% c("no quiere", "en 5 anos")) %>%
  mutate(perfil_genero = as.character(perfil_genero)) %>%
  left_join(sig_C, by = c("outcome", "perfil_genero")) %>%
  mutate(sig = if_else(perfil_plan_futuro == "no quiere", TRUE, sig),
         label = factor(LBL[outcome], levels = LBL_LEVELS),
         x_val = factor(recode(as.character(perfil_plan_futuro), "no quiere" = "No plans", "en 5 anos" = "In 5 years"),
                        levels = c("No plans", "In 5 years")),
         point_shape = factor(if_else(sig, "sig", "ns"), levels = c("sig", "ns")))

# --- D. Additional child penalty (set2b) ------------------------------------
# Parents (1-2 children): plans in 2 years vs no further plans, by gender + DiD.
contrast_D <- get_contrast(set2b, variables = list(perfil_plan_futuro = c("no quiere", "en 2 anos")),
                            by = "perfil_genero", newdata = subset_D)
add_log(log_contrast(contrast_D, "D", "Additional child penalty: 2yr vs no further plans, by gender (parents)", "set2b"))

contrast_D_DiD <- get_contrast(set2b, variables = list(perfil_plan_futuro = c("no quiere", "en 2 anos")),
                                by = "perfil_genero", hypothesis = ~revpairwise, newdata = subset_D)
add_log(log_contrast(contrast_D_DiD, "D", "Additional child penalty: 2yr vs no further plans, women-minus-men DiD (parents)", "set2b"))

add_topbox(topbox_batch(subset_D %>% filter(perfil_genero == "Mujer"), "perfil_plan_futuro", "no quiere", "en 2 anos", "D: 2yr vs no further plans (parents)", "Mujer"))
add_topbox(topbox_batch(subset_D %>% filter(perfil_genero == "Hombre"), "perfil_plan_futuro", "no quiere", "en 2 anos", "D: 2yr vs no further plans (parents)", "Hombre"))

sig_D <- contrast_D %>% transmute(outcome, perfil_genero = as.character(perfil_genero), sig = p.adj < .05)
means_D_final <- means_D_full %>%
  filter(perfil_plan_futuro %in% c("no quiere", "en 2 anos")) %>%
  mutate(perfil_genero = as.character(perfil_genero)) %>%
  left_join(sig_D, by = c("outcome", "perfil_genero")) %>%
  mutate(sig = if_else(perfil_plan_futuro == "no quiere", TRUE, sig),
         label = factor(LBL[outcome], levels = LBL_LEVELS),
         x_val = factor(recode(as.character(perfil_plan_futuro), "no quiere" = "No further plans", "en 2 anos" = "In 2 years"),
                        levels = c("No further plans", "In 2 years")),
         point_shape = factor(if_else(sig, "sig", "ns"), levels = c("sig", "ns")))

# --- E. Pure motherhood penalty (set2a) -------------------------------------
# No plans to have children: one child vs none, two children vs none, by
# gender + both DiDs.
subset_E <- df %>% filter(perfil_plan_futuro == "no quiere")
print_subgroup_counts(subset_E, c("perfil_genero", "perfil_parental"), "Family E -- no future plans, by gender x parental status")

means_E <- get_means(set2a, c("perfil_parental", "perfil_genero"), subset_E)

contrast_E1 <- get_contrast(set2a, variables = list(perfil_parental = c("actualmente no tiene hijos", "tiene un hijo")),
                             by = "perfil_genero", newdata = subset_E)
add_log(log_contrast(contrast_E1, "E", "Pure motherhood penalty: one child vs none, by gender (no plans)", "set2a"))
contrast_E1_DiD <- get_contrast(set2a, variables = list(perfil_parental = c("actualmente no tiene hijos", "tiene un hijo")),
                                 by = "perfil_genero", hypothesis = ~revpairwise, newdata = subset_E)
add_log(log_contrast(contrast_E1_DiD, "E", "Pure motherhood penalty: one child vs none, women-minus-men DiD (no plans)", "set2a"))

contrast_E2 <- get_contrast(set2a, variables = list(perfil_parental = c("actualmente no tiene hijos", "tiene dos hijos")),
                             by = "perfil_genero", newdata = subset_E)
add_log(log_contrast(contrast_E2, "E", "Pure motherhood penalty: two children vs none, by gender (no plans)", "set2a"))
contrast_E2_DiD <- get_contrast(set2a, variables = list(perfil_parental = c("actualmente no tiene hijos", "tiene dos hijos")),
                                 by = "perfil_genero", hypothesis = ~revpairwise, newdata = subset_E)
add_log(log_contrast(contrast_E2_DiD, "E", "Pure motherhood penalty: two children vs none, women-minus-men DiD (no plans)", "set2a"))

add_topbox(topbox_batch(subset_E %>% filter(perfil_genero == "Mujer"), "perfil_parental", "actualmente no tiene hijos", "tiene un hijo", "E: one child vs none (no plans)", "Mujer"))
add_topbox(topbox_batch(subset_E %>% filter(perfil_genero == "Hombre"), "perfil_parental", "actualmente no tiene hijos", "tiene un hijo", "E: one child vs none (no plans)", "Hombre"))
add_topbox(topbox_batch(subset_E %>% filter(perfil_genero == "Mujer"), "perfil_parental", "actualmente no tiene hijos", "tiene dos hijos", "E: two children vs none (no plans)", "Mujer"))
add_topbox(topbox_batch(subset_E %>% filter(perfil_genero == "Hombre"), "perfil_parental", "actualmente no tiene hijos", "tiene dos hijos", "E: two children vs none (no plans)", "Hombre"))

sig_E1 <- contrast_E1 %>% transmute(outcome, perfil_genero = as.character(perfil_genero), parental_level = "tiene un hijo", sig = p.adj < .05)
sig_E2 <- contrast_E2 %>% transmute(outcome, perfil_genero = as.character(perfil_genero), parental_level = "tiene dos hijos", sig = p.adj < .05)
sig_E <- bind_rows(sig_E1, sig_E2)

means_E_final <- means_E %>%
  mutate(perfil_genero = as.character(perfil_genero), perfil_parental = as.character(perfil_parental)) %>%
  left_join(sig_E, by = c("outcome", "perfil_genero", "perfil_parental" = "parental_level")) %>%
  mutate(sig = if_else(perfil_parental == "actualmente no tiene hijos", TRUE, sig),
         label = factor(LBL[outcome], levels = LBL_LEVELS),
         x_val = factor(recode(perfil_parental, "actualmente no tiene hijos" = "None",
                               "tiene un hijo" = "One child", "tiene dos hijos" = "Two children"),
                        levels = c("None", "One child", "Two children")),
         point_shape = factor(if_else(sig, "sig", "ns"), levels = c("sig", "ns")))

# ===========================================================================
# 5. IDENTICAL Y-RANGE PER OUTCOME, ACROSS EVERY GROUP IN ANY FIGURE
# ===========================================================================
# Computed once across A-E's plotted means (not the C-parents/raw-item add-
# ons, which never appear in a figure), padded 10%, then forced onto every
# panel via a geom_blank() anchor -- this is what makes "the same panel
# looks flat for men and moves for women" a fair visual claim: every figure
# literally shares the same y-window per outcome.
y_range_df <- bind_rows(
  means_A_final %>% select(label, conf.low, conf.high),
  means_B_final %>% select(label, conf.low, conf.high),
  means_C_final %>% select(label, conf.low, conf.high),
  means_D_final %>% select(label, conf.low, conf.high),
  means_E_final %>% select(label, conf.low, conf.high)
) %>%
  group_by(label) %>%
  summarise(ymin = min(conf.low, na.rm = TRUE), ymax = max(conf.high, na.rm = TRUE), .groups = "drop") %>%
  mutate(pad = (ymax - ymin) * 0.10, ymin = ymin - pad, ymax = ymax + pad) %>%
  pivot_longer(c(ymin, ymax), names_to = "bound", values_to = "value") %>%
  select(label, value)

# ===========================================================================
# 6. PLOTTING FUNCTION -- shared by all fifteen A-E small multiples
# ===========================================================================

plot_small_multiple <- function(data, x_title, genders = c("Hombre", "Mujer"),
                                 connect = TRUE, caption_extra = "") {
  d <- data %>% filter(perfil_genero %in% genders)
  single_gender <- n_distinct(d$perfil_genero) == 1

  yr <- y_range_df %>%
    filter(label %in% levels(droplevels(d$label))) %>%
    mutate(x_val = factor(levels(d$x_val)[1], levels = levels(d$x_val)))

  p <- ggplot(d, aes(x = x_val, y = estimate, group = perfil_genero)) +
    geom_blank(data = yr, aes(x = x_val, y = value), inherit.aes = FALSE)

  if (connect) p <- p + geom_line(aes(colour = perfil_genero), linewidth = 0.7)

  p <- p +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high, colour = perfil_genero), width = 0.09, linewidth = 0.4) +
    geom_point(aes(colour = perfil_genero, shape = point_shape), size = 2.8) +
    facet_wrap(~ label, nrow = 3, ncol = 3, scales = "free_y") +
    scale_colour_manual(values = c(Hombre = PAL$teal, Mujer = PAL$accent), labels = c(Hombre = "Men", Mujer = "Women")) +
    scale_shape_manual(values = c(sig = 16, ns = 1),
                       labels = c(sig = "Reference level, or contrast BH-significant", ns = "Contrast not BH-significant"),
                       name = NULL) +
    labs(x = x_title, y = "Marginal mean (7-point scale)",
         caption = paste0("Filled point = reference level or a contrast that survives BH correction; hollow = does not.", caption_extra)) +
    theme_conf() +
    theme(strip.text = element_text(hjust = 0))

  if (single_gender) p <- p + guides(colour = "none")
  p
}

save_fig <- function(p, filename) {
  ggsave(file.path(out_dir, filename), p, width = 11, height = 8.5, dpi = 300, bg = "white")
}

# --- Family A figures --------------------------------------------------------
save_fig(plot_small_multiple(means_A_final, "Gender", genders = "Mujer", connect = FALSE,
                              caption_extra = "\nChildless applicants with no future fertility plans."),
         "figA_pure_gender_women.png")
save_fig(plot_small_multiple(means_A_final, "Gender", genders = "Hombre", connect = FALSE,
                              caption_extra = "\nChildless applicants with no future fertility plans."),
         "figA_pure_gender_men.png")
save_fig(plot_small_multiple(means_A_final, "Gender", genders = c("Hombre", "Mujer"), connect = FALSE,
                              caption_extra = "\nChildless applicants with no future fertility plans."),
         "figA_pure_gender_combined.png")

# --- Family B figures --------------------------------------------------------
save_fig(plot_small_multiple(means_B_final, "Family plans", genders = "Mujer",
                              caption_extra = "\nChildless applicants."),
         "figB_future_motherhood_women.png")
save_fig(plot_small_multiple(means_B_final, "Family plans", genders = "Hombre",
                              caption_extra = "\nChildless applicants."),
         "figB_future_motherhood_men.png")
save_fig(plot_small_multiple(means_B_final, "Family plans", genders = c("Hombre", "Mujer"),
                              caption_extra = "\nChildless applicants."),
         "figB_future_motherhood_combined.png")

# --- Family C figures (childless population) ---------------------------------
save_fig(plot_small_multiple(means_C_final, "Family plans", genders = "Mujer",
                              caption_extra = "\nChildless applicants. See all_contrasts.csv for the parents-population version of this same contrast."),
         "figC_five_year_women.png")
save_fig(plot_small_multiple(means_C_final, "Family plans", genders = "Hombre",
                              caption_extra = "\nChildless applicants. See all_contrasts.csv for the parents-population version of this same contrast."),
         "figC_five_year_men.png")
save_fig(plot_small_multiple(means_C_final, "Family plans", genders = c("Hombre", "Mujer"),
                              caption_extra = "\nChildless applicants. See all_contrasts.csv for the parents-population version of this same contrast."),
         "figC_five_year_combined.png")

# --- Family D figures ---------------------------------------------------------
save_fig(plot_small_multiple(means_D_final, "Family plans", genders = "Mujer",
                              caption_extra = "\nApplicants who already have children."),
         "figD_additional_child_women.png")
save_fig(plot_small_multiple(means_D_final, "Family plans", genders = "Hombre",
                              caption_extra = "\nApplicants who already have children."),
         "figD_additional_child_men.png")
save_fig(plot_small_multiple(means_D_final, "Family plans", genders = c("Hombre", "Mujer"),
                              caption_extra = "\nApplicants who already have children."),
         "figD_additional_child_combined.png")

# --- Family E figures ---------------------------------------------------------
save_fig(plot_small_multiple(means_E_final, "Parental status", genders = "Mujer",
                              caption_extra = "\nApplicants with no future fertility plans."),
         "figE_pure_motherhood_women.png")
save_fig(plot_small_multiple(means_E_final, "Parental status", genders = "Hombre",
                              caption_extra = "\nApplicants with no future fertility plans."),
         "figE_pure_motherhood_men.png")
save_fig(plot_small_multiple(means_E_final, "Parental status", genders = c("Hombre", "Mujer"),
                              caption_extra = "\nApplicants with no future fertility plans."),
         "figE_pure_motherhood_combined.png")

message("Fifteen small-multiple figures (A-E x women/men/combined) written.")

# ===========================================================================
# 7. FIGURE F -- dissociation, top-two-box shift, single shared axis
# ===========================================================================
# Not standardized (no Cohen's d / Glass's delta): the top-two-box metric is
# already comparable across outcomes without choosing a denominator, needs
# no benchmark, and is invariant to any monotonic rescaling -- which matters
# because several evaluative outcomes have 22-25% of responses at the
# ceiling.

id_rows <- split(seq_len(nrow(subset_A)), subset_A$numericalId)
uid <- names(id_rows)

boot_ttb_shift <- function(outcome_vals, gender_vals, n_boot = 1000, seed = 2026) {
  set.seed(seed)
  out <- numeric(n_boot)
  for (i in seq_len(n_boot)) {
    samp_ids <- sample(uid, length(uid), replace = TRUE)
    rows <- unlist(id_rows[samp_ids], use.names = FALSE)
    g <- gender_vals[rows]; v <- outcome_vals[rows]
    out[i] <- mean(v[g == "Mujer"] >= 6, na.rm = TRUE) - mean(v[g == "Hombre"] >= 6, na.rm = TRUE)
  }
  out
}

gends_A <- as.character(subset_A$perfil_genero)
f_boot <- map_dfr(outcomes_all, function(y) {
  vals <- subset_A[[y]]
  point_est <- mean(vals[gends_A == "Mujer"] >= 6, na.rm = TRUE) - mean(vals[gends_A == "Hombre"] >= 6, na.rm = TRUE)
  boot_vals <- boot_ttb_shift(vals, gends_A)
  tibble(outcome = y, estimate = point_est,
         conf.low = as.numeric(quantile(boot_vals, .025, na.rm = TRUE)),
         conf.high = as.numeric(quantile(boot_vals, .975, na.rm = TRUE)))
})

evaluation_outcomes <- c("recommendation_probability", "good_investment_score", "idx_competencia", "idx_crecimiento")

f_dat <- f_boot %>%
  mutate(
    reverse      = reverse_for_worse[outcome],
    plotted      = if_else(reverse, -estimate, estimate) * 100,
    plotted_low  = if_else(reverse, -conf.high, conf.low) * 100,
    plotted_high = if_else(reverse, -conf.low, conf.high) * 100,
    label = factor(LBL[outcome], levels = LBL_LEVELS),
    block = if_else(outcome %in% evaluation_outcomes, "Evaluation", "Predicted conduct")
  )

figF <- ggplot(f_dat, aes(x = plotted, y = factor(label, levels = rev(LBL_LEVELS)))) +
  geom_vline(xintercept = 0, colour = PAL$ink, linewidth = 0.9) +
  geom_errorbar(aes(xmin = plotted_low, xmax = plotted_high), width = 0.2, colour = PAL$soft, orientation = "y") +
  geom_point(size = 3.2, colour = PAL$teal) +
  facet_grid(factor(block, levels = c("Evaluation", "Predicted conduct")) ~ ., scales = "free_y", space = "free_y", switch = "y") +
  labs(x = "Change in P(rating >= 6), women minus men, percentage points\n(reversed for evaluative outcomes, so rightward = worse for the applicant)",
       y = NULL,
       caption = "Childless, no future plans. 95% CI: respondent-clustered bootstrap, 1000 reps.\nReversed (x -1) for: hiring rec., good investment, competence, growth -- noted, not colour-coded.") +
  theme_conf() +
  theme(strip.text.y.left = element_text(colour = PAL$teal, face = "bold", hjust = 0.5, vjust = 0.5),
        panel.spacing.y = unit(1.1, "lines"))

ggsave(file.path(out_dir, "figF_dissociation_topbox.png"), figF, width = 8, height = 6, dpi = 300, bg = "white")
message("Figure F (dissociation, top-two-box) written.")

# ===========================================================================
# 8. OUTPUT -- CSVs and console summaries
# ===========================================================================

all_contrasts <- bind_rows(contrast_log)
write_csv(all_contrasts, file.path(out_dir, "all_contrasts.csv"))

topbox_shifts <- bind_rows(topbox_log)
write_csv(topbox_shifts, file.path(out_dir, "topbox_shifts.csv"))

low_df <- all_contrasts %>% filter(df < 30)
cat("\n--- Contrasts with Satterthwaite/CR2 df < 30", if (nrow(low_df) == 0) "(none) ---\n" else "---\n")
if (nrow(low_df) > 0) print(low_df %>% select(contrast_label, outcome, gender, df, p.value), n = Inf)

cat("\nAlphas -- idx_competencia:", round(alpha_competencia$total$raw_alpha, 3),
    " idx_crecimiento:", round(alpha_crecimiento$total$raw_alpha, 3),
    " idx_riesgo:", round(alpha_riesgo$total$raw_alpha, 3), "\n")

message("\nDone. Figures and tables written to: ", out_dir)
