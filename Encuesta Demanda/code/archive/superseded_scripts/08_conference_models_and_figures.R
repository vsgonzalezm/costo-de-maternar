# ============================================================================
# 08 - CONFERENCE: models and figures
# ----------------------------------------------------------------------------
# Consolidated script for the conference presentation. Fits ONLY the model sets
# that appear in the talk and produces twenty figures over 8 outcomes
# (salary_costly_score and idx_evaluation excluded, see notes below):
#   1, 1b                 - anchor (gender), all outcomes, childless+any plan
#   2, 2b, 2c, 2c-b        - fertility horizon (no plans/2yr/5yr), childless
#   2d                     - gendered effect of the 2yr contrast, childless
#   2e, 2f                 - fertility horizon (2yr only) and its gendered
#                            effect, AMONG MOTHERS
#   3                      - parental threshold (none/1/2 children), by gender
#   3e, 3f                 - one-child / two-children vs childless, AMONG
#                            WOMEN (general population, any plan_futuro)
#   3b, 3c                 - one-child / two-children vs childless, among
#                            women with NO future fertility plans
#   3d                     - gendered effect of the two-children contrast,
#                            among people with no future fertility plans
#   4, 5, 6, 7             - occupational moderation, response distributions,
#                            dominance CDFs, summary matrix
#   8                      - outcome x contrast heatmap, every headline
#                            contrast computed above in one grid
# "b"/"d"/"f" figures plot a contrast directly (with its own CI, against
# zero) instead of the two means/two per-gender-contrasts it's computed
# from; 2c/3b/3c isolate one specific level comparison as a Figure-1-style
# single-panel dumbbell instead of a multi-facet, multi-level figure.
#
# Presented model sets (all pooled, occupational level as an additive control
# unless noted) and what is actually COMPUTED (as a formal contrast with an
# SE/p-value, in contrasts_all) from each -- as opposed to just visualized as
# a descriptive marginal mean (Figures 2/3 show means, not tested contrasts):
#   SET 5  - main effects (no interactions)      -> Anchor (fixed-profile contrast)
#   SET 2b - gender x family plans                -> mm_plan (Fig 2); H4 (2yr DiD)
#            [H1, H5 (5yr DiD) are NOT computed as formal contrasts here]
#   SET 2a - gender x parental                    -> mm_parental (Fig 3); H9 (2-child DiD)
#            [H3, H8 (1-child DiD) are NOT computed as formal contrasts here]
#   SET 4b - gender x parental x plan_futuro       -> mothers' and no-future-
#            plans subgroup figures (2e/2f/3b/3c/3d); H6 (Fig 2f)
#   SET 5a - gender x occupation                  -> H11 (moderation test)
#   SET 5b - parental x occupation                -> H12 (moderation test, 2-child level)
#   SET 5c - plan_futuro x occupation             -> H13 (moderation test, 2yr level)
# H10, H14, H15 are not tested here (no SET 5 main-effect-of-occupation
# contrast, and no SET 5d/5e double-differenced moderation tests are fit).
#
# NOT fitted here (appendix only, see 04_models.R/07_Indices.R): SET 1 split
# by condition, SET 3, SET 4a, SET 5d, SET 5e.
#
# Outputs: Outputs/Tables/conference/
# ============================================================================

library(tidyverse)
library(here)
library(estimatr)
library(marginaleffects)
library(patchwork)
library(writexl)

project_dir <- if (dir.exists(here("Encuesta Demanda", "Data"))) here("Encuesta Demanda") else here()
out_dir <- file.path(project_dir, "Outputs", "Tables", "conference")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------
# Palette - matches custom.scss in the deck
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
# 1. DATA  - factor coding must match 04_models.R exactly
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
    # Mirrors 07_Indices.R exactly (constructos + alpha results, agosto 2026),
    # EXCEPT idx_evaluation, which was dropped here (and only here -- it's
    # still defined/fit in 07_Indices.R; flagged as a drift to reconcile if
    # that file should also drop it):
    #   costo (3 items, no salary)          alpha = .504 -> fails, stays disaggregated
    #   riesgo (+ long_term_commitment_rev) alpha = .710 -> idx_riesgo
    #   crecimiento                          alpha = .837 -> idx_crecimiento
    #   competencia (2 items)                alpha = .757 -> idx_competencia
    #   investment (new)                     alpha = .717 -> idx_investment
    # long_term_commitment_score is reverse-direction relative to the rest of
    # riesgo (higher commitment = lower risk); reverse-scored (1-7 scale)
    # before entering that index, same as in 07_Indices.R.
    # salary_costly_score is intentionally in no construct (see 07_Indices.R
    # notes: reverse-coding it doesn't fix a direction problem -- it just
    # isn't measuring the same latent thing as costo or investment here) --
    # and is dropped as an analyzed outcome entirely in this deck (not just
    # unconstructed), per request. It's still computed/present in `df` above
    # since it's part of `base_long_final`, just never added to outcomes_all.
    long_term_commitment_score_rev = 8 - long_term_commitment_score,
    idx_riesgo = rowMeans(across(c(risky_continuity_score, expected_late_absence,
                                   expected_quit_work, long_term_commitment_score_rev)), na.rm = TRUE),
    idx_crecimiento = rowMeans(across(c(ambitious_score, promising_score,
                                        expected_new_challenges, expected_promotion)), na.rm = TRUE),
    idx_competencia = rowMeans(across(c(trustworthy_score, competent_score)), na.rm = TRUE),
    idx_investment  = rowMeans(across(c(recommendation_probability, good_investment_score)), na.rm = TRUE),
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

# Guard against the anos/años normalisation bug
stopifnot(
  identical(levels(df$perfil_plan_futuro), c("en 2 anos", "en 5 anos", "no quiere")),
  identical(levels(df$perfil_genero), c("Mujer", "Hombre")),
  !any(is.na(df$perfil_plan_futuro))
)

# Outcome set mirrors 07_Indices.R's outcomes_indices (8 total here --
# idx_evaluation and salary_costly_score removed, see notes above):
# recommendation_probability is
# kept standalone in addition to feeding idx_investment -- it's the
# project's primary headline DV (pre-reg item 5, the Anchor contrast) --
# see 07_Indices.R's note on this. good_investment_score/trustworthy_score/
# competent_score/etc. are NOT listed individually any more: they're fully
# absorbed into idx_investment/idx_competencia now that those pass the
# reliability bar.
outcomes_idx <- c("idx_riesgo", "idx_crecimiento", "idx_competencia", "idx_investment")
outcomes_raw <- c("recommendation_probability", "operational_costly_score",
                  "expected_benefits_use", "expected_accommodations")
outcomes_all <- c(outcomes_raw, outcomes_idx)

LBL <- c(
  recommendation_probability = "Hiring recommendation",
  idx_riesgo      = "Risk index",
  idx_crecimiento = "Growth index",
  idx_competencia = "Competence index",
  idx_investment  = "Investment index",
  operational_costly_score = "Operational cost",
  expected_benefits_use    = "Use of company benefits",
  expected_accommodations  = "Request for accommodations"
)

# Thematic grouping of outcomes_all for Figure 1's facets.
BLOCK_MAP <- c(
  operational_costly_score = "Cost", expected_benefits_use = "Cost",
  expected_accommodations = "Cost",
  idx_riesgo = "Risk",
  recommendation_probability = "Evaluation", idx_crecimiento = "Evaluation",
  idx_competencia = "Evaluation", idx_investment = "Evaluation"
)
BLOCK_LEVELS <- c("Cost", "Risk", "Evaluation")

# ===========================================================================
# 2. MODELS  - only the sets that appear in the talk
# ===========================================================================

controls <- "pantalla_orden + template_contexto"

rhs_main <- paste(
  "perfil_genero + perfil_estado_civil + perfil_parental +",
  "perfil_plan_futuro + perfil_exp + perfil_desempeno +",
  "condicion_ocupacional +", controls
)

model_sets <- list(
  set5   = rhs_main,
  set2a  = paste("perfil_genero * perfil_parental + perfil_estado_civil + perfil_plan_futuro +",
                 "perfil_exp + perfil_desempeno + condicion_ocupacional +", controls),
  set2b  = paste("perfil_genero * perfil_plan_futuro + perfil_estado_civil + perfil_parental +",
                 "perfil_exp + perfil_desempeno + condicion_ocupacional +", controls),
  set5a  = paste("perfil_genero * condicion_ocupacional + perfil_estado_civil + perfil_parental +",
                 "perfil_plan_futuro + perfil_exp + perfil_desempeno +", controls),
  set5b  = paste("perfil_parental * condicion_ocupacional + perfil_genero + perfil_estado_civil +",
                 "perfil_plan_futuro + perfil_exp + perfil_desempeno +", controls),
  set5c  = paste("perfil_plan_futuro * condicion_ocupacional + perfil_genero + perfil_estado_civil +",
                 "perfil_parental + perfil_exp + perfil_desempeno +", controls),
  # Three-way interaction (mirrors 07_Indices.R's set4b_pooled): the only
  # model here where the plan_futuro effect is allowed to vary by parental
  # status, needed for the "among mothers" / "no future plans" figures
  # below -- SET2a/SET2b each have only one of those two interactions, so
  # restricting their newdata to a subgroup would silently reproduce the
  # pooled-population estimate rather than a genuinely subgroup-specific one.
  set4b  = paste("perfil_genero * perfil_parental * perfil_plan_futuro + perfil_estado_civil +",
                 "perfil_exp + perfil_desempeno + condicion_ocupacional +", controls)
)

fit_set <- function(rhs, data = df) {
  map(outcomes_all, function(y) {
    tryCatch(
      lm_robust(as.formula(paste(y, "~", rhs)), data = data,
                clusters = data$numericalId, se_type = "CR2"),
      error = function(e) { message(sprintf("  failed: %s", y)); NULL }
    )
  }) %>% set_names(outcomes_all)
}

message("Fitting presented model sets...")
M <- imap(model_sets, function(rhs, nm) { message("  ", nm); fit_set(rhs) })

# Sanity checks: a two-level marginal contrast must equal 2x its sum contrast
# AND have the right SIGN. variables = "perfil_genero" (bare string, no
# explicit level order) turned out to give Hombre-minus-Mujer, not Mujer-
# minus-Hombre -- the opposite of every other contrast in this project's
# convention (ref level first, comparison level second: level-minus-ref).
# Confirmed empirically (see conversation notes) and now avoided everywhere
# below via the explicit variables = list(perfil_genero = c("Hombre",
# "Mujer")) form. This second check would have caught it immediately.
m_chk <- M$set5$recommendation_probability
b_chk <- coef(m_chk)[["perfil_generoMujer"]]
c_chk <- avg_comparisons(m_chk, variables = list(perfil_genero = c("Hombre", "Mujer")), newdata = df)$estimate[1]
stopifnot(abs(abs(c_chk) - abs(2 * b_chk)) < 1e-6)
mg_chk <- avg_predictions(m_chk, by = "perfil_genero", newdata = df)
true_sign <- sign(mg_chk$estimate[mg_chk$perfil_genero == "Mujer"] - mg_chk$estimate[mg_chk$perfil_genero == "Hombre"])
stopifnot(sign(c_chk) == true_sign)
message("Contrast-coding check passed: |marginal contrast| = 2 x |coefficient|, sign matches Mujer-minus-Hombre")

# ===========================================================================
# 3. MARGINAL MEANS AND CONTRASTS
# ===========================================================================

# Conservative, uniform df choice (mirrors 05b_marginal_means.R /
# 05c_marginal_means_pooled.R): marginaleffects does not compute exact
# Satterthwaite df for compound linear combinations (revpairwise hypotheses,
# by-grouped contrasts), so every contrast drawn from a given model uses that
# model's minimum per-coefficient CR2 df. Passed explicitly to every call
# below -- without it, marginaleffects silently falls back to Inf/NA df for
# several of these call shapes, which breaks both the reported p-values'
# reference distribution and the low_df_flag guardrail.
model_df <- function(model) min(model$df)

# Significance in every dumbbell/contrast figure is shown two ways only:
# each point's own CI (does it cross zero / do two CIs overlap), and the
# numeric estimate + stars label below. No line/point weighting by
# significance tier (removed per feedback -- it duplicated what the CI
# already shows and implied a third visual channel that wasn't needed).
sig_stars <- function(p_adj) {
  case_when(p_adj < .001 ~ "***", p_adj < .01 ~ "**", p_adj < .05 ~ "*", TRUE ~ "")
}

# Precomputed as plain scalars (not referenced as `df$...` inside a later
# mutate()): avg_predictions()/avg_comparisons() return a `df` COLUMN once we
# pass df = model_df(m) below, which would shadow the global `df` dataset
# inside any chained mutate(n_obs = nrow(df), ...) and break with "$ operator
# is invalid for atomic vectors".
N_ALL <- nrow(df); K_ALL <- n_distinct(df$numericalId)

safe_map <- function(models, f) {
  imap_dfr(compact(models), function(m, y) {
    tryCatch(as_tibble(f(m)) %>% mutate(outcome = y), error = function(e) NULL)
  })
}

# Outcome -> construct family. Kept for reference and Figure 7's construct
# labels; BH correction itself (3g-3j, add_bh() below) is pooled across all
# tests in each contrast block, not grouped by family.
FAMILY <- c(
  recommendation_probability = "hiring",
  operational_costly_score = "cost",
  expected_benefits_use = "cost", expected_accommodations = "cost",
  idx_riesgo      = "risk",
  idx_crecimiento = "growth",
  idx_competencia = "competence",
  idx_investment  = "investment"
)

# --- 3a. Marginal means by gender (Figure 1) --------------------------------
mm_gender <- safe_map(M$set5, function(m)
  avg_predictions(m, by = "perfil_genero", newdata = df, df = model_df(m))) %>%
  mutate(n_obs = N_ALL, n_clusters = K_ALL) %>%
  select(outcome, perfil_genero, estimate, conf.low, conf.high, n_obs, n_clusters)

# --- 3b. Anchor contrast: fixed profile, single/childless/no plans ----------
anchor <- safe_map(M$set5, function(m)
  predictions(m,
    newdata = datagrid(
      perfil_genero = c("Hombre", "Mujer"),
      perfil_estado_civil = "soltero/a",
      perfil_parental = "actualmente no tiene hijos",
      perfil_plan_futuro = "no quiere",
      newdata = df),
    hypothesis = ~revpairwise,
    df = model_df(m))) %>%
  mutate(n_obs = N_ALL, n_clusters = K_ALL)

# --- 3c. Fertility horizon, childless women (Figure 2) ----------------------
df_cw <- df %>% filter(perfil_genero == "Mujer",
                       perfil_parental == "actualmente no tiene hijos")
N_CW <- nrow(df_cw); K_CW <- n_distinct(df_cw$numericalId)

mm_plan <- safe_map(M$set2b, function(m)
  avg_predictions(m, by = "perfil_plan_futuro", newdata = df_cw, df = model_df(m))) %>%
  mutate(n_obs = N_CW, n_clusters = K_CW) %>%
  select(outcome, perfil_plan_futuro, estimate, conf.low, conf.high, n_obs, n_clusters)

# --- 3d. Parental status x gender (Figure 3) --------------------------------
mm_parental <- safe_map(M$set2a, function(m)
  avg_predictions(m, by = c("perfil_parental", "perfil_genero"), newdata = df, df = model_df(m))) %>%
  mutate(n_obs = N_ALL, n_clusters = K_ALL) %>%
  select(outcome, perfil_parental, perfil_genero, estimate, conf.low, conf.high, n_obs, n_clusters)

# H9: two children vs childless, women minus men (difference-in-differences)
did_parental <- safe_map(M$set2a, function(m)
  avg_comparisons(m,
    variables = list(perfil_parental = c("actualmente no tiene hijos", "tiene dos hijos")),
    by = "perfil_genero", hypothesis = ~revpairwise, newdata = df, df = model_df(m))) %>%
  mutate(n_obs = N_ALL, n_clusters = K_ALL)

# H4: 2-year plans vs none, women minus men
# NOTE: newdata = df here (not the childless-women subset df_cw used for mm_plan)
# -- see conversation notes. This does not change the point estimate/CI/p-value
# for SET2b (no perfil_parental x perfil_plan_futuro interaction in that model,
# so the per-row comparison is constant across parental status), but n_obs/
# n_clusters below describe the full pooled sample, not "childless women" --
# keep that in mind if this cell count gets quoted on a slide as H4's N.
did_plan <- safe_map(M$set2b, function(m)
  avg_comparisons(m,
    variables = list(perfil_plan_futuro = c("no quiere", "en 2 anos")),
    by = "perfil_genero", hypothesis = ~revpairwise, newdata = df, df = model_df(m))) %>%
  mutate(n_obs = N_ALL, n_clusters = K_ALL)

# --- 3e. Occupational moderation (Figure 4) ---------------------------------
# H11 (SET5a, gender x occupation), H12 (SET5b, parental x occupation, two-
# children level -- the same headline level used for did_parental/H9 above),
# H13 (SET5c, plan_futuro x occupation, 2-year level -- same headline level
# as did_plan/H4). set5b/set5c were already being fit for this purpose; only
# their results were previously left unused, so H12/H13 restrict pooling
# support to the same one variable (gender) the deck's other figures don't
# turn on -- parental status and family plans (Figures 2-3) were untested.
mod_gender_occ <- safe_map(M$set5a, function(m)
  avg_comparisons(m, variables = list(perfil_genero = c("Hombre", "Mujer")),
                  by = "condicion_ocupacional", hypothesis = ~revpairwise,
                  newdata = df, df = model_df(m))) %>%
  mutate(n_obs = N_ALL, n_clusters = K_ALL)

mod_parental_occ <- safe_map(M$set5b, function(m)
  avg_comparisons(m,
    variables = list(perfil_parental = c("actualmente no tiene hijos", "tiene dos hijos")),
    by = "condicion_ocupacional", hypothesis = ~revpairwise, newdata = df, df = model_df(m))) %>%
  mutate(n_obs = N_ALL, n_clusters = K_ALL)

mod_plan_occ <- safe_map(M$set5c, function(m)
  avg_comparisons(m,
    variables = list(perfil_plan_futuro = c("no quiere", "en 2 anos")),
    by = "condicion_ocupacional", hypothesis = ~revpairwise, newdata = df, df = model_df(m))) %>%
  mutate(n_obs = N_ALL, n_clusters = K_ALL)

# --- 3g. Gender contrast (averaged marginal) -- matches mm_gender's estimand
# (Figure 1's dumbbell shows means; this is the actual women-minus-men test
# behind it, distinct from "anchor" which is a fixed hypothetical profile).
gender_contrast <- safe_map(M$set5, function(m)
  avg_comparisons(m, variables = list(perfil_genero = c("Hombre", "Mujer")), newdata = df, df = model_df(m))) %>%
  mutate(n_obs = N_ALL, n_clusters = K_ALL, family = FAMILY[outcome]) %>%
  mutate(p.adj = p.adjust(p.value, method = "BH"))  # pooled across all tests in this block, not by family

# --- 3h. Within-women fertility-horizon contrasts vs. "no plans" (Figure 2) -
plan_contrast_women <- safe_map(M$set2b, function(m)
  bind_rows(
    avg_comparisons(m, variables = list(perfil_plan_futuro = c("no quiere", "en 2 anos")),
                     newdata = df_cw, df = model_df(m)) %>% mutate(horizon = "en 2 anos"),
    avg_comparisons(m, variables = list(perfil_plan_futuro = c("no quiere", "en 5 anos")),
                     newdata = df_cw, df = model_df(m)) %>% mutate(horizon = "en 5 anos")
  )) %>%
  mutate(n_obs = N_CW, n_clusters = K_CW, family = FAMILY[outcome]) %>%
  mutate(p.adj = p.adjust(p.value, method = "BH"))  # pooled across all tests in this block, not by family

# --- 3i. Per-level gender gap in parental status (Figure 3 dumbbell) --------
parental_gender_gap <- safe_map(M$set2a, function(m)
  avg_comparisons(m, variables = list(perfil_genero = c("Hombre", "Mujer")), by = "perfil_parental", newdata = df, df = model_df(m))) %>%
  mutate(n_obs = N_ALL, n_clusters = K_ALL, family = FAMILY[outcome]) %>%
  mutate(p.adj = p.adjust(p.value, method = "BH"))  # pooled across all tests in this block, not by family

# --- 3j. Gendered fertility horizon, among the childless (new Figure 2b) ---
df_c <- df %>% filter(perfil_parental == "actualmente no tiene hijos")
N_C <- nrow(df_c); K_C <- n_distinct(df_c$numericalId)

mm_plan_gender <- safe_map(M$set2b, function(m)
  avg_predictions(m, by = c("perfil_plan_futuro", "perfil_genero"), newdata = df_c, df = model_df(m))) %>%
  mutate(n_obs = N_C, n_clusters = K_C) %>%
  select(outcome, perfil_plan_futuro, perfil_genero, estimate, conf.low, conf.high, n_obs, n_clusters)

plan_gender_gap <- safe_map(M$set2b, function(m)
  avg_comparisons(m, variables = list(perfil_genero = c("Hombre", "Mujer")), by = "perfil_plan_futuro", newdata = df_c, df = model_df(m))) %>%
  mutate(n_obs = N_C, n_clusters = K_C, family = FAMILY[outcome]) %>%
  mutate(p.adj = p.adjust(p.value, method = "BH"))  # pooled across all tests in this block, not by family

# --- 3k. Gendered effect of the 2yr-vs-no-plans contrast, childless (Fig 2d)
# The two per-gender contrasts (not means) and their difference (the DiD),
# both restricted to the childless. Uses SET2b, same model as Figures 2/2c.
plan_contrast_gender_c <- safe_map(M$set2b, function(m)
  avg_comparisons(m, variables = list(perfil_plan_futuro = c("no quiere", "en 2 anos")),
                   by = "perfil_genero", newdata = df_c, df = model_df(m))) %>%
  mutate(n_obs = N_C, n_clusters = K_C, family = FAMILY[outcome]) %>%
  mutate(p.adj = p.adjust(p.value, method = "BH"))

plan_did_c <- safe_map(M$set2b, function(m)
  avg_comparisons(m, variables = list(perfil_plan_futuro = c("no quiere", "en 2 anos")),
                   by = "perfil_genero", hypothesis = ~revpairwise, newdata = df_c, df = model_df(m))) %>%
  mutate(n_obs = N_C, n_clusters = K_C, family = FAMILY[outcome]) %>%
  mutate(p.adj = p.adjust(p.value, method = "BH"))

# --- 3l. Fertility horizon among MOTHERS (SET4b) -- Figures 2e/2f ----------
df_m  <- df %>% filter(perfil_parental %in% c("tiene un hijo", "tiene dos hijos"))
df_mw <- df_m %>% filter(perfil_genero == "Mujer")
N_M  <- nrow(df_m);  K_M  <- n_distinct(df_m$numericalId)
N_MW <- nrow(df_mw); K_MW <- n_distinct(df_mw$numericalId)

mm_plan_mothers <- safe_map(M$set4b, function(m)
  avg_predictions(m, by = "perfil_plan_futuro", newdata = df_mw, df = model_df(m))) %>%
  mutate(n_obs = N_MW, n_clusters = K_MW) %>%
  select(outcome, perfil_plan_futuro, estimate, conf.low, conf.high, n_obs, n_clusters)

plan_contrast_mothers <- safe_map(M$set4b, function(m)
  bind_rows(
    avg_comparisons(m, variables = list(perfil_plan_futuro = c("no quiere", "en 2 anos")),
                     newdata = df_mw, df = model_df(m)) %>% mutate(horizon = "en 2 anos"),
    avg_comparisons(m, variables = list(perfil_plan_futuro = c("no quiere", "en 5 anos")),
                     newdata = df_mw, df = model_df(m)) %>% mutate(horizon = "en 5 anos")
  )) %>%
  mutate(n_obs = N_MW, n_clusters = K_MW, family = FAMILY[outcome]) %>%
  mutate(p.adj = p.adjust(p.value, method = "BH"))

plan_contrast_gender_m <- safe_map(M$set4b, function(m)
  avg_comparisons(m, variables = list(perfil_plan_futuro = c("no quiere", "en 2 anos")),
                   by = "perfil_genero", newdata = df_m, df = model_df(m))) %>%
  mutate(n_obs = N_M, n_clusters = K_M, family = FAMILY[outcome]) %>%
  mutate(p.adj = p.adjust(p.value, method = "BH"))

# H6 (pre-registration): mothers, en-2-anos-vs-no-quiere, women minus men.
plan_did_m <- safe_map(M$set4b, function(m)
  avg_comparisons(m, variables = list(perfil_plan_futuro = c("no quiere", "en 2 anos")),
                   by = "perfil_genero", hypothesis = ~revpairwise, newdata = df_m, df = model_df(m))) %>%
  mutate(n_obs = N_M, n_clusters = K_M, family = FAMILY[outcome]) %>%
  mutate(p.adj = p.adjust(p.value, method = "BH"))

# --- 3l2. One child / two children vs. childless, AMONG WOMEN (general
# population -- not restricted by plan_futuro; that restricted version is
# 3m below, Figures 3b/3c). Uses SET2a (same model as Figure 3); mm_parental
# already has the per-gender means, so this only adds the within-women
# CONTRAST (with its own CI/p-value) for Figures 3e/3f.
df_w <- df %>% filter(perfil_genero == "Mujer")
N_W <- nrow(df_w); K_W <- n_distinct(df_w$numericalId)

parental_contrast_women <- safe_map(M$set2a, function(m)
  bind_rows(
    avg_comparisons(m, variables = list(perfil_parental = c("actualmente no tiene hijos", "tiene un hijo")),
                     newdata = df_w, df = model_df(m)) %>% mutate(level = "tiene un hijo"),
    avg_comparisons(m, variables = list(perfil_parental = c("actualmente no tiene hijos", "tiene dos hijos")),
                     newdata = df_w, df = model_df(m)) %>% mutate(level = "tiene dos hijos")
  )) %>%
  mutate(n_obs = N_W, n_clusters = K_W, family = FAMILY[outcome]) %>%
  mutate(p.adj = p.adjust(p.value, method = "BH"))

# --- 3m. Parental threshold among women with NO future fertility plans
# (SET4b) -- Figures 3b/3c/3d. Restricting SET2a's newdata here would be a
# no-op (SET2a has no plan_futuro interaction), so this needs SET4b too.
df_nq  <- df %>% filter(perfil_plan_futuro == "no quiere")
df_nqw <- df_nq %>% filter(perfil_genero == "Mujer")
N_NQ  <- nrow(df_nq);  K_NQ  <- n_distinct(df_nq$numericalId)
N_NQW <- nrow(df_nqw); K_NQW <- n_distinct(df_nqw$numericalId)

mm_parental_noplans_w <- safe_map(M$set4b, function(m)
  avg_predictions(m, by = "perfil_parental", newdata = df_nqw, df = model_df(m))) %>%
  mutate(n_obs = N_NQW, n_clusters = K_NQW) %>%
  select(outcome, perfil_parental, estimate, conf.low, conf.high, n_obs, n_clusters)

parental_contrast_noplans_w <- safe_map(M$set4b, function(m)
  bind_rows(
    avg_comparisons(m, variables = list(perfil_parental = c("actualmente no tiene hijos", "tiene un hijo")),
                     newdata = df_nqw, df = model_df(m)) %>% mutate(level = "tiene un hijo"),
    avg_comparisons(m, variables = list(perfil_parental = c("actualmente no tiene hijos", "tiene dos hijos")),
                     newdata = df_nqw, df = model_df(m)) %>% mutate(level = "tiene dos hijos")
  )) %>%
  mutate(n_obs = N_NQW, n_clusters = K_NQW, family = FAMILY[outcome]) %>%
  mutate(p.adj = p.adjust(p.value, method = "BH"))

# Two-children-vs-childless only (the gendered-effect figure, 3d, only asked
# for this level, not the one-child level).
parental_contrast_gender_noplans <- safe_map(M$set4b, function(m)
  avg_comparisons(m, variables = list(perfil_parental = c("actualmente no tiene hijos", "tiene dos hijos")),
                   by = "perfil_genero", newdata = df_nq, df = model_df(m))) %>%
  mutate(n_obs = N_NQ, n_clusters = K_NQ, family = FAMILY[outcome]) %>%
  mutate(p.adj = p.adjust(p.value, method = "BH"))

parental_did_noplans <- safe_map(M$set4b, function(m)
  avg_comparisons(m, variables = list(perfil_parental = c("actualmente no tiene hijos", "tiene dos hijos")),
                   by = "perfil_genero", hypothesis = ~revpairwise, newdata = df_nq, df = model_df(m))) %>%
  mutate(n_obs = N_NQ, n_clusters = K_NQ, family = FAMILY[outcome]) %>%
  mutate(p.adj = p.adjust(p.value, method = "BH"))

# --- 3f. BH adjustment, pooled across all tests in contrasts_all -----------
# Was grouped by outcome family (FAMILY, defined above); switched to a single
# pooled correction because most families now contain exactly one outcome
# (idx_riesgo/idx_crecimiento/idx_investment are singleton
# families post-07_Indices.R restructuring), so p.adjust() on a family of 1
# is a no-op -- family grouping was silently applying zero correction to most
# rows. `family` is still attached for reference/Figure 7's construct labels.
add_bh <- function(d) {
  d %>% mutate(family = FAMILY[outcome], low_df_flag = df < 30,
               p.adj = p.adjust(p.value, method = "BH"))
}

contrasts_all <- bind_rows(
  anchor          %>% mutate(hypothesis = "Anchor"),
  did_plan        %>% mutate(hypothesis = "H4"),
  did_parental    %>% mutate(hypothesis = "H9"),
  mod_gender_occ  %>% mutate(hypothesis = "H11"),
  mod_parental_occ %>% mutate(hypothesis = "H12"),
  mod_plan_occ    %>% mutate(hypothesis = "H13")
) %>% add_bh()

write_xlsx(contrasts_all,        file.path(out_dir, "conference_contrasts.xlsx"))
write_xlsx(mm_gender,            file.path(out_dir, "mm_gender.xlsx"))
write_xlsx(mm_plan,              file.path(out_dir, "mm_plan.xlsx"))
write_xlsx(mm_parental,          file.path(out_dir, "mm_parental.xlsx"))
write_xlsx(gender_contrast,      file.path(out_dir, "gender_contrast.xlsx"))
write_xlsx(plan_contrast_women,  file.path(out_dir, "plan_contrast_women.xlsx"))
write_xlsx(parental_gender_gap,  file.path(out_dir, "parental_gender_gap.xlsx"))
write_xlsx(mm_plan_gender,       file.path(out_dir, "mm_plan_gender.xlsx"))
write_xlsx(plan_gender_gap,      file.path(out_dir, "plan_gender_gap.xlsx"))
write_xlsx(plan_contrast_gender_c,          file.path(out_dir, "plan_contrast_gender_childless.xlsx"))
write_xlsx(plan_did_c,                      file.path(out_dir, "plan_did_childless.xlsx"))
write_xlsx(parental_contrast_women,         file.path(out_dir, "parental_contrast_women.xlsx"))
write_xlsx(mm_plan_mothers,                 file.path(out_dir, "mm_plan_mothers.xlsx"))
write_xlsx(plan_contrast_mothers,           file.path(out_dir, "plan_contrast_mothers.xlsx"))
write_xlsx(plan_contrast_gender_m,          file.path(out_dir, "plan_contrast_gender_mothers.xlsx"))
write_xlsx(plan_did_m,                      file.path(out_dir, "plan_did_mothers.xlsx"))
write_xlsx(mm_parental_noplans_w,           file.path(out_dir, "mm_parental_noplans_women.xlsx"))
write_xlsx(parental_contrast_noplans_w,     file.path(out_dir, "parental_contrast_noplans_women.xlsx"))
write_xlsx(parental_contrast_gender_noplans, file.path(out_dir, "parental_contrast_gender_noplans.xlsx"))
write_xlsx(parental_did_noplans,            file.path(out_dir, "parental_did_noplans.xlsx"))

# ===========================================================================
# 4. FIGURES
# ===========================================================================
# NOTE: the stochastic-dominance bootstrap (CDF-crossing markers) that used to
# live here has been removed -- it only ever fed Figure 1's old "n/6" margin
# text, and Figure 1 no longer shows that. Figure 6's CDF curves are computed
# directly from the empirical data (no bootstrap involved), so removing this
# didn't affect them.

# --- FIGURE 1: anchor dumbbell, with significance of the gender gap --------
f1_dat <- mm_gender %>%
  filter(outcome %in% outcomes_all) %>%
  mutate(block = factor(BLOCK_MAP[outcome], levels = BLOCK_LEVELS),
         label = LBL[outcome])

f1_seg <- f1_dat %>%
  distinct(outcome, label, block) %>%
  left_join(gender_contrast %>% select(outcome, gap = estimate, p.value, p.adj), by = "outcome") %>%
  mutate(sig_label = paste0(sprintf("%+.2f", gap), sig_stars(p.adj)))

fig1 <- ggplot(f1_dat, aes(y = fct_reorder(label, estimate))) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high, colour = perfil_genero),
                width = 0.4, orientation = "y", position = position_dodge(width = 0.5)) +
  geom_point(aes(x = estimate, colour = perfil_genero), size = 3.2,
             position = position_dodge(width = 0.5)) +
  geom_text(data = f1_seg, aes(y = label, x = 6.35, label = sig_label),
            size = 2.9, colour = PAL$ink, hjust = 1, inherit.aes = FALSE) +
  facet_grid(block ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_colour_manual(values = c(Hombre = PAL$teal, Mujer = PAL$accent),
                      labels = c(Hombre = "Men", Mujer = "Women")) +
  scale_x_continuous(limits = c(3, 6.6), breaks = 3:6) +
  labs(x = "Marginal mean (7-point scale)", y = NULL,
       caption = "Right margin: women-minus-men gap and its significance.\n* p.adj<.05  ** p.adj<.01  *** p.adj<.001 (Benjamini-Hochberg, pooled across all tests shown).") +
  theme_conf()

ggsave(file.path(out_dir, "fig1_anchor_dumbbell.png"), fig1,
       width = 10, height = 5.4, dpi = 300, bg = "white")

# --- FIGURE 1b: anchor gap only -- the gender CONTRAST, not both means -----
# Companion to Figure 1: same rows/blocks, but plots the women-minus-men
# difference itself (with its CI) against a zero reference line, so the sign
# and significance of the gap is the only thing on the page -- no mental
# subtraction of two dots required. Colour = direction (which group is
# higher, reusing the deck's teal=men/accent=women convention). Significance
# is shown only by the CI and the estimate+stars label -- no size/weight
# encoding. No fixed x-limits (auto scaled) -- unlike the mean-level
# figures, contrasts don't share one natural range across outcomes.
f1b_dat <- gender_contrast %>%
  filter(outcome %in% outcomes_all) %>%
  mutate(
    block     = factor(BLOCK_MAP[outcome], levels = BLOCK_LEVELS),
    label     = LBL[outcome],
    direction = if_else(estimate > 0, "Higher for women", "Higher for men")
  )

fig1b <- ggplot(f1b_dat, aes(x = estimate, y = fct_reorder(label, estimate))) +
  geom_vline(xintercept = 0, colour = PAL$soft, linewidth = 0.4) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2, colour = PAL$soft,
                orientation = "y") +
  geom_point(aes(colour = direction), size = 3.4) +
  facet_grid(block ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_colour_manual(values = c(`Higher for women` = PAL$accent, `Higher for men` = PAL$teal)) +
  labs(x = "Women-minus-men gap (marginal means, 7-point scale)", y = NULL,
       caption = "Benjamini-Hochberg, pooled across all tests shown.") +
  theme_conf()

ggsave(file.path(out_dir, "fig1b_anchor_contrast.png"), fig1b,
       width = 9, height = 5.4, dpi = 300, bg = "white")

# --- FIGURE 2: fertility horizon, childless women ---------------------------
# Faceted dumbbell rows (one series: women) against a "no plans" baseline.
# Continuous scale (3-6), same as Figures 1/2b/3, so magnitudes are directly
# comparable outcome to outcome and figure to figure. One CI upper bound
# (recommendation_probability's "no plans" baseline, 6.06) is clipped very
# slightly by the 6 ceiling here.
f2_dat <- mm_plan %>%
  filter(outcome %in% outcomes_all) %>%
  mutate(
    horizon = factor(as.character(perfil_plan_futuro),
                     levels = c("no quiere", "en 2 anos", "en 5 anos"),
                     labels = c("No plans", "In 2 years", "In 5 years")),
    label = factor(LBL[outcome], levels = LBL[outcomes_all])
  ) %>%
  arrange(label, horizon)

f2_sig <- plan_contrast_women %>%
  mutate(
    horizon = factor(recode(horizon, "en 2 anos" = "In 2 years", "en 5 anos" = "In 5 years"),
                     levels = c("In 2 years", "In 5 years")),
    label = factor(LBL[outcome], levels = LBL[outcomes_all]),
    sig_label = { s <- sig_stars(p.adj); if_else(s == "", "n.s.", s) }
  )

f2_baseline <- f2_dat %>% filter(horizon == "No plans") %>% select(outcome, label, x0 = estimate)

fig2 <- ggplot(f2_dat, aes(x = estimate, y = horizon)) +
  geom_vline(data = f2_baseline, aes(xintercept = x0),
             colour = PAL$rule, linewidth = 0.6, linetype = "dashed") +
  geom_path(aes(group = 1), colour = PAL$soft, linewidth = 0.5) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.15, colour = PAL$soft,
                orientation = "y") +
  geom_point(size = 2.8, colour = PAL$accent) +
  geom_text(data = f2_sig, aes(x = 6.35, y = horizon, label = sig_label),
            size = 2.9, colour = PAL$ink, hjust = 1) +
  facet_wrap(~ label, ncol = 5) +
  scale_x_continuous(limits = c(3, 6.6), breaks = 3:6) +
  labs(x = "Marginal mean (7-point scale)", y = NULL,
       subtitle = "Childless women. Dashed line: the \"no plans\" baseline. Right margin: significance vs. that baseline.",
       caption = "* p.adj<.05  ** p.adj<.01  *** p.adj<.001 (Benjamini-Hochberg, pooled across all tests shown); n.s. = not significant.") +
  theme_conf() +
  theme(panel.grid.major.y = element_blank())

ggsave(file.path(out_dir, "fig2_fertility_horizon.png"), fig2,
       width = 14, height = 6.4, dpi = 300, bg = "white")

# --- FIGURE 2b: fertility horizon, BY GENDER, among the childless ----------
# New figure: the gendered counterpart to Figure 2 -- does the horizon effect
# itself differ for men vs. women? Same faceted-dumbbell grammar as Figure 3
# (paired gender dots per row, thin slope lines for the within-gender trend,
# segment weight/label = significance of the gender gap at that row).
# Scale is 3-6, matching Figures 1/2 -- two CI bounds are clipped slightly
# here (idx_riesgo/Hombre/"no plans" low end at 2.96; recommendation_
# probability's "no plans" high end at 6.06 for both genders and its "2yr"
# high end at 6.00 for Hombre), all well under a tenth of a scale point.
# y_pos: manual dodge (not ggplot's position_dodge, which doesn't reliably
# keep geom_path aligned with dodged points/errorbars across facets) -- each
# gender's row of points is nudged +/-0.15 off the row's nominal position,
# and geom_path/errorbar/point all read the same offset column, so the
# trend line always lands exactly on its own gender's dots.
f2b_dat <- mm_plan_gender %>%
  filter(outcome %in% outcomes_all) %>%
  mutate(
    horizon = factor(as.character(perfil_plan_futuro),
                     levels = c("no quiere", "en 2 anos", "en 5 anos"),
                     labels = c("No plans", "In 2 years", "In 5 years")),
    label = factor(LBL[outcome], levels = LBL[outcomes_all]),
    y_pos = as.integer(horizon) + if_else(perfil_genero == "Mujer", 0.15, -0.15)
  ) %>%
  arrange(label, horizon)

f2b_gap <- plan_gender_gap %>%
  mutate(
    horizon = factor(as.character(perfil_plan_futuro),
                     levels = c("no quiere", "en 2 anos", "en 5 anos"),
                     labels = c("No plans", "In 2 years", "In 5 years")),
    label = factor(LBL[outcome], levels = LBL[outcomes_all])
  )

f2b_seg <- f2b_dat %>%
  distinct(outcome, label, horizon) %>%
  left_join(f2b_gap %>% select(outcome, horizon, p.value, p.adj), by = c("outcome", "horizon")) %>%
  mutate(sig_label = { s <- sig_stars(p.adj); if_else(s == "", "n.s.", s) })

fig2b <- ggplot() +
  geom_path(data = f2b_dat, aes(x = estimate, y = y_pos, group = perfil_genero, colour = perfil_genero),
            linewidth = 0.5, alpha = 0.5) +
  geom_errorbar(data = f2b_dat, aes(xmin = conf.low, xmax = conf.high, y = y_pos, colour = perfil_genero),
                width = 0.15, orientation = "y") +
  geom_point(data = f2b_dat, aes(x = estimate, y = y_pos, colour = perfil_genero), size = 2.6) +
  geom_text(data = f2b_seg, aes(x = 6.35, y = as.integer(horizon), label = sig_label),
            size = 2.9, colour = PAL$ink, hjust = 1) +
  facet_wrap(~ label, ncol = 5) +
  scale_colour_manual(values = c(Hombre = PAL$teal, Mujer = PAL$accent),
                      labels = c(Hombre = "Men", Mujer = "Women")) +
  scale_x_continuous(limits = c(3, 6.6), breaks = 3:6) +
  scale_y_continuous(breaks = seq_along(levels(f2b_dat$horizon)), labels = levels(f2b_dat$horizon),
                      limits = c(0.6, length(levels(f2b_dat$horizon)) + 0.4)) +
  labs(x = "Marginal mean (7-point scale)", y = NULL,
       subtitle = "Among the childless. Right margin: significance of the women-minus-men gap at that horizon.",
       caption = "* p.adj<.05  ** p.adj<.01  *** p.adj<.001 (Benjamini-Hochberg, pooled across all tests shown); n.s. = not significant.") +
  theme_conf() +
  theme(panel.grid.major.y = element_blank())

ggsave(file.path(out_dir, "fig2b_gendered_fertility_horizon.png"), fig2b,
       width = 14, height = 6.4, dpi = 300, bg = "white")

# --- FIGURE 2c: fertility horizon, 2 years vs no plans only (dumbbell) -----
# A clearer, focused alternative to Figure 2 for just the headline contrast:
# one row per outcome (block-grouped exactly like Figure 1), two dodged
# points with their own CIs (No plans vs In 2 years) -- same visual grammar
# as Figure 1, applied to the horizon comparison instead of the gender
# comparison. The 5-year horizon (which mostly tracks "no plans", see Figure
# 2) is deliberately left out to keep this one focused on the single
# contrast that matters most.
# Scale 3-6, same as Figure 2 (one CI upper bound -- recommendation_
# probability's "no plans" baseline, 6.06 -- clipped very slightly, as noted
# there).
f2c_dat <- mm_plan %>%
  filter(outcome %in% outcomes_all,
         perfil_plan_futuro %in% c("no quiere", "en 2 anos")) %>%
  mutate(
    block   = factor(BLOCK_MAP[outcome], levels = BLOCK_LEVELS),
    label   = LBL[outcome],
    horizon = factor(recode(as.character(perfil_plan_futuro),
                            "no quiere" = "No plans", "en 2 anos" = "In 2 years"),
                     levels = c("No plans", "In 2 years"))
  )

f2c_seg <- f2c_dat %>%
  distinct(outcome, label, block) %>%
  left_join(
    plan_contrast_women %>% filter(horizon == "en 2 anos") %>%
      select(outcome, gap = estimate, p.value, p.adj),
    by = "outcome"
  ) %>%
  mutate(sig_label = paste0(sprintf("%+.2f", gap), sig_stars(p.adj)))

fig2c <- ggplot(f2c_dat, aes(y = fct_reorder(label, estimate))) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high, colour = horizon),
                width = 0.4, orientation = "y", position = position_dodge(width = 0.5)) +
  geom_point(aes(x = estimate, colour = horizon), size = 3.2,
             position = position_dodge(width = 0.5)) +
  geom_text(data = f2c_seg, aes(y = label, x = 6.35, label = sig_label),
            size = 2.9, colour = PAL$ink, hjust = 1, inherit.aes = FALSE) +
  facet_grid(block ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_colour_manual(values = c(`No plans` = PAL$teal, `In 2 years` = PAL$accent)) +
  scale_x_continuous(limits = c(3, 6.6), breaks = 3:6) +
  labs(x = "Marginal mean (7-point scale)", y = NULL,
       caption = "Childless women. Right margin: 2-years-minus-no-plans gap and its significance.\n* p.adj<.05  ** p.adj<.01  *** p.adj<.001 (Benjamini-Hochberg, pooled across all tests shown).") +
  theme_conf()

ggsave(file.path(out_dir, "fig2c_fertility_2yr_dumbbell.png"), fig2c,
       width = 10, height = 5.4, dpi = 300, bg = "white")

# --- FIGURE 2c-b: fertility horizon, 2yr-vs-no-plans CONTRAST only ---------
# Companion to Figure 2c: same rows/blocks, plots the 2-years-minus-no-plans
# gap itself (with CI) against zero, instead of both means. No fixed
# x-limits, same reasoning as Figure 1b.
f2cb_dat <- plan_contrast_women %>%
  filter(outcome %in% outcomes_all, horizon == "en 2 anos") %>%
  mutate(
    block     = factor(BLOCK_MAP[outcome], levels = BLOCK_LEVELS),
    label     = LBL[outcome],
    direction = if_else(estimate > 0, "In 2 years higher", "No plans higher")
  )

fig2c_b <- ggplot(f2cb_dat, aes(x = estimate, y = fct_reorder(label, estimate))) +
  geom_vline(xintercept = 0, colour = PAL$soft, linewidth = 0.4) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2, colour = PAL$soft,
                orientation = "y") +
  geom_point(aes(colour = direction), size = 3.2) +
  facet_grid(block ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_colour_manual(values = c(`In 2 years higher` = PAL$accent, `No plans higher` = PAL$teal)) +
  labs(x = "2-years-minus-no-plans gap (marginal means, 7-point scale)", y = NULL,
       caption = "Childless women. Benjamini-Hochberg, pooled across all tests shown.") +
  theme_conf()

ggsave(file.path(out_dir, "fig2c_b_fertility_2yr_contrast.png"), fig2c_b,
       width = 9, height = 5.4, dpi = 300, bg = "white")

# --- FIGURE 2d: gendered effect of the 2yr-vs-no-plans contrast, childless -
# "Like Figure 2c" grammar, but the two dodged points (with their own CIs)
# are the WOMEN's and MEN's own 2yr-vs-no-plans contrast (not raw means) --
# shows directly whether the fertility-plan penalty itself differs by
# gender, among the childless. Right-margin label: the gender gap in that
# contrast (the DiD, SET2b restricted to df_c) and its significance. No
# fixed x-limits (a contrast, not a mean).
f2d_dat <- plan_contrast_gender_c %>%
  filter(outcome %in% outcomes_all) %>%
  mutate(block = factor(BLOCK_MAP[outcome], levels = BLOCK_LEVELS),
         label = LBL[outcome])

f2d_seg <- f2d_dat %>%
  distinct(outcome, label, block) %>%
  left_join(plan_did_c %>% select(outcome, gap = estimate, p.value, p.adj), by = "outcome") %>%
  mutate(sig_label = paste0(sprintf("%+.2f", gap), sig_stars(p.adj)))

f2d_label_x <- max(f2d_dat$conf.high, na.rm = TRUE) + 0.05

fig2d <- ggplot(f2d_dat, aes(y = fct_reorder(label, estimate))) +
  geom_vline(xintercept = 0, colour = PAL$rule, linewidth = 0.5, linetype = "dashed") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high, colour = perfil_genero),
                width = 0.4, orientation = "y", position = position_dodge(width = 0.5)) +
  geom_point(aes(x = estimate, colour = perfil_genero), size = 3.2,
             position = position_dodge(width = 0.5)) +
  geom_text(data = f2d_seg, aes(y = label, x = f2d_label_x, label = sig_label),
            size = 2.9, colour = PAL$ink, hjust = 0, inherit.aes = FALSE) +
  facet_grid(block ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_colour_manual(values = c(Hombre = PAL$teal, Mujer = PAL$accent),
                      labels = c(Hombre = "Men", Mujer = "Women")) +
  labs(x = "2-years-minus-no-plans contrast, by gender (7-point scale)", y = NULL,
       caption = "Childless. Right margin: women-minus-men gap in that contrast, and its significance.\n* p.adj<.05  ** p.adj<.01  *** p.adj<.001 (Benjamini-Hochberg, pooled across all tests shown).") +
  theme_conf()

ggsave(file.path(out_dir, "fig2d_gendered_2yr_contrast_childless.png"), fig2d,
       width = 11, height = 5.4, dpi = 300, bg = "white")

# --- FIGURE 2e: fertility horizon, 2 years vs no plans, AMONG MOTHERS ------
# Same grammar as Figure 2c, restricted to mothers (SET4b) instead of the
# childless (SET2b) -- shows whether stating near-term plans for another
# child reads differently once a candidate already has children.
f2e_dat <- mm_plan_mothers %>%
  filter(outcome %in% outcomes_all,
         perfil_plan_futuro %in% c("no quiere", "en 2 anos")) %>%
  mutate(
    block   = factor(BLOCK_MAP[outcome], levels = BLOCK_LEVELS),
    label   = LBL[outcome],
    horizon = factor(recode(as.character(perfil_plan_futuro),
                            "no quiere" = "No plans", "en 2 anos" = "In 2 years"),
                     levels = c("No plans", "In 2 years"))
  )

f2e_seg <- f2e_dat %>%
  distinct(outcome, label, block) %>%
  left_join(
    plan_contrast_mothers %>% filter(horizon == "en 2 anos") %>%
      select(outcome, gap = estimate, p.value, p.adj),
    by = "outcome"
  ) %>%
  mutate(sig_label = paste0(sprintf("%+.2f", gap), sig_stars(p.adj)))

fig2e <- ggplot(f2e_dat, aes(y = fct_reorder(label, estimate))) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high, colour = horizon),
                width = 0.4, orientation = "y", position = position_dodge(width = 0.5)) +
  geom_point(aes(x = estimate, colour = horizon), size = 3.2,
             position = position_dodge(width = 0.5)) +
  geom_text(data = f2e_seg, aes(y = label, x = 6.35, label = sig_label),
            size = 2.9, colour = PAL$ink, hjust = 1, inherit.aes = FALSE) +
  facet_grid(block ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_colour_manual(values = c(`No plans` = PAL$teal, `In 2 years` = PAL$accent)) +
  scale_x_continuous(limits = c(3, 6.6), breaks = 3:6) +
  labs(x = "Marginal mean (7-point scale)", y = NULL,
       caption = "Mothers. Right margin: 2-years-minus-no-plans gap and its significance.\n* p.adj<.05  ** p.adj<.01  *** p.adj<.001 (Benjamini-Hochberg, pooled across all tests shown).") +
  theme_conf()

ggsave(file.path(out_dir, "fig2e_fertility_2yr_mothers.png"), fig2e,
       width = 10, height = 5.4, dpi = 300, bg = "white")

# --- FIGURE 2f: gendered effect of the 2yr-vs-no-plans contrast, MOTHERS ---
# Same grammar as Figure 2d, restricted to mothers (SET4b). This is H6 from
# the pre-registration (mothers: en 2 anos vs no quiere, women minus men).
f2f_dat <- plan_contrast_gender_m %>%
  filter(outcome %in% outcomes_all) %>%
  mutate(block = factor(BLOCK_MAP[outcome], levels = BLOCK_LEVELS),
         label = LBL[outcome])

f2f_seg <- f2f_dat %>%
  distinct(outcome, label, block) %>%
  left_join(plan_did_m %>% select(outcome, gap = estimate, p.value, p.adj), by = "outcome") %>%
  mutate(sig_label = paste0(sprintf("%+.2f", gap), sig_stars(p.adj)))

f2f_label_x <- max(f2f_dat$conf.high, na.rm = TRUE) + 0.05

fig2f <- ggplot(f2f_dat, aes(y = fct_reorder(label, estimate))) +
  geom_vline(xintercept = 0, colour = PAL$rule, linewidth = 0.5, linetype = "dashed") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high, colour = perfil_genero),
                width = 0.4, orientation = "y", position = position_dodge(width = 0.5)) +
  geom_point(aes(x = estimate, colour = perfil_genero), size = 3.2,
             position = position_dodge(width = 0.5)) +
  geom_text(data = f2f_seg, aes(y = label, x = f2f_label_x, label = sig_label),
            size = 2.9, colour = PAL$ink, hjust = 0, inherit.aes = FALSE) +
  facet_grid(block ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_colour_manual(values = c(Hombre = PAL$teal, Mujer = PAL$accent),
                      labels = c(Hombre = "Men", Mujer = "Women")) +
  labs(x = "2-years-minus-no-plans contrast, by gender (7-point scale)", y = NULL,
       caption = "Mothers. Right margin: women-minus-men gap in that contrast, and its significance.\n* p.adj<.05  ** p.adj<.01  *** p.adj<.001 (Benjamini-Hochberg, pooled across all tests shown).") +
  theme_conf()

ggsave(file.path(out_dir, "fig2f_gendered_2yr_contrast_mothers.png"), fig2f,
       width = 11, height = 5.4, dpi = 300, bg = "white")

# --- FIGURE 3: parental threshold by gender ---------------------------------
# Same faceted grammar as Figure 2b (paired gender dots with their own CIs,
# thin slope lines for the within-gender trend across levels; manual y_pos
# dodge keeps the trend line aligned with its own gender's dots/errorbars).
# Scale is 3-6, matching Figures 1/2/2b -- fits with no clipping here.
f3_dat <- mm_parental %>%
  filter(outcome %in% outcomes_all) %>%
  mutate(
    parental = factor(as.character(perfil_parental),
                      levels = c("actualmente no tiene hijos", "tiene un hijo", "tiene dos hijos"),
                      labels = c("None", "One child", "Two children")),
    label = factor(LBL[outcome], levels = LBL[outcomes_all]),
    y_pos = as.integer(parental) + if_else(perfil_genero == "Mujer", 0.15, -0.15)
  ) %>%
  arrange(label, parental)

f3_gap <- parental_gender_gap %>%
  mutate(
    parental = factor(as.character(perfil_parental),
                      levels = c("actualmente no tiene hijos", "tiene un hijo", "tiene dos hijos"),
                      labels = c("None", "One child", "Two children")),
    label = factor(LBL[outcome], levels = LBL[outcomes_all])
  )

f3_seg <- f3_dat %>%
  distinct(outcome, label, parental) %>%
  left_join(f3_gap %>% select(outcome, parental, p.value, p.adj), by = c("outcome", "parental")) %>%
  mutate(sig_label = { s <- sig_stars(p.adj); if_else(s == "", "n.s.", s) })

fig3 <- ggplot() +
  geom_path(data = f3_dat, aes(x = estimate, y = y_pos, group = perfil_genero, colour = perfil_genero),
            linewidth = 0.5, alpha = 0.5) +
  geom_errorbar(data = f3_dat, aes(xmin = conf.low, xmax = conf.high, y = y_pos, colour = perfil_genero),
                width = 0.15, orientation = "y") +
  geom_point(data = f3_dat, aes(x = estimate, y = y_pos, colour = perfil_genero), size = 2.6) +
  geom_text(data = f3_seg, aes(x = 6.35, y = as.integer(parental), label = sig_label),
            size = 2.9, colour = PAL$ink, hjust = 1) +
  facet_wrap(~ label, ncol = 5) +
  scale_colour_manual(values = c(Hombre = PAL$teal, Mujer = PAL$accent),
                      labels = c(Hombre = "Men", Mujer = "Women")) +
  scale_x_continuous(limits = c(3, 6.6), breaks = 3:6) +
  scale_y_continuous(breaks = seq_along(levels(f3_dat$parental)), labels = levels(f3_dat$parental),
                      limits = c(0.6, length(levels(f3_dat$parental)) + 0.4)) +
  labs(x = "Marginal mean (7-point scale)", y = NULL,
       subtitle = "Right margin: significance of the women-minus-men gap at that parental level.",
       caption = "* p.adj<.05  ** p.adj<.01  *** p.adj<.001 (Benjamini-Hochberg, pooled across all tests shown); n.s. = not significant.") +
  theme_conf() +
  theme(panel.grid.major.y = element_blank())

ggsave(file.path(out_dir, "fig3_parental_threshold.png"), fig3,
       width = 14, height = 6.6, dpi = 300, bg = "white")

# --- FIGURE 3e: one child vs childless, AMONG WOMEN (general population) ---
# "Like Figure 2c" grammar: one row per outcome, two dodged points with their
# own CIs (Childless, One child). Uses SET2a (same model as Figure 3) on all
# women, no plan_futuro restriction -- the general-population counterpart to
# Figure 3b, which isolates the no-future-plans subgroup specifically.
f3e_dat <- mm_parental %>%
  filter(outcome %in% outcomes_all, perfil_genero == "Mujer",
         perfil_parental %in% c("actualmente no tiene hijos", "tiene un hijo")) %>%
  mutate(
    block    = factor(BLOCK_MAP[outcome], levels = BLOCK_LEVELS),
    label    = LBL[outcome],
    parental = factor(recode(as.character(perfil_parental),
                             "actualmente no tiene hijos" = "Childless", "tiene un hijo" = "One child"),
                      levels = c("Childless", "One child"))
  )

f3e_seg <- f3e_dat %>%
  distinct(outcome, label, block) %>%
  left_join(
    parental_contrast_women %>% filter(level == "tiene un hijo") %>%
      select(outcome, gap = estimate, p.value, p.adj),
    by = "outcome"
  ) %>%
  mutate(sig_label = paste0(sprintf("%+.2f", gap), sig_stars(p.adj)))

fig3e <- ggplot(f3e_dat, aes(y = fct_reorder(label, estimate))) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high, colour = parental),
                width = 0.4, orientation = "y", position = position_dodge(width = 0.5)) +
  geom_point(aes(x = estimate, colour = parental), size = 3.2,
             position = position_dodge(width = 0.5)) +
  geom_text(data = f3e_seg, aes(y = label, x = 6.35, label = sig_label),
            size = 2.9, colour = PAL$ink, hjust = 1, inherit.aes = FALSE) +
  facet_grid(block ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_colour_manual(values = c(Childless = PAL$teal, `One child` = PAL$accent)) +
  scale_x_continuous(limits = c(3, 6.6), breaks = 3:6) +
  labs(x = "Marginal mean (7-point scale)", y = NULL,
       caption = "Women. Right margin: one-child-minus-childless gap and its significance.\n* p.adj<.05  ** p.adj<.01  *** p.adj<.001 (Benjamini-Hochberg, pooled across all tests shown).") +
  theme_conf()

ggsave(file.path(out_dir, "fig3e_one_child_women.png"), fig3e,
       width = 10, height = 5.4, dpi = 300, bg = "white")

# --- FIGURE 3f: two children vs childless, AMONG WOMEN (general population) -
# Same grammar as Figure 3e, parental level = two children instead of one.
f3f_dat <- mm_parental %>%
  filter(outcome %in% outcomes_all, perfil_genero == "Mujer",
         perfil_parental %in% c("actualmente no tiene hijos", "tiene dos hijos")) %>%
  mutate(
    block    = factor(BLOCK_MAP[outcome], levels = BLOCK_LEVELS),
    label    = LBL[outcome],
    parental = factor(recode(as.character(perfil_parental),
                             "actualmente no tiene hijos" = "Childless", "tiene dos hijos" = "Two children"),
                      levels = c("Childless", "Two children"))
  )

f3f_seg <- f3f_dat %>%
  distinct(outcome, label, block) %>%
  left_join(
    parental_contrast_women %>% filter(level == "tiene dos hijos") %>%
      select(outcome, gap = estimate, p.value, p.adj),
    by = "outcome"
  ) %>%
  mutate(sig_label = paste0(sprintf("%+.2f", gap), sig_stars(p.adj)))

fig3f <- ggplot(f3f_dat, aes(y = fct_reorder(label, estimate))) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high, colour = parental),
                width = 0.4, orientation = "y", position = position_dodge(width = 0.5)) +
  geom_point(aes(x = estimate, colour = parental), size = 3.2,
             position = position_dodge(width = 0.5)) +
  geom_text(data = f3f_seg, aes(y = label, x = 6.35, label = sig_label),
            size = 2.9, colour = PAL$ink, hjust = 1, inherit.aes = FALSE) +
  facet_grid(block ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_colour_manual(values = c(Childless = PAL$teal, `Two children` = PAL$accent)) +
  scale_x_continuous(limits = c(3, 6.6), breaks = 3:6) +
  labs(x = "Marginal mean (7-point scale)", y = NULL,
       caption = "Women. Right margin: two-children-minus-childless gap and its significance.\n* p.adj<.05  ** p.adj<.01  *** p.adj<.001 (Benjamini-Hochberg, pooled across all tests shown).") +
  theme_conf()

ggsave(file.path(out_dir, "fig3f_two_children_women.png"), fig3f,
       width = 10, height = 5.4, dpi = 300, bg = "white")

# --- FIGURE 3b: one child vs childless, among women with no future plans ---
# "Like Figure 2c" grammar: one row per outcome, two points (Childless, One
# child) connected by a segment. Uses SET4b restricted to women who say they
# don't want (more) children -- isolates the parental-status penalty from
# any confound with stated future plans (SET2a, used for Figure 3, has no
# plan_futuro interaction, so it can't isolate this subgroup).
f3b_dat <- mm_parental_noplans_w %>%
  filter(outcome %in% outcomes_all,
         perfil_parental %in% c("actualmente no tiene hijos", "tiene un hijo")) %>%
  mutate(
    block    = factor(BLOCK_MAP[outcome], levels = BLOCK_LEVELS),
    label    = LBL[outcome],
    parental = factor(recode(as.character(perfil_parental),
                             "actualmente no tiene hijos" = "Childless", "tiene un hijo" = "One child"),
                      levels = c("Childless", "One child"))
  )

f3b_seg <- f3b_dat %>%
  distinct(outcome, label, block) %>%
  left_join(
    parental_contrast_noplans_w %>% filter(level == "tiene un hijo") %>%
      select(outcome, gap = estimate, p.value, p.adj),
    by = "outcome"
  ) %>%
  mutate(sig_label = paste0(sprintf("%+.2f", gap), sig_stars(p.adj)))

fig3b <- ggplot(f3b_dat, aes(y = fct_reorder(label, estimate))) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high, colour = parental),
                width = 0.4, orientation = "y", position = position_dodge(width = 0.5)) +
  geom_point(aes(x = estimate, colour = parental), size = 3.2,
             position = position_dodge(width = 0.5)) +
  geom_text(data = f3b_seg, aes(y = label, x = 6.35, label = sig_label),
            size = 2.9, colour = PAL$ink, hjust = 1, inherit.aes = FALSE) +
  facet_grid(block ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_colour_manual(values = c(Childless = PAL$teal, `One child` = PAL$accent)) +
  scale_x_continuous(limits = c(3, 6.6), breaks = 3:6) +
  labs(x = "Marginal mean (7-point scale)", y = NULL,
       caption = "Women with no future fertility plans. Right margin: one-child-minus-childless gap and its significance.\n* p.adj<.05  ** p.adj<.01  *** p.adj<.001 (Benjamini-Hochberg, pooled across all tests shown).") +
  theme_conf()

ggsave(file.path(out_dir, "fig3b_one_child_noplans.png"), fig3b,
       width = 10, height = 5.4, dpi = 300, bg = "white")

# --- FIGURE 3c: two children vs childless, among women with no future plans -
# Same grammar as Figure 3b, parental level = two children instead of one.
f3c_dat <- mm_parental_noplans_w %>%
  filter(outcome %in% outcomes_all,
         perfil_parental %in% c("actualmente no tiene hijos", "tiene dos hijos")) %>%
  mutate(
    block    = factor(BLOCK_MAP[outcome], levels = BLOCK_LEVELS),
    label    = LBL[outcome],
    parental = factor(recode(as.character(perfil_parental),
                             "actualmente no tiene hijos" = "Childless", "tiene dos hijos" = "Two children"),
                      levels = c("Childless", "Two children"))
  )

f3c_seg <- f3c_dat %>%
  distinct(outcome, label, block) %>%
  left_join(
    parental_contrast_noplans_w %>% filter(level == "tiene dos hijos") %>%
      select(outcome, gap = estimate, p.value, p.adj),
    by = "outcome"
  ) %>%
  mutate(sig_label = paste0(sprintf("%+.2f", gap), sig_stars(p.adj)))

fig3c <- ggplot(f3c_dat, aes(y = fct_reorder(label, estimate))) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high, colour = parental),
                width = 0.4, orientation = "y", position = position_dodge(width = 0.5)) +
  geom_point(aes(x = estimate, colour = parental), size = 3.2,
             position = position_dodge(width = 0.5)) +
  geom_text(data = f3c_seg, aes(y = label, x = 6.35, label = sig_label),
            size = 2.9, colour = PAL$ink, hjust = 1, inherit.aes = FALSE) +
  facet_grid(block ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_colour_manual(values = c(Childless = PAL$teal, `Two children` = PAL$accent)) +
  scale_x_continuous(limits = c(3, 6.6), breaks = 3:6) +
  labs(x = "Marginal mean (7-point scale)", y = NULL,
       caption = "Women with no future fertility plans. Right margin: two-children-minus-childless gap and its significance.\n* p.adj<.05  ** p.adj<.01  *** p.adj<.001 (Benjamini-Hochberg, pooled across all tests shown).") +
  theme_conf()

ggsave(file.path(out_dir, "fig3c_two_children_noplans.png"), fig3c,
       width = 10, height = 5.4, dpi = 300, bg = "white")

# --- FIGURE 3d: gendered effect of the two-children-vs-childless contrast,
# among people with no future fertility plans ------------------------------
# Same grammar as Figure 2d/2f: two dodged per-gender contrasts, each with
# its own CI. Right-margin label: the gender gap in that contrast (the DiD,
# SET4b restricted to df_nq) and its significance. No fixed x-limits (a
# contrast, not a mean).
f3d_dat <- parental_contrast_gender_noplans %>%
  filter(outcome %in% outcomes_all) %>%
  mutate(block = factor(BLOCK_MAP[outcome], levels = BLOCK_LEVELS),
         label = LBL[outcome])

f3d_seg <- f3d_dat %>%
  distinct(outcome, label, block) %>%
  left_join(parental_did_noplans %>% select(outcome, gap = estimate, p.value, p.adj), by = "outcome") %>%
  mutate(sig_label = paste0(sprintf("%+.2f", gap), sig_stars(p.adj)))

f3d_label_x <- max(f3d_dat$conf.high, na.rm = TRUE) + 0.05

fig3d <- ggplot(f3d_dat, aes(y = fct_reorder(label, estimate))) +
  geom_vline(xintercept = 0, colour = PAL$rule, linewidth = 0.5, linetype = "dashed") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high, colour = perfil_genero),
                width = 0.4, orientation = "y", position = position_dodge(width = 0.5)) +
  geom_point(aes(x = estimate, colour = perfil_genero), size = 3.2,
             position = position_dodge(width = 0.5)) +
  geom_text(data = f3d_seg, aes(y = label, x = f3d_label_x, label = sig_label),
            size = 2.9, colour = PAL$ink, hjust = 0, inherit.aes = FALSE) +
  facet_grid(block ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_colour_manual(values = c(Hombre = PAL$teal, Mujer = PAL$accent),
                      labels = c(Hombre = "Men", Mujer = "Women")) +
  labs(x = "Two-children-minus-childless contrast, by gender (7-point scale)", y = NULL,
       caption = "People with no future fertility plans. Right margin: women-minus-men gap in that contrast, and its significance.\n* p.adj<.05  ** p.adj<.01  *** p.adj<.001 (Benjamini-Hochberg, pooled across all tests shown).") +
  theme_conf()

ggsave(file.path(out_dir, "fig3d_gendered_two_children_noplans.png"), fig3d,
       width = 11, height = 5.4, dpi = 300, bg = "white")

# --- FIGURE 4: occupational moderation, all three manipulated variables -----
# NOTE: no equivalence band. A CI lying fully within some band width would
# support "moderation is negligible"; every one of the 19 H11 CIs (and most
# H12/H13 ones) extends past a naive +/-0.14 band, so the honest claim is only
# "no CI excludes zero" (fails to reject H0: no moderation), not "moderation
# is excluded". If an equivalence claim is wanted later, the band needs a
# principled SESOI (e.g. half the smallest effect the design is powered to
# detect), not a width picked to look tidy on the axis.
f4_dat <- bind_rows(
  mod_gender_occ   %>% mutate(test = "H11: Gender x occupation"),
  mod_parental_occ %>% mutate(test = "H12: Parental (2 children) x occupation"),
  mod_plan_occ     %>% mutate(test = "H13: Plan (2 years) x occupation")
) %>%
  filter(outcome %in% outcomes_all) %>%
  mutate(label = LBL[outcome],
         test = factor(test, levels = c("H11: Gender x occupation",
                                        "H12: Parental (2 children) x occupation",
                                        "H13: Plan (2 years) x occupation")))

fig4 <- ggplot(f4_dat, aes(x = estimate, y = fct_reorder(label, estimate))) +
  geom_vline(xintercept = 0, colour = PAL$soft, linewidth = 0.4) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, colour = PAL$soft) +
  geom_point(size = 2.4, colour = PAL$teal) +
  facet_wrap(~ test, nrow = 1) +
  labs(x = "Contrast, technical minus professional", y = NULL,
       subtitle = "No interval excludes zero: no evidence occupation moderates the gender, parental, or plan effects.",
       caption = "SET 5a/5b/5c. Absence of a significant interaction is not evidence of equivalence (no pre-specified SESOI).") +
  theme_conf()

ggsave(file.path(out_dir, "fig4_null_moderation.png"), fig4,
       width = 14, height = 6.2, dpi = 300, bg = "white")

# --- FIGURE 5: response distributions ---------------------------------------
# Restricted to outcomes_raw (the non-indexed items): indices are continuous
# means (e.g. 3.67), so they can't be shown as a clean 7-segment stacked bar
# the way a genuine 1-7 Likert item can -- their distributions are
# represented elsewhere (Figures 1-4) via the index itself.
f5_dat <- df %>%
  select(all_of(outcomes_raw)) %>%
  pivot_longer(everything(), names_to = "outcome", values_to = "v") %>%
  filter(!is.na(v)) %>%
  count(outcome, v) %>%
  group_by(outcome) %>% mutate(pct = n / sum(n)) %>% ungroup() %>%
  mutate(label = LBL[outcome])

ord <- f5_dat %>% filter(v == 7) %>% arrange(pct) %>% pull(label)

fig5 <- ggplot(f5_dat, aes(x = pct, y = factor(label, levels = ord), fill = factor(v))) +
  geom_col(width = 0.72) +
  scale_fill_brewer(palette = "GnBu", name = "Response") +
  scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
  labs(x = NULL, y = NULL,
       subtitle = "Response distributions for the outcomes that stay disaggregated (not folded into a preregistered index).") +
  theme_conf() +
  theme(panel.grid.major.y = element_blank(), legend.position = "bottom")

ggsave(file.path(out_dir, "fig5_response_distributions.png"), fig5,
       width = 9, height = 5.4, dpi = 300, bg = "white")

# --- FIGURE 6: dominance, CDF overlay --------------------------------------
f6_dat <- df %>%
  select(perfil_genero, all_of(outcomes_all)) %>%
  pivot_longer(-perfil_genero, names_to = "outcome", values_to = "v") %>%
  filter(!is.na(v)) %>%
  group_by(outcome, perfil_genero, v) %>% summarise(n = n(), .groups = "drop") %>%
  group_by(outcome, perfil_genero) %>%
  arrange(v) %>% mutate(cdf = cumsum(n) / sum(n)) %>% ungroup() %>%
  mutate(label = factor(LBL[outcome], levels = LBL[outcomes_all]))

fig6 <- ggplot(f6_dat, aes(v, cdf, colour = perfil_genero)) +
  geom_step(linewidth = 0.9, direction = "hv") +
  geom_point(size = 1.6) +
  facet_wrap(~ label, nrow = 2) +
  scale_colour_manual(values = c(Hombre = PAL$teal, Mujer = PAL$accent),
                      labels = c(Hombre = "Men", Mujer = "Women")) +
  scale_x_continuous(breaks = 1:7) +
  labs(x = "Response (indices: continuous mean of their component items)", y = "Cumulative proportion",
       subtitle = "Curves that never cross indicate a ranking that holds under any monotonic transformation of the scale.") +
  theme_conf()

ggsave(file.path(out_dir, "fig6_dominance_cdf.png"), fig6,
       width = 15, height = 6.4, dpi = 300, bg = "white")

# --- FIGURE 7: summary matrix ----------------------------------------------
# Evidence strength per hypothesis x construct. Extend `rows` as further
# hypotheses are added; values: "bh", "raw", "null".

f7_dat <- contrasts_all %>%
  filter(hypothesis %in% c("Anchor", "H4", "H9")) %>%
  mutate(
    construct = recode(FAMILY[outcome],
                       hiring = "Hiring", cost = "Cost", risk = "Risk",
                       growth = "Growth", competence = "Competence",
                       investment = "Investment"),
    strength = case_when(p.adj < .05 ~ "Survives BH",
                         p.value < .05 ~ "Raw p < .05",
                         TRUE ~ "Null")
  ) %>%
  group_by(hypothesis, construct) %>%
  summarise(strength = factor(
    if (any(strength == "Survives BH")) "Survives BH"
    else if (any(strength == "Raw p < .05")) "Raw p < .05" else "Null",
    levels = c("Null", "Raw p < .05", "Survives BH")), .groups = "drop")

fig7 <- ggplot(f7_dat, aes(construct, fct_rev(hypothesis))) +
  geom_point(aes(size = strength, colour = strength)) +
  scale_size_manual(values = c(Null = 2.2, `Raw p < .05` = 4, `Survives BH` = 5.6)) +
  scale_colour_manual(values = c(Null = PAL$rule, `Raw p < .05` = PAL$soft,
                                 `Survives BH` = PAL$accent)) +
  labs(x = NULL, y = NULL,
       subtitle = "Evidence by hypothesis and construct. Benjamini-Hochberg pooled across all tests shown.") +
  theme_conf() +
  theme(panel.grid.major = element_line(colour = PAL$rule, linewidth = 0.25))

ggsave(file.path(out_dir, "fig7_summary_matrix.png"), fig7,
       width = 8, height = 4.2, dpi = 300, bg = "white")

# --- FIGURE 8: outcome x contrast heatmap, all 8 outcomes -------------------
# Full-grid summary of every headline contrast computed in this script: rows
# = the 8 analyzed outcomes, columns = every distinct contrast (grouped into
# main effects / gendered effects (DiD) / occupational moderation). Fill =
# estimate on a diverging scale (hue = direction, intensity = magnitude);
# black outline + stars = significant. p.adj here is a FRESH pool across all
# 8 x 14 = 112 cells shown in this grid specifically -- not reusing any
# single contrast-block's own separately-pooled p.adj computed earlier in
# the script, since displaying this many different-origin tests side by
# side calls for one consistent correction scope across the whole grid.
heat_src <- list(
  list(name = "Gender gap (Anchor, averaged)",                group = "Main effect",
       data = gender_contrast),
  list(name = "2yr vs No plans (Childless, Women)",           group = "Main effect",
       data = plan_contrast_women %>% filter(horizon == "en 2 anos")),
  list(name = "2yr vs No plans (Mothers)",                    group = "Main effect",
       data = plan_contrast_mothers %>% filter(horizon == "en 2 anos")),
  list(name = "One child vs Childless (Women)",               group = "Main effect",
       data = parental_contrast_women %>% filter(level == "tiene un hijo")),
  list(name = "Two children vs Childless (Women)",            group = "Main effect",
       data = parental_contrast_women %>% filter(level == "tiene dos hijos")),
  list(name = "One child vs Childless (Women, no plans)",     group = "Main effect",
       data = parental_contrast_noplans_w %>% filter(level == "tiene un hijo")),
  list(name = "Two children vs Childless (Women, no plans)",  group = "Main effect",
       data = parental_contrast_noplans_w %>% filter(level == "tiene dos hijos")),
  list(name = "Gendered gap: 2yr vs No plans (Childless)",    group = "Gendered effect",
       data = plan_did_c),
  list(name = "Gendered gap: 2yr vs No plans (Mothers)",      group = "Gendered effect",
       data = plan_did_m),
  list(name = "Gendered gap: Two children (Women)",           group = "Gendered effect",
       data = did_parental),
  list(name = "Gendered gap: Two children (No plans)",        group = "Gendered effect",
       data = parental_did_noplans)
)
heat_names <- map_chr(heat_src, "name")

heat_dat <- map_dfr(heat_src, function(x)
  x$data %>% filter(outcome %in% outcomes_all) %>%
    transmute(outcome, estimate, p.value, contrast = x$name, group = x$group)) %>%
  mutate(p.adj = p.adjust(p.value, method = "BH")) %>%
  mutate(
    label    = factor(LBL[outcome], levels = rev(LBL[outcomes_all])),
    contrast = factor(contrast, levels = heat_names),
    group    = factor(group, levels = c("Main effect", "Gendered effect")),
    stars    = sig_stars(p.adj),
    sig      = p.adj < .05
  )

heat_lim <- max(abs(heat_dat$estimate), na.rm = TRUE)

fig8 <- ggplot(heat_dat, aes(x = contrast, y = label, fill = estimate)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_tile(data = filter(heat_dat, sig), fill = NA, colour = PAL$ink, linewidth = 1) +
  geom_text(aes(label = stars), colour = PAL$ink, size = 3.2, fontface = "bold") +
  facet_grid(~ group, scales = "free_x", space = "free_x") +
  scale_fill_gradient2(low = PAL$teal, mid = "white", high = PAL$accent,
                       midpoint = 0, limits = c(-heat_lim, heat_lim), name = "Estimate") +
  labs(x = NULL, y = NULL,
       subtitle = "Every headline contrast x outcome. Outline + stars: p.adj<.05.\n* p.adj<.05  ** p.adj<.01  *** p.adj<.001 (Benjamini-Hochberg, pooled across all 112 cells shown here).") +
  theme_conf() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 8),
        panel.grid = element_blank(),
        strip.text = element_text(size = 9))

ggsave(file.path(out_dir, "fig8_heatmap_summary.png"), fig8,
       width = 16, height = 6.5, dpi = 300, bg = "white")

# ===========================================================================
# 5. POOLING JUSTIFICATION  - cluster-robust omnibus Wald test
# ===========================================================================
# Joint test that condicion_ocupacional does not interact with any of the
# three manipulated variables (gender, parental, plan_futuro) at once.
#
# Previously this compared two plain lm() fits with anova(), a classical
# F-test that assumes independent errors. That's inconsistent with the CR2
# clustering used for every other number in this project (see 04_models.R's
# "bouncing beta problem" comment: nd = 2 profiles/respondent means residuals
# are correlated within respondent, which is exactly what motivated CR2 in
# the first place) -- so the classical test's p-values weren't trustworthy
# on the same terms as the rest of the pipeline, even though they happened to
# tell the same qualitative story here.
#
# Fixed as a manual cluster-robust Wald test (no new package dependency):
# fit the interacted model with lm_robust(se_type = "CR2"), take the CR2
# sandwich vcov block for just the added interaction coefficients, and test
# whether they're jointly zero. Reported as an F-statistic using this
# script's existing conservative df convention (model_df(): the interacted
# model's minimum per-coefficient CR2 df) for the denominator, matching how
# every other contrast's inference is handled throughout this script.

rhs_int <- paste(
  "(perfil_genero + perfil_parental + perfil_plan_futuro) * condicion_ocupacional +",
  "perfil_estado_civil + perfil_exp + perfil_desempeno +", controls
)

cluster_wald_test <- function(y, rhs_reduced, rhs_full, data) {
  m_no  <- lm_robust(as.formula(paste(y, "~", rhs_reduced)), data = data,
                      clusters = data$numericalId, se_type = "CR2")
  m_int <- lm_robust(as.formula(paste(y, "~", rhs_full)), data = data,
                      clusters = data$numericalId, se_type = "CR2")

  # Coefficients present in the interacted model but not the reduced one --
  # i.e. exactly the *:condicion_ocupacional interaction terms.
  added <- setdiff(names(coef(m_int)), names(coef(m_no)))
  stopifnot(length(added) > 0, all(grepl(":", added)))

  b   <- coef(m_int)[added]
  V   <- vcov(m_int)[added, added]
  df1 <- length(added)
  df2 <- model_df(m_int)

  W     <- as.numeric(t(b) %*% solve(V) %*% b)
  Fstat <- W / df1
  p     <- pf(Fstat, df1, df2, lower.tail = FALSE)

  tibble(outcome = y, df1 = df1, df2 = df2, F = Fstat, p = p)
}

omnibus <- map_dfr(c("recommendation_probability", outcomes_idx),
                    cluster_wald_test, rhs_reduced = rhs_main, rhs_full = rhs_int, data = df)

print(omnibus)
write_xlsx(omnibus, file.path(out_dir, "omnibus_pooling_test.xlsx"))

message("\nDone. Figures and tables written to: ", out_dir)

# ============================================================================
# NOT INCLUDED - see next-steps slide
#   - ordered probit for the seven outcomes above 20% at ceiling
#   - stacked model testing the conduct/evaluation dissociation directly
#   - attention-check sensitivity analysis (full vs restricted sample)
#   - unadjusted marginal means alongside the model-adjusted estimates
#   - balance check on evaluator characteristics across occupational arms
#   - secondary moderation by evaluator characteristics
# ============================================================================
