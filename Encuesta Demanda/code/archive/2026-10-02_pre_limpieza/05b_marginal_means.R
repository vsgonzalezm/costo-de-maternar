# ============================================================================
# 05b - Predicted marginal means and contrasts (AsPredicted #297542, item 5)
# Author: Tania Hutt
# Date: agosto 2026
# Purpose: Produces the predicted marginal means / contrasts committed to in
#          the pre-registration (Preregistration/AsPredicted #297542.pdf),
#          using the cluster-robust models fitted in 04_analysis_cluster_robust_models.R
#          (and their composite-index counterparts from 07_Indices.R).
#
# Ground truth for H1-H16 used throughout this script is the pre-registration
# PDF, not the paraphrase in SPEC_05b_marginal_means.md. Where the PDF leaves a
# specific contrast unnumbered (which of the six H1/H2/H3-derived DiDs is H4 vs
# H5 vs ... H9; which parental level anchors the H3/H8/H9/H12/H14 console-summary
# row), the choice was confirmed with the analyst before writing this script:
#   - H4=childless 2yr-vs-none, H5=childless 5yr-vs-none,
#     H6=mothers 2yr-vs-none,   H7=mothers 5yr-vs-none,
#     H8=1-child-vs-childless,  H9=2-child-vs-childless
#   - Headline parental contrast for H3/H8/H9/H12/H14 is 2-children-vs-childless
#     (both levels are still reported in the full CSV either way).
#   - Full sample only (no attention-check-restricted re-run) at this pass.
# ============================================================================

library(tidyverse)
library(here)
library(estimatr)
library(marginaleffects)
library(modelsummary)

project_dir <- if (dir.exists(here("Encuesta Demanda", "Data"))) {
  here("Encuesta Demanda")
} else {
  here()
}

models_dir     <- file.path(project_dir, "Outputs", "models")
tables_dir     <- file.path(project_dir, "Outputs", "tables")
mm_tables_dir  <- file.path(tables_dir, "marginal_means")
dir.create(mm_tables_dir, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# 1. REBUILD df / df_prof / df_tec IDENTICALLY TO 04_analysis_cluster_robust_models.R
# ==============================================================================
# lm_robust objects do not retain the model frame, so marginal means need the
# original data, factored identically to the fitted models. Copied verbatim
# from 04_analysis_cluster_robust_models.R (do not hand-edit independently).

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

# Any difference in level order silently changes the contrast matrix and
# produces wrong numbers with no error (this is exactly the años/anos bug
# that already bit 04_analysis_cluster_robust_models.R once).
stopifnot(identical(levels(df$perfil_genero),         c("Mujer", "Hombre")))
stopifnot(identical(levels(df$perfil_estado_civil),   c("casado/a", "soltero/a")))
stopifnot(identical(levels(df$perfil_parental),       c("tiene un hijo", "tiene dos hijos", "actualmente no tiene hijos")))
stopifnot(identical(levels(df$perfil_plan_futuro),    c("en 2 anos", "en 5 anos", "no quiere")))
stopifnot(identical(levels(df$perfil_exp),            c("7 años", "8 años", "6 años")))
stopifnot(identical(levels(df$condicion_ocupacional), c("tecnico", "profesional")))

df_prof <- df %>% filter(VISITAS_1_BRAZO == "profesional")
df_tec  <- df %>% filter(VISITAS_1_BRAZO == "tecnico")

# ==============================================================================
# 2. OUTCOMES: 16 individual items + the 3 composite indices (costo stays
#    individual: alpha = .631, below the .70 threshold; see 07_Indices.R)
# ==============================================================================

idx_outcomes <- c("idx_riesgo", "idx_crecimiento", "idx_competencia")

all_outcomes <- c(
  "recommendation_probability",
  "ambitious_score", "trustworthy_score", "competent_score", "risky_continuity_score",
  "salary_costly_score", "operational_costly_score", "long_term_commitment_score",
  "promising_score", "good_investment_score",
  "expected_late_absence", "expected_benefits_use", "expected_accommodations",
  "expected_quit_work", "expected_new_challenges", "expected_promotion",
  idx_outcomes
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
  "idx_riesgo" = "Perceived risk (index)", "idx_crecimiento" = "Growth potential (index)",
  "idx_competencia" = "Competence (index)"
)

# Families for the BH correction (spec section 6), mirroring the four
# theoretical constructs from 07_Indices.R plus hiring/investment as their
# own (single-item) families.
outcome_family <- c(
  recommendation_probability = "hiring",
  salary_costly_score = "cost", operational_costly_score = "cost",
  expected_benefits_use = "cost", expected_accommodations = "cost",
  risky_continuity_score = "risk", expected_late_absence = "risk",
  expected_quit_work = "risk", idx_riesgo = "risk",
  ambitious_score = "growth", promising_score = "growth",
  expected_new_challenges = "growth", expected_promotion = "growth", idx_crecimiento = "growth",
  trustworthy_score = "competence", competent_score = "competence",
  long_term_commitment_score = "competence", idx_competencia = "competence",
  good_investment_score = "investment"
)

# ==============================================================================
# 3. MODEL LOADING
# ==============================================================================
# purrr::compact() drops the NULL entries left by run_models()'s tryCatch.

load_model_list <- function(set_id, suffix) {
  regular <- compact(readRDS(file.path(models_dir, paste0(set_id, "_models", suffix, ".rds"))))
  idx     <- compact(readRDS(file.path(models_dir, paste0(set_id, "_idx_models", suffix, ".rds"))))
  c(regular, idx[intersect(idx_outcomes, names(idx))])
}

get_model <- function(model_list, outcome) model_list[[outcome]]

# ==============================================================================
# 4. SANITY CHECKS (spec section 3 and section 8) - run before trusting anything
# ==============================================================================

cat("=== Sanity checks ===\n")

m_check <- get_model(load_model_list("set1", "_prof"), "recommendation_probability")

# (a) CR2 vcov flows into marginaleffects automatically.
vcov_ok <- isTRUE(all.equal(vcov(m_check), marginaleffects::get_vcov(m_check)))
cat("CR2 vcov matches marginaleffects::get_vcov():", vcov_ok, "\n")
stopifnot(vcov_ok)

# (b) avg_comparisons on a two-level factor must equal 2x the sum-contrast coefficient.
ac_gender <- avg_comparisons(m_check, variables = "perfil_genero", newdata = df_prof)
ratio <- abs(ac_gender$estimate) / abs(coef(m_check)["perfil_generoMujer"])
cat("avg_comparisons(gender) / sum-contrast coefficient (expect ~2):", round(ratio, 3), "\n")
stopifnot(abs(ratio - 2) < 0.01)

# (c) Hiring marginal mean must be in [1,7] and near the SET1(prof) intercept (~5.78).
pred_hiring <- avg_predictions(m_check, newdata = df_prof)
cat("Predicted mean hiring recommendation (SET1, prof):", round(pred_hiring$estimate, 3),
    "| model intercept:", round(coef(m_check)["(Intercept)"], 3), "\n")
stopifnot(pred_hiring$estimate >= 1, pred_hiring$estimate <= 7)

# (d) Marginal means must be invariant to contrast coding (treatment vs. sum contrasts).
df_prof_treatment <- df_prof %>%
  mutate(
    perfil_genero      = factor(as.character(perfil_genero), levels = c("Hombre", "Mujer")),
    perfil_estado_civil = factor(as.character(perfil_estado_civil), levels = c("soltero/a", "casado/a")),
    perfil_parental     = factor(as.character(perfil_parental), levels = c("actualmente no tiene hijos", "tiene un hijo", "tiene dos hijos")),
    perfil_plan_futuro  = factor(as.character(perfil_plan_futuro), levels = c("no quiere", "en 2 anos", "en 5 anos")),
    perfil_exp          = factor(as.character(perfil_exp)),
    perfil_desempeno    = factor(as.character(perfil_desempeno)),
    pantalla_orden      = factor(as.character(pantalla_orden)),
    template_contexto   = factor(as.character(template_contexto))
  )
m_treatment <- lm_robust(
  recommendation_probability ~ perfil_genero + perfil_estado_civil + perfil_parental +
    perfil_plan_futuro + perfil_exp + perfil_desempeno + pantalla_orden + template_contexto,
  data = df_prof_treatment, clusters = df_prof_treatment$numericalId, se_type = "CR2"
)
pred_treatment <- avg_predictions(m_treatment, newdata = df_prof_treatment)
cat("Predicted mean hiring recommendation (treatment contrasts):", round(pred_treatment$estimate, 3),
    "| difference from sum-contrast version:", round(pred_treatment$estimate - pred_hiring$estimate, 6), "\n")
stopifnot(abs(pred_treatment$estimate - pred_hiring$estimate) < 1e-6)

cat("All sanity checks passed.\n\n")

# ==============================================================================
# 5. HELPERS SHARED ACROSS ALL CONTRASTS
# ==============================================================================

# Conservative, uniform choice of degrees of freedom: marginaleffects does not
# compute exact Satterthwaite df for compound linear combinations, so every
# contrast drawn from a given model uses that model's minimum per-coefficient
# CR2 df. This never overstates precision and is the conservative direction
# for the low-df guardrail below.
model_df <- function(model) min(model$df)

cell_info <- function(data) {
  tibble(n_obs = nrow(data), n_clusters = n_distinct(data$numericalId))
}

# Converts a marginaleffects comparisons/predictions/hypotheses object into
# this script's target row schema, attaching the descriptive columns.
#
# df_value is required rather than read off mm_obj: hypotheses() (used for
# every diff-in-diff / double-diff row) uses the df passed into the original
# avg_comparisons()/predictions() call internally for its p-value, but does
# not expose a "df" column in its own output. Passing model_df(m) explicitly
# at every call site avoids silently writing NA for more than half the rows.
to_rows <- function(mm_obj, set_id, condition, outcome, estimand, contrast, hypothesis_id, n_info, df_value) {
  d <- as.data.frame(mm_obj)
  tibble(
    set_id      = set_id,
    condition   = condition,
    outcome     = outcome,
    estimand    = estimand,
    hypothesis  = hypothesis_id,
    contrast    = contrast,
    estimate    = d$estimate,
    std.error   = d$std.error,
    conf.low    = d$conf.low,
    conf.high   = d$conf.high,
    df          = df_value,
    p.value     = d$p.value,
    n_obs       = n_info$n_obs,
    n_clusters  = n_info$n_clusters
  )
}

# One explicit two-level comparison (reference vs. level), optionally split
# `by` a grouping variable. Explicit levels are required: sum-contrast level
# reordering moves the substantive reference (e.g. "actualmente no tiene
# hijos", "no quiere") to the *last* factor level, so marginaleffects' default
# vs-first-level comparison would silently contrast the wrong pair.
level_contrast <- function(model, var, ref, level, newdata, by = NULL) {
  vars <- setNames(list(c(ref, level)), var)
  avg_comparisons(model, variables = vars, newdata = newdata, by = by, df = model_df(model))
}

# Difference-in-differences from a single `by`-grouped comparison (2 rows -> 1).
diff_in_diff <- function(comp_obj) {
  hypotheses(comp_obj, hypothesis = ~revpairwise)
}

# Double-difference (triple interaction) from a comparison grouped `by` two
# variables (4 rows -> 1), e.g. (Mujer,prof - Hombre,prof) - (Mujer,tec - Hombre,tec).
# Row indices are located programmatically (never hard-coded) because
# marginaleffects' own documentation warns that positional b1/b2/... indices
# depend on row order, which can vary across runs/versions.
double_diff <- function(comp_obj, group1_var, group1_a, group1_b, group2_var, group2_a, group2_b) {
  d <- as.data.frame(comp_obj)
  i_a1 <- which(d[[group1_var]] == group1_a & d[[group2_var]] == group2_a)
  i_a2 <- which(d[[group1_var]] == group1_a & d[[group2_var]] == group2_b)
  i_b1 <- which(d[[group1_var]] == group1_b & d[[group2_var]] == group2_a)
  i_b2 <- which(d[[group1_var]] == group1_b & d[[group2_var]] == group2_b)
  stopifnot(length(i_a1) == 1, length(i_a2) == 1, length(i_b1) == 1, length(i_b2) == 1)
  hyp_str <- sprintf("b%d - b%d - b%d + b%d = 0", i_a1, i_a2, i_b1, i_b2)
  hypotheses(comp_obj, hypothesis = hyp_str)
}

results <- list()
add_result <- function(rows) results[[length(results) + 1]] <<- rows

# ==============================================================================
# 6. ANCHOR - fixed-profile contrast from SET1 (spec section 4b)
# ==============================================================================
# Male vs. female, both single, no children, no family plans - a conditional
# prediction (datagrid), not an averaged marginal mean. Reported for both
# occupational conditions.

for (cond in list(list(id = "profesional", suffix = "_prof", data = df_prof),
                   list(id = "tecnico",     suffix = "_tec",  data = df_tec))) {
  models <- load_model_list("set1", cond$suffix)
  rows <- map_dfr(all_outcomes, function(outcome) {
    m <- get_model(models, outcome)
    if (is.null(m)) return(NULL)
    anc <- predictions(
      m,
      newdata = datagrid(
        perfil_genero       = c("Hombre", "Mujer"),
        perfil_estado_civil = "soltero/a",
        perfil_parental     = "actualmente no tiene hijos",
        perfil_plan_futuro  = "no quiere",
        newdata             = cond$data
      ),
      hypothesis = ~revpairwise,
      df = model_df(m)
    )
    to_rows(anc, "set1", cond$id, outcome, "fixed_profile",
            "Male vs. female, single, childless, no plans", "Anchor", cell_info(cond$data),
            df_value = model_df(m))
  })
  add_result(rows)
}

# ==============================================================================
# 7. H1 / H2: FERTILITY-PLAN PENALTY AMONG CHILDLESS WOMEN (SET2b) / MOTHERS (SET4b)
#    + H4-H7: THE SAME CONTRASTS, GENDER-DIFFERENCED
# ==============================================================================

fertility_blocks <- tribble(
  ~set_id, ~subset_label,  ~subset_fn,
  "set2b", "childless",    function(d) filter(d, perfil_parental == "actualmente no tiene hijos"),
  "set4b", "mothers",      function(d) filter(d, perfil_parental %in% c("tiene un hijo", "tiene dos hijos"))
)

for (b in seq_len(nrow(fertility_blocks))) {
  set_id       <- fertility_blocks$set_id[b]
  subset_label <- fertility_blocks$subset_label[b]
  subset_fn    <- fertility_blocks$subset_fn[[b]]
  # H1 (childless) uses SET2b; H2 (mothers) uses SET4b.
  h_main_id <- if (set_id == "set2b") "H1" else "H2"
  h_did_ids <- if (set_id == "set2b") c("en 2 anos" = "H4", "en 5 anos" = "H5")
               else c("en 2 anos" = "H6", "en 5 anos" = "H7")

  for (cond in list(list(id = "profesional", suffix = "_prof", data = df_prof),
                     list(id = "tecnico",     suffix = "_tec",  data = df_tec))) {
    models  <- load_model_list(set_id, cond$suffix)
    subdata <- subset_fn(cond$data)

    for (level in c("en 2 anos", "en 5 anos")) {
      main_rows <- map_dfr(all_outcomes, function(outcome) {
        m <- get_model(models, outcome)
        if (is.null(m)) return(NULL)
        comp <- level_contrast(m, "perfil_plan_futuro", "no quiere", level, subdata, by = "perfil_genero")
        comp_df <- as.data.frame(comp)
        mujer_row <- comp_df[comp_df$perfil_genero == "Mujer", , drop = FALSE]
        n_info <- cell_info(filter(subdata, perfil_genero == "Mujer"))
        to_rows(mujer_row, set_id, cond$id, outcome, "avg_comparison",
                sprintf("%s: %s vs. no quiere (women)", subset_label, level),
                h_main_id, n_info, df_value = model_df(m))
      })
      add_result(main_rows)

      did_rows <- map_dfr(all_outcomes, function(outcome) {
        m <- get_model(models, outcome)
        if (is.null(m)) return(NULL)
        comp <- level_contrast(m, "perfil_plan_futuro", "no quiere", level, subdata, by = "perfil_genero")
        dd <- diff_in_diff(comp)
        to_rows(dd, set_id, cond$id, outcome, "diff_in_diff",
                sprintf("%s: %s vs. no quiere, women minus men", subset_label, level),
                h_did_ids[level], cell_info(subdata), df_value = model_df(m))
      })
      add_result(did_rows)
    }
  }
}

# ==============================================================================
# 8. H3: MOTHERHOOD PENALTY (SET2a) + H8-H9: GENDER-DIFFERENCED
# ==============================================================================

parental_levels <- c("tiene un hijo", "tiene dos hijos")
parental_h_did  <- c("tiene un hijo" = "H8", "tiene dos hijos" = "H9")

for (cond in list(list(id = "profesional", suffix = "_prof", data = df_prof),
                   list(id = "tecnico",     suffix = "_tec",  data = df_tec))) {
  models <- load_model_list("set2a", cond$suffix)

  for (level in parental_levels) {
    main_rows <- map_dfr(all_outcomes, function(outcome) {
      m <- get_model(models, outcome)
      if (is.null(m)) return(NULL)
      comp <- level_contrast(m, "perfil_parental", "actualmente no tiene hijos", level, cond$data, by = "perfil_genero")
      comp_df <- as.data.frame(comp)
      mujer_row <- comp_df[comp_df$perfil_genero == "Mujer", , drop = FALSE]
      n_info <- cell_info(filter(cond$data, perfil_genero == "Mujer"))
      to_rows(mujer_row, "set2a", cond$id, outcome, "avg_comparison",
              sprintf("%s vs. childless (women)", level), "H3", n_info, df_value = model_df(m))
    })
    add_result(main_rows)

    did_rows <- map_dfr(all_outcomes, function(outcome) {
      m <- get_model(models, outcome)
      if (is.null(m)) return(NULL)
      comp <- level_contrast(m, "perfil_parental", "actualmente no tiene hijos", level, cond$data, by = "perfil_genero")
      dd <- diff_in_diff(comp)
      to_rows(dd, "set2a", cond$id, outcome, "diff_in_diff",
              sprintf("%s vs. childless, women minus men", level), parental_h_did[level], cell_info(cond$data),
              df_value = model_df(m))
    })
    add_result(did_rows)
  }
}

# ==============================================================================
# 9. H16: SINGLE VS. MARRIED MOTHERHOOD PENALTY GAP (SET4a), GENDER-DIFFERENCED
# ==============================================================================

for (cond in list(list(id = "profesional", suffix = "_prof", data = df_prof),
                   list(id = "tecnico",     suffix = "_tec",  data = df_tec))) {
  models  <- load_model_list("set4a", cond$suffix)
  parents <- filter(cond$data, perfil_parental %in% c("tiene un hijo", "tiene dos hijos"))

  main_rows <- map_dfr(all_outcomes, function(outcome) {
    m <- get_model(models, outcome)
    if (is.null(m)) return(NULL)
    comp <- level_contrast(m, "perfil_estado_civil", "soltero/a", "casado/a", parents, by = "perfil_genero")
    comp_df <- as.data.frame(comp)
    mujer_row <- comp_df[comp_df$perfil_genero == "Mujer", , drop = FALSE]
    n_info <- cell_info(filter(parents, perfil_genero == "Mujer"))
    to_rows(mujer_row, "set4a", cond$id, outcome, "avg_comparison",
            "Married vs. single, among mothers (women)", "H16_component", n_info, df_value = model_df(m))
  })
  add_result(main_rows)

  did_rows <- map_dfr(all_outcomes, function(outcome) {
    m <- get_model(models, outcome)
    if (is.null(m)) return(NULL)
    comp <- level_contrast(m, "perfil_estado_civil", "soltero/a", "casado/a", parents, by = "perfil_genero")
    dd <- diff_in_diff(comp)
    to_rows(dd, "set4a", cond$id, outcome, "diff_in_diff",
            "Married vs. single among parents, women minus men", "H16", cell_info(parents),
            df_value = model_df(m))
  })
  add_result(did_rows)
}

# ==============================================================================
# 10. H10-H15: OCCUPATIONAL-LEVEL MODERATION (POOLED SETS 5, 5a-5e)
# ==============================================================================
# H10 (SET5) has no interaction term with gender/parental/plan_futuro, so it
# cannot itself test moderation - it establishes whether occupation has a main
# effect at all. H11-H15 (SET5a-5e) carry the actual moderation tests.

# --- H10: main effect of occupational level (SET5) ---
models_set5 <- load_model_list("set5", "")
h10_rows <- map_dfr(all_outcomes, function(outcome) {
  m <- get_model(models_set5, outcome)
  if (is.null(m)) return(NULL)
  comp <- avg_comparisons(m, variables = "condicion_ocupacional", newdata = df, df = model_df(m))
  to_rows(comp, "set5", "pooled", outcome, "avg_comparison",
          "Technical vs. professional (main effect, no interaction)", "H10", cell_info(df),
          df_value = model_df(m))
})
add_result(h10_rows)

# --- H11: does the gender anchor gap differ by occupation? (SET5a) ---
models_set5a <- load_model_list("set5a", "")
h11_rows <- map_dfr(all_outcomes, function(outcome) {
  m <- get_model(models_set5a, outcome)
  if (is.null(m)) return(NULL)
  comp <- level_contrast(m, "perfil_genero", "Hombre", "Mujer", df, by = "condicion_ocupacional")
  dd <- diff_in_diff(comp)
  to_rows(dd, "set5a", "pooled", outcome, "diff_in_diff",
          "Mujer vs. Hombre, technical minus professional", "H11", cell_info(df),
          df_value = model_df(m))
})
add_result(h11_rows)

# --- H12: does the H3 motherhood penalty differ by occupation? (SET5b) ---
models_set5b <- load_model_list("set5b", "")
for (level in parental_levels) {
  rows <- map_dfr(all_outcomes, function(outcome) {
    m <- get_model(models_set5b, outcome)
    if (is.null(m)) return(NULL)
    comp <- level_contrast(m, "perfil_parental", "actualmente no tiene hijos", level, df, by = "condicion_ocupacional")
    dd <- diff_in_diff(comp)
    to_rows(dd, "set5b", "pooled", outcome, "diff_in_diff",
            sprintf("%s vs. childless, technical minus professional", level), "H12", cell_info(df),
            df_value = model_df(m))
  })
  add_result(rows)
}

# --- H13: does the H1 fertility-plan penalty differ by occupation? (SET5c) ---
models_set5c <- load_model_list("set5c", "")
for (level in c("en 2 anos", "en 5 anos")) {
  rows <- map_dfr(all_outcomes, function(outcome) {
    m <- get_model(models_set5c, outcome)
    if (is.null(m)) return(NULL)
    comp <- level_contrast(m, "perfil_plan_futuro", "no quiere", level, df, by = "condicion_ocupacional")
    dd <- diff_in_diff(comp)
    to_rows(dd, "set5c", "pooled", outcome, "diff_in_diff",
            sprintf("%s vs. no quiere, technical minus professional", level), "H13", cell_info(df),
            df_value = model_df(m))
  })
  add_result(rows)
}

# --- H14: does the H8/H9 gender-DiD of the motherhood penalty differ by occupation? (SET5d) ---
models_set5d <- load_model_list("set5d", "")
for (level in parental_levels) {
  rows <- map_dfr(all_outcomes, function(outcome) {
    m <- get_model(models_set5d, outcome)
    if (is.null(m)) return(NULL)
    comp <- level_contrast(m, "perfil_parental", "actualmente no tiene hijos", level, df,
                            by = c("perfil_genero", "condicion_ocupacional"))
    dd <- double_diff(comp, "perfil_genero", "Mujer", "Hombre",
                       "condicion_ocupacional", "profesional", "tecnico")
    to_rows(dd, "set5d", "pooled", outcome, "double_diff",
            sprintf("%s vs. childless, (women-men) DiD, professional minus technical", level),
            "H14", cell_info(df), df_value = model_df(m))
  })
  add_result(rows)
}

# --- H15: does the H4-H7 gender-DiD of the fertility-plan penalty differ by occupation? (SET5e) ---
models_set5e <- load_model_list("set5e", "")
for (level in c("en 2 anos", "en 5 anos")) {
  rows <- map_dfr(all_outcomes, function(outcome) {
    m <- get_model(models_set5e, outcome)
    if (is.null(m)) return(NULL)
    comp <- level_contrast(m, "perfil_plan_futuro", "no quiere", level, df,
                            by = c("perfil_genero", "condicion_ocupacional"))
    dd <- double_diff(comp, "perfil_genero", "Mujer", "Hombre",
                       "condicion_ocupacional", "profesional", "tecnico")
    to_rows(dd, "set5e", "pooled", outcome, "double_diff",
            sprintf("%s vs. no quiere, (women-men) DiD, professional minus technical", level),
            "H15", cell_info(df), df_value = model_df(m))
  })
  add_result(rows)
}

# ==============================================================================
# 11. ASSEMBLE, BH-ADJUST, FLAG LOW DF
# ==============================================================================

marginal_means_all <- bind_rows(results) %>%
  mutate(
    outcome_family = outcome_family[outcome],
    low_df_flag = df < 30
  ) %>%
  group_by(outcome_family) %>%
  mutate(p.adj = p.adjust(p.value, method = "BH")) %>%
  ungroup() %>%
  select(set_id, condition, outcome, estimand, hypothesis, contrast,
         estimate, std.error, conf.low, conf.high, df, p.value, p.adj,
         n_obs, n_clusters, outcome_family, low_df_flag)

n_flagged <- sum(marginal_means_all$low_df_flag, na.rm = TRUE)
cat(sprintf("\n%d of %d contrasts have CR2 df < 30 (flagged in low_df_flag).\n",
            n_flagged, nrow(marginal_means_all)))

# ==============================================================================
# 12. OUTPUTS
# ==============================================================================

write_csv(marginal_means_all, file.path(tables_dir, "marginal_means_all.csv"))
cat("  done:", file.path(tables_dir, "marginal_means_all.csv"), "\n")

# Per-hypothesis .docx tables, matching the modelsummary styling used in 05_Tables.R.
hyp_ids <- c("Anchor", paste0("H", 1:16))
for (h in hyp_ids) {
  h_data <- marginal_means_all %>%
    filter(hypothesis == h | (h == "H16" & hypothesis == "H16_component")) %>%
    mutate(outcome_label = outcome_labels[outcome]) %>%
    select(outcome_label, condition, contrast, estimate, std.error,
           conf.low, conf.high, df, p.value, p.adj, n_obs, n_clusters)
  if (nrow(h_data) == 0) next
  filename <- file.path(mm_tables_dir, paste0(h, "_marginal_means.docx"))
  modelsummary::datasummary_df(
    h_data,
    title = paste0(h, ": predicted marginal means / contrasts"),
    notes = c(
      "OLS with cluster-robust standard errors (CR2, clustered by respondent).",
      "df is the minimum per-coefficient CR2 df in the source model (conservative).",
      "p.adj is Benjamini-Hochberg-adjusted within the outcome's construct family."
    ),
    output = filename
  )
  cat("  done:", filename, "\n")
}

# Console summary (spec section 7.3): the single contrast that most directly
# tests each hypothesis, restricted to the hiring outcome as the common
# reference point across all of them (full detail is in the CSV/docx above).
cat("\n=== Console summary: one headline contrast per hypothesis (hiring recommendation) ===\n")

headline_filter <- tribble(
  ~hypothesis, ~contrast_match,
  "Anchor", NA_character_,
  "H1",  "en 2 anos vs. no quiere \\(women\\)$",
  "H2",  "en 2 anos vs. no quiere \\(women\\)$",
  "H3",  "^tiene dos hijos vs\\. childless \\(women\\)$",
  "H4",  NA_character_, "H5", NA_character_, "H6", NA_character_, "H7", NA_character_,
  "H8",  NA_character_,
  "H9",  "^tiene dos hijos vs\\. childless, women minus men$",
  "H10", NA_character_, "H11", NA_character_,
  "H12", "^tiene dos hijos vs\\. childless, technical minus professional$",
  "H13", "^en 2 anos vs\\. no quiere, technical minus professional$",
  "H14", "^tiene dos hijos vs\\. childless.*professional minus technical$",
  "H15", "^en 2 anos vs\\. no quiere.*professional minus technical$",
  "H16", NA_character_
)

summary_rows <- map_dfr(c("Anchor", paste0("H", 1:16)), function(h) {
  h_lookup <- if (h == "H16") "H16" else h
  candidates <- marginal_means_all %>%
    filter(hypothesis == h_lookup, outcome == "recommendation_probability")
  if (nrow(candidates) == 0) return(NULL)
  pat <- headline_filter$contrast_match[headline_filter$hypothesis == h]
  pat <- if (length(pat) == 0) NA_character_ else pat[1]
  chosen <- if (!is.na(pat)) candidates %>% filter(str_detect(contrast, pat)) else candidates
  if (nrow(chosen) == 0) chosen <- candidates
  # Keep one row per condition (profesional/tecnico) where the hypothesis is
  # estimated separately by occupation; pooled hypotheses (H10-H15) have one row.
  chosen %>%
    transmute(hypothesis = h, condition, contrast, estimate, conf.low, conf.high, p.adj)
})

print(summary_rows, n = Inf, width = Inf)

cat("\nDone. Full detail: ", file.path(tables_dir, "marginal_means_all.csv"),
    "\nPer-hypothesis tables: ", mm_tables_dir, "\n", sep = "")
