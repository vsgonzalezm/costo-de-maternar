# ============================================================================
# 05c - Predicted marginal means and contrasts, POOLED sample with occupation
#       as a control (AsPredicted #297542, item 5)
# Author: Tania Hutt
# Date: agosto 2026
# Purpose: Re-runs the Anchor / H1-H9 / H16 contrasts from 05b_marginal_means.R
#          on the POOLED sample (profesional + tecnico together), using model
#          sets that add condicion_ocupacional as an ADDITIVE control rather
#          than splitting the sample by occupation. This answers "does the
#          fertility/motherhood penalty hold once occupation is held constant",
#          as opposed to 05b's "does it hold within each occupation separately"
#          and 05b's H10-H15 "does occupation moderate the penalty" (that
#          moderation question is already pooled by construction and is not
#          repeated here - see 05b sections 10).
#
# Model sets used here (fit in 04_analysis_cluster_robust_models.R /
# 07_Indices.R, pooled, condicion_ocupacional entered additively):
#   - SET5          (= SET1 + condicion_ocupacional, pooled) -> Anchor
#   - SET2A_POOLED  (= SET2A + condicion_ocupacional, pooled) -> H3, H8, H9
#   - SET2B_POOLED  (= SET2B + condicion_ocupacional, pooled) -> H1, H4, H5 (childless)
#   - SET4B_POOLED  (= SET4B + condicion_ocupacional, pooled) -> H2, H6, H7 (mothers)
#   - SET4A_POOLED  (= SET4A + condicion_ocupacional, pooled) -> H16
#
# Ground truth for H1-H16 is the pre-registration PDF, not the paraphrase in
# SPEC_05b_marginal_means.md. Hypothesis numbering/anchoring choices are
# identical to 05b_marginal_means.R (see that script's header) - only the
# sample (pooled vs. split-by-occupation) and control set differ here.
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
mm_tables_dir  <- file.path(tables_dir, "marginal_means_pooled")
dir.create(mm_tables_dir, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# 1. REBUILD df IDENTICALLY TO 04_analysis_cluster_robust_models.R
# ==============================================================================
# lm_robust objects do not retain the model frame, so marginal means need the
# original data, factored identically to the fitted models. Copied verbatim
# from 04_analysis_cluster_robust_models.R (do not hand-edit independently).
# Unlike 05b, there is no df_prof/df_tec split: every contrast in this script
# is estimated on the full pooled df, with condicion_ocupacional entered as a
# control in the underlying model instead.

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

cat("Pooled: N profiles =", nrow(df), "| N respondents =", n_distinct(df$numericalId), "\n")

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

# Families for the BH correction, mirroring 05b_marginal_means.R.
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
# purrr::compact() drops the NULL entries left by run_models()'s tryCatch. All
# sets used here are pooled (fit with split_by_condition = FALSE in
# 04_analysis_cluster_robust_models.R / 07_Indices.R), so suffix is always "".

load_model_list <- function(set_id) {
  regular <- compact(readRDS(file.path(models_dir, paste0(set_id, "_models.rds"))))
  idx     <- compact(readRDS(file.path(models_dir, paste0(set_id, "_idx_models.rds"))))
  c(regular, idx[intersect(idx_outcomes, names(idx))])
}

get_model <- function(model_list, outcome) model_list[[outcome]]

# ==============================================================================
# 4. SANITY CHECKS (mirrors 05b_marginal_means.R sections 3/8) - run before
#    trusting anything
# ==============================================================================

cat("=== Sanity checks ===\n")

m_check <- get_model(load_model_list("set5"), "recommendation_probability")

# (a) CR2 vcov flows into marginaleffects automatically.
vcov_ok <- isTRUE(all.equal(vcov(m_check), marginaleffects::get_vcov(m_check)))
cat("CR2 vcov matches marginaleffects::get_vcov():", vcov_ok, "\n")
stopifnot(vcov_ok)

# (b) avg_comparisons on a two-level factor must equal 2x the sum-contrast coefficient.
ac_gender <- avg_comparisons(m_check, variables = "perfil_genero", newdata = df)
ratio <- abs(ac_gender$estimate) / abs(coef(m_check)["perfil_generoMujer"])
cat("avg_comparisons(gender) / sum-contrast coefficient (expect ~2):", round(ratio, 3), "\n")
stopifnot(abs(ratio - 2) < 0.01)

# (c) Hiring marginal mean must be in [1,7].
pred_hiring <- avg_predictions(m_check, newdata = df)
cat("Predicted mean hiring recommendation (SET5, pooled):", round(pred_hiring$estimate, 3),
    "| model intercept:", round(coef(m_check)["(Intercept)"], 3), "\n")
stopifnot(pred_hiring$estimate >= 1, pred_hiring$estimate <= 7)

cat("All sanity checks passed.\n\n")

# ==============================================================================
# 5. HELPERS SHARED ACROSS ALL CONTRASTS (identical to 05b_marginal_means.R)
# ==============================================================================

model_df <- function(model) min(model$df)

cell_info <- function(data) {
  tibble(n_obs = nrow(data), n_clusters = n_distinct(data$numericalId))
}

to_rows <- function(mm_obj, set_id, outcome, estimand, contrast, hypothesis_id, n_info, df_value) {
  d <- as.data.frame(mm_obj)
  tibble(
    set_id      = set_id,
    condition   = "pooled",
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

level_contrast <- function(model, var, ref, level, newdata, by = NULL) {
  vars <- setNames(list(c(ref, level)), var)
  avg_comparisons(model, variables = vars, newdata = newdata, by = by, df = model_df(model))
}

diff_in_diff <- function(comp_obj) {
  hypotheses(comp_obj, hypothesis = ~revpairwise)
}

results <- list()
add_result <- function(rows) results[[length(results) + 1]] <<- rows

# ==============================================================================
# 6. ANCHOR - fixed-profile contrast from SET5 (pooled + occupation control)
# ==============================================================================
# Male vs. female, both single, no children, no family plans - a conditional
# prediction (datagrid), not an averaged marginal mean. condicion_ocupacional
# is left unspecified in the datagrid, so it is held at its sample mode
# (profesional), consistent with the rest of the covariates.

models_set5 <- load_model_list("set5")
anchor_rows <- map_dfr(all_outcomes, function(outcome) {
  m <- get_model(models_set5, outcome)
  if (is.null(m)) return(NULL)
  anc <- predictions(
    m,
    newdata = datagrid(
      perfil_genero       = c("Hombre", "Mujer"),
      perfil_estado_civil = "soltero/a",
      perfil_parental     = "actualmente no tiene hijos",
      perfil_plan_futuro  = "no quiere",
      newdata             = df
    ),
    hypothesis = ~revpairwise,
    df = model_df(m)
  )
  to_rows(anc, "set5", outcome, "fixed_profile",
          "Male vs. female, single, childless, no plans", "Anchor", cell_info(df),
          df_value = model_df(m))
})
add_result(anchor_rows)

# ==============================================================================
# 7. H1 / H2: FERTILITY-PLAN PENALTY AMONG CHILDLESS WOMEN (SET2B_POOLED) /
#    MOTHERS (SET4B_POOLED) + H4-H7: THE SAME CONTRASTS, GENDER-DIFFERENCED
# ==============================================================================

fertility_blocks <- tribble(
  ~set_id,         ~subset_label, ~subset_fn,
  "set2b_pooled",  "childless",   function(d) filter(d, perfil_parental == "actualmente no tiene hijos"),
  "set4b_pooled",  "mothers",     function(d) filter(d, perfil_parental %in% c("tiene un hijo", "tiene dos hijos"))
)

for (b in seq_len(nrow(fertility_blocks))) {
  set_id       <- fertility_blocks$set_id[b]
  subset_label <- fertility_blocks$subset_label[b]
  subset_fn    <- fertility_blocks$subset_fn[[b]]
  # H1 (childless) uses SET2B_POOLED; H2 (mothers) uses SET4B_POOLED.
  h_main_id <- if (subset_label == "childless") "H1" else "H2"
  h_did_ids <- if (subset_label == "childless") c("en 2 anos" = "H4", "en 5 anos" = "H5")
               else c("en 2 anos" = "H6", "en 5 anos" = "H7")

  models  <- load_model_list(set_id)
  subdata <- subset_fn(df)

  for (level in c("en 2 anos", "en 5 anos")) {
    main_rows <- map_dfr(all_outcomes, function(outcome) {
      m <- get_model(models, outcome)
      if (is.null(m)) return(NULL)
      comp <- level_contrast(m, "perfil_plan_futuro", "no quiere", level, subdata, by = "perfil_genero")
      comp_df <- as.data.frame(comp)
      mujer_row <- comp_df[comp_df$perfil_genero == "Mujer", , drop = FALSE]
      n_info <- cell_info(filter(subdata, perfil_genero == "Mujer"))
      to_rows(mujer_row, set_id, outcome, "avg_comparison",
              sprintf("%s: %s vs. no quiere (women)", subset_label, level),
              h_main_id, n_info, df_value = model_df(m))
    })
    add_result(main_rows)

    did_rows <- map_dfr(all_outcomes, function(outcome) {
      m <- get_model(models, outcome)
      if (is.null(m)) return(NULL)
      comp <- level_contrast(m, "perfil_plan_futuro", "no quiere", level, subdata, by = "perfil_genero")
      dd <- diff_in_diff(comp)
      to_rows(dd, set_id, outcome, "diff_in_diff",
              sprintf("%s: %s vs. no quiere, women minus men", subset_label, level),
              h_did_ids[level], cell_info(subdata), df_value = model_df(m))
    })
    add_result(did_rows)
  }
}

# ==============================================================================
# 8. H3: MOTHERHOOD PENALTY (SET2A_POOLED) + H8-H9: GENDER-DIFFERENCED
# ==============================================================================

parental_levels <- c("tiene un hijo", "tiene dos hijos")
parental_h_did  <- c("tiene un hijo" = "H8", "tiene dos hijos" = "H9")

models_set2a <- load_model_list("set2a_pooled")

for (level in parental_levels) {
  main_rows <- map_dfr(all_outcomes, function(outcome) {
    m <- get_model(models_set2a, outcome)
    if (is.null(m)) return(NULL)
    comp <- level_contrast(m, "perfil_parental", "actualmente no tiene hijos", level, df, by = "perfil_genero")
    comp_df <- as.data.frame(comp)
    mujer_row <- comp_df[comp_df$perfil_genero == "Mujer", , drop = FALSE]
    n_info <- cell_info(filter(df, perfil_genero == "Mujer"))
    to_rows(mujer_row, "set2a_pooled", outcome, "avg_comparison",
            sprintf("%s vs. childless (women)", level), "H3", n_info, df_value = model_df(m))
  })
  add_result(main_rows)

  did_rows <- map_dfr(all_outcomes, function(outcome) {
    m <- get_model(models_set2a, outcome)
    if (is.null(m)) return(NULL)
    comp <- level_contrast(m, "perfil_parental", "actualmente no tiene hijos", level, df, by = "perfil_genero")
    dd <- diff_in_diff(comp)
    to_rows(dd, "set2a_pooled", outcome, "diff_in_diff",
            sprintf("%s vs. childless, women minus men", level), parental_h_did[level], cell_info(df),
            df_value = model_df(m))
  })
  add_result(did_rows)
}

# ==============================================================================
# 9. H16: SINGLE VS. MARRIED MOTHERHOOD PENALTY GAP (SET4A_POOLED),
#    GENDER-DIFFERENCED
# ==============================================================================

models_set4a <- load_model_list("set4a_pooled")
parents <- filter(df, perfil_parental %in% c("tiene un hijo", "tiene dos hijos"))

main_rows <- map_dfr(all_outcomes, function(outcome) {
  m <- get_model(models_set4a, outcome)
  if (is.null(m)) return(NULL)
  comp <- level_contrast(m, "perfil_estado_civil", "soltero/a", "casado/a", parents, by = "perfil_genero")
  comp_df <- as.data.frame(comp)
  mujer_row <- comp_df[comp_df$perfil_genero == "Mujer", , drop = FALSE]
  n_info <- cell_info(filter(parents, perfil_genero == "Mujer"))
  to_rows(mujer_row, "set4a_pooled", outcome, "avg_comparison",
          "Married vs. single, among mothers (women)", "H16_component", n_info, df_value = model_df(m))
})
add_result(main_rows)

did_rows <- map_dfr(all_outcomes, function(outcome) {
  m <- get_model(models_set4a, outcome)
  if (is.null(m)) return(NULL)
  comp <- level_contrast(m, "perfil_estado_civil", "soltero/a", "casado/a", parents, by = "perfil_genero")
  dd <- diff_in_diff(comp)
  to_rows(dd, "set4a_pooled", outcome, "diff_in_diff",
          "Married vs. single among parents, women minus men", "H16", cell_info(parents),
          df_value = model_df(m))
})
add_result(did_rows)

# ==============================================================================
# 10. ASSEMBLE, BH-ADJUST, FLAG LOW DF
# ==============================================================================

marginal_means_pooled <- bind_rows(results) %>%
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

n_flagged <- sum(marginal_means_pooled$low_df_flag, na.rm = TRUE)
cat(sprintf("\n%d of %d contrasts have CR2 df < 30 (flagged in low_df_flag).\n",
            n_flagged, nrow(marginal_means_pooled)))

# ==============================================================================
# 11. OUTPUTS
# ==============================================================================

write_csv(marginal_means_pooled, file.path(tables_dir, "marginal_means_pooled_all.csv"))
cat("  done:", file.path(tables_dir, "marginal_means_pooled_all.csv"), "\n")

# Per-hypothesis .docx tables, matching the modelsummary styling used in 05_Tables.R / 05b.
hyp_ids <- c("Anchor", "H1", "H2", "H3", "H4", "H5", "H6", "H7", "H8", "H9", "H16")
for (h in hyp_ids) {
  h_data <- marginal_means_pooled %>%
    filter(hypothesis == h | (h == "H16" & hypothesis == "H16_component")) %>%
    mutate(outcome_label = outcome_labels[outcome]) %>%
    select(outcome_label, contrast, estimate, std.error,
           conf.low, conf.high, df, p.value, p.adj, n_obs, n_clusters)
  if (nrow(h_data) == 0) next
  filename <- file.path(mm_tables_dir, paste0(h, "_marginal_means_pooled.docx"))
  modelsummary::datasummary_df(
    h_data,
    title = paste0(h, ": predicted marginal means / contrasts (pooled sample, occupation controlled)"),
    notes = c(
      "OLS with cluster-robust standard errors (CR2, clustered by respondent).",
      "Pooled sample (profesional + tecnico); condicion_ocupacional entered as an additive control.",
      "df is the minimum per-coefficient CR2 df in the source model (conservative).",
      "p.adj is Benjamini-Hochberg-adjusted within the outcome's construct family."
    ),
    output = filename
  )
  cat("  done:", filename, "\n")
}

# Console summary: the single contrast that most directly tests each
# hypothesis, restricted to the hiring outcome as the common reference point.
cat("\n=== Console summary: one headline contrast per hypothesis (hiring recommendation, pooled) ===\n")

headline_filter <- tribble(
  ~hypothesis, ~contrast_match,
  "Anchor", NA_character_,
  "H1",  "en 2 anos vs. no quiere \\(women\\)$",
  "H2",  "en 2 anos vs. no quiere \\(women\\)$",
  "H3",  "^tiene dos hijos vs\\. childless \\(women\\)$",
  "H4",  NA_character_, "H5", NA_character_, "H6", NA_character_, "H7", NA_character_,
  "H8",  NA_character_,
  "H9",  "^tiene dos hijos vs\\. childless, women minus men$",
  "H16", NA_character_
)

summary_rows <- map_dfr(hyp_ids, function(h) {
  h_lookup <- if (h == "H16") "H16" else h
  candidates <- marginal_means_pooled %>%
    filter(hypothesis == h_lookup, outcome == "recommendation_probability")
  if (nrow(candidates) == 0) return(NULL)
  pat <- headline_filter$contrast_match[headline_filter$hypothesis == h]
  pat <- if (length(pat) == 0) NA_character_ else pat[1]
  chosen <- if (!is.na(pat)) candidates %>% filter(str_detect(contrast, pat)) else candidates
  if (nrow(chosen) == 0) chosen <- candidates
  chosen %>%
    transmute(hypothesis = h, contrast, estimate, conf.low, conf.high, p.adj)
})

print(summary_rows, n = Inf, width = Inf)

cat("\nDone. Full detail: ", file.path(tables_dir, "marginal_means_pooled_all.csv"),
    "\nPer-hypothesis tables: ", mm_tables_dir, "\n", sep = "")
