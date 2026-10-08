# ============================================================================
# 10 - Marginal means and contrasts, BASELINE models (pooled, occupation controlled)
# Author: Tania Hutt | Octubre 2026: fusiona 05c_marginal_means_pooled.R + el calculo de
#   contrastes de 08_conference_models_and_figures.R + los bloques de contraste de
#   10_fig_hiring.R. NINGUN modelo se reajusta aqui: todo sale de 07_fit_models_baseline.R.
# Purpose (AsPredicted #297542, item 5):
#   PART A  Anchor, H1-H9, H16 contrasts on the pooled sample (hypothesis tables, BH by family)
#   PART B  Contrasts / marginal means behind the "by comparison" figures (11)
#   PART C  The four contrast blocks by outcome (figures "by outcome", 11)
#   PART D  Cluster-robust Wald test that occupation does not moderate (pooling justification)
# Model sets used: set5 (Anchor, H10), set2a_pooled (H3, H8, H9), set2b_pooled (H1, H4, H5),
#   set4b_pooled (H2, H6, H7), set4a_pooled (H16), set5a/b/c (H11-H13, occupational moderation).
# Ground truth for H1-H16 = pre-registration PDF. H4=childless 2yr, H5=childless 5yr,
#   H6=mothers 2yr, H7=mothers 5yr, H8=1 child, H9=2 children (all women minus men).
# Outputs: outputs/10_marginal_means_baseline/ (csv, docx, xlsx + 10_figure_objects.rda, que lee 11)
# ============================================================================
proj <- if (dir.exists(here::here("Encuesta Demanda", "data"))) here::here("Encuesta Demanda") else here::here()
source(file.path(proj, "code", "00_setup.R"))
suppressPackageStartupMessages({ library(marginaleffects); library(modelsummary); library(writexl) })
init_output("10_marginal_means_baseline")

df <- load_analysis_data()
cat("Pooled: N profiles =", nrow(df), "| N respondents =", n_distinct(df$numericalId), "\n")

# Outcomes de la PARTE A (los mismos de 05c: 16 items + 3 indices; costo queda desagregado)
mm_outcomes <- c(outcomes_items, "idx_riesgo", "idx_crecimiento", "idx_competencia")

# Familias para la correccion BH de la parte A
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

load_model_list <- function(set_id) compact(load_models(set_id, "pooled"))[mm_outcomes] %>% compact()
get_model <- function(model_list, outcome) model_list[[outcome]]

# ==============================================================================
# PART A -- 4. SANITY CHECKS (mirrors 05b_marginal_means.R sections 3/8) - run before
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
anchor_rows <- map_dfr(mm_outcomes, function(outcome) {
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
    main_rows <- map_dfr(mm_outcomes, function(outcome) {
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

    did_rows <- map_dfr(mm_outcomes, function(outcome) {
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
  main_rows <- map_dfr(mm_outcomes, function(outcome) {
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

  did_rows <- map_dfr(mm_outcomes, function(outcome) {
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

main_rows <- map_dfr(mm_outcomes, function(outcome) {
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

did_rows <- map_dfr(mm_outcomes, function(outcome) {
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

write_csv(marginal_means_pooled, out_path("marginal_means_pooled_all.csv"))
cat("  done:", out_path("marginal_means_pooled_all.csv"), "\n")

# Per-hypothesis .docx tables, matching the modelsummary styling used in 05_Tables.R / 05b.
hyp_ids <- c("Anchor", "H1", "H2", "H3", "H4", "H5", "H6", "H7", "H8", "H9", "H16")
for (h in hyp_ids) {
  h_data <- marginal_means_pooled %>%
    filter(hypothesis == h | (h == "H16" & hypothesis == "H16_component")) %>%
    mutate(outcome_label = outcome_labels[outcome]) %>%
    select(outcome_label, contrast, estimate, std.error,
           conf.low, conf.high, df, p.value, p.adj, n_obs, n_clusters)
  if (nrow(h_data) == 0) next
  filename <- out_path("tables_by_hypothesis", paste0(h, "_marginal_means_pooled.docx"))
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

cat("
Done part A (hypothesis contrasts).
")

# ==============================================================================
# PART A2 -- H10-H15: occupational-level moderation (pooled sets 5, 5a-5e)
# ==============================================================================
# H10 (SET5) is the main effect of occupation; H11-H15 (SET5a-5e) carry the moderation
# tests. Own BH table (by outcome family) so that Part A's p.adj stay as pre-registered.
parental_levels <- c("tiene un hijo", "tiene dos hijos")
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


results_occ <- list()
add_result_occ <- function(rows) results_occ[[length(results_occ) + 1]] <<- rows

# --- H10: main effect of occupational level (SET5) ---
models_set5 <- load_model_list("set5")
h10_rows <- map_dfr(mm_outcomes, function(outcome) {
  m <- get_model(models_set5, outcome)
  if (is.null(m)) return(NULL)
  comp <- avg_comparisons(m, variables = "condicion_ocupacional", newdata = df, df = model_df(m))
  to_rows(comp, "set5", outcome, "avg_comparison",
          "Technical vs. professional (main effect, no interaction)", "H10", cell_info(df),
          df_value = model_df(m))
})
add_result_occ(h10_rows)

# --- H11: does the gender anchor gap differ by occupation? (SET5a) ---
models_set5a <- load_model_list("set5a")
h11_rows <- map_dfr(mm_outcomes, function(outcome) {
  m <- get_model(models_set5a, outcome)
  if (is.null(m)) return(NULL)
  comp <- level_contrast(m, "perfil_genero", "Hombre", "Mujer", df, by = "condicion_ocupacional")
  dd <- diff_in_diff(comp)
  to_rows(dd, "set5a", outcome, "diff_in_diff",
          "Mujer vs. Hombre, technical minus professional", "H11", cell_info(df),
          df_value = model_df(m))
})
add_result_occ(h11_rows)

# --- H12: does the H3 motherhood penalty differ by occupation? (SET5b) ---
models_set5b <- load_model_list("set5b")
for (level in parental_levels) {
  rows <- map_dfr(mm_outcomes, function(outcome) {
    m <- get_model(models_set5b, outcome)
    if (is.null(m)) return(NULL)
    comp <- level_contrast(m, "perfil_parental", "actualmente no tiene hijos", level, df, by = "condicion_ocupacional")
    dd <- diff_in_diff(comp)
    to_rows(dd, "set5b", outcome, "diff_in_diff",
            sprintf("%s vs. childless, technical minus professional", level), "H12", cell_info(df),
            df_value = model_df(m))
  })
  add_result_occ(rows)
}

# --- H13: does the H1 fertility-plan penalty differ by occupation? (SET5c) ---
models_set5c <- load_model_list("set5c")
for (level in c("en 2 anos", "en 5 anos")) {
  rows <- map_dfr(mm_outcomes, function(outcome) {
    m <- get_model(models_set5c, outcome)
    if (is.null(m)) return(NULL)
    comp <- level_contrast(m, "perfil_plan_futuro", "no quiere", level, df, by = "condicion_ocupacional")
    dd <- diff_in_diff(comp)
    to_rows(dd, "set5c", outcome, "diff_in_diff",
            sprintf("%s vs. no quiere, technical minus professional", level), "H13", cell_info(df),
            df_value = model_df(m))
  })
  add_result_occ(rows)
}

# --- H14: does the H8/H9 gender-DiD of the motherhood penalty differ by occupation? (SET5d) ---
models_set5d <- load_model_list("set5d")
for (level in parental_levels) {
  rows <- map_dfr(mm_outcomes, function(outcome) {
    m <- get_model(models_set5d, outcome)
    if (is.null(m)) return(NULL)
    comp <- level_contrast(m, "perfil_parental", "actualmente no tiene hijos", level, df,
                            by = c("perfil_genero", "condicion_ocupacional"))
    dd <- double_diff(comp, "perfil_genero", "Mujer", "Hombre",
                       "condicion_ocupacional", "profesional", "tecnico")
    to_rows(dd, "set5d", outcome, "double_diff",
            sprintf("%s vs. childless, (women-men) DiD, professional minus technical", level),
            "H14", cell_info(df), df_value = model_df(m))
  })
  add_result_occ(rows)
}

# --- H15: does the H4-H7 gender-DiD of the fertility-plan penalty differ by occupation? (SET5e) ---
models_set5e <- load_model_list("set5e")
for (level in c("en 2 anos", "en 5 anos")) {
  rows <- map_dfr(mm_outcomes, function(outcome) {
    m <- get_model(models_set5e, outcome)
    if (is.null(m)) return(NULL)
    comp <- level_contrast(m, "perfil_plan_futuro", "no quiere", level, df,
                            by = c("perfil_genero", "condicion_ocupacional"))
    dd <- double_diff(comp, "perfil_genero", "Mujer", "Hombre",
                       "condicion_ocupacional", "profesional", "tecnico")
    to_rows(dd, "set5e", outcome, "double_diff",
            sprintf("%s vs. no quiere, (women-men) DiD, professional minus technical", level),
            "H15", cell_info(df), df_value = model_df(m))
  })
  add_result_occ(rows)
}


marginal_means_occupation <- bind_rows(results_occ) %>%
  mutate(outcome_family = outcome_family[outcome], low_df_flag = df < 30) %>%
  group_by(outcome_family) %>% mutate(p.adj = p.adjust(p.value, method = "BH")) %>% ungroup() %>%
  select(set_id, condition, outcome, estimand, hypothesis, contrast, estimate, std.error, conf.low,
         conf.high, df, p.value, p.adj, n_obs, n_clusters, outcome_family, low_df_flag)
write_csv(marginal_means_occupation, out_path("marginal_means_occupation_moderation.csv"))
for (h in paste0("H", 10:15)) {
  h_data <- marginal_means_occupation %>% filter(hypothesis == h) %>%
    mutate(outcome_label = outcome_labels[outcome]) %>%
    select(outcome_label, contrast, estimate, std.error, conf.low, conf.high, df, p.value, p.adj, n_obs, n_clusters)
  if (nrow(h_data) == 0) next
  modelsummary::datasummary_df(h_data, title = paste0(h, ": occupational moderation (pooled sample)"),
    notes = c("OLS with cluster-robust standard errors (CR2, clustered by respondent).",
              "df is the minimum per-coefficient CR2 df in the source model (conservative).",
              "p.adj is Benjamini-Hochberg-adjusted within the outcome's construct family."),
    output = out_path("tables_by_hypothesis", paste0(h, "_occupation_moderation.docx")))
}


# ==============================================================================
# PART B -- contrasts behind the "by comparison" figures (outcomes_headline: 8 outcomes)
# ==============================================================================
outcomes_idx_fig <- c("idx_riesgo", "idx_crecimiento", "idx_competencia", "idx_investment")
M <- list(
  set5  = load_models("set5"),         set2a = load_models("set2a_pooled"),
  set2b = load_models("set2b_pooled"), set4b = load_models("set4b_pooled"),
  set5a = load_models("set5a"),        set5b = load_models("set5b"), set5c = load_models("set5c")
) %>% map(~ compact(.x[outcomes_headline]))
rhs_main <- sets_baseline$formula_rhs[sets_baseline$set_id == "set5"]
.before_B <- ls()

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

write_xlsx(contrasts_all,        out_path("figure_data", "conference_contrasts.xlsx"))
write_xlsx(mm_gender,            out_path("figure_data", "mm_gender.xlsx"))
write_xlsx(mm_plan,              out_path("figure_data", "mm_plan.xlsx"))
write_xlsx(mm_parental,          out_path("figure_data", "mm_parental.xlsx"))
write_xlsx(gender_contrast,      out_path("figure_data", "gender_contrast.xlsx"))
write_xlsx(plan_contrast_women,  out_path("figure_data", "plan_contrast_women.xlsx"))
write_xlsx(parental_gender_gap,  out_path("figure_data", "parental_gender_gap.xlsx"))
write_xlsx(mm_plan_gender,       out_path("figure_data", "mm_plan_gender.xlsx"))
write_xlsx(plan_gender_gap,      out_path("figure_data", "plan_gender_gap.xlsx"))
write_xlsx(plan_contrast_gender_c,          out_path("figure_data", "plan_contrast_gender_childless.xlsx"))
write_xlsx(plan_did_c,                      out_path("figure_data", "plan_did_childless.xlsx"))
write_xlsx(parental_contrast_women,         out_path("figure_data", "parental_contrast_women.xlsx"))
write_xlsx(mm_plan_mothers,                 out_path("figure_data", "mm_plan_mothers.xlsx"))
write_xlsx(plan_contrast_mothers,           out_path("figure_data", "plan_contrast_mothers.xlsx"))
write_xlsx(plan_contrast_gender_m,          out_path("figure_data", "plan_contrast_gender_mothers.xlsx"))
write_xlsx(plan_did_m,                      out_path("figure_data", "plan_did_mothers.xlsx"))
write_xlsx(mm_parental_noplans_w,           out_path("figure_data", "mm_parental_noplans_women.xlsx"))
write_xlsx(parental_contrast_noplans_w,     out_path("figure_data", "parental_contrast_noplans_women.xlsx"))
write_xlsx(parental_contrast_gender_noplans, out_path("figure_data", "parental_contrast_gender_noplans.xlsx"))
write_xlsx(parental_did_noplans,            out_path("figure_data", "parental_did_noplans.xlsx"))

# ==============================================================================
# PART C -- four contrast blocks by outcome (women / men / women-minus-men), headline outcomes
# ==============================================================================
# Gender differences = "women minus men": with Hombre as ref level the factor order is
# c("Mujer","Hombre"); by="perfil_genero" returns (Mujer, Hombre) and hypothesis=~revpairwise
# gives row1 - row2. Do not switch to "pairwise" without re-checking the sign.
gender_diff <- function(comp_obj) hypotheses(comp_obj, hypothesis = ~revpairwise)

to_row <- function(mm_obj, block, contrast, row_label, group, df_used) {
  d <- as.data.frame(mm_obj)
  tibble(block = block, contrast = contrast, row_label = row_label, group = group,
         estimate = d$estimate, std.error = d$std.error, conf.low = d$conf.low,
         conf.high = d$conf.high, df = df_used, p.value = d$p.value)
}

build_gender_block <- function(model, newdata, var, ref, level, block, desc_prefix) {
  comp    <- level_contrast(model, var, ref, level, newdata, by = "perfil_genero")
  comp_df <- as.data.frame(comp)
  women   <- comp_df[comp_df$perfil_genero == "Mujer", , drop = FALSE]
  men     <- comp_df[comp_df$perfil_genero == "Hombre", , drop = FALSE]
  diff    <- gender_diff(comp)
  df_used <- model_df(model)
  bind_rows(
    to_row(women, block, paste(desc_prefix, "-- women"),          "Women",       "women", df_used),
    to_row(men,   block, paste(desc_prefix, "-- men"),             "Men",         "men",   df_used),
    to_row(diff,  block, paste(desc_prefix, "-- women minus men"), "Women - Men", "diff",  df_used)
  )
}

block_levels <- c(
  "Gender (among childless, no plans)",
  "Imminent fertility (among childless)",
  "Imminent fertility (among parents)",
  "Two children (among no childbearing plan)"
)
group_colors <- c(women = "#b4552d", men = "#1f5f63", diff = "#1c2b33")
group_labels <- c(women = "Women", men = "Men", diff = "Gender difference")

contrasts_by_outcome <- list()
for (outcome in outcomes_headline) {
  m_set2a <- M$set2a[[outcome]]   # = set2a_pooled
  m_set2b <- M$set2b[[outcome]]   # = set2b_pooled

  # 1. Women vs. men, among childless applicants stating no plans
  subset1 <- df %>% filter(perfil_parental == "actualmente no tiene hijos", perfil_plan_futuro == "no quiere")
  comp1   <- avg_comparisons(m_set2a, variables = list(perfil_genero = c("Hombre", "Mujer")),
                             newdata = subset1, df = model_df(m_set2a))
  rows_block1 <- to_row(comp1, block_levels[1], "Women vs. men -- childless, no family plans",
                        "Women - Men", "diff", model_df(m_set2a))
  # 2. Plans in 2 years vs. no plans, childless
  rows_block2 <- build_gender_block(m_set2b, df %>% filter(perfil_parental == "actualmente no tiene hijos"),
    "perfil_plan_futuro", "no quiere", "en 2 anos", block_levels[2], "Plans in 2 years vs. no plans (childless)")
  # 3. Plans in 2 years vs. no further plans, parents
  rows_block3 <- build_gender_block(m_set2b, df %>% filter(perfil_parental %in% c("tiene un hijo", "tiene dos hijos")),
    "perfil_plan_futuro", "no quiere", "en 2 anos", block_levels[3], "Plans in 2 years vs. no further plans (parents)")
  # 4. Two children vs. none, among applicants stating no plans
  rows_block4 <- build_gender_block(m_set2a, df %>% filter(perfil_plan_futuro == "no quiere"),
    "perfil_parental", "actualmente no tiene hijos", "tiene dos hijos", block_levels[4],
    "Two children vs. none (no family plans)")

  # BH within this outcome's family of contrasts
  contrasts_by_outcome[[outcome]] <- bind_rows(rows_block1, rows_block2, rows_block3, rows_block4) %>%
    mutate(block = factor(block, levels = block_levels),
           row_label = factor(row_label, levels = c("Women - Men", "Men", "Women")),
           group = factor(group, levels = c("women", "men", "diff")),
           p.adj = p.adjust(p.value, method = "BH"),
           significant = p.adj < 0.05,
           fill_hex = if_else(significant, group_colors[as.character(group)], "white"))
  contrasts_by_outcome[[outcome]] %>%
    select(block, contrast, group, estimate, std.error, conf.low, conf.high, df, p.value, p.adj, significant) %>%
    write_csv(out_path("by_outcome", paste0("contrasts_", outcome, ".csv")))
}


# ==============================================================================
# PART D -- pooling justification
# ==============================================================================
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

omnibus <- map_dfr(c("recommendation_probability", outcomes_idx_fig),
                    cluster_wald_test, rhs_reduced = rhs_main, rhs_full = rhs_int, data = df)

print(omnibus)
write_xlsx(omnibus, out_path("figure_data", "omnibus_pooling_test.xlsx"))
# ==============================================================================
# Objetos que necesita 11_figures_hypotheses_baseline.R
# ==============================================================================
bundle <- setdiff(ls(), c(.before_B, ".before_B", "M"))
save(list = bundle, file = out_path("figure_objects.rda"))
cat("Guardados", length(bundle), "objetos para 11 en", out_path("figure_objects.rda"), "\n")
finish_output()
