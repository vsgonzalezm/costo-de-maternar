# ============================================================================
# 20 - Corrected confirmatory contrasts: H1/H2 (+H4-H7), H16, "most penalised group"
# Noviembre 2026. BASELINE models (pooled, occupation controlled). NO model is refit
# here: everything is read from 07_fit_models_baseline.R.
#
# Why this script exists (see analysis-plan-paper-demanda, section "Issues to resolve"):
#   1. H1 in script 10 uses SET 2b, which has no parental x plans term, so its
#      "childless women" estimate is really the plans effect for ALL women. Here
#      H1 (childless) and H2 (mothers) both come from SET 4b (gender x parental x
#      plans), so they are comparable, and their equality is tested formally.
#   2. H16 in script 10 compares married vs. single applicants among parents. The
#      preregistered H16 is about the MOTHERHOOD PENALTY (mother - childless) being
#      larger for single than for married women, with no such gap for men. Here it
#      is the difference of penalties (SET 4a), for women and women minus men.
#   3. H3 states that mothers with short-term plans are the most penalised group.
#      Script 10 never tests it. Here: predicted means for every gender x parental
#      x plans cell (SET 4b), contrasts of the target cell against the others, and
#      its rank.
#
# Script 10 is NOT modified (script 11 reads its figure objects). For the paper,
# the rows of H1, H2, H4-H7 and H16 in 10_marginal_means_pooled_all.csv are
# SUPERSEDED by 20_confirmatory_contrasts_corrected.csv, which also carries over
# the unchanged rows (Anchor, H3, H8, H9) and recomputes the BH adjustment over
# the full confirmatory set with corrected outcome families.
#
# Method: G-computation. avg_predictions() sets the focal attribute(s) to each
# level for every observed profile in the group, averages, and the contrasts are
# linear combinations of those averages with the CR2 cluster-robust vcov. Checked
# against script 10: H2 and H6 (hiring) reproduce exactly.
#
# Outputs: outputs/20_contrasts_corrected/
# ============================================================================
proj <- if (dir.exists(here::here("Encuesta Demanda", "data"))) here::here("Encuesta Demanda") else here::here()
source(file.path(proj, "code", "00_setup.R"))
suppressPackageStartupMessages(library(marginaleffects))
options(marginaleffects_safe = FALSE)
init_output("20_contrasts_corrected")

df <- load_analysis_data()
cat("Pooled: N profiles =", nrow(df), "| N respondents =", n_distinct(df$numericalId), "\n")

# ==============================================================================
# 1. OUTCOMES AND FAMILIES
# ==============================================================================
# Confirmatory outcomes: 16 items + the 3 indices that passed alpha >= .70.
# idx_investment is not preregistered: it is estimated too, but every row is
# flagged exploratory and left out of the BH adjustment.
outcomes_confirmatory <- c(outcomes_items, "idx_riesgo", "idx_crecimiento", "idx_competencia")
outcomes_exploratory  <- "idx_investment"
outcomes_all          <- c(outcomes_confirmatory, outcomes_exploratory)

# Corrected families: each item sits with its preregistered construct.
# (Script 10 put long_term_commitment_score under "competence"; it belongs to risk.)
outcome_family <- c(
  recommendation_probability = "hiring",
  salary_costly_score = "cost", operational_costly_score = "cost",
  expected_benefits_use = "cost", expected_accommodations = "cost",
  risky_continuity_score = "risk", expected_late_absence = "risk",
  expected_quit_work = "risk", long_term_commitment_score = "risk", idx_riesgo = "risk",
  ambitious_score = "growth", promising_score = "growth",
  expected_new_challenges = "growth", expected_promotion = "growth", idx_crecimiento = "growth",
  trustworthy_score = "competence", competent_score = "competence", idx_competencia = "competence",
  good_investment_score = "investment", idx_investment = "investment"
)
stopifnot(all(outcomes_all %in% names(outcome_family)))

# Outcomes where a HIGHER score is the worse evaluation (used only for ranking cells).
higher_is_worse <- c("salary_costly_score", "operational_costly_score", "expected_benefits_use",
                     "expected_accommodations", "risky_continuity_score", "expected_late_absence",
                     "expected_quit_work", "idx_riesgo")

primary_outcomes <- c("recommendation_probability", "idx_riesgo", "idx_crecimiento", "idx_competencia",
                      "operational_costly_score", "salary_costly_score",
                      "expected_benefits_use", "expected_accommodations")

# ==============================================================================
# 2. LEVELS, LABELS AND HELPERS
# ==============================================================================
lv_gen  <- c(W = "Mujer", M = "Hombre")
lv_par  <- c(none = "actualmente no tiene hijos", one = "tiene un hijo", two = "tiene dos hijos")
lv_plan <- c(none = "no quiere", y5 = "en 5 anos", y2 = "en 2 anos")
lv_civ  <- c(single = "soltero/a", married = "casado/a")
stopifnot(setequal(lv_gen, levels(df$perfil_genero)), setequal(lv_par, levels(df$perfil_parental)),
          setequal(lv_plan, levels(df$perfil_plan_futuro)), setequal(lv_civ, levels(df$perfil_estado_civil)))

model_df <- function(m) min(m$df)   # conservative: minimum CR2 df in the model, as in script 10

# One row per contrast, from a prediction object and a named list of weight vectors.
contrast_rows <- function(pred, weights, m) {
  W <- do.call(cbind, weights)
  h <- as.data.frame(hypotheses(pred, hypothesis = W, df = model_df(m)))
  tibble(contrast_id = names(weights), estimate = h$estimate, std.error = h$std.error,
         conf.low = h$conf.low, conf.high = h$conf.high, df = model_df(m), p.value = h$p.value)
}

n_cells <- function(data) tibble(n_obs = nrow(data), n_clusters = n_distinct(data$numericalId))

# ==============================================================================
# 3. BLOCK A - FERTILITY PLANS BY GENDER AND PARENTAL GROUP (SET 4b)
#    H1 (childless women), H2 (mothers), H4-H7 (women minus men), equality tests
# ==============================================================================
# Groups are the OBSERVED gender and parental group (childless / parent) of each
# profile; the plans attribute is set counterfactually to each of its 3 levels.
by_plan <- expand.grid(perfil_genero = unname(lv_gen), perfil_parental = unname(lv_par),
                       perfil_plan_futuro = unname(lv_plan), stringsAsFactors = FALSE) %>%
  mutate(by = paste(perfil_genero,
                    if_else(perfil_parental == lv_par[["none"]], "childless", "parent"),
                    perfil_plan_futuro, sep = "|"))

plan_defs <- tribble(
  ~contrast_id,          ~hypothesis, ~tier,           ~contrast,
  "cw_2_vs_0",           "H1",        "confirmatory",  "Childless women: plans in 2 years vs. no plans",
  "cw_5_vs_0",           "H1",        "confirmatory",  "Childless women: plans in 5 years vs. no plans",
  "cw_2_vs_5",           "H1",        "confirmatory",  "Childless women: plans in 2 years vs. in 5 years",
  "mo_2_vs_0",           "H2",        "confirmatory",  "Mothers: plans in 2 years vs. no plans",
  "mo_5_vs_0",           "H2",        "confirmatory",  "Mothers: plans in 5 years vs. no plans",
  "mo_2_vs_5",           "H2",        "confirmatory",  "Mothers: plans in 2 years vs. in 5 years",
  "did_c_2_vs_0",        "H4",        "confirmatory",  "Childless: plans in 2 years vs. no plans, women minus men",
  "did_c_5_vs_0",        "H5",        "confirmatory",  "Childless: plans in 5 years vs. no plans, women minus men",
  "did_p_2_vs_0",        "H6",        "confirmatory",  "Parents: plans in 2 years vs. no plans, women minus men",
  "did_p_5_vs_0",        "H7",        "confirmatory",  "Parents: plans in 5 years vs. no plans, women minus men",
  "cm_2_vs_0",           "H4 (men)",  "auxiliary",     "Childless men: plans in 2 years vs. no plans",
  "cm_5_vs_0",           "H5 (men)",  "auxiliary",     "Childless men: plans in 5 years vs. no plans",
  "fa_2_vs_0",           "H6 (men)",  "auxiliary",     "Fathers: plans in 2 years vs. no plans",
  "fa_5_vs_0",           "H7 (men)",  "auxiliary",     "Fathers: plans in 5 years vs. no plans",
  "did_c_2_vs_5",        "H4-H5",     "auxiliary",     "Childless: plans in 2 years vs. in 5 years, women minus men",
  "did_p_2_vs_5",        "H6-H7",     "auxiliary",     "Parents: plans in 2 years vs. in 5 years, women minus men",
  "eq_w_2_vs_0",         "H1 = H2",   "auxiliary",     "Women: 2-years penalty among mothers minus among childless",
  "eq_w_5_vs_0",         "H1 = H2",   "auxiliary",     "Women: 5-years penalty among mothers minus among childless",
  "eq_did_2_vs_0",       "H4 = H6",   "auxiliary",     "Gender gap in 2-years penalty: parents minus childless"
)

plan_block <- function(m) {
  pred <- avg_predictions(m, newdata = df, variables = list(perfil_plan_futuro = unname(lv_plan)),
                          by = by_plan, df = model_df(m))
  d <- as.data.frame(pred)
  cell <- function(g, grp, pl) {
    w <- as.numeric(d$by == paste(lv_gen[[g]], grp, lv_plan[[pl]], sep = "|"))
    stopifnot(sum(w) == 1); w
  }
  eff <- function(g, grp, hi, lo) cell(g, grp, hi) - cell(g, grp, lo)
  weights <- list(
    cw_2_vs_0 = eff("W", "childless", "y2", "none"),
    cw_5_vs_0 = eff("W", "childless", "y5", "none"),
    cw_2_vs_5 = eff("W", "childless", "y2", "y5"),
    mo_2_vs_0 = eff("W", "parent", "y2", "none"),
    mo_5_vs_0 = eff("W", "parent", "y5", "none"),
    mo_2_vs_5 = eff("W", "parent", "y2", "y5"),
    did_c_2_vs_0 = eff("W", "childless", "y2", "none") - eff("M", "childless", "y2", "none"),
    did_c_5_vs_0 = eff("W", "childless", "y5", "none") - eff("M", "childless", "y5", "none"),
    did_p_2_vs_0 = eff("W", "parent", "y2", "none") - eff("M", "parent", "y2", "none"),
    did_p_5_vs_0 = eff("W", "parent", "y5", "none") - eff("M", "parent", "y5", "none"),
    cm_2_vs_0 = eff("M", "childless", "y2", "none"),
    cm_5_vs_0 = eff("M", "childless", "y5", "none"),
    fa_2_vs_0 = eff("M", "parent", "y2", "none"),
    fa_5_vs_0 = eff("M", "parent", "y5", "none"),
    did_c_2_vs_5 = eff("W", "childless", "y2", "y5") - eff("M", "childless", "y2", "y5"),
    did_p_2_vs_5 = eff("W", "parent", "y2", "y5") - eff("M", "parent", "y2", "y5"),
    eq_w_2_vs_0 = eff("W", "parent", "y2", "none") - eff("W", "childless", "y2", "none"),
    eq_w_5_vs_0 = eff("W", "parent", "y5", "none") - eff("W", "childless", "y5", "none"),
    eq_did_2_vs_0 = (eff("W", "parent", "y2", "none") - eff("M", "parent", "y2", "none")) -
      (eff("W", "childless", "y2", "none") - eff("M", "childless", "y2", "none"))
  )
  contrast_rows(pred, weights, m)
}

# Sample behind each contrast (profiles and respondents in the groups involved).
is_childless <- df$perfil_parental == lv_par[["none"]]
is_woman     <- df$perfil_genero == lv_gen[["W"]]
plan_n <- bind_rows(
  tibble(contrast_id = c("cw_2_vs_0", "cw_5_vs_0", "cw_2_vs_5"), n_cells(df[is_childless & is_woman, ])),
  tibble(contrast_id = c("mo_2_vs_0", "mo_5_vs_0", "mo_2_vs_5"), n_cells(df[!is_childless & is_woman, ])),
  tibble(contrast_id = c("cm_2_vs_0", "cm_5_vs_0"), n_cells(df[is_childless & !is_woman, ])),
  tibble(contrast_id = c("fa_2_vs_0", "fa_5_vs_0"), n_cells(df[!is_childless & !is_woman, ])),
  tibble(contrast_id = c("did_c_2_vs_0", "did_c_5_vs_0", "did_c_2_vs_5"), n_cells(df[is_childless, ])),
  tibble(contrast_id = c("did_p_2_vs_0", "did_p_5_vs_0", "did_p_2_vs_5"), n_cells(df[!is_childless, ])),
  tibble(contrast_id = c("eq_w_2_vs_0", "eq_w_5_vs_0"), n_cells(df[is_woman, ])),
  tibble(contrast_id = "eq_did_2_vs_0", n_cells(df))
)

# ==============================================================================
# 4. BLOCK B - H16: MOTHERHOOD PENALTY BY MARITAL STATUS (SET 4a)
# ==============================================================================
# Penalty(g, c, k) = mean(g, c, k children) - mean(g, c, childless), with the
# parental attribute set counterfactually and averaged over the observed profiles
# of gender g and marital status c. "Any child" = equal-weight average of 1 and 2.
# H16 predicts single minus married < 0 for favourable outcomes (hiring, growth,
# competence) and > 0 for cost and risk, for women and for women minus men.
by_civ <- expand.grid(perfil_genero = unname(lv_gen), perfil_estado_civil = unname(lv_civ),
                      perfil_parental = unname(lv_par), stringsAsFactors = FALSE) %>%
  mutate(by = paste(perfil_genero, perfil_estado_civil, perfil_parental, sep = "|"))

h16_defs <- tribble(
  ~contrast_id,       ~hypothesis,      ~tier,          ~contrast,
  "h16_w_any",        "H16 (women)",    "confirmatory", "Motherhood penalty (any child), single minus married women",
  "h16_ddd_any",      "H16",            "confirmatory", "Parenthood penalty (any child), single minus married, women minus men",
  "h16_w_one",        "H16 (women)",    "auxiliary",    "Motherhood penalty (one child), single minus married women",
  "h16_w_two",        "H16 (women)",    "auxiliary",    "Motherhood penalty (two children), single minus married women",
  "h16_ddd_one",      "H16",            "auxiliary",    "Parenthood penalty (one child), single minus married, women minus men",
  "h16_ddd_two",      "H16",            "auxiliary",    "Parenthood penalty (two children), single minus married, women minus men",
  "h16_m_any",        "H16 (men)",      "auxiliary",    "Fatherhood penalty (any child), single minus married men",
  "pen_w_single",     "H16 component",  "auxiliary",    "Motherhood penalty (any child) among single women",
  "pen_w_married",    "H16 component",  "auxiliary",    "Motherhood penalty (any child) among married women",
  "pen_m_single",     "H16 component",  "auxiliary",    "Fatherhood penalty (any child) among single men",
  "pen_m_married",    "H16 component",  "auxiliary",    "Fatherhood penalty (any child) among married men",
  "marr_w_childless", "H16 component",  "auxiliary",    "Married minus single, childless women (marriage premium)",
  "marr_w_mothers",   "H16 component",  "auxiliary",    "Married minus single, mothers (any child)"
)

h16_block <- function(m) {
  pred <- avg_predictions(m, newdata = df, variables = list(perfil_parental = unname(lv_par)),
                          by = by_civ, df = model_df(m))
  d <- as.data.frame(pred)
  cell <- function(g, c, k) {
    w <- as.numeric(d$by == paste(lv_gen[[g]], lv_civ[[c]], lv_par[[k]], sep = "|"))
    stopifnot(sum(w) == 1); w
  }
  pen <- function(g, c, k) {
    if (k == "any") return(0.5 * (pen(g, c, "one") + pen(g, c, "two")))
    cell(g, c, k) - cell(g, c, "none")
  }
  gap <- function(g, k) pen(g, "single", k) - pen(g, "married", k)
  any_child <- function(g, c) 0.5 * (cell(g, c, "one") + cell(g, c, "two"))
  weights <- list(
    h16_w_any   = gap("W", "any"),
    h16_ddd_any = gap("W", "any") - gap("M", "any"),
    h16_w_one   = gap("W", "one"),
    h16_w_two   = gap("W", "two"),
    h16_ddd_one = gap("W", "one") - gap("M", "one"),
    h16_ddd_two = gap("W", "two") - gap("M", "two"),
    h16_m_any   = gap("M", "any"),
    pen_w_single  = pen("W", "single", "any"),
    pen_w_married = pen("W", "married", "any"),
    pen_m_single  = pen("M", "single", "any"),
    pen_m_married = pen("M", "married", "any"),
    marr_w_childless = cell("W", "married", "none") - cell("W", "single", "none"),
    marr_w_mothers   = any_child("W", "married") - any_child("W", "single")
  )
  contrast_rows(pred, weights, m)
}

is_single <- df$perfil_estado_civil == lv_civ[["single"]]
h16_n <- bind_rows(
  tibble(contrast_id = c("h16_w_any", "h16_w_one", "h16_w_two", "marr_w_childless", "marr_w_mothers"),
         n_cells(df[is_woman, ])),
  tibble(contrast_id = c("h16_ddd_any", "h16_ddd_one", "h16_ddd_two"), n_cells(df)),
  tibble(contrast_id = "h16_m_any", n_cells(df[!is_woman, ])),
  tibble(contrast_id = "pen_w_single",  n_cells(df[is_woman & is_single, ])),
  tibble(contrast_id = "pen_w_married", n_cells(df[is_woman & !is_single, ])),
  tibble(contrast_id = "pen_m_single",  n_cells(df[!is_woman & is_single, ])),
  tibble(contrast_id = "pen_m_married", n_cells(df[!is_woman & !is_single, ]))
)

# ==============================================================================
# 5. BLOCK C - "MOST PENALISED GROUP": CELL MEANS AND CONTRASTS (SET 4b)
# ==============================================================================
# Parental status AND plans are both set counterfactually; means are averaged over
# the observed profiles of each gender, so cells differ only in those attributes.
# Target = mothers (equal-weight average of one and two children) with plans in
# 2 years. Collapsed cells = {childless, mothers} x {no plans, 5 years, 2 years}.
by_cell <- expand.grid(perfil_genero = unname(lv_gen), perfil_parental = unname(lv_par),
                       perfil_plan_futuro = unname(lv_plan), stringsAsFactors = FALSE) %>%
  mutate(by = paste(perfil_genero, perfil_parental, perfil_plan_futuro, sep = "|"))

most_defs <- tribble(
  ~contrast_id,        ~hypothesis,          ~tier,          ~contrast,
  "t_vs_others",       "H3 (most penalised)", "confirmatory", "Mothers with plans in 2 years vs. mean of the other five groups of women",
  "t_vs_c_none",       "H3 (most penalised)", "auxiliary",    "Mothers, 2 years vs. childless women, no plans",
  "t_vs_c_y5",         "H3 (most penalised)", "auxiliary",    "Mothers, 2 years vs. childless women, 5 years",
  "t_vs_c_y2",         "H3 (most penalised)", "auxiliary",    "Mothers, 2 years vs. childless women, 2 years",
  "t_vs_m_none",       "H3 (most penalised)", "auxiliary",    "Mothers, 2 years vs. mothers, no plans",
  "t_vs_m_y5",         "H3 (most penalised)", "auxiliary",    "Mothers, 2 years vs. mothers, 5 years",
  "t2_vs_c_none",      "H3 (most penalised)", "auxiliary",    "Mothers of two, 2 years vs. childless women, no plans",
  "t_vs_fathers",      "H3 (most penalised)", "auxiliary",    "Mothers, 2 years vs. fathers, 2 years",
  "t_did_others",      "H3 (most penalised)", "auxiliary",    "Target vs. other five groups, women minus men"
)

most_block <- function(m) {
  pred <- avg_predictions(m, newdata = df,
                          variables = list(perfil_parental = unname(lv_par), perfil_plan_futuro = unname(lv_plan)),
                          by = by_cell, df = model_df(m))
  d <- as.data.frame(pred)
  cell <- function(g, k, pl) {
    if (k == "mother") return(0.5 * (cell(g, "one", pl) + cell(g, "two", pl)))
    w <- as.numeric(d$by == paste(lv_gen[[g]], lv_par[[k]], lv_plan[[pl]], sep = "|"))
    stopifnot(sum(w) == 1); w
  }
  others <- function(g) (cell(g, "none", "none") + cell(g, "none", "y5") + cell(g, "none", "y2") +
                           cell(g, "mother", "none") + cell(g, "mother", "y5")) / 5
  target <- function(g) cell(g, "mother", "y2")
  weights <- list(
    t_vs_others  = target("W") - others("W"),
    t_vs_c_none  = target("W") - cell("W", "none", "none"),
    t_vs_c_y5    = target("W") - cell("W", "none", "y5"),
    t_vs_c_y2    = target("W") - cell("W", "none", "y2"),
    t_vs_m_none  = target("W") - cell("W", "mother", "none"),
    t_vs_m_y5    = target("W") - cell("W", "mother", "y5"),
    t2_vs_c_none = cell("W", "two", "y2") - cell("W", "none", "none"),
    t_vs_fathers = target("W") - target("M"),
    t_did_others = (target("W") - others("W")) - (target("M") - others("M"))
  )
  means <- tibble(perfil_genero = sub("\\|.*", "", d$by),
                  perfil_parental = sapply(strsplit(d$by, "|", fixed = TRUE), `[`, 2),
                  perfil_plan_futuro = sapply(strsplit(d$by, "|", fixed = TRUE), `[`, 3),
                  estimate = d$estimate, std.error = d$std.error,
                  conf.low = d$conf.low, conf.high = d$conf.high)
  # Collapsed means (mothers = average of one and two children), with their SEs.
  grid6 <- expand.grid(g = names(lv_gen), k = c("none", "mother"), pl = names(lv_plan), stringsAsFactors = FALSE)
  w6 <- setNames(lapply(seq_len(nrow(grid6)), function(i) cell(grid6$g[i], grid6$k[i], grid6$pl[i])),
                 paste(grid6$g, grid6$k, grid6$pl, sep = "|"))
  collapsed <- contrast_rows(pred, w6, m) %>%
    separate(contrast_id, c("gender", "parental_group", "plans"), sep = "\\|")
  list(contrasts = contrast_rows(pred, weights, m), means = means, collapsed = collapsed)
}

most_n <- bind_rows(
  tibble(contrast_id = setdiff(most_defs$contrast_id, c("t_vs_fathers", "t_did_others")), n_cells(df[is_woman, ])),
  tibble(contrast_id = c("t_vs_fathers", "t_did_others"), n_cells(df))
)

# ==============================================================================
# 6. RUN ALL OUTCOMES
# ==============================================================================
models_4b <- compact(load_models("set4b_pooled"))
models_4a <- compact(load_models("set4a_pooled"))
stopifnot(all(outcomes_all %in% names(models_4b)), all(outcomes_all %in% names(models_4a)))

cat("Estimating contrasts for", length(outcomes_all), "outcomes...\n")
res_plan <- map_dfr(outcomes_all, ~ plan_block(models_4b[[.x]]) %>% mutate(outcome = .x))
res_h16  <- map_dfr(outcomes_all, ~ h16_block(models_4a[[.x]]) %>% mutate(outcome = .x))
res_most <- map(outcomes_all, ~ most_block(models_4b[[.x]])) %>% set_names(outcomes_all)

new_rows <- bind_rows(
  res_plan %>% left_join(plan_defs, by = "contrast_id") %>% left_join(plan_n, by = "contrast_id") %>%
    mutate(set_id = "set4b_pooled"),
  res_h16 %>% left_join(h16_defs, by = "contrast_id") %>% left_join(h16_n, by = "contrast_id") %>%
    mutate(set_id = "set4a_pooled"),
  imap_dfr(res_most, ~ .x$contrasts %>% mutate(outcome = .y)) %>%
    left_join(most_defs, by = "contrast_id") %>% left_join(most_n, by = "contrast_id") %>%
    mutate(set_id = "set4b_pooled")
) %>%
  mutate(source_script = "20", outcome_tier = if_else(outcome %in% outcomes_exploratory, "exploratory", "confirmatory"))
stopifnot(!anyNA(new_rows$contrast), !anyNA(new_rows$n_obs))

# ==============================================================================
# 7. SANITY CHECKS
# ==============================================================================
# (a) H2 / H6 must reproduce script 10 (same model, same estimand).
old <- read_csv(file.path(outputs_dir, "10_marginal_means_baseline", "10_marginal_means_pooled_all.csv"),
                show_col_types = FALSE)
chk <- function(id_new, hyp_old, pattern_old) {
  a <- new_rows %>% filter(contrast_id == id_new, outcome %in% outcomes_confirmatory) %>% select(outcome, new = estimate)
  b <- old %>% filter(hypothesis == hyp_old, str_detect(contrast, pattern_old)) %>% select(outcome, old = estimate)
  d <- inner_join(a, b, by = "outcome")
  stopifnot(nrow(d) == length(outcomes_confirmatory), max(abs(d$new - d$old)) < 1e-6)
}
chk("mo_2_vs_0", "H2", "en 2 anos"); chk("mo_5_vs_0", "H2", "en 5 anos")
chk("did_p_2_vs_0", "H6", "en 2 anos"); chk("did_p_5_vs_0", "H7", "en 5 anos")
# (b) Identity: married-vs-single among mothers minus among childless = -(H16 women).
idn <- new_rows %>% filter(contrast_id %in% c("h16_w_any", "marr_w_childless", "marr_w_mothers")) %>%
  select(outcome, contrast_id, estimate) %>% pivot_wider(names_from = contrast_id, values_from = estimate)
stopifnot(max(abs((idn$marr_w_mothers - idn$marr_w_childless) + idn$h16_w_any)) < 1e-8)
cat("Sanity checks passed: H2/H6/H7 reproduce script 10; H16 identity holds.\n")

# ==============================================================================
# 8. CONFIRMATORY TABLE (corrected rows + unchanged rows from script 10) AND BH
# ==============================================================================
carried <- old %>%
  filter(hypothesis %in% c("Anchor", "H3", "H8", "H9")) %>%
  transmute(
    contrast_id = paste0("s10_", hypothesis, "_", str_extract(contrast, "un hijo|dos hijos|Male")),
    hypothesis, tier = "confirmatory",
    contrast = case_when(
      hypothesis == "Anchor" ~ "Woman vs. man, both single, childless, no plans (additive model)",
      hypothesis == "H3" & str_detect(contrast, "un hijo")  ~ "Women: one child vs. childless",
      hypothesis == "H3" & str_detect(contrast, "dos hijos") ~ "Women: two children vs. childless",
      hypothesis == "H8" ~ "One child vs. childless, women minus men",
      hypothesis == "H9" ~ "Two children vs. childless, women minus men"),
    set_id, outcome, estimate, std.error, conf.low, conf.high, df, p.value, n_obs, n_clusters,
    source_script = "10", outcome_tier = "confirmatory"
  )

hyp_order <- c("Anchor", "H1", "H2", "H3", "H3 (most penalised)", "H4", "H5", "H6", "H7", "H8", "H9",
               "H16 (women)", "H16")

confirmatory <- bind_rows(
  carried,
  new_rows %>% filter(tier == "confirmatory", outcome_tier == "confirmatory")
) %>%
  mutate(outcome_family = outcome_family[outcome], outcome_label = outcome_labels[outcome]) %>%
  group_by(outcome_family) %>%
  mutate(p.adj = p.adjust(p.value, method = "BH")) %>%
  ungroup() %>%
  mutate(low_df_flag = df < 30) %>%
  arrange(match(hypothesis, hyp_order), contrast_id, match(outcome, outcomes_all)) %>%
  select(hypothesis, contrast, outcome, outcome_label, outcome_family, estimate, std.error, conf.low,
         conf.high, df, p.value, p.adj, n_obs, n_clusters, set_id, source_script, contrast_id, low_df_flag)

auxiliary <- new_rows %>%
  filter(tier == "auxiliary" | outcome_tier == "exploratory") %>%
  mutate(outcome_family = outcome_family[outcome], outcome_label = outcome_labels[outcome]) %>%
  arrange(match(contrast_id, c(plan_defs$contrast_id, h16_defs$contrast_id, most_defs$contrast_id)),
          match(outcome, outcomes_all)) %>%
  select(hypothesis, contrast, outcome, outcome_label, outcome_family, outcome_tier, estimate, std.error,
         conf.low, conf.high, df, p.value, n_obs, n_clusters, set_id, contrast_id)

# ==============================================================================
# 9. CELL MEANS AND RANK OF THE TARGET GROUP
# ==============================================================================
cell_means <- imap_dfr(res_most, ~ .x$means %>% mutate(outcome = .y)) %>%
  mutate(outcome_label = outcome_labels[outcome]) %>%
  select(outcome, outcome_label, everything())

cell_means_collapsed <- imap_dfr(res_most, ~ .x$collapsed %>% mutate(outcome = .y)) %>%
  mutate(outcome_label = outcome_labels[outcome],
         gender = recode(gender, W = "Women", M = "Men"),
         parental_group = recode(parental_group, none = "Childless", mother = "Parent (1 or 2 children)"),
         plans = recode(plans, none = "No plans", y5 = "In 5 years", y2 = "In 2 years")) %>%
  select(outcome, outcome_label, gender, parental_group, plans, estimate, std.error, conf.low, conf.high)

# Rank 1 = worst evaluated. Among the 6 groups of women and among all 12 groups.
ranks <- cell_means_collapsed %>%
  mutate(badness = if_else(outcome %in% higher_is_worse, estimate, -estimate)) %>%
  group_by(outcome) %>% mutate(rank_all12 = rank(-badness)) %>%
  group_by(outcome, gender) %>% mutate(rank_within_gender = rank(-badness)) %>% ungroup() %>%
  filter(gender == "Women", parental_group != "Childless", plans == "In 2 years") %>%
  transmute(outcome, outcome_label, target_mean = estimate,
            rank_among_6_women_groups = rank_within_gender, rank_among_12_groups = rank_all12)

# ==============================================================================
# 10. OUTPUTS
# ==============================================================================
write_csv(confirmatory, out_path("confirmatory_contrasts_corrected.csv"))
write_csv(auxiliary, out_path("auxiliary_contrasts.csv"))
write_csv(cell_means, out_path("cell_means_gender_parental_plan.csv"))
write_csv(cell_means_collapsed, out_path("cell_means_collapsed.csv"))
write_csv(ranks, out_path("most_penalised_ranks.csv"))

# Wide summary for the primary outcomes: estimate with stars from BH-adjusted p.
stars <- function(p) case_when(is.na(p) ~ "", p < .001 ~ "***", p < .01 ~ "**", p < .05 ~ "*", TRUE ~ "")
summary_primary <- confirmatory %>%
  filter(outcome %in% primary_outcomes) %>%
  mutate(cell = sprintf("%.2f%s", estimate, stars(p.adj))) %>%
  select(hypothesis, contrast, outcome_label, cell) %>%
  pivot_wider(names_from = outcome_label, values_from = cell)
write_csv(summary_primary, out_path("summary_primary_outcomes.csv"))

if (requireNamespace("openxlsx", quietly = TRUE)) {
  openxlsx::write.xlsx(
    list(Summary_primary = summary_primary, Confirmatory = confirmatory, Auxiliary = auxiliary,
         Cell_means_collapsed = cell_means_collapsed, Cell_means_18 = cell_means, Ranks = ranks),
    out_path("contrasts_corrected.xlsx"), overwrite = TRUE)
}

# Draft of main-text Figure 4: predicted means by parental group x plans, by gender.
fig_outcomes <- c("recommendation_probability", "idx_riesgo")
fig_data <- cell_means_collapsed %>%
  filter(outcome %in% fig_outcomes) %>%
  mutate(plans = factor(plans, levels = c("No plans", "In 5 years", "In 2 years")),
         outcome_label = factor(outcome_label, levels = outcome_labels[fig_outcomes]),
         gender = factor(gender, levels = c("Women", "Men")))
fig4 <- ggplot(fig_data, aes(x = plans, y = estimate, colour = gender, shape = parental_group,
                             group = interaction(gender, parental_group))) +
  geom_line(aes(linetype = parental_group), position = position_dodge(width = 0.35), linewidth = 0.5) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.35), size = 0.45) +
  facet_wrap(~ outcome_label, scales = "free_y") +
  scale_colour_manual(values = c(Women = "#B2182B", Men = "#2166AC")) +
  labs(x = "Applicant's family plans", y = "Predicted mean (1-7 scale), 95% CI", colour = NULL,
       shape = NULL, linetype = NULL,
       caption = "SET 4b, pooled sample. Parents = equal-weight average of one and two children.") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", legend.box = "vertical", panel.grid.minor = element_blank())
ggsave(out_path("fig4_draft_cell_means.png"), fig4, width = 8.5, height = 4.8, dpi = 300, bg = "white")

# ------------------------------------------------------------------------------
# Console summary
# ------------------------------------------------------------------------------
cat("\n=== Primary outcomes, confirmatory contrasts (stars = BH-adjusted p) ===\n")
print(as.data.frame(summary_primary %>% mutate(contrast = str_trunc(contrast, 58))), right = FALSE, row.names = FALSE)
cat("\n=== Equality of the plans penalty: mothers vs. childless women (raw p) ===\n")
print(as.data.frame(auxiliary %>% filter(contrast_id %in% c("eq_w_2_vs_0", "eq_did_2_vs_0"), outcome %in% primary_outcomes) %>%
                      transmute(contrast_id, outcome_label, estimate = round(estimate, 2), p = round(p.value, 3))), row.names = FALSE)
cat("\n=== Rank of mothers with plans in 2 years (1 = worst evaluated) ===\n")
print(as.data.frame(ranks %>% filter(outcome %in% primary_outcomes) %>% mutate(target_mean = round(target_mean, 2)) %>% select(-outcome)), row.names = FALSE)

finish_output()
