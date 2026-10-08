# ============================================================================
# 07 - First-order stochastic dominance + top-box probability shifts
# Purpose: establish which conclusions hold under ANY monotonic transformation
#          of the response scale, and therefore survive Bond & Lang (2019).
#          Run this BEFORE the ordered probit — if dominance holds, the probit
#          becomes optional rather than load-bearing.
# Pooled sample.
#
# Logic: if group A's CDF lies weakly below group B's at every cut point
#        (no crossings), then A > B in mean under every monotonic
#        cardinalisation of the scale. No normality, no metric assumption,
#        no sensitivity to ceiling compression.
# ============================================================================

library(tidyverse)
library(here)

set.seed(20260818)
N_BOOT <- 1000

project_dir <- if (dir.exists(here("Encuesta Demanda", "Data"))) here("Encuesta Demanda") else here()

df <- read_csv(
  file.path(project_dir, "Data", "processed", "base_demanda_analysis_ready.csv"),
  show_col_types = FALSE
)

outcomes <- c(
  "recommendation_probability",
  "ambitious_score", "trustworthy_score", "competent_score",
  "risky_continuity_score", "salary_costly_score", "operational_costly_score",
  "long_term_commitment_score", "promising_score", "good_investment_score",
  "expected_late_absence", "expected_benefits_use", "expected_accommodations",
  "expected_quit_work", "expected_new_challenges", "expected_promotion"
)

CUTS <- 1:6   # P(Y <= k) for k = 1..6 on a 7-point scale

# ---------------------------------------------------------------------------
# Contrast definitions.
# Each entry: a filter applied to the data, plus the grouping variable and the
# two levels to compare (level_hi = the group hypothesised to score HIGHER).
# Randomisation makes the marginal distributions directly comparable: all other
# attributes are balanced across levels of the contrast variable by design.
# ---------------------------------------------------------------------------

contrasts_spec <- list(
  list(
    id       = "gender_overall",
    label    = "Women vs. men (all profiles)",
    filter   = function(d) d,
    var      = "perfil_genero",
    level_hi = "Mujer",
    level_lo = "Hombre"
  ),
  list(
    id       = "plan2_vs_none_women_childless",
    label    = "Childless women: plans in 2 years vs. no plans",
    filter   = function(d) filter(d, perfil_genero == "Mujer",
                                 perfil_parental == "actualmente no tiene hijos"),
    var      = "perfil_plan_futuro",
    level_hi = "en 2 anos",
    level_lo = "no quiere"
  ),
  list(
    id       = "plan5_vs_none_women_childless",
    label    = "Childless women: plans in 5 years vs. no plans",
    filter   = function(d) filter(d, perfil_genero == "Mujer",
                                 perfil_parental == "actualmente no tiene hijos"),
    var      = "perfil_plan_futuro",
    level_hi = "en 5 anos",
    level_lo = "no quiere"
  ),
  list(
    id       = "twokids_vs_childless_women",
    label    = "Women: two children vs. childless",
    filter   = function(d) filter(d, perfil_genero == "Mujer"),
    var      = "perfil_parental",
    level_hi = "tiene dos hijos",
    level_lo = "actualmente no tiene hijos"
  ),
  list(
    id       = "twokids_vs_childless_men",
    label    = "Men: two children vs. childless (gendering check)",
    filter   = function(d) filter(d, perfil_genero == "Hombre"),
    var      = "perfil_parental",
    level_hi = "tiene dos hijos",
    level_lo = "actualmente no tiene hijos"
  )
)

# ---------------------------------------------------------------------------
# Core: CDF differences at every cut point, with a respondent-cluster bootstrap
# ---------------------------------------------------------------------------

cdf_diff <- function(d, outcome, var, hi, lo) {
  y  <- d[[outcome]]
  g  <- d[[var]]
  ok <- !is.na(y) & !is.na(g)
  y  <- y[ok]; g <- g[ok]
  yh <- y[g == hi]; yl <- y[g == lo]
  if (length(yh) < 5 || length(yl) < 5) return(rep(NA_real_, length(CUTS)))
  # F_hi(k) - F_lo(k).  Consistently NEGATIVE => hi group dominates (scores higher).
  vapply(CUTS, function(k) mean(yh <= k) - mean(yl <= k), numeric(1))
}

boot_cdf_diff <- function(d, outcome, var, hi, lo, n_boot = N_BOOT) {
  ids <- unique(d$numericalId)
  idx <- split(seq_len(nrow(d)), d$numericalId)
  replicate(n_boot, {
    draw <- sample(ids, length(ids), replace = TRUE)
    dd   <- d[unlist(idx[as.character(draw)], use.names = FALSE), , drop = FALSE]
    cdf_diff(dd, outcome, var, hi, lo)
  })
}

classify <- function(point, lo_ci, hi_ci) {
  # dominance requires a consistent sign across ALL cut points
  neg <- all(point <= 1e-9)          # hi group scores higher everywhere
  pos <- all(point >= -1e-9)         # hi group scores lower everywhere
  sig <- any(lo_ci > 0 | hi_ci < 0)  # at least one cut point separates
  if (neg && sig) "DOMINANCE: hi group higher"
  else if (pos && sig) "DOMINANCE: hi group lower"
  else if (neg || pos) "consistent sign, not significant"
  else "CDFs CROSS - ranking not transformation-invariant"
}

results <- list()
topbox  <- list()

for (spec in contrasts_spec) {
  d <- spec$filter(df)
  d <- d[d[[spec$var]] %in% c(spec$level_hi, spec$level_lo), , drop = FALSE]
  message(sprintf("-- %s  (n obs = %d, n respondents = %d)",
                  spec$label, nrow(d), n_distinct(d$numericalId)))

  for (o in outcomes) {
    pt <- cdf_diff(d, o, spec$var, spec$level_hi, spec$level_lo)
    if (all(is.na(pt))) next
    bs <- boot_cdf_diff(d, o, spec$var, spec$level_hi, spec$level_lo)
    lo <- apply(bs, 1, quantile, 0.025, na.rm = TRUE)
    hi <- apply(bs, 1, quantile, 0.975, na.rm = TRUE)

    results[[length(results) + 1]] <- tibble(
      contrast_id = spec$id, contrast = spec$label, outcome = o,
      cut = CUTS, cdf_diff = pt, ci_low = lo, ci_high = hi,
      verdict = classify(pt, lo, hi),
      n_obs = nrow(d), n_clusters = n_distinct(d$numericalId)
    )

    # Top-box / bottom-box shifts: distribution-based, transformation-free,
    # and far more intuitive for an audience than scale points or Cohen's d.
    y <- d[[o]]; g <- d[[spec$var]]
    ok <- !is.na(y) & !is.na(g); y <- y[ok]; g <- g[ok]
    topbox[[length(topbox) + 1]] <- tibble(
      contrast_id = spec$id, contrast = spec$label, outcome = o,
      p_top2_hi = mean(y[g == spec$level_hi] >= 6),
      p_top2_lo = mean(y[g == spec$level_lo] >= 6),
      d_top2    = mean(y[g == spec$level_hi] >= 6) - mean(y[g == spec$level_lo] >= 6),
      p_bot2_hi = mean(y[g == spec$level_hi] <= 2),
      p_bot2_lo = mean(y[g == spec$level_lo] <= 2),
      d_bot2    = mean(y[g == spec$level_hi] <= 2) - mean(y[g == spec$level_lo] <= 2)
    )
  }
}

res <- bind_rows(results)
tb  <- bind_rows(topbox)

# ---------------------------------------------------------------------------
# Summary: one row per contrast x outcome
# ---------------------------------------------------------------------------

summary_tbl <- res %>%
  group_by(contrast_id, contrast, outcome) %>%
  summarise(
    verdict       = first(verdict),
    max_abs_diff  = max(abs(cdf_diff)),
    n_cuts_signif = sum(ci_low > 0 | ci_high < 0),
    .groups = "drop"
  ) %>%
  left_join(tb %>% select(contrast_id, outcome, d_top2, d_bot2),
            by = c("contrast_id", "outcome")) %>%
  arrange(contrast_id, desc(max_abs_diff))

print(summary_tbl, n = Inf, width = Inf)

cat("\n\n==== DOMINANCE HOLDS FOR: ====\n")
summary_tbl %>%
  filter(str_detect(verdict, "^DOMINANCE")) %>%
  print(n = Inf, width = Inf)

cat("\n\n==== CDFs CROSS (ranking NOT transformation-invariant): ====\n")
summary_tbl %>%
  filter(str_detect(verdict, "CROSS")) %>%
  print(n = Inf, width = Inf)

# ---------------------------------------------------------------------------
# Plot: CDF differences by cut point. Bars entirely on one side of zero = dominance.
# ---------------------------------------------------------------------------

p <- res %>%
  filter(contrast_id %in% c("gender_overall", "twokids_vs_childless_women")) %>%
  ggplot(aes(x = factor(cut), y = cdf_diff, fill = contrast)) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                position = position_dodge(width = 0.8), width = 0.2) +
  facet_wrap(~ outcome, ncol = 4, scales = "free_y") +
  labs(x = "Cut point k  (P(Y <= k))", y = "CDF difference",
       title = "First-order stochastic dominance check",
       subtitle = "All bars on one side of zero = ranking holds under any monotonic transformation") +
  theme_minimal(base_size = 8) +
  theme(legend.position = "bottom")

out_dir <- file.path(project_dir, "Outputs", "Tables", "diagnostics")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

write_csv(res,         file.path(out_dir, "stochastic_dominance_full.csv"))
write_csv(summary_tbl, file.path(out_dir, "stochastic_dominance_summary.csv"))
write_csv(tb,          file.path(out_dir, "topbox_shifts.csv"))
ggsave(file.path(out_dir, "stochastic_dominance.png"), p, width = 12, height = 9, dpi = 150)

cat("\nSaved to", out_dir, "\n")

# ---------------------------------------------------------------------------
# HOW TO READ THE OUTPUT
#
# "DOMINANCE: hi group higher"  -> the strongest possible result. Report as:
#     "Women are rated higher on expected absenteeism under ANY cardinalisation
#      of the response scale (first-order stochastic dominance, all six cut
#      points, respondent-clustered bootstrap)."
#     This survives Bond & Lang (2019), Liddell & Kruschke (2018), and the
#     ceiling-compression objection simultaneously. No ordinal model needed.
#
# "CDFs CROSS" -> the mean ranking depends on cardinalisation. Do NOT claim a
#     ranking without an explicit scale assumption; fall back to the ordered
#     probit and flag the sensitivity.
#
# "consistent sign, not significant" -> underpowered, not contradicted.
#
# d_top2 / d_bot2 -> report these alongside means in the talk. A shift of
#     +0.08 in P(rated 6 or 7) is more legible to an audience than either
#     0.47 scale points or d = 0.27.
# ============================================================================
