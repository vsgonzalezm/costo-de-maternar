# ============================================================================
# 12 - Ceiling / floor diagnostics  (STEP 1 of the ordinal robustness check)
# Purpose: establish whether the 7-point outcomes are compressed against the
#          scale bounds, and whether that compression is differential across
#          the groups being compared. Run BEFORE deciding on ordinal models.
# Pooled sample.
# ============================================================================

library(tidyverse)
library(here)

proj <- if (dir.exists(here::here("Encuesta Demanda", "data"))) here::here("Encuesta Demanda") else here::here()
source(file.path(proj, "code", "00_setup.R"))
init_output("12_robustness_ceiling")

df <- read_csv(analysis_ready_path, show_col_types = FALSE)

outcomes <- c(
  "recommendation_probability",
  "ambitious_score", "trustworthy_score", "competent_score",
  "risky_continuity_score", "salary_costly_score", "operational_costly_score",
  "long_term_commitment_score", "promising_score", "good_investment_score",
  "expected_late_absence", "expected_benefits_use", "expected_accommodations",
  "expected_quit_work", "expected_new_challenges", "expected_promotion"
)

# ---- 1. Overall distribution per outcome -----------------------------------

dist_tbl <- df %>%
  select(all_of(outcomes)) %>%
  pivot_longer(everything(), names_to = "outcome", values_to = "v") %>%
  filter(!is.na(v)) %>%
  group_by(outcome) %>%
  summarise(
    n        = n(),
    mean     = mean(v),
    sd       = sd(v),
    median   = median(v),
    pct_1    = 100 * mean(v == 1),
    pct_7    = 100 * mean(v == 7),
    pct_6or7 = 100 * mean(v >= 6),
    skew     = mean((v - mean(v))^3) / sd(v)^3,
    .groups  = "drop"
  ) %>%
  mutate(
    ceiling_risk = case_when(
      pct_7 >= 20 ~ "HIGH - ordinal model required",
      pct_7 >= 10 ~ "MODERATE - run ordinal robustness",
      TRUE        ~ "low - OLS defensible"
    )
  ) %>%
  arrange(desc(pct_7))

print(dist_tbl, n = Inf, width = Inf)

# ---- 2. Full response frequency table (the raw material) -------------------

freq_tbl <- df %>%
  select(all_of(outcomes)) %>%
  pivot_longer(everything(), names_to = "outcome", values_to = "v") %>%
  filter(!is.na(v)) %>%
  count(outcome, v) %>%
  group_by(outcome) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup() %>%
  select(-n) %>%
  pivot_wider(names_from = v, values_from = pct, names_prefix = "pct_") %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))

print(freq_tbl, n = Inf, width = Inf)

# ---- 3. DIFFERENTIAL ceiling: is the bound binding unequally? --------------
# This is the diagnostic that actually matters. If the share at 7 is similar
# across the groups being contrasted, compression cannot be generating a
# spurious null. If one group is markedly more piled up at 7, it can.

diff_ceiling <- function(data, group_var) {
  data %>%
    select(all_of(outcomes), grp = all_of(group_var)) %>%
    pivot_longer(all_of(outcomes), names_to = "outcome", values_to = "v") %>%
    filter(!is.na(v), !is.na(grp)) %>%
    group_by(outcome, grp) %>%
    summarise(pct_7 = 100 * mean(v == 7), mean = mean(v), .groups = "drop") %>%
    pivot_wider(names_from = grp, values_from = c(pct_7, mean)) %>%
    mutate(across(where(is.numeric), ~ round(.x, 1)))
}

cat("\n--- % at ceiling by PROFILE GENDER ---\n")
print(diff_ceiling(df, "perfil_genero"), n = Inf, width = Inf)

cat("\n--- % at ceiling by PARENTAL STATUS ---\n")
print(diff_ceiling(df, "perfil_parental"), n = Inf, width = Inf)

cat("\n--- % at ceiling by FAMILY PLANS ---\n")
print(diff_ceiling(df, "perfil_plan_futuro"), n = Inf, width = Inf)

# ---- 4. Visual ------------------------------------------------------------

p <- df %>%
  select(all_of(outcomes)) %>%
  pivot_longer(everything(), names_to = "outcome", values_to = "v") %>%
  filter(!is.na(v)) %>%
  ggplot(aes(x = factor(v))) +
  geom_bar(aes(y = after_stat(prop), group = 1)) +
  facet_wrap(~ outcome, ncol = 4) +
  labs(x = "Response", y = "Proportion",
       title = "Response distributions: check for pile-up at 7") +
  theme_minimal(base_size = 9)

# ---- 5. Save --------------------------------------------------------------


write_csv(dist_tbl, out_path("ceiling_summary.csv"))
write_csv(freq_tbl, out_path("response_frequencies.csv"))
ggsave(out_path("response_distributions.png"), p,
       width = 11, height = 8, dpi = 150)

finish_output()

# ---- DECISION RULE --------------------------------------------------------
# pct_7 < 10%           -> OLS is fine; note it and move on.
# pct_7 10-20%          -> run the ordered-probit robustness check on that outcome.
# pct_7 > 20%           -> ordinal model needed in the main text, not the appendix.
#
# SEPARATELY, and more important: if pct_7 differs by more than ~5 points
# between the groups you are contrasting (e.g. male vs female profiles on
# hiring recommendation), the ceiling is binding DIFFERENTIALLY and a linear
# model will misstate that specific contrast regardless of the overall pct_7.
# ============================================================================
