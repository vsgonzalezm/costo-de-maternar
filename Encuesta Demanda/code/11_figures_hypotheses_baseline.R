# ============================================================================
# 11 - Figures for the hypotheses, BASELINE models (pooled, occupation controlled)
# Author: Tania Hutt | Octubre 2026: fusiona las figuras de 08_conference_models_and_figures.R
#   y 10_fig_hiring.R. No ajusta modelos ni calcula contrastes: lee los objetos de
#   10_marginal_means_baseline.R (10_figure_objects.rda).
# Outputs:
#   outputs/11_figures_hypotheses_baseline/by_comparison/  figs 1-4 y 7-8 (una figura por comparacion)
#   outputs/11_figures_hypotheses_baseline/by_outcome/     una figura por outcome + matriz resumen
# Las distribuciones de respuesta y las CDF (antiguas fig 5 y 6) NO se repiten aqui:
#   viven en 12_robustness_ceiling.R y 13_robustness_stochastic_dominance.R.
# ============================================================================
proj <- if (dir.exists(here::here("Encuesta Demanda", "data"))) here::here("Encuesta Demanda") else here::here()
source(file.path(proj, "code", "00_setup.R"))
suppressPackageStartupMessages({ library(marginaleffects); library(patchwork); library(writexl) })
init_output("11_figures_hypotheses_baseline")

df <- load_analysis_data()
load(file.path(outputs_dir, "10_marginal_means_baseline", "10_figure_objects.rda"))  # objetos de las partes B y C de 10

# ---------------------------------------------------------------------------
# Paleta y tema (igual que custom.scss del deck)
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

# ---------------------------------------------------------------------------
# Outcomes, etiquetas y bloques tematicos de las figuras (8 outcomes headline)
# ---------------------------------------------------------------------------
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
# 4. FIGURES BY COMPARISON
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

ggsave(out_path("by_comparison", "fig1_anchor_dumbbell.png"), fig1,
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

ggsave(out_path("by_comparison", "fig1b_anchor_contrast.png"), fig1b,
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

ggsave(out_path("by_comparison", "fig2_fertility_horizon.png"), fig2,
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

ggsave(out_path("by_comparison", "fig2b_gendered_fertility_horizon.png"), fig2b,
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

ggsave(out_path("by_comparison", "fig2c_fertility_2yr_dumbbell.png"), fig2c,
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

ggsave(out_path("by_comparison", "fig2c_b_fertility_2yr_contrast.png"), fig2c_b,
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

ggsave(out_path("by_comparison", "fig2d_gendered_2yr_contrast_childless.png"), fig2d,
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

ggsave(out_path("by_comparison", "fig2e_fertility_2yr_mothers.png"), fig2e,
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

ggsave(out_path("by_comparison", "fig2f_gendered_2yr_contrast_mothers.png"), fig2f,
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

ggsave(out_path("by_comparison", "fig3_parental_threshold.png"), fig3,
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

ggsave(out_path("by_comparison", "fig3e_one_child_women.png"), fig3e,
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

ggsave(out_path("by_comparison", "fig3f_two_children_women.png"), fig3f,
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

ggsave(out_path("by_comparison", "fig3b_one_child_noplans.png"), fig3b,
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

ggsave(out_path("by_comparison", "fig3c_two_children_noplans.png"), fig3c,
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

ggsave(out_path("by_comparison", "fig3d_gendered_two_children_noplans.png"), fig3d,
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

ggsave(out_path("by_comparison", "fig4_null_moderation.png"), fig4,
       width = 14, height = 6.2, dpi = 300, bg = "white")

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

ggsave(out_path("by_comparison", "fig7_summary_matrix.png"), fig7,
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

ggsave(out_path("by_comparison", "fig8_heatmap_summary.png"), fig8,
       width = 16, height = 6.5, dpi = 300, bg = "white")
# ===========================================================================
# 5. FIGURES BY OUTCOME -- one figure per outcome, four contrast blocks stacked
#    (women / men / women-minus-men). Data: contrasts_by_outcome (script 10, PART C)
# ===========================================================================
theme_by_outcome <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        strip.placement = "outside", strip.text.y.left = element_text(angle = 0, hjust = 0, face = "bold"),
        legend.position = "bottom", plot.caption = element_text(hjust = 0))

plot_outcome_contrasts <- function(d, outcome_label, caption_extra = "", xlim = NULL) {
  p <- ggplot(d, aes(x = estimate, y = row_label)) +
    geom_vline(xintercept = 0, linewidth = 0.5, color = "black") +
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high, color = group), orientation = "y", width = 0, linewidth = 1) +
    geom_point(aes(color = group, fill = fill_hex), shape = 21, size = 3, stroke = 1) +
    scale_color_manual(values = group_colors, labels = group_labels, breaks = names(group_colors), name = NULL) +
    scale_fill_identity() +
    facet_grid(block ~ ., scales = "free_y", space = "free_y", switch = "y", labeller = label_wrap_gen(width = 26)) +
    labs(x = sprintf("Difference in %s (scale points)", tolower(outcome_label)), y = NULL, title = outcome_label,
         caption = str_wrap(paste(
           "95% CI from cluster-robust (CR2) SEs, clustered by respondent.",
           "Filled points indicate statistical significance at alpha = .05 (Benjamini-Hochberg adjustment",
           "within this outcome's family of contrasts).", caption_extra), width = 90)) +
    theme_by_outcome
  if (!is.null(xlim)) p <- p + scale_x_continuous(limits = xlim)
  p
}

for (outcome in outcomes_headline) {
  p <- plot_outcome_contrasts(contrasts_by_outcome[[outcome]], LBL[[outcome]])
  ggsave(out_path("by_outcome", paste0("fig_hiring_", outcome, ".png")), p, width = 9, height = 6, dpi = 300, bg = "white")
}

# Second set: without the base gender contrast (block 1) and with one shared x-axis range
block_levels_no_anchor <- block_levels[-1]
no_anchor_all <- bind_rows(contrasts_by_outcome, .id = "outcome") %>%
  filter(block != block_levels[1]) %>%
  mutate(block = factor(as.character(block), levels = block_levels_no_anchor))
est_range <- range(c(no_anchor_all$conf.low, no_anchor_all$conf.high, 0))
shared_xlim <- est_range + c(-1, 1) * diff(est_range) * 0.05

for (o in outcomes_headline) {
  p <- plot_outcome_contrasts(filter(no_anchor_all, outcome == o), LBL[[o]],
         "Excludes the base gender contrast. Estimate axis fixed to the same range across all outcomes.", shared_xlim)
  ggsave(out_path("by_outcome", paste0("fig_hiring_no_anchor_", o, ".png")), p, width = 9, height = 5, dpi = 300, bg = "white")
}

# ---- Summary matrix: direction + significance only (BH pooled across the grid) ----
summary_outcomes <- c("recommendation_probability", "idx_competencia", "idx_crecimiento",
                      "operational_costly_score", "expected_accommodations", "idx_riesgo")

matrix_dat <- bind_rows(contrasts_by_outcome[summary_outcomes], .id = "outcome") %>%
  mutate(
    sub_col = factor(recode(group, women = "Women", men = "Men", diff = "Gender diff"), levels = c("Women", "Men", "Gender diff")),
    outcome_f = factor(outcome, levels = summary_outcomes),
    label = factor(unname(LBL[outcome]), levels = rev(unname(LBL[summary_outcomes]))),
    direction = if_else(estimate > 0, "Higher", "Lower"),
    p.adj_matrix = p.adjust(p.value, method = "BH"),
    sig = p.adj_matrix < .05,
    stars = sig_stars(p.adj_matrix)
  )
women_men_dat <- matrix_dat %>% filter(sub_col != "Gender diff") %>% mutate(cell_label = if_else(sig, stars, "n.s."))
diff_dat <- matrix_dat %>% filter(sub_col == "Gender diff") %>%
  mutate(higher_who = if_else(estimate > 0, "Women higher", "Men higher"),
         cell_label = if_else(sig, paste0(higher_who, "\n", stars), "n.s."))
dir_colors <- c(Higher = group_colors[["women"]], Lower = group_colors[["men"]])

fig_matrix <- ggplot(mapping = aes(x = sub_col, y = label)) +
  geom_tile(data = filter(women_men_dat, !sig), fill = "grey92", colour = "white", linewidth = 0.6) +
  geom_text(data = filter(women_men_dat, !sig), aes(label = cell_label), colour = "grey45", size = 2.5) +
  geom_tile(data = filter(diff_dat, !sig), fill = "grey92", colour = "white", linewidth = 0.6) +
  geom_text(data = filter(diff_dat, !sig), aes(label = cell_label), colour = "grey45", size = 2.5) +
  geom_tile(data = filter(women_men_dat, sig), aes(fill = direction), colour = "white", linewidth = 0.6) +
  geom_tile(data = filter(women_men_dat, sig), fill = NA, colour = "#1c2b33", linewidth = 1) +
  geom_text(data = filter(women_men_dat, sig), aes(label = cell_label), colour = "#1c2b33", size = 3.2, fontface = "bold") +
  geom_tile(data = filter(diff_dat, sig), aes(fill = direction), colour = "white", linewidth = 0.6, alpha = 0.35) +
  geom_tile(data = filter(diff_dat, sig), fill = NA, colour = "#1c2b33", linewidth = 1) +
  geom_text(data = filter(diff_dat, sig), aes(label = cell_label), colour = "#1c2b33", fontface = "bold", size = 2.5, lineheight = 0.9) +
  facet_grid(~ block, scales = "free_x", space = "free_x", labeller = label_wrap_gen(width = 20)) +
  scale_y_discrete(limits = levels(matrix_dat$label)) +   # fija el orden pedido (ggplot lo reordena por capa si falta una categoria)
  scale_fill_manual(values = dir_colors, name = "Direction", na.translate = FALSE) +
  labs(x = NULL, y = NULL, title = "Summary: direction and significance of every key contrast, by outcome",
       caption = str_wrap(paste(
         "Direction only, no magnitude (outcomes sit on different scales). Grey n.s. cells: not significant.",
         "Women/Men tiles: outline + stars mean that within-gender effect is significant. Gender diff: which",
         "gender's effect is larger, shown only when that difference is itself significant. Benjamini-Hochberg",
         "adjustment pooled across every cell in this grid. * p.adj<.05  ** p.adj<.01  *** p.adj<.001."), width = 105)) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(size = 8), panel.grid = element_blank(),
        strip.text = element_text(face = "bold", size = 8), plot.caption = element_text(hjust = 0))
ggsave(out_path("by_outcome", "fig_hiring_summary_matrix.png"), fig_matrix, width = 12, height = 6, dpi = 300, bg = "white")
matrix_dat %>%
  select(outcome = outcome_f, label, block, sub_col, group, direction, estimate, std.error, conf.low, conf.high,
         df, p.value, p.adj = p.adj_matrix, sig, stars) %>%
  arrange(outcome, block, sub_col) %>% write_csv(out_path("by_outcome", "fig_hiring_summary_matrix.csv"))

finish_output()
