# ============================================================================
# 10 - Prototype figure: every experimental contrast, one figure per outcome
# Author: Tania Hutt
# Date: agosto 2026
# Purpose: Test whether organizing a talk BY OUTCOME (one figure per outcome,
#          every contrast stacked on one axis) reads better than the current
#          BY COMPARISON layout in 05c_marginal_means_pooled.R / 06_Figures.R.
#          Loops over the eight outcomes used in 08_conference_models_and_
#          figures.R (outcomes_all there) and saves one figure + one table per
#          outcome.
# ============================================================================

library(tidyverse)
library(here)
library(estimatr)
library(marginaleffects)

# ==============================================================================
# 0. OUTCOMES TO PLOT -- mirrors outcomes_all in 08_conference_models_and_figures.R
# ==============================================================================

outcomes_to_run <- c(
  "recommendation_probability", "operational_costly_score",
  "expected_benefits_use", "expected_accommodations",
  "idx_riesgo", "idx_crecimiento", "idx_competencia", "idx_investment"
)

outcome_labels <- c(
  "recommendation_probability" = "Hiring recommendation",
  "idx_riesgo" = "Risk index", "idx_crecimiento" = "Growth index",
  "idx_competencia" = "Competence index", "idx_investment" = "Investment index",
  "ambitious_score" = "Ambitious", "trustworthy_score" = "Trustworthy",
  "competent_score" = "Competent", "risky_continuity_score" = "Risky (job continuity)",
  "salary_costly_score" = "Costly (salary)", "operational_costly_score" = "Operationally costly",
  "long_term_commitment_score" = "Committed long-term", "promising_score" = "Promising",
  "good_investment_score" = "Good investment",
  "expected_late_absence" = "Expected: late/absent", "expected_benefits_use" = "Expected: use benefits",
  "expected_accommodations" = "Use accommodations", "expected_quit_work" = "Expected: quit",
  "expected_new_challenges" = "Expected: take on new challenges", "expected_promotion" = "Expected: be promoted"
)

project_dir <- if (dir.exists(here("Encuesta Demanda", "Data"))) {
  here("Encuesta Demanda")
} else {
  here()
}

figures_dir <- file.path(project_dir, "Outputs", "Figures")
tables_dir  <- file.path(project_dir, "Outputs", "Tables")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir,  recursive = TRUE, showWarnings = FALSE)

# The presented conference deliverables (the 8 block figures, section 8, and
# the summary matrix, section 10) are saved here instead of Outputs/. The
# second, unpresented figure set (section 9, "no anchor") stays under
# Outputs/Figures + Outputs/Tables since it isn't part of the conference deck.
conference_dir <- file.path(project_dir, "Presentations", "conference_figures")
dir.create(conference_dir, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# 1. DATA + SUM CONTRASTS -- copied verbatim from 04_analysis_cluster_robust_models.R
# ==============================================================================

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
    # idx_* mirror 08_conference_models_and_figures.R / 07_Indices.R exactly
    # (constructos + alpha results, agosto 2026): long_term_commitment_score
    # is reverse-direction relative to the rest of "riesgo" (higher
    # commitment = lower risk), so it's reverse-scored (1-7 scale) before
    # entering that index.
    long_term_commitment_score_rev = 8 - long_term_commitment_score,
    idx_riesgo = rowMeans(across(c(risky_continuity_score, expected_late_absence,
                                   expected_quit_work, long_term_commitment_score_rev)), na.rm = TRUE),
    idx_crecimiento = rowMeans(across(c(ambitious_score, promising_score,
                                        expected_new_challenges, expected_promotion)), na.rm = TRUE),
    idx_competencia = rowMeans(across(c(trustworthy_score, competent_score)), na.rm = TRUE),
    idx_investment  = rowMeans(across(c(recommendation_probability, good_investment_score)), na.rm = TRUE),
    perfil_genero         = apply_sum_contrasts(factor(perfil_genero, levels = c("Hombre", "Mujer")), ref_level = "Hombre"),
    perfil_estado_civil    = apply_sum_contrasts(factor(perfil_estado_civil, levels = c("soltero/a", "casado/a")), ref_level = "soltero/a"),
    perfil_parental        = apply_sum_contrasts(factor(perfil_parental, levels = c("actualmente no tiene hijos", "tiene un hijo", "tiene dos hijos")), ref_level = "actualmente no tiene hijos"),
    # Sin tilde: perfil_plan_futuro pasa por normalizar_texto() en 01_..., que convierte
    # "años" -> "anos". Con tilde aqui, "en 2/5 años" no calzaba con ningun nivel y quedaba
    # NA para el 66% de las filas, tumbando practicamente todos los modelos (ver revision).
    perfil_plan_futuro     = apply_sum_contrasts(factor(perfil_plan_futuro, levels = c("no quiere", "en 2 anos", "en 5 anos")), ref_level = "no quiere"),
    perfil_exp             = apply_sum_contrasts(factor(perfil_exp, levels = c("6 años", "7 años", "8 años"))),
    perfil_desempeno       = apply_sum_contrasts(as.factor(perfil_desempeno)),
    pantalla_orden         = apply_sum_contrasts(as.factor(pantalla_orden)),
    template_contexto      = apply_sum_contrasts(as.factor(template_contexto)),
    condicion_ocupacional  = apply_sum_contrasts(factor(VISITAS_1_BRAZO, levels = c("profesional", "tecnico")), ref_level = "profesional")
  )

# ==============================================================================
# 2. MODELS -- pooled sample, set2a (gender x parental) and set2b (gender x
#    family plans); condicion_ocupacional entered as an additive control, same
#    formulas as set2a_pooled / set2b_pooled in 04_analysis_cluster_robust_models.R
#    (fit inside the per-outcome loop below, section 8)
# ==============================================================================

controls <- paste(
  "perfil_estado_civil + perfil_exp + perfil_desempeno +",
  "condicion_ocupacional + pantalla_orden + template_contexto"
)

model_df <- function(model) min(model$df)

# ==============================================================================
# 3. CONTRAST HELPERS
# ==============================================================================
# Gender differences are always reported as "women minus men". With
# apply_sum_contrasts(perfil_genero, ref_level = "Hombre") the factor's level
# order becomes c("Mujer", "Hombre"), and a `by = "perfil_genero"` grouped
# avg_comparisons() therefore returns rows in that order (Mujer, Hombre).
# hypothesis = "revpairwise" on a 2-row object returns (row 1) - (row 2), i.e.
# Mujer - Hombre = women - men. Verified against a toy model before writing
# this script; do not switch to "pairwise" (which flips the sign) without
# re-checking.

level_contrast <- function(model, var, ref, level, newdata, by = NULL) {
  vars <- setNames(list(c(ref, level)), var)
  avg_comparisons(model, variables = vars, newdata = newdata, by = by, df = model_df(model))
}

gender_diff <- function(comp_obj) {
  hypotheses(comp_obj, hypothesis = ~revpairwise)
}

to_row <- function(mm_obj, block, contrast, row_label, group, df_used) {
  d <- as.data.frame(mm_obj)
  tibble(
    block     = block,
    contrast  = contrast,
    row_label = row_label,
    group     = group,
    estimate  = d$estimate,
    std.error = d$std.error,
    conf.low  = d$conf.low,
    conf.high = d$conf.high,
    df        = df_used,
    p.value   = d$p.value
  )
}

# Shared shape for blocks 2-4: a level-vs-reference contrast, split by gender,
# reported as women / men / (women - men).
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

# ==============================================================================
# 8. LOOP -- fit set2a/set2b, build the four contrast blocks, plot, save --
#    once per outcome (outcomes_to_run, section 0)
# ==============================================================================

contrasts_by_outcome <- list()

for (outcome in outcomes_to_run) {

  cat("\n==== ", outcome, " ====\n", sep = "")
  outcome_label <- unname(outcome_labels[outcome])

  # ---- 2. MODELS ----
  fml_set2a <- as.formula(paste(outcome, "~ perfil_genero * perfil_parental +", controls, "+ perfil_plan_futuro"))
  fml_set2b <- as.formula(paste(outcome, "~ perfil_genero * perfil_plan_futuro +", controls, "+ perfil_parental"))

  m_set2a <- lm_robust(fml_set2a, data = df, clusters = df$numericalId, se_type = "CR2")
  m_set2b <- lm_robust(fml_set2b, data = df, clusters = df$numericalId, se_type = "CR2")

  # ---- 4. THE FOUR CONTRAST BLOCKS ----

  # 1. Women vs. men, among childless applicants stating no plans.
  subset1 <- df %>% filter(perfil_parental == "actualmente no tiene hijos", perfil_plan_futuro == "no quiere")
  comp1   <- avg_comparisons(m_set2a, variables = list(perfil_genero = c("Hombre", "Mujer")),
                              newdata = subset1, df = model_df(m_set2a))
  rows_block1 <- to_row(comp1, block_levels[1], "Women vs. men -- childless, no family plans",
                         "Women - Men", "diff", model_df(m_set2a))

  # 2. Plans in 2 years vs. no plans, among childless.
  rows_block2 <- build_gender_block(
    m_set2b,
    df %>% filter(perfil_parental == "actualmente no tiene hijos"),
    "perfil_plan_futuro", "no quiere", "en 2 anos",
    block_levels[2], "Plans in 2 years vs. no plans (childless)"
  )

  # 3. Plans in 2 years vs. no further plans, among parents.
  rows_block3 <- build_gender_block(
    m_set2b,
    df %>% filter(perfil_parental %in% c("tiene un hijo", "tiene dos hijos")),
    "perfil_plan_futuro", "no quiere", "en 2 anos",
    block_levels[3], "Plans in 2 years vs. no further plans (parents)"
  )

  # 4. Two children vs. none, among applicants stating no plans.
  rows_block4 <- build_gender_block(
    m_set2a,
    df %>% filter(perfil_plan_futuro == "no quiere"),
    "perfil_parental", "actualmente no tiene hijos", "tiene dos hijos",
    block_levels[4], "Two children vs. none (no family plans)"
  )

  # ---- 5. ASSEMBLE + BH ADJUSTMENT (within this outcome's family of contrasts) ----

  contrasts_all <- bind_rows(rows_block1, rows_block2, rows_block3, rows_block4) %>%
    mutate(
      block       = factor(block, levels = block_levels),
      row_label   = factor(row_label, levels = c("Women - Men", "Men", "Women")),
      group       = factor(group, levels = c("women", "men", "diff")),
      p.adj       = p.adjust(p.value, method = "BH"),
      significant = p.adj < 0.05,
      fill_hex    = if_else(significant, group_colors[as.character(group)], "white")
    )

  contrasts_by_outcome[[outcome]] <- contrasts_all

  # ---- 6. FIGURE ----

  p <- ggplot(contrasts_all, aes(x = estimate, y = row_label)) +
    geom_vline(xintercept = 0, linewidth = 0.5, color = "black") +
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high, color = group), orientation = "y", width = 0, linewidth = 1) +
    geom_point(aes(color = group, fill = fill_hex), shape = 21, size = 3, stroke = 1) +
    scale_color_manual(values = group_colors, labels = group_labels, breaks = names(group_colors), name = NULL) +
    scale_fill_identity() +
    facet_grid(block ~ ., scales = "free_y", space = "free_y", switch = "y",
               labeller = label_wrap_gen(width = 26)) +
    labs(
      x = sprintf("Difference in %s (scale points)", tolower(outcome_label)),
      y = NULL,
      title = outcome_label,
      caption = str_wrap(paste(
        "95% CI from cluster-robust (CR2) SEs, clustered by respondent.",
        "Filled points indicate statistical significance at α = .05 (Benjamini-Hochberg adjustment",
        "within this outcome's family of contrasts)."
      ), width = 90)
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.placement    = "outside",
      strip.text.y.left  = element_text(angle = 0, hjust = 0, face = "bold"),
      legend.position    = "bottom",
      plot.caption       = element_text(hjust = 0)
    )

  fig_path <- file.path(conference_dir, paste0("fig_hiring_", outcome, ".png"))
  ggsave(fig_path, plot = p, width = 9, height = 6, dpi = 300, bg = "white")

  # ---- 7. UNDERLYING ESTIMATES ----

  table_path <- file.path(tables_dir, paste0("fig_hiring_contrasts_", outcome, ".csv"))
  contrasts_all %>%
    select(block, contrast, group, estimate, std.error, conf.low, conf.high, df, p.value, p.adj, significant) %>%
    write_csv(table_path)

  cat("Saved:", fig_path, "\n")
  cat("Saved:", table_path, "\n")
}

# ==============================================================================
# 9. SECOND FIGURE SET -- same contrasts, minus the base gender contrast
#    (block 1, "Gender gap -- childless, no plans"), and with the estimate
#    axis fixed to the same range on every outcome so the figures are
#    directly comparable to each other.
# ==============================================================================

block_levels_no_anchor <- block_levels[-1]

contrasts_no_anchor_all <- bind_rows(contrasts_by_outcome, .id = "outcome") %>%
  filter(block != block_levels[1]) %>%
  mutate(block = factor(as.character(block), levels = block_levels_no_anchor))

# Shared estimate-axis range across all outcomes (5% padding, like ggplot's
# own default expansion), so every panel in this set uses the same x scale.
est_range <- range(c(contrasts_no_anchor_all$conf.low, contrasts_no_anchor_all$conf.high, 0))
est_pad   <- diff(est_range) * 0.05
shared_xlim <- est_range + c(-est_pad, est_pad)

for (outcome in outcomes_to_run) {

  outcome_label <- unname(outcome_labels[outcome])
  contrasts_no_anchor <- contrasts_no_anchor_all %>% filter(outcome == !!outcome)

  p_no_anchor <- ggplot(contrasts_no_anchor, aes(x = estimate, y = row_label)) +
    geom_vline(xintercept = 0, linewidth = 0.5, color = "black") +
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high, color = group), orientation = "y", width = 0, linewidth = 1) +
    geom_point(aes(color = group, fill = fill_hex), shape = 21, size = 3, stroke = 1) +
    scale_color_manual(values = group_colors, labels = group_labels, breaks = names(group_colors), name = NULL) +
    scale_fill_identity() +
    scale_x_continuous(limits = shared_xlim) +
    facet_grid(block ~ ., scales = "free_y", space = "free_y", switch = "y",
               labeller = label_wrap_gen(width = 26)) +
    labs(
      x = sprintf("Difference in %s (scale points)", tolower(outcome_label)),
      y = NULL,
      title = outcome_label,
      caption = str_wrap(paste(
        "95% CI from cluster-robust (CR2) SEs, clustered by respondent.",
        "Filled points indicate statistical significance at α = .05 (Benjamini-Hochberg adjustment",
        "within this outcome's family of contrasts). Excludes the base gender contrast (see block",
        "figures above). Estimate axis fixed to the same range across all outcomes for comparability."
      ), width = 90)
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.placement    = "outside",
      strip.text.y.left  = element_text(angle = 0, hjust = 0, face = "bold"),
      legend.position    = "bottom",
      plot.caption       = element_text(hjust = 0)
    )

  fig_path_no_anchor <- file.path(figures_dir, paste0("fig_hiring_no_anchor_", outcome, ".png"))
  ggsave(fig_path_no_anchor, plot = p_no_anchor, width = 9, height = 5, dpi = 300, bg = "white")

  table_path_no_anchor <- file.path(tables_dir, paste0("fig_hiring_contrasts_no_anchor_", outcome, ".csv"))
  contrasts_no_anchor %>%
    select(block, contrast, group, estimate, std.error, conf.low, conf.high, df, p.value, p.adj, significant) %>%
    write_csv(table_path_no_anchor)

  cat("Saved:", fig_path_no_anchor, "\n")
  cat("Saved:", table_path_no_anchor, "\n")
}

# ==============================================================================
# 10. SUMMARY MATRIX -- outcome x contrast grid, DIRECTION + SIGNIFICANCE ONLY
#     (no magnitude fill -- estimates live on different outcome scales, so a
#     shared continuous fill can make a small-scale outcome's effect look
#     trivial next to a large-scale one even when both are equally real;
#     direction + BH-adjusted significance is what's actually comparable
#     across outcomes here). Every non-significant cell -- Women, Men, AND
#     Gender diff alike -- gets the same flat neutral "n.s." shading; only a
#     significant cell gets a direction-coloured tile. Columns, per block:
#     Women, then Men (direction + significance of that within-gender
#     effect), then a "Gender diff" column stating which gender's effect is
#     larger -- shown only when that gendered difference is itself
#     statistically significant. The Anchor block is a direct women-vs-men
#     contrast (not decomposed by gender), so it only has the Gender diff
#     column.
#     Restricted to the 6 outcomes/order requested for this summary (a subset
#     of the 8 run in the full block figures above): hiring recommendation,
#     competence index, growth index, operationally costly, use
#     accommodations, risk index.
#     p.adj is a FRESH pool across every cell shown in THIS grid -- not
#     reusing each outcome's own BH pool (computed above, within that
#     outcome's 7-contrast family) -- since this grid puts many
#     different-origin tests side by side and needs one consistent
#     correction scope across the whole thing (same reasoning as
#     fig8_heatmap_summary.png's comment in 08_conference_models_and_figures.R).
# ==============================================================================

sig_stars <- function(p_adj) {
  case_when(p_adj < .001 ~ "***", p_adj < .01 ~ "**", p_adj < .05 ~ "*", TRUE ~ "")
}

summary_outcomes <- c(
  "recommendation_probability", "idx_competencia", "idx_crecimiento",
  "operational_costly_score", "expected_accommodations", "idx_riesgo"
)

matrix_dat <- bind_rows(contrasts_by_outcome[summary_outcomes], .id = "outcome") %>%
  mutate(
    sub_col   = factor(recode(group, women = "Women", men = "Men", diff = "Gender diff"),
                        levels = c("Women", "Men", "Gender diff")),
    outcome_f    = factor(outcome, levels = summary_outcomes),
    label        = factor(unname(outcome_labels[outcome]), levels = rev(unname(outcome_labels[summary_outcomes]))),
    direction    = if_else(estimate > 0, "Higher", "Lower"),
    p.adj_matrix = p.adjust(p.value, method = "BH"),
    sig          = p.adj_matrix < .05,
    stars        = sig_stars(p.adj_matrix)
  )

# Women/Men cells: a plain "n.s." neutral tile when not significant; a
# two-colour direction tile + outline + stars when the within-gender effect
# itself is significant.
women_men_dat <- matrix_dat %>%
  filter(sub_col != "Gender diff") %>%
  mutate(cell_label = if_else(sig, stars, "n.s."))

# Gender-diff cells: a plain-language statement instead of a bare tile --
# which gender's effect is larger, with stars, but only when that difference
# itself clears significance; "n.s." (no direction claimed) otherwise.
diff_dat <- matrix_dat %>%
  filter(sub_col == "Gender diff") %>%
  mutate(
    higher_who = if_else(estimate > 0, "Women higher", "Men higher"),
    cell_label = if_else(sig, paste0(higher_who, "\n", stars), "n.s.")
  )

dir_colors <- c(Higher = group_colors[["women"]], Lower = group_colors[["men"]])

fig_matrix <- ggplot(mapping = aes(x = sub_col, y = label)) +
  # Non-significant cells (Women/Men AND Gender diff alike): flat neutral shading.
  geom_tile(data = filter(women_men_dat, !sig), fill = "grey92", colour = "white", linewidth = 0.6) +
  geom_text(data = filter(women_men_dat, !sig), aes(label = cell_label), colour = "grey45", size = 2.5) +
  geom_tile(data = filter(diff_dat, !sig), fill = "grey92", colour = "white", linewidth = 0.6) +
  geom_text(data = filter(diff_dat, !sig), aes(label = cell_label), colour = "grey45", size = 2.5) +
  # Significant Women/Men cells: direction-coloured tile, outlined, starred.
  geom_tile(data = filter(women_men_dat, sig), aes(fill = direction), colour = "white", linewidth = 0.6) +
  geom_tile(data = filter(women_men_dat, sig), fill = NA, colour = "#1c2b33", linewidth = 1) +
  geom_text(data = filter(women_men_dat, sig), aes(label = cell_label), colour = "#1c2b33",
            size = 3.2, fontface = "bold") +
  # Significant Gender-diff cells: direction-tinted tile + plain-language statement.
  geom_tile(data = filter(diff_dat, sig), aes(fill = direction), colour = "white", linewidth = 0.6, alpha = 0.35) +
  geom_tile(data = filter(diff_dat, sig), fill = NA, colour = "#1c2b33", linewidth = 1) +
  geom_text(data = filter(diff_dat, sig), aes(label = cell_label), colour = "#1c2b33",
            fontface = "bold", size = 2.5, lineheight = 0.9) +
  facet_grid(~ block, scales = "free_x", space = "free_x", labeller = label_wrap_gen(width = 20)) +
  # Pinned explicitly: when a y-category is entirely absent from the FIRST
  # geom layer (e.g. an outcome with no non-significant Women/Men cell at
  # all), ggplot's per-layer discrete-scale training silently appends it at
  # the end instead of respecting the factor's levels -- seen with "Use
  # accommodations" (all its Women/Men cells are significant) landing last
  # instead of in requested order. limits= forces the intended order.
  scale_y_discrete(limits = levels(matrix_dat$label)) +
  scale_fill_manual(values = dir_colors, name = "Direction", na.translate = FALSE) +
  labs(
    x = NULL, y = NULL,
    title = "Summary: direction and significance of every key contrast, by outcome",
    caption = str_wrap(paste(
      "Direction only, no magnitude (outcomes sit on different scales). Grey \"n.s.\" cells: not",
      "significant. Women/Men tiles: outline + stars mean that within-gender effect is significant.",
      "Gender diff: which gender's effect is larger, shown only when that gendered difference itself is",
      "significant. Benjamini-Hochberg adjustment pooled across every cell shown in this grid (a fresh",
      "pool, not each outcome's own adjustment from the block figures above).",
      "* p.adj<.05  ** p.adj<.01  *** p.adj<.001."
    ), width = 105)
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x  = element_text(size = 8),
    panel.grid   = element_blank(),
    strip.text   = element_text(face = "bold", size = 8),
    plot.caption = element_text(hjust = 0)
  )

fig_matrix_path <- file.path(conference_dir, "fig_hiring_summary_matrix.png")
ggsave(fig_matrix_path, fig_matrix, width = 12, height = 6, dpi = 300, bg = "white")

table_matrix_path <- file.path(conference_dir, "fig_hiring_summary_matrix.csv")
matrix_dat %>%
  select(outcome = outcome_f, label, block, sub_col, group, direction, estimate, std.error, conf.low, conf.high,
         df, p.value, p.adj = p.adj_matrix, sig, stars) %>%
  arrange(outcome, block, sub_col) %>%
  write_csv(table_matrix_path)

cat("Saved:", fig_matrix_path, "\n")
cat("Saved:", table_matrix_path, "\n")
