# ============================================================================
# 21 - EXPLORATORY "conversion" analysis (supplementary material only)
# Noviembre 2026. NOT preregistered. Descriptive, not a mediation estimate.
#
# Question: evaluators expect both women and men with family signals to request
# accommodations. Is that expectation "converted" into a worse evaluation to the
# same extent for women and for men?
#
# What is estimated: the association between an evaluator's expectation that the
# applicant will request accommodations (or use benefits) and their evaluation of
# the same applicant, separately by applicant gender, and the difference between
# the two slopes.
#
# Two specifications (OLS):
#   (a) pooled      : outcome ~ expectation x woman + all attributes + occupation
#                     + order + template, CR2 errors clustered by respondent
#   (b) within      : first differences. Each respondent rated 2 profiles, so
#                     every variable is replaced by (profile 2 - profile 1). This
#                     is numerically the respondent fixed-effects estimator; it
#                     removes each person's response style (people who use high
#                     numbers everywhere). One row per respondent, HC2 errors.
#                     The intercept absorbs the order effect. Preferred.
#                     (lm_robust with fixed effects + CR2 on 1,449 clusters did
#                     not finish in 10 minutes, hence the explicit differences.)
#
# Caveats to state wherever this is reported:
#   - The expectation is measured AFTER the vignette, like the outcomes. It is not
#     randomised. Slopes are associations; they can reflect reverse inference or a
#     shared overall impression of the applicant.
#   - The gender difference in slopes is identified from who was randomly shown a
#     woman or a man, but not the slope itself.
#
# Outputs: outputs/21_exploratory_conversion/
# ============================================================================
proj <- if (dir.exists(here::here("Encuesta Demanda", "data"))) here::here("Encuesta Demanda") else here::here()
source(file.path(proj, "code", "00_setup.R"))
init_output("21_exploratory_conversion")

df <- load_analysis_data() %>%
  mutate(woman = as.numeric(perfil_genero == "Mujer"))

expectations <- c("expected_accommodations", "expected_benefits_use")
outcomes_conv <- c("recommendation_probability", "good_investment_score", "idx_riesgo",
                   "operational_costly_score", "idx_crecimiento", "idx_competencia")
attributes_rhs <- paste("perfil_estado_civil + perfil_parental + perfil_plan_futuro +",
                        "perfil_exp + perfil_desempeno +", controls)
# Within respondent, presentation order is constant after differencing (the intercept).
attributes_rhs_within <- paste("perfil_estado_civil + perfil_parental + perfil_plan_futuro +",
                               "perfil_exp + perfil_desempeno + template_contexto")

# Slopes for men (b_x), women (b_x + b_int) and their difference (b_int), with CR2 vcov.
slopes_from <- function(m, spec, x, y, nx = "x_c", ni = "x_c:woman", n_clusters = m$nclusters) {
  b <- coef(m); V <- vcov(m)
  stopifnot(all(c(nx, ni) %in% names(b)), !anyNA(b[c(nx, ni)]))
  est <- c(men = b[[nx]], women = b[[nx]] + b[[ni]], difference = b[[ni]])
  se  <- c(men = sqrt(V[nx, nx]),
           women = sqrt(V[nx, nx] + V[ni, ni] + 2 * V[nx, ni]),
           difference = sqrt(V[ni, ni]))
  dfree <- min(m$df[c(nx, ni)])
  tibble(spec = spec, expectation = x, outcome = y, group = names(est),
         estimate = unname(est), std.error = unname(se), df = dfree,
         conf.low = estimate - qt(.975, dfree) * std.error,
         conf.high = estimate + qt(.975, dfree) * std.error,
         p.value = 2 * pt(-abs(estimate / std.error), dfree),
         n_obs = m$nobs, n_clusters = n_clusters)
}

# First-difference data: (profile shown second) - (profile shown first), per respondent.
first_differences <- function(d, y) {
  d <- d %>% arrange(numericalId, pantalla_orden)
  stopifnot(all(table(d$numericalId) == 2))
  X <- model.matrix(as.formula(paste("~ x_c * woman +", attributes_rhs_within)), d)[, -1, drop = FALSE]
  colnames(X) <- make.names(sub("x_c:woman", "x_c_woman", colnames(X), fixed = TRUE))
  i2 <- seq(2, nrow(d), by = 2); i1 <- i2 - 1
  dX <- X[i2, , drop = FALSE] - X[i1, , drop = FALSE]
  dX <- dX[, apply(dX, 2, function(v) length(unique(v)) > 1), drop = FALSE]  # drop constants
  data.frame(dy = d[[y]][i2] - d[[y]][i1], dX, check.names = FALSE)
}

results <- map_dfr(expectations, function(x) {
  d <- df %>% mutate(x_c = .data[[x]] - mean(.data[[x]], na.rm = TRUE))
  map_dfr(outcomes_conv, function(y) {
    f_pooled <- as.formula(paste(y, "~ x_c * woman +", attributes_rhs, "+", occ))
    m_pooled <- lm_robust(f_pooled, data = d, clusters = numericalId, se_type = "CR2")
    fd <- first_differences(d, y)
    m_within <- lm_robust(dy ~ ., data = fd, se_type = "HC2")
    bind_rows(slopes_from(m_pooled, "pooled", x, y),
              slopes_from(m_within, "within respondent", x, y, ni = "x_c_woman", n_clusters = nrow(fd)))
  })
}) %>%
  mutate(outcome_label = outcome_labels[outcome], expectation_label = outcome_labels[expectation]) %>%
  group_by(spec, expectation, group) %>%
  mutate(p.adj = if_else(group == "difference", p.adjust(p.value, method = "BH"), NA_real_)) %>%
  ungroup() %>%
  select(spec, expectation, expectation_label, outcome, outcome_label, group, estimate, std.error,
         conf.low, conf.high, df, p.value, p.adj, n_obs, n_clusters)

write_csv(results, out_path("conversion_slopes.csv"))

# Context: how much the expectation itself responds to the family signals, by gender
# (raw means; the model-based versions are in scripts 10 and 20).
context <- df %>%
  mutate(gender = if_else(woman == 1, "Women", "Men")) %>%
  group_by(gender, perfil_parental, perfil_plan_futuro) %>%
  summarise(n = n(), across(all_of(c(expectations, "recommendation_probability", "idx_riesgo")), mean),
            .groups = "drop")
write_csv(context, out_path("raw_means_by_cell.csv"))

# Figure: slopes for women and men, by outcome and specification (accommodations).
fig_data <- results %>%
  filter(group != "difference") %>%
  mutate(group = factor(group, levels = c("women", "men"), labels = c("Women", "Men")),
         outcome_label = factor(outcome_label, levels = rev(outcome_labels[outcomes_conv])),
         spec = factor(spec, levels = c("within respondent", "pooled"),
                       labels = c("Within respondent (preferred)", "Pooled")))
fig <- ggplot(fig_data, aes(x = estimate, y = outcome_label, colour = group)) +
  geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), position = position_dodge(width = 0.5), size = 0.35) +
  facet_grid(expectation_label ~ spec) +
  scale_colour_manual(values = c(Women = "#B2182B", Men = "#2166AC")) +
  labs(x = "Change in the evaluation per 1-point higher expectation (1-7 scales), 95% CI",
       y = NULL, colour = "Applicant",
       caption = "Exploratory and descriptive: expectations are measured after the vignette, not randomised.") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
ggsave(out_path("conversion_slopes.png"), fig, width = 8.5, height = 5.5, dpi = 300, bg = "white")

cat("\n=== Slopes on 'Expected: request accommodations' (within respondent) ===\n")
print(as.data.frame(
  results %>% filter(expectation == "expected_accommodations", spec == "within respondent") %>%
    transmute(outcome_label, group, estimate = round(estimate, 3), se = round(std.error, 3),
              p = round(p.value, 4), p.adj = round(p.adj, 3))
), row.names = FALSE)
cat("\n=== Gender difference in slopes, all specifications ===\n")
print(as.data.frame(
  results %>% filter(group == "difference") %>%
    transmute(spec, expectation, outcome_label, estimate = round(estimate, 3),
              p = round(p.value, 4), p.adj = round(p.adj, 3))
), row.names = FALSE)

finish_output()
