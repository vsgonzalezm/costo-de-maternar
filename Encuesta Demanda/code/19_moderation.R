# ============================================================================
# 19 - Moderation by EVALUATOR characteristics, BASELINE models (pooled)
# ============================================================================

proj <- if (dir.exists(here::here("Encuesta Demanda", "data"))) here::here("Encuesta Demanda") else here::here()
source(file.path(proj, "code", "00_setup.R"))
suppressPackageStartupMessages({ library(marginaleffects); library(writexl) })
init_output("19_moderation_evaluator_baseline")

df <- load_analysis_data()
cat("Pooled: N profiles =", nrow(df), "| N respondents =", n_distinct(df$numericalId), "\n")

# ==============================================================================
# PARAMETERS (edit here)
# ==============================================================================

outcomes_moderation <- c(
  "recommendation_probability",   # hiring (item)
  "good_investment_score",        # good investment (item)
  "idx_competencia",              # competence (index)
  "idx_crecimiento",              # growth (index)
  "operational_costly_score",     # operational cost (item)
  "expected_accommodations",      # accommodations (item)
  "expected_benefits_use",        # benefits (item)
  "idx_riesgo"                    # risk (index)
)
stopifnot(all(outcomes_moderation %in% outcomes_models))

#Workplace-norm attitude items (7-point): P16_1-3 = how much the evaluator's own
#company expects availability / avoiding absences / prioritising work;
#P17_1-2 = agreement that advancing requires prioritising work / a good worker
#is always available. Family-policy items (P17_3-4) are NOT included.
norm_items <- c("empresa_disp_fuera_horario", "empresa_evitar_ausencias",
                "empresa_priorizar_trabajo", "acuerdo_progresar_priorizar",
                "acuerdo_buen_trabaja_disp")

#Industry and job level are analysed in TWO ways: (a) with every original category as in
#the questionnaire, and (b) grouped (only when include_grouped_versions = TRUE). The
#grouping rules are the tables industry_map / job_level_map below; the script writes a
#record of every original category, its group, the rule and the n (moderator_grouping_record.csv).
include_grouped_versions <- TRUE

#With many levels the pairwise contrasts between levels are not computed (the omnibus
#Wald test covers the overall question; within-level contrasts are always computed).
max_levels_pairwise <- 4

#--- Industry grouping by workforce gender composition (a-priori; EDIT if the pre-registration
#    or the theory uses another one). key = filtro_rubro_empresa code, or "R:<category>" for
#    respondents who answered "Otro" (98), using the recoding of their open text made in
#    01_prepare_long_dataset.R (filtro_rubro_simple). group = NA means not classified: left out
#    of the grouped moderator only (they stay in the original-categories moderator).
industry_map <- tribble(
  ~key,                            ~group,             ~rule,                                   ~note,
  "6",                             "Female-dominated", "questionnaire category",                "Salud",
  "7",                             "Female-dominated", "questionnaire category",                "Educacion",
  "3",                             "Male-dominated",   "questionnaire category",                "Industria / manufactura",
  "8",                             "Male-dominated",   "questionnaire category",                "Construccion e inmobiliario",
  "9",                             "Male-dominated",   "questionnaire category",                "Transporte y logistica",
  "10",                            "Male-dominated",   "questionnaire category",                "Agroindustria / mineria / recursos naturales",
  "1",                             "Mixed",            "questionnaire category",                "Comercio",
  "2",                             "Mixed",            "questionnaire category",                "Servicios",
  "4",                             "Mixed",            "questionnaire category",                "Tecnologia y telecomunicaciones",
  "5",                             "Mixed",            "questionnaire category",                "Finanzas y seguros",
  "11",                            "Mixed",            "questionnaire category",                "Sector publico / ONG",
  "R:Servicios profesionales",     "Mixed",            "recoded open text: equivalent",         "same as Servicios (consulting, HR, IT consulting)",
  "R:Comercio/Alimentos",          "Mixed",            "recoded open text: equivalent",         "same as Comercio (sales, restaurants)",
  "R:Sector publico/Mutualidades", "Mixed",            "recoded open text: equivalent",         "same as Sector publico / ONG",
  "R:Turismo/Eventos",             "Mixed",            "recoded open text: judgment",           "tourism and entertainment services",
  "R:Industria/Tecnologia/Agro",   "Male-dominated",   "recoded open text: judgment",           "mostly mining, agriculture, aquaculture, metallurgy; a few are engineering/technology",
  "R:Mantenimiento/Seguridad",     NA,                 "not classified",                        "maintenance, mechanics, vehicle repair: no clear equivalent",
  "R:Independiente",               NA,                 "not classified",                        "self-employed, no industry given",
  "R:Sin trabajo",                 NA,                 "not classified",                        "currently without a job",
  "R:Otro",                        NA,                 "not classified",                        "heterogeneous open text"
)

#--- Job level grouping. key = filtro_cargo_actual code, or "R:<category>" for "Otro cargo" (98),
#    using the recoding of the open text in 01 (filtro_cargo_simple).
job_level_map <- tribble(
  ~key,                           ~group,                       ~rule,                              ~note,
  "1",                            "Owner / senior management",  "questionnaire category",           "Dueno/a o socio/a de la empresa",
  "2",                            "Owner / senior management",  "questionnaire category",           "Gerente o director/a",
  "3",                            "Owner / senior management",  "questionnaire category",           "Subgerente",
  "4",                            "Supervisor",                 "questionnaire category",           "Jefe/a o supervisor/a",
  "5",                            "Staff",                      "questionnaire category",           "Administrativo o asistente",
  "R:Jefatura/Direccion",         "Supervisor",                 "recoded open text: equivalent",    "same as Jefe/a o supervisor/a (unit head, inspector, contract administrator)",
  "R:Profesional/Administrativo", "Staff",                      "recoded open text: equivalent",    "non-managerial professional or administrative staff",
  "R:Tecnico/Operativo",          "Staff",                      "recoded open text: equivalent",    "non-managerial technical or operational staff",
  "R:Independiente",              "Owner / senior management",  "recoded open text: judgment",      "self-employed run their own business, as owners/partners do",
  "R:Sin trabajo/Cesante",        NA,                           "not classified",                   "currently without a job: no job level",
  "R:Otro",                       NA,                           "not classified",                   "heterogeneous open text"
)

# ==============================================================================
# 1. MODERATOR VARIABLES (respondent level; constant across a person's 2 profiles)
# ==============================================================================

stopifnot(all(norm_items %in% names(df)),
          all(map_lgl(norm_items, ~ all(df[[.x]] %in% 1:7))))

resp0 <- df %>% distinct(numericalId, .keep_all = TRUE) %>%
  mutate(norms_mean = rowMeans(across(all_of(norm_items))))

#Reliability of the attitude scale (and of its two parts, plus the excluded policy items)
alpha_of <- function(items) {
  as.numeric(psych::alpha(as.data.frame(resp0[, items]), check.keys = FALSE)$total$raw_alpha)
}
reliability <- tibble(
  scale = c("Workplace norms, 5 items (used as moderator)",
            "  company norms P16_1-3", "  ideal-worker agreement P17_1-2",
            "Family-policy items P17_3-4 (not used)"),
  alpha = c(alpha_of(norm_items), alpha_of(norm_items[1:3]), alpha_of(norm_items[4:5]),
            alpha_of(c("acuerdo_postnatal_1ano", "acuerdo_salacuna_todos")))
) %>% mutate(alpha = round(alpha, 3))
cat("\n=== Reliability of the attitude scale (Cronbach alpha) ===\n")
print(reliability)
write_csv(reliability, out_path("norms_reliability.csv"))

#Reference level of the sum-coded factors = most frequent category (does not affect the tests)
ref_industry <- names(which.max(table(resp0$filtro_rubro_empresa_label)))
ref_job      <- names(which.max(table(resp0$filtro_cargo_actual_label)))

norms_mu <- mean(resp0$norms_mean); norms_sd <- sd(resp0$norms_mean)
norms_breaks <- quantile(resp0$norms_mean, c(0, 1/3, 2/3, 1))
stopifnot(length(unique(norms_breaks)) == 4)

df <- df %>%
  mutate(
    norms_mean = rowMeans(across(all_of(norm_items))),
    #Evaluator gender: "Prefiero no decirlo" (SEXO == 3, only 8 respondents) is left out: with
    #16 profiles spread over the design cells it would leave empty cells and no estimable terms
    mod_sexo = apply_sum_contrasts(
      factor(case_when(SEXO == 1 ~ "Man", SEXO == 2 ~ "Woman"), levels = c("Man", "Woman")),
      ref_level = "Man"),
    mod_hijos = apply_sum_contrasts(
      factor(if_else(enc_tiene_hijos == 1, "Parent", "Non-parent"), levels = c("Non-parent", "Parent")),
      ref_level = "Non-parent"),
    #Industry and job level: every original category (12 and 6 levels, "Otro" included)
    mod_industria = apply_sum_contrasts(factor(filtro_rubro_empresa_label), ref_level = ref_industry),
    mod_cargo     = apply_sum_contrasts(factor(filtro_cargo_actual_label),  ref_level = ref_job),
    #Grouped versions (rules in industry_map / job_level_map; "Otro" uses the recoded open text)
    industry_key = if_else(filtro_rubro_empresa == 98, paste0("R:", filtro_rubro_simple),
                           as.character(filtro_rubro_empresa)),
    job_key      = if_else(filtro_cargo_actual == 98, paste0("R:", filtro_cargo_simple),
                           as.character(filtro_cargo_actual)),
    mod_industria_grp = apply_sum_contrasts(
      factor(unname(setNames(industry_map$group, industry_map$key)[industry_key]),
             levels = c("Female-dominated", "Male-dominated", "Mixed")),
      ref_level = "Mixed"),
    mod_cargo_grp = apply_sum_contrasts(
      factor(unname(setNames(job_level_map$group, job_level_map$key)[job_key]),
             levels = c("Owner / senior management", "Supervisor", "Staff")),
      ref_level = "Staff"),
    #Attitudes: standardised (continuous, used for the Wald tests) and tertiles (for contrasts)
    mod_norms_z = (norms_mean - norms_mu) / norms_sd,
    mod_norms_grp = apply_sum_contrasts(
      cut(norms_mean, breaks = norms_breaks, include.lowest = TRUE, labels = c("Low", "Mid", "High")),
      ref_level = "Mid")
  )

#Every category present in the data must be covered by a mapping rule (nothing is dropped silently)
keys_data <- df %>% distinct(numericalId, .keep_all = TRUE)
stopifnot(all(keys_data$industry_key %in% industry_map$key),
          all(keys_data$job_key %in% job_level_map$key))

#Record of how each original category was grouped (who goes where, by which rule, how many)
grouping_record <- function(moderator, data, key_col, code_col, label_col, map_tbl) {
  data %>%
    transmute(key = .data[[key_col]],
              original_category = if_else(.data[[code_col]] == 98,
                                          paste0(.data[[label_col]], " -> ", str_remove(.data[[key_col]], "^R:")),
                                          as.character(.data[[label_col]]))) %>%
    count(key, original_category, name = "n_respondents") %>%
    left_join(map_tbl, by = "key") %>%
    mutate(moderator = moderator,
           group = coalesce(group, "(left out: not classified)"),
           pct_respondents = round(100 * n_respondents / nrow(data), 1)) %>%
    arrange(group, desc(n_respondents)) %>%
    select(moderator, group, original_category, key, rule, note, n_respondents, pct_respondents)
}
grouping_rec <- bind_rows(
  grouping_record("Industry", keys_data, "industry_key", "filtro_rubro_empresa",
                  "filtro_rubro_empresa_label", industry_map),
  grouping_record("Job level", keys_data, "job_key", "filtro_cargo_actual",
                  "filtro_cargo_actual_label", job_level_map)
)
cat("\n=== Record of the groupings (also saved as moderator_grouping_record.csv) ===\n")
print(grouping_rec %>% select(moderator, group, original_category, rule, n_respondents), n = Inf, width = Inf)
cat("\nLeft out of the GROUPED moderators (kept in the original-categories ones):\n")
grouping_rec %>% filter(group == "(left out: not classified)") %>%
  group_by(moderator) %>% summarise(n_respondents = sum(n_respondents),
                                    pct = round(100 * sum(n_respondents) / nrow(keys_data), 1)) %>% print()
write_csv(grouping_rec, out_path("moderator_grouping_record.csv"))

moderators <- tribble(
  ~mod_id,     ~label,                           ~test_var,        ~contrast_var,
  "sexo",      "Evaluator gender",               "mod_sexo",       "mod_sexo",
  "hijos",     "Evaluator parental status",      "mod_hijos",      "mod_hijos",
  "industria", "Industry (original categories)", "mod_industria",  "mod_industria",
  "cargo",     "Job level (original categories)", "mod_cargo",     "mod_cargo",
  "norms",     "Workplace-norm attitudes",       "mod_norms_z",    "mod_norms_grp"
)
if (include_grouped_versions) {
  moderators <- bind_rows(moderators, tribble(
    ~mod_id,         ~label,                                    ~test_var,           ~contrast_var,
    "industria_grp", "Industry (grouped by gender composition)", "mod_industria_grp", "mod_industria_grp",
    "cargo_grp",     "Job level (grouped)",                      "mod_cargo_grp",     "mod_cargo_grp"
  ))
}

#Who is in each level, and who is left out of each moderator
mod_descriptives <- pmap_dfr(moderators, function(mod_id, label, test_var, contrast_var) {
  d <- df %>% filter(!is.na(.data[[test_var]]), !is.na(.data[[contrast_var]]))
  lv <- d %>% group_by(level = as.character(.data[[contrast_var]])) %>%
    summarise(n_respondents = n_distinct(numericalId), n_profiles = n(), .groups = "drop")
  bind_rows(
    lv %>% mutate(moderator = label, status = "included"),
    tibble(moderator = label, level = "(left out: missing / not classified)",
           n_respondents = n_distinct(df$numericalId) - n_distinct(d$numericalId),
           n_profiles = nrow(df) - nrow(d), status = "excluded")
  )
}) %>% select(moderator, level, status, n_respondents, n_profiles)
cat("\n=== Moderator levels and sample sizes ===\n")
print(mod_descriptives, n = Inf)
write_csv(mod_descriptives, out_path("moderator_descriptives.csv"))

# ==============================================================================
# 2. SPECIFICATIONS
# ==============================================================================

specs <- list(
  parental = list(attr_var = "perfil_parental", ref = "actualmente no tiene hijos",
                  levels = c("tiene un hijo", "tiene dos hijos"),
                  other = "perfil_estado_civil + perfil_plan_futuro"),
  plan     = list(attr_var = "perfil_plan_futuro", ref = "no quiere",
                  levels = c("en 2 anos", "en 5 anos"),
                  other = "perfil_estado_civil + perfil_parental")
)

make_rhs <- function(sp, mod_var) {
  paste0("perfil_genero * ", sp$attr_var, " * ", mod_var, " + ", sp$other,
         " + perfil_exp + perfil_desempeno + ", occ, " + ", controls)
}

level_labels <- c("tiene un hijo" = "1 child", "tiene dos hijos" = "2 children",
                  "en 2 anos" = "Plans: 2 years", "en 5 anos" = "Plans: 5 years")

model_df <- function(model) min(model$df, na.rm = TRUE)

# ==============================================================================
# 3. FIT MODELS AND WALD TESTS
# ==============================================================================

wald_f <- function(model, terms) {
  terms <- terms[!is.na(coef(model)[terms])]
  if (length(terms) == 0) {
    return(tibble(n_terms = 0L, df2 = NA_real_, F = NA_real_, p = NA_real_))
  }
  b <- coef(model)[terms]
  V <- vcov(model)[terms, terms, drop = FALSE]
  W <- tryCatch(as.numeric(t(b) %*% solve(V) %*% b), error = function(e) NA_real_)
  df1 <- length(terms); df2 <- model_df(model)
  Fstat <- W / df1
  tibble(n_terms = df1, df2 = df2, F = Fstat, p = pf(Fstat, df1, df2, lower.tail = FALSE))
}

#Terms that carry the moderator: all interactions, and only the 3-way ones
moderator_terms <- function(model, mod_var) {
  nm  <- names(coef(model))
  hit <- nm[str_detect(nm, fixed(mod_var))]
  list(omnibus = hit[str_detect(hit, ":")], three_way = hit[str_count(hit, ":") >= 2])
}

fitted_models <- list()
wald_results  <- list()

for (i in seq_len(nrow(moderators))) {
  md <- moderators[i, ]
  d  <- df %>% filter(!is.na(.data[[md$test_var]]), !is.na(.data[[md$contrast_var]]))
  for (sp_id in names(specs)) {
    sp <- specs[[sp_id]]
    cat(sprintf("\n%-26s %-9s N = %d respondents\n", md$label, sp_id, n_distinct(d$numericalId)))

    m_test <- run_models(make_rhs(sp, md$test_var), outcomes_moderation, d)
    #Contrast model: same as test model unless the moderator is continuous (attitudes)
    m_contrast <- if (md$test_var == md$contrast_var) m_test else
      run_models(make_rhs(sp, md$contrast_var), outcomes_moderation, d)
    fitted_models[[paste(md$mod_id, sp_id, sep = "|")]] <- list(data = d, contrast = m_contrast)

    wald_results[[length(wald_results) + 1]] <- map_dfr(outcomes_moderation, function(y) {
      m <- m_test[[y]]
      if (is.null(m)) return(NULL)
      tt <- moderator_terms(m, md$test_var)
      bind_rows(
        wald_f(m, tt$omnibus)   %>% mutate(test = "omnibus (all interactions with moderator)"),
        wald_f(m, tt$three_way) %>% mutate(test = "three_way (gender x attribute x moderator)")
      ) %>% mutate(outcome = y, moderator = md$label, spec = sp_id,
                   n_respondents = n_distinct(d$numericalId))
    })
  }
}

wald <- bind_rows(wald_results) %>%
  group_by(moderator, spec, test) %>%
  mutate(p_bh = p.adjust(p, method = "BH")) %>%
  ungroup() %>%
  mutate(outcome_label = outcome_labels[outcome]) %>%
  select(moderator, spec, test, outcome, outcome_label, n_respondents, n_terms, df2, F, p, p_bh)

cat("\n=== Wald tests: three-way terms (does the moderator change the GENDERED penalty?) ===\n")
wald %>% filter(str_detect(test, "^three_way")) %>%
  select(moderator, spec, outcome_label, F, p, p_bh) %>%
  mutate(across(c(F, p, p_bh), ~ round(.x, 3))) %>% print(n = Inf)
cat(sprintf("\nTests with p < .05 (unadjusted): %d of %d | p_bh < .05: %d\n",
            sum(wald$p < 0.05, na.rm = TRUE), nrow(wald), sum(wald$p_bh < 0.05, na.rm = TRUE)))

write_csv(wald, out_path("moderation_wald_tests.csv"))

# ==============================================================================
# 4. CONTRASTS: women-minus-men difference of the penalty, by moderator level
#   within level:  (penalty for women) - (penalty for men) at one moderator level
#   between levels: that gap at level A minus that gap at level B
# Same marginaleffects pattern as 10_marginal_means_baseline.R (level_contrast,
# double_diff); positions are found programmatically, never hard-coded.
# ==============================================================================

level_contrast <- function(model, var, ref, level, newdata, by = NULL) {
  vars <- setNames(list(c(ref, level)), var)
  avg_comparisons(model, variables = vars, newdata = newdata, by = by, df = model_df(model))
}

row_index <- function(comp_df, mod_var, lvl, gender) {
  i <- which(comp_df$perfil_genero == gender & as.character(comp_df[[mod_var]]) == lvl)
  stopifnot(length(i) == 1)
  i
}

to_rows <- function(obj, meta) {
  d <- as.data.frame(obj)
  bind_cols(meta, tibble(estimate = d$estimate, std.error = d$std.error,
                         conf.low = d$conf.low, conf.high = d$conf.high, p.value = d$p.value))
}

contrast_results <- list()

for (key in names(fitted_models)) {
  parts  <- str_split(key, "\\|")[[1]]
  md     <- moderators[moderators$mod_id == parts[1], ]
  sp     <- specs[[parts[2]]]
  d      <- fitted_models[[key]]$data
  models <- fitted_models[[key]]$contrast
  cv     <- md$contrast_var
  lvls   <- levels(d[[cv]])

  for (y in outcomes_moderation) {
    m <- models[[y]]
    if (is.null(m)) next
    for (lv in sp$levels) {
      comp <- tryCatch(level_contrast(m, sp$attr_var, sp$ref, lv, d, by = c("perfil_genero", cv)),
                       error = function(e) { message("  contrast failed: ", key, " ", y, " ", lv); NULL })
      if (is.null(comp)) next
      cd <- as.data.frame(comp)
      base_meta <- tibble(moderator = md$label, spec = parts[2], outcome = y,
                          attr_level = lv, contrast = paste0(level_labels[[lv]], " vs. ", sp$ref))

      #Within each level of the moderator: women minus men
      for (L in lvls) {
        h <- sprintf("b%d - b%d = 0", row_index(cd, cv, L, "Mujer"), row_index(cd, cv, L, "Hombre"))
        contrast_results[[length(contrast_results) + 1]] <-
          to_rows(hypotheses(comp, hypothesis = h),
                  base_meta %>% mutate(type = "within level", group = L))
      }
      #Between levels: gap at A minus gap at B
      if (length(lvls) >= 2 && length(lvls) <= max_levels_pairwise) {
        for (pr in combn(lvls, 2, simplify = FALSE)) {
          h <- sprintf("b%d - b%d - b%d + b%d = 0",
                       row_index(cd, cv, pr[1], "Mujer"),  row_index(cd, cv, pr[2], "Mujer"),
                       row_index(cd, cv, pr[1], "Hombre"), row_index(cd, cv, pr[2], "Hombre"))
          contrast_results[[length(contrast_results) + 1]] <-
            to_rows(hypotheses(comp, hypothesis = h),
                    base_meta %>% mutate(type = "between levels", group = paste(pr[1], "minus", pr[2])))
        }
      }
    }
  }
}

contrasts_tbl <- bind_rows(contrast_results) %>%
  mutate(outcome_label = outcome_labels[outcome]) %>%
  select(moderator, spec, outcome, outcome_label, contrast, type, group,
         estimate, std.error, conf.low, conf.high, p.value)
write_csv(contrasts_tbl, out_path("moderation_contrasts.csv"))
cat(sprintf("\nContrasts saved: %d rows\n", nrow(contrasts_tbl)))

write_xlsx(list(wald_tests = wald, contrasts = contrasts_tbl,
                moderator_descriptives = mod_descriptives, grouping_record = grouping_rec,
                reliability = reliability),
           out_path("moderation_evaluator_results.xlsx"))

# ==============================================================================
# 5. FIGURES: gender gap in the penalty, by moderator level (within-level contrasts)
# ==============================================================================

plot_moderator <- function(md_label, spec_id) {
  d <- contrasts_tbl %>%
    filter(moderator == md_label, spec == spec_id, type == "within level") %>%
    mutate(outcome_label = factor(outcome_label, levels = outcome_labels[outcomes_moderation]),
           contrast = factor(contrast, levels = unique(contrast)))
  if (nrow(d) == 0) return(invisible(NULL))
  p <- ggplot(d, aes(x = estimate, y = group)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
    geom_pointrange(aes(xmin = conf.low, xmax = conf.high), size = 0.35, linewidth = 0.5) +
    facet_grid(outcome_label ~ contrast, scales = "free_x") +
    labs(title = paste0(md_label, " - ", spec_id, " penalty"),
         subtitle = "Women minus men difference in the penalty, by moderator level (95% CI, CR2)",
         x = "Women minus men", y = NULL) +
    theme_bw(base_size = 10) +
    theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold", size = 8),
          strip.text.y = element_text(angle = 0))
  n_lv <- n_distinct(d$group)
  ggsave(out_path("figures", paste0(str_replace_all(tolower(md_label), "[^a-z]+", "_"), "_", spec_id, ".png")),
         p, width = 9, height = length(outcomes_moderation) * (0.35 * n_lv + 0.6) + 1.5,
         dpi = 200, limitsize = FALSE)
  invisible(p)
}

for (md_label in moderators$label) for (sp_id in names(specs)) plot_moderator(md_label, sp_id)

finish_output()