# ============================================================================
# 12 - Response-time metrics and respondent quality flags
# ============================================================================

library(tidyverse)
library(here)

project_dir <- if (dir.exists(here("Encuesta Demanda", "Data"))) {
  here("Encuesta Demanda")
} else {
  here()
}

tables_dir  <- file.path(project_dir, "Outputs", "tables",  "quality_flags")
figures_dir <- file.path(project_dir, "Outputs", "figures", "quality_flags")
dir.create(tables_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# PARAMETERS 
# ==============================================================================

#Rules used in section 1 to decide which variables CAN be used to flag
max_pct_na <- 5    #max % missing
min_unique <- 20   #min distinct values (excludes counts and 0/1 codes)

#Variables chosen to flag (must pass the section 1 rules)
flag_vars <- c(
  duration    = "DURATION_MINUTES",
  cargo       = "TIME_SCREEN_CARGO",
  tarea       = "TIME_SCREEN_TAREA",
  postulante1 = "TIME_SCREEN_POSTULANTE_1",
  postulante2 = "TIME_SCREEN_POSTULANTE_2"
)
var_labels <- c(
  duration    = "Total duration (min)",
  cargo       = "Career screen (s)",
  tarea       = "Task screen (s)",
  postulante1 = "Candidate 1 screen (s)",
  postulante2 = "Candidate 2 screen (s)"
)
screen_short <- c("cargo", "tarea", "postulante1", "postulante2")

#"Fast" = below this percentile in every variable (0.25 = Q1)
low_prob <- 0.25

#"Slow" = abnormally long only, not everyone above Q3: value above the upper fence
#Q3 + high_iqr_mult * IQR (Tukey). 1.5 = outliers; 3 = extreme outliers.
high_iqr_mult <- 1.5

#A respondent is "fast on screens" if at least this many of the 4 screens are below the low cut
min_fast_screens <- 2

#A respondent is low quality if they meet at least this many red-flag combinations
min_red_flags_low_quality <- 1

#FALSE = stop after the descriptives (sections 1-2) to review them before flagging
flags_on <- TRUE

# ==============================================================================
# 1. DATA AND INVENTORY OF TIME-RELATED VARIABLES
# ==============================================================================

base_long_final <- read_csv(
  file.path(project_dir, "Data", "processed", "base_demanda_analysis_ready.csv"),
  show_col_types = FALSE
)

#Structure check: 2 rows per respondent
stopifnot(all(count(base_long_final, numericalId)$n == 2))

resp <- base_long_final %>% distinct(numericalId, .keep_all = TRUE)
n_total <- nrow(resp)
cat(sprintf("Respondents: %d | profiles: %d\n", n_total, nrow(base_long_final)))

#Every column whose name suggests timing
time_cols <- names(base_long_final)[
  str_detect(names(base_long_final), regex("time|duration|popit", ignore_case = TRUE))
]

inv_basic <- map_dfr(time_cols, function(v) {
  x <- base_long_final[[v]]
  varies <- base_long_final %>%
    group_by(numericalId) %>%
    summarise(k = n_distinct(.data[[v]]), .groups = "drop") %>%
    summarise(any_varies = any(k > 1)) %>%
    pull(any_varies)
  has_num <- is.numeric(x) && any(!is.na(x))
  tibble(
    variable = v,
    class    = class(x)[1],
    level    = if_else(varies, "profile", "respondent"),
    n_na     = sum(is.na(x)),
    pct_na   = round(100 * mean(is.na(x)), 1),
    n_unique = n_distinct(x),
    min      = if (has_num) min(x, na.rm = TRUE)    else NA_real_,
    median   = if (has_num) median(x, na.rm = TRUE) else NA_real_,
    max      = if (has_num) max(x, na.rm = TRUE)    else NA_real_
  )
})

#Columns that carry the same information (correlation ~ 1), among low-missing numerics
cand_num <- inv_basic %>% filter(class %in% c("numeric", "integer"), pct_na <= max_pct_na) %>%
  pull(variable)
same_as <- tibble(
  variable = cand_num,
  same_as  = map_chr(cand_num, function(v) {
    others <- setdiff(cand_num, v)
    hit <- others[map_lgl(others, function(o) {
      r <- suppressWarnings(cor(base_long_final[[v]], base_long_final[[o]], use = "complete.obs"))
      isTRUE(abs(r) > 0.9999)
    })]
    if (length(hit)) paste(hit, collapse = ", ") else NA_character_
  })
)

time_inventory <- inv_basic %>%
  left_join(same_as, by = "variable") %>%
  mutate(
    rule_numeric     = class %in% c("numeric", "integer"),
    rule_respondent  = level == "respondent",
    rule_low_na      = pct_na <= max_pct_na,
    rule_continuous  = n_unique >= min_unique,
    eligible_to_flag = rule_numeric & rule_respondent & rule_low_na & rule_continuous
  )

cat("\n=== 1. INVENTORY OF TIME-RELATED VARIABLES ===\n")
cat(sprintf("Rules: numeric, respondent level, <= %d%% missing, >= %d distinct values\n",
            max_pct_na, min_unique))
print(time_inventory, n = Inf, width = Inf)
write_csv(time_inventory, file.path(tables_dir, "time_variable_inventory.csv"))

#Chosen variables must pass the rules
stopifnot(all(flag_vars %in% time_inventory$variable[time_inventory$eligible_to_flag]))
cat("\nChosen variables all pass the rules. Columns carrying the same information (same_as):\n")
print(time_inventory %>% filter(!is.na(same_as)) %>% select(variable, same_as))

# ==============================================================================
# 2. DESCRIPTIVES OF THE CHOSEN VARIABLES 
# ==============================================================================

skew_fn <- function(x) { x <- x[!is.na(x)]; mean((x - mean(x))^3) / sd(x)^3 }

describe_time <- function(x, label) {
  x <- as.numeric(x)
  q <- function(p) unname(quantile(x, p, na.rm = TRUE))
  tibble(
    variable = label, n = sum(!is.na(x)), n_na = sum(is.na(x)),
    mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE), skew = skew_fn(x),
    min = min(x, na.rm = TRUE), p10 = q(.10), q1 = q(.25), median = q(.50),
    q3 = q(.75), p90 = q(.90), max = max(x, na.rm = TRUE), iqr = q(.75) - q(.25),
    n_at_min = sum(x == min(x, na.rm = TRUE), na.rm = TRUE),
    pct_at_min = round(100 * sum(x == min(x, na.rm = TRUE), na.rm = TRUE) / sum(!is.na(x)), 1)
  )
}

time_summary <- map_dfr(names(flag_vars), function(nm) {
  describe_time(resp[[flag_vars[[nm]]]], var_labels[[nm]])
})
cat("\n=== 2. DESCRIPTIVES ===\n")
print(time_summary, width = Inf)
cat("\nn_at_min > 1 means several people share the lowest recorded value.\n")
write_csv(time_summary, file.path(tables_dir, "time_descriptives.csv"))

cat("\n--- Spearman correlations ---\n")
time_cor <- cor(resp[, unname(flag_vars)], use = "pairwise.complete.obs", method = "spearman") %>%
  round(2)
print(time_cor)
write_csv(as_tibble(time_cor, rownames = "variable"), file.path(tables_dir, "time_spearman.csv"))

#Cutoffs: low = chosen percentile; high = Q3 + high_iqr_mult * IQR
cutoffs <- map_dfr(names(flag_vars), function(nm) {
  x  <- resp[[flag_vars[[nm]]]]
  q1 <- unname(quantile(x, 0.25, na.rm = TRUE))
  q3 <- unname(quantile(x, 0.75, na.rm = TRUE))
  tibble(short = nm, variable = flag_vars[[nm]], label = var_labels[[nm]],
         q_low  = unname(quantile(x, low_prob, na.rm = TRUE)),
         median = unname(quantile(x, 0.5,      na.rm = TRUE)),
         q3     = q3,
         iqr    = q3 - q1,
         q_high = q3 + high_iqr_mult * (q3 - q1),
         lowest = min(x, na.rm = TRUE))
})
cat(sprintf("\n--- Cutoffs (low = P%d; high = Q3 + %.1f x IQR) ---\n",
            round(100 * low_prob), high_iqr_mult))
print(cutoffs, width = Inf)
write_csv(cutoffs, file.path(tables_dir, "flag_cutoffs.csv"))

#Histograms (log scale) with the cutoffs
hist_data <- resp %>%
  select(numericalId, all_of(unname(flag_vars))) %>%
  pivot_longer(-numericalId, names_to = "variable", values_to = "value") %>%
  mutate(short = names(flag_vars)[match(variable, flag_vars)],
         label = factor(var_labels[short], levels = var_labels)) %>%
  filter(!is.na(value), value > 0)

cut_lines <- cutoffs %>%
  mutate(label = factor(label, levels = var_labels)) %>%
  select(label, q_low, q_high, lowest) %>%
  pivot_longer(c(q_low, q_high, lowest), names_to = "line", values_to = "x") %>%
  mutate(line = recode(line, q_low = "Low cut", q_high = "High cut (outlier fence)",
                       lowest = "Lowest recorded value"))

p_hist <- ggplot(hist_data, aes(x = value)) +
  geom_histogram(bins = 40, fill = "grey75", color = "white") +
  geom_vline(data = cut_lines, aes(xintercept = x, color = line, linetype = line),
             linewidth = 0.6) +
  scale_x_log10() +
  scale_color_manual(values = c("Low cut" = "#E41A1C", "High cut (outlier fence)" = "#377EB8",
                                "Lowest recorded value" = "#4DAF4A"), name = NULL) +
  scale_linetype_manual(values = c("Low cut" = "solid", "High cut (outlier fence)" = "solid",
                                   "Lowest recorded value" = "dashed"), name = NULL) +
  facet_wrap(~ label, ncol = 2, scales = "free") +
  labs(title = "Response-time variables",
       subtitle = "Respondent level, log10 x-axis",
       x = NULL, y = "Respondents") +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"))

ggsave(file.path(figures_dir, "time_histograms.pdf"), p_hist,
       width = 9, height = 9, device = "pdf")

if (!flags_on) {
  cat("\nflags_on = FALSE: stopped after the descriptives. Review them, then set flags_on <- TRUE.\n")
} else {

# ==============================================================================
# 3. FLAGS PER VARIABLE: BELOW THE LOW CUT (fast) / ABOVE THE OUTLIER FENCE (slow)
# ==============================================================================

for (i in seq_len(nrow(cutoffs))) {
  nm <- cutoffs$short[i]; v <- cutoffs$variable[i]
  resp[[paste0("low_",  nm)]] <- coalesce(resp[[v]] < cutoffs$q_low[i],  FALSE)
  resp[[paste0("high_", nm)]] <- coalesce(resp[[v]] > cutoffs$q_high[i], FALSE)
}
cat(sprintf("\nMissing values: %s\n",
            paste(names(flag_vars), map_int(flag_vars, ~ sum(is.na(resp[[.x]]))),
                  sep = "=", collapse = ", ")))

flag_counts <- map_dfr(names(flag_vars), function(nm) {
  lo <- resp[[paste0("low_", nm)]]; hi <- resp[[paste0("high_", nm)]]
  tibble(variable = var_labels[[nm]],
         n_fast = sum(lo), pct_fast = round(100 * mean(lo), 1),
         n_slow_outlier = sum(hi), pct_slow_outlier = round(100 * mean(hi), 1))
})
cat("\n=== 3. FLAGGED PER VARIABLE ===\n")
print(flag_counts, width = Inf)
write_csv(flag_counts, file.path(tables_dir, "flag_counts_by_variable.csv"))

resp <- resp %>%
  mutate(
    n_fast_screens = rowSums(across(all_of(paste0("low_",  screen_short)))),
    #Number of screens with an abnormally long time (outlier)
    n_slow_screens = rowSums(across(all_of(paste0("high_", screen_short))))
  )
cat("\nNumber of screens below the low cut (of 4):\n")
print(count(resp, n_fast_screens))

# ==============================================================================
# 4. RED FLAGS = COMBINATIONS OF FLAGS
#   ac_fail       failed the attention check (P5)
#   fast_total    total duration below the low cut
#   fast_screens  at least min_fast_screens of the 4 screens below the low cut
# ==============================================================================

resp <- resp %>%
  mutate(
    ac_fail      = FAILED_ATTENTION_CHECK == 1,
    fast_total   = low_duration,
    fast_screens = n_fast_screens >= min_fast_screens,
    rf_ac_fast_total           = ac_fail & fast_total,
    rf_ac_fast_screens         = ac_fail & fast_screens,
    rf_fast_total_fast_screens = fast_total & fast_screens,
    rf_all_three               = ac_fail & fast_total & fast_screens,
    #Number of pairwise combinations met (0-3)
    n_red_flags = rf_ac_fast_total + rf_ac_fast_screens + rf_fast_total_fast_screens,
    low_quality = n_red_flags >= min_red_flags_low_quality
  )
stopifnot(!anyNA(resp$ac_fail))

rule_summary <- function(rule, flag) {
  tibble(rule = rule, n = sum(flag), pct = round(100 * mean(flag), 1))
}
rules_table <- bind_rows(
  rule_summary("Failed attention check",                                  resp$ac_fail),
  rule_summary("Total duration below low cut",                            resp$fast_total),
  rule_summary("Fast on screens (>= min_fast_screens below low cut)",     resp$fast_screens),
  rule_summary("RED FLAG: AC fail AND fast total duration",               resp$rf_ac_fast_total),
  rule_summary("RED FLAG: AC fail AND fast on screens",                   resp$rf_ac_fast_screens),
  rule_summary("RED FLAG: fast total duration AND fast on screens",       resp$rf_fast_total_fast_screens),
  rule_summary("RED FLAG: all three",                                     resp$rf_all_three),
  rule_summary("LOW QUALITY (>= min_red_flags_low_quality red flags)",    resp$low_quality)
)
cat("\n=== 4. RED FLAGS ===\n")
print(rules_table, width = Inf)
write_csv(rules_table, file.path(tables_dir, "red_flag_counts.csv"))

pattern_table <- resp %>%
  count(ac_fail, fast_total, fast_screens, name = "n") %>%
  mutate(pct = round(100 * n / n_total, 1)) %>%
  arrange(desc(ac_fail + fast_total + fast_screens), desc(n))
cat("\n--- All combinations of the three base flags ---\n")
print(pattern_table)
write_csv(pattern_table, file.path(tables_dir, "red_flag_patterns.csv"))

low_quality_by_condition <- resp %>%
  group_by(VISITAS_1_BRAZO) %>%
  summarise(n = n(), n_low_quality = sum(low_quality),
            pct_low_quality = round(100 * mean(low_quality), 1), .groups = "drop")
cat("\n--- Low quality by occupational condition ---\n")
print(low_quality_by_condition)
write_csv(low_quality_by_condition, file.path(tables_dir, "low_quality_by_condition.csv"))

# ==============================================================================
# 5. RESPONDENT-LEVEL FLAG TABLE (who is flagged and why)
# ==============================================================================

flag_table <- resp %>%
  select(numericalId, VISITAS_1_BRAZO, all_of(unname(flag_vars)),
         starts_with("low_"), starts_with("high_"),
         n_fast_screens, n_slow_screens, ac_fail, fast_total, fast_screens,
         starts_with("rf_"), n_red_flags, low_quality)

write_csv(flag_table, file.path(tables_dir, "quality_flags_by_respondent.csv"))
cat(sprintf("\nRespondent-level flags saved (%d rows).\n", nrow(flag_table)))

# ==============================================================================
# 6. FIGURE: respondents per red flag
# ==============================================================================

p_rules <- rules_table %>%
  filter(str_detect(rule, "^RED FLAG|^LOW QUALITY")) %>%
  mutate(rule = factor(rule, levels = rev(rule))) %>%
  ggplot(aes(x = n, y = rule)) +
  geom_col(fill = "#E41A1C", width = 0.6) +
  geom_text(aes(label = paste0(n, " (", pct, "%)")), hjust = -0.1, size = 3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(title = "Respondents flagged by each combination",
       x = "Respondents", y = NULL) +
  theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank())

ggsave(file.path(figures_dir, "red_flags.pdf"), p_rules,
       width = 9, height = 4, device = "pdf")

}

cat("\n=== Script 12 completed ===\n")
cat(sprintf("Tables:  %s\nFigures: %s\n", tables_dir, figures_dir))