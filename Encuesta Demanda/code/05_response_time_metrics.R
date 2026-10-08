# ============================================================================
# 05 - Response-time metrics and respondent quality flags
# ============================================================================

library(tidyverse)
library(here)
 
proj <- if (dir.exists(here::here("Encuesta Demanda", "data"))) here::here("Encuesta Demanda") else here::here()
source(file.path(proj, "code", "00_setup.R"))
init_output("05_response_time_metrics")
 
 
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
 
#"Fast" is produced with TWO definitions side by side (suffix _q1 and _p10 in the
#output columns and a "definition" column in the tables). Each one is "at the bottom of
#the distribution of EACH variable":
#  q1  = first quartile (P25) of that variable
#  p10 = 10th percentile of that variable (not "10 seconds": the P10 is 7.6 min for
#        total duration, 11 s for career, 10 s for task, 21 s for the candidate screens)
fast_defs <- c(q1 = 0.25, p10 = 0.10)
 
#TRUE: fast = value <= the cut; FALSE: strict "<". Times pile up at the recording floor
#(10 s / 20 s): with "<" the P10 flags nobody on the task screen (its P10 equals its
#minimum). q1 is strict (as in the first run); p10 is inclusive.
low_inclusive <- c(q1 = FALSE, p10 = TRUE)
stopifnot(identical(names(fast_defs), names(low_inclusive)))
 
#"Slow" = truly extreme long times only (outliers), not a percentile. A time is slow if it
#is above the upper fence Q3 + high_iqr_mult * IQR (Tukey). Computed on the log scale when
#high_scale = "log" (times are very right-skewed), on the raw scale when "raw".
#"log" + 1.5 flags about 0.3-2% per variable; "raw" + 3 is the alternative with similar yield.
high_scale    <- "log"
high_iqr_mult <- 1.5
stopifnot(high_scale %in% c("log", "raw"))
 
#A respondent is "fast on screens" if at least this many of the 4 screens are below the low cut
min_fast_screens <- 2
 
#A respondent is low quality if they meet at least this many red-flag combinations
min_red_flags_low_quality <- 1
 
#Section 2b: wrong answer to the attention check whose validity is questioned.
#Must match the label in the data. It only sets the display order of the groups.
ac_weak_option <- "Último cargo"
 
#Section 2b (descriptive only, nobody is excluded with these): a respondent is
#"extreme" on a time measure if at or below this percentile (<= because times pile
#up at the recording floor), and the table also reports the share extreme on at
#least this many of the 5 time measures.
extreme_prob <- 0.05
min_extreme_measures <- 3
 
#FALSE = stop after the descriptives (sections 1-2b) to review them before flagging
flags_on <- TRUE
 
# ==============================================================================
# 1. DATA AND INVENTORY OF TIME-RELATED VARIABLES
# ==============================================================================
 
base_long_final <- read_csv(
  analysis_ready_path,
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
write_csv(time_inventory, out_path("descriptives", "time_variable_inventory.csv"))
 
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
write_csv(time_summary, out_path("descriptives", "time_descriptives.csv"))
 
cat("\n--- Spearman correlations ---\n")
time_cor <- cor(resp[, unname(flag_vars)], use = "pairwise.complete.obs", method = "spearman") %>%
  round(2)
print(time_cor)
write_csv(as_tibble(time_cor, rownames = "variable"), out_path("descriptives", "time_spearman.csv"))
 
#Cutoffs: fast = one cut per definition; slow = outlier fence (Tukey, log or raw scale)
cutoffs <- map_dfr(names(flag_vars), function(nm) {
  x  <- resp[[flag_vars[[nm]]]]
  q1 <- unname(quantile(x, 0.25, na.rm = TRUE))
  q3 <- unname(quantile(x, 0.75, na.rm = TRUE))
  if (high_scale == "log") {
    lx <- log(x[!is.na(x) & x > 0])
    l1 <- unname(quantile(lx, 0.25)); l3 <- unname(quantile(lx, 0.75))
    q_high <- exp(l3 + high_iqr_mult * (l3 - l1))
  } else {
    q_high <- q3 + high_iqr_mult * (q3 - q1)
  }
  tibble(short = nm, variable = flag_vars[[nm]], label = var_labels[[nm]],
         median = unname(quantile(x, 0.5, na.rm = TRUE)),
         q3 = q3, iqr = q3 - q1, q_high = q_high, lowest = min(x, na.rm = TRUE))
})
for (tag in names(fast_defs)) {
  cutoffs[[paste0("q_low_", tag)]] <- map_dbl(
    cutoffs$variable, ~ unname(quantile(resp[[.x]], fast_defs[[tag]], na.rm = TRUE)))
}
cutoffs <- cutoffs %>% relocate(starts_with("q_low_"), .after = label)
 
cat(sprintf("\n--- Cutoffs (fast: %s; slow: > Q3 + %.1f x IQR, %s scale) ---\n",
            paste0(toupper(names(fast_defs)), " = P", round(100 * fast_defs), collapse = ", "),
            high_iqr_mult, high_scale))
print(cutoffs, width = Inf)
write_csv(cutoffs, out_path("flags", "flag_cutoffs.csv"))
 
#Histograms (log scale) with the cutoffs
hist_data <- resp %>%
  select(numericalId, all_of(unname(flag_vars))) %>%
  pivot_longer(-numericalId, names_to = "variable", values_to = "value") %>%
  mutate(short = names(flag_vars)[match(variable, flag_vars)],
         label = factor(var_labels[short], levels = var_labels)) %>%
  filter(!is.na(value), value > 0)
 
cut_lines <- cutoffs %>%
  mutate(label = factor(label, levels = var_labels)) %>%
  select(label, starts_with("q_low_"), q_high, lowest) %>%
  pivot_longer(-label, names_to = "line", values_to = "x") %>%
  mutate(line = case_when(
    line == "q_high" ~ "Slow cut (outlier fence)",
    line == "lowest" ~ "Lowest recorded value",
    TRUE             ~ paste0("Fast cut: ", toupper(str_remove(line, "^q_low_")))
  ))
 
p_hist <- ggplot(hist_data, aes(x = value)) +
  geom_histogram(bins = 40, fill = "grey75", color = "white") +
  geom_vline(data = cut_lines, aes(xintercept = x, color = line, linetype = line),
             linewidth = 0.6) +
  scale_x_log10() +
  scale_color_manual(values = c("Fast cut: Q1" = "#E41A1C", "Fast cut: P10" = "#FF7F00",
                                "Slow cut (outlier fence)" = "#377EB8",
                                "Lowest recorded value" = "#4DAF4A"), name = NULL) +
  scale_linetype_manual(values = c("Fast cut: Q1" = "solid", "Fast cut: P10" = "solid",
                                   "Slow cut (outlier fence)" = "solid",
                                   "Lowest recorded value" = "dashed"), name = NULL) +
  facet_wrap(~ label, ncol = 2, scales = "free") +
  labs(title = "Response-time variables",
       subtitle = "Respondent level, log10 x-axis",
       x = NULL, y = "Respondents") +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"))
 
ggsave(out_path("descriptives", "time_histograms.pdf"), p_hist,
       width = 9, height = 9, device = "pdf")
 
 
# ==============================================================================
# 2b. ATTENTION CHECK BY ANSWER OPTION vs. RESPONSE TIMES
# Independent of the flag definitions below: it runs even with flags_on = FALSE
# and nobody is excluded here.
# Question: do people who failed the check by choosing a given wrong option also
# answer faster than people who passed? If an option shows no speed difference,
# failing through it is weak evidence of inattention.
# ==============================================================================
 
resp <- resp %>% mutate(ac_fail = FAILED_ATTENTION_CHECK == 1)
stopifnot(!anyNA(resp$ac_fail))
stopifnot(ac_weak_option %in% resp$check_atencion_memoria_label[resp$ac_fail])
 
ac_levels <- c(
  "Passed", ac_weak_option,
  setdiff(names(sort(table(resp$check_atencion_memoria_label[resp$ac_fail]), decreasing = TRUE)),
          ac_weak_option)
)
resp <- resp %>%
  mutate(ac_option = factor(
    if_else(ac_fail, as.character(check_atencion_memoria_label), "Passed"),
    levels = ac_levels))
stopifnot(!anyNA(resp$ac_option))
 
cat("\n=== 2b. ATTENTION CHECK BY ANSWER OPTION ===\n")
print(count(resp, ac_option))
 
# --- Extreme-speed measures (descriptive) and a second, independent measure ---
extreme_cuts <- map_dbl(flag_vars, ~ unname(quantile(resp[[.x]], extreme_prob, na.rm = TRUE)))
cat(sprintf("\nExtreme cuts (<= P%d): %s\n", round(100 * extreme_prob),
            paste(names(extreme_cuts), round(extreme_cuts, 2), sep = "=", collapse = ", ")))
 
for (nm in names(flag_vars)) {
  resp[[paste0("extreme_", nm)]] <- coalesce(resp[[flag_vars[[nm]]]] <= extreme_cuts[[nm]], FALSE)
}
resp <- resp %>%
  mutate(n_extreme = rowSums(across(all_of(paste0("extreme_", names(flag_vars))))))
 
#Straightlining: identical answer on all 16 items in at least one profile.
#Not used to define any flag; it is an independent check on the time-based evidence.
straight_by_resp <- base_long_final %>%
  mutate(item_sd = apply(across(all_of(outcomes_items)), 1, sd, na.rm = TRUE)) %>%
  group_by(numericalId) %>%
  summarise(straight_any = any(item_sd == 0, na.rm = TRUE), .groups = "drop")
resp <- resp %>% left_join(straight_by_resp, by = "numericalId")
stopifnot(!anyNA(resp$straight_any))
 
# --- Response times by answer option, compared with those who passed ---
#Wilcoxon (one-sided: group faster than passers); prob_faster = P(group faster) + 0.5 P(tie)
time_by_option <- map_dfr(names(flag_vars), function(nm) {
  v <- flag_vars[[nm]]
  x_pass <- resp[[v]][resp$ac_option == "Passed"]
  map_dfr(levels(resp$ac_option), function(g) {
    x <- resp[[v]][resp$ac_option == g]
    tst <- if (g == "Passed") NULL else
      suppressWarnings(wilcox.test(x, x_pass, alternative = "less", exact = FALSE))
    tibble(
      variable = var_labels[[nm]], ac_option = g, n = sum(!is.na(x)),
      median = median(x, na.rm = TRUE),
      median_ratio_vs_passed = round(median(x, na.rm = TRUE) / median(x_pass, na.rm = TRUE), 2),
      q1 = unname(quantile(x, 0.25, na.rm = TRUE)),
      q3 = unname(quantile(x, 0.75, na.rm = TRUE)),
      pct_extreme = round(100 * mean(x <= extreme_cuts[[nm]], na.rm = TRUE), 1),
      prob_faster_than_passed = if (is.null(tst)) NA_real_ else
        round(1 - unname(tst$statistic) / (sum(!is.na(x)) * sum(!is.na(x_pass))), 3),
      p_wilcox = if (is.null(tst)) NA_real_ else tst$p.value
    )
  })
}) %>%
  mutate(p_holm = p.adjust(p_wilcox, method = "holm"),
         ac_option = factor(ac_option, levels = ac_levels))
 
cat("\n--- Response times by attention-check answer (vs. those who passed) ---\n")
print(time_by_option, n = Inf, width = Inf)
cat("How to read: median_ratio_vs_passed near 1 and prob_faster_than_passed near 0.5 mean the\n",
    "group answers like those who passed. p_wilcox is one-sided and uncorrected; p_holm corrects\n",
    "across all comparisons in this table. Small groups (n < 30) have little power.\n", sep = "")
write_csv(time_by_option, out_path("ac_option", "ac_option_timing.csv"))
 
option_summary <- resp %>%
  group_by(ac_option) %>%
  summarise(
    n = n(),
    mean_extreme_measures = round(mean(n_extreme), 2),
    pct_extreme_on_k_measures = round(100 * mean(n_extreme >= min_extreme_measures), 1),
    pct_straightlining = round(100 * mean(straight_any), 1),
    .groups = "drop"
  )
cat(sprintf("\n--- By answer option: extreme measures (<= P%d) and straightlining ---\n",
            round(100 * extreme_prob)))
print(option_summary, width = Inf)
write_csv(option_summary, out_path("ac_option", "ac_option_summary.csv"))
 
# --- Figures ---
opt_n   <- count(resp, ac_option)
opt_lab <- setNames(paste0(opt_n$ac_option, "\n(n=", opt_n$n, ")"), opt_n$ac_option)
 
fig_data <- resp %>%
  select(numericalId, ac_option, all_of(unname(flag_vars))) %>%
  pivot_longer(all_of(unname(flag_vars)), names_to = "variable", values_to = "value") %>%
  mutate(short = names(flag_vars)[match(variable, flag_vars)],
         label = factor(var_labels[short], levels = var_labels)) %>%
  filter(!is.na(value), value > 0)
 
p_opt <- ggplot(fig_data, aes(x = ac_option, y = value, fill = ac_option)) +
  geom_boxplot(outlier.size = 0.4, width = 0.6) +
  scale_x_discrete(labels = opt_lab) +
  scale_y_log10() +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  facet_wrap(~ label, scales = "free_y", ncol = 3) +
  labs(title = "Response times by attention-check answer",
       subtitle = "Respondent level, log10 y-axis",
       x = NULL, y = NULL) +
  theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"),
        axis.text.x = element_text(size = 7))
ggsave(out_path("ac_option", "ac_option_timing.pdf"), p_opt, width = 11, height = 7, device = "pdf")
 
ext_label <- paste0("Extreme on >= ", min_extreme_measures, " of 5 time measures")
p_ind <- option_summary %>%
  pivot_longer(c(pct_extreme_on_k_measures, pct_straightlining),
               names_to = "measure", values_to = "pct") %>%
  mutate(measure = if_else(measure == "pct_extreme_on_k_measures", ext_label,
                           "Identical answers on all 16 items (any profile)"),
         ac_option = factor(ac_option, levels = ac_levels)) %>%
  ggplot(aes(x = ac_option, y = pct, fill = measure)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            position = position_dodge(width = 0.75), vjust = -0.3, size = 3) +
  scale_x_discrete(labels = opt_lab) +
  scale_fill_manual(values = c("#E41A1C", "#377EB8"), name = NULL) +
  labs(title = "Inattention indicators by attention-check answer",
       x = NULL, y = "% of respondents") +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
ggsave(out_path("ac_option", "ac_option_inattention.pdf"), p_ind, width = 9, height = 5, device = "pdf")
 
if (!flags_on) {
  cat("\nflags_on = FALSE: stopped after the descriptives. Review them, then set flags_on <- TRUE.\n")
} else {
 
# ==============================================================================
# 3. FLAGS PER VARIABLE
#   fast: two definitions (suffix _q1 and _p10), at the bottom of each variable
#   slow: only truly extreme long times (outlier fence), one definition
# A missing time is not flagged (counted as FALSE) and is reported.
# ==============================================================================
 
for (i in seq_len(nrow(cutoffs))) {
  nm <- cutoffs$short[i]; v <- cutoffs$variable[i]
  resp[[paste0("high_", nm)]] <- coalesce(resp[[v]] > cutoffs$q_high[i], FALSE)
  for (tag in names(fast_defs)) {
    cut <- cutoffs[[paste0("q_low_", tag)]][i]
    resp[[paste0("low_", nm, "_", tag)]] <- coalesce(
      if (low_inclusive[[tag]]) resp[[v]] <= cut else resp[[v]] < cut, FALSE)
  }
}
cat(sprintf("\nMissing values: %s\n",
            paste(names(flag_vars), map_int(flag_vars, ~ sum(is.na(resp[[.x]]))),
                  sep = "=", collapse = ", ")))
 
flag_counts <- map_dfr(names(fast_defs), function(tag) {
  map_dfr(names(flag_vars), function(nm) {
    lo <- resp[[paste0("low_", nm, "_", tag)]]; hi <- resp[[paste0("high_", nm)]]
    tibble(definition = toupper(tag), variable = var_labels[[nm]],
           n_fast = sum(lo), pct_fast = round(100 * mean(lo), 1),
           n_slow_outlier = sum(hi), pct_slow_outlier = round(100 * mean(hi), 1))
  })
})
cat("\n=== 3. FLAGGED PER VARIABLE ===\n")
print(flag_counts, n = Inf, width = Inf)
write_csv(flag_counts, out_path("flags", "flag_counts_by_variable.csv"))
 
#Number of screens (of 4) that are fast, per definition; and slow (outlier) screens
resp <- resp %>%
  mutate(n_slow_screens = rowSums(across(all_of(paste0("high_", screen_short)))))
for (tag in names(fast_defs)) {
  resp[[paste0("n_fast_screens_", tag)]] <-
    rowSums(as.matrix(resp[, paste0("low_", screen_short, "_", tag)]))
  cat(sprintf("\nNumber of screens that are fast (of 4), definition %s:\n", toupper(tag)))
  print(count(resp, n_fast = .data[[paste0("n_fast_screens_", tag)]]))
}
 
# ==============================================================================
# 4. RED FLAGS = COMBINATIONS OF FLAGS (one set per fast definition)
#   ac_fail       failed the attention check (P5), same in both definitions
#   fast_total    total duration is fast
#   fast_screens  at least min_fast_screens of the 4 screens are fast
# ==============================================================================
 
stopifnot(!anyNA(resp$ac_fail))   #ac_fail is defined in section 2b
 
for (tag in names(fast_defs)) {
  ft <- resp[[paste0("low_duration_", tag)]]
  fs <- resp[[paste0("n_fast_screens_", tag)]] >= min_fast_screens
  ac <- resp$ac_fail
  resp[[paste0("fast_total_", tag)]]                 <- ft
  resp[[paste0("fast_screens_", tag)]]               <- fs
  resp[[paste0("rf_ac_fast_total_", tag)]]           <- ac & ft
  resp[[paste0("rf_ac_fast_screens_", tag)]]         <- ac & fs
  resp[[paste0("rf_fast_total_fast_screens_", tag)]] <- ft & fs
  resp[[paste0("rf_all_three_", tag)]]               <- ac & ft & fs
  #Number of pairwise combinations met (0-3)
  n_rf <- (ac & ft) + (ac & fs) + (ft & fs)
  resp[[paste0("n_red_flags_", tag)]] <- n_rf
  resp[[paste0("low_quality_", tag)]] <- n_rf >= min_red_flags_low_quality
}
 
rule_summary <- function(rule, flag) {
  tibble(rule = rule, n = sum(flag), pct = round(100 * mean(flag), 1))
}
rules_table <- map_dfr(names(fast_defs), function(tag) {
  g <- function(x) resp[[paste0(x, "_", tag)]]
  bind_rows(
    rule_summary("Failed attention check",                                  resp$ac_fail),
    rule_summary("Total duration is fast",                                  g("fast_total")),
    rule_summary("Fast on screens (>= min_fast_screens fast)",              g("fast_screens")),
    rule_summary("RED FLAG: AC fail AND fast total duration",               g("rf_ac_fast_total")),
    rule_summary("RED FLAG: AC fail AND fast on screens",                   g("rf_ac_fast_screens")),
    rule_summary("RED FLAG: fast total duration AND fast on screens",       g("rf_fast_total_fast_screens")),
    rule_summary("RED FLAG: all three",                                     g("rf_all_three")),
    rule_summary("LOW QUALITY (>= min_red_flags_low_quality red flags)",    g("low_quality"))
  ) %>% mutate(definition = toupper(tag), .before = 1)
})
cat("\n=== 4. RED FLAGS ===\n")
print(rules_table, n = Inf, width = Inf)
write_csv(rules_table, out_path("flags", "red_flag_counts.csv"))
 
pattern_table <- map_dfr(names(fast_defs), function(tag) {
  resp %>%
    count(ac_fail,
          fast_total   = .data[[paste0("fast_total_", tag)]],
          fast_screens = .data[[paste0("fast_screens_", tag)]], name = "n") %>%
    mutate(definition = toupper(tag), pct = round(100 * n / n_total, 1), .before = 1) %>%
    arrange(desc(ac_fail + fast_total + fast_screens), desc(n))
})
cat("\n--- All combinations of the three base flags, by definition ---\n")
print(pattern_table, n = Inf)
write_csv(pattern_table, out_path("flags", "red_flag_patterns.csv"))
 
low_quality_by_condition <- map_dfr(names(fast_defs), function(tag) {
  lq <- paste0("low_quality_", tag)
  resp %>%
    group_by(VISITAS_1_BRAZO) %>%
    summarise(n = n(), n_low_quality = sum(.data[[lq]]),
              pct_low_quality = round(100 * mean(.data[[lq]]), 1), .groups = "drop") %>%
    mutate(definition = toupper(tag), .before = 1)
})
cat("\n--- Low quality by occupational condition, by definition ---\n")
print(low_quality_by_condition)
write_csv(low_quality_by_condition, out_path("flags", "low_quality_by_condition.csv"))
 
# ==============================================================================
# 5. RESPONDENT-LEVEL FLAG TABLE (who is flagged and why; both definitions)
# ==============================================================================
 
flag_table <- resp %>%
  select(numericalId, VISITAS_1_BRAZO, all_of(unname(flag_vars)),
         starts_with("low_"), starts_with("high_"),
         starts_with("n_fast_screens"), n_slow_screens, ac_fail,
         starts_with("fast_total"), starts_with("fast_screens"),
         starts_with("rf_"), starts_with("n_red_flags"))
 
write_csv(flag_table, out_path("flags", "quality_flags_by_respondent.csv"))
cat(sprintf("\nRespondent-level flags saved (%d rows).\n", nrow(flag_table)))
 
# ==============================================================================
# 6. FIGURE: respondents per red flag, by fast definition
# ==============================================================================
 
p_rules <- rules_table %>%
  filter(str_detect(rule, "^RED FLAG|^LOW QUALITY")) %>%
  mutate(rule = factor(rule, levels = rev(unique(rule)))) %>%
  ggplot(aes(x = n, y = rule)) +
  geom_col(fill = "#E41A1C", width = 0.6) +
  geom_text(aes(label = paste0(n, " (", pct, "%)")), hjust = -0.1, size = 3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.3))) +
  facet_wrap(~ definition, ncol = 2) +
  labs(title = "Respondents flagged by each combination",
       subtitle = "Panels = definition of \"fast\" (Q1 vs. P10)",
       x = "Respondents", y = NULL) +
  theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"))
 
ggsave(out_path("flags", "red_flags.pdf"), p_rules,
       width = 12, height = 4.5, device = "pdf")
 
}
 
cat("\n=== Script 05 completed ===\n")
finish_output()