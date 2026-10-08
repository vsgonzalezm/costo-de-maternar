library(tidyverse)
library(here)
 
base_long_final <- read_csv(
  here("Data", "Processed", "base_demanda_long_final_with_labels_and_flags.csv"),
  show_col_types = FALSE
)
 
out_dir <- here("outputs", "tables")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
 # Generic descriptive function with percentiles
 
describe_numeric <- function(x, label) {
  x <- suppressWarnings(as.numeric(x))
  tibble(
    variable = label,
    n      = sum(!is.na(x)),
    n_na   = sum(is.na(x)),
    mean   = mean(x, na.rm = TRUE),
    sd     = sd(x, na.rm = TRUE),
    min    = min(x, na.rm = TRUE),
    p10    = quantile(x, 0.10, na.rm = TRUE),
    p25    = quantile(x, 0.25, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    p75    = quantile(x, 0.75, na.rm = TRUE),
    p90    = quantile(x, 0.90, na.rm = TRUE),
    p95    = quantile(x, 0.95, na.rm = TRUE),
    max    = max(x, na.rm = TRUE)
  )
}
# 1. RESPONDENT-LEVEL DESCRIPTIVES (one row per person)
 
respondent_level_data <- base_long_final %>% distinct(key, .keep_all = TRUE)
 
respondent_time_summary <- bind_rows(
  describe_numeric(respondent_level_data$DURATION_MINUTES, "Total survey duration (minutes)"),
  describe_numeric(respondent_level_data$TIME_SCREEN_CARGO, "Career screen time (seconds)"),
  describe_numeric(respondent_level_data$TIME_SCREEN_TAREA, "Task screen time (seconds)")
)
 
cat("\n=== 1. RESPONDENT-LEVEL TIMING (n =", nrow(respondent_level_data), ") ===\n")
print(respondent_time_summary, width = Inf)
 
 
# 2. PROFILE-LEVEL DESCRIPTIVES (two rows per person, n = 2898)
# NOTE: use the median as the main reference for comparisons, not the
# mean -- profile timing always has a long right tail (paused and
# resumed surveys) that inflates the mean and can mask real group
# differences.
 
profile_time_summary <- bind_rows(
  describe_numeric(base_long_final$time_screen_perfil, "Profile evaluation time (seconds)"),
  describe_numeric(base_long_final$time_popit, "Pop-it open time (seconds)")
)
 
cat("\n=== 2. PROFILE-LEVEL TIMING (n =", nrow(base_long_final), ") ===\n")
print(profile_time_summary, width = Inf)
 
 
# 3. PROFILE EVALUATION TIME BY POSITION (1st vs. 2nd profile shown)
# The 2nd profile is expected to be evaluated faster (task already
# understood) -- compare mainly the MEDIANS, not the means.
 
profile_time_by_position <- base_long_final %>%
  group_by(pantalla_orden) %>%
  summarise(
    n      = sum(!is.na(time_screen_perfil)),
    mean   = mean(time_screen_perfil, na.rm = TRUE),
    median = median(time_screen_perfil, na.rm = TRUE),
    sd     = sd(time_screen_perfil, na.rm = TRUE),
    min    = min(time_screen_perfil, na.rm = TRUE),
    p75    = quantile(time_screen_perfil, 0.75, na.rm = TRUE),
    max    = max(time_screen_perfil, na.rm = TRUE),
    .groups = "drop"
  )
 
cat("\n=== 3. PROFILE EVALUATION TIME BY POSITION (1st vs. 2nd) ===\n")
print(profile_time_by_position, width = Inf)
 
 
# 4. POP-IT USAGE (additional info tooltip)
 
if ("click_popit" %in% names(base_long_final)) {
  popit_usage_summary <- base_long_final %>%
    summarise(
      n_evaluations = n(),
      n_with_click = sum(as.logical(click_popit) %in% TRUE, na.rm = TRUE),
      pct_with_click = round(100 * n_with_click / n_evaluations, 1)
    )
  cat("\n=== 4. POP-IT USAGE ===\n")
  print(popit_usage_summary)
}
 
 
# 5. TIME-RELATED QUALITY FLAGS (already computed in script 02)
# Reported together so the descriptives above can be read in context --
# e.g., a low minimum is not alarming if almost no one triggers these
# flags.
 
time_flag_vars <- c(
  "TIME_CAPTURE_ERROR_2PLUS", "UNDER_P10_ALL_3_TASKS",
  "DURATION_LT_6MIN", "DURATION_GT_P75", "DURATION_GT_P95"
)
time_flag_vars <- intersect(time_flag_vars, names(respondent_level_data))
 
time_flag_summary <- map_dfr(time_flag_vars, function(v) {
  respondent_level_data %>%
    summarise(
      flag = v,
      n_total = n(),
      n_TRUE = sum(.data[[v]] %in% TRUE, na.rm = TRUE),
      pct_TRUE = round(100 * n_TRUE / n_total, 1)
    )
})
 
cat("\n=== 5. TIME-RELATED QUALITY FLAGS (respondent level) ===\n")
print(time_flag_summary, width = Inf)
 
 
# 6. TOP 10 LONGEST AND SHORTEST CASES (for spot-checking)
 
extreme_duration_cases <- bind_rows(
  respondent_level_data %>% arrange(desc(DURATION_MINUTES)) %>% slice_head(n = 10) %>%
    mutate(case_type = "Longest"),
  respondent_level_data %>% arrange(DURATION_MINUTES) %>% slice_head(n = 10) %>%
    mutate(case_type = "Shortest")
) %>%
  select(case_type, numericalId, DURATION_MINUTES, TIME_CAPTURE_ERROR_2PLUS,
         DURATION_LT_6MIN, DURATION_GT_P95)
 
cat("\n=== 6. TOP 10 LONGEST AND SHORTEST CASES (total duration) ===\n")
print(extreme_duration_cases, n = 20)

# 7. EXPORT
 
write_csv(respondent_time_summary, file.path(out_dir, "timing_respondent_level.csv"))
write_csv(profile_time_summary, file.path(out_dir, "timing_profile_level.csv"))
write_csv(profile_time_by_position, file.path(out_dir, "timing_profile_by_position.csv"))
write_csv(time_flag_summary, file.path(out_dir, "timing_quality_flags.csv"))
write_csv(extreme_duration_cases, file.path(out_dir, "timing_extreme_cases.csv"))
 
cat("\nExported to:", out_dir, "\n")
