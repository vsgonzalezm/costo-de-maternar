# ============================================================================
# 02 - Add Netquest labels, timing checks, and analysis flags
# Author: valentinagonzalez
# Modified: Tania Hutt - arreglo de errores
# Date: agosto 2026
# Purpose: Adds value labels from Netquest codebook sheets, cleans NULL markers, creates timing/duration/attention/block flags, and exports the final analysis CSV.
# ============================================================================


library(tidyverse)
library(readxl)
library(here)

proj <- if (dir.exists(here::here("Encuesta Demanda", "data"))) here::here("Encuesta Demanda") else here::here()
source(file.path(proj, "code", "00_setup.R"))
init_output("02_labels_timing_quality_flags")  # QC -> outputs/02_labels_timing_quality_flags/

clean_netquest_nulls <- function(data) {
  data %>%
    mutate(across(where(is.character), ~ na_if(str_squish(.x), "#NULL!"))) %>%
    mutate(across(where(is.character), ~ na_if(str_squish(.x), "NULL!")))
}

excel_path <- file.path(raw_dir, "UCXXCL_289723_cMain_20260730.xlsx")
base_long_path <- file.path(proc_dir, "base_demanda_long_final.csv")
processed_dir <- proc_dir
# qc_dir: cada archivo se escribe con out_path() (prefijo 02_)

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

raw_data <- read_excel(excel_path, sheet = "Datos") %>% clean_netquest_nulls()
raw_labels <- read_excel(excel_path, sheet = "Labels") %>% clean_netquest_nulls()
variables_raw <- read_excel(excel_path, sheet = "Variables") %>% clean_netquest_nulls()
codes_raw <- read_excel(excel_path, sheet = "Codes") %>% clean_netquest_nulls()
base_long_final <- read_csv(base_long_path, show_col_types = FALSE) %>% clean_netquest_nulls()

id_var <- if ("key" %in% names(raw_data)) "key" else names(raw_data)[1]

# --- 1. NETQUEST CODEBOOK AND VALUE LABELS ---


variables_codebook <- variables_raw

variable_col <- intersect(c("Var1", "Variable", "variable"), names(variables_codebook))[1]
label_col <- intersect(c("Etiqueta", "etiqueta", "Label", "label"), names(variables_codebook))[1]

if (!is.na(variable_col) && !is.na(label_col)) {
  variables_codebook <- variables_codebook %>%
    mutate(
      variable_label_original = as.character(.data[[label_col]]),
      variable_label_corrected = str_replace_all(
        variable_label_original,
        regex("risk/NOT RISKY", ignore_case = TRUE),
        "investment"
      ),
      variable_label_corrected = if_else(
        .data[[variable_col]] == "P3",
        "Open-ended reason: candidate is a good investment (cost-benefit)",
        variable_label_corrected
      )
    )
}

codes <- codes_raw %>%
  select(1:min(3, ncol(codes_raw)))

names(codes) <- c("variable", "value", "value_label")[seq_along(names(codes))]

write_csv(variables_codebook, out_path("netquest_variables_codebook.csv"))
write_csv(codes, out_path("netquest_value_labels.csv"))

investment_label_audit <- variables_codebook %>%
  filter(
    if (!is.na(variable_col)) {
      .data[[variable_col]] %in% c("P3", "P2_TA_P1_9", "P2_TA_P2_9", "P2_TB_P1_9", "P2_TB_P2_9") |
        str_detect(as.character(.data[[variable_col]]), regex("P3|P2_.*_9", ignore_case = TRUE))
    } else {
      row_number() == 0
    }
  )

write_csv(investment_label_audit, out_path("investment_label_audit.csv"))

# --- 2. RESPONDENT-LEVEL LABELS FROM NETQUEST LABELS SHEET ---

coalesce_existing <- function(data, vars) {
  vars <- unlist(vars, use.names = FALSE)
  existing_vars <- intersect(vars, names(data))
  if (length(existing_vars) == 0) {
    return(rep(NA_character_, nrow(data)))
  }
  as.character(dplyr::coalesce(!!!data[existing_vars]))
}

add_label_column <- function(data, labels_data, new_name, source_vars) {
  data[[new_name]] <- coalesce_existing(labels_data, source_vars)
  data
}

respondent_labels <- raw_labels %>%
  transmute(key = .data[[id_var]])

respondent_label_map <- tribble(
  ~label_col, ~source_vars,
  "DEVICE_label", list(c("DEVICE")),
  "SEXO_label", list(c("SEXO")),
  "EDADR_label", list(c("EDADR")),
  "NSE_label", list(c("NSE")),
  "ZONA_label", list(c("ZONA")),
  "TARGET_label", list(c("TARGET")),
  "CONSENT_label", list(c("CONSENT")),
  "check_atencion_memoria_label", list(c("P5")),
  "enc_estado_civil_label", list(c("P6")),
  "enc_tiene_hijos_label", list(c("P7")),
  "enc_numero_hijos_label", list(c("P8")),
  "enc_educacion_label", list(c("P9")),
  "filtro_cargo_actual_label", list(c("P10")),
  "filtro_anos_experiencia_label", list(c("P11")),
  "filtro_rubro_empresa_label", list(c("P12")),
  "filtro_decision_contratacion_label", list(c("P13")),
  "filtro_personal_cargo_label", list(c("P14")),
  # Nombres alineados al contenido real de P16/P17 (ver revision) y a los
  # nombres corregidos en 01_prepare_long_profile_dataset_from_netquest_excel.R.
  # P17 solo tiene 4 items reales (P17_5/P17_6 no existen en el codebook).
  "empresa_disp_fuera_horario_label", list(c("P16_1")),
  "empresa_evitar_ausencias_label", list(c("P16_2")),
  "empresa_priorizar_trabajo_label", list(c("P16_3")),
  "acuerdo_progresar_priorizar_label", list(c("P17_1")),
  "acuerdo_buen_trabaja_disp_label", list(c("P17_2")),
  "acuerdo_postnatal_1ano_label", list(c("P17_3")),
  "acuerdo_salacuna_todos_label", list(c("P17_4"))
)

for (i in seq_len(nrow(respondent_label_map))) {
  respondent_labels <- add_label_column(
    respondent_labels,
    raw_labels,
    respondent_label_map$label_col[[i]],
    respondent_label_map$source_vars[[i]]
  )
}

# --- 3. PROFILE-SPECIFIC LABELS FOR THE TWO PROFILES ---

profile_label_map <- tribble(
  ~label_col, ~question, ~item,
  "recommendation_probability_label", "P1", NA_integer_,
  "ambitious_score_label", "P2", 1L,
  "trustworthy_score_label", "P2", 2L,
  "competent_score_label", "P2", 3L,
  "risky_continuity_score_label", "P2", 4L,
  "salary_costly_score_label", "P2", 5L,
  "operational_costly_score_label", "P2", 6L,
  "long_term_commitment_score_label", "P2", 7L,
  "promising_score_label", "P2", 8L,
  "good_investment_score_label", "P2", 9L,
  "expected_late_absence_label", "P3", 1L,
  "expected_benefits_use_label", "P3", 2L,
  "expected_accommodations_label", "P3", 3L,
  "expected_quit_work_label", "P3", 4L,
  "expected_new_challenges_label", "P3", 5L,
  "expected_promotion_label", "P3", 6L
)

source_vars_for_profile <- function(question, item, profile_number) {
  if (question == "P1") {
    return(c(
      paste0("P1_TA_P", profile_number),
      paste0("P1_TB_P", profile_number)
    ))
  }

  c(
    paste0(question, "_TA_P", profile_number, "_", item),
    paste0(question, "_TB_P", profile_number, "_", item)
  )
}

make_profile_labels <- function(profile_number) {
  out <- tibble(
    key = raw_labels[[id_var]],
    pantalla_orden = profile_number
  )

  for (i in seq_len(nrow(profile_label_map))) {
    out[[profile_label_map$label_col[[i]]]] <- coalesce_existing(
      raw_labels,
      source_vars_for_profile(
        profile_label_map$question[[i]],
        profile_label_map$item[[i]],
        profile_number
      )
    )
  }

  out
}

profile_labels <- bind_rows(
  make_profile_labels(1),
  make_profile_labels(2)
)

# --- 4. BLOCK DUPLICATION / EXTRA RESPONSES ---


block_counts <- raw_data %>%
  count(POSTULANTE_1_BLOCK, name = "n_respondents") %>%
  rename(bloque_id = POSTULANTE_1_BLOCK) %>%
  mutate(
    n_blocks = n_distinct(raw_data$POSTULANTE_1_BLOCK, na.rm = TRUE),
    expected_mean = nrow(raw_data) / n_blocks,
    expected_upper = ceiling(expected_mean),
    extra_over_expected_upper = n_respondents - expected_upper,
    NETQUEST_EXTRA_BLOCK_FLAG = extra_over_expected_upper >= 2
  ) %>%
  arrange(desc(extra_over_expected_upper), bloque_id)

extra_blocks <- block_counts %>%
  filter(NETQUEST_EXTRA_BLOCK_FLAG) %>%
  pull(bloque_id)

write_csv(block_counts, out_path("block_balance_check.csv"))

# --- 5. 50-50 ORDER AND CONDITION BALANCE CHECKS ---

balance_vars <- intersect(
  c(
    "pantalla_orden", "template_contexto", "perfil_genero", "perfil_ocupacion",
    "perfil_estado_civil", "perfil_hijos", "perfil_edad_hijos",
    "perfil_plan_futuro", "perfil_edad", "bloque_id"
  ),
  names(base_long_final)
)

order_50_50_check <- map_dfr(balance_vars, function(var_name) {
  base_long_final %>%
    count(level = as.character(.data[[var_name]]), name = "n") %>%
    mutate(
      check_variable = var_name,
      pct = n / sum(n),
      abs_deviation_from_50 = if (n_distinct(level) == 2) abs(pct - 0.5) else NA_real_
    ) %>%
    select(check_variable, level, n, pct, abs_deviation_from_50)
})

write_csv(order_50_50_check, out_path("order_50_50_check.csv"))



raw_time_vars <- intersect(
  c("TIME_SCREEN_CARGO", "TIME_SCREEN_TAREA", "TIME_SCREEN_POSTULANTE_1", "TIME_SCREEN_POSTULANTE_2"),
  names(raw_data)
)

raw_time_checks <- raw_data %>%
  select(key = all_of(id_var), all_of(raw_time_vars))

for (time_var in raw_time_vars) {
  raw_time_checks[[paste0(time_var, "_CAPTURE_ERROR")]] <-
    is.na(raw_time_checks[[time_var]]) |
    suppressWarnings(as.numeric(raw_time_checks[[time_var]]) == 0)
}

capture_error_vars <- str_subset(names(raw_time_checks), "_CAPTURE_ERROR$")

raw_time_checks <- raw_time_checks %>%
  mutate(
    TIME_CAPTURE_ERROR_COUNT = rowSums(across(all_of(capture_error_vars)), na.rm = TRUE),
    TIME_CAPTURE_ERROR_2PLUS = TIME_CAPTURE_ERROR_COUNT >= 2
  )

write_csv(raw_time_checks, out_path("time_capture_error_check.csv"))

analysis_time_vars <- intersect(
  c("TIME_SCREEN_CARGO", "TIME_SCREEN_TAREA", "time_screen_perfil"),
  names(base_long_final)
)

time_p10 <- base_long_final %>%
  summarise(across(
    all_of(analysis_time_vars),
    ~ quantile(.x, probs = 0.10, na.rm = TRUE),
    .names = "{.col}_p10"
  )) %>%
  pivot_longer(everything(), names_to = "time_variable", values_to = "p10")

write_csv(time_p10, out_path("time_p10_thresholds.csv"))

p10_lookup <- time_p10 %>%
  mutate(time_variable = str_remove(time_variable, "_p10$"))

base_with_time_flags <- base_long_final

for (time_var in analysis_time_vars) {
  threshold <- p10_lookup %>%
    filter(time_variable == time_var) %>%
    pull(p10)

  flag_name <- paste0("UNDER_P10_", str_to_upper(time_var))
  base_with_time_flags[[flag_name]] <- base_with_time_flags[[time_var]] < threshold
}

under_p10_flags <- str_subset(names(base_with_time_flags), "^UNDER_P10_")

if (length(under_p10_flags) == 3) {
  base_with_time_flags <- base_with_time_flags %>%
    mutate(
      UNDER_P10_ALL_3_TASKS = rowSums(across(all_of(under_p10_flags)), na.rm = TRUE) == 3
    )
} else {
  base_with_time_flags <- base_with_time_flags %>%
    mutate(UNDER_P10_ALL_3_TASKS = NA)
}

# --- 7. SURVEY DURATION FLAGS ---
# Duration is stored in seconds. Long cases can occur when the survey was left


duration_thresholds <- base_with_time_flags %>%
  distinct(key, .keep_all = TRUE) %>%
  summarise(
    p10 = quantile(duration / 60, 0.10, na.rm = TRUE),
    p15 = quantile(duration / 60, 0.15, na.rm = TRUE),
    p25 = quantile(duration / 60, 0.25, na.rm = TRUE),
    p50 = quantile(duration / 60, 0.50, na.rm = TRUE),
    p75 = quantile(duration / 60, 0.75, na.rm = TRUE),
    p90 = quantile(duration / 60, 0.90, na.rm = TRUE),
    p95 = quantile(duration / 60, 0.95, na.rm = TRUE),
    p99 = quantile(duration / 60, 0.99, na.rm = TRUE)
  )

write_csv(duration_thresholds, out_path("duration_minute_thresholds.csv"))

p75_duration <- duration_thresholds$p75[[1]]
p95_duration <- duration_thresholds$p95[[1]]

# --- 8. FINAL LABELED AND FLAGGED LONG DATASET ---

base_labeled_flags <- base_with_time_flags %>%
  left_join(respondent_labels, by = "key") %>%
  left_join(profile_labels, by = c("key", "pantalla_orden")) %>%
  left_join(
    raw_time_checks %>%
      select(key, TIME_CAPTURE_ERROR_COUNT, TIME_CAPTURE_ERROR_2PLUS),
    by = "key"
  ) %>%
  mutate(
    NETQUEST_EXTRA_BLOCK_FLAG = bloque_id %in% extra_blocks,
    DURATION_MINUTES = duration / 60,
    DURATION_LT_6MIN = DURATION_MINUTES < 6,
    DURATION_GT_P75 = DURATION_MINUTES > p75_duration,
    DURATION_GT_P95 = DURATION_MINUTES > p95_duration,
    # P5 pregunta que informacion NO estaba en los perfiles; la respuesta correcta es "RUT".
    # Se usa la etiqueta (no el codigo numerico) porque es la fuente confiable de esa semantica.
    FAILED_ATTENTION_CHECK = case_when(
      check_atencion_memoria_label == "RUT" ~ 0L,
      TRUE ~ 1L
    )
  ) %>%
  # flag_atencion_fallida (de 01_...) queda reemplazado por FAILED_ATTENTION_CHECK, que es
  # equivalente pero mas confiable al usar la etiqueta en vez del codigo numerico.
  select(-flag_atencion_fallida)

write_csv(
  base_labeled_flags,
  file.path(processed_dir, "base_demanda_long_final_with_labels_and_flags.csv")
)

base_analysis_ready <- base_labeled_flags %>%
  rename(
    recommendation_probability = ans_p1_global,
    ambitious_score = ans_p2_1,
    trustworthy_score = ans_p2_2,
    competent_score = ans_p2_3,
    risky_continuity_score = ans_p2_4,
    salary_costly_score = ans_p2_5,
    operational_costly_score = ans_p2_6,
    long_term_commitment_score = ans_p2_7,
    promising_score = ans_p2_8,
    good_investment_score = ans_p2_9,
    expected_late_absence = ans_p3_1,
    expected_benefits_use = ans_p3_2,
    expected_accommodations = ans_p3_3,
    expected_quit_work = ans_p3_4,
    expected_new_challenges = ans_p3_5,
    expected_promotion = ans_p3_6
  ) %>%
  select(-matches("^(CLICK_POPIT_P1_T[AB]_P[12]|TIME_POPIT_P1_T[AB]_P[12])$"))

write_csv(
  base_analysis_ready,
  file.path(processed_dir, "base_demanda_analysis_ready.csv")
)

attention_check_summary <- base_labeled_flags %>%
  distinct(key, .keep_all = TRUE) %>%
  count(FAILED_ATTENTION_CHECK, check_atencion_memoria, check_atencion_memoria_label, name = "n") %>%
  arrange(desc(FAILED_ATTENTION_CHECK), check_atencion_memoria_label)

write_csv(attention_check_summary, out_path("attention_check_summary.csv"))

high_duration_cases <- base_labeled_flags %>%
  distinct(key, .keep_all = TRUE) %>%
  filter(DURATION_GT_P75 | DURATION_LT_6MIN) %>%
  select(
    key, numericalId, duration, DURATION_MINUTES,
    DURATION_LT_6MIN, DURATION_GT_P75, DURATION_GT_P95,
    accessCount, startTime, endTime, DEVICE, DEVICE_label
  ) %>%
  arrange(desc(DURATION_MINUTES))

write_csv(high_duration_cases, out_path("duration_flagged_cases.csv"))

qc_summary <- tibble(
  metric = c(
    "n_long_rows",
    "n_respondents",
    "n_profiles_per_respondent_expected",
    "netquest_extra_blocks_flagged",
    "respondents_with_time_capture_error_2plus",
    "respondents_under_p10_all_3_tasks",
    "respondents_duration_lt_6min",
    "respondents_duration_gt_p75",
    "respondents_failed_attention_check"
  ),
  value = c(
    nrow(base_labeled_flags),
    n_distinct(base_labeled_flags$key),
    2,
    paste(extra_blocks, collapse = "; "),
    raw_time_checks %>% summarise(n = sum(TIME_CAPTURE_ERROR_2PLUS, na.rm = TRUE)) %>% pull(n),
    base_labeled_flags %>% distinct(key, .keep_all = TRUE) %>% summarise(n = sum(UNDER_P10_ALL_3_TASKS, na.rm = TRUE)) %>% pull(n),
    base_labeled_flags %>% distinct(key, .keep_all = TRUE) %>% summarise(n = sum(DURATION_LT_6MIN, na.rm = TRUE)) %>% pull(n),
    base_labeled_flags %>% distinct(key, .keep_all = TRUE) %>% summarise(n = sum(DURATION_GT_P75, na.rm = TRUE)) %>% pull(n),
    base_labeled_flags %>% distinct(key, .keep_all = TRUE) %>% summarise(n = sum(FAILED_ATTENTION_CHECK, na.rm = TRUE)) %>% pull(n)
  )
)

write_csv(qc_summary, out_path("qc_summary.csv"))

print(qc_summary)
print(time_p10)
print(duration_thresholds)
print(block_counts %>% filter(NETQUEST_EXTRA_BLOCK_FLAG))
print(attention_check_summary)
finish_output()
