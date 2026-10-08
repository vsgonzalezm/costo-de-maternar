# ============================================================================
# 03 - Univariate descriptive checks for final analysis dataset
# Author: valentinagonzalez
# Modified: Tania Hutt - arreglo de errores
# Date: agosto 2026
# Purpose: Runs respondent-level and profile-level descriptive checks using the final labelled dataset.
# ============================================================================

library(tidyverse)
library(here)

project_dir <- if (dir.exists(here("Encuesta Demanda", "Data"))) {
  here("Encuesta Demanda")
} else {
  here()
}

clean_netquest_nulls <- function(data) {
  data %>%
    mutate(across(where(is.character), ~ na_if(str_squish(.x), "#NULL!"))) %>%
    mutate(across(where(is.character), ~ na_if(str_squish(.x), "NULL!")))
}

# ============================================================================
# DESCRIPTIVE SCRIPT - DEMANDA
# ============================================================================
# Simple univariate descriptives for the analysis-ready long database.
# Run after 01_prepare_long_profile_dataset_from_netquest_excel.R and 02_add_netquest_labels_timing_quality_flags.R.
base_long_final <- read_csv(
  file.path(project_dir, 'Data', 'processed', 'base_demanda_analysis_ready.csv'),
  show_col_types = FALSE
) %>%
  mutate(across(where(is.character), ~ na_if(.x, ''))) %>%
  clean_netquest_nulls()

print_section <- function(title) {
  print('')
  print(paste0('=== ', title, ' ==='))
}

freq_table <- function(x, recode_map = NULL) {
  values <- as.character(x)
  if (!is.null(recode_map)) {
    values <- recode(values, !!!recode_map, .default = values)
  }
  tibble(value = values) %>%
    mutate(value = replace_na(value, '(NA)')) %>%
    count(value, name = 'n', sort = TRUE) %>%
    mutate(pct = round(100 * n / sum(n), 1))
}

num_table <- function(x) {
  tibble(value = as.numeric(x)) %>%
    summarise(
      n = sum(!is.na(value)),
      mean = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      min = min(value, na.rm = TRUE),
      p25 = quantile(value, 0.25, na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      p75 = quantile(value, 0.75, na.rm = TRUE),
      max = max(value, na.rm = TRUE)
    )
}

label_var_map <- c(
  SEXO = "SEXO_label",
  NSE = "NSE_label",
  ZONA = "ZONA_label",
  EDADR = "EDADR_label",
  check_atencion_memoria = "check_atencion_memoria_label",
  enc_estado_civil = "enc_estado_civil_label",
  enc_tiene_hijos = "enc_tiene_hijos_label",
  enc_numero_hijos = "enc_numero_hijos_label",
  enc_educacion = "enc_educacion_label",
  filtro_cargo_actual = "filtro_cargo_actual_label",
  filtro_anos_experiencia = "filtro_anos_experiencia_label",
  filtro_rubro_empresa = "filtro_rubro_empresa_label",
  filtro_decision_contratacion = "filtro_decision_contratacion_label",
  filtro_personal_cargo = "filtro_personal_cargo_label"
)

resolve_freq_var <- function(data, var) {
  mapped_var <- unname(label_var_map[var])
  auto_label_var <- paste0(var, "_label")

  if (!is.na(mapped_var) && mapped_var %in% names(data)) {
    return(mapped_var)
  }

  if (auto_label_var %in% names(data)) {
    return(auto_label_var)
  }

  var
}

show_freq <- function(label, data, var, recode_map = NULL) {
  print_section(label)
  display_var <- resolve_freq_var(data, var)
  print(freq_table(data[[display_var]], recode_map = recode_map))
}

show_num <- function(label, data, var) {
  print_section(label)
  print(num_table(data[[var]]))
}

respondent_level_data <- function(data) {
  data %>% distinct(key, .keep_all = TRUE)
}

show_freq_respondent <- function(label, data, var, recode_map = NULL) {
  show_freq(label, respondent_level_data(data), var, recode_map = recode_map)
}

show_num_respondent <- function(label, data, var) {
  show_num(label, respondent_level_data(data), var)
}

# ============================================================================
# 1. BASIC DATA STRUCTURE
# ============================================================================
print_section('BASE STRUCTURE')
print(dim(base_long_final))
print(n_distinct(base_long_final$numericalId))
print(base_long_final %>% count(pantalla_orden, name = 'n'))
print(base_long_final %>% count(numericalId, name = 'n_rows') %>% count(n_rows, name = 'respondents'))

# ============================================================================
# 2. RESPONDENT-LEVEL DESCRIPTIVES
# ============================================================================
show_freq_respondent('RESPONDENT CHARACTERISTICS: Gender', base_long_final, 'SEXO')
show_freq_respondent('RESPONDENT CHARACTERISTICS: Socioeconomic status', base_long_final, 'NSE')
show_freq_respondent('RESPONDENT CHARACTERISTICS: Region/zone', base_long_final, 'ZONA')
show_num_respondent('RESPONDENT CHARACTERISTICS: Age', base_long_final, 'EDAD')
show_freq_respondent('RESPONDENT CHARACTERISTICS: Marital status', base_long_final, 'enc_estado_civil')
show_freq_respondent('RESPONDENT CHARACTERISTICS: Has children', base_long_final, 'enc_tiene_hijos')
show_freq_respondent('RESPONDENT CHARACTERISTICS: Number of children', base_long_final, 'enc_numero_hijos')
show_freq_respondent('RESPONDENT CHARACTERISTICS: Education', base_long_final, 'enc_educacion')

show_freq_respondent('ATTENTION AND SCREENERS: Attention check', base_long_final, 'check_atencion_memoria')
show_freq_respondent('ATTENTION AND SCREENERS: Failed attention check', base_long_final, 'FAILED_ATTENTION_CHECK')
show_freq_respondent('ATTENTION AND SCREENERS: Decision-making frequency', base_long_final, 'filtro_decision_contratacion')
show_freq_respondent('ATTENTION AND SCREENERS: Current job title', base_long_final, 'filtro_cargo_actual')
show_freq_respondent('ATTENTION AND SCREENERS: Years of experience', base_long_final, 'filtro_anos_experiencia')
show_freq_respondent('ATTENTION AND SCREENERS: Industry sector', base_long_final, 'filtro_rubro_empresa')
show_freq_respondent('ATTENTION AND SCREENERS: People management role', base_long_final, 'filtro_personal_cargo')
show_freq_respondent('ATTENTION AND SCREENERS: Weekly hours', base_long_final, 'filtro_horas_semana_abierta')

# ============================================================================
# 3. PROFILE-LEVEL DESCRIPTIVES
# ============================================================================
show_freq('PROFILE STRUCTURE: Order', base_long_final, 'pantalla_orden')
show_freq('PROFILE STRUCTURE: Block', base_long_final, 'bloque_id')
show_freq('PROFILE STRUCTURE: Context template', base_long_final, 'template_contexto')
show_freq('PROFILE ATTRIBUTES: Gender', base_long_final, 'perfil_genero')
show_freq('PROFILE ATTRIBUTES: Occupation', base_long_final, 'perfil_ocupacion')
show_freq('PROFILE ATTRIBUTES: Education', base_long_final, 'perfil_educacion')
show_freq('PROFILE ATTRIBUTES: Experience', base_long_final, 'perfil_exp')
show_freq('PROFILE ATTRIBUTES: Performance', base_long_final, 'perfil_desempeno')
show_freq('PROFILE ATTRIBUTES: Marital status', base_long_final, 'perfil_estado_civil')
show_freq('PROFILE ATTRIBUTES: Parental status', base_long_final, 'perfil_parental')
show_freq('PROFILE ATTRIBUTES: Fertility plans', base_long_final, 'perfil_plan_futuro')

show_freq('PROFILE SCALE: Recommendation probability', base_long_final, 'recommendation_probability')
show_freq('PROFILE SCALE: Ambitious', base_long_final, 'ambitious_score')
show_freq('PROFILE SCALE: Trustworthy', base_long_final, 'trustworthy_score')
show_freq('PROFILE SCALE: Competent', base_long_final, 'competent_score')
show_freq('PROFILE SCALE: Risky in job continuity', base_long_final, 'risky_continuity_score')
show_freq('PROFILE SCALE: Costly in salary terms', base_long_final, 'salary_costly_score')
show_freq('PROFILE SCALE: Costly operationally', base_long_final, 'operational_costly_score')
show_freq('PROFILE SCALE: Committed long term', base_long_final, 'long_term_commitment_score')
show_freq('PROFILE SCALE: Promising labor market potential', base_long_final, 'promising_score')
show_freq('PROFILE SCALE: Good investment', base_long_final, 'good_investment_score')

show_freq('EXPECTED BEHAVIOR: Arrive late / be absent', base_long_final, 'expected_late_absence')
show_freq('EXPECTED BEHAVIOR: Use company benefits', base_long_final, 'expected_benefits_use')
show_freq('EXPECTED BEHAVIOR: Request accommodations', base_long_final, 'expected_accommodations')
show_freq('EXPECTED BEHAVIOR: Quit or stop working', base_long_final, 'expected_quit_work')
show_freq('EXPECTED BEHAVIOR: Take on new challenges', base_long_final, 'expected_new_challenges')
show_freq('EXPECTED BEHAVIOR: Be promoted', base_long_final, 'expected_promotion')

# ============================================================================
# 4. OPEN-ENDED QUESTIONS AND SIMPLE CODING
# ============================================================================
show_freq('OPEN-ENDED RAW RESPONSES: Costly', base_long_final, 'abierta_costoso')
show_freq('OPEN-ENDED RAW RESPONSES: Investment', base_long_final, 'abierta_inversion')

show_freq('OPEN-ENDED THEMES: Costly', base_long_final, 'abierta_costoso_simple', recode_map = c(
  'Costo/sueldo' = 'Cost / salary',
  'Capacitacion/experiencia' = 'Training / experience',
  'Licencias/ausencias/maternidad' = 'Leave / absences / maternity',
  'Desempeno/productividad' = 'Performance / productivity',
  'Confianza/compromiso' = 'Trust / commitment',
  'Otro' = 'Other'
))
show_freq('OPEN-ENDED THEMES: Investment', base_long_final, 'abierta_inversion_simple', recode_map = c(
  'Habilidades/experiencia' = 'Skills / experience',
  'Aporte/productividad' = 'Contribution / productivity',
  'Carga/complejidad' = 'Workload / complexity',
  'Compromiso/confianza' = 'Commitment / trust',
  'Otro' = 'Other'
))

show_freq('OTHER OPEN FIELDS: Job title', base_long_final, 'filtro_cargo_otro')
show_freq('OTHER OPEN FIELDS: Industry sector', base_long_final, 'filtro_rubro_otro')
show_freq('OTHER OPEN FIELDS: Cleaned job title', base_long_final, 'filtro_cargo_otro_clean')
show_freq('OTHER OPEN FIELDS: Cleaned industry sector', base_long_final, 'filtro_rubro_otro_clean')
show_freq('OTHER OPEN FIELDS: Simplified job title', base_long_final, 'filtro_cargo_simple', recode_map = c(
  'Independiente' = 'Independent',
  'Sin trabajo/Cesante' = 'Unemployed',
  'Profesional/Administrativo' = 'Professional / administrative',
  'Jefatura/Direccion' = 'Management / leadership',
  'Tecnico/Operativo' = 'Technical / operational',
  'Otro' = 'Other'
))
show_freq('OTHER OPEN FIELDS: Simplified industry sector', base_long_final, 'filtro_rubro_simple', recode_map = c(
  'Turismo/Eventos' = 'Tourism / events',
  'Comercio/Alimentos' = 'Retail / food',
  'Servicios profesionales' = 'Professional services',
  'Industria/Tecnologia/Agro' = 'Industry / technology / agriculture',
  'Mantenimiento/Seguridad' = 'Maintenance / security',
  'Sin trabajo' = 'Unemployed',
  'Independiente' = 'Independent',
  'Sector publico/Mutualidades' = 'Public sector / mutual societies',
  'Otro' = 'Other'
))

# ============================================================================
# 5. BEHAVIOR AND TIME VARIABLES
# ============================================================================
show_freq('BEHAVIOR: Pop-it click', base_long_final, 'click_popit')
show_num('BEHAVIOR: Pop-it time', base_long_final, 'time_popit')
show_num('BEHAVIOR: Profile screen time', base_long_final, 'time_screen_perfil')
show_num_respondent('BEHAVIOR: Career screen time', base_long_final, 'TIME_SCREEN_CARGO')
show_num_respondent('BEHAVIOR: Task screen time', base_long_final, 'TIME_SCREEN_TAREA')
show_freq_respondent('BEHAVIOR FLAGS: Under p10 in all 3 timing tasks', base_long_final, 'UNDER_P10_ALL_3_TASKS')
show_freq_respondent('BEHAVIOR FLAGS: Time capture error count', base_long_final, 'TIME_CAPTURE_ERROR_COUNT')
show_freq_respondent('BEHAVIOR FLAGS: Time capture error 2+', base_long_final, 'TIME_CAPTURE_ERROR_2PLUS')
show_freq_respondent('BEHAVIOR FLAGS: Duration below 6 minutes', base_long_final, 'DURATION_LT_6MIN')
show_freq_respondent('BEHAVIOR FLAGS: Duration above p75', base_long_final, 'DURATION_GT_P75')
show_freq_respondent('BEHAVIOR FLAGS: Netquest extra block', base_long_final, 'NETQUEST_EXTRA_BLOCK_FLAG')
show_num_respondent('BEHAVIOR: Postulante 1 screen time', base_long_final, 'TIME_SCREEN_POSTULANTE_1')
show_num_respondent('BEHAVIOR: Postulante 2 screen time', base_long_final, 'TIME_SCREEN_POSTULANTE_2')


# ============================================================================
# 6. QUICK NUMERIC CHECKS
# ============================================================================
show_num_respondent('NUMERIC SUMMARY: Duration', base_long_final, 'duration')
show_num_respondent('NUMERIC SUMMARY: Age', base_long_final, 'EDAD')
show_num('NUMERIC SUMMARY: Pop-it time', base_long_final, 'time_popit')
show_num('NUMERIC SUMMARY: Profile screen time', base_long_final, 'time_screen_perfil')
show_num_respondent('NUMERIC SUMMARY: Career screen time', base_long_final, 'TIME_SCREEN_CARGO')
show_num_respondent('NUMERIC SUMMARY: Task screen time', base_long_final, 'TIME_SCREEN_TAREA')

