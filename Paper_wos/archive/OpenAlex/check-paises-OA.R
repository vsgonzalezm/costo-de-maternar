############################################################
# ANALYZE OpenAlex long country-audit file
# Focus:
# 1) missing institution country code despite raw affiliation text
# 2) mismatch between authorship countries and institution country code
############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(purrr)
})

# ---------------------------------------------------------
# 1) Load file
# ---------------------------------------------------------
long_file <- "~/Google Drive/My Drive/Paper_Metodos_Exp/OpenAlex/test_ASR_2015_2025_ALLVARS_COUNTRY_AUDIT_LONG.csv"

d <- read_csv(long_file, show_col_types = FALSE)

glimpse(d)

# ---------------------------------------------------------
# 2) Basic cleaning
# ---------------------------------------------------------
d2 <- d %>%
  mutate(
    across(c(
      work_id, title, author_id, author_name, authorship_countries,
      raw_affiliation_strings, institution_id, institution_name,
      institution_country_code
    ), ~na_if(.x, "")),
    
    has_raw_affiliation = !is.na(raw_affiliation_strings),
    has_institution_id = !is.na(institution_id),
    has_institution_name = !is.na(institution_name),
    has_institution_country = !is.na(institution_country_code),
    has_authorship_countries = !is.na(authorship_countries)
  )

# helper: split semicolon-separated country string into vector
split_codes <- function(x){
  if (is.na(x) || x == "") return(character(0))
  vals <- str_split(x, ";", simplify = FALSE)[[1]]
  vals <- str_trim(vals)
  vals[vals != ""]
}

# one row may have authorship_countries like "US; CA"
# flag whether institution_country_code is inside that set
d2 <- d2 %>%
  rowwise() %>%
  mutate(
    authorship_country_n = length(unique(split_codes(authorship_countries))),
    institution_matches_authorship =
      ifelse(
        is.na(authorship_countries) | is.na(institution_country_code),
        NA,
        institution_country_code %in% split_codes(authorship_countries)
      )
  ) %>%
  ungroup()

# ---------------------------------------------------------
# 3) High-level descriptives
# ---------------------------------------------------------
cat("\n==============================\n")
cat("BASIC COUNTS\n")
cat("==============================\n")

cat("Rows:", nrow(d2), "\n")
cat("Unique works:", n_distinct(d2$work_id), "\n")
cat("Unique authors (IDs):", n_distinct(d2$author_id, na.rm = TRUE), "\n")
cat("Unique institutions (IDs):", n_distinct(d2$institution_id, na.rm = TRUE), "\n")

cat("\nShare with raw affiliation string:\n")
print(mean(d2$has_raw_affiliation, na.rm = TRUE))

cat("\nShare with institution country code:\n")
print(mean(d2$has_institution_country, na.rm = TRUE))

cat("\nShare with authorship countries:\n")
print(mean(d2$has_authorship_countries, na.rm = TRUE))

cat("\nDistribution of institution_country_code:\n")
print(d2 %>% count(institution_country_code, sort = TRUE), n = 50)

cat("\nDistribution of authorship_country_n:\n")
print(d2 %>% count(authorship_country_n, sort = FALSE))

# ---------------------------------------------------------
# 4) CASE A: missing institution country code
# ---------------------------------------------------------
cat("\n==============================\n")
cat("CASE A: MISSING institution_country_code\n")
cat("==============================\n")

missing_inst_country <- d2 %>%
  filter(!has_institution_country)

cat("Rows with missing institution country code:", nrow(missing_inst_country), "\n")
cat("Unique works:", n_distinct(missing_inst_country$work_id), "\n")

cat("\nBreakdown of missing institution country cases:\n")
print(
  missing_inst_country %>%
    summarise(
      pct_with_raw_affiliation = mean(has_raw_affiliation, na.rm = TRUE),
      pct_with_institution_id = mean(has_institution_id, na.rm = TRUE),
      pct_with_institution_name = mean(has_institution_name, na.rm = TRUE),
      pct_with_authorship_countries = mean(has_authorship_countries, na.rm = TRUE)
    )
)

cat("\nMost common institution names among missing country cases:\n")
print(
  missing_inst_country %>%
    count(institution_name, sort = TRUE) %>%
    filter(!is.na(institution_name)),
  n = 50
)

cat("\nMost common raw affiliation strings among missing country cases:\n")
print(
  missing_inst_country %>%
    count(raw_affiliation_strings, sort = TRUE) %>%
    filter(!is.na(raw_affiliation_strings)),
  n = 50
)

cat("\nRows with raw affiliation present but institution country missing:\n")
missing_raw_present <- missing_inst_country %>%
  filter(has_raw_affiliation)

print(
  missing_raw_present %>%
    select(work_id, title, authorship_position, author_name,
           raw_affiliation_strings, institution_name, institution_id,
           authorship_countries, institution_country_code) %>%
    arrange(title, authorship_position),
  n = 100
)

# save these for manual review
write_csv(
  missing_raw_present,
  "~/Google Drive/My Drive/Paper_Metodos_Exp/OpenAlex/diagnostic_missing_inst_country_raw_present.csv"
)

# ---------------------------------------------------------
# 5) CASE B: mismatch between authorship countries and institution country
# ---------------------------------------------------------
cat("\n==============================\n")
cat("CASE B: authorship/institution mismatch\n")
cat("==============================\n")

mismatch <- d2 %>%
  filter(
    !is.na(authorship_countries),
    !is.na(institution_country_code),
    institution_matches_authorship == FALSE
  )

cat("Rows with mismatch:", nrow(mismatch), "\n")
cat("Unique works with mismatch:", n_distinct(mismatch$work_id), "\n")

cat("\nExamples of mismatch rows:\n")
print(
  mismatch %>%
    select(work_id, title, authorship_position, author_name,
           authorship_countries, institution_name, institution_country_code,
           raw_affiliation_strings) %>%
    arrange(title, authorship_position),
  n = 100
)

cat("\nMost common mismatch institution country codes:\n")
print(
  mismatch %>%
    count(institution_country_code, sort = TRUE),
  n = 50
)

cat("\nMost common authorship country strings in mismatch rows:\n")
print(
  mismatch %>%
    count(authorship_countries, sort = TRUE),
  n = 50
)

cat("\nCross-tab of mismatch patterns:\n")
print(
  mismatch %>%
    count(authorship_countries, institution_country_code, sort = TRUE),
  n = 100
)

# save for manual review
write_csv(
  mismatch,
  "~/Google Drive/My Drive/Paper_Metodos_Exp/OpenAlex/diagnostic_authorship_institution_mismatch.csv"
)

# ---------------------------------------------------------
# 6) Diagnose whether mismatch is driven by multi-country authorships
# ---------------------------------------------------------
cat("\n==============================\n")
cat("WHY MISMATCH? MULTI-COUNTRY AUTHORSHIPS\n")
cat("==============================\n")

print(
  d2 %>%
    count(authorship_country_n, institution_matches_authorship) %>%
    arrange(authorship_country_n, institution_matches_authorship)
)

cat("\nMismatch rate by number of authorship countries:\n")
print(
  d2 %>%
    filter(!is.na(institution_matches_authorship)) %>%
    group_by(authorship_country_n) %>%
    summarise(
      n = n(),
      mismatch_rate = mean(!institution_matches_authorship),
      .groups = "drop"
    ) %>%
    arrange(authorship_country_n)
)

# ---------------------------------------------------------
# 7) Collapse to authorship level
# One author-position per work, to see if mismatch happens because
# one author has multiple institutions attached
# ---------------------------------------------------------
authorship_level <- d2 %>%
  group_by(work_id, title, authorship_position, author_id, author_name,
           authorship_countries, raw_affiliation_strings) %>%
  summarise(
    n_rows = n(),
    n_institutions = n_distinct(institution_id, na.rm = TRUE),
    institution_names = paste(unique(na.omit(institution_name)), collapse = " | "),
    institution_countries = paste(unique(na.omit(institution_country_code)), collapse = "; "),
    any_missing_inst_country = any(is.na(institution_country_code)),
    any_mismatch = any(institution_matches_authorship == FALSE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    n_institution_countries =
      map_int(institution_countries, ~length(unique(split_codes(.x))))
  )

cat("\n==============================\n")
cat("AUTHORSHIP-LEVEL SUMMARY\n")
cat("==============================\n")

cat("\nDistribution of number of institutions attached to one authorship:\n")
print(authorship_level %>% count(n_institutions, sort = FALSE))

cat("\nDistribution of number of institution countries within one authorship:\n")
print(authorship_level %>% count(n_institution_countries, sort = FALSE))

cat("\nAuthorships with >1 institution attached:\n")
print(
  authorship_level %>%
    filter(n_institutions > 1) %>%
    select(work_id, title, authorship_position, author_name,
           authorship_countries, institution_names, institution_countries,
           n_institutions, n_institution_countries, any_missing_inst_country, any_mismatch),
  n = 100
)

cat("\nAuthorships with mismatch at the authorship level:\n")
print(
  authorship_level %>%
    filter(any_mismatch) %>%
    select(work_id, title, authorship_position, author_name,
           authorship_countries, institution_names, institution_countries,
           n_institutions, n_institution_countries),
  n = 100
)

write_csv(
  authorship_level,
  "~/Google Drive/My Drive/Paper_Metodos_Exp/OpenAlex/diagnostic_authorship_level_summary.csv"
)

# ---------------------------------------------------------
# 8) Work-level summary
# ---------------------------------------------------------
work_level <- d2 %>%
  group_by(work_id, title) %>%
  summarise(
    n_rows = n(),
    n_authors = n_distinct(author_id, na.rm = TRUE),
    n_authorship_positions = n_distinct(authorship_position, na.rm = TRUE),
    n_institutions = n_distinct(institution_id, na.rm = TRUE),
    n_institution_countries = n_distinct(institution_country_code, na.rm = TRUE),
    any_missing_inst_country = any(is.na(institution_country_code)),
    any_mismatch = any(institution_matches_authorship == FALSE, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n==============================\n")
cat("WORK-LEVEL SUMMARY\n")
cat("==============================\n")

cat("\nWorks with any missing institution country:\n")
print(mean(work_level$any_missing_inst_country, na.rm = TRUE))

cat("\nWorks with any mismatch:\n")
print(mean(work_level$any_mismatch, na.rm = TRUE))

print(
  work_level %>%
    count(any_missing_inst_country, any_mismatch)
)

write_csv(
  work_level,
  "~/Google Drive/My Drive/Paper_Metodos_Exp/OpenAlex/diagnostic_work_level_summary.csv"
)

# ---------------------------------------------------------
# 9) Narrow tables to inspect by hand
# ---------------------------------------------------------
# A. Most suspicious missing-country cases:
suspect_missing <- d2 %>%
  filter(
    is.na(institution_country_code),
    !is.na(raw_affiliation_strings)
  ) %>%
  select(work_id, title, authorship_position, author_name,
         raw_affiliation_strings, institution_name, institution_id,
         authorship_countries)

write_csv(
  suspect_missing,
  "~/Google Drive/My Drive/Paper_Metodos_Exp/OpenAlex/inspect_missing_country_cases.csv"
)

# B. Most suspicious mismatches:
suspect_mismatch <- d2 %>%
  filter(
    !is.na(authorship_countries),
    !is.na(institution_country_code),
    institution_matches_authorship == FALSE
  ) %>%
  select(work_id, title, authorship_position, author_name,
         authorship_countries, institution_country_code,
         institution_name, institution_id, raw_affiliation_strings)

write_csv(
  suspect_mismatch,
  "~/Google Drive/My Drive/Paper_Metodos_Exp/OpenAlex/inspect_mismatch_cases.csv"
)

cat("\nDone. Diagnostic files written.\n")