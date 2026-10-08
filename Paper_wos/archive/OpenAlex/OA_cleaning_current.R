############################################################
# STEP 1 — Sanity checks + cleaning for
# works_LATAM_2000_2025_SUBFIELD.csv
############################################################

library(dplyr)
library(readr)
library(stringr)

# --- 0) Load data ---
# Set your working dir if needed:
setwd("~/Google Drive/My Drive/Paper_Metodos_Exp/OpenAlex/")

df <- read_csv("works_LATAM_2000_2025_SUBFIELD.csv", show_col_types = FALSE)

cat("Rows:", nrow(df), " | Cols:", ncol(df), "\n")
cat(
  "Years:",
  min(df$year, na.rm = TRUE),
  "to",
  max(df$year, na.rm = TRUE),
  "\n"
)
cat("Countries:", n_distinct(df$country), "\n")

# --- 1) Column presence / types ---
expected_cols <- c(
  "id",
  "title",
  "year",
  "doi",
  "journal",
  "source_id",
  "type",
  "language",
  "version",
  "keywords",
  "abstract",
  "cited_by",
  "domain_name",
  "field_name",
  "subfield_name",
  "topic_ids",
  "topic_names",
  "country",
  "from_date",
  "to_date"
)

missing_cols <- setdiff(expected_cols, names(df))
if (length(missing_cols) > 0) {
  cat("\n⚠️ Missing columns:\n")
  print(missing_cols)
} else {
  cat("\n✅ All expected columns present.\n")
}

# Ensure key columns are the right type
df <- df %>%
  mutate(
    year = as.integer(year),
    cited_by = suppressWarnings(as.integer(cited_by)),
    doi = if_else(is.na(doi) | doi == "", NA_character_, doi),
    abstract = if_else(
      is.na(abstract) | abstract == "",
      NA_character_,
      abstract
    ),
    keywords = if_else(
      is.na(keywords) | keywords == "",
      NA_character_,
      keywords
    ),
    title = if_else(is.na(title) | title == "", NA_character_, title),
    journal = if_else(is.na(journal) | journal == "", NA_character_, journal),
    source_id = if_else(
      is.na(source_id) | source_id == "",
      NA_character_,
      source_id
    )
  )

# --- 2) Coverage checks (years/countries/types) ---
cat("\n--- Year counts (top/bottom) ---\n")
print(df %>% count(year, sort = TRUE) %>% head(10))
print(df %>% count(year, sort = FALSE) %>% head(10))

cat("\n--- Country counts (top 15) ---\n")
print(df %>% count(country, sort = TRUE) %>% head(15))

cat("\n--- Type counts (top 20) ---\n")
print(df %>% count(type, sort = TRUE) %>% head(20))

cat("\n--- Field/Subfield counts (top 20) ---\n")
print(df %>% count(field_name, sort = TRUE) %>% head(20))
print(df %>% count(subfield_name, sort = TRUE) %>% head(20))

# --- 3) Missingness diagnostics ---
miss_summary <- df %>%
  summarise(
    n = n(),
    pct_missing_title = mean(is.na(title)) * 100,
    pct_missing_abstract = mean(is.na(abstract)) * 100,
    pct_missing_keywords = mean(is.na(keywords)) * 100,
    pct_missing_doi = mean(is.na(doi)) * 100,
    pct_missing_journal = mean(is.na(journal)) * 100,
    pct_missing_field = mean(is.na(field_name)) * 100,
    pct_missing_subfield = mean(is.na(subfield_name)) * 100
  )

cat("\n--- Missingness (%) ---\n")
print(miss_summary)

# Missingness by year (abstract is key for your keyword search later)
miss_by_year <- df %>%
  group_by(year) %>%
  summarise(
    n = n(),
    pct_missing_abstract = round(mean(is.na(abstract)) * 100, 2),
    pct_missing_keywords = round(mean(is.na(keywords)) * 100, 2),
    pct_missing_title = round(mean(is.na(title)) * 100, 2),
    .groups = "drop"
  )

cat("\n--- Missingness by year (last 10 years) ---\n")
print(miss_by_year %>% arrange(desc(year)) %>% head(10))

# --- 4) Duplicate diagnostics ---
dup_id <- df %>%
  count(id, sort = TRUE) %>%
  filter(n > 1)

dup_doi <- df %>%
  filter(!is.na(doi)) %>%
  count(doi, sort = TRUE) %>%
  filter(n > 1)

cat("\n--- Duplicates by OpenAlex id ---\n")
cat("Duplicate ids:", nrow(dup_id), "\n")
if (nrow(dup_id) > 0) {
  print(head(dup_id, 10))
}

cat("\n--- Duplicates by DOI ---\n")
cat("Duplicate DOIs:", nrow(dup_doi), "\n")
if (nrow(dup_doi) > 0) {
  print(head(dup_doi, 10))
}


# --- 5) Create a cleaned, deduplicated version ---
# Rule:
#  - prefer the row with a non-missing abstract
#  - then prefer higher cited_by
#  - then keep first
df_clean <- df %>%
  mutate(
    has_abs = !is.na(abstract),
    cited_by = if_else(is.na(cited_by), 0L, cited_by)
  ) %>%
  arrange(desc(has_abs), desc(cited_by)) %>%
  distinct(id, .keep_all = TRUE) %>%
  select(-has_abs)

cat("\n✅ After dedup by id:\n")
cat("Rows:", nrow(df_clean), "\n")


## Identify fake dois
df_clean <- df_clean %>%
  mutate(
    doi_placeholder = str_detect(coalesce(doi, ""), "%x$")
  )

cat("\n--- DOI placeholder flag ---\n")
print(table(df_clean$doi_placeholder, useNA = "ifany"))


# Check remaining DOI duplicates after id-dedup (expected to still exist sometimes)
doi_dups_after <- df_clean %>%
  filter(!is.na(doi)) %>%
  count(doi, sort = TRUE) %>%
  filter(n > 1)

cat("\n--- Remaining duplicate DOIs after id-dedup ---\n")
cat("Count:", nrow(doi_dups_after), "\n")
if (nrow(doi_dups_after) > 0) {
  print(head(doi_dups_after, 10))
}

## We treat OpenAlex work identifiers as the unit of analysis.
## Some works share publisher-assigned DOI placeholders or represent different versions (e.g., preprints and published articles),
## which we retain as separate records but address in robustness checks

# --- 6) Quick “does topics look sane?” checks ---
# These should look like semi-colon separated strings, not 'c("...")'
cat("\n--- Topics format checks ---\n")
print(
  df_clean %>%
    transmute(
      topic_names_example = topic_names,
      topic_ids_example = topic_ids
    ) %>%
    slice_head(n = 5)
)

bad_topics <- df_clean %>%
  filter(
    str_detect(coalesce(topic_names, ""), "^c\\(") |
      str_detect(coalesce(topic_ids, ""), "^c\\(")
  )

cat("\nBad topic rows (should be 0):", nrow(bad_topics), "\n")

# --- 7) Save cleaned dataset for next steps ---
write_csv(df_clean, "works_LATAM_2000_2025_SUBFIELD_clean.csv")
cat("\n💾 Saved: works_LATAM_2000_2025_SUBFIELD_clean.csv\n")


### Now, we look at types of works to clean dataset for journal articles only.
df_clean <- read_csv("works_LATAM_2000_2025_SUBFIELD_clean.csv", show_col_types = FALSE)

df_clean %>%
  count(type, sort = TRUE) %>%
  mutate(pct = round(100 * n / sum(n), 2)) %>%
  print(n = 50)

## 92% are "article"

df_clean %>%
  count(year, type) %>%
  group_by(year) %>%
  mutate(pct = round(100 * n / sum(n), 2)) %>%
  arrange(year, desc(n)) %>%
  filter(year %in% c(2000, 2005, 2010, 2015, 2020, 2025)) %>%
  print(n = 200)

## journal/source coverage
df_clean %>%
  summarise(
    pct_with_journal = mean(!is.na(journal)) * 100,
    pct_with_source = mean(!is.na(source_id)) * 100,
    pct_with_doi = mean(!is.na(doi)) * 100
  ) %>%
  print()

## coverage by type
df_clean %>%
  group_by(type) %>%
  summarise(
    n = n(),
    pct_with_journal = round(mean(!is.na(journal)) * 100, 1),
    pct_with_source = round(mean(!is.na(source_id)) * 100, 1),
    pct_with_doi = round(mean(!is.na(doi)) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(n)) %>%
  print(n = 50)

## version info
df_clean %>%
  count(version, sort = TRUE) %>%
  mutate(pct = round(100 * n / sum(n), 2)) %>%
  print(n = 50)

df_clean %>%
  count(type, version, sort = TRUE) %>%
  print(n = 50)

## journals that are actually repositories / non-journal outlets (top)
df_clean %>%
  filter(!is.na(journal)) %>%
  count(journal, sort = TRUE) %>%
  head(30) %>%
  print(n = 30)


############################################################
# STEP 8 — Blacklist + peer-reviewed journal article filters
# (INCORPORATES YOUR NEW RULE: must have DOI + must have journal)
############################################################

# Updated blacklist (repositories, working papers, books, mega-journals)
journal_blacklist_terms <- c(
  # repositories / indexes / aggregators / conferences / pre-print
  "DOAJ",
  "Redalyc",
  "Dialnet",
  "Zenodo",
  "RePEc",
  "SSRN",
  "Repositorio",
  "Repository",
  "Institutional Repository",
  "SciELO Preprints",
  "OSF",
  "SocArXiv",
  "arXiv",
  "bioRxiv",
  "medRxiv",
  "AECID Library",
  "593 Digital Publisher CEIT",
  "Anais",

  # books / chapters platforms
  "eBooks",
  "Books",
  "Cambridge University Press eBooks",

  # NON–SOCIAL SCIENCE JOURNAL TERMS
  "Medical",
  "Medicine",
  "Clinical",
  "Health",
  "Public Health",
  "Nursing",
  "Midwifery",
  "Fertility",
  "Sterility",
  "Obstetrics",
  "Gynecology",
  "Gynaecology",
  "Pediatrics",
  "Paediatrics",
  "Oncology",
  "Cardiology",
  "Neurology",
  "Psychiatry",
  "Pharmacology",
  "Toxicology",
  "Nutrition",
  "Epidemiology",
  "Dentistry",
  "Veterinary",
  "Sports Medicine",
  "Physical Therapy",
  "Physiotherapy"
)

journal_blacklist <- regex(
  paste(journal_blacklist_terms, collapse = "|"),
  ignore_case = TRUE
)
# Quick check: how much of "articles with journals" get blacklisted?
df_clean %>%
  filter(type == "article", !is.na(journal)) %>%
  mutate(blacklisted = str_detect(journal, journal_blacklist)) %>%
  count(blacklisted) %>%
  mutate(pct = round(100 * n / sum(n), 2)) %>%
  print()

#“Approximately 8% of records labeled as journal articles in OpenAlex correspond to repository, working-paper, or non-journal platforms and are excluded from the main analysis.”

## Start filtering the data (MAIN SPEC)
# Rule you requested:
# - journal must be present
# - DOI must be present
# - must be an article
# - blacklist exclusions
# - drop DOI placeholders (%x)
df_articles <- df_clean %>%
  filter(
    type == "article",
    !is.na(journal),
    !is.na(doi),
    !doi_placeholder,
    !str_detect(journal, journal_blacklist)
  )

cat("\nRows before filter:", nrow(df_clean), "\n")
cat("Rows after journal+DOI article filter:", nrow(df_articles), "\n")

## check filtering
df_articles %>%
  count(year) %>%
  arrange(year) %>%
  print(n = 50)

df_articles %>%
  count(country, sort = TRUE) %>%
  head(15) %>%
  print(n = 15)

df_articles %>%
  summarise(
    pct_missing_abstract = round(mean(is.na(abstract)) * 100, 2),
    pct_missing_keywords = round(mean(is.na(keywords)) * 100, 2)
  ) %>%
  print()

# drop if no abstract and no keywords
df_articles <- df_articles %>%
  filter(
    !(is.na(abstract) & is.na(keywords))
  )

cat("Rows after abstract/keyword filter:", nrow(df_articles), "\n")

## filter data just with available abstracts
df_articles_abs <- df_articles %>%
  filter(!is.na(abstract) & str_trim(abstract) != "")

cat("Rows before:", nrow(df_articles), "\n")
cat("Rows with abstract:", nrow(df_articles_abs), "\n")
cat("Dropped:", nrow(df_articles) - nrow(df_articles_abs), "\n")

# Save final analysis dataset
write_csv(
  df_articles,
  "works_LATAM_2000_2025_peerreviewed_articles.csv"
)

write_csv(
  df_articles_abs,
  "works_LATAM_2000_2025_peerreviewed_articles_absonly.csv"
)




### Clean by journal title
df_articles_abs <- read_csv("works_LATAM_2000_2025_peerreviewed_articles_absonly.csv", show_col_types = FALSE)

## keep filtering by journal title
library(dplyr)
library(stringr)
library(tidyr)
library(stringi)

# df_articles = your current dataset (already filtered to type=="article" etc.)

# 1) Top journal names (to inspect for blacklisting)

top_not_blacklisted <- df_articles_abs %>%
  filter(!str_detect(journal, journal_blacklist)) %>%
  count(journal, sort = TRUE)

print(top_not_blacklisted, n = 50)

# 2) Quick “suggestion” tokens from those journal names (to spot patterns)
#    (remove very common stop-words; keep tokens >=4 chars)
stop_words <- c(
  "journal",
  "revista",
  "rev",
  "revue",
  "annals",
  "acta",
  "bulletin",
  "international",
  "research",
  "studies",
  "study",
  "science",
  "sciences",
  "scientific",
  "proceedings",
  "transactions",
  "series",
  "letters",
  "reports",
  "review",
  "reviews",
  "de",
  "da",
  "do",
  "del",
  "la",
  "el",
  "los",
  "las",
  "y",
  "e",
  "and",
  "of",
  "the",
  "for",
  "en",
  "in",
  "on",
  "a",
  "an"
)

token_counts <- top_not_blacklisted %>%
  mutate(journal_lc = str_to_lower(journal)) %>%
  mutate(journal_lc = str_replace_all(journal_lc, "[^[:alnum:]\\s]", " ")) %>%
  mutate(journal_lc = str_squish(journal_lc)) %>%
  separate_rows(journal_lc, sep = "\\s+") %>%
  filter(nchar(journal_lc) >= 4) %>%
  filter(!journal_lc %in% stop_words) %>%
  count(journal_lc, sort = TRUE) %>%
  slice_head(n = 200)

print(token_counts, n = 50)

# 3) For any token you suspect is "non social science", inspect examples quickly:
#    change token_here to whatever you want to check
token_here <- "policy"

df_articles_abs %>%
  filter(!is.na(journal)) %>%
  filter(!str_detect(journal, journal_blacklist)) %>%
  filter(str_detect(str_to_lower(journal), fixed(token_here))) %>%
  count(journal, sort = TRUE) %>%
  slice_head(n = 30) %>%
  print(n = 30)


## ===================================================================
## UNIFIED SOCIAL SCIENCE TERMS - Use for all disciplinary filters
## ===================================================================
## These terms indicate journals likely relevant to economics, 
## sociology, or political science
## ===================================================================

social_science_terms <- c(
  # Core social science identifiers
  "social", "sociales",
  "ciencia", "ciencias", "cientifica", "cientifico",
  "humano", "humanos", "humana", "humanas",
  
  # Research/study terms
  "trabajo", "trabajos",
  "estudio", "estudios",
  "investigaci", "investigacion",
  "research",
  
  # Political science
  "politic", "politica", "politicas", "politico", "politicos",
  "policy", "policies",
  "government", "gobierno", "governance", "gobernanza",
  "democracy", "democratico", "democratica",
  "poder", "power",
  
  # Economics
  "econom", "economia", "economicas", "economico", "economic",
  "desarrollo", "development",
  "mercado", "market",
  "financiero", "financial",
  "fiscal",
  "pobreza", "poverty",
  "desigualdad", "inequality",
  "empleo", "employment",
  
  # Sociology
  "sociedad", "sociedades", "society",
  "sociolog", "sociologia", "sociological",
  "cultura", "cultural",
  "comunidad", "community",
  "urbano", "urbana", "urban",
  "rural",
  "familia", "family",
  "genero", "gender",
  "clase", "class",
  "raza", "race", "racial", "etnico", "ethnic",
  "migracion", "migration", "inmigracion", "immigration",
  
  # Public/policy related
  "publica", "publico", "public",
  "estado", "state",
  "institucion", "institucional", "institution", "institutional",
  
  # Labor/work
  "labor", "labour", "laboral",
  "trabajo", "work",
  "sindicato", "union",
  
  # Social issues/mobility
  "equidad", "equity",
  "justicia", "justice",
  "movilidad", "mobility",
  "bienestar", "welfare", "wellbeing",
  
  # Organizational (relevant to organizational sociology/economics)
  "organizacion", "organizacional", "organization", "organizational",
  
  # Behavioral (behavioral economics, etc.)
  "comportamiento", "behavior", "behaviour", "behavioral", "behavioural",
  
  # Strategy (relevant to political science/economics)
  "estrateg", "strategic", "strategy",
  
  # Resource management (human resources → labor economics/sociology)
  "resource", "recursos",
  
  # Criminology/law & society (sociology/political science)
  "criminolog", "criminologia",
  "penal", "criminal"
)

# Create the pattern once for reuse
pattern_social_sci <- paste0(social_science_terms, collapse = "|")

# Normalize journal titles (strip accents, lowercase, squish)
df_articles_abs <- df_articles_abs %>%
  mutate(
    journal_norm = str_to_lower(stri_trans_general(coalesce(journal, ""), "Latin-ASCII")),
    journal_norm = str_squish(journal_norm)
  )

# ===== LAW FILTER =====
## Filter law journals (keeping only those with clear social science indicators)
# Create logical flags - capture more law-related terms
df_articles_abs <- df_articles_abs %>%
  mutate(
    has_law = str_detect(journal_norm, "\\b(derecho|juridico|juridica|legal|direito|jurisprudencia|ley|leyes|law)\\b"),
    has_social_sci = str_detect(journal_norm, pattern_social_sci)
  )

# Journals to remove: contain law terms but no social-science indicators
removed_law_journals <- df_articles_abs %>%
  filter(has_law & !has_social_sci) %>%
  count(journal, sort = TRUE)

# Inspect top removed journals (IMPORTANT: review before finalizing)
cat("\n=== TOP 50 LAW JOURNALS TO BE REMOVED ===\n")
removed_law_journals %>% slice_head(n = 50) %>% print(n = 50)

# Remove those rows from your dataset
df_articles_abs_clean <- df_articles_abs %>%
  filter(!(has_law & !has_social_sci)) %>%
  select(-has_law, -has_social_sci)

# Print summary stats
cat("\n=== SUMMARY ===\n")
cat("Total law journals removed:", nrow(removed_law_journals), "\n")
cat("Total articles removed:", sum(removed_law_journals$n), "\n")

# Print all removed journals
cat("\n=== ALL REMOVED LAW JOURNALS ===\n")
removed_law_journals %>% print(n = Inf)


# ===== MANAGEMENT FILTER =====
## Filter management journals (keeping only those with social science indicators)
df_articles_abs <- df_articles_abs %>%
  mutate(
    has_management = str_detect(journal_norm, "\\b(management|manag|gestion|gerencia|administracion|administra)\\b"),
    has_social_sci = str_detect(journal_norm, pattern_social_sci)
  )

# Journals to remove: contain "management" terms but no social-science indicators
removed_mgmt_journals <- df_articles_abs %>%
  filter(has_management & !has_social_sci) %>%
  count(journal, sort = TRUE)

# Inspect top removed journals (IMPORTANT: review before finalizing)
cat("\n=== TOP 30 MANAGEMENT JOURNALS TO BE REMOVED ===\n")
removed_mgmt_journals %>% slice_head(n = 30) %>% print(n = 30)

# Remove those rows from your dataset
df_articles_abs_clean <- df_articles_abs %>%
  filter(!(has_management & !has_social_sci)) %>%
  select(-has_management, -has_social_sci)

# Print summary stats
cat("\n=== SUMMARY ===\n")
cat("Total management journals removed:", nrow(removed_mgmt_journals), "\n")
cat("Total articles removed:", sum(removed_mgmt_journals$n), "\n")

# Print all removed journals
cat("\n=== ALL REMOVED MANAGEMENT JOURNALS ===\n")
removed_mgmt_journals %>% print(n = Inf)

# ===== EDUCATION FILTER =====
df_articles_abs <- df_articles_abs %>%
  mutate(
    has_education = str_detect(journal_norm, "\\b(education|educacion|educacao|educativa|educativo|pedagogia|pedagogy|ensenanza|ensino|teaching)\\b"),
    has_social_sci = str_detect(journal_norm, pattern_social_sci)
  )

removed_education_journals <- df_articles_abs %>%
  filter(has_education & !has_social_sci) %>%
  count(journal, sort = TRUE)

cat("\n=== TOP 50 EDUCATION JOURNALS TO BE REMOVED ===\n")
removed_education_journals %>% slice_head(n = 50) %>% print(n = 50)

df_articles_abs_clean <- df_articles_abs %>%
  filter(!(has_education & !has_social_sci)) %>%
  select(-has_education)

cat("\n=== EDUCATION SUMMARY ===\n")
cat("Total education journals removed:", nrow(removed_education_journals), "\n")
cat("Total articles removed:", sum(removed_education_journals$n), "\n")


# ===== BUSINESS FILTER =====
df_articles_abs_clean <- df_articles_abs_clean %>%
  mutate(
    has_business = str_detect(journal_norm, "\\b(business|negocio|negocios|empresa|empresarial|commerce|comercio|comercial|marketing|accounti|contab)\\b")
    , 
    has_social_sci = str_detect(journal_norm, pattern_social_sci)
  )

removed_business_journals <- df_articles_abs_clean %>%
  filter(has_business & !has_social_sci) %>%
  count(journal, sort = TRUE)

cat("\n=== TOP 50 BUSINESS JOURNALS TO BE REMOVED ===\n")
removed_business_journals %>% slice_head(n = 50) %>% print(n = 50)

df_articles_abs_clean <- df_articles_abs_clean %>%
  filter(!(has_business & !has_social_sci)) %>%
  select(-has_business)

cat("\n=== BUSINESS SUMMARY ===\n")
cat("Total business journals removed:", nrow(removed_business_journals), "\n")
cat("Total articles removed:", sum(removed_business_journals$n), "\n")

# ===== TECHNOLOGY FILTER =====
df_articles_abs_clean <- df_articles_abs_clean %>%
  mutate(
    has_technology = str_detect(journal_norm, "\\b(technology|tecnologia|technolog|technical|tecnica|tecnico|information|informatic|computing|computer|software|digital|sistemas|system)\\b")
    ,
    has_social_sci = str_detect(journal_norm, pattern_social_sci)
  )

removed_technology_journals <- df_articles_abs_clean %>%
  filter(has_technology & !has_social_sci) %>%
  count(journal, sort = TRUE)

cat("\n=== TOP 50 TECHNOLOGY JOURNALS TO BE REMOVED ===\n")
removed_technology_journals %>% slice_head(n = 50) %>% print(n = 50)

df_articles_abs_clean <- df_articles_abs_clean %>%
  filter(!(has_technology & !has_social_sci)) %>%
  select(-has_technology)

cat("\n=== TECHNOLOGY SUMMARY ===\n")
cat("Total technology journals removed:", nrow(removed_technology_journals), "\n")
cat("Total articles removed:", sum(removed_technology_journals$n), "\n")


# ===== ENGINEERING FILTER =====
df_articles_abs_clean <- df_articles_abs_clean %>%
  mutate(
    has_engineering = str_detect(journal_norm, "\\b(engineering|engineer|ingenieria|ingeniero|engenharia)\\b"),
    has_social_sci = str_detect(journal_norm, pattern_social_sci)
  )

removed_engineering_journals <- df_articles_abs_clean %>%
  filter(has_engineering & !has_social_sci) %>%
  count(journal, sort = TRUE)

cat("\n=== TOP 50 ENGINEERING JOURNALS TO BE REMOVED ===\n")
removed_engineering_journals %>% slice_head(n = 50) %>% print(n = 50)

df_articles_abs_clean <- df_articles_abs_clean %>%
  filter(!(has_engineering & !has_social_sci)) %>%
  select(-has_engineering, -has_social_sci)

cat("\n=== ENGINEERING SUMMARY ===\n")
cat("Total engineering journals removed:", nrow(removed_engineering_journals), "\n")
cat("Total articles removed:", sum(removed_engineering_journals$n), "\n")


## ===== HISTORY FILTER =====
df_articles_abs_clean <- df_articles_abs_clean %>%
  mutate(
    has_history = str_detect(journal_norm, "\\b(history|historia|historic|historico|historica)\\b"),
    has_social_sci = str_detect(journal_norm, pattern_social_sci)
  )

removed_history_journals <- df_articles_abs_clean %>%
  filter(has_history & !has_social_sci) %>%
  count(journal, sort = TRUE)

cat("\n=== TOP 50 HISTORY JOURNALS TO BE REMOVED ===\n")
removed_history_journals %>% slice_head(n = 50) %>% print(n = 50)

df_articles_abs_clean <- df_articles_abs_clean %>%
  filter(!(has_history & !has_social_sci)) %>%
  select(-has_history, -has_social_sci)

cat("\n=== HISTORY SUMMARY ===\n")
cat("Total history journals removed:", nrow(removed_history_journals), "\n")
cat("Total articles removed:", sum(removed_history_journals$n), "\n")


## ===== INFORMATION FILTER =====
df_articles_abs_clean <- df_articles_abs_clean %>%
  mutate(
    has_information = str_detect(journal_norm, "\\b(information|informacion|informacao|informatic|informatica)\\b"),
    has_social_sci = str_detect(journal_norm, pattern_social_sci)
  )

removed_information_journals <- df_articles_abs_clean %>%
  filter(has_information & !has_social_sci) %>%
  count(journal, sort = TRUE)

cat("\n=== TOP 50 INFORMATION JOURNALS TO BE REMOVED ===\n")
removed_information_journals %>% slice_head(n = 50) %>% print(n = 50)

df_articles_abs_clean <- df_articles_abs_clean %>%
  filter(!(has_information & !has_social_sci)) %>%
  select(-has_information, -has_social_sci)

cat("\n=== INFORMATION SUMMARY ===\n")
cat("Total information journals removed:", nrow(removed_information_journals), "\n")
cat("Total articles removed:", sum(removed_information_journals$n), "\n")


## ===== HEALTH FILTER =====
df_articles_abs_clean <- df_articles_abs_clean %>%
  mutate(
    has_health = str_detect(journal_norm, "\\b(health|salud|saude|medicina|medical|medico|medica|clinic|clinica|hospital|enferm|nursing|nurse|epidemiol|disease|doenca|enfermedad)\\b"),
    has_social_sci = str_detect(journal_norm, pattern_social_sci)
  )

removed_health_journals <- df_articles_abs_clean %>%
  filter(has_health & !has_social_sci) %>%
  count(journal, sort = TRUE)

cat("\n=== TOP 50 HEALTH JOURNALS TO BE REMOVED ===\n")
removed_health_journals %>% slice_head(n = 50) %>% print(n = 50)

df_articles_abs_clean <- df_articles_abs_clean %>%
  filter(!(has_health & !has_social_sci)) %>%
  select(-has_health, -has_social_sci)

cat("\n=== HEALTH SUMMARY ===\n")
cat("Total health journals removed:", nrow(removed_health_journals), "\n")
cat("Total articles removed:", sum(removed_health_journals$n), "\n")


## ===== SYSTEMS FILTER =====
df_articles_abs_clean <- df_articles_abs_clean %>%
  mutate(
    has_systems = str_detect(journal_norm, "\\b(systems|sistema|sistemas|systema)\\b"),
    has_social_sci = str_detect(journal_norm, pattern_social_sci)
  )

removed_systems_journals <- df_articles_abs_clean %>%
  filter(has_systems & !has_social_sci) %>%
  count(journal, sort = TRUE)

cat("\n=== TOP 50 SYSTEMS JOURNALS TO BE REMOVED ===\n")
removed_systems_journals %>% slice_head(n = 50) %>% print(n = 50)

df_articles_abs_clean <- df_articles_abs_clean %>%
  filter(!(has_systems & !has_social_sci)) %>%
  select(-has_systems, -has_social_sci)

cat("\n=== SYSTEMS SUMMARY ===\n")
cat("Total systems journals removed:", nrow(removed_systems_journals), "\n")
cat("Total articles removed:", sum(removed_systems_journals$n), "\n")


## ===== ENVIRONMENTAL FILTER =====
df_articles_abs_clean <- df_articles_abs_clean %>%
  mutate(
    has_environmental = str_detect(journal_norm, "\\b(environmental|environment|medio ambiente|ambiente|ambiental|ecolog|ecology|ecological|conservation|conservacion|conservacao|sustainability|sustentab)\\b"),
    has_social_sci = str_detect(journal_norm, pattern_social_sci)
  )

removed_environmental_journals <- df_articles_abs_clean %>%
  filter(has_environmental & !has_social_sci) %>%
  count(journal, sort = TRUE)

cat("\n=== TOP 50 ENVIRONMENTAL JOURNALS TO BE REMOVED ===\n")
removed_environmental_journals %>% slice_head(n = 50) %>% print(n = 50)

df_articles_abs_clean <- df_articles_abs_clean %>%
  filter(!(has_environmental & !has_social_sci)) %>%
  select(-has_environmental, -has_social_sci)

cat("\n=== ENVIRONMENTAL SUMMARY ===\n")
cat("Total environmental journals removed:", nrow(removed_environmental_journals), "\n")
cat("Total articles removed:", sum(removed_environmental_journals$n), "\n")


## ===== PSYCHOLOGY FILTER =====
df_articles_abs_clean <- df_articles_abs_clean %>%
  mutate(
    has_psychology = str_detect(journal_norm, "\\b(psychology|psicolog|psycholog|psiquiatr|psychiatr|mental|cognitiv)\\b"),
    has_social_sci = str_detect(journal_norm, pattern_social_sci)
  )

removed_psychology_journals <- df_articles_abs_clean %>%
  filter(has_psychology & !has_social_sci) %>%
  count(journal, sort = TRUE)

cat("\n=== TOP 50 PSYCHOLOGY JOURNALS TO BE REMOVED ===\n")
removed_psychology_journals %>% slice_head(n = 50) %>% print(n = 50)

df_articles_abs_clean <- df_articles_abs_clean %>%
  filter(!(has_psychology & !has_social_sci)) %>%
  select(-has_psychology, -has_social_sci)

cat("\n=== PSYCHOLOGY SUMMARY ===\n")
cat("Total psychology journals removed:", nrow(removed_psychology_journals), "\n")
cat("Total articles removed:", sum(removed_psychology_journals$n), "\n")


## ===== FINANCE FILTER =====
df_articles_abs_clean <- df_articles_abs_clean %>%
  mutate(
    has_finance = str_detect(journal_norm, "\\b(finance|finanza|financas|financeiro|financiera|financial|banking|banco|banca)\\b"),
    has_social_sci = str_detect(journal_norm, pattern_social_sci)
  )

removed_finance_journals <- df_articles_abs_clean %>%
  filter(has_finance & !has_social_sci) %>%
  count(journal, sort = TRUE)

cat("\n=== TOP 50 FINANCE JOURNALS TO BE REMOVED ===\n")
removed_finance_journals %>% slice_head(n = 50) %>% print(n = 50)

df_articles_abs_clean <- df_articles_abs_clean %>%
  filter(!(has_finance & !has_social_sci)) %>%
  select(-has_finance, -has_social_sci)

cat("\n=== FINANCE SUMMARY ===\n")
cat("Total finance journals removed:", nrow(removed_finance_journals), "\n")
cat("Total articles removed:", sum(removed_finance_journals$n), "\n")


## ===== COMMUNICATION FILTER =====
df_articles_abs_clean <- df_articles_abs_clean %>%
  mutate(
    has_communication = str_detect(journal_norm, "\\b(communication|comunicacion|comunicacao|comunica|media|medios|journalism|periodismo|jornalismo)\\b"),
    has_social_sci = str_detect(journal_norm, pattern_social_sci)
  )

removed_communication_journals <- df_articles_abs_clean %>%
  filter(has_communication & !has_social_sci) %>%
  count(journal, sort = TRUE)

cat("\n=== TOP 50 COMMUNICATION JOURNALS TO BE REMOVED ===\n")
removed_communication_journals %>% slice_head(n = 50) %>% print(n = 50)

df_articles_abs_clean <- df_articles_abs_clean %>%
  filter(!(has_communication & !has_social_sci)) %>%
  select(-has_communication, -has_social_sci)

cat("\n=== COMMUNICATION SUMMARY ===\n")
cat("Total communication journals removed:", nrow(removed_communication_journals), "\n")
cat("Total articles removed:", sum(removed_communication_journals$n), "\n")


## ===== ELECTRONICS FILTER =====
df_articles_abs_clean <- df_articles_abs_clean %>%
  mutate(
    has_electronics = str_detect(journal_norm, "\\b(electronic|electronica|eletronica|eletrica|electrica|electrical|circuit|semiconductor)\\b"),
    has_social_sci = str_detect(journal_norm, pattern_social_sci)
  )

removed_electronics_journals <- df_articles_abs_clean %>%
  filter(has_electronics & !has_social_sci) %>%
  count(journal, sort = TRUE)

cat("\n=== TOP 50 ELECTRONICS JOURNALS TO BE REMOVED ===\n")
removed_electronics_journals %>% slice_head(n = 50) %>% print(n = 50)

df_articles_abs_clean <- df_articles_abs_clean %>%
  filter(!(has_electronics & !has_social_sci)) %>%
  select(-has_electronics, -has_social_sci)

cat("\n=== ELECTRONICS SUMMARY ===\n")
cat("Total electronics journals removed:", nrow(removed_electronics_journals), "\n")
cat("Total articles removed:", sum(removed_electronics_journals$n), "\n")


## ===== FINAL OUTPUT =====
cat("\n=== OVERALL SUMMARY ===\n")
cat("Rows remaining after all filters:", nrow(df_articles_abs_clean), "\n")

# Save final cleaned dataset
readr::write_csv(df_articles_abs_clean, "works_cleaned_all_filters.csv")

## ===================================================================
## CREATE COMPREHENSIVE EXCEL FILE OF ALL REMOVED JOURNALS
## ===================================================================

library(writexl)

# Combine all removed journal datasets with filter labels
all_removed_journals <- bind_rows(
  removed_law_journals %>% mutate(filter = "Law"),
  removed_mgmt_journals %>% mutate(filter = "Management"),
  removed_education_journals %>% mutate(filter = "Education"),
  removed_business_journals %>% mutate(filter = "Business"),
  removed_technology_journals %>% mutate(filter = "Technology"),
  removed_engineering_journals %>% mutate(filter = "Engineering"),
  removed_policy_journals %>% mutate(filter = "Policy"),
  removed_history_journals %>% mutate(filter = "History"),
  removed_information_journals %>% mutate(filter = "Information"),
  removed_health_journals %>% mutate(filter = "Health"),
  removed_systems_journals %>% mutate(filter = "Systems"),
  removed_environmental_journals %>% mutate(filter = "Environmental"),
  removed_psychology_journals %>% mutate(filter = "Psychology"),
  removed_finance_journals %>% mutate(filter = "Finance"),
  removed_communication_journals %>% mutate(filter = "Communication"),
  removed_electronics_journals %>% mutate(filter = "Electronics")
)

# Rename columns for clarity
all_removed_journals <- all_removed_journals %>%
  rename(
    journal_title = journal,
    articles_removed = n,
    removed_by_filter = filter
  ) %>%
  select(removed_by_filter, journal_title, articles_removed) %>%
  arrange(removed_by_filter, desc(articles_removed))

# Create summary statistics by filter
summary_by_filter <- all_removed_journals %>%
  group_by(removed_by_filter) %>%
  summarise(
    total_journals_removed = n(),
    total_articles_removed = sum(articles_removed),
    .groups = "drop"
  ) %>%
  arrange(desc(total_articles_removed))

# Create overall summary
overall_summary <- tibble(
  metric = c(
    "Total unique journals removed",
    "Total articles removed",
    "Total filters applied"
  ),
  value = c(
    nrow(all_removed_journals),
    sum(all_removed_journals$articles_removed),
    n_distinct(all_removed_journals$removed_by_filter)
  )
)

# Write to Excel with multiple sheets
write_xlsx(
  list(
    "All Removed Journals" = all_removed_journals,
    "Summary by Filter" = summary_by_filter,
    "Overall Summary" = overall_summary
  ),
  path = "removed_journals_comprehensive.xlsx"
)

cat("\n=== EXCEL FILE CREATED ===\n")
cat("File: removed_journals_comprehensive.xlsx\n")
cat("Sheets:\n")
cat("  1. All Removed Journals - Complete list with filter and article counts\n")
cat("  2. Summary by Filter - Totals per filter\n")
cat("  3. Overall Summary - Grand totals\n\n")

# Also print summary to console
cat("=== SUMMARY BY FILTER ===\n")
print(summary_by_filter, n = Inf)

cat("\n=== OVERALL SUMMARY ===\n")
print(overall_summary)