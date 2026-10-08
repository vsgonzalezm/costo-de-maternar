library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# Load merged data
all_data <- read_csv("works_LATAM_2000_2025.csv")

# -------------------------
# 1. Clean / expand topics
# -------------------------
all_data_topics <- all_data %>%
  separate_rows(topics, sep = ";") %>%       # split multiple topics
  mutate(topics = str_trim(topics)) %>%
  filter(!is.na(topics), topics != "")

# Example: Summarise works by topic/year
topic_summary <- all_data_topics %>%
  group_by(topics, year) %>%
  summarise(
    n_total = n(),
    .groups = "drop"
  )

# -------------------------
# 2. Expand authorships
# -------------------------
all_data_authors <- all_data %>%
  separate_rows(authors, sep = ";") %>%       # each author/institution becomes a row
  mutate(authors = str_trim(authors)) %>%
  filter(!is.na(authors), authors != "")

# Example: Count local vs foreign authors
author_summary <- all_data_authors %>%
  mutate(is_latam = str_detect(authors, "\\(AR|BO|BR|CL|CO|CR|CU|DO|EC|GT|HN|MX|NI|PA|PE|PY|SV|UY|VE\\)")) %>%
  group_by(year) %>%
  summarise(
    total_authors = n(),
    latam_authors = sum(is_latam, na.rm = TRUE),
    pct_latam     = round(100 * latam_authors / total_authors, 2),
    .groups = "drop"
  )

# -------------------------
# 3. Experimental flag
# -------------------------
keywords <- c(
  # English
  "field experiment", "survey experiment", "audit study",
  "correspondence experiment","correspondence audit", "conjoint",
  "list experiment", "factorial experiment","factorial survey",
  "vignette experiment", "vignette survey",
  
  # Spanish
  "experimento de campo", "encuesta experimental",
  "estudio de correspondencia", "experimento de correspondencia", "análisis de conjunto",
  "experimento de lista", "experimento factorial", "encuesta factorial",
  "experimento de viñeta","encuesta de viñeta",
  
  # Portuguese
  "experimento de campo", "inquérito experimental", 
  "estudo de correspondência", "experimento de correspondência",
  "análise conjunta", "experimento de lista", 
  "experimento fatorial", "inquérito fatorial",
  "experimento de vinheta","inquérito de vinheta"
)

pattern <- paste(keywords, collapse = "|")

all_data <- all_data %>%
  mutate(
    exp_flag = str_detect(
      str_c(abstract, keywords, sep = " "),   # safe row-wise concatenation
      regex(pattern, ignore_case = TRUE)
    )
  )

# -------------------------
# 4. Summaries by field/year
# -------------------------
field_summary <- all_data_topics %>%
  left_join(all_data %>% select(id, year, exp_flag), by = c("id","year")) %>%
  group_by(topics, year) %>%
  summarise(
    n_total = n(),
    n_exp   = sum(exp_flag, na.rm = TRUE),
    pct_exp = round(100 * n_exp / n_total, 2),
    .groups = "drop"
  )

# -------------------------
# 5. Save tidy versions
# -------------------------
write_csv(all_data_topics, "works_LATAM_topics_long.csv")
write_csv(all_data_authors, "works_LATAM_authors_long.csv")
write_csv(field_summary, "summary_by_field.csv")
write_csv(author_summary, "summary_authorship_latam.csv")

cat("✅ Post-processing complete. Tidy datasets saved.\n")
