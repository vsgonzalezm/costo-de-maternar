library(dplyr)
library(stringr)
library(ggplot2)
library(readr)

# Load the peer-reviewed dataset if not already in memory
df <- read_csv("works_LATAM_2000_2025_peerreviewed_articles_absonly.csv", show_col_types = FALSE)

# Discipline variable (as defined before)
df <- df %>%
  mutate(
    discipline = case_when(
      subfield_name == "Sociology and Political Science" ~ "Sociology",
      subfield_name == "Political Science and International Relations" ~ "Political Science",
      field_name == "Economics, Econometrics and Finance" ~ "Economics",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(discipline))

# Multilingual experimental keyword pattern (already validated)
exp_keywords <- c(
  # English
  "field experiment", "survey experiment", "audit study",
  "correspondence experiment", "correspondence audit",
  "conjoint", "list experiment",
  "factorial experiment", "factorial survey",
  "vignette experiment", "vignette survey",
  
  # Spanish
  "experimento de campo", "encuesta experimental",
  "estudio de correspondencia", "experimento de correspondencia",
  "análisis de conjunto", "experimento de lista",
  "experimento factorial", "encuesta factorial",
  "experimento de viñeta", "encuesta de viñeta",
  
  # Portuguese
  "experimento de campo", "inquérito experimental",
  "estudo de correspondência", "experimento de correspondência",
  "análise conjunta", "experimento de lista",
  "experimento fatorial", "inquérito fatorial",
  "experimento de vinheta", "inquérito de vinheta"
)

exp_pattern <- regex(paste(exp_keywords, collapse = "|"), ignore_case = TRUE)

df <- df %>%
  mutate(
    text_all = paste(title, abstract, keywords, sep = " "),
    exp_flag = str_detect(text_all, exp_pattern)
  )


## Summary table
year_disc_summary <- df %>%
  group_by(year, discipline) %>%
  summarise(
    n_total = n(),
    n_exp   = sum(exp_flag, na.rm = TRUE),
    pct_exp = 100 * n_exp / n_total,
    .groups = "drop"
  )


# FIGURE 1 — Number of experimental papers over time (by discipline)
ggplot(year_disc_summary, aes(x = year, y = n_exp, color = discipline)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Experimental journal articles in Latin America (2000–2025)",
    x = "Year",
    y = "Number of experimental articles",
    color = "Discipline"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )


# FIGURE 2 — Share of experimental papers (by discipline)
ggplot(year_disc_summary, aes(x = year, y = pct_exp, color = discipline)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Share of experimental journal articles in Latin America (2000–2025)",
    x = "Year",
    y = "Percent of articles using experimental methods",
    color = "Discipline"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )


# Separate panels
ggplot(year_disc_summary, aes(x = year, y = pct_exp)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 1.2) +
  facet_wrap(~ discipline, scales = "free_y") +
  labs(
    title = "Share of experimental journal articles in Latin America (2000–2025)",
    x = "Year",
    y = "Percent experimental"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

## Count by discpline
df %>%
  count(year, discipline) %>%
  ggplot(aes(x = year, y = n, color = discipline)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "Total journal articles by discipline",
    y = "Number of articles"
  )

