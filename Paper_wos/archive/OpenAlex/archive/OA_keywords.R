library(openalexR)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)

setwd("~/Google Drive/My Drive/Paper_Metodos_Exp/OpenAlex/")

## Load data
all_data <- read_csv("works_LATAM_2000_2025.csv")



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


# Make a regex pattern (case-insensitive)
pattern <- paste(keywords, collapse = "|")

all_data <- all_data %>%
  mutate(
    exp_flag = str_detect(
      paste(abstract, keywords, sep = " "),    # combine abstract + keywords cols
      regex(pattern, ignore_case = TRUE)
    )
  )


# Quick global descriptives
global_summary <- all_data %>%
  group_by(year) %>%
  summarise(
    n_total = n(),
    n_exp   = sum(exp_flag, na.rm = TRUE),
    pct_exp = round(100 * n_exp / n_total, 2),
    .groups = "drop"
  )

print(global_summary)

overall_summary <- all_data %>%
  summarise(
    n_total = n(),
    n_exp   = sum(exp_flag, na.rm = TRUE),
    pct_exp = round(100 * n_exp / n_total, 2)
  )

print(overall_summary)


ggplot(global_summary, aes(x = year)) +
  geom_line(aes(y = n_exp), linewidth = 1, color = "steelblue") +
  geom_point(aes(y = n_exp), color = "steelblue") +
  labs(
    title = "Experimental Publications in Latin America (2015–2025)",
    x = "Year",
    y = "Number of experimental publications"
  ) +
  theme_minimal()