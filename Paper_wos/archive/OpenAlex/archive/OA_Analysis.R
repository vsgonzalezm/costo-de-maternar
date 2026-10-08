
library(tidyr)
library(dplyr)



## Load data
all_data <- read_csv("works_LATAM_2000_2025.csv")

## Unnest topics
all_data_long <- all_data %>%
  separate_rows(topics, sep = "; ") %>%
  filter(!is.na(topics))

## Topic summary
topic_summary <- all_data_long %>%
  group_by(topics, year) %>%
  summarise(
    n_total = n(),
    n_exp   = sum(exp_flag, na.rm = TRUE),
    pct_exp = round(100 * n_exp / n_total, 2),
    .groups = "drop"
  )

ggplot(topic_summary %>% filter(topics %in% c("Economics", "Political Science", "Sociology")),
       aes(x = year, y = n_exp, color = topics)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Experimental Publications in LATAM by Topic",
    x = "Year", y = "Experimental publications (count)", color = "Topic"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")