library(openalexR)
library(dplyr)


unique_sources <- unique(na.omit(all_data$source_id))
length(unique_sources)


options(openalexR.mailto = "tania.hutt@uc.cl")

# Split into chunks of 50 to be safe
split_sources <- split(unique_sources, ceiling(seq_along(unique_sources)/50))

fetch_sources <- function(ids) {
  Sys.sleep(1)  # rate-limit safety
  tryCatch({
    oa_fetch(entity = "sources", id = ids)
  }, error = function(e) {
    message("Error fetching chunk: ", conditionMessage(e))
    tibble()
  })
}

sources_meta <- bind_rows(lapply(split_sources, fetch_sources))


names(sources_meta)
str(sources_meta$topics[1], max.level = 2)

## Unnest topics
library(tidyr)

source_fields <- sources_meta %>%
  select(source_id = id, source_name = display_name, topics) %>%
  unnest(topics) %>%
  filter(type == "field") %>%   # only keep top-level field assignments
  select(source_id, source_name, field = display_name) %>%
  distinct()

# Define the "allowed" fields
fields_keep <- c(
  "Sociology",
  "Political Science",
  "Social Sciences",
  "Decision Sciences",
  "Economics, Econometrics and Finance",
  "Psychology"
)

# Collapse fields per source, and keep only those journals that match
source_fields_filtered <- source_fields %>%
  group_by(source_id) %>%
  summarise(fields_all = list(unique(field)), .groups = "drop") %>%
  rowwise() %>%
  mutate(
    keep = any(fields_all %in% fields_keep),   # TRUE if any field matches
    fields_selected = list(intersect(fields_all, fields_keep))
  ) %>%
  ungroup() %>%
  filter(keep) %>%
  mutate(
    field_final = paste(unlist(fields_selected), collapse = "; ")
  ) %>%
  select(source_id, field_final)

## Journals can have multiple fields associated with, so priritizing the 4 ones of interest, if present. If not, keeping all original ones.
all_data_filtered <- all_data %>%
  left_join(source_fields_filtered, by = "source_id") %>%
  filter(!is.na(field_final))   # keep only works in journals with desired fields

cat("Kept works:", nrow(all_data_filtered), "of", nrow(all_data), "\n")
table(all_data_filtered$field_final)

## Figure by field, as in OA.

# Unnest

field_long <- field_summary %>%
  separate_rows(field_final, sep = "; ")   # split multiple fields into separate rows

field_long <- all_data_filtered %>%
  separate_rows(field_final, sep = "; ") %>%
  filter(field_final %in% c("Economics, Econometrics and Finance",
                            "Sociology", "Political Science",
                            "Social Sciences", "Decision Sciences", "Psychology")) %>%
  group_by(field_final, year) %>%
  summarise(
    n_total = n(),
    n_exp   = sum(exp_flag, na.rm = TRUE),
    pct_exp = round(100 * n_exp / n_total, 2),
    .groups = "drop"
  )

## Fig
ggplot(field_long, aes(x = year, y = n_exp, color = field_short)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Experimental Publications in Latin America by Field (2015–2025)",
    x = "Year",
    y = "Experimental publications (count)",
    color = "Field"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
