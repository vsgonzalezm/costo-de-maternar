# =============================================================================
# Latinobarometro 2024 - Perceived discrimination / intergroup exclusion
# =============================================================================
#
# DATA NOTE (read before using this script):
# The 2024 questionnaire (and the 2023 wave, checked for comparison) does NOT
# contain a direct self-report discrimination item ("have you personally
# experienced discrimination?") nor a direct "which groups are discriminated
# against in this country?" item. Both were searched for across the full 2024
# questionnaire, the 2024 data dictionary, and the 2023 questionnaire/codebook.
#
# The closest available measure in the 2024 wave is Q3NOIJ / P3NOIJ:
#   "This list includes various groups of people. Could you indicate those
#    you wouldn't want as neighbors?" (check all that apply)
# This is a social-distance / exclusionary-attitudes battery (the same family
# of item used in the World Values Survey) and is used here as the closest
# available proxy for perceived group-based rejection/discrimination.
#
# S7 (respondent's own ethnicity/race) is included as a covariate: it is not
# a discrimination question itself, but identifies who in the sample belongs
# to a group that could plausibly be a target of the attitudes captured by
# P3NOIJ.
# =============================================================================

library(readxl)
library(dplyr)
library(tidyr)

# ---- 1. Load data -----------------------------------------------------------

data_dir  <- "/Users/taniahutt/Library/CloudStorage/GoogleDrive-tcabellohutt@gmail.com/My Drive/Costo de Maternar/Avanza UC 2026/Latinobarometro"
file_path <- file.path(data_dir, "Latinobarometro_2024_Excel_eng_v20250817.xlsx")

raw <- read_excel(file_path, sheet = "Data")

# Column headers arrive as "CODE: long description". Keep the short code as
# the working variable name and store the full description in a lookup table
# (used below to describe the key variables).
full_names <- names(raw)
var_codes  <- sub("^([^:]+):.*$", "\\1", full_names)
var_labels <- trimws(sub("^[^:]+:\\s*", "", full_names))
names(raw) <- var_codes

var_dictionary <- tibble(variable = var_codes, description = var_labels)

# ---- 2. Country and region lookup -------------------------------------------

# IDENPA uses ISO-3166-1 numeric country codes. These are the 17 countries
# present in the 2024 wave (Nicaragua and Spain are not included in 2024).
country_lookup <- tibble(
  IDENPA = c(32, 68, 76, 152, 170, 188, 214, 218, 222, 320, 340, 484, 591, 600, 604, 858, 862),
  country = c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica",
              "Dominican Republic", "Ecuador", "El Salvador", "Guatemala", "Honduras",
              "Mexico", "Panama", "Paraguay", "Peru", "Uruguay", "Venezuela")
)

# Latinobarometro does not provide a Latin-American sub-region variable, so we
# define one here. Edit this mapping if a different grouping is preferred.
country_lookup <- country_lookup %>%
  mutate(region = case_when(
    country %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
                   "Ecuador", "Paraguay", "Peru", "Uruguay", "Venezuela") ~ "South America",
    country %in% c("Mexico", "Costa Rica", "El Salvador", "Guatemala",
                   "Honduras", "Panama") ~ "Mexico & Central America",
    country == "Dominican Republic" ~ "Caribbean"
  ))

# ---- 3. Key variables ---------------------------------------------------------

# (a) P3NOIJ - "Groups you would NOT want as neighbors"
#     Multi-response battery: each item is 0 = not mentioned, 1 = mentioned.
neighbor_vars <- c(
  P3NOIJ.1 = "Different race or ethnicity",
  P3NOIJ.2 = "Immigrants / foreign workers",
  P3NOIJ.3 = "Homosexuals",
  P3NOIJ.4 = "Different religion",
  P3NOIJ.5 = "Drinkers / drug addicts / alcoholics",
  P3NOIJ.6 = "Unmarried cohabiting couples",
  P3NOIJ.7 = "People who speak a different language",
  P3NOIJ.8 = "Youth"
)
# P3NOIJ.9 is a DK/NA flag for the whole battery - reported separately, not
# treated as a substantive group.

# (b) S7 - respondent's own ethnicity/race (context covariate)
ethnicity_labels <- c(`1` = "Asian", `2` = "Black", `3` = "Indigenous",
                       `4` = "Mestizo", `5` = "Mulatto", `6` = "White",
                       `7` = "Other race")

# ---- 4. Describe the key variables -------------------------------------------

cat("\n================ KEY VARIABLES: DESCRIPTION ================\n")

cat("\n--- P3NOIJ: 'Groups you would not want as neighbors' (check all that apply) ---\n")
print(var_dictionary %>% filter(variable %in% c(names(neighbor_vars), "P3NOIJ.9")))

cat("\nUnweighted frequency of each item (0 = not mentioned, 1 = mentioned):\n")
for (v in names(neighbor_vars)) {
  cat("\n", v, "-", neighbor_vars[[v]], "\n")
  print(table(raw[[v]], useNA = "ifany"))
}

cat("\n--- S7: Respondent's own ethnicity/race (covariate, not a discrimination item) ---\n")
print(var_dictionary %>% filter(variable == "S7"))
cat("\nUnweighted frequency (raw codes; negative codes = missing/DNK/NA):\n")
print(table(raw$S7, useNA = "ifany"))

# ---- 5. Build analysis dataset -------------------------------------------------

analysis <- raw %>%
  select(IDENPA, WT, all_of(names(neighbor_vars)), S7) %>%
  left_join(country_lookup, by = "IDENPA") %>%
  mutate(S7_label = recode(as.character(S7), !!!ethnicity_labels, .default = NA_character_))

# ---- 6. Country tables: % who would NOT want each group as neighbor -----------
# WT is pre-normalized within each country (mean = 1), so a straight weighted
# mean per country gives the standard Latinobarometro national estimate.

neighbor_long <- analysis %>%
  select(country, region, WT, all_of(names(neighbor_vars))) %>%
  pivot_longer(cols = all_of(names(neighbor_vars)), names_to = "item", values_to = "value") %>%
  mutate(item_label = recode(item, !!!neighbor_vars))

country_table_neighbors <- neighbor_long %>%
  group_by(country, item_label) %>%
  summarise(pct = round(weighted.mean(value, WT, na.rm = TRUE) * 100, 1), .groups = "drop") %>%
  pivot_wider(names_from = item_label, values_from = pct) %>%
  arrange(country)

cat("\n================ TABLE 1: % who would NOT want each group as a neighbor, by country ================\n")
print(country_table_neighbors)

# ---- 7. Region averages: mean of country-level percentages --------------------
# Each country counts equally in the regional average (standard Latinobarometro
# convention), regardless of its sample size.

region_table_neighbors <- country_table_neighbors %>%
  left_join(country_lookup %>% select(country, region), by = "country") %>%
  pivot_longer(cols = -c(country, region), names_to = "item_label", values_to = "pct") %>%
  group_by(region, item_label) %>%
  summarise(pct = round(mean(pct, na.rm = TRUE), 1), .groups = "drop") %>%
  pivot_wider(names_from = item_label, values_from = pct)

cat("\n================ TABLE 2: % who would NOT want each group as a neighbor, by region ================\n")
print(region_table_neighbors)

# ---- 8. S7 ethnicity distribution by country and region (context table) -------

country_table_ethnicity <- analysis %>%
  filter(!is.na(S7_label)) %>%
  group_by(country) %>%
  mutate(country_wt = sum(WT)) %>%
  group_by(country, S7_label) %>%
  summarise(pct = round(sum(WT) / first(country_wt) * 100, 1), .groups = "drop") %>%
  pivot_wider(names_from = S7_label, values_from = pct, values_fill = 0) %>%
  arrange(country)

cat("\n================ TABLE 3: Self-identified ethnicity/race (%), by country ================\n")
print(country_table_ethnicity)

region_table_ethnicity <- country_table_ethnicity %>%
  left_join(country_lookup %>% select(country, region), by = "country") %>%
  pivot_longer(cols = -c(country, region), names_to = "S7_label", values_to = "pct") %>%
  group_by(region, S7_label) %>%
  summarise(pct = round(mean(pct, na.rm = TRUE), 1), .groups = "drop") %>%
  pivot_wider(names_from = S7_label, values_from = pct)

cat("\n================ TABLE 4: Self-identified ethnicity/race (%), by region ================\n")
print(region_table_ethnicity)

# ---- 9. Save outputs ------------------------------------------------------------

write.csv(country_table_neighbors, file.path(data_dir, "table1_neighbors_by_country.csv"), row.names = FALSE)
write.csv(region_table_neighbors,  file.path(data_dir, "table2_neighbors_by_region.csv"),  row.names = FALSE)
write.csv(country_table_ethnicity, file.path(data_dir, "table3_ethnicity_by_country.csv"), row.names = FALSE)
write.csv(region_table_ethnicity,  file.path(data_dir, "table4_ethnicity_by_region.csv"),  row.names = FALSE)

cat("\nDone. CSV files written to:\n", data_dir, "\n")
