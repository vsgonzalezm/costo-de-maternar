# =============================================================================
# Latinobarometro 2020 - Perceived discrimination (original analysis)
# =============================================================================
#
# The 2020 wave contains the two direct discrimination items that are absent
# from the 2023/2024 waves (checked separately - see discriminacion_analysis_
# 2024.R for that data note):
#
#   p57st - "Would you describe yourself as part of a discriminated group in
#            (country) or not?" (Yes / No)
#            -> SELF-REPORT of experiencing discrimination.
#
#   p58st - "As far as you know or have heard, which do you think are the
#            most discriminated people or groups of people in (country), or
#            do you think there are no groups or people discriminated in
#            (country)?" (open-ended, coded into 40 categories + None/Other)
#            -> PERCEPTION of which groups are discriminated against.
#
# Source: Latinobarometro_2020_Eng_Rdata_v1_0.rdata (data.frame
# "Latinobarometro_2020_Eng"), variable codes and value labels taken from
# F00011662-Latinobarometro_2020_Codebook.pdf.
# =============================================================================

library(dplyr)
library(tidyr)

# ---- 1. Load data -----------------------------------------------------------

data_dir  <- "/Users/taniahutt/Library/CloudStorage/GoogleDrive-tcabellohutt@gmail.com/My Drive/Costo de Maternar/Avanza UC 2026/Latinobarometro"
load(file.path(data_dir, "Latinobarometro_2020_Eng_Rdata_v1_0.rdata"))
raw <- Latinobarometro_2020_Eng

# ---- 2. Country and region lookup -------------------------------------------

# idenpa uses ISO-3166-1 numeric country codes. 18 countries in 2020
# (Nicaragua included, unlike 2024; Spain not included).
country_lookup <- tibble(
  idenpa = c(32, 68, 76, 152, 170, 188, 214, 218, 222, 320, 340, 484, 558, 591, 600, 604, 858, 862),
  country = c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica",
              "Dominican Republic", "Ecuador", "El Salvador", "Guatemala", "Honduras",
              "Mexico", "Nicaragua", "Panama", "Paraguay", "Peru", "Uruguay", "Venezuela")
)

# Latinobarometro does not provide a Latin-American sub-region variable, so we
# define one here (same grouping used for the 2024 analysis, extended with
# Nicaragua). Edit this mapping if a different grouping is preferred.
country_lookup <- country_lookup %>%
  mutate(region = case_when(
    country %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
                   "Ecuador", "Paraguay", "Peru", "Uruguay", "Venezuela") ~ "South America",
    country %in% c("Mexico", "Costa Rica", "El Salvador", "Guatemala",
                   "Honduras", "Panama", "Nicaragua") ~ "Mexico & Central America",
    country == "Dominican Republic" ~ "Caribbean"
  ))

# ---- 3. Key variables: value labels -----------------------------------------

# (a) p57st - self-report of being part of a discriminated group
p57_labels <- c(`1` = "Yes", `2` = "No")  # negative codes = DK/NA/not asked -> treated as missing

# (b) p58st - perceived most discriminated group (open-ended, coded)
p58_labels <- c(
  `1` = "Black people / African descent", `2` = "Indigenous and native people",
  `3` = "White people", `4` = "Mulattos", `5` = "Mestizos", `6` = "Asian people",
  `7` = "Arab people", `8` = "Muslims", `9` = "Jews", `10` = "Poor people",
  `11` = "Immigrants (in general)", `12` = "Immigrants from Latin America",
  `13` = "Immigrants from Venezuela", `14` = "Immigrants from Colombia",
  `15` = "Immigrants from Bolivia", `16` = "Immigrants from Peru",
  `17` = "Immigrants from El Salvador, Guatemala and Honduras", `18` = "Handicapped",
  `19` = "AIDS patients", `20` = "Homosexuals", `21` = "Drug addicts",
  `22` = "People with tattoos", `23` = "Women", `24` = "Old people",
  `25` = "Young people", `26` = "Illiterates",
  `27` = "Religious people (evangelicals, mormons, pentecostals)",
  `28` = "Members of a political party (communists, socialists)",
  `29` = "Union members", `30` = "Feminists",
  `31` = "People discriminated by minority ideologies", `32` = "Single mothers",
  `33` = "Homeless people", `34` = "Peasants (field workers)",
  `35` = "People who live in rural areas", `36` = "Criminals / gang members",
  `37` = "Members of armed groups", `38` = "Alcoholics",
  `39` = "People discriminated by race", `40` = "Doctors or health workers",
  `96` = "Other", `97` = "None (no one is discriminated)"
)

# ---- 4. Describe the key variables -------------------------------------------

cat("\n================ KEY VARIABLES: DESCRIPTION ================\n")

cat("\n--- p57st: 'Part of a discriminated group in (country)?' (Yes/No) ---\n")
cat("Unweighted frequency (raw codes; negative codes = DK/NA/not asked):\n")
print(table(raw$p57st, useNA = "ifany"))

cat("\n--- p58st: 'Most discriminated people or group in (country)' (open-ended, coded) ---\n")
cat("Unweighted frequency, top 15 categories (raw codes; -1/-2 = DK/NA):\n")
print(head(sort(table(raw$p58st, useNA = "ifany"), decreasing = TRUE), 15))

# ---- 5. Build analysis dataset -------------------------------------------------

analysis <- raw %>%
  select(idenpa, wt, p57st, p58st) %>%
  left_join(country_lookup, by = "idenpa") %>%
  mutate(
    p57_label = recode(as.character(p57st), !!!p57_labels, .default = NA_character_),
    p58_label = recode(as.character(p58st), !!!p58_labels, .default = NA_character_)
  )

# ---- 6. Table 1: self-reported discrimination (p57st), by country -------------
# wt is pre-normalized within each country (mean ~ 1), so a weighted mean of
# the Yes/No indicator gives the standard Latinobarometro national estimate.
# Missing (DK/NA) responses are excluded from the percentage base.

country_table_p57 <- analysis %>%
  filter(!is.na(p57_label)) %>%
  mutate(is_yes = as.numeric(p57_label == "Yes")) %>%
  group_by(country) %>%
  summarise(
    pct_yes = round(weighted.mean(is_yes, wt, na.rm = TRUE) * 100, 1),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(country)

cat("\n================ TABLE 1: % who consider themselves part of a discriminated group, by country ================\n")
print(country_table_p57)

# ---- 7. Table 2: self-reported discrimination (p57st), by region --------------
# Region average = mean of country-level percentages (each country counts
# equally), the standard Latinobarometro convention.

region_table_p57 <- country_table_p57 %>%
  left_join(country_lookup %>% select(country, region), by = "country") %>%
  group_by(region) %>%
  summarise(pct_yes = round(mean(pct_yes, na.rm = TRUE), 1), .groups = "drop")

cat("\n================ TABLE 2: % who consider themselves part of a discriminated group, by region ================\n")
print(region_table_p57)

# ---- 8. Table 3: perceived most-discriminated group (p58st), by country -------
# % of valid (non-missing) responses naming each group, per country.

country_table_p58 <- analysis %>%
  filter(!is.na(p58_label)) %>%
  group_by(country) %>%
  mutate(country_n = n()) %>%
  group_by(country, p58_label) %>%
  summarise(pct = round(n() / first(country_n) * 100, 1), .groups = "drop") %>%
  pivot_wider(names_from = p58_label, values_from = pct, values_fill = 0) %>%
  arrange(country)

cat("\n================ TABLE 3: Perceived most-discriminated group (%% of valid responses), by country ================\n")
print(country_table_p58)

# ---- 9. Table 4: perceived most-discriminated group (p58st), by region --------

region_table_p58 <- country_table_p58 %>%
  left_join(country_lookup %>% select(country, region), by = "country") %>%
  pivot_longer(cols = -c(country, region), names_to = "p58_label", values_to = "pct") %>%
  group_by(region, p58_label) %>%
  summarise(pct = round(mean(pct, na.rm = TRUE), 1), .groups = "drop") %>%
  pivot_wider(names_from = p58_label, values_from = pct)

cat("\n================ TABLE 4: Perceived most-discriminated group (%% of valid responses), by region ================\n")
print(region_table_p58)

# ---- 10. Quick leaderboard: top-5 perceived discriminated groups per country --
# (Easier to read than the full wide table above.)

top5_by_country <- analysis %>%
  filter(!is.na(p58_label)) %>%
  group_by(country) %>%
  mutate(country_n = n()) %>%
  group_by(country, p58_label) %>%
  summarise(pct = round(n() / first(country_n) * 100, 1), .groups = "drop") %>%
  group_by(country) %>%
  slice_max(pct, n = 5) %>%
  arrange(country, desc(pct))

cat("\n================ TABLE 5: Top 5 perceived most-discriminated groups per country ================\n")
print(top5_by_country, n = Inf)

# ---- 11. Save outputs ------------------------------------------------------------

write.csv(country_table_p57, file.path(data_dir, "2020_table1_selfreport_by_country.csv"), row.names = FALSE)
write.csv(region_table_p57,  file.path(data_dir, "2020_table2_selfreport_by_region.csv"),  row.names = FALSE)
write.csv(country_table_p58, file.path(data_dir, "2020_table3_perceived_group_by_country.csv"), row.names = FALSE)
write.csv(region_table_p58,  file.path(data_dir, "2020_table4_perceived_group_by_region.csv"),  row.names = FALSE)
write.csv(top5_by_country,   file.path(data_dir, "2020_table5_top5_perceived_group_by_country.csv"), row.names = FALSE)

cat("\nDone. CSV files written to:\n", data_dir, "\n")
