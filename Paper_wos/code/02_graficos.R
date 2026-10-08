#-----------------------------------------------
# 02 - Graficos principales
# Valentina Gonzalez Madariaga
# Agosto 2026
#-----------------------------------------------
library(tidyverse)
library(here)
library(scales)

# Cargar bases y resultados creados en los scripts anteriores
conteos_wos_year <- readRDS(here("Paper_wos", "data", "intermediate", "conteos_wos_year.rds"))
tasas_wos_year <- readRDS(here("Paper_wos", "data", "intermediate", "tasas_wos_year.rds"))
corpus_unico <- readRDS(here("Paper_wos", "data", "intermediate", "corpus_unico_inicial.rds"))
corpus_unico_core_area_long <- readRDS(here("Paper_wos", "data", "intermediate", "corpus_unico_core_area_long.rds"))

# Definir paleta fija para todos los graficos
paleta_wes <- c(
  "Latin America" = "#D55E00",
  "Latin America (endogenous)" = "#D55E00",
  "Chile (endogenous)" = "#1F6F68",
  "USA" = "#0072B2",
  "Endogenous" = "#7B9E89",
  "Laboratory" = "#8E7DBE",
  "Total corpus" = "#D8A657",
  "Endogenous only" = "#7B9E89",
  "Laboratory only" = "#8E7DBE",
  "Both" = "#D8A657",
  "Economics" = "#CC79A7",
  "Political Science" = "#009E73",
  "Sociology" = "#E69F00",
  "Psychology" = "#56B4E9",
  "Business/Management" = "#F0E442",
  "Other" = "#999999"
)

# Marcar 2026 como anio parcial en los ejes.
year_breaks <- c(1990, 2000, 2010, 2020, 2026)
year_labels <- c("1990", "2000", "2010", "2020", "2026*")

# A. Comparar produccion experimental absoluta de USA y America Latina
usa_latam_year <- conteos_wos_year |>
  filter(type %in% c("latam_endogenous_experimental_output", "usa_experimental_output")) |>
  mutate(region = if_else(country == "Latin America", "Latin America", "USA")) |>
  select(region, year, n) |>
  complete(region, year = 1990:2026, fill = list(n = 0)) |>
  arrange(region, year)

usa_latam_table <- usa_latam_year |>
  pivot_wider(names_from = region, values_from = n, values_fill = 0) |>
  mutate(
    total_combined = `Latin America` + USA,
    latin_america_share = if_else(total_combined > 0, `Latin America` / total_combined, NA_real_),
    usa_share = if_else(total_combined > 0, USA / total_combined, NA_real_),
    usa_to_latin_america_ratio = if_else(`Latin America` > 0, USA / `Latin America`, NA_real_)
  ) |>
  arrange(year)

write_csv(usa_latam_table, here("Paper_wos", "outputs", "tables", "usa_latam_year.csv"))

usa_latam_plot <- ggplot(usa_latam_year, aes(x = year, y = n, color = region)) +
  geom_line(data = ~ filter(.x, year <= 2025), linewidth = 1.1) +
  geom_line(data = ~ filter(.x, year >= 2025), linewidth = 1.1, linetype = "dotted") +
  geom_point(size = 1.6) +
  facet_wrap(~region, scales = "free_y", ncol = 1) +
  scale_color_manual(values = paleta_wes) +
  scale_x_continuous(breaks = year_breaks, labels = year_labels) +
  scale_y_continuous(labels = comma) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = "Publication year",
    y = "Number of experimental papers",
    color = NULL
  ) +
  theme_minimal(base_size = 26) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 22),
    panel.grid.minor = element_blank(),
    plot.margin = margin(14, 24, 20, 48)
  )

ggsave(here("Paper_wos", "outputs", "figures", "usa_latam_year.png"), usa_latam_plot, width = 12.6, height = 6.8, dpi = 320)

# B. Comparar produccion experimental proporcional de USA y America Latina
usa_latam_rate_year <- tasas_wos_year$usa_latam |>
  mutate(region = if_else(country == "Latin America", "Latin America", "USA"))

usa_latam_rate_snapshot <- usa_latam_rate_year |>
  filter(year %in% c(2025, 2026)) |>
  mutate(year = if_else(year == 2026, "2026*", as.character(year))) |>
  select(year, region, total_n, experimental_n, rate_per_1000) |>
  arrange(year, region)

write_csv(usa_latam_rate_snapshot, here("Paper_wos", "outputs", "tables", "usa_latam_rate_snapshot.csv"))

usa_latam_rate_plot <- ggplot(usa_latam_rate_year, aes(x = year, y = rate_per_1000, color = region)) +
  geom_line(data = ~ filter(.x, year <= 2025), linewidth = 1.1) +
  geom_line(data = ~ filter(.x, year >= 2025), linewidth = 1.1, linetype = "dotted") +
  geom_point(size = 1.6) +
  scale_color_manual(values = paleta_wes) +
  scale_x_continuous(breaks = year_breaks, labels = year_labels) +
  scale_y_continuous(labels = number_format(accuracy = 1)) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = "Publication year",
    y = "Experimental papers\nper 1,000 records",
    color = NULL
  ) +
  theme_minimal(base_size = 25) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.margin = margin(18, 34, 24, 62)
  )

ggsave(here("Paper_wos", "outputs", "figures", "usa_latam_rate_year.png"), usa_latam_rate_plot, width = 12.6, height = 6.8, dpi = 320)
# B2. Comparar tasas anuales de produccion endogena de America Latina y Chile con USA
# La tasa de Chile usa la suma de denominadores por disciplina y puede contener solapamientos entre categorias WoS.
rate_latam_chile_usa_year <- read_csv(
  here("Paper_wos", "outputs", "tables", "rate_latam_chile_usa_year.csv"),
  show_col_types = FALSE
)
rate_latam_chile_usa_plot <- ggplot(
  rate_latam_chile_usa_year,
  aes(x = year, y = rate_per_1000, color = series)
) +
  geom_line(data = ~ filter(.x, year <= 2025), linewidth = 1.1) +
  geom_line(data = ~ filter(.x, year >= 2025), linewidth = 1.1, linetype = "dotted") +
  geom_point(size = 1.6) +
  scale_color_manual(values = paleta_wes) +
  scale_x_continuous(breaks = year_breaks, labels = year_labels) +
  scale_y_continuous(labels = number_format(accuracy = 0.1)) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = "Publication year",
    y = "Experimental papers\nper 1,000 records",
    color = NULL
  ) +
  theme_minimal(base_size = 25) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.margin = margin(18, 34, 24, 62)
  )

ggsave(
  here("Paper_wos", "outputs", "figures", "rate_latam_chile_usa_year.png"),
  rate_latam_chile_usa_plot,
  width = 12.6,
  height = 6.8,
  dpi = 320
)

# C. Comparar tendencias de los universos de busqueda
universes_year <- bind_rows(
  corpus_unico |>
    filter(base_endogeno == 1) |>
    count(universe = "Endogenous", year = as.integer(PY), name = "n"),
  corpus_unico |>
    filter(base_laboratorio == 1) |>
    count(universe = "Laboratory", year = as.integer(PY), name = "n"),
  corpus_unico |>
    count(universe = "Total corpus", year = as.integer(PY), name = "n")
) |>
  complete(universe, year = 1990:2026, fill = list(n = 0)) |>
  arrange(universe, year)

write_csv(universes_year, here("Paper_wos", "outputs", "tables", "universes_year.csv"))

universes_plot <- ggplot(universes_year, aes(x = year, y = n, color = universe)) +
  geom_line(data = ~ filter(.x, year <= 2025), linewidth = 1.1) +
  geom_line(data = ~ filter(.x, year >= 2025), linewidth = 1.1, linetype = "dotted") +
  geom_point(size = 1.6) +
  scale_color_manual(values = paleta_wes) +
  scale_x_continuous(breaks = year_breaks, labels = year_labels) +
  scale_y_continuous(labels = comma) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = "Publication year",
    y = "Number of papers",
    color = NULL
  ) +
  theme_minimal(base_size = 25) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.margin = margin(18, 34, 24, 62)
  )

ggsave(here("Paper_wos", "outputs", "figures", "universes_year.png"), universes_plot, width = 12, height = 6.4, dpi = 320)

# D. Mostrar composicion del corpus sin superposicion
status_year <- corpus_unico |>
  mutate(status = case_when(
    base_endogeno == 1 & base_laboratorio == 1 ~ "Both",
    base_endogeno == 1 & base_laboratorio == 0 ~ "Endogenous only",
    base_endogeno == 0 & base_laboratorio == 1 ~ "Laboratory only"
  )) |>
  count(status, year = as.integer(PY), name = "n") |>
  complete(status, year = 1990:2026, fill = list(n = 0)) |>
  arrange(status, year)

write_csv(status_year, here("Paper_wos", "outputs", "tables", "status_year.csv"))

status_plot <- ggplot(status_year, aes(x = year, y = n, color = status)) +
  geom_line(data = ~ filter(.x, year <= 2025), linewidth = 1.1) +
  geom_line(data = ~ filter(.x, year >= 2025), linewidth = 1.1, linetype = "dotted") +
  geom_point(size = 1.6) +
  scale_color_manual(values = paleta_wes) +
  scale_x_continuous(breaks = year_breaks, labels = year_labels) +
  scale_y_continuous(labels = comma) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = "Publication year",
    y = "Number of papers",
    color = NULL
  ) +
  theme_minimal(base_size = 25) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.margin = margin(18, 34, 24, 62)
  )

ggsave(here("Paper_wos", "outputs", "figures", "status_year.png"), status_plot, width = 12, height = 6.4, dpi = 320)

# E. Comparar disciplinas segun presencia exclusiva o compartida en los dos universos.
# Un paper puede contar en mas de una core discipline si WoS lo clasifica en varias areas.
discipline_universe <- corpus_unico_core_area_long |>
  mutate(universe = case_when(
    base_endogeno == 1 & base_laboratorio == 1 ~ "Both",
    base_endogeno == 1 & base_laboratorio == 0 ~ "Endogenous only",
    base_endogeno == 0 & base_laboratorio == 1 ~ "Laboratory only"
  )) |>
  mutate(universe = factor(universe, levels = c("Laboratory only", "Endogenous only", "Both"))) |>
  count(universe, core_area, name = "n") |>
  group_by(core_area) |>
  mutate(total_core = sum(n)) |>
  ungroup() |>
  arrange(desc(total_core), universe)

write_csv(discipline_universe, here("Paper_wos", "outputs", "tables", "discipline_universe.csv"))

discipline_plot <- ggplot(discipline_universe, aes(x = reorder(core_area, total_core), y = n, fill = universe)) +
  geom_col(position = position_dodge(width = 0.78), width = 0.68) +
  coord_flip() +
  scale_fill_manual(values = paleta_wes, breaks = c("Laboratory only", "Endogenous only", "Both")) +
  scale_y_continuous(labels = comma) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = "Discipline",
    y = "Number of papers",
    fill = NULL
  ) +
  theme_minimal(base_size = 26) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top",
    axis.title.y = element_text(size = 24, margin = margin(r = 12)),
    axis.text = element_text(size = 21),
    panel.grid.minor = element_blank(),
    plot.margin = margin(12, 24, 18, 42)
  )

ggsave(here("Paper_wos", "outputs", "figures", "discipline_universe.png"), discipline_plot, width = 12.6, height = 6.8, dpi = 320)

# F. Contar produccion endogena por pais de afiliacion
# Si un paper tiene afiliaciones en mas de un pais LatAm, cuenta una vez para cada pais.
countries_year <- corpus_unico |>
  filter(base_endogeno == 1) |>
  select(UT, PY, afiliacion_latam) |>
  separate_rows(afiliacion_latam, sep = ";") |>
  mutate(country = str_squish(afiliacion_latam)) |>
  filter(!is.na(country), country != "") |>
  count(country, year = as.integer(PY), name = "n") |>
  complete(country, year = 1990:2026, fill = list(n = 0)) |>
  arrange(country, year)

countries_total <- countries_year |>
  group_by(country) |>
  summarise(total = sum(n), .groups = "drop") |>
  arrange(desc(total))

countries_show <- countries_total |>
  filter(total >= 10) |>
  pull(country)

country_top <- countries_total |>
  filter(country %in% countries_show) |>
  slice_max(total, n = 1) |>
  pull(country)

country_low <- countries_total |>
  filter(country %in% countries_show) |>
  slice_min(total, n = 1) |>
  pull(country)

countries_year_plot_data <- countries_year |>
  filter(country %in% countries_show)

countries_labels <- countries_year_plot_data |>
  filter(country %in% c(country_top, country_low, "Chile"), year == max(year)) |>
  mutate(label = country)

write_csv(countries_year, here("Paper_wos", "outputs", "tables", "countries_year.csv"))
write_csv(countries_total, here("Paper_wos", "outputs", "tables", "countries_total.csv"))

countries_plot <- ggplot(countries_year_plot_data, aes(x = year, y = n, color = country)) +
  geom_line(data = ~ filter(.x, year <= 2025), linewidth = 1) +
  geom_line(data = ~ filter(.x, year >= 2025), linewidth = 1, linetype = "dotted") +
  geom_point(size = 1.4) +
  geom_text(
    data = countries_labels,
    aes(label = label),
    hjust = -0.05,
    size = 3.5,
    show.legend = FALSE
  ) +
  scale_x_continuous(breaks = year_breaks, labels = year_labels, limits = c(1990, 2029)) +
  scale_y_continuous(labels = comma) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = "Publication year",
    y = "Number of papers",
    color = NULL
  ) +
  theme_minimal(base_size = 25) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.margin = margin(18, 34, 24, 62)
  )

ggsave(here("Paper_wos", "outputs", "figures", "countries_year.png"), countries_plot, width = 12, height = 6.6, dpi = 320)

# G. Graficar tasas de produccion experimental por pais latinoamericano
latam_rate_country_year <- tasas_wos_year$latam_country

countries_selected <- c("Brazil", "Chile", "Colombia", "Mexico", "Argentina", "Peru")

latam_rate_country_plot_data <- latam_rate_country_year |>
  filter(country %in% countries_selected, year >= 1990, year <= 2026) |>
  group_by(country) |>
  arrange(year, .by_group = TRUE) |>
  mutate(rate_per_1000_3yr = (coalesce(lag(rate_per_1000), rate_per_1000) + rate_per_1000 + coalesce(lead(rate_per_1000), rate_per_1000)) / 3) |>
  ungroup()

# Label Chile, Argentina, and Colombia at the final year
country_rate_labels <- latam_rate_country_plot_data |>
  filter(country %in% c("Chile", "Argentina", "Colombia"), year == max(year)) |>
  mutate(label = country)

latam_rate_country_plot <- ggplot(latam_rate_country_plot_data, aes(x = year, y = rate_per_1000_3yr, color = country)) +
  geom_line(data = ~ filter(.x, year <= 2025), linewidth = 1.1) +
  geom_line(data = ~ filter(.x, year >= 2025), linewidth = 1.1, linetype = "dotted") +
  geom_point(size = 1.6) +
  geom_text(
    data = country_rate_labels,
    aes(label = label),
    hjust = -0.08,
    size = 5,
    show.legend = FALSE
  ) +
  scale_color_manual(values = c(
    "Brazil" = "#1F6F68",
    "Chile" = "#D55E00",
    "Colombia" = "#8E7DBE",
    "Mexico" = "#D8A657",
    "Argentina" = "#7B9E89",
    "Peru" = "#56B4E9"
  )) +
  scale_x_continuous(breaks = year_breaks, labels = year_labels, limits = c(1990, 2033)) +
  scale_y_continuous(labels = number_format(accuracy = 0.1)) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = "Publication year",
    y = "Experimental papers\nper 1,000 records",
    color = NULL
  ) +
  theme_minimal(base_size = 25) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.margin = margin(18, 34, 24, 62)
  )

ggsave(here("Paper_wos", "outputs", "figures", "rate_latam_country_year.png"), latam_rate_country_plot, width = 12.6, height = 6.8, dpi = 320)

# H. Mostrar la tasa USA por separado
usa_rate_year <- usa_latam_rate_year |>
  filter(country == "USA")

usa_rate_plot <- ggplot(usa_rate_year, aes(x = year, y = rate_per_1000, color = country)) +
  geom_line(data = ~ filter(.x, year <= 2025), linewidth = 1.1) +
  geom_line(data = ~ filter(.x, year >= 2025), linewidth = 1.1, linetype = "dotted") +
  geom_point(size = 1.6) +
  scale_color_manual(values = paleta_wes) +
  scale_x_continuous(breaks = year_breaks, labels = year_labels) +
  scale_y_continuous(labels = number_format(accuracy = 0.1)) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = "Publication year",
    y = "Experimental papers\nper 1,000 records",
    color = NULL
  ) +
  theme_minimal(base_size = 25) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.margin = margin(18, 34, 24, 62)
  )

ggsave(here("Paper_wos", "outputs", "figures", "rate_usa_year.png"), usa_rate_plot, width = 12, height = 6.6, dpi = 320)

# I. Agrupar disenos experimentales en familias metodologicas
# Survey-based = Survey + Vignette + Factorial + List + Online
# Conjoint / choice = Conjoint + Choice
# Field / lab = Field experiment
# Audit / correspondence = Audit + Correspondence + Resume/CV audit
method_family_map <- tribble(
  ~des_exp, ~method_family,
  "Survey experiment", "Survey-based experiments",
  "Vignette experiment/survey", "Survey-based experiments",
  "Factorial experiment/survey", "Survey-based experiments",
  "List experiment", "Survey-based experiments",
  "Online experiment", "Survey-based experiments",
  "Conjoint", "Conjoint / choice designs",
  "Choice experiment", "Conjoint / choice designs",
  "Field experiment", "Field / lab interventions",
  "Audit study", "Audit / correspondence designs",
  "Correspondence experiment/audit", "Audit / correspondence designs",
  "Resume/CV audit", "Audit / correspondence designs"
)

write_csv(method_family_map, here("Paper_wos", "outputs", "tables", "method_family_map.csv"))

methods_discipline <- corpus_unico |>
  select(UT, des_exp) |>
  separate_rows(des_exp, sep = ";") |>
  mutate(des_exp = str_squish(des_exp)) |>
  inner_join(method_family_map, by = "des_exp") |>
  inner_join(corpus_unico_core_area_long |> select(UT, core_area), by = "UT", relationship = "many-to-many") |>
  distinct(UT, core_area, method_family) |>
  count(core_area, method_family, name = "n") |>
  arrange(core_area, desc(n))

write_csv(methods_discipline, here("Paper_wos", "outputs", "tables", "methods_discipline.csv"))

core_area_labels <- c(
  "Business/Management" = "Business/\nManagement",
  "Economics" = "Economics",
  "Other" = "Other",
  "Political Science" = "Political\nScience",
  "Psychology" = "Psychology",
  "Sociology" = "Sociology"
)

method_family_labels <- c(
  "Survey-based experiments" = "Survey-based\nexperiments",
  "Field / lab interventions" = "Field / lab\ninterventions",
  "Conjoint / choice designs" = "Conjoint / choice\ndesigns",
  "Audit / correspondence designs" = "Audit / correspondence\ndesigns"
)

method_family_order <- c(
  "Survey-based experiments",
  "Field / lab interventions",
  "Conjoint / choice designs",
  "Audit / correspondence designs"
)

core_area_order <- c(
  "Economics",
  "Political Science",
  "Psychology",
  "Business/Management",
  "Other",
  "Sociology"
)

methods_plot <- ggplot(methods_discipline, aes(x = method_family, y = core_area, fill = n)) +
  geom_tile(color = "white", linewidth = 0.9) +
  geom_text(aes(label = n), size = 7.0, color = "#1C1C1C") +
  scale_x_discrete(limits = method_family_order, labels = method_family_labels, expand = expansion(add = 0)) +
  scale_y_discrete(limits = rev(core_area_order), labels = core_area_labels, expand = expansion(add = 0)) +
  scale_fill_gradient(low = "#F5F1E6", high = "#1F6F68", labels = comma) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = "Experimental design family",
    y = "Core discipline",
    fill = "Papers"
  ) +
  theme_minimal(base_size = 26) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(size = 25, margin = margin(t = 14)),
    axis.title.y = element_text(size = 25, margin = margin(r = 14)),
    axis.text.x = element_text(size = 20, lineheight = 0.95, margin = margin(t = 10)),
    axis.text.y = element_text(size = 21, lineheight = 0.95, margin = margin(r = 10)),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    panel.grid = element_blank(),
    plot.margin = margin(10, 20, 18, 20)
  )

ggsave(here("Paper_wos", "outputs", "figures", "methods_discipline.png"), methods_plot, width = 14, height = 7.2, dpi = 320)

# Mostrar graficos solo en sesiones interactivas.
if (interactive()) {
  print(usa_latam_plot)
  print(usa_latam_rate_plot)
  print(rate_latam_chile_usa_plot)
  print(universes_plot)
  print(status_plot)
  print(discipline_plot)
  print(countries_plot)
  print(latam_rate_country_plot)
  print(usa_rate_plot)
  print(methods_plot)
}


