# -----------------------------------------------------------------------------
# 07 - Tablas y figuras principales del paper affi_latam
# -----------------------------------------------------------------------------
# Organiza resultados para el manuscrito sin modificar la base oficial ni crear
# bases intermedias.
#
# Flujo:
#   00_inicial.R -> base_oficial.rds, una fila por paper.
#   05_descriptivos_bibliometricos.R -> afiliaciones de autor auditadas.
#   06_topic_model_exploratorio.R -> gamma y terminos del topic model.
#
# Disciplinas, paises y metodos no son mutuamente excluyentes. Esa decision se
# conserva en las tablas y se documenta en la hoja Notes.
# -----------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(writexl)
library(here)
library(wesanderson)

# -----------------------------------------------------------------------------
# 1. Rutas, parametros y estilo grafico
# -----------------------------------------------------------------------------
# Concentrar los parametros aqui permite actualizar el corte temporal y las
# etiquetas sin buscar valores dispersos en el script.

tables_dir <- here("Paper_wos", "outputs", "tables")
figures_dir <- here("Paper_wos", "outputs", "figures")

official_path <- here(
  "Paper_wos", "data", "intermediate", "base_oficial.rds"
)
descriptive_path <- file.path(tables_dir, "descriptivos_affi_latam.xlsx")
topic_path <- file.path(tables_dir, "topic_model_affi_latam.xlsx")
output_table_path <- file.path(
  tables_dir,
  "resultados_paper_affi_latam.xlsx"
)

first_year <- 1990L
last_year <- 2026L
last_month_2026 <- "July"
# 2026 se conserva, pero se identifica como ano parcial en tablas y figuras.

five_year_levels <- c(
  "1990-94", "1995-99", "2000-04", "2005-09",
  "2010-14", "2015-19", "2020-24", "2025-29"
)
decade_levels <- c("1990s", "2000s", "2010s", "2020-26")

theme_paper <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 8, hjust = 0),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

save_publication_figure <- function(plot, file_stem, width, height) {
  # Se guarda solo PNG de alta resolucion para revision y uso editorial.
  ggsave(
    file.path(figures_dir, str_c(file_stem, ".png")),
    plot,
    width = width,
    height = height,
    dpi = 400,
    bg = "white"
  )
}

five_year_period <- function(year) {
  # Convierte un ano en bloques de cinco anos desde 1990.
  start <- first_year + 5L * floor((as.integer(year) - first_year) / 5L)
  str_c(start, "-", str_sub(as.character(start + 4L), -2L))
}

decade_period <- function(year) {
  # La ultima categoria termina en julio de 2026 y no es una decada completa.
  case_when(
    year < 2000L ~ "1990s",
    year < 2010L ~ "2000s",
    year < 2020L ~ "2010s",
    TRUE ~ "2020-26"
  )
}

# -----------------------------------------------------------------------------
# 2. Cargar y validar los tres insumos
# -----------------------------------------------------------------------------
# base_oficial define el universo. Los otros archivos aportan solo afiliaciones
# individuales ya auditadas y resultados del topic model.

base_oficial <- readRDS(official_path)

required_official <- c(
  "UT", "TI", "PY", "SO", "WE", "RP", "core_area",
  "affi_latam_countries", "des_exp"
)
missing_official <- setdiff(required_official, names(base_oficial))

if (length(missing_official) > 0L) {
  stop(
    "Faltan columnas en base_oficial.rds: ",
    str_c(missing_official, collapse = ", ")
  )
}

if (nrow(base_oficial) != 675L || anyDuplicated(base_oficial$UT) > 0L) {
  stop("La base oficial debe contener 675 UT unicas.")
}

authors_long <- read_excel(descriptive_path, sheet = "Authors_long")
topic_gamma <- read_excel(topic_path, sheet = "Paper_topic_gamma")
paper_dominant <- read_excel(topic_path, sheet = "Paper_dominant_topic")
topic_terms <- read_excel(topic_path, sheet = "Top_terms")
topic_summary <- read_excel(topic_path, sheet = "Model_summary")

recommended_k <- topic_summary |>
  filter(measure == "Recommended k") |>
  pull(value) |>
  as.integer()

if (recommended_k != 7L) {
  stop(
    "Las etiquetas sustantivas de 07 fueron definidas para k = 7. ",
    "Revise el topic model antes de continuar."
  )
}

if (
  n_distinct(topic_gamma$UT) != 675L ||
    nrow(topic_gamma) != 675L * recommended_k ||
    n_distinct(paper_dominant$UT) != 675L
) {
  stop("Los resultados de 06 no cubren los 675 papers y siete topics.")
}

papers <- base_oficial |>
  transmute(
    UT,
    year = as.integer(PY),
    title = TI,
    journal = SO,
    wos_index = WE,
    corresponding_address = RP,
    core_area,
    latam_countries = affi_latam_countries,
    experimental_method = des_exp,
    period_5year = factor(
      five_year_period(year),
      levels = five_year_levels,
      ordered = TRUE
    ),
    decade = factor(
      decade_period(year),
      levels = decade_levels,
      ordered = TRUE
    )
  )

if (any(is.na(papers$year)) || any(papers$year > last_year)) {
  stop("La base contiene anos faltantes o posteriores al corte definido.")
}

core_area_long <- papers |>
  select(UT, year, period_5year, core_area) |>
  separate_rows(core_area, sep = "\\s*;\\s*|\\s*\\|\\s*") |>
  mutate(core_area = str_squish(core_area)) |>
  filter(!is.na(core_area), core_area != "") |>
  distinct(UT, core_area, .keep_all = TRUE)

# -----------------------------------------------------------------------------
# 3. Table 2: produccion por disciplina y periodos de cinco anos
# -----------------------------------------------------------------------------
# Un paper con dos core areas cuenta una vez en cada una.

table2_long <- core_area_long |>
  count(core_area, period_5year, name = "n_papers") |>
  complete(
    core_area,
    period_5year = factor(
      five_year_levels,
      levels = five_year_levels,
      ordered = TRUE
    ),
    fill = list(n_papers = 0L)
  )

table2_disciplines <- table2_long |>
  mutate(period_5year = as.character(period_5year)) |>
  pivot_wider(names_from = period_5year, values_from = n_papers) |>
  mutate(Total = rowSums(across(all_of(five_year_levels)))) |>
  arrange(desc(Total), core_area)

# -----------------------------------------------------------------------------
# 4. Table 3: menciones de paises LatAm por decada
# -----------------------------------------------------------------------------
# Cada UT cuenta una vez por pais, pero puede aparecer en varios paises.

country_mentions <- papers |>
  select(UT, year, decade, latam_countries) |>
  separate_rows(latam_countries, sep = "\\s*;\\s*") |>
  transmute(
    UT,
    year,
    decade,
    country = str_squish(latam_countries)
  ) |>
  filter(!is.na(country), country != "") |>
  distinct(UT, country, .keep_all = TRUE)

if (n_distinct(country_mentions$UT) != 675L) {
  stop("No todos los papers conservan al menos una mencion de pais LatAm.")
}

table3_countries <- country_mentions |>
  count(country, decade, name = "n_papers") |>
  complete(
    country,
    decade = factor(
      decade_levels,
      levels = decade_levels,
      ordered = TRUE
    ),
    fill = list(n_papers = 0L)
  ) |>
  mutate(decade = as.character(decade)) |>
  pivot_wider(names_from = decade, values_from = n_papers) |>
  mutate(Total = rowSums(across(all_of(decade_levels)))) |>
  arrange(desc(Total), country)

# -----------------------------------------------------------------------------
# 5. Tables 4 and Figure 4: metodos experimentales
# -----------------------------------------------------------------------------
# Los metodos separados por punto y coma se cuentan individualmente. Etiquetas
# genericas o faltantes forman Other / not classified.

generic_method_labels <- c(
  "Experimental methods",
  "Other experimental design"
)

methods_long <- papers |>
  select(UT, year, period_5year, experimental_method) |>
  mutate(
    experimental_method = replace_na(
      experimental_method,
      "Other / not classified"
    )
  ) |>
  separate_rows(experimental_method, sep = "\\s*;\\s*") |>
  mutate(
    experimental_method = str_squish(experimental_method),
    experimental_method = if_else(
      experimental_method %in% generic_method_labels |
        experimental_method == "",
      "Other / not classified",
      experimental_method
    )
  ) |>
  distinct(UT, experimental_method, .keep_all = TRUE)

table4_methods_long <- methods_long |>
  inner_join(
    core_area_long |> select(UT, core_area),
    by = "UT",
    relationship = "many-to-many"
  ) |>
  distinct(UT, core_area, experimental_method, period_5year) |>
  count(
    core_area,
    experimental_method,
    period_5year,
    name = "n_papers"
  )

table4_methods_disciplines <- table4_methods_long |>
  mutate(period_5year = as.character(period_5year)) |>
  pivot_wider(
    names_from = period_5year,
    values_from = n_papers,
    values_fill = 0L
  ) |>
  mutate(
    across(any_of(five_year_levels), ~ replace_na(.x, 0L)),
    Total = rowSums(across(any_of(five_year_levels)))
  ) |>
  arrange(core_area, desc(Total), experimental_method)

method_order <- methods_long |>
  count(experimental_method, name = "total") |>
  arrange(
    experimental_method == "Other / not classified",
    desc(total)
  ) |>
  pull(experimental_method)

method_year <- methods_long |>
  count(year, experimental_method, name = "n_papers") |>
  complete(
    year = seq.int(min(papers$year), max(papers$year)),
    experimental_method = method_order,
    fill = list(n_papers = 0L)
  ) |>
  mutate(
    experimental_method = factor(
      experimental_method,
      levels = method_order
    )
  )

method_colors <- set_names(
  wes_palette("Zissou1", length(method_order), type = "continuous"),
  method_order
)

figure4 <- ggplot(
  method_year,
  aes(year, n_papers, color = experimental_method)
) +
  geom_line(linewidth = 0.7, show.legend = FALSE) +
  geom_point(size = 1.2, show.legend = FALSE) +
  facet_wrap(
    ~ experimental_method,
    ncol = 3,
    scales = "fixed",
    labeller = label_wrap_gen(width = 28)
  ) +
  scale_color_manual(values = method_colors, drop = FALSE) +
  scale_x_continuous(breaks = seq(2000, 2025, 5)) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(n = 4),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "Experimental methods over time",
    subtitle = "Annual number of Latin America-affiliated papers",
    x = NULL,
    y = "Number of papers",
    caption = str_c(
      "Methods are not mutually exclusive. Other includes papers without a ",
      "specific method label. 2026 includes records through ",
      last_month_2026, "."
    )
  ) +
  theme_paper +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -----------------------------------------------------------------------------
# 6. Table 5: topics principales por disciplina
# -----------------------------------------------------------------------------
# Se prueban topic dominante y promedio de gamma. Gamma conserva la mezcla
# completa de topics de cada paper.

topic_dictionary <- tribble(
  ~topic, ~topic_label,
  1L, "Natural resources, communities, and service provision",
  2L, "Consumption, food, and sustainability",
  3L, "Electoral behavior and politics",
  4L, "Public opinion, political attitudes, and policy preferences",
  5L, "Education, gender, and labor inequality",
  6L, "Economic preferences and willingness to pay",
  7L, "Information, risk, and cognitive decision-making"
)

topic_core_gamma <- topic_gamma |>
  select(UT, topic, gamma) |>
  inner_join(
    core_area_long |> select(UT, core_area),
    by = "UT",
    relationship = "many-to-many"
  ) |>
  group_by(core_area, topic) |>
  summarise(
    n_papers_in_area = n_distinct(UT),
    mean_gamma = mean(gamma),
    expected_papers = sum(gamma),
    .groups = "drop"
  ) |>
  left_join(topic_dictionary, by = "topic") |>
  group_by(core_area) |>
  arrange(desc(mean_gamma), topic, .by_group = TRUE) |>
  mutate(
    topic_rank = row_number(),
    value_percent = 100 * mean_gamma,
    approach = "Average topic proportion"
  ) |>
  ungroup()

topic_core_dominant <- paper_dominant |>
  select(UT, dominant_topic) |>
  inner_join(
    core_area_long |> select(UT, core_area),
    by = "UT",
    relationship = "many-to-many"
  ) |>
  count(core_area, dominant_topic, name = "n_dominant_papers") |>
  group_by(core_area) |>
  mutate(
    n_papers_in_area = sum(n_dominant_papers),
    value_percent = 100 * n_dominant_papers / n_papers_in_area
  ) |>
  ungroup() |>
  rename(topic = dominant_topic) |>
  left_join(topic_dictionary, by = "topic") |>
  group_by(core_area) |>
  arrange(desc(value_percent), topic, .by_group = TRUE) |>
  mutate(
    topic_rank = row_number(),
    approach = "Dominant topic"
  ) |>
  ungroup()

table5_all_topics <- bind_rows(
  topic_core_gamma |>
    transmute(
      approach, core_area, topic_rank, topic, topic_label,
      value_percent, n_papers_in_area, expected_papers,
      n_dominant_papers = NA_integer_
    ),
  topic_core_dominant |>
    transmute(
      approach, core_area, topic_rank, topic, topic_label,
      value_percent, n_papers_in_area,
      expected_papers = NA_real_, n_dominant_papers
    )
) |>
  arrange(approach, core_area, topic_rank)

table5_top3_topics <- table5_all_topics |>
  filter(topic_rank <= 3L)

# Esta tabla pone ambas medidas en una misma fila. Se conserva completa en el
# Excel; el grafico usa solo los tres topics con mayor gamma por disciplina.
topics_discipline_plot <- topic_core_gamma |>
  transmute(
    core_area, topic, topic_label,
    gamma_rank = topic_rank,
    average_gamma_percent = value_percent,
    expected_papers,
    n_papers_in_area
  ) |>
  left_join(
    topic_core_dominant |>
      transmute(
        core_area, topic,
        dominant_rank = topic_rank,
        dominant_topic_percent = value_percent,
        n_dominant_papers
      ),
    by = c("core_area", "topic")
  ) |>
  mutate(
    dominant_topic_percent = replace_na(dominant_topic_percent, 0),
    n_dominant_papers = replace_na(n_dominant_papers, 0L),
    is_top3_gamma = gamma_rank <= 3L,
    topic_display = str_wrap(
      str_c("T", topic, ". ", topic_label),
      width = 38
    )
  ) |>
  arrange(core_area, gamma_rank)

topics_discipline_top3 <- topics_discipline_plot |>
  filter(is_top3_gamma) |>
  mutate(
    topic_axis = tidytext::reorder_within(
      topic_display,
      average_gamma_percent,
      core_area
    )
  )

topics_discipline_top3_long <- topics_discipline_top3 |>
  select(
    core_area, topic_axis,
    average_gamma_percent, dominant_topic_percent
  ) |>
  pivot_longer(
    cols = c(average_gamma_percent, dominant_topic_percent),
    names_to = "metric",
    values_to = "percent"
  ) |>
  mutate(
    metric = recode(
      metric,
      average_gamma_percent = "Average topic proportion (gamma)",
      dominant_topic_percent = "Papers with dominant topic"
    ),
    metric = factor(
      metric,
      levels = c(
        "Average topic proportion (gamma)",
        "Papers with dominant topic"
      )
    )
  )

topic_discipline_colors <- set_names(
  wes_palette("Darjeeling1", 2, type = "continuous"),
  levels(topics_discipline_top3_long$metric)
)

figure_topics_by_discipline <- ggplot() +
  geom_segment(
    data = topics_discipline_top3,
    aes(
      x = average_gamma_percent,
      xend = dominant_topic_percent,
      y = topic_axis,
      yend = topic_axis
    ),
    color = "grey72",
    linewidth = 0.8
  ) +
  geom_point(
    data = topics_discipline_top3_long,
    aes(x = percent, y = topic_axis, color = metric, shape = metric),
    size = 3,
    stroke = 1.1
  ) +
  facet_wrap(
    ~ core_area,
    ncol = 2,
    scales = "free_y",
    labeller = label_wrap_gen(width = 28)
  ) +
  tidytext::scale_y_reordered() +
  scale_color_manual(values = topic_discipline_colors, drop = FALSE) +
  scale_shape_manual(values = c(16, 1), drop = FALSE) +
  scale_x_continuous(
    labels = scales::label_percent(scale = 1),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  labs(
    title = "Leading topics by discipline",
    subtitle = "Top three topics selected by average topic proportion",
    x = "Topic prominence within discipline",
    y = NULL,
    color = NULL,
    shape = NULL,
    caption = paste(
      "Average gamma preserves each paper's topic mixture;",
      "the dominant-topic measure assigns each paper to one topic.",
      "Disciplines are not mutually exclusive."
    )
  ) +
  theme_paper

# -----------------------------------------------------------------------------
# 7. Figure 6: liderazgo del primer autor o autor corresponsal
# -----------------------------------------------------------------------------
# Para primer autor se usa 05. Para el corresponsal se recuperan paises desde RP.

latam_countries <- c(
  "Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica",
  "Cuba", "Dominican Rep", "Ecuador", "El Salvador", "Guatemala", "Haiti",
  "Honduras", "Mexico", "Nicaragua", "Panama", "Paraguay", "Peru",
  "Puerto Rico", "Uruguay", "Venezuela"
)

europe_countries <- c(
  "Austria", "Belgium", "Croatia", "Czechia", "Denmark", "Finland",
  "France", "Germany", "Hungary", "Ireland", "Italy", "Netherlands",
  "Norway", "Poland", "Portugal", "Russia", "Slovakia", "Spain",
  "Sweden", "Switzerland", "Turkey", "United Kingdom"
)

standard_country_tokens <- c(
  "ARGENTINA", "AUSTRALIA", "AUSTRIA", "BELGIUM", "BENIN", "BOLIVIA",
  "BRAZIL", "BURKINA FASO", "CAMBODIA", "CANADA", "CHILE", "COLOMBIA",
  "COSTA RICA", "COTE IVOIRE", "CROATIA", "CUBA", "DENMARK", "ECUADOR",
  "ETHIOPIA", "FINLAND", "FRANCE", "GERMANY", "GUATEMALA", "HONDURAS",
  "HUNGARY", "INDIA", "INDONESIA", "IRELAND", "ISRAEL", "ITALY", "JAPAN",
  "KENYA", "KUWAIT", "MEXICO", "MOROCCO", "NEPAL", "NETHERLANDS",
  "NIGERIA", "NORWAY", "PAKISTAN", "PANAMA", "PARAGUAY", "PERU",
  "PHILIPPINES", "POLAND", "PORTUGAL", "PUERTO RICO", "RUSSIA", "SENEGAL",
  "SINGAPORE", "SLOVAKIA", "SOUTH AFRICA", "SPAIN", "SWEDEN",
  "SWITZERLAND", "TAIWAN", "TANZANIA", "THAILAND", "TURKEY", "URUGUAY",
  "VENEZUELA"
)

country_lookup <- bind_rows(
  tibble(
    country_raw = standard_country_tokens,
    country = str_to_title(str_to_lower(standard_country_tokens))
  ),
  tribble(
    ~country_raw, ~country,
    "USA", "United States",
    "ENGLAND", "United Kingdom",
    "SCOTLAND", "United Kingdom",
    "WALES", "United Kingdom",
    "NORTH IRELAND", "United Kingdom",
    "PEOPLES R CHINA", "China",
    "SOUTH KOREA", "South Korea",
    "CZECH REPUBLIC", "Czechia",
    "DOMINICAN REP", "Dominican Rep",
    "U ARAB EMIRATES", "United Arab Emirates",
    "TURKIYE", "Turkey"
  )
) |>
  distinct(country_raw, .keep_all = TRUE)

extract_country_mentions <- function(text) {
  # Admite mas de un corresponsal o afiliacion en la misma celda RP.
  if (is.na(text) || str_squish(text) == "") return(character())

  normalized_text <- text |>
    iconv(from = "", to = "ASCII//TRANSLIT") |>
    str_to_upper()

  country_lookup |>
    filter(
      map_lgl(
        country_raw,
        ~ str_detect(
          normalized_text,
          regex(str_c("(?<![A-Z])", .x, "(?![A-Z])"))
        )
      )
    ) |>
    pull(country) |>
    unique()
}

classify_leadership <- function(countries) {
  # LatAm tiene prioridad si una persona declara afiliaciones en varias regiones.
  countries <- countries[!is.na(countries) & countries != ""]

  case_when(
    any(countries %in% latam_countries) ~ "Latin America",
    any(countries %in% c("United States", europe_countries)) ~ "US/Europe",
    TRUE ~ "Other/unknown"
  )
}

first_author_region <- authors_long |>
  filter(author_position == 1L) |>
  select(UT, author_countries) |>
  separate_rows(author_countries, sep = "\\s*;\\s*", convert = FALSE) |>
  group_by(UT) |>
  summarise(
    first_author_region = classify_leadership(author_countries),
    .groups = "drop"
  )

corresponding_region <- papers |>
  transmute(
    UT,
    corresponding_countries = map(
      corresponding_address,
      extract_country_mentions
    )
  ) |>
  mutate(
    corresponding_region = map_chr(
      corresponding_countries,
      classify_leadership
    )
  ) |>
  select(UT, corresponding_region)

leadership_paper <- papers |>
  select(UT, year, period_5year) |>
  left_join(first_author_region, by = "UT") |>
  left_join(corresponding_region, by = "UT") |>
  mutate(
    across(
      c(first_author_region, corresponding_region),
      ~ replace_na(.x, "Other/unknown")
    ),
    combined_leadership = case_when(
      first_author_region == "Latin America" |
        corresponding_region == "Latin America" ~ "Latin America",
      first_author_region == "US/Europe" |
        corresponding_region == "US/Europe" ~ "US/Europe",
      TRUE ~ "Other/unknown"
    )
  )

leadership_by_discipline <- leadership_paper |>
  inner_join(
    core_area_long |> select(UT, core_area),
    by = "UT",
    relationship = "many-to-many"
  )

leadership_separate <- leadership_by_discipline |>
  select(
    UT, core_area, period_5year,
    first_author_region, corresponding_region
  ) |>
  pivot_longer(
    c(first_author_region, corresponding_region),
    names_to = "leadership_measure",
    values_to = "region"
  ) |>
  count(
    leadership_measure, core_area, period_5year, region,
    name = "n_papers"
  ) |>
  group_by(leadership_measure, core_area, period_5year) |>
  mutate(percent = 100 * n_papers / sum(n_papers)) |>
  ungroup()

leadership_combined <- leadership_by_discipline |>
  count(
    core_area, period_5year, combined_leadership,
    name = "n_papers"
  ) |>
  group_by(core_area, period_5year) |>
  mutate(percent = 100 * n_papers / sum(n_papers)) |>
  ungroup()

# Para la figura se muestra la proporcion sustantiva de interes: papers con
# liderazgo LatAm. El tamano de cada punto transparenta cuantos papers sostienen
# el porcentaje y evita equiparar periodos con uno y con muchos registros.
figure6_plot_data <- leadership_combined |>
  group_by(core_area, period_5year) |>
  summarise(
    n_papers = sum(n_papers),
    n_latam_leadership = sum(
      n_papers[combined_leadership == "Latin America"]
    ),
    percent_latam_leadership = 100 * n_latam_leadership / n_papers,
    .groups = "drop"
  )

leadership_point_color <- wes_palette("Darjeeling1", 3L)[[1]]

figure6 <- figure6_plot_data |>
  ggplot(aes(period_5year, percent_latam_leadership, group = 1)) +
  geom_line(color = "grey65", linewidth = 0.65) +
  geom_point(
    aes(size = n_papers),
    shape = 21,
    fill = leadership_point_color,
    color = "white",
    stroke = 0.7,
    alpha = 0.95
  ) +
  facet_wrap(~ core_area, ncol = 2) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 25),
    labels = scales::label_percent(scale = 1),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_size_area(
    max_size = 9,
    breaks = scales::breaks_pretty(n = 4),
    name = "Papers in period"
  ) +
  labs(
    title = "Leadership of Latin America-affiliated experimental papers",
    subtitle = paste(
      "Share with a Latin American first or corresponding author;",
      "point size shows the number of papers"
    ),
    x = NULL,
    y = "Papers with Latin American leadership",
    caption = str_c(
      "Latin America is assigned when either the first or corresponding ",
      "author has a Latin American affiliation. Disciplines overlap. ",
      "2025-29 includes records through ", last_month_2026, " 2026."
    )
  ) +
  theme_paper +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_line(color = "grey92")
  )

# -----------------------------------------------------------------------------
# 8. Figure 7: terminos principales del topic model
# -----------------------------------------------------------------------------
# Las etiquetas se basan en la lectura preliminar de papers representativos.

topic_colors <- set_names(
  wes_palette("FantasticFox1", recommended_k, type = "continuous"),
  as.character(seq_len(recommended_k))
)

figure7_data <- topic_terms |>
  left_join(topic_dictionary, by = "topic") |>
  group_by(topic) |>
  slice_max(beta, n = 10L, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    term_display = str_replace_all(term, "_", " "),
    term_display = tidytext::reorder_within(
      term_display,
      beta,
      topic_label
    )
  )

figure7 <- ggplot(
  figure7_data,
  aes(beta, term_display, fill = factor(topic))
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(
    ~ topic_label,
    scales = "free_y",
    ncol = 2,
    labeller = label_wrap_gen(width = 38)
  ) +
  tidytext::scale_y_reordered() +
  scale_fill_manual(values = topic_colors) +
  labs(
    title = "Substantive topics in Latin America-affiliated papers",
    subtitle = str_c("Top terms from the k = ", recommended_k, " LDA model"),
    x = "Term probability within topic",
    y = NULL,
    caption = paste(
      "Topic labels are substantive interpretations;",
      "multiword expressions are retained as single terms."
    )
  ) +
  theme_paper

# -----------------------------------------------------------------------------
# 9. Auditoria ESCI y notas
# -----------------------------------------------------------------------------
# WE documenta la coleccion de indexacion sin alterar el universo analitico.

esci_audit <- papers |>
  mutate(
    is_esci = str_detect(
      wos_index,
      regex("EMERGING SOURCES", ignore_case = TRUE)
    )
  ) |>
  count(year, is_esci, name = "n_papers") |>
  group_by(year) |>
  mutate(percent_in_year = 100 * n_papers / sum(n_papers)) |>
  ungroup() |>
  arrange(year, desc(is_esci))

notes <- tribble(
  ~output, ~note,
  "All", "The analytical corpus contains 675 papers with at least one Latin American affiliation.",
  "All", str_c("2026 is partial and includes records available through ", last_month_2026, "."),
  "Table 2", "Core disciplines are not mutually exclusive; a paper can contribute to more than one discipline.",
  "Table 3", "Country mentions are not mutually exclusive; each paper counts once per Latin American country affiliation.",
  "Table 4 / Figure 4", "Experimental methods are not mutually exclusive; Other includes records without a specific method label.",
  "Table 5", "Average gamma is the primary topic measure; dominant-topic counts are a complementary simplification.",
  "Figure 6", "Latin American leadership is assigned when the first or corresponding author has a Latin American affiliation.",
  "Figure 6", "First-author and corresponding-author classifications are exported separately for reporting in the text.",
  "ESCI", "WE identifies ESCI, SSCI, SCI-EXPANDED, and other WoS citation indexes."
)

# La primera hoja funciona como mapa del libro y de las figuras externas.
results_index <- tribble(
  ~item, ~location, ~description,
  "Table 2", "Table2_disciplines", "Production by core discipline and five-year period.",
  "Table 3", "Table3_countries", "Latin American country mentions by decade; countries overlap.",
  "Table 4", "Table4_methods_disciplines", "Experimental methods by discipline and period.",
  "Table 5", "Table5_top3_topics", "Top three topics per discipline using gamma and dominant-topic measures.",
  "Topic appendix", "Appendix_topics_full", "All topics by discipline under both topic measures.",
  "Topic plot data", "Topics_plot_data", "Exact values for the topic-by-discipline comparison; all seven topics are retained.",
  "Leadership", "Leadership_combined", "Combined first- or corresponding-author leadership classification.",
  "Leadership detail", "Leadership_separate", "First and corresponding authors reported separately.",
  "ESCI audit", "ESCI_audit", "Annual identification of records indexed in ESCI.",
  "Topic labels", "Topic_dictionary", "Substantive label assigned to each topic.",
  "Method notes", "Notes", "Definitions and non-mutual-exclusivity notes.",
  "Figure 4", "outputs/figures/figure4_methods_over_time.png", "Annual papers by experimental method.",
  "Figure 6", "outputs/figures/figure6_leadership.png", "Latin American leadership share by discipline and period; point size is the number of papers.",
  "Figure 7", "outputs/figures/figure7_topic_terms.png", "Top terms defining each topic.",
  "Topics by discipline", "outputs/figures/figure_topics_by_discipline.png", "Replacement for the former heatmap: top-three gamma topics with dominant-topic percentage as comparison."
)

# -----------------------------------------------------------------------------
# 10. Exportar un libro y las figuras
# -----------------------------------------------------------------------------

dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

write_xlsx(
  list(
    README = results_index,
    Table2_disciplines = table2_disciplines,
    Table3_countries = table3_countries,
    Table4_methods_disciplines = table4_methods_disciplines,
    Table5_top3_topics = table5_top3_topics,
    Appendix_topics_full = table5_all_topics,
    Topics_plot_data = topics_discipline_plot,
    Leadership_combined = leadership_combined,
    Leadership_separate = leadership_separate,
    Figure6_plot_data = figure6_plot_data,
    ESCI_audit = esci_audit,
    Topic_dictionary = topic_dictionary,
    Notes = notes
  ),
  output_table_path
)

save_publication_figure(
  figure4, "figure4_methods_over_time",
  width = 12, height = 11
)
save_publication_figure(
  figure6, "figure6_leadership",
  width = 12, height = 9
)
save_publication_figure(
  figure7, "figure7_topic_terms",
  width = 13, height = 11
)
save_publication_figure(
  figure_topics_by_discipline, "figure_topics_by_discipline",
  width = 12, height = 10
)

# -----------------------------------------------------------------------------
# 11. Controles visibles al terminar
# -----------------------------------------------------------------------------
# Se imprimen en consola para no agregar diagnosticos al libro principal.

diagnostics <- tibble(
  measure = c(
    "Official papers",
    "Discipline mentions",
    "Latin American country mentions",
    "Papers with a specific method",
    "Papers in Other / not classified",
    "ESCI papers",
    "First-author Other/unknown",
    "Corresponding-author Other/unknown"
  ),
  value = c(
    n_distinct(papers$UT),
    nrow(core_area_long),
    nrow(country_mentions),
    n_distinct(methods_long$UT[
      methods_long$experimental_method != "Other / not classified"
    ]),
    n_distinct(methods_long$UT[
      methods_long$experimental_method == "Other / not classified"
    ]),
    sum(str_detect(
      papers$wos_index,
      regex("EMERGING SOURCES", ignore_case = TRUE)
    )),
    sum(leadership_paper$first_author_region == "Other/unknown"),
    sum(leadership_paper$corresponding_region == "Other/unknown")
  )
)

cat("\nResultados del paper creados correctamente\n")
cat("Tabla: ", output_table_path, "\n", sep = "")
print(diagnostics)