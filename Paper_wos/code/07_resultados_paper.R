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
library(patchwork)

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
# El ultimo bloque de cinco anos se rotula 2025-26 (no hay datos despues de 2026).

five_year_levels <- c(
  "1990-94", "1995-99", "2000-04", "2005-09",
  "2010-14", "2015-19", "2020-24", "2025-26"
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
  label <- str_c(start, "-", str_sub(as.character(start + 4L), -2L))
  # No hay papers despues de 2026: el ultimo bloque se rotula 2025-26 (parcial)
  # para no sugerir cobertura hasta 2029.
  if_else(label == "2025-29", "2025-26", label)
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
# 5b. Figure 5: origen de la produccion (colaboracion de afiliaciones)
# -----------------------------------------------------------------------------
# Usa las afiliaciones por autor de 05 (Authors_long, ya con las correcciones
# del chequeo manual de Tania incorporadas en 00_inicial.R). Cada paper cae en
# UNA sola categoria (mutuamente excluyentes). Todos los papers tienen al menos
# un autor LatAm. Si hay autores de Europa y/o EE.UU., esa presencia define la
# categoria aunque haya ademas autores de otras regiones; "Latam-other" queda
# solo para papers cuyos coautores no LatAm provienen exclusivamente de otras
# regiones (Canada, Australia, Asia, etc.).

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

origin_levels <- c(
  "Latam only", "Latam-Europe", "Latam-U.S.",
  "Latam-Europe + U.S.", "Latam-other"
)

paper_origin <- authors_long |>
  select(UT, author_countries) |>
  separate_rows(author_countries, sep = "\\s*;\\s*") |>
  mutate(author_countries = na_if(author_countries, "")) |>
  group_by(UT) |>
  summarise(
    has_latam = any(author_countries %in% latam_countries),
    has_europe = any(author_countries %in% europe_countries),
    has_us = any(author_countries == "United States"),
    has_other = any(
      !is.na(author_countries) &
        !author_countries %in% c(latam_countries, europe_countries, "United States")
    ),
    .groups = "drop"
  ) |>
  mutate(
    origin = case_when(
      has_europe & has_us ~ "Latam-Europe + U.S.",
      has_europe ~ "Latam-Europe",
      has_us ~ "Latam-U.S.",
      has_other ~ "Latam-other",
      TRUE ~ "Latam only"
    ),
    origin = factor(origin, levels = origin_levels)
  )

if (nrow(paper_origin) != 675L || !all(paper_origin$has_latam)) {
  stop("Figure 5: todos los 675 papers deben tener al menos un autor LatAm.")
}

table_figure5_origin <- paper_origin |>
  count(origin, name = "n_papers", .drop = FALSE) |>
  mutate(percent = round(100 * n_papers / sum(n_papers), 1)) |>
  bind_rows(
    tibble(
      origin = NA, n_papers = nrow(paper_origin), percent = 100
    )
  ) |>
  mutate(origin = as.character(origin) |> replace_na("Total"))

figure5_origin_note <- str_c(
  "Mutually exclusive categories based on author affiliations. ",
  "Latam only: no co-author outside Latin America. ",
  "Latam-Europe / Latam-U.S. / Latam-Europe + U.S.: at least one author ",
  "affiliated in Europe and/or the United States; this presence defines the ",
  "category even when authors from other regions are also present. ",
  "Latam-other: non-Latin American co-authors come only from other regions ",
  "(e.g., Canada, Australia, Asia), with no Europe or U.S. affiliation."
)

# La nota viaja con la tabla (ultima fila) ademas de la hoja Notes.
table_figure5_origin <- table_figure5_origin |>
  bind_rows(tibble(
    origin = str_c("Note: ", figure5_origin_note),
    n_papers = NA_integer_, percent = NA_real_
  ))

# Origen por disciplina (total en el tiempo). Las disciplinas se superponen: un
# paper cuenta en cada disciplina a la que pertenece, asi que la suma de filas
# supera 675 (mismo criterio que Table 2).
table_figure5_by_discipline <- paper_origin |>
  select(UT, origin) |>
  inner_join(
    core_area_long |> select(UT, core_area),
    by = "UT",
    relationship = "many-to-many"
  ) |>
  count(core_area, origin, name = "n_papers", .drop = FALSE) |>
  group_by(core_area) |>
  mutate(
    n_discipline = sum(n_papers),
    percent = round(100 * n_papers / n_discipline, 1)
  ) |>
  ungroup() |>
  filter(n_discipline > 0L) |>
  mutate(label = str_c(n_papers, " (", percent, "%)")) |>
  select(core_area, n_discipline, origin, label) |>
  pivot_wider(names_from = origin, values_from = label) |>
  arrange(desc(n_discipline)) |>
  rename(`Core discipline` = core_area, `Papers in discipline` = n_discipline)

figure5_discipline_note <- str_c(
  "Origin categories as in Figure 5 (mutually exclusive within each discipline). ",
  "Disciplines are not mutually exclusive: a paper counts in every discipline ",
  "it belongs to, so discipline totals add to more than 675. ",
  "Cells show n (% of the discipline's papers); totals cover all years through ",
  last_month_2026, " 2026."
)

table_figure5_by_discipline <- table_figure5_by_discipline |>
  bind_rows(
    tibble(`Core discipline` = str_c("Note: ", figure5_discipline_note))
  )

# Figure 5 por disciplina: proporcion de cada origen sobre el total de papers de
# la disciplina (barras horizontales al 100%, n de la disciplina en la etiqueta).
figure5_discipline_data <- paper_origin |>
  select(UT, origin) |>
  inner_join(
    core_area_long |> select(UT, core_area),
    by = "UT",
    relationship = "many-to-many"
  ) |>
  count(core_area, origin, name = "n_papers", .drop = FALSE) |>
  group_by(core_area) |>
  mutate(
    n_discipline = sum(n_papers),
    percent = 100 * n_papers / n_discipline
  ) |>
  ungroup() |>
  filter(n_discipline > 0L) |>
  group_by(core_area) |>
  mutate(
    discipline_label = str_c(core_area, "\n(n = ", n_discipline, ")"),
    # Orden: de mayor a menor % de "Latam only" (primera fila arriba).
    latam_only_percent = percent[origin == "Latam only"][1]
  ) |>
  ungroup() |>
  mutate(
    discipline_label = fct_reorder(discipline_label, latam_only_percent)
  )

figure5_by_discipline <- ggplot(
  figure5_discipline_data,
  aes(percent, discipline_label, fill = fct_rev(origin))
) +
  geom_col(width = 0.7, color = "white", linewidth = 0.3, alpha = 0.7) +
  geom_text(
    aes(label = if_else(percent >= 5, str_c(round(percent), "%"), "")),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  scale_fill_manual(
    values = origin_colors,
    breaks = origin_levels,
    name = "Origin"
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, 25),
    labels = scales::label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(
    title = "Origin of experimental papers by discipline",
    subtitle = "Share of each discipline's Latin America-affiliated papers, all years",
    x = "Share of papers in discipline",
    y = NULL,
    caption = str_wrap(
      str_c(
        figure5_origin_note,
        " Disciplines overlap, so a paper counts in each of its disciplines.",
        " Includes records through ", last_month_2026, " 2026."
      ),
      width = 150
    )
  ) +
  theme_paper +
  theme(
    panel.grid.major.x = element_line(color = "grey92"),
    panel.grid.major.y = element_blank()
  )

# Figure 5: solo evolucion en el tiempo, periodos de cinco anos. Se muestra el
# porcentaje sobre el total de papers del periodo y el n total sobre cada barra,
# porque los periodos tempranos tienen pocos papers.
figure5_plot_data <- paper_origin |>
  left_join(papers |> select(UT, period_5year), by = "UT") |>
  # 1995-2009 se agrupa: tiene solo 17 papers repartidos en tres periodos.
  mutate(
    period_5year = fct_collapse(
      factor(period_5year, ordered = FALSE),
      "1995-2009" = c("1995-99", "2000-04", "2005-09")
    ) |>
      fct_relevel("1995-2009")
  ) |>
  count(period_5year, origin, name = "n_papers", .drop = FALSE) |>
  group_by(period_5year) |>
  mutate(
    n_period = sum(n_papers),
    percent = 100 * n_papers / n_period
  ) |>
  ungroup() |>
  filter(n_period > 0L)

figure5_totals <- figure5_plot_data |>
  distinct(period_5year, n_period)

# Misma familia de paletas que Figure 4 (Zissou1), en tonos suaves.
origin_colors <- setNames(
  wes_palette("Zissou1", length(origin_levels), type = "continuous"),
  origin_levels
)

figure5 <- ggplot(
  figure5_plot_data,
  aes(period_5year, percent, fill = fct_rev(origin))
) +
  geom_col(width = 0.75, color = "white", linewidth = 0.3, alpha = 0.7) +
  geom_text(
    data = figure5_totals,
    aes(period_5year, 100, label = str_c("n = ", n_period)),
    inherit.aes = FALSE,
    vjust = -0.5,
    size = 3
  ) +
  scale_fill_manual(
    values = origin_colors,
    breaks = origin_levels,
    name = "Origin"
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 25),
    labels = scales::label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.08))
  ) +
  guides(fill = guide_legend(nrow = 1, reverse = FALSE)) +
  labs(
    title = "Origin of experimental papers over time",
    subtitle = "Share of Latin America-affiliated papers by author affiliations; 1995-2009 pooled, then five-year periods",
    x = NULL,
    y = "Share of papers in period",
    caption = str_wrap(
      str_c(
        figure5_origin_note, " 2025-26 includes records through ",
        last_month_2026, " 2026."
      ),
      width = 150
    )
  ) +
  theme_paper +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.caption = element_text(size = 8, hjust = 0)
  )

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

# Figure 6: liderazgo latinoamericano (LatAm vs. todo el resto junto).
# Clasificacion mutuamente excluyente con prioridad al primer autor:
#   - "First author": el primer autor tiene afiliacion LatAm.
#   - "Corresponding author only": el primer autor NO es LatAm pero el autor
#     corresponsal si. Asi un paper nunca se cuenta dos veces y la suma de ambas
#     lineas es el liderazgo LatAm combinado (primer autor o corresponsal).
# Todo lo demas (US/Europe, otras regiones, desconocido) se agrupa como "sin
# liderazgo LatAm" y no se grafica. Se corrige un error previo: antes el
# porcentaje se calculaba sobre un n_papers ya sobreescrito y salia 100%/0%.
leadership_role_levels <- c("First author", "Corresponding author only")

assign_leadership_role <- function(data) {
  data |>
    mutate(
      leadership_role = case_when(
        first_author_region == "Latin America" ~ "First author",
        corresponding_region == "Latin America" ~ "Corresponding author only",
        TRUE ~ "No Latin American leadership"
      )
    )
}

summarise_leadership_role <- function(data, group_var) {
  data |>
    assign_leadership_role() |>
    group_by({{ group_var }}) |>
    summarise(
      n_papers = n(),
      n_first_author = sum(leadership_role == "First author"),
      n_corresponding_only = sum(leadership_role == "Corresponding author only"),
      .groups = "drop"
    ) |>
    mutate(
      n_latam_leadership = n_first_author + n_corresponding_only,
      percent_first_author = 100 * n_first_author / n_papers,
      percent_corresponding_only = 100 * n_corresponding_only / n_papers,
      percent_latam_leadership = 100 * n_latam_leadership / n_papers
    )
}

# 1995-2009 se agrupa por el mismo motivo que en Figure 5.
figure6_time_summary <- leadership_paper |>
  mutate(
    period_5year = fct_collapse(
      factor(period_5year, ordered = FALSE),
      "1995-2009" = c("1995-99", "2000-04", "2005-09")
    ) |>
      fct_relevel("1995-2009")
  ) |>
  summarise_leadership_role(period_5year) |>
  mutate(
    period_label = str_c(period_5year, "\n(n = ", n_papers, ")"),
    period_label = fct_inorder(period_label)
  )

figure6_discipline_summary <- leadership_by_discipline |>
  summarise_leadership_role(core_area) |>
  mutate(
    discipline_label = str_c(core_area, " (n = ", n_papers, ")"),
    discipline_label = fct_reorder(discipline_label, percent_latam_leadership)
  )

# Datos exactos de la figura, un registro por periodo y por disciplina.
figure6_plot_data <- bind_rows(
  figure6_time_summary |>
    transmute(panel = "Over time", group = as.character(period_5year), n_papers,
              n_first_author, n_corresponding_only, n_latam_leadership,
              percent_first_author, percent_corresponding_only,
              percent_latam_leadership),
  figure6_discipline_summary |>
    transmute(panel = "By discipline", group = core_area, n_papers,
              n_first_author, n_corresponding_only, n_latam_leadership,
              percent_first_author, percent_corresponding_only,
              percent_latam_leadership)
)

figure6_discipline_note <- str_c(
  "Latin American leadership vs. all other leadership combined (US/Europe, ",
  "other regions and unknown). Mutually exclusive roles with first-author ",
  "priority: First author = first author has a Latin American affiliation; ",
  "Corresponding author only = first author is not Latin American but the ",
  "corresponding author is; Total = First author + Corresponding author only. ",
  "Disciplines are not mutually exclusive, so a paper counts in each of its ",
  "disciplines. Cells show n (% of the discipline's papers); all years through ",
  last_month_2026, " 2026."
)

table_figure6_by_discipline <- figure6_discipline_summary |>
  arrange(desc(n_papers)) |>
  transmute(
    `Core discipline` = core_area,
    `Papers in discipline` = n_papers,
    `First author` = str_c(n_first_author, " (", round(percent_first_author, 1), "%)"),
    `Corresponding author only` = str_c(
      n_corresponding_only, " (", round(percent_corresponding_only, 1), "%)"
    ),
    `Total Latin American leadership` = str_c(
      n_latam_leadership, " (", round(percent_latam_leadership, 1), "%)"
    )
  ) |>
  bind_rows(tibble(`Core discipline` = str_c("Note: ", figure6_discipline_note)))

leadership_colors <- c(
  "First author" = wes_palette("Zissou1", 5L)[[1]],
  "Corresponding author only" = wes_palette("Zissou1", 5L)[[4]]
)

role_long <- function(data) {
  data |>
    pivot_longer(
      c(percent_first_author, percent_corresponding_only),
      names_to = "leadership_role",
      values_to = "percent"
    ) |>
    mutate(
      leadership_role = recode(
        leadership_role,
        percent_first_author = "First author",
        percent_corresponding_only = "Corresponding author only"
      ),
      leadership_role = factor(leadership_role, levels = leadership_role_levels)
    )
}

figure6_caption <- str_wrap(
  str_c(
    "Latin American leadership vs. all other leadership combined. A paper is ",
    "counted once: as First author if the first author has a Latin American ",
    "affiliation; otherwise as Corresponding author only if the corresponding ",
    "author does. The two series add up to the combined first-or-corresponding ",
    "share. Disciplines overlap, so a paper counts in each of its disciplines. ",
    "2025-26 includes records through ", last_month_2026, " 2026."
  ),
  width = 150
)

figure6_time <- role_long(figure6_time_summary) |>
  ggplot(aes(period_label, percent, color = leadership_role, group = leadership_role)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.4) +
  geom_text(
    aes(label = str_c(round(percent), "%")),
    vjust = -1, size = 3, show.legend = FALSE
  ) +
  scale_color_manual(values = leadership_colors, name = "Latin American leadership") +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 25),
    labels = scales::label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Latin American leadership of Latin America-affiliated experimental papers",
    subtitle = "Over time: share of papers whose first author (or, if not, corresponding author) is Latin American",
    x = NULL, y = "Share of papers in period"
  ) +
  theme_paper +
  theme(
    panel.grid.major.x = element_line(color = "grey92"),
    legend.position = "bottom"
  )

# Por disciplina solo se muestra el total (first + corresponding only); el
# detalle first/corresponding va en la tabla Table_Figure6_by_discipline.
figure6_disc <- figure6_discipline_summary |>
  ggplot(aes(percent_latam_leadership, discipline_label)) +
  geom_col(
    width = 0.6, alpha = 0.7,
    fill = wes_palette("Zissou1", 5L)[[2]]
  ) +
  geom_text(
    aes(label = str_c(round(percent_latam_leadership), "%")),
    hjust = -0.2, size = 3
  ) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 25),
    labels = scales::label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    subtitle = "By discipline (all years): first author or corresponding author only, combined",
    x = "Share of papers in discipline with Latin American leadership",
    y = NULL,
    caption = figure6_caption
  ) +
  theme_paper +
  theme(
    panel.grid.major.x = element_line(color = "grey92"),
    panel.grid.major.y = element_blank()
  )

figure6 <- figure6_time / figure6_disc + plot_layout(heights = c(1, 1.1))

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
  "Figure 5", figure5_origin_note,
  "Figure 5 by discipline", figure5_discipline_note,
  "Figure 6", "Latin American leadership is assigned when the first or corresponding author has a Latin American affiliation; all other leadership (US/Europe, other, unknown) is grouped.",
  "Figure 6 by discipline", figure6_discipline_note,
  "Figure 6", "Mutually exclusive roles: First author takes priority; Corresponding author only counts papers whose first author is not Latin American but whose corresponding author is. The two shares add up to combined Latin American leadership.",
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
  "Figure 5", "Table_Figure5_origin", "Origin of papers (mutually exclusive); totals with a note on the classification criterion in Notes.",
  "Figure 5 by discipline", "Table_Fig5_by_discipline", "Origin of papers by core discipline, totals over time; disciplines overlap.",
  "Figure 6 by discipline", "Table_Fig6_by_discipline", "Latin American leadership by core discipline: first author, corresponding author only and total; disciplines overlap.",
  "Leadership", "Leadership_combined", "Combined first- or corresponding-author leadership classification.",
  "Leadership detail", "Leadership_separate", "First and corresponding authors reported separately.",
  "ESCI audit", "ESCI_audit", "Annual identification of records indexed in ESCI.",
  "Topic labels", "Topic_dictionary", "Substantive label assigned to each topic.",
  "Method notes", "Notes", "Definitions and non-mutual-exclusivity notes.",
  "Figure 4", "outputs/figures/figure4_methods_over_time.png", "Annual papers by experimental method.",
  "Figure 5", "outputs/figures/figure5_origin_over_time.png", "Origin of papers by five-year period (100% stacked bars; n per period above bars).",
  "Figure 5 by discipline", "outputs/figures/figure5_origin_by_discipline.png", "Origin of papers as a share of each discipline's total (100% stacked bars; disciplines overlap).",
  "Figure 6", "outputs/figures/figure6_leadership.png", "Latin American leadership over time (first author vs. corresponding author only, mutually exclusive) and total share by discipline.",
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
    Table_Figure5_origin = table_figure5_origin,
    Figure5_plot_data = figure5_plot_data,
    Table_Fig5_by_discipline = table_figure5_by_discipline,
    Leadership_combined = leadership_combined,
    Leadership_separate = leadership_separate,
    Figure6_plot_data = figure6_plot_data,
    Table_Fig6_by_discipline = table_figure6_by_discipline,
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
  figure5, "figure5_origin_over_time",
  width = 11, height = 7.5
)
save_publication_figure(
  figure5_by_discipline, "figure5_origin_by_discipline",
  width = 11, height = 6.5
)
save_publication_figure(
  figure6, "figure6_leadership",
  width = 11, height = 9
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