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

# En Windows, las carpetas de denominadores contienen "año". Este ajuste hace
# que R pueda listar esas rutas con tildes y eñes de manera consistente.
if (!l10n_info()$`UTF-8`) {
  invisible(if (.Platform$OS.type == "windows") {
    Sys.setlocale("LC_CTYPE", "Spanish_Chile.utf8")
  } else {
    Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
  })
}

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

analysis_first_year <- 1995L
analysis_last_year <- 2025L
expected_analysis_papers <- 611L
# La base oficial conserva 675 papers hasta julio de 2026. Todos los outputs
# principales usan 1995-2025: no hay registros antes de 1995 y 2026 es parcial.

five_year_levels <- c(
  "1995-99", "2000-04", "2005-09",
  "2010-14", "2015-19", "2020-24", "2025"
)
decade_levels <- c("1995-99", "2000-09", "2010-19", "2020-25")

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
  # Convierte un ano en bloques de cinco anos desde 1995.
  start <- analysis_first_year +
    5L * floor((as.integer(year) - analysis_first_year) / 5L)
  label <- str_c(start, "-", str_sub(as.character(start + 4L), -2L))
  if_else(label == "2025-29", "2025", label)
}

figure_period <- function(period) {
  # Figuras 5 y 6 agrupan 1995-2009 porque hay pocos papers en esos periodos.
  factor(period, ordered = FALSE) |>
    fct_collapse("1995-2009" = c("1995-99", "2000-04", "2005-09")) |>
    fct_relevel("1995-2009")
}

decade_period <- function(year) {
  # Los extremos reflejan la cobertura real 1995-2025.
  case_when(
    year < 2000L ~ "1995-99",
    year < 2010L ~ "2000-09",
    year < 2020L ~ "2010-19",
    TRUE ~ "2020-25"
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
  "affi_latam_countries", "des_exp", "AB", "keywords_analisis"
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
topic_manual_review <- read_excel(topic_path, sheet = "Topic_manual_review")
topic_distant_pairs <- read_excel(topic_path, sheet = "Topic_distant_pairs")

recommended_k <- topic_summary |>
  filter(measure == "Recommended k") |>
  pull(value) |>
  as.integer()

if (length(recommended_k) != 1L || is.na(recommended_k) || recommended_k < 2L) {
  stop(
    "06 no informa un unico k recomendado valido. ",
    "Revise el topic model antes de continuar."
  )
}

if (
  n_distinct(topic_gamma$UT) != expected_analysis_papers ||
    nrow(topic_gamma) != expected_analysis_papers * recommended_k ||
    n_distinct(paper_dominant$UT) != expected_analysis_papers
) {
  stop("Los resultados de 06 no cubren los 611 papers de 1995-2025.")
}

papers <- base_oficial |>
  transmute(
    UT,
    year = as.integer(PY),
    title = TI,
    abstract = AB,
    keywords = keywords_analisis,
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
  ) |>
  filter(between(year, analysis_first_year, analysis_last_year))

if (
  nrow(papers) != expected_analysis_papers ||
    any(is.na(papers$year)) ||
    anyDuplicated(papers$UT) > 0L
) {
  stop("El universo analitico debe contener 611 UT unicas de 1995-2025.")
}

core_area_long <- papers |>
  select(UT, year, period_5year, core_area) |>
  separate_rows(core_area, sep = "\\s*;\\s*|\\s*\\|\\s*") |>
  mutate(core_area = str_squish(core_area)) |>
  filter(!is.na(core_area), core_area != "") |>
  distinct(UT, core_area, .keep_all = TRUE)

# -----------------------------------------------------------------------------
# 3. Table 2: produccion experimental y total por region, 1995-2025
# -----------------------------------------------------------------------------
# Los conteos regionales provienen de los TXT de WoS usados en Figure 2.
# Las regiones no son mutuamente excluyentes: un paper con afiliaciones en mas
# de una region puede estar presente en mas de una fila.

raw_dir <- here("Paper_wos", "data", "raw")
denominator_dir <- file.path(raw_dir, "totales denominador")
region_dir <- list.dirs(denominator_dir, recursive = FALSE) |>
  keep(~ str_detect(basename(.x), "region"))

if (length(region_dir) != 1L) {
  stop("Debe existir una unica carpeta de denominadores por region.")
}

read_wos_year_counts <- function(path) {
  # Lee las exportaciones Analyze Results de WoS y conserva ano y conteo.
  lines <- read_lines(path)
  lines <- lines[str_detect(lines, "^\\d{4}\\t\\d+")]
  tibble(line = lines) |>
    separate(line, into = c("year", "n"), sep = "\\t", extra = "drop") |>
    transmute(year = as.integer(year), n = as.numeric(n))
}

regional_files <- tribble(
  ~region, ~experimental_file, ~total_file,
  "Latin America",
  file.path(raw_dir, "experimental", "latam_exp.txt"),
  file.path(region_dir, "latam_gral.txt"),
  "United States",
  file.path(raw_dir, "experimental", "usa_exp.txt"),
  file.path(region_dir, "usa_gral.txt"),
  "Europe",
  file.path(raw_dir, "experimental", "europa_exp.txt"),
  file.path(region_dir, "europa_gral.txt")
)

table2_regions <- regional_files |>
  mutate(
    experimental = map(experimental_file, read_wos_year_counts),
    total = map(total_file, read_wos_year_counts)
  ) |>
  transmute(
    region,
    experimental_n = map_dbl(
      experimental,
      ~ sum(.x$n[between(.x$year, analysis_first_year, analysis_last_year)])
    ),
    total_social_science_n = map_dbl(
      total,
      ~ sum(.x$n[between(.x$year, analysis_first_year, analysis_last_year)])
    )
  ) |>
  arrange(match(region, c("Latin America", "United States", "Europe")))

# -----------------------------------------------------------------------------
# 4. Table 3: produccion por disciplina y periodos de cinco anos
# -----------------------------------------------------------------------------
# Un paper con dos core areas cuenta una vez en cada una.

table3_long <- core_area_long |>
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

table3_disciplines <- table3_long |>
  mutate(period_5year = as.character(period_5year)) |>
  pivot_wider(names_from = period_5year, values_from = n_papers) |>
  mutate(
    Total = rowSums(across(all_of(five_year_levels))),
    Percent_of_papers = round(100 * Total / expected_analysis_papers, 1)
  ) |>
  arrange(desc(Total), core_area)
# El porcentaje usa como denominador los 611 papers analiticos. Como un paper
# puede pertenecer a mas de una disciplina, los porcentajes no suman 100%.

# -----------------------------------------------------------------------------
# 5. Table 4: menciones de paises LatAm por periodo
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

if (n_distinct(country_mentions$UT) != expected_analysis_papers) {
  stop("No todos los papers 1995-2025 conservan una mencion de pais LatAm.")
}

table4_countries <- country_mentions |>
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
  mutate(
    Total = rowSums(across(all_of(decade_levels))),
    Percent_of_papers = 100 * Total / expected_analysis_papers
  ) |>
  arrange(desc(Total), country)

# -----------------------------------------------------------------------------
# 6. Table 5 and Figure 4: familias de diseno experimental
# -----------------------------------------------------------------------------
# Los metodos detectados en 00 se agrupan en cuatro familias definidas antes del
# analisis. Un paper puede pertenecer a mas de una familia. Other identifica solo
# papers sin ninguna familia mapeada y se exporta con UT para revision.

method_family_map <- tribble(
  ~experimental_method, ~method_family,
  "Survey experiment", "Survey-based experiments",
  "Vignette experiment/survey", "Survey-based experiments",
  "Factorial experiment/survey", "Survey-based experiments",
  "List experiment", "Survey-based experiments",
  "Online experiment", "Survey-based experiments",
  "Field experiment", "Field/lab interventions",
  "Lab-in-the-field experiment", "Field/lab interventions",
  "Laboratory experiment", "Field/lab interventions",
  "Experimental game", "Field/lab interventions",
  "Controlled experiment", "Field/lab interventions",
  "Randomized field intervention", "Field/lab interventions",
  "Conjoint", "Conjoint/choice designs",
  "Choice experiment", "Conjoint/choice designs",
  "Audit study", "Audit/correspondence designs",
  "Correspondence experiment/audit", "Audit/correspondence designs",
  "Resume/CV audit", "Audit/correspondence designs"
)

substantive_family_levels <- c(
  "Survey-based experiments",
  "Field/lab interventions",
  "Conjoint/choice designs",
  "Audit/correspondence designs"
)
method_family_levels <- c(substantive_family_levels, "Other/not classified")

methods_raw <- papers |>
  select(UT, year, period_5year, experimental_method) |>
  mutate(experimental_method = replace_na(experimental_method, "")) |>
  separate_rows(experimental_method, sep = "\\s*;\\s*") |>
  mutate(experimental_method = str_squish(experimental_method)) |>
  distinct(UT, experimental_method, .keep_all = TRUE)

methods_mapped <- methods_raw |>
  inner_join(method_family_map, by = "experimental_method") |>
  distinct(UT, year, period_5year, method_family)

method_families <- bind_rows(
  methods_mapped,
  papers |>
    anti_join(methods_mapped |> distinct(UT), by = "UT") |>
    transmute(
      UT, year, period_5year,
      method_family = "Other/not classified"
    )
) |>
  mutate(
    method_family = factor(method_family, levels = method_family_levels)
  )

if (n_distinct(method_families$UT) != expected_analysis_papers) {
  stop("Table 5 debe clasificar los 611 papers en una familia u Other.")
}

other_method_audit <- method_families |>
  filter(method_family == "Other/not classified") |>
  select(UT) |>
  left_join(
    papers |>
      select(
        UT, year, title, abstract, keywords, journal,
        experimental_method
      ),
    by = "UT"
  ) |>
  arrange(year, UT) |>
  mutate(
    eligible_experimental = NA_character_,
    specific_method = NA_character_,
    method_family = NA_character_,
    evidence = NA_character_,
    review_notes = NA_character_
  )

discipline_sizes <- core_area_long |>
  group_by(core_area) |>
  summarise(n_papers_in_discipline = n_distinct(UT), .groups = "drop")

table5_method_families <- method_families |>
  inner_join(
    core_area_long |> select(UT, core_area),
    by = "UT",
    relationship = "many-to-many"
  ) |>
  distinct(UT, core_area, method_family) |>
  count(core_area, method_family, name = "n_papers") |>
  complete(
    core_area,
    method_family = factor(
      method_family_levels,
      levels = method_family_levels
    ),
    fill = list(n_papers = 0L)
  ) |>
  left_join(discipline_sizes, by = "core_area") |>
  mutate(
    percent_within_discipline =
      100 * n_papers / n_papers_in_discipline
  ) |>
  arrange(core_area, method_family)

method_year <- method_families |>
  filter(method_family %in% substantive_family_levels) |>
  mutate(method_family = droplevels(method_family)) |>
  count(year, method_family, name = "n_papers") |>
  complete(
    year = analysis_first_year:analysis_last_year,
    method_family = factor(
      substantive_family_levels,
      levels = substantive_family_levels
    ),
    fill = list(n_papers = 0L)
  )

method_colors <- set_names(
  wes_palette("Zissou1", length(substantive_family_levels), type = "continuous"),
  substantive_family_levels
)

figure4 <- ggplot(
  method_year,
  aes(year, n_papers, color = method_family)
) +
  geom_line(linewidth = 0.8, show.legend = FALSE) +
  geom_point(size = 1.3, show.legend = FALSE) +
  facet_wrap(
    ~ method_family,
    ncol = 2,
    scales = "fixed",
    labeller = label_wrap_gen(width = 28)
  ) +
  scale_color_manual(values = method_colors, drop = FALSE) +
  scale_x_continuous(breaks = seq(1995, 2025, 5)) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(n = 4),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "Experimental design families over time",
    subtitle = "Annual number of Latin America-affiliated papers, 1995-2025",
    x = NULL,
    y = "Number of papers",
    caption = paste(
      "Design families are not mutually exclusive.",
      "Papers without a mapped family are tracked as Other in Table 5",
      "and are not shown in this figure."
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

# Misma familia de paletas que Figure 4 (Zissou1), en tonos suaves.
origin_colors <- setNames(
  wes_palette("Zissou1", length(origin_levels), type = "continuous"),
  origin_levels
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
  ) |>
  semi_join(papers |> select(UT), by = "UT")

if (
  nrow(paper_origin) != expected_analysis_papers ||
    !all(paper_origin$has_latam)
) {
  stop("Figure 5: los 611 papers deben tener al menos un autor LatAm.")
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
# supera 611 (mismo criterio de conteo no excluyente que Table 3).
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
  "it belongs to, so discipline totals add to more than ", nrow(paper_origin), ". ",
  "Cells show n (% of the discipline's papers); totals cover 1995-2025."
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
        " 1995-2025."
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
  # 1995-2009 se agrupa: tiene pocos papers repartidos en tres periodos.
  mutate(period_5year = figure_period(period_5year)) |>
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
    subtitle = "Share of Latin America-affiliated papers by author affiliations; 1995-2009 pooled, then five-year periods through 2025",
    x = NULL,
    y = "Share of papers in period",
    caption = str_wrap(
      str_c(
        figure5_origin_note, " 1995-2025.",
        " 2025 is a single year."
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
# 7. Table 6: topics principales por disciplina
# -----------------------------------------------------------------------------
# No se asignan nombres sustantivos en esta etapa. Cada topic se identifica por
# su numero y sus seis terminos con mayor beta. La interpretacion final requiere
# lectura manual de los papers exportados por 06.

topic_words <- topic_terms |>
  group_by(topic) |>
  slice_max(beta, n = 6L, with_ties = FALSE) |>
  arrange(desc(beta), .by_group = TRUE) |>
  summarise(
    top_terms = str_c(term, collapse = ", "),
    .groups = "drop"
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
  left_join(topic_words, by = "topic") |>
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
  left_join(topic_words, by = "topic") |>
  group_by(core_area) |>
  arrange(desc(value_percent), topic, .by_group = TRUE) |>
  mutate(
    topic_rank = row_number(),
    approach = "Dominant topic"
  ) |>
  ungroup()

table6_all_topics <- bind_rows(
  topic_core_gamma |>
    transmute(
      approach, core_area, topic_rank, topic, top_terms,
      value_percent, n_papers_in_area, expected_papers,
      n_dominant_papers = NA_integer_
    ),
  topic_core_dominant |>
    transmute(
      approach, core_area, topic_rank, topic, top_terms,
      value_percent, n_papers_in_area,
      expected_papers = NA_real_, n_dominant_papers
    )
) |>
  arrange(approach, core_area, topic_rank)

table6_top3_topics <- table6_all_topics |>
  filter(topic_rank <= 3L)

# Esta tabla pone ambas medidas en una misma fila. Se conserva completa en el
# Excel; el grafico usa solo los tres topics con mayor gamma por disciplina.
topics_discipline_plot <- topic_core_gamma |>
  transmute(
    core_area, topic, top_terms,
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
    topic_display = str_c("Topic ", topic)
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
# 7. Figure 6: afiliacion del primer autor (corresponsal en suplementario)
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

# El objeto papers ya esta restringido a 1995-2025.
leadership_paper <- papers |>
  select(UT, year, period_5year, corresponding_address) |>
  left_join(first_author_region, by = "UT") |>
  left_join(corresponding_region, by = "UT") |>
  mutate(
    across(
      c(first_author_region, corresponding_region),
      ~ replace_na(.x, "Other/unknown")
    )
  )

leadership_by_discipline <- leadership_paper |>
  inner_join(
    core_area_long |> select(UT, core_area),
    by = "UT",
    relationship = "many-to-many"
  )

# Detalle por medida (primer autor y corresponsal por separado). Solo el primer
# autor entra en el analisis principal; el corresponsal queda para el
# material suplementario.
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

# Figure 6: afiliacion del primer autor (Latin America / US-Europe / otro o
# desconocido), por disciplina (todos los anos) y total en el tiempo.
# Categorias mutuamente excluyentes; LatAm tiene prioridad si el primer autor
# declara afiliaciones en varias regiones (ver classify_leadership).
first_author_levels <- c("Latin America", "US/Europe", "Other/unknown")

summarise_first_author <- function(data, group_var) {
  data |>
    mutate(
      first_author_region = factor(first_author_region, levels = first_author_levels)
    ) |>
    count({{ group_var }}, first_author_region, name = "n_papers", .drop = FALSE) |>
    group_by({{ group_var }}) |>
    mutate(
      n_group = sum(n_papers),
      percent = 100 * n_papers / n_group
    ) |>
    ungroup() |>
    filter(n_group > 0L)
}

figure6_time_summary <- leadership_paper |>
  mutate(period_5year = figure_period(period_5year)) |>
  summarise_first_author(period_5year) |>
  mutate(period_label = fct_inorder(str_c(period_5year, "\n(n = ", n_group, ")")))

figure6_discipline_summary <- leadership_by_discipline |>
  summarise_first_author(core_area) |>
  group_by(core_area) |>
  mutate(
    discipline_label = str_c(core_area, " (n = ", n_group, ")"),
    latam_percent = percent[first_author_region == "Latin America"][1]
  ) |>
  ungroup() |>
  mutate(discipline_label = fct_reorder(discipline_label, latam_percent))

# Datos exactos de la figura, un registro por periodo y por disciplina.
figure6_plot_data <- bind_rows(
  figure6_time_summary |>
    transmute(panel = "Over time (total)", group = as.character(period_5year),
              first_author_region, n_papers, n_group, percent),
  figure6_discipline_summary |>
    transmute(panel = "By discipline", group = core_area,
              first_author_region, n_papers, n_group, percent)
)

figure6_discipline_note <- str_c(
  "First-author affiliation, 1995-2025. ",
  "Mutually exclusive categories: Latin America (the first author has a Latin ",
  "American affiliation; priority if several regions are declared), US/Europe, ",
  "and Other/unknown. Disciplines are not mutually exclusive, so a paper counts ",
  "in each of its disciplines. Cells show n (% of the discipline's papers)."
)

table_figure6_by_discipline <- figure6_discipline_summary |>
  mutate(label = str_c(n_papers, " (", round(percent, 1), "%)")) |>
  select(core_area, n_group, first_author_region, label) |>
  pivot_wider(names_from = first_author_region, values_from = label) |>
  arrange(desc(n_group)) |>
  rename(`Core discipline` = core_area, `Papers in discipline` = n_group) |>
  bind_rows(tibble(`Core discipline` = str_c("Note: ", figure6_discipline_note)))

first_author_colors <- c(
  "Latin America" = wes_palette("Zissou1", 5L)[[1]],
  "US/Europe" = wes_palette("Zissou1", 5L)[[4]],
  "Other/unknown" = "grey70"
)

figure6_caption <- str_wrap(
  str_c(
    "First-author affiliation of Latin America-affiliated papers. Mutually ",
    "exclusive categories; Latin America takes priority if the first author ",
    "declares several regions. Disciplines overlap, so a paper counts in each of ",
    "its disciplines. The analysis covers 1995-2025; 2025 is a single year."
  ),
  width = 150
)

figure6_time <- ggplot(
  figure6_time_summary,
  aes(period_label, percent, fill = fct_rev(first_author_region))
) +
  geom_col(width = 0.75, color = "white", linewidth = 0.3, alpha = 0.7) +
  geom_text(
    aes(label = if_else(percent >= 5, str_c(round(percent), "%"), "")),
    position = position_stack(vjust = 0.5), size = 3
  ) +
  scale_fill_manual(values = first_author_colors, breaks = first_author_levels, name = "First author") +
  scale_y_continuous(
    breaks = seq(0, 100, 25),
    labels = scales::label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(
    title = "First-author affiliation of Latin America-affiliated experimental papers",
    subtitle = "Total over time: 1995-2009 pooled, then five-year periods through 2025",
    x = NULL, y = "Share of papers in period"
  ) +
  theme_paper

figure6_disc <- ggplot(
  figure6_discipline_summary,
  aes(percent, discipline_label, fill = fct_rev(first_author_region))
) +
  geom_col(width = 0.7, color = "white", linewidth = 0.3, alpha = 0.7) +
  geom_text(
    aes(label = if_else(percent >= 5, str_c(round(percent), "%"), "")),
    position = position_stack(vjust = 0.5), size = 3
  ) +
  scale_fill_manual(values = first_author_colors, breaks = first_author_levels, name = "First author") +
  scale_x_continuous(
    breaks = seq(0, 100, 25),
    labels = scales::label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(
    subtitle = "By discipline (1995-2025)",
    x = "Share of papers in discipline",
    y = NULL,
    caption = figure6_caption
  ) +
  theme_paper +
  theme(
    panel.grid.major.x = element_line(color = "grey92"),
    panel.grid.major.y = element_blank()
  )

# Misma leyenda en ambos paneles: se muestra solo en el inferior.
figure6 <- (figure6_time + theme(legend.position = "none")) / figure6_disc +
  plot_layout(heights = c(1, 1.1))

# Figure 6b: alternativa a Figure 6, % de papers con primer autor LatAm por ano,
# una linea por disciplina mas el total. Los puntos con menos de
# min_n_figure6b papers en el ano no se grafican (proporciones inestables);
# el tamano del punto indica n.
min_n_figure6b <- 5L

figure6b_data <- bind_rows(
  leadership_by_discipline |>
    mutate(group = core_area),
  leadership_paper |>
    mutate(group = "All papers")
) |>
  group_by(group, year) |>
  summarise(
    n_papers = n(),
    n_latam_first = sum(first_author_region == "Latin America"),
    percent_latam_first = 100 * n_latam_first / n_papers,
    .groups = "drop"
  ) |>
  mutate(shown = n_papers >= min_n_figure6b)

figure6b_levels <- c(
  "All papers",
  figure6b_data |>
    filter(group != "All papers") |>
    count(group, wt = n_papers, sort = TRUE) |>
    pull(group)
)

figure6b_colors <- c(
  "All papers" = "black",
  setNames(
    c(wes_palette("Darjeeling1", 5L), "#6B4C9A")[seq_len(length(figure6b_levels) - 1L)],
    figure6b_levels[-1L]
  )
)

figure6b_plot_data <- figure6b_data |>
  filter(shown) |>
  mutate(group = factor(group, levels = figure6b_levels))

figure6b <- ggplot(
  figure6b_plot_data,
  aes(year, percent_latam_first, color = group, group = group)
) +
  geom_line(
    aes(linewidth = group == "All papers"), show.legend = c(linewidth = FALSE)
  ) +
  geom_point(aes(size = n_papers), alpha = 0.85) +
  scale_linewidth_manual(values = c(`TRUE` = 1.4, `FALSE` = 0.7)) +
  scale_color_manual(values = figure6b_colors, name = NULL) +
  scale_x_continuous(breaks = seq(1995, 2025, 5)) +
  scale_size_continuous(range = c(1, 4), name = "Papers in year", breaks = c(5, 20, 50, 100)) +
  scale_y_continuous(
    limits = c(25, 100),
    breaks = seq(25, 100, 25),
    labels = scales::label_percent(scale = 1)
  ) +
  guides(color = guide_legend(nrow = 1, order = 1), size = guide_legend(nrow = 1, order = 2)) +
  labs(
    title = "Latin American first authorship in Latin America-affiliated experimental papers",
    subtitle = "Share of papers whose first author has a Latin American affiliation, by discipline and total",
    x = NULL,
    y = "Share of papers with a Latin American first author",
    caption = str_wrap(
      str_c(
        "Annual values, 1995-2025. Disciplines ",
        "overlap, so a paper counts in each of its disciplines. Years with fewer ",
        "than ", min_n_figure6b, " papers in a group are not shown. Point size shows the number of papers. ",
        "The y-axis starts at 25%."
      ),
      width = 150
    )
  ) +
  theme_paper +
  theme(
    panel.grid.major.x = element_line(color = "grey92"),
    legend.box = "vertical",
    legend.spacing.y = unit(0, "pt")
  )

# Supplementary comparison: first vs. corresponding author (Table A2). Se
# conserva para reportar la magnitud de la diferencia y justificar por que el
# autor corresponsal no entra en el analisis principal.
summarise_comparison <- function(data, group_var) {
  data |>
    mutate(
      corresponding_missing = is.na(corresponding_address) |
        str_squish(corresponding_address) == "",
      first_latam = first_author_region == "Latin America",
      corr_latam = corresponding_region == "Latin America"
    ) |>
    group_by({{ group_var }}) |>
    summarise(
      n_papers = n(),
      n_first_latam = sum(first_latam),
      pct_first_latam = round(100 * n_first_latam / n_papers, 1),
      n_corr_latam = sum(corr_latam),
      pct_corr_latam = round(100 * n_corr_latam / n_papers, 1),
      n_both_latam = sum(first_latam & corr_latam),
      n_first_only_latam = sum(first_latam & !corr_latam),
      n_corr_only_latam = sum(!first_latam & corr_latam),
      n_same_region = sum(first_author_region == corresponding_region),
      pct_same_region = round(100 * n_same_region / n_papers, 1),
      n_corr_address_missing = sum(corresponding_missing),
      n_corr_region_unknown = sum(corresponding_region == "Other/unknown"),
      .groups = "drop"
    )
}

table_a2_comparison <- bind_rows(
  leadership_paper |>
    mutate(group = "All papers") |>
    summarise_comparison(group) |>
    mutate(panel = "Total", .before = 1),
  leadership_paper |>
    mutate(group = as.character(figure_period(period_5year))) |>
    summarise_comparison(group) |>
    mutate(panel = "By period", .before = 1),
  leadership_by_discipline |>
    mutate(group = core_area) |>
    summarise_comparison(group) |>
    arrange(desc(n_papers)) |>
    mutate(panel = "By discipline", .before = 1)
)

# -----------------------------------------------------------------------------
# 8. Figure 7: terminos principales del topic model
# -----------------------------------------------------------------------------
# Los topics se muestran por numero y palabras; no se asignan nombres sustantivos.

topic_colors <- set_names(
  wes_palette("FantasticFox1", recommended_k, type = "continuous"),
  as.character(seq_len(recommended_k))
)

figure7_data <- topic_terms |>
  group_by(topic) |>
  slice_max(beta, n = 10L, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    topic_label = str_c("Topic ", topic),
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
  scale_x_continuous(
    labels = scales::label_percent(accuracy = 1),
    expand = expansion(mult = c(0, 0.03))
  ) +
  labs(
    title = "Topic-model terms in Latin America-affiliated papers",
    subtitle = str_c("Top terms from the k = ", recommended_k, " LDA model"),
    x = "Term probability within topic (beta, %)",
    y = NULL,
    caption = paste(
      "Topics remain numbered pending manual substantive review;",
      "multiword expressions are retained as single terms."
    )
  ) +
  theme_paper

# Beta y gamma responden preguntas distintas. La figura principal usa beta para
# mostrar que palabras caracterizan cada topic. Estos dos diagnosticos usan
# gamma: el primero resume cuanto pesa cada topic en todo el corpus y el segundo
# muestra cuan concentrada o mezclada es la asignacion dominante de los papers.
topic_prevalence <- topic_gamma |>
  group_by(topic) |>
  summarise(
    mean_gamma = mean(gamma),
    expected_papers = sum(gamma),
    .groups = "drop"
  ) |>
  left_join(topic_words, by = "topic") |>
  mutate(
    topic_label = factor(
      str_c("Topic ", topic),
      levels = str_c("Topic ", sort(unique(topic)))
    ),
    percent_gamma = 100 * mean_gamma
  )

figure_topic_prevalence <- ggplot(
  topic_prevalence,
  aes(topic_label, percent_gamma, fill = factor(topic))
) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(
    aes(label = str_c(round(expected_papers, 1), " expected papers")),
    vjust = -0.45,
    size = 3.2
  ) +
  scale_fill_manual(values = topic_colors) +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.14))
  ) +
  labs(
    title = "Overall topic prevalence",
    subtitle = "Average topic proportion across the 611 papers",
    x = NULL,
    y = "Mean topic proportion (gamma)",
    caption = paste(
      "Expected papers is the sum of gamma weights, not a hard assignment.",
      "The five topic shares sum to 100%."
    )
  ) +
  theme_paper

topic_assignment_strength <- paper_dominant |>
  transmute(
    UT,
    topic = dominant_topic,
    dominant_gamma = topic_probability,
    topic_label = factor(
      str_c("Topic ", dominant_topic),
      levels = str_c("Topic ", sort(unique(dominant_topic)))
    )
  )

figure_topic_assignment_strength <- ggplot(
  topic_assignment_strength,
  aes(topic_label, dominant_gamma, fill = factor(topic))
) +
  geom_violin(alpha = 0.55, color = NA, trim = TRUE, show.legend = FALSE) +
  geom_boxplot(
    width = 0.16,
    outlier.shape = NA,
    color = "grey25",
    fill = "white",
    linewidth = 0.45
  ) +
  scale_fill_manual(values = topic_colors) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(
    title = "Strength of each paper's dominant-topic assignment",
    subtitle = "Distribution of the largest gamma value within each paper",
    x = NULL,
    y = "Dominant topic proportion (gamma)",
    caption = paste(
      str_c(
        "Gamma values sum to 1 across the ", recommended_k,
        " topics for each paper."
      ),
      "Lower values indicate substantively mixed papers, not missing coverage."
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
  "All", "The official database contains 675 papers; the main analysis uses 611 papers published in 1995-2025. No eligible records occur before 1995 and 2026 is excluded because it is incomplete.",
  "Table 2", "Regional totals sum annual Web of Science counts over 1995-2025.",
  "Table 3", "Core disciplines are not mutually exclusive; a paper can contribute to more than one discipline.",
  "Table 4", "Country mentions are not mutually exclusive. Percentages use the 611 analytical papers as denominator and therefore do not sum to 100%.",
  "Table 5 / Figure 4", "Design families are not mutually exclusive. Other contains papers without a label mapped to the four predefined families and is retained for audit, but is not plotted in Figure 4.",
  "Table 6", "Average gamma is the primary topic measure; dominant-topic counts are a complementary simplification. Topics remain numbered pending manual substantive review.",
  "Figure 5", figure5_origin_note,
  "Figure 5 by discipline", figure5_discipline_note,
  "Figure 5 / Figure 6", "Restricted to 1995-2025.",
  "Figure 6", "Leadership is measured only by the first author's affiliation (Latin America, US/Europe, Other/unknown; Latin America takes priority if several regions are declared). Panels: total over time and by discipline.",
  "Figure 6 by discipline", figure6_discipline_note,
  "Table A2", "Supplementary comparison of first vs. corresponding author affiliation (n and %, agreement, missing corresponding-author address), total, by period and by discipline, to report the magnitude of the difference and justify excluding the corresponding author from the main analysis.",
  "ESCI", "WE identifies ESCI, SSCI, SCI-EXPANDED, and other WoS citation indexes."
)

# Plantilla breve para revision manual. Reune solo los papers que requieren una
# decision humana y no reemplaza las tablas del manuscrito ni la base oficial.
review_instructions <- tribble(
  ~section, ~instruction,
  "Topic review", "Review the selected papers within each numbered topic together.",
  "Topic review", "substantive_theme: summarize the paper's substantive subject in a short phrase.",
  "Topic review", "fits_topic: enter yes, no, or unclear according to whether the paper belongs with the other papers assigned to that topic.",
  "Topic review", "proposed_topic_name: propose a concise substantive name after reading the representative, borderline, and distant papers in that topic.",
  "Topic review", "For a distant pair, verify whether both papers still share a defensible substantive theme; explain any inconsistency in review_notes.",
  "Method review", "eligible_experimental: enter yes, no, or unclear after checking whether the paper reports an experiment rather than only mentioning experiments or using observational, theoretical, review, simulation, natural-experiment, or quasi-experimental evidence.",
  "Method review", "specific_method: enter every applicable design using the labels in the Method labels sheet; separate multiple labels with a semicolon.",
  "Method review", "method_family: enter every corresponding family using the Family map sheet; separate multiple families with a semicolon.",
  "Method review", "evidence: copy a short phrase and, when available, page or section showing manipulation, treatment, random assignment, experimental task, or design.",
  "Method review", "If eligible_experimental is no, leave method fields blank and explain the exclusion reason in review_notes. Do not infer a method from the title alone."
)

method_labels <- tibble(
  specific_method = c(
    "Survey experiment", "Vignette experiment/survey",
    "Factorial experiment/survey", "List experiment", "Online experiment",
    "Field experiment", "Lab-in-the-field experiment",
    "Laboratory experiment", "Experimental game", "Controlled experiment",
    "Randomized field intervention", "Conjoint", "Choice experiment",
    "Audit study", "Correspondence experiment/audit", "Resume/CV audit"
  )
)

# -----------------------------------------------------------------------------
# 10. Exportar un Excel separado por tabla o figura
# -----------------------------------------------------------------------------

dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

# Cada archivo corresponde a un output del manuscrito. Las hojas adicionales
# contienen datos del grafico o auditorias necesarias para reproducirlo.
write_xlsx(list(`Table 2` = table2_regions, Notes = filter(notes, output %in% c("All", "Table 2"))), file.path(tables_dir, "table2_regional_totals.xlsx"))
write_xlsx(list(`Table 3` = table3_disciplines, Notes = filter(notes, output %in% c("All", "Table 3"))), file.path(tables_dir, "table3_disciplines.xlsx"))
write_xlsx(list(`Table 4` = table4_countries, Notes = filter(notes, output %in% c("All", "Table 4"))), file.path(tables_dir, "table4_countries.xlsx"))
write_xlsx(
  list(
    `Table 5` = table5_method_families,
    `Other audit` = other_method_audit,
    `Family map` = method_family_map,
    Notes = filter(notes, output %in% c("All", "Table 5 / Figure 4"))
  ),
  file.path(tables_dir, "table5_method_families.xlsx")
)
write_xlsx(
  list(
    `Top 3` = table6_top3_topics,
    `All topics` = table6_all_topics,
    `Plot data` = topics_discipline_plot,
    `Manual review` = topic_manual_review,
    `Distant pairs` = topic_distant_pairs,
    Notes = filter(notes, output %in% c("All", "Table 6"))
  ),
  file.path(tables_dir, "table6_topics.xlsx")
)
write_xlsx(list(`Figure 4 data` = method_year, `Other audit` = other_method_audit, `Family map` = method_family_map), file.path(tables_dir, "figure4_data.xlsx"))
write_xlsx(list(`Figure 5 time` = figure5_plot_data, `Figure 5 discipline` = figure5_discipline_data, `Origin totals` = table_figure5_origin, Notes = filter(notes, str_detect(output, "All|Figure 5"))), file.path(tables_dir, "figure5_data.xlsx"))
write_xlsx(list(`Figure 6 data` = figure6_plot_data, `Figure 6 discipline` = table_figure6_by_discipline, `Figure 6b data` = figure6b_data, `Leadership detail` = leadership_separate, Notes = filter(notes, str_detect(output, "All|Figure 6"))), file.path(tables_dir, "figure6_data.xlsx"))
write_xlsx(
  list(
    `Figure 7 data` = figure7_data,
    `Topic prevalence` = topic_prevalence,
    `Assignment strength` = topic_assignment_strength,
    `Topics discipline` = topics_discipline_plot,
    Notes = filter(notes, output %in% c("All", "Table 6"))
  ),
  file.path(tables_dir, "figure7_data.xlsx")
)
write_xlsx(list(`Table A2` = table_a2_comparison), file.path(tables_dir, "tableA2_first_vs_corresponding.xlsx"))
write_xlsx(list(`ESCI audit` = esci_audit, Notes = filter(notes, output %in% c("All", "ESCI"))), file.path(tables_dir, "appendix_esci_audit.xlsx"))
write_xlsx(
  list(
    Instructions = review_instructions,
    `Topic review` = topic_manual_review,
    `Method review` = other_method_audit,
    `Method labels` = method_labels,
    `Family map` = method_family_map
  ),
  file.path(tables_dir, "revision_isi.xlsx")
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
  figure6b, "figure6b_first_author_by_discipline",
  width = 11, height = 7
)
save_publication_figure(
  figure7, "figure7_topic_terms",
  width = 13, height = 11
)
save_publication_figure(
  figure_topics_by_discipline, "figure_topics_by_discipline",
  width = 12, height = 10
)
save_publication_figure(
  figure_topic_prevalence, "figure7b_topic_prevalence",
  width = 9, height = 6
)
save_publication_figure(
  figure_topic_assignment_strength, "figure7c_topic_assignment_strength",
  width = 9, height = 6
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
    "Papers mapped to at least one design family",
    "Papers in Other / not classified",
    "ESCI papers",
    "First-author Other/unknown",
    "Corresponding-author Other/unknown"
  ),
  value = c(
    n_distinct(papers$UT),
    nrow(core_area_long),
    nrow(country_mentions),
    n_distinct(methods_mapped$UT),
    n_distinct(other_method_audit$UT),
    sum(str_detect(
      papers$wos_index,
      regex("EMERGING SOURCES", ignore_case = TRUE)
    )),
    sum(leadership_paper$first_author_region == "Other/unknown"),
    sum(leadership_paper$corresponding_region == "Other/unknown")
  )
)

cat("\nResultados del paper creados correctamente\n")
cat("Excels separados guardados en: ", tables_dir, "\n", sep = "")
print(diagnostics)
