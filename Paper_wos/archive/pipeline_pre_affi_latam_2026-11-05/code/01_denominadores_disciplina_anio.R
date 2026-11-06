#-----------------------------------------------
# 01 - Denominadores y tasas WoS por year
# Valentina Gonzalez Madariaga
# Agosto 2026
#-----------------------------------------------

library(tidyverse)
library(here)

# Leer correctamente carpetas con acentos
Sys.setlocale("LC_CTYPE", "Spanish_Chile.utf8")

# Fijar rutas de trabajo
raw_dir <- here("Paper_wos", "data", "raw")

# Cargar produccion general por disciplina, pais y year
carpetas_denominadores <- list.dirs(raw_dir, recursive = FALSE, full.names = TRUE) |>
  keep(~ str_detect(basename(.x), "pais x"))

archivos_denominadores <- tibble(carpeta = carpetas_denominadores) |>
  mutate(
    discipline = case_when(
      str_detect(basename(carpeta), "business") ~ "Business/Management",
      str_detect(basename(carpeta), "economics") ~ "Economics",
      str_detect(basename(carpeta), "other") ~ "Other",
      str_detect(basename(carpeta), "political") ~ "Political Science",
      str_detect(basename(carpeta), "psychology") ~ "Psychology",
      str_detect(basename(carpeta), "sociology") ~ "Sociology"
    ),
    file = map(carpeta, ~ list.files(.x, pattern = "\\.txt$", full.names = TRUE))
  ) |>
  unnest(file) |>
  mutate(country = str_remove(basename(file), "\\.txt$") |> str_squish())

denominadores_disciplina_year <- archivos_denominadores |>
  mutate(data = map(file, ~ read_tsv(.x, show_col_types = FALSE))) |>
  unnest(data) |>
  transmute(
    type = "general_output_by_discipline",
    discipline,
    country,
    year = as.integer(`Publication Years`),
    n = as.integer(`Record Count`)
  ) |>
  arrange(discipline, country, year) |>
  mutate(
    region = case_when(
      country == "USA" ~ "USA",
      country == "Europa" ~ "Europe",
      country %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
                     "Costa Rica", "Cuba", "Dominican Rep", "Ecuador",
                     "Guatemala", "Mexico", "Peru", "Uruguay", "Venezuela", "Brasil") ~ "Latin America",
      TRUE ~ "Other"
    ),
    overlap_note = "Puede haber overlap entre disciplinas: un paper puede aparecer en mas de una disciplina."
  )

# Cargar produccion experimental total de USA por year
usa_exp_dir <- list.dirs(raw_dir, recursive = FALSE, full.names = TRUE) |>
  keep(~ str_detect(basename(.x), "usa_exp"))

usa_exp_year <- list.files(usa_exp_dir, pattern = "\\.txt$", full.names = TRUE) |>
  read_tsv(show_col_types = FALSE) |>
  transmute(
    type = "usa_experimental_output",
    discipline = NA_character_,
    country = "USA",
    year = as.integer(`Final Publication Year`),
    n = as.integer(`Record Count`)
  ) |>
  arrange(year)

# Cargar produccion experimental endogena de America Latina por year
endogeno <- readRDS(here("Paper_wos", "data", "intermediate", "endogeno_inicial.rds"))
laboratorio <- readRDS(here("Paper_wos", "data", "intermediate", "laboratorio_inicial.rds"))

# Endogenous y Laboratory pueden compartir papers; sus conteos no son disjuntos.
universes_experimental_year <- bind_rows(
  endogeno |> transmute(UT, year = as.integer(PY), universe = "Endogenous"),
  laboratorio |> transmute(UT, year = as.integer(PY), universe = "Laboratory"),
  inner_join(
    endogeno |> distinct(UT, year = as.integer(PY)),
    laboratorio |> distinct(UT, year = as.integer(PY)),
    by = c("UT", "year")
  ) |> mutate(universe = "Shared")
) |>
  distinct(universe, UT, year) |>
  count(universe, year, name = "n") |>
  complete(universe = c("Endogenous", "Laboratory", "Shared"), year = 1990:2026, fill = list(n = 0)) |>
  mutate(overlap_note = "Endogenous y Laboratory pueden superponerse; Shared identifica papers presentes en ambos.") |>
  arrange(universe, year)
# Crear tabla anual solo para la produccion endogena
tab_year <- endogeno |>
  count(year = as.integer(PY), name = "n") |>
  complete(year = 1990:2026, fill = list(n = 0)) |>
  arrange(year)

# Contar produccion endogena por disciplina en 2025 para America Latina y Chile
# Un paper puede contar en mas de una disciplina, pero solo una vez dentro de cada disciplina.
discipline_2025 <- bind_rows(
  endogeno |>
    filter(as.integer(PY) == 2025) |>
    select(UT, core_area) |>
    separate_rows(core_area, sep = ";") |>
    mutate(region = "Latin America", core_area = str_squish(core_area)),
  endogeno |>
    filter(as.integer(PY) == 2025, str_detect(afiliacion_latam, "(^|;\\s*)Chile($|;)")) |>
    select(UT, core_area) |>
    separate_rows(core_area, sep = ";") |>
    mutate(region = "Chile", core_area = str_squish(core_area))
) |>
  filter(!is.na(core_area), core_area != "") |>
  distinct(region, UT, core_area) |>
  count(region, core_area, name = "n") |>
  complete(
    region = c("Latin America", "Chile"),
    core_area = c("Business/Management", "Economics", "Other", "Political Science", "Psychology", "Sociology"),
    fill = list(n = 0)
  ) |>
  arrange(region, desc(n))

latam_exp_year <- endogeno |>
  count(year = as.integer(PY), name = "n") |>
  transmute(
    type = "latam_endogenous_experimental_output",
    discipline = NA_character_,
    country = "Latin America",
    year,
    n
  ) |>
  arrange(year)

# Cargar produccion general total de USA por year
usa_general_year <- read_tsv(here("Paper_wos", "data", "raw", "gral", "usa_gral.txt"), show_col_types = FALSE) |>
  transmute(
    type = "usa_general_output",
    discipline = NA_character_,
    country = "USA",
    year = as.integer(`Publication Years`),
    n = as.integer(`Record Count`)
  ) |>
  arrange(year)

# Cargar produccion general total de America Latina por year
latam_general_year <- read_tsv(here("Paper_wos", "data", "raw", "gral", "latam_gral.txt"), show_col_types = FALSE) |>
  transmute(
    type = "latam_general_output",
    discipline = NA_character_,
    country = "Latin America",
    year = as.integer(`Publication Years`),
    n = as.integer(`Record Count`)
  ) |>
  arrange(year)

# Cargar produccion general total de Chile por year.
# Esta busqueda no filtra por palabras clave experimentales y entrega un
# denominador deduplicado, sin sumar categorias disciplinares superpuestas.
chile_general_year <- read_tsv(here("Paper_wos", "data", "raw", "gral", "chile_gral.txt"), show_col_types = FALSE) |>
  transmute(
    type = "chile_general_output",
    discipline = NA_character_,
    country = "Chile",
    year = as.integer(.data[["Publication Years"]]),
    n = as.integer(.data[["Record Count"]])
  ) |>
  arrange(year)

# Unir todos los conteos en una base long
conteos_wos_year <- bind_rows(
  denominadores_disciplina_year,
  usa_exp_year,
  latam_exp_year,
  usa_general_year,
  latam_general_year,
  chile_general_year
) |>
  arrange(type, discipline, country, year)

# Calcular tasas por pais latinoamericano con los denominadores por disciplina
# Las disciplinas WoS pueden solaparse; estas tasas son indicadores bibliometricos.
latam_total_country_year <- denominadores_disciplina_year |>
  filter(region == "Latin America") |>
  group_by(country, year) |>
  summarise(total_n = sum(n), .groups = "drop")

latam_exp_country_year <- endogeno |>
  select(UT, PY, afiliacion_latam) |>
  separate_rows(afiliacion_latam, sep = ";") |>
  mutate(country = str_squish(afiliacion_latam)) |>
  filter(!is.na(country), country != "") |>
  count(country, year = as.integer(PY), name = "experimental_n")

latam_rate_country_year <- latam_total_country_year |>
  left_join(latam_exp_country_year, by = c("country", "year")) |>
  mutate(
    experimental_n = replace_na(experimental_n, 0),
    rate_per_1000 = if_else(total_n > 0, experimental_n / total_n * 1000, NA_real_)
  ) |>
  arrange(country, year)

# Calcular tasas comparables de USA y America Latina con los nuevos denominadores agregados
usa_latam_rate_year <- bind_rows(
  usa_general_year |> select(country, year, total_n = n),
  latam_general_year |> select(country, year, total_n = n)
) |>
  left_join(
    bind_rows(
      usa_exp_year |> select(country, year, experimental_n = n),
      latam_exp_year |> select(country, year, experimental_n = n)
    ),
    by = c("country", "year")
  ) |>
  mutate(
    experimental_n = replace_na(experimental_n, 0),
    rate_per_1000 = if_else(total_n > 0, experimental_n / total_n * 1000, NA_real_)
  ) |>
  arrange(country, year)

# Calcular la tasa de Chile con el total general deduplicado.
chile_rate_year <- chile_general_year |>
  select(country, year, total_n = n) |>
  left_join(
    latam_exp_country_year |>
      filter(country == "Chile") |>
      select(country, year, experimental_n),
    by = c("country", "year")
  ) |>
  mutate(
    experimental_n = replace_na(experimental_n, 0),
    rate_per_1000 = if_else(total_n > 0, experimental_n / total_n * 1000, NA_real_)
  ) |>
  arrange(year)

# Crear tabla comparable de tasas para America Latina, Chile y USA.
rate_latam_chile_usa_year <- bind_rows(
  usa_latam_rate_year |>
    transmute(
      series = if_else(country == "Latin America", "Latin America (endogenous)", "USA"),
      year,
      total_n,
      experimental_n,
      rate_per_1000
    ),
  chile_rate_year |>
    transmute(
      series = "Chile (endogenous)",
      year,
      total_n,
      experimental_n,
      rate_per_1000
    )
) |>
  arrange(series, year)

# Identificar overlap entre los dos universos experimentales.
overlap_papers <- full_join(
  endogeno |> distinct(UT) |> mutate(in_endogenous = TRUE),
  laboratorio |> distinct(UT) |> mutate(in_laboratory = TRUE),
  by = "UT"
) |>
  mutate(
    in_endogenous = replace_na(in_endogenous, FALSE),
    in_laboratory = replace_na(in_laboratory, FALSE),
    universe_overlap = case_when(
      in_endogenous & in_laboratory ~ "Shared",
      in_endogenous ~ "Endogenous only",
      in_laboratory ~ "Laboratory only"
    ),
    overlap_note = "La pertenencia a Endogenous y Laboratory puede superponerse; no son universos disjuntos."
  )

# Guardar las bases necesarias para los graficos y tablas.
saveRDS(universes_experimental_year, here("Paper_wos", "data", "intermediate", "universes_experimental_year.rds"))
saveRDS(overlap_papers, here("Paper_wos", "data", "intermediate", "overlap_papers.rds"))
saveRDS(conteos_wos_year, here("Paper_wos", "data", "intermediate", "conteos_wos_year.rds"))
saveRDS(
  list(
    latam_country = latam_rate_country_year,
    usa_latam = usa_latam_rate_year,
    chile = chile_rate_year
  ),
  here("Paper_wos", "data", "intermediate", "tasas_wos_year.rds")
)

# Guardar tablas de resultados
write_csv(usa_latam_rate_year, here("Paper_wos", "outputs", "tables", "usa_latam_rate_year.csv"))
write_csv(latam_rate_country_year, here("Paper_wos", "outputs", "tables", "latam_country_rate_year.csv"))
write_csv(discipline_2025, here("Paper_wos", "outputs", "tables", "discipline_2025_latam_chile.csv"))
write_csv(rate_latam_chile_usa_year, here("Paper_wos", "outputs", "tables", "rate_latam_chile_usa_year.csv"))
write_csv(tab_year, here("Paper_wos", "outputs", "tables", "endogeno_year.csv"))
write_csv(universes_experimental_year, here("Paper_wos", "outputs", "tables", "universes_experimental_year.csv"))
write_csv(overlap_papers, here("Paper_wos", "outputs", "tables", "overlap_papers.csv"))

# Mostrar resumen basico en consola
print(count(conteos_wos_year, type, country))
print(usa_latam_rate_year)
print(tab_year)
print(discipline_2025)
