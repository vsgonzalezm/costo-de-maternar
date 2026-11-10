# -----------------------------------------------------------------------------
# 05 - Descriptivos bibliometricos del corpus con afiliacion latinoamericana
# -----------------------------------------------------------------------------
# Este script usa exclusivamente base_oficial.rds, creada por 00_inicial.R.
# El universo analitico contiene 675 papers con al menos una afiliacion LatAm.
# No reconstruye el universo ni vuelve a leer la revision manual.
#
# Output unico:
#   outputs/tables/descriptivos_affi_latam.xlsx
#
# La unidad de Authors_long es autor-paper: un paper con N autores genera N
# filas. Author_affiliations tiene una fila por direccion atribuible al autor.
# C3 se asigna a un autor solo cuando la revision manual conserva esa posicion;
# el C3 completo tambien se mantiene a nivel paper.

library(tidyverse)
library(here)
library(writexl)

# 1. Carga y controles del insumo --------------------------------------------
# Se exige la salida oficial de 00. Los controles evitan trabajar por error con
# una version anterior o con un universo distinto del acordado.
data_dir <- here("Paper_wos", "data")
tables_dir <- here("Paper_wos", "outputs", "tables")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

base_oficial <- readRDS(
  file.path(data_dir, "intermediate", "base_oficial.rds")
)

required_columns <- c(
  "UT", "TI", "PY", "SO", "DI", "AU", "AF", "RP", "C1", "C3",
  "WC", "core_area", "des_exp", "author_affiliation_map"
)
missing_columns <- setdiff(required_columns, names(base_oficial))

if (length(missing_columns) > 0L) {
  stop(
    "Faltan columnas producidas por 00: ",
    str_c(missing_columns, collapse = ", ")
  )
}

if (nrow(base_oficial) != 675L || n_distinct(base_oficial$UT) != 675L) {
  stop("base_oficial.rds debe contener 675 papers con UT unica.")
}

# 2. Funciones de apoyo -------------------------------------------------------
# Las claves normalizadas permiten comparar formas exactamente equivalentes.
# No constituyen una desambiguacion probabilistica de personas homonimas.
normalize_name <- function(x) {
  x |>
    iconv(from = "", to = "ASCII//TRANSLIT") |>
    str_to_upper() |>
    str_replace_all("[^A-Z0-9 ]", " ") |>
    str_squish()
}

collapse_unique <- function(x) {
  values <- x |>
    as.character() |>
    discard(~ is.na(.x) || str_squish(.x) == "") |>
    str_squish() |>
    unique() |>
    sort()

  if (length(values) == 0L) NA_character_ else str_c(values, collapse = "; ")
}

split_semicolon <- function(x) {
  if (is.na(x) || str_squish(x) == "") return(character())
  x |>
    str_split(";", simplify = FALSE) |>
    unlist() |>
    str_squish() |>
    discard(~ is.na(.x) || .x == "")
}

value_at_position <- function(x, position) {
  values <- split_semicolon(x)
  if (position > length(values)) NA_character_ else values[[position]]
}

latam_country_names <- c(
  "Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica",
  "Cuba", "Dominican Rep", "Ecuador", "El Salvador", "Guatemala", "Haiti",
  "Honduras", "Mexico", "Nicaragua", "Panama", "Paraguay", "Peru",
  "Puerto Rico", "Uruguay", "Venezuela"
)

# La lista contiene las formas WoS observadas. Si aparece una forma nueva, el
# script se detiene para que sea revisada en lugar de perderla silenciosamente.
standard_wos_countries <- c(
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
    country_raw = standard_wos_countries,
    country = str_to_title(str_to_lower(standard_wos_countries))
  ),
  tribble(
    ~country_raw, ~country,
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

# Algunas celdas C1 manuales contienen mas de una direccion concatenada, por
# ejemplo "..., CHILE. UCLOUVAIN ..., BELGIUM.". Esta funcion inserta un
# separador despues de cada pais y evita perder la primera afiliacion.
country_tokens <- c(country_lookup$country_raw, "USA") |>
  unique() |>
  sort(decreasing = TRUE)
country_boundary_pattern <- str_c(
  "(,\\s*(?:",
  str_c(country_tokens, collapse = "|"),
  ")\\.?)(?=\\s+[[:upper:]])"
)

split_c1_addresses <- function(x) {
  if (is.na(x) || str_squish(x) == "") return(character())

  x |>
    str_replace_all(";", "|||" ) |>
    str_replace_all(
      regex(country_boundary_pattern, ignore_case = TRUE),
      "\\1|||"
    ) |>
    str_split(fixed("|||"), simplify = FALSE) |>
    unlist() |>
    str_squish() |>
    discard(~ is.na(.x) || .x == "")
}
extract_country_raw <- function(address) {
  address |>
    str_remove("[.;[:space:]]+$") |>
    str_extract("[^,]+$") |>
    str_squish() |>
    str_to_upper()
}

normalize_country <- function(address) {
  raw_country <- extract_country_raw(address)
  lookup_position <- match(raw_country, country_lookup$country_raw)

  case_when(
    str_detect(raw_country, "(^|\\s)USA$") ~ "United States",
    !is.na(lookup_position) ~ country_lookup$country[lookup_position],
    TRUE ~ NA_character_
  )
}

five_year_period <- function(year) {
  start <- 1990L + 5L * floor((as.integer(year) - 1990L) / 5L)
  str_c(start, "-", start + 4L)
}

# 3. Base maestra de autores -------------------------------------------------
# author_affiliation_map fue construido por 00 a partir del orden de AF/AU y de
# las correcciones c1_autN_manual. Al desanidarlo se obtiene exactamente una
# fila por autor y paper.
authors <- base_oficial |>
  select(UT, AU, author_affiliation_map) |>
  unnest(author_affiliation_map) |>
  mutate(
    author_abbreviated = map2_chr(AU, author_position, value_at_position),
    author_name = na_if(str_squish(author_name), ""),
    author_name_key = normalize_name(author_name),
    c1_manual = na_if(str_squish(c1_manual), ""),
    c3_manual = na_if(str_squish(c3_manual), "")
  ) |>
  select(-AU)

expected_author_counts <- base_oficial |>
  transmute(
    UT,
    expected_authors = map_int(AU, ~ length(split_semicolon(.x)))
  )

author_count_audit <- expected_author_counts |>
  left_join(authors |> count(UT, name = "author_rows"), by = "UT") |>
  mutate(
    author_rows = replace_na(author_rows, 0L),
    count_matches = expected_authors == author_rows
  )

if (any(!author_count_audit$count_matches)) {
  stop("El numero de filas autor-paper no coincide con AU.")
}

if (any(is.na(authors$author_name) | authors$author_name == "")) {
  stop("Hay autores sin nombre en author_affiliation_map.")
}

# 4. Lectura de los vinculos originales de C1 -------------------------------
# Los TXT conservan los corchetes de WoS: [autor] direccion. Esos corchetes
# permiten atribuir una direccion a uno o varios autores sin usar el orden de
# las entradas. Solo se recuperan C1 y UT; el universo sigue definido por 00.
extract_field <- function(record, tag) {
  start <- which(str_detect(record, paste0("^", tag, " ")))[1]
  if (is.na(start)) return(character())

  next_tag <- which(
    seq_along(record) > start & str_detect(record, "^[A-Z0-9]{2} ")
  )
  end <- if (length(next_tag) == 0L) length(record) else min(next_tag) - 1L
  values <- record[start:end]
  values[1] <- str_remove(values[1], paste0("^", tag, " "))
  str_squish(values)
}

merge_c1_lines <- function(lines) {
  if (length(lines) == 0L) return(character())
  entries <- character()

  for (line in lines) {
    if (str_starts(line, fixed("[")) || length(entries) == 0L) {
      entries <- c(entries, line)
    } else {
      entries[length(entries)] <- str_c(entries[length(entries)], line, sep = " ")
    }
  }

  entries
}

parse_wos_record <- function(record) {
  ut <- first(extract_field(record, "UT"), default = NA_character_)
  entries <- merge_c1_lines(extract_field(record, "C1"))

  if (length(entries) == 0L) {
    return(tibble(
      UT = character(), c1_author_name = character(), address = character(),
      link_status = character()
    ))
  }

  map_dfr(entries, function(entry) {
    matched <- str_match(entry, "^\\[(.*?)\\]\\s*(.*)$")

    if (is.na(matched[1, 2])) {
      return(tibble(
        UT = ut,
        c1_author_name = NA_character_,
        address = entry,
        link_status = "no_author_link"
      ))
    }

    str_split(matched[1, 2], ";", simplify = FALSE)[[1]] |>
      str_squish() |>
      tibble(c1_author_name = _) |>
      mutate(
        UT = ut,
        address = matched[1, 3],
        link_status = "wos_author_link",
        .before = 1
      )
  })
}

parse_wos_file <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  record_id <- cumsum(str_detect(lines, "^PT "))
  records <- split(lines[record_id > 0L], record_id[record_id > 0L])
  map_dfr(records, parse_wos_record)
}

raw_files <- c(
  list.files(
    file.path(data_dir, "raw", "endogeno"),
    pattern = "\\.txt$", full.names = TRUE
  ),
  list.files(
    file.path(data_dir, "raw", "laboratorio"),
    pattern = "\\.txt$", full.names = TRUE
  )
)

if (length(raw_files) == 0L) {
  stop("No se encontraron TXT originales para recuperar los vinculos C1.")
}

raw_c1 <- raw_files |>
  map_dfr(parse_wos_file) |>
  filter(UT %in% base_oficial$UT) |>
  mutate(
    address = na_if(str_squish(address), ""),
    c1_author_key = normalize_name(c1_author_name)
  ) |>
  filter(!is.na(address)) |>
  distinct(UT, c1_author_name, address, link_status, .keep_all = TRUE)

# 5. Afiliaciones atribuibles a cada autor ----------------------------------
# Las correcciones manuales reemplazan el vinculo WoS de ese autor. Si WoS no
# identifica personas y el paper tiene un solo autor, la direccion puede
# asignarse sin ambiguedad. Los demas casos sin vinculo se detienen abajo.
duplicate_author_keys <- authors |>
  filter(!is.na(author_name_key), author_name_key != "") |>
  count(UT, author_name_key) |>
  filter(n > 1L)

unique_author_keys <- authors |>
  anti_join(duplicate_author_keys, by = c("UT", "author_name_key")) |>
  select(UT, author_position, author_name_key)

wos_author_affiliations <- raw_c1 |>
  filter(link_status == "wos_author_link", !is.na(c1_author_key)) |>
  inner_join(
    unique_author_keys,
    by = c("UT", "c1_author_key" = "author_name_key")
  ) |>
  transmute(
    UT, author_position, address,
    affiliation_source = "wos_c1_linked"
  ) |>
  distinct()

manual_author_affiliations <- authors |>
  filter(!is.na(c1_manual)) |>
  select(UT, author_position, c1_manual) |>
  mutate(address = map(c1_manual, split_c1_addresses)) |>
  select(-c1_manual) |>
  unnest_longer(address) |>
  mutate(
    address = na_if(str_squish(address), ""),
    affiliation_source = "manual_c1"
  ) |>
  filter(!is.na(address)) |>
  distinct()

manual_author_keys <- manual_author_affiliations |>
  distinct(UT, author_position)

author_affiliations <- wos_author_affiliations |>
  anti_join(manual_author_keys, by = c("UT", "author_position")) |>
  bind_rows(manual_author_affiliations)

raw_paper_addresses <- raw_c1 |>
  select(UT, address) |>
  distinct()

single_author_fallback <- authors |>
  count(UT, name = "n_authors") |>
  filter(n_authors == 1L) |>
  select(UT) |>
  inner_join(authors |> select(UT, author_position), by = "UT") |>
  anti_join(
    author_affiliations |> distinct(UT, author_position),
    by = c("UT", "author_position")
  ) |>
  inner_join(raw_paper_addresses, by = "UT") |>
  mutate(affiliation_source = "single_author_c1")

author_affiliations <- author_affiliations |>
  bind_rows(single_author_fallback) |>
  mutate(
    affiliation_unit = str_extract(address, "^[^,]+") |> str_squish(),
    country_raw = extract_country_raw(address),
    country = normalize_country(address)
  ) |>
  distinct(UT, author_position, address, .keep_all = TRUE)

# Casos documentados por Isidora como autores independientes sin afiliacion.
confirmed_no_affiliation <- tribble(
  ~UT, ~author_position, ~affiliation_note,
  "WOS:000324740600002", 1L, "Investigador independiente; sin institucion.",
  "WOS:000379962400003", 3L, "Sin afiliacion al momento de la publicacion.",
  "WOS:000632971700002", 5L, "Profesional independiente; sin institucion.",
  "WOS:000632971700002", 10L, "Investigador independiente; sin institucion.",
  "WOS:000796681800001", 1L, "Investigador independiente; sin institucion. Confirmado en chequeo manual de Tania (nov 2026).",
  "WOS:001491371500001", 19L, "Investigador independiente; sin institucion.",
  "WOS:001541599500001", 5L, "Investigador independiente; sin institucion."
) |>
  semi_join(authors, by = c("UT", "author_position"))

unresolved_authors <- authors |>
  anti_join(
    author_affiliations |> distinct(UT, author_position),
    by = c("UT", "author_position")
  ) |>
  anti_join(
    confirmed_no_affiliation,
    by = c("UT", "author_position")
  )

if (nrow(unresolved_authors) > 0L) {
  unresolved_ids <- unresolved_authors |>
    distinct(UT) |>
    slice_head(n = 10L) |>
    pull(UT) |>
    str_c(collapse = ", ")

  stop(
    "Persisten autores sin afiliacion atribuible. Primeros UT: ",
    unresolved_ids
  )
}

# 6. Paises a nivel paper y auditoria exhaustiva -----------------------------
# Se unen las direcciones originales y manuales. Cada direccion debe terminar
# en un pais reconocido. Un paper puede participar en varios paises.
paper_addresses <- base_oficial |>
  select(UT, C1) |>
  mutate(address = map(C1, split_c1_addresses)) |>
  select(-C1) |>
  unnest_longer(address) |>
  mutate(
    address = na_if(str_squish(address), ""),
    address_source = "corrected_paper_c1",
    country_raw = extract_country_raw(address),
    country = normalize_country(address)
  ) |>
  filter(!is.na(address)) |>
  distinct(UT, address, .keep_all = TRUE)

unmapped_addresses <- paper_addresses |>
  filter(is.na(country))

if (nrow(unmapped_addresses) > 0L) {
  unknown_values <- unmapped_addresses |>
    distinct(country_raw) |>
    slice_head(n = 10L) |>
    pull(country_raw) |>
    str_c(collapse = ", ")
  stop("Hay paises sin mapear en C1: ", unknown_values)
}

if (any(is.na(author_affiliations$country))) {
  stop("Hay afiliaciones de autor cuyo pais no pudo mapearse.")
}

paper_countries <- paper_addresses |>
  distinct(UT, country)

paper_country_summary <- paper_countries |>
  group_by(UT) |>
  summarise(
    n_countries = n_distinct(country),
    countries = collapse_unique(country),
    n_latam_countries = n_distinct(country[country %in% latam_country_names]),
    latam_countries = collapse_unique(country[country %in% latam_country_names]),
    has_non_latam = any(!country %in% latam_country_names),
    has_united_states = any(country == "United States"),
    .groups = "drop"
  )

papers_without_latam <- paper_country_summary |>
  filter(n_latam_countries == 0L)

if (nrow(paper_country_summary) != 675L || nrow(papers_without_latam) > 0L) {
  stop("El mapeo de paises no reproduce los 675 papers con afiliacion LatAm.")
}

# 7. Base larga autor-paper ---------------------------------------------------
# La hoja Authors_long mantiene una fila por autor y paper. Las afiliaciones se
# resumen solo cuando pudieron atribuirse a esa persona mediante C1.
author_affiliation_summary <- author_affiliations |>
  group_by(UT, author_position) |>
  summarise(
    n_author_affiliations = n_distinct(address),
    author_addresses = collapse_unique(address),
    author_affiliation_units = collapse_unique(affiliation_unit),
    author_countries = collapse_unique(country),
    affiliation_source = collapse_unique(affiliation_source),
    .groups = "drop"
  )

paper_variables <- base_oficial |>
  transmute(
    UT,
    year = as.integer(PY),
    title = TI,
    journal = SO,
    doi = DI,
    core_area,
    experimental_method = des_exp,
    paper_C3 = C3
  )

authors_long <- authors |>
  left_join(author_affiliation_summary, by = c("UT", "author_position")) |>
  left_join(confirmed_no_affiliation, by = c("UT", "author_position")) |>
  inner_join(paper_variables, by = "UT") |>
  mutate(
    n_author_affiliations = replace_na(n_author_affiliations, 0L),
    author_institutions = coalesce(c3_manual, author_affiliation_units),
    institution_source = case_when(
      !is.na(c3_manual) ~ "manual_c3",
      !is.na(author_affiliation_units) ~ "derived_from_c1_address",
      !is.na(affiliation_note) ~ "confirmed_no_affiliation",
      TRUE ~ "missing"
    ),
    affiliation_status = case_when(
      !is.na(affiliation_note) ~ "confirmed_no_affiliation",
      affiliation_source == "manual_c1" ~ "manual_c1",
      !is.na(affiliation_source) ~ affiliation_source,
      TRUE ~ "unresolved"
    )
  ) |>
  arrange(year, UT, author_position)

if (nrow(authors_long) != sum(expected_author_counts$expected_authors)) {
  stop("Authors_long no contiene exactamente una fila por autor-paper.")
}

# Una fila por direccion atribuible. Esta hoja permite observar autores con dos
# o mas afiliaciones sin duplicarlos en Authors_long.
author_affiliation_detail <- author_affiliations |>
  left_join(
    authors |>
      select(
        UT, author_position, author_name_key, author_name,
        normalized_institutions = c3_manual
      ),
    by = c("UT", "author_position")
  ) |>
  left_join(
    paper_variables |> select(UT, year, title, journal),
    by = "UT"
  ) |>
  select(
    UT, year, title, journal, author_position, author_name_key, author_name,
    normalized_institutions, affiliation_unit, country, address,
    affiliation_source
  ) |>
  arrange(author_name, year, UT, author_position, address)

# 8. Tablas sustantivas del paper --------------------------------------------
# Papers conserva una fila por publicacion y resume su composicion geografica.
papers <- base_oficial |>
  transmute(
    UT,
    year = as.integer(PY),
    title = TI,
    journal = SO,
    doi = DI,
    document_type = DT,
    citations = TC,
    authors = AF,
    corresponding_author = RP,
    author_keywords = keywords_autor,
    analysis_keywords = keywords_analisis,
    wos_categories = WC,
    core_area,
    experimental_method = des_exp,
    C1,
    C3
  ) |>
  left_join(paper_country_summary, by = "UT") |>
  arrange(year, UT)

# Las disciplinas no son mutuamente excluyentes: un paper aparece una vez en
# cada core area asignada.
disciplines_5year <- papers |>
  select(UT, year, core_area) |>
  separate_rows(core_area, sep = ";") |>
  mutate(
    core_area = str_squish(core_area),
    period_5year = five_year_period(year)
  ) |>
  filter(!is.na(core_area), core_area != "") |>
  count(core_area, period_5year, name = "n_papers") |>
  group_by(core_area) |>
  mutate(total_papers = sum(n_papers)) |>
  ungroup() |>
  arrange(core_area, period_5year)

# Los metodos y disciplinas tampoco son mutuamente excluyentes. Los papers sin
# palabra metodologica identificada se conservan como Not classified.
methods_discipline <- papers |>
  transmute(
    UT,
    year,
    period_5year = five_year_period(year),
    core_area,
    experimental_method = replace_na(experimental_method, "Not classified")
  ) |>
  separate_rows(core_area, sep = ";") |>
  separate_rows(experimental_method, sep = ";") |>
  mutate(
    core_area = str_squish(core_area),
    experimental_method = str_squish(experimental_method)
  ) |>
  count(
    core_area, experimental_method, period_5year,
    name = "n_papers"
  ) |>
  arrange(core_area, experimental_method, period_5year)

# Cada paper cuenta una vez por pais. Por eso los totales entre paises no deben
# sumarse como si fueran categorias excluyentes.
countries_decade <- paper_countries |>
  left_join(papers |> select(UT, year), by = "UT") |>
  mutate(decade = 10L * floor(year / 10L)) |>
  count(country, decade, name = "n_papers") |>
  group_by(country) |>
  arrange(decade, .by_group = TRUE) |>
  mutate(
    cumulative_papers = cumsum(n_papers),
    total_papers = sum(n_papers)
  ) |>
  ungroup() |>
  arrange(desc(total_papers), country, decade)

country_pairs <- paper_countries |>
  group_by(UT) |>
  group_modify(~ {
    countries <- sort(unique(.x$country))
    if (length(countries) < 2L) return(tibble())
    pairs <- t(combn(countries, 2L))
    tibble(country_1 = pairs[, 1], country_2 = pairs[, 2])
  }) |>
  ungroup() |>
  filter(
    country_1 %in% latam_country_names |
      country_2 %in% latam_country_names
  ) |>
  count(country_1, country_2, name = "n_papers", sort = TRUE)

# El resumen de autores usa la forma completa normalizada exacta. Esto integra
# diferencias de mayusculas y tildes, pero no fusiona nombres distintos.
author_countries_by_name <- author_affiliation_detail |>
  group_by(author_name_key) |>
  summarise(
    n_countries = n_distinct(country),
    countries = collapse_unique(country),
    .groups = "drop"
  )

author_institutions_by_name <- authors_long |>
  select(author_name_key, author_institutions) |>
  separate_rows(author_institutions, sep = ";") |>
  mutate(author_institutions = na_if(str_squish(author_institutions), "")) |>
  filter(!is.na(author_institutions)) |>
  group_by(author_name_key) |>
  summarise(
    n_affiliation_units = n_distinct(author_institutions),
    affiliations = collapse_unique(author_institutions),
    .groups = "drop"
  )

author_affiliation_by_name <- full_join(
  author_countries_by_name,
  author_institutions_by_name,
  by = "author_name_key"
)

authors_summary <- authors_long |>
  group_by(author_name_key) |>
  summarise(
    author_name = first(author_name),
    name_forms = collapse_unique(author_name),
    n_papers = n_distinct(UT),
    first_year = min(year),
    last_year = max(year),
    n_affiliation_patterns = n_distinct(
      author_addresses[!is.na(author_addresses)]
    ),
    changed_affiliation = n_papers > 1L & n_affiliation_patterns > 1L,
    .groups = "drop"
  ) |>
  left_join(author_affiliation_by_name, by = "author_name_key") |>
  mutate(
    n_countries = replace_na(n_countries, 0L),
    n_affiliation_units = replace_na(n_affiliation_units, 0L)
  ) |>
  select(-author_name_key) |>
  arrange(desc(n_papers), author_name)

# Las diadas se cuentan una vez por paper y se muestran solo si reaparecen en
# dos o mas publicaciones.
author_name_lookup <- authors_long |>
  group_by(author_name_key) |>
  summarise(author_name = first(author_name), .groups = "drop")

coauthor_pairs <- authors_long |>
  distinct(UT, year, author_name_key) |>
  group_by(UT, year) |>
  group_modify(~ {
    paper_authors <- sort(unique(.x$author_name_key))
    if (length(paper_authors) < 2L) return(tibble())
    pairs <- t(combn(paper_authors, 2L))
    tibble(author_1_key = pairs[, 1], author_2_key = pairs[, 2])
  }) |>
  ungroup() |>
  group_by(author_1_key, author_2_key) |>
  summarise(
    n_papers = n_distinct(UT),
    years = str_c(sort(unique(year)), collapse = "; "),
    .groups = "drop"
  ) |>
  filter(n_papers > 1L) |>
  left_join(
    author_name_lookup |> rename(author_1_key = author_name_key, author_1 = author_name),
    by = "author_1_key"
  ) |>
  left_join(
    author_name_lookup |> rename(author_2_key = author_name_key, author_2 = author_name),
    by = "author_2_key"
  ) |>
  select(author_1, author_2, n_papers, years) |>
  arrange(desc(n_papers), author_1, author_2)

journals <- papers |>
  filter(!is.na(journal), str_squish(journal) != "") |>
  group_by(journal) |>
  summarise(
    n_papers = n_distinct(UT),
    first_year = min(year),
    last_year = max(year),
    .groups = "drop"
  ) |>
  arrange(desc(n_papers), journal)

journals_year <- papers |>
  filter(!is.na(journal), str_squish(journal) != "") |>
  count(journal, year, name = "n_papers") |>
  arrange(journal, year)

# 9. Exportacion limpia -------------------------------------------------------
# No se incluyen hojas de diagnostico ni variantes de nombres. Los controles
# anteriores detienen el script si la estructura necesaria para el paper falla.
authors_long_export <- authors_long |>
  select(
    UT, year, title, journal, core_area, experimental_method,
    author_position, author_name, author_abbreviated,
    n_author_affiliations, author_institutions, institution_source,
    author_countries, author_addresses, affiliation_status, affiliation_note
  )

write_xlsx(
  list(
    Papers = papers,
    Disciplines_5year = disciplines_5year,
    Methods_discipline = methods_discipline,
    Countries_decade = countries_decade,
    Country_pairs = country_pairs,
    Authors_long = authors_long_export,
    Author_affiliations = author_affiliation_detail |>
      select(-author_name_key),
    Authors_summary = authors_summary,
    Repeated_coauthors = coauthor_pairs,
    Journals = journals,
    Journals_year = journals_year
  ),
  file.path(tables_dir, "descriptivos_affi_latam.xlsx")
)

# 10. Diagnosticos visibles en consola ---------------------------------------
# Estos valores sirven para auditar la ejecucion, pero no agregan hojas al Excel.
diagnostics <- tibble(
  measure = c(
    "Papers",
    "Author-paper rows",
    "Unique normalized author names",
    "Attributed author affiliations",
    "Authors with multiple affiliations in one paper",
    "Confirmed independent authors",
    "Unresolved authors",
    "Papers with international collaboration",
    "Papers with United States collaboration",
    "Unmapped addresses"
  ),
  n = c(
    n_distinct(papers$UT),
    nrow(authors_long),
    n_distinct(authors$author_name_key),
    nrow(author_affiliation_detail),
    author_affiliation_detail |>
      count(UT, author_position) |>
      filter(n > 1L) |>
      nrow(),
    nrow(confirmed_no_affiliation),
    nrow(unresolved_authors),
    sum(papers$has_non_latam),
    sum(papers$has_united_states),
    nrow(unmapped_addresses)
  )
)

cat("\nDescriptivos affi_latam creados correctamente\n")
print(diagnostics)
cat(
  "\nOutput:",
  file.path(tables_dir, "descriptivos_affi_latam.xlsx"),
  "\n"
)