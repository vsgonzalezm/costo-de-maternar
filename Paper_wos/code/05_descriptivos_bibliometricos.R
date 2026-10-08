#-----------------------------------------------
# Descriptivos bibliometricos de paises, autores, afiliaciones y revistas.
# Valentina Gonzalez Madariaga
# Agosto 2026
#-----------------------------------------------



# Este script usa exclusivamente el corpus depurado por 00_inicial.R. Produce
# un solo libro Excel con tablas descriptivas y bases de auditoria, ademas de
# las dos figuras. No crea nuevas bases intermedias.

library(tidyverse)
library(here)
library(writexl)
library(igraph)
library(readxl)

# 1. Rutas y carga de insumos -------------------------------------------------
# corpus_unico_inicial.rds ya contiene las correcciones de Isidora, las
# exclusiones acordadas y las variables derivadas en 00_inicial.R.
data_dir <- here("Paper_wos", "data")
tables_dir <- here("Paper_wos", "outputs", "tables")
figures_dir <- here("Paper_wos", "outputs", "figures")

dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

corpus_completo <- readRDS(
  file.path(data_dir, "intermediate", "corpus_unico_inicial.rds")
)

# La base de revision conserva C1 y C3 manuales por posicion de autor. Se usa
# solo para recuperar ese vinculo; el universo de papers siempre lo define el
# corpus ya depurado por 00.
revision_manual <- read_excel(
  file.path(data_dir, "intermediate", "base final_Isi.xlsx")
)

# Version compacta a nivel paper para los descriptivos. C1 ya incorpora las
# correcciones manuales realizadas en 00.
corpus <- corpus_completo |>
  transmute(
    UT,
    year = as.integer(PY),
    title = TI,
    journal = SO,
    C1,
    base_endogeno,
    base_laboratorio
  )

# Un mismo paper puede pertenecer a ambas estrategias de busqueda. Esta tabla
# permite producir resultados separados sin duplicarlo dentro del corpus.
paper_base <- bind_rows(
  corpus |> filter(base_endogeno == 1L) |> select(UT) |> mutate(base = "Endogenous"),
  corpus |> filter(base_laboratorio == 1L) |> select(UT) |> mutate(base = "Laboratory")
)

# 2. Funciones de normalizacion ----------------------------------------------
# normalize_name crea claves comparables, pero no desambigua autores. Las
# variantes quedan visibles para revision en una tabla especifica.
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

latam_country_names <- c(
  "Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica",
  "Cuba", "Dominican Rep", "Ecuador", "El Salvador", "Guatemala", "Haiti",
  "Honduras", "Mexico", "Nicaragua", "Panama", "Paraguay", "Peru",
  "Puerto Rico", "Uruguay", "Venezuela"
)

# WoS usa un vocabulario acotado de paises. La lista incluye todos los valores
# observados actualmente en C1; las formas especiales se homologan abajo. Si
# aparece una forma nueva, el script se detiene para que no se pierda en silencio.
standard_wos_countries <- c(
  "ARGENTINA", "AUSTRALIA", "AUSTRIA", "BELGIUM", "BENIN", "BOLIVIA",
  "BRAZIL", "BURKINA FASO", "CAMBODIA", "CANADA", "CHILE", "COLOMBIA",
  "COSTA RICA", "COTE IVOIRE", "CROATIA", "CUBA", "DENMARK", "ECUADOR",
  "ETHIOPIA", "FINLAND", "FRANCE", "GERMANY", "GUATEMALA", "HONDURAS",
  "HUNGARY", "INDIA", "INDONESIA", "IRELAND", "ISRAEL", "ITALY", "JAPAN",
  "KENYA", "KUWAIT", "MEXICO", "MOROCCO", "NEPAL", "NETHERLANDS",
  "NIGERIA", "NORWAY", "PAKISTAN", "PERU", "PHILIPPINES", "POLAND",
  "PORTUGAL", "RUSSIA", "SENEGAL", "SINGAPORE", "SLOVAKIA", "SOUTH AFRICA",
  "SPAIN", "SWEDEN", "SWITZERLAND", "TAIWAN", "TANZANIA", "THAILAND",
  "TURKEY", "URUGUAY", "VENEZUELA"
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

# 3. Lectura de autores y C1 desde los TXT originales ------------------------
# Los TXT conservan AF (nombre completo) y los corchetes de C1 que vinculan
# autores con direcciones. Esos elementos no se pueden reconstruir con certeza
# desde el C1 colapsado, por eso se recuperan aqui y luego se restringen al
# corpus final producido por 00.
extract_field <- function(record, tag) {
  start <- which(str_detect(record, paste0("^", tag, " ")))[1]

  if (is.na(start)) {
    return(character())
  }

  next_tag <- which(seq_along(record) > start & str_detect(record, "^[A-Z0-9]{2} "))
  end <- if (length(next_tag) == 0L) length(record) else min(next_tag) - 1L
  values <- record[start:end]
  values[1] <- str_remove(values[1], paste0("^", tag, " "))
  str_squish(values)
}

merge_c1_lines <- function(lines) {
  if (length(lines) == 0L) {
    return(character())
  }

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

parse_wos_record <- function(record, source_base) {
  ut <- first(extract_field(record, "UT"), default = NA_character_)
  au <- extract_field(record, "AU")
  af <- extract_field(record, "AF")
  n_authors <- max(length(au), length(af))

  if (n_authors == 0L) {
    authors <- tibble()
  } else {
    length(au) <- n_authors
    length(af) <- n_authors
    authors <- tibble(
      UT = ut,
      source_base,
      author_position = seq_len(n_authors),
      author = au,
      author_full = af,
      author_full_key = normalize_name(af)
    )
  }

  c1_entries <- merge_c1_lines(extract_field(record, "C1"))

  affiliations <- map_dfr(seq_along(c1_entries), function(position) {
    entry <- c1_entries[[position]]
    matched <- str_match(entry, "^\\[(.*?)\\]\\s*(.*)$")

    if (is.na(matched[1, 2])) {
      return(tibble(
        UT = ut,
        source_base,
        affiliation_entry = position,
        c1_author_full = NA_character_,
        address = entry,
        mapping_status = "No author link in C1"
      ))
    }

    str_split(matched[1, 2], ";", simplify = FALSE)[[1]] |>
      str_squish() |>
      tibble(c1_author_full = _) |>
      mutate(
        UT = ut,
        source_base,
        affiliation_entry = position,
        address = matched[1, 3],
        mapping_status = "Author linked in C1",
        .before = 1
      )
  })

  list(authors = authors, affiliations = affiliations)
}

parse_wos_file <- function(path) {
  source_base <- if_else(
    str_detect(str_to_lower(path), "endogeno"),
    "Endogenous",
    "Laboratory"
  )
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  record_id <- cumsum(str_detect(lines, "^PT "))
  records <- split(lines[record_id > 0L], record_id[record_id > 0L])
  map(records, parse_wos_record, source_base = source_base)
}

raw_files <- c(
  list.files(file.path(data_dir, "raw", "endogeno"), pattern = "\\.txt$", full.names = TRUE),
  list.files(file.path(data_dir, "raw", "laboratorio"), pattern = "\\.txt$", full.names = TRUE)
)

parsed_records <- raw_files |>
  map(parse_wos_file) |>
  flatten()

# 4. Correcciones manuales por posicion de autor -----------------------------
# name_autN, c1_autN_manual y c3_autN_manual comparten la misma posicion N.
# Se transforman a formato largo para unirlas sin crear un numero fijo de
# columnas en la base final de autores.
manual_column <- function(data, column_name) {
  if (column_name %in% names(data)) {
    as.character(data[[column_name]])
  } else {
    rep(NA_character_, nrow(data))
  }
}

author_positions <- str_match(
  names(revision_manual),
  "^name_aut([0-9]+)$"
)[, 2] |>
  as.integer() |>
  discard(is.na)

max_author_position <- max(author_positions)

manual_author_corrections <- map_dfr(seq_len(max_author_position), function(position) {
  tibble(
    UT = revision_manual$UT,
    author_position = position,
    author_manual = manual_column(revision_manual, str_c("name_aut", position)),
    c1_manual = manual_column(revision_manual, str_c("c1_aut", position, "_manual")),
    c3_manual = manual_column(revision_manual, str_c("c3_aut", position, "_manual"))
  )
}) |>
  filter(UT %in% corpus$UT) |>
  mutate(
    across(
      c(author_manual, c1_manual, c3_manual),
      ~ na_if(str_squish(.x), "")
    )
  )

# 5. Base maestra de autores -------------------------------------------------
# AF entrega el nombre completo y AU la forma abreviada. author_name_normalized
# identifica coincidencias exactas despues de normalizar. author_candidate_key
# agrupa solo candidatos con igual apellido e inicial; no declara que sean la
# misma persona y se usa exclusivamente para auditar variantes.
authors <- parsed_records |>
  map("authors") |>
  bind_rows() |>
  distinct(UT, author_position, .keep_all = TRUE) |>
  inner_join(corpus |> select(-C1), by = "UT") |>
  left_join(manual_author_corrections, by = c("UT", "author_position")) |>
  mutate(
    author = na_if(str_squish(author), ""),
    author_full = na_if(str_squish(author_full), ""),
    author_name = coalesce(author_full, author_manual, author),
    author_name_normalized = normalize_name(author_name),
    author_surname = if_else(
      str_detect(author_name, fixed(",")),
      str_extract(author_name, "^[^,]+"),
      str_remove(author, "\\s+[^ ]+$")
    ),
    author_given = if_else(
      str_detect(author_name, fixed(",")),
      str_remove(author_name, "^[^,]+,\\s*"),
      str_extract(author, "[^ ]+$")
    ),
    author_candidate_key = str_c(
      normalize_name(author_surname),
      str_sub(normalize_name(author_given), 1L, 1L),
      sep = "|"
    )
  )

# Esta auditoria garantiza la regla central: un paper con N autores debe
# producir exactamente N filas en la base autor-paper.
author_count_audit <- corpus_completo |>
  transmute(
    UT,
    expected_authors = case_when(
      is.na(AU) | str_squish(AU) == "" ~ 0L,
      TRUE ~ str_count(AU, fixed(";")) + 1L
    )
  ) |>
  left_join(authors |> count(UT, name = "author_rows"), by = "UT") |>
  mutate(
    author_rows = replace_na(author_rows, 0L),
    count_matches = expected_authors == author_rows
  )

if (any(!author_count_audit$count_matches)) {
  stop("La cantidad de filas autor-paper no coincide con AU. Revisar author_count_audit.")
}

# 6. Afiliaciones originales y manuales por autor ---------------------------
# Las entradas C1 entre corchetes de los TXT vinculan uno o varios nombres
# completos con una direccion. Primero se recuperan esos vinculos originales.
raw_affiliations <- parsed_records |>
  map("affiliations") |>
  bind_rows() |>
  filter(UT %in% corpus$UT) |>
  distinct(UT, affiliation_entry, c1_author_full, address, .keep_all = TRUE) |>
  mutate(
    author_full_key = normalize_name(c1_author_full),
    address = na_if(str_squish(address), "")
  )

author_key <- authors |>
  filter(!is.na(author_full_key), author_full_key != "") |>
  distinct(
    UT, author_position, author, author_full, author_name,
    author_name_normalized, author_candidate_key, author_full_key
  )

wos_author_affiliations <- raw_affiliations |>
  filter(!is.na(c1_author_full), !is.na(address)) |>
  inner_join(author_key, by = c("UT", "author_full_key")) |>
  transmute(
    UT,
    author_position,
    address,
    affiliation_source = "wos_c1_linked"
  ) |>
  distinct()

# Cuando Isidora completo C1 por autor, esa informacion reemplaza el vinculo
# original de ese autor. Una celda puede contener mas de una direccion.
manual_author_affiliations <- manual_author_corrections |>
  filter(!is.na(c1_manual)) |>
  transmute(UT, author_position, address = c1_manual) |>
  separate_rows(address, sep = ";") |>
  mutate(
    address = na_if(str_squish(address), ""),
    affiliation_source = "manual_c1"
  ) |>
  filter(!is.na(address)) |>
  distinct()

manual_c1_keys <- manual_author_affiliations |>
  distinct(UT, author_position)

author_affiliations <- wos_author_affiliations |>
  anti_join(manual_c1_keys, by = c("UT", "author_position")) |>
  bind_rows(manual_author_affiliations)

# Si el paper tiene un solo autor y WoS no trae el vinculo entre nombre y C1,
# todas las direcciones corregidas del paper pueden asignarse sin ambiguedad.
single_author_fallback <- authors |>
  count(UT, name = "n_authors") |>
  filter(n_authors == 1L) |>
  select(UT) |>
  inner_join(authors |> select(UT, author_position), by = "UT") |>
  anti_join(
    author_affiliations |> distinct(UT, author_position),
    by = c("UT", "author_position")
  ) |>
  inner_join(corpus |> select(UT, C1), by = "UT") |>
  rename(address = C1) |>
  separate_rows(address, sep = ";") |>
  mutate(
    address = na_if(str_squish(address), ""),
    affiliation_source = "paper_c1_single_author"
  ) |>
  filter(!is.na(address)) |>
  distinct(UT, author_position, address, affiliation_source)

author_affiliations <- author_affiliations |>
  bind_rows(single_author_fallback) |>
  mutate(
    institution_address = str_extract(address, "^[^,]+") |> str_squish(),
    country_raw = extract_country_raw(address),
    country = normalize_country(address)
  ) |>
  distinct(UT, author_position, address, .keep_all = TRUE)

# 7. Auditoria completa del mapeo de paises ---------------------------------
# El universo de control usa el C1 corregido a nivel paper. Cada direccion debe
# terminar en una forma reconocida. Si alguna no se reconoce, el script se
# detiene antes de generar resultados incompletos.
paper_address_entries <- corpus |>
  select(UT, C1) |>
  rename(address = C1) |>
  separate_rows(address, sep = ";") |>
  mutate(
    address = na_if(str_squish(address), ""),
    country_raw = extract_country_raw(address),
    country = normalize_country(address)
  ) |>
  filter(!is.na(address))

unmapped_country_entries <- paper_address_entries |>
  filter(is.na(country))

if (nrow(unmapped_country_entries) > 0L) {
  unknown_values <- unmapped_country_entries |>
    distinct(country_raw) |>
    slice_head(n = 10L) |>
    pull(country_raw) |>
    str_c(collapse = ", ")

  stop(str_c("Hay formas de pais sin mapear en C1: ", unknown_values))
}

if (any(is.na(author_affiliations$country))) {
  stop("Hay afiliaciones de autor con pais sin mapear.")
}

country_mapping_audit <- paper_address_entries |>
  count(country_raw, country, name = "n_address_entries", sort = TRUE) |>
  mutate(mapping_status = "mapped")

paper_countries <- paper_address_entries |>
  distinct(UT, country)

# 8. Base larga autor-paper ---------------------------------------------------
# author_paper contiene exactamente una fila por autor y paper. Conserva todas
# las variables del paper, los nombres WoS sin desambiguar y las afiliaciones
# atribuibles al autor. paper_C1 y paper_C3 se repiten para mantener disponible
# la informacion completa aun cuando no pueda asignarse a una persona concreta.
author_affiliation_summary <- author_affiliations |>
  group_by(UT, author_position) |>
  summarise(
    n_author_affiliations = n_distinct(address),
    author_addresses = collapse_unique(address),
    institutions_from_address = collapse_unique(institution_address),
    author_countries = collapse_unique(country),
    affiliation_sources = collapse_unique(affiliation_source),
    .groups = "drop"
  )

author_paper <- authors |>
  select(
    UT, author_position, author, author_full, author_name,
    author_name_normalized, author_candidate_key, author_surname, author_given,
    c1_manual, c3_manual
  ) |>
  inner_join(corpus_completo, by = "UT") |>
  rename(
    paper_author_list = AU,
    paper_C1 = C1,
    paper_C3 = C3
  ) |>
  left_join(author_affiliation_summary, by = c("UT", "author_position")) |>
  mutate(
    n_author_affiliations = replace_na(n_author_affiliations, 0L),
    author_institutions = coalesce(c3_manual, institutions_from_address),
    institution_source = case_when(
      !is.na(c3_manual) ~ "manual_c3",
      !is.na(institutions_from_address) ~ "derived_from_author_c1",
      !is.na(paper_C3) & str_squish(paper_C3) != "" ~ "paper_c3_unassigned",
      TRUE ~ "missing"
    ),
    affiliation_status = case_when(
      !is.na(c1_manual) ~ "manual_c1",
      n_author_affiliations > 0L ~ affiliation_sources,
      !is.na(paper_C1) & str_squish(paper_C1) != "" ~ "paper_c1_unassigned",
      TRUE ~ "missing_c1"
    )
  ) |>
  arrange(UT, author_position)

# 8a. Hoja de revision para papers con afiliaciones no asignables -----------
# Estos papers si tienen C1 y C3, pero una parte de sus autores no aparece
# vinculada a una direccion. Se entrega una fila por paper con los autores
# pendientes y los campos originales para revision manual.
affiliation_review_authors <- author_paper |>
  filter(affiliation_status == "paper_c1_unassigned") |>
  group_by(UT) |>
  summarise(
    n_unassigned_authors = n(),
    unassigned_authors = collapse_unique(author_name),
    .groups = "drop"
  )

affiliation_review_40 <- affiliation_review_authors |>
  left_join(
    corpus |> select(UT, year, title, journal),
    by = "UT"
  ) |>
  left_join(
    revision_manual |>
      select(any_of(c("UT", "AU", "AF", "C1", "C1raw", "C3", "review_status", "review_notes"))),
    by = "UT"
  ) |>
  mutate(
    n_authors = str_count(AU, fixed(";")) + 1L,
    c1raw_has_brackets = str_detect(C1raw, fixed("[")),
    review_scope = if_else(
      c1raw_has_brackets,
      "Revisar autores sin vinculo en C1raw",
      "Revisar asignacion de todos los autores"
    )
  ) |>
  arrange(desc(c1raw_has_brackets), UT)

if (nrow(affiliation_review_40) != 40L) {
  stop("La hoja de revision no contiene los 40 papers esperados.")
}
# Tabla separada con una fila por afiliacion atribuible. Permite revisar casos
# con varias instituciones sin romper la regla de una fila por autor-paper.
author_affiliation_history <- author_affiliations |>
  left_join(
    authors |>
      select(
        UT, author_position, author, author_full, author_name,
        author_name_normalized, author_candidate_key
      ),
    by = c("UT", "author_position")
  ) |>
  left_join(
    corpus |> select(UT, year, title, base_endogeno, base_laboratorio),
    by = "UT"
  ) |>
  select(
    author_name, author_full, author, author_name_normalized,
    author_candidate_key, author_position, UT, year, title,
    institution_address, country, address, affiliation_source,
    base_endogeno, base_laboratorio
  ) |>
  arrange(author_name_normalized, year, UT, institution_address)

# 8b. Conteo de afiliaciones multiples ----------------------------------------
# Se calculan dos medidas distintas. La primera identifica autores con dos o
# mas direcciones en un mismo paper. La segunda identifica autores que aparecen
# con mas de una direccion o institucion distinta a lo largo de la base. Solo
# cuentan afiliaciones que pudieron atribuirse a un autor.
affiliations_per_author_paper <- author_affiliation_history |>
  group_by(author_name_normalized, UT, author_position) |>
  summarise(
    n_affiliations_same_paper = n_distinct(address),
    .groups = "drop"
  )

author_affiliation_counts <- author_paper |>
  distinct(author_name_normalized, author_name) |>
  left_join(
    author_affiliation_history |>
      group_by(author_name_normalized) |>
      summarise(
        n_distinct_addresses = n_distinct(address),
        n_distinct_institutions = n_distinct(institution_address),
        n_papers_with_affiliation = n_distinct(UT),
        .groups = "drop"
      ),
    by = "author_name_normalized"
  ) |>
  left_join(
    affiliations_per_author_paper |>
      group_by(author_name_normalized) |>
      summarise(
        max_affiliations_same_paper = max(n_affiliations_same_paper),
        .groups = "drop"
      ),
    by = "author_name_normalized"
  ) |>
  mutate(
    across(
      c(n_distinct_addresses, n_distinct_institutions,
        n_papers_with_affiliation, max_affiliations_same_paper),
      ~ replace_na(.x, 0L)
    ),
    affiliation_data_status = if_else(
      n_papers_with_affiliation > 0L,
      "Attribution observable",
      "No affiliation assigned to author"
    )
  ) |>
  arrange(desc(max_affiliations_same_paper), desc(n_distinct_institutions), author_name)

affiliation_multiplicity <- author_affiliation_counts |>
  count(max_affiliations_same_paper, name = "n_authors") |>
  arrange(max_affiliations_same_paper)
# 9. Descriptivos de paises y coocurrencia ----------------------------------
country_by_paper <- paper_countries |>
  group_by(UT) |>
  summarise(
    n_countries = n_distinct(country),
    countries = collapse_unique(country),
    latam_countries = collapse_unique(country[country %in% latam_country_names]),
    n_latam_countries = n_distinct(country[country %in% latam_country_names]),
    has_latam = any(country %in% latam_country_names),
    has_non_latam = any(!country %in% latam_country_names),
    .groups = "drop"
  )

papers_country_summary <- corpus |>
  select(-C1) |>
  left_join(country_by_paper, by = "UT") |>
  mutate(
    n_countries = replace_na(n_countries, 0L),
    n_latam_countries = replace_na(n_latam_countries, 0L),
    has_latam = replace_na(has_latam, FALSE),
    has_non_latam = replace_na(has_non_latam, FALSE),
    latam_with_other_regions = has_latam & has_non_latam
  )

summary_base <- paper_base |>
  left_join(papers_country_summary, by = "UT") |>
  group_by(base) |>
  summarise(
    n_papers = n_distinct(UT),
    papers_with_country = n_distinct(UT[n_countries > 0L]),
    papers_with_latam = n_distinct(UT[has_latam]),
    papers_latam_and_other_regions = n_distinct(UT[latam_with_other_regions]),
    percent_with_latam = 100 * papers_with_latam / n_papers,
    percent_latam_international = 100 * papers_latam_and_other_regions / papers_with_latam,
    .groups = "drop"
  )

papers_latam <- papers_country_summary |>
  filter(has_latam) |>
  left_join(paper_base, by = "UT", relationship = "many-to-many") |>
  arrange(base, year, UT) |>
  select(
    base, UT, year, title, journal, n_countries, countries,
    n_latam_countries, latam_countries, latam_with_other_regions
  )

country_participation <- paper_countries |>
  left_join(paper_base, by = "UT", relationship = "many-to-many") |>
  group_by(base, country) |>
  summarise(n_papers = n_distinct(UT), .groups = "drop") |>
  mutate(region = if_else(country %in% latam_country_names, "Latin America", "Other")) |>
  arrange(base, desc(n_papers), country)

country_pairs <- paper_countries |>
  group_by(UT) |>
  group_modify(~ {
    countries <- sort(unique(.x$country))
    if (length(countries) < 2L) return(tibble())
    pairs <- t(combn(countries, 2L))
    tibble(country_1 = pairs[, 1], country_2 = pairs[, 2])
  }) |>
  ungroup()

country_cooccurrence <- country_pairs |>
  left_join(paper_base, by = "UT", relationship = "many-to-many") |>
  filter(country_1 %in% latam_country_names | country_2 %in% latam_country_names) |>
  group_by(base, country_1, country_2) |>
  summarise(n_papers = n_distinct(UT), .groups = "drop") |>
  arrange(base, desc(n_papers), country_1, country_2)

# 10. Autores, variantes de nombre, afiliaciones y coautoria ----------------
# Los resumenes usan el nombre completo normalizado exacto. Las claves
# candidatas solo senalan grupos que requieren revision manual.
authors_summary <- author_paper |>
  filter(!is.na(author_name_normalized), author_name_normalized != "") |>
  group_by(author_name_normalized) |>
  summarise(
    author_name = first(author_name),
    full_name_forms = collapse_unique(author_full),
    abbreviated_forms = collapse_unique(author),
    n_papers = n_distinct(UT),
    first_year = min(as.integer(PY), na.rm = TRUE),
    last_year = max(as.integer(PY), na.rm = TRUE),
    n_endogenous = n_distinct(UT[base_endogeno == 1L]),
    n_laboratory = n_distinct(UT[base_laboratorio == 1L]),
    .groups = "drop"
  ) |>
  arrange(desc(n_papers), author_name)

author_name_variants <- author_paper |>
  filter(!is.na(author_candidate_key), author_candidate_key != "|") |>
  group_by(author_candidate_key) |>
  summarise(
    n_normalized_names = n_distinct(author_name_normalized),
    name_forms = collapse_unique(author_name),
    abbreviated_forms = collapse_unique(author),
    n_papers = n_distinct(UT),
    first_year = min(as.integer(PY), na.rm = TRUE),
    last_year = max(as.integer(PY), na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(n_normalized_names > 1L) |>
  arrange(desc(n_normalized_names), desc(n_papers), author_candidate_key)

author_paper_affiliations <- author_paper |>
  filter(!is.na(author_name_normalized), author_name_normalized != "") |>
  transmute(
    author_name_normalized,
    UT,
    institution_set = replace_na(author_institutions, ""),
    country_set = replace_na(author_countries, "")
  ) |>
  distinct()

affiliation_changes <- author_paper |>
  filter(!is.na(author_name_normalized), author_name_normalized != "") |>
  group_by(author_name_normalized) |>
  summarise(
    author_name = first(author_name),
    n_papers = n_distinct(UT),
    n_papers_with_author_affiliation = n_distinct(UT[n_author_affiliations > 0L]),
    first_year = min(as.integer(PY), na.rm = TRUE),
    last_year = max(as.integer(PY), na.rm = TRUE),
    n_institutions = n_distinct(author_institutions, na.rm = TRUE),
    institutions = collapse_unique(author_institutions),
    n_countries = n_distinct(author_countries, na.rm = TRUE),
    countries = collapse_unique(author_countries),
    .groups = "drop"
  ) |>
  left_join(
    author_paper_affiliations |>
      group_by(author_name_normalized) |>
      summarise(
        n_affiliation_patterns = n_distinct(institution_set[institution_set != ""]),
        n_country_patterns = n_distinct(country_set[country_set != ""]),
        affiliation_changed_between_papers = n() > 1L & n_affiliation_patterns > 1L,
        country_changed_between_papers = n() > 1L & n_country_patterns > 1L,
        .groups = "drop"
      ),
    by = "author_name_normalized"
  ) |>
  arrange(desc(affiliation_changed_between_papers), desc(n_papers), author_name)

coauthor_pairs <- author_paper |>
  filter(!is.na(author_name_normalized), author_name_normalized != "") |>
  distinct(UT, author_name_normalized) |>
  group_by(UT) |>
  group_modify(~ {
    paper_authors <- sort(unique(.x$author_name_normalized))
    if (length(paper_authors) < 2L) return(tibble())
    pairs <- t(combn(paper_authors, 2L))
    tibble(author_1 = pairs[, 1], author_2 = pairs[, 2])
  }) |>
  ungroup() |>
  left_join(corpus |> select(UT, year, base_endogeno, base_laboratorio), by = "UT") |>
  group_by(author_1, author_2) |>
  summarise(
    n_papers = n_distinct(UT),
    years = str_c(sort(unique(year)), collapse = "; "),
    n_endogenous = n_distinct(UT[base_endogeno == 1L]),
    n_laboratory = n_distinct(UT[base_laboratorio == 1L]),
    .groups = "drop"
  ) |>
  filter(n_papers > 1L) |>
  arrange(desc(n_papers), author_1, author_2)

# 11. Revistas y evolucion temporal ------------------------------------------
# Se mantienen los conteos por estrategia de busqueda y por ano.
journals <- paper_base |>
  left_join(corpus |> select(UT, year, journal), by = "UT") |>
  filter(!is.na(journal), journal != "") |>
  group_by(base, journal) |>
  summarise(
    n_papers = n_distinct(UT),
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(base, desc(n_papers), journal)

journals_year <- paper_base |>
  left_join(corpus |> select(UT, year, journal), by = "UT") |>
  filter(!is.na(journal), journal != "", !is.na(year)) |>
  count(base, journal, year, name = "n_papers") |>
  arrange(base, journal, year)

# 12. Diagnosticos y libro de salida -----------------------------------------
# Diagnostics resume las coberturas criticas. Author_count_audit permite
# comprobar paper por paper que el numero de filas coincide con AU.
diagnostics <- tibble(
  measure = c(
    "Unique papers",
    "Author-paper rows",
    "Papers with author-count mismatch",
    "Author rows with manual C1",
    "Author rows with linked WoS C1",
    "Author rows with single-author C1 fallback",
    "Author rows with paper C1 not assignable",
    "Author rows with missing C1",
    "Corrected paper C1 address entries",
    "Unmapped country entries",
    "Candidate name-variant groups",
    "Authors with >1 address in one paper",
    "Authors with >1 observable address overall",
    "Authors with >1 observable institution overall",
    "Maximum addresses for one author-paper"
  ),
  n = c(
    n_distinct(corpus$UT),
    nrow(author_paper),
    sum(!author_count_audit$count_matches),
    sum(author_paper$affiliation_status == "manual_c1", na.rm = TRUE),
    sum(str_detect(author_paper$affiliation_status, "wos_c1_linked"), na.rm = TRUE),
    sum(str_detect(author_paper$affiliation_status, "paper_c1_single_author"), na.rm = TRUE),
    sum(author_paper$affiliation_status == "paper_c1_unassigned", na.rm = TRUE),
    sum(author_paper$affiliation_status == "missing_c1", na.rm = TRUE),
    nrow(paper_address_entries),
    nrow(unmapped_country_entries),
    nrow(author_name_variants),
    sum(author_affiliation_counts$max_affiliations_same_paper > 1L),
    sum(author_affiliation_counts$n_distinct_addresses > 1L),
    sum(author_affiliation_counts$n_distinct_institutions > 1L),
    max(author_affiliation_counts$max_affiliations_same_paper)
  )
)

# Todas las tablas se guardan en un unico libro. Author_paper es la base larga
# principal; Author_affiliations contiene el detalle cuando un autor posee mas
# de una direccion. No se crean archivos de datos intermedios adicionales.
write_xlsx(
  list(
    Summary = summary_base,
    Papers_LatAm = papers_latam,
    Countries = country_participation,
    Country_cooccurrence = country_cooccurrence,
    Country_mapping = country_mapping_audit,
    Authors_summary = authors_summary,
    Author_paper = author_paper,
    Affiliation_review_40 = affiliation_review_40,
    Author_name_variants = author_name_variants,
    Author_affiliations = author_affiliation_history,
    Author_affiliation_counts = author_affiliation_counts,
    Affiliation_multiplicity = affiliation_multiplicity,
    Affiliation_changes = affiliation_changes,
    Repeated_coauthor_pairs = coauthor_pairs,
    Journals = journals,
    Journals_by_year = journals_year,
    Author_count_audit = author_count_audit,
    Diagnostics = diagnostics
  ),
  file.path(tables_dir, "descriptivos_endogeno_laboratorio.xlsx")
)

# 13. Figuras descriptivas ----------------------------------------------------
# Red de coocurrencia con al menos un pais latinoamericano en cada vinculo.
network_edges <- country_pairs |>
  filter(country_1 %in% latam_country_names | country_2 %in% latam_country_names) |>
  count(country_1, country_2, name = "n_papers")

if (nrow(network_edges) > 0L) {
  network <- graph_from_data_frame(network_edges, directed = FALSE)
  vertex_region <- if_else(V(network)$name %in% latam_country_names, "Latin America", "Other")

  png(
    file.path(figures_dir, "country_cooccurrence_latam.png"),
    width = 2600,
    height = 1900,
    res = 240
  )
  set.seed(20260929)
  plot(
    network,
    layout = layout_with_fr(network, weights = E(network)$n_papers),
    vertex.color = if_else(vertex_region == "Latin America", "#D55E00", "#56B4E9"),
    vertex.frame.color = "white",
    vertex.size = 7 + 3 * log1p(degree(network)),
    vertex.label.cex = 0.75,
    vertex.label.color = "#1C1C1C",
    edge.width = 0.7 + 1.5 * log1p(E(network)$n_papers),
    edge.color = adjustcolor("#666666", alpha.f = 0.45),
    main = "Country co-occurrence in papers with Latin American affiliations"
  )
  legend(
    "topleft",
    legend = c("Latin America", "Other"),
    col = c("#D55E00", "#56B4E9"),
    pch = 19,
    bty = "n"
  )
  dev.off()
}

top_journals <- journals |>
  group_by(journal) |>
  summarise(n_papers = sum(n_papers), .groups = "drop") |>
  slice_max(n_papers, n = 12L, with_ties = FALSE) |>
  pull(journal)

journals_plot <- journals_year |>
  filter(journal %in% top_journals) |>
  ggplot(aes(year, n_papers, color = base)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  facet_wrap(~ journal, scales = "free_y", ncol = 3) +
  scale_color_manual(values = c("Endogenous" = "#D55E00", "Laboratory" = "#0072B2")) +
  scale_x_continuous(breaks = scales::breaks_pretty(4)) +
  labs(x = "Year", y = "Papers", color = "Database") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

ggsave(
  file.path(figures_dir, "journals_top_over_time.png"),
  journals_plot,
  width = 13,
  height = 10,
  dpi = 320
)

print(summary_base)
print(diagnostics)
print(affiliation_multiplicity)
