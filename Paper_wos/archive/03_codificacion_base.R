# Preparar una copia del corpus para codificacion manual.

library(dplyr)
library(here)
library(readxl)
library(writexl)

input_path <- here(
  "Paper_wos",
  "data",
  "intermediate",
  "corpus_unico.xlsx"
)

output_path <- here(
  "Paper_wos",
  "data",
  "intermediate",
  "corpus_unico_codificacion_base.xlsx"
)

if (!file.exists(input_path)) {
  stop("No se encontro el archivo de entrada: ", input_path)
}

if (file.exists(output_path)) {
  archivo_eliminado <- file.remove(output_path)

  if (!archivo_eliminado) {
    stop(
      "No se pudo reemplazar el archivo de salida. ",
      "Compruebe que no este abierto en Excel: ",
      output_path
    )
  }
}

corpus_unico <- read_excel(input_path, sheet = "Sheet1")

columnas_originales <- c("AU", "AB", "DE", "C1", "C3")
columnas_faltantes <- setdiff(columnas_originales, names(corpus_unico))

if (length(columnas_faltantes) > 0) {
  stop(
    "Faltan columnas originales en corpus_unico.xlsx: ",
    paste(columnas_faltantes, collapse = ", ")
  )
}

columnas_fijas_nuevas <- c(
  "n_authors",
  "n_c1_entries",
  "n_c3_entries",
  "flag_ab_missing",
  "flag_de_missing",
  "flag_c1_count_mismatch",
  "flag_c3_count_mismatch",
  "AB_manual",
  "DE_manual",
  "review_status",
  "exclude_reason",
  "review_notes",
  "reviewer"
)

columnas_dinamicas_existentes <- grep(
  "^(name_aut[0-9]+|c1_entry[0-9]+|c3_entry[0-9]+|c1_aut[0-9]+_manual|c3_aut[0-9]+_manual)$",
  names(corpus_unico),
  value = TRUE
)

columnas_ya_existentes <- c(
  intersect(columnas_fijas_nuevas, names(corpus_unico)),
  columnas_dinamicas_existentes
)

if (length(columnas_ya_existentes) > 0) {
  stop(
    "Las siguientes columnas nuevas ya existen: ",
    paste(columnas_ya_existentes, collapse = ", ")
  )
}

# Separar elementos por punto y coma e ignorar entradas vacias.
split_semicolon_entries <- function(value) {
  if (is.na(value) || trimws(value) == "") {
    return(character())
  }

  entries <- trimws(strsplit(value, ";", fixed = TRUE)[[1]])
  entries[nzchar(entries)]
}

# Crear tantas columnas como el maximo de entradas observado en cada campo.
entries_to_wide <- function(entries, prefix) {
  max_entries <- max(lengths(entries), 0L)

  if (max_entries == 0L) {
    return(tibble())
  }

  columns <- lapply(
    seq_len(max_entries),
    function(position) {
      vapply(
        entries,
        function(row_entries) {
          if (length(row_entries) >= position) {
            row_entries[[position]]
          } else {
            NA_character_
          }
        },
        character(1)
      )
    }
  )

  names(columns) <- paste0(prefix, seq_len(max_entries))
  as_tibble(columns)
}

is_missing_text <- function(x) {
  is.na(x) | trimws(x) == ""
}

author_entries <- lapply(corpus_unico$AU, split_semicolon_entries)
c1_entries <- lapply(corpus_unico$C1, split_semicolon_entries)
c3_entries <- lapply(corpus_unico$C3, split_semicolon_entries)

authors_wide <- entries_to_wide(author_entries, "name_aut")

# Crear al final las celdas para registrar las afiliaciones revisadas.
authors_manual_wide <- bind_cols(lapply(
  seq_len(3L),
  function(position) {
    tibble(
      !!paste0("c1_aut", position, "_manual") := NA_character_,
      !!paste0("c3_aut", position, "_manual") := NA_character_
    )
  }
))
c1_wide <- entries_to_wide(c1_entries, "c1_entry")
c3_wide <- entries_to_wide(c3_entries, "c3_entry")

# Todas las variables generadas se agregan al final de las columnas originales.
# Los bloques AU, C1 y C3 se dimensionan por separado para no perder entradas.
corpus_unico_codificacion <- bind_cols(
  corpus_unico,
  authors_wide,
  c1_wide,
  c3_wide
) |>
  mutate(
    n_authors = lengths(author_entries),
    n_c1_entries = lengths(c1_entries),
    n_c3_entries = lengths(c3_entries),
    flag_ab_missing = as.integer(is_missing_text(AB)),
    flag_de_missing = as.integer(is_missing_text(DE)),
    flag_c1_count_mismatch = case_when(
      n_authors == 0L ~ NA_integer_,
      n_authors != n_c1_entries ~ 1L,
      TRUE ~ 0L
    ),
    flag_c3_count_mismatch = case_when(
      n_authors == 0L ~ NA_integer_,
      n_authors != n_c3_entries ~ 1L,
      TRUE ~ 0L
    ),
    AB_manual = NA_character_,
    DE_manual = NA_character_
  ) |>
  bind_cols(authors_manual_wide) |>
  mutate(
    review_status = NA_character_,
    exclude_reason = NA_character_,
    review_notes = NA_character_,
    reviewer = NA_character_
  )

write_xlsx(
  list(Sheet1 = corpus_unico_codificacion),
  path = output_path
)
