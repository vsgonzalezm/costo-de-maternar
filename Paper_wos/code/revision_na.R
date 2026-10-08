library(tidyverse)

base_dir <- here::here("Paper_wos")
data_dir <- file.path(base_dir, "data", "intermediate")
out_dir  <- file.path(base_dir, "outputs", "tables")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

bases <- tibble(
  base = c(
    "corpus_unico_inicial",
    "endogeno_inicial",
    "laboratorio_inicial"
  ),
  archivo = file.path(
    data_dir,
    c(
      "corpus_unico_inicial.rds",
      "endogeno_inicial.rds",
      "laboratorio_inicial.rds"
    )
  )
) %>%
  filter(file.exists(archivo))

variables_revision <- list(
  abstract = c("AB", "abstract", "Abstract"),
  c1 = c("C1", "c1"),
  c3 = c("C3", "c3"),
  keywords_originales = c("DE", "author_keywords", "keywords_originales"),
  keywords_wos = c("ID", "keywords_plus", "keywords_wos"),
  doi = c("DI", "DOI", "doi")
)

detectar_columna <- function(datos, opciones) {
  nombres <- names(datos)
  encontrada <- nombres[tolower(nombres) %in% tolower(opciones)]

  if (length(encontrada) == 0) {
    return(NA_character_)
  }

  encontrada[[1]]
}

contar_missing <- function(datos, variable, opciones) {
  columna <- detectar_columna(datos, opciones)
  n_total <- nrow(datos)

  if (is.na(columna)) {
    return(tibble(
      variable = variable,
      columna = NA_character_,
      n_total = n_total,
      n_missing = n_total,
      n_con_dato = 0,
      pct_missing = 100,
      estado = "columna_no_encontrada"
    ))
  }

  valores <- datos[[columna]]
  valores_chr <- as.character(valores)

  missing <- is.na(valores) | str_trim(valores_chr) == ""

  tibble(
    variable = variable,
    columna = columna,
    n_total = n_total,
    n_missing = sum(missing),
    n_con_dato = n_total - n_missing,
    pct_missing = round(100 * n_missing / n_total, 1),
    estado = "ok"
  )
}

tabla_missing <- bases %>%
  mutate(datos = map(archivo, readRDS)) %>%
  mutate(
    revision = map(datos, function(datos_base) {
      imap_dfr(
        variables_revision,
        function(opciones, variable) {
          contar_missing(datos_base, variable = variable, opciones = opciones)
        }
      )
    })
  ) %>%
  select(base, revision) %>%
  unnest(revision) %>%
  arrange(base, variable)

tabla_missing_resumen <- tabla_missing %>%
  select(base, variable, n_missing, pct_missing) %>%
  pivot_wider(
    names_from = variable,
    values_from = c(n_missing, pct_missing)
  )

write_csv(
  tabla_missing,
  file.path(out_dir, "revision_na.csv")
)

write_csv(
  tabla_missing_resumen,
  file.path(out_dir, "revision_na_resumen.csv")
)

print(tabla_missing)
