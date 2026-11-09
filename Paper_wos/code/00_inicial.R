# ------------------------------------------------------------
# 00 - Construccion de la base oficial con afiliacion LatAm
# Valentina Gonzalez Madariaga
# Noviembre 2026
# ------------------------------------------------------------

# Reconstruye la base desde los TXT originales. La procedencia de la busqueda
# se usa solo para auditar el origen; no define universos analiticos separados.
# El universo final exige al menos una afiliacion en uno de los 14 paises LatAm ( los que mantuvimos del filtro de wos)
# incluidos en la estrategia original de busqueda


library(tidyverse)
library(bibliometrix)
library(here)
library(readxl)

# 1. Carga y conciliacion de los TXT originales -----------------------------

raw_dir <- here("Paper_wos", "data", "raw")
endogeno_dir <- file.path(raw_dir, "endogeno")
laboratorio_dir <- file.path(raw_dir, "laboratorio")
intermediate_dir <- here("Paper_wos", "data", "intermediate")

archivos_endogeno <- list.files(endogeno_dir, pattern = "\\.txt$", full.names = TRUE)
archivos_laboratorio <- list.files(laboratorio_dir, pattern = "\\.txt$", full.names = TRUE)

if (length(archivos_endogeno) == 0L || length(archivos_laboratorio) == 0L) {
  stop("Faltan TXT originales en las carpetas endogeno o laboratorio.")
}

base_endogeno_original <- convert2df(
  archivos_endogeno, dbsource = "wos", format = "plaintext"
) |>
  as_tibble()

base_laboratorio_original <- convert2df(
  archivos_laboratorio, dbsource = "wos", format = "plaintext"
) |>
  as_tibble()

# Estos conteos fijan el punto de partida del paper. Si los TXT cambian, el
# script se detiene para que la actualizacion sea revisada y documentada.
if (nrow(base_endogeno_original) != 716L ||
    anyDuplicated(base_endogeno_original$UT) > 0L) {
  stop("La busqueda endogena original ya no contiene 716 UT unicas.")
}

if (nrow(base_laboratorio_original) != 839L ||
    anyDuplicated(base_laboratorio_original$UT) > 0L) {
  stop("La busqueda laboratorio original ya no contiene 839 UT unicas.")
}

# Se unen las busquedas solo para recuperar todos los candidatos. Un paper
# presente en ambas se conserva una vez mediante UT  (esto es porque despues de la recuperacion manual de afiliaciones que hizo isi2, hay un paper qeu solo aparece en lab que tiene afiliación latam).
candidatos_originales <- bind_rows(
  base_endogeno_original |>
    mutate(en_busqueda_endogena = TRUE, en_busqueda_laboratorio = FALSE),
  base_laboratorio_original |>
    mutate(en_busqueda_endogena = FALSE, en_busqueda_laboratorio = TRUE)
) |>
  group_by(UT) |>
  mutate(
    en_busqueda_endogena = any(en_busqueda_endogena),
    en_busqueda_laboratorio = any(en_busqueda_laboratorio)
  ) |>
  slice(1L) |>
  ungroup()

n_overlap_original <- sum(
  candidatos_originales$en_busqueda_endogena &
    candidatos_originales$en_busqueda_laboratorio
)

if (nrow(candidatos_originales) != 1137L || n_overlap_original != 418L) {
  stop("La union original debe contener 1.137 papers y 418 coincidencias.")
}

# 2. Integracion de la revision manual --------------------------------------

revision_original <- read_excel(
  file.path(intermediate_dir, "base_octubre.xlsx")
)

if (anyDuplicated(revision_original$UT) > 0L) {
  stop("base_octubre.xlsx contiene UT duplicadas.")
}

# Devuelve NA cuando una columna opcional no existe.
columna_o_na <- function(data, nombre) {
  if (nombre %in% names(data)) {
    as.character(data[[nombre]])
  } else {
    rep(NA_character_, nrow(data))
  }
}

# Reune las correcciones de C1 o C3 registradas por posicion de autor.
combinar_columnas_manuales <- function(data, columnas) {
  if (length(columnas) == 0L) {
    return(rep(NA_character_, nrow(data)))
  }

  apply(data[, columnas, drop = FALSE], 1L, function(valores) {
    entradas <- valores |>
      as.character() |>
      str_split(";", simplify = FALSE) |>
      unlist() |>
      str_squish() |>
      discard(~ is.na(.x) || .x == "") |>
      unique()

    if (length(entradas) == 0L) NA_character_ else str_c(entradas, collapse = "; ")
  })
}

# Conserva el vinculo por posicion entre cada autor y las afiliaciones C1/C3
# revisadas por Isidora. La posicion se extrae exclusivamente del numero entre
# _aut y _manual: el 12 de c3_aut12_manual corresponde al autor 12.
# El mapa anidado evita sumar 72 columnas a la base oficial y permite construir
# la base larga sin volver a cargar el Excel de revision.
construir_mapa_autor_afiliacion <- function(data, columnas_c1, columnas_c3) {
  posicion_columna <- function(columnas) {
    posiciones <- str_match(columnas, "_aut([0-9]+)_manual$")[, 2]
    setNames(columnas, posiciones)
  }

  columnas_c1_pos <- posicion_columna(columnas_c1)
  columnas_c3_pos <- posicion_columna(columnas_c3)
  autores_af <- columna_o_na(data, "AF")
  autores_au <- columna_o_na(data, "AU")

  valor_por_posicion <- function(data, fila, columnas_pos, posicion) {
    columna <- unname(columnas_pos[as.character(posicion)])
    if (length(columna) == 0L || is.na(columna)) {
      return(NA_character_)
    }

    valor <- str_squish(as.character(data[[columna]][fila]))
    if (is.na(valor) || valor == "") NA_character_ else valor
  }

  map(seq_len(nrow(data)), function(fila) {
    autores_texto <- autores_af[fila]
    if (is.na(autores_texto) || str_squish(autores_texto) == "") {
      autores_texto <- autores_au[fila]
    }

    autores <- autores_texto |>
      coalesce("") |>
      str_split(";", simplify = FALSE) |>
      unlist() |>
      str_squish() |>
      discard(~ is.na(.x) || .x == "")

    posiciones_disponibles <- sort(unique(as.integer(c(
      names(columnas_c1_pos), names(columnas_c3_pos)
    ))))
    posiciones_con_datos <- keep(posiciones_disponibles, function(posicion) {
      c1 <- valor_por_posicion(data, fila, columnas_c1_pos, posicion)
      c3 <- valor_por_posicion(data, fila, columnas_c3_pos, posicion)
      !is.na(c1) || !is.na(c3)
    })

    # En un registro de la revision, la misma institucion del unico autor fue
    # copiada tambien en las posiciones 2 a 5. Esas copias redundantes se
    # ignoran. Una afiliacion distinta fuera del rango sigue siendo un error,
    # porque no podria vincularse de forma defendible con una persona.
    posiciones_fuera <- posiciones_con_datos[
      posiciones_con_datos > length(autores)
    ]

    if (length(posiciones_fuera) > 0L) {
      valores_fuera <- c(
        map_chr(
          posiciones_fuera,
          ~ valor_por_posicion(data, fila, columnas_c1_pos, .x)
        ),
        map_chr(
          posiciones_fuera,
          ~ valor_por_posicion(data, fila, columnas_c3_pos, .x)
        )
      ) |>
        discard(is.na) |>
        unique()

      posiciones_validas <- seq_along(autores)
      valores_validos <- c(
        map_chr(
          posiciones_validas,
          ~ valor_por_posicion(data, fila, columnas_c1_pos, .x)
        ),
        map_chr(
          posiciones_validas,
          ~ valor_por_posicion(data, fila, columnas_c3_pos, .x)
        )
      ) |>
        discard(is.na) |>
        unique()

      if (length(setdiff(valores_fuera, valores_validos)) > 0L) {
        stop(
          "Hay C1/C3 manual fuera del rango de autores en ",
          as.character(data$UT[fila])
        )
      }
    }

    if (length(autores) == 0L) {
      return(tibble(
        author_position = integer(),
        author_name = character(),
        c1_manual = character(),
        c3_manual = character()
      ))
    }

    tibble(
      author_position = seq_along(autores),
      author_name = autores,
      c1_manual = map_chr(
        author_position,
        ~ valor_por_posicion(data, fila, columnas_c1_pos, .x)
      ),
      c3_manual = map_chr(
        author_position,
        ~ valor_por_posicion(data, fila, columnas_c3_pos, .x)
      )
    )
  })
}
# Agrega la correccion manual sin borrar informacion original valida.
combinar_original_y_manual <- function(original, manual) {
  map2_chr(original, manual, function(original_i, manual_i) {
    entradas <- c(original_i, manual_i) |>
      as.character() |>
      str_split(";", simplify = FALSE) |>
      unlist() |>
      str_squish() |>
      discard(~ is.na(.x) || .x == "") |>
      unique()

    if (length(entradas) == 0L) NA_character_ else str_c(entradas, collapse = "; ")
  })
}

c1_manual_cols <- grep(
  "^c1_aut[0-9]+_manual$", names(revision_original), value = TRUE
)
c3_manual_cols <- grep(
  "^c3_aut[0-9]+_manual$", names(revision_original), value = TRUE
)

author_affiliation_map <- construir_mapa_autor_afiliacion(
  revision_original, c1_manual_cols, c3_manual_cols
)

# 2b. Correcciones del chequeo manual de Tania (9-nov-2026) ------------------
# Origen: revision manual de Tania Hutt sobre los papers cuyo universo LatAm
# (affi_latam_countries) no tenia ningun autor LatAm a nivel de autor en
# Authors_long. En los tres casos la direccion LatAm ya estaba en C1 del paper,
# pero el vinculo autor-afiliacion (author_affiliation_map) la dejaba sin
# asignar o la asignaba a otro pais. Estas correcciones se aplican por UT +
# posicion de autor y se validan contra el nombre del autor.
#
# modo = "reemplaza": sustituye el c1/c3 que tenia el autor en el mapa.
# modo = "agrega":    suma la afiliacion a la que el autor ya tenia (el autor
#                     declara mas de una afiliacion).
#
# Cambios (antes -> despues):
#  1. WOS:001204896900001 (Voters' expectations...), pos. 4 MURR, ANDREAS E.
#     UNIV WARWICK (England) -> CIDE, Mexico City, MEXICO. Reemplaza. Tania:
#     "Andreas E. Murr es de afiliacion en Mexico". La direccion de Warwick
#     sigue en C1 del paper, pero queda sin asignar a un autor.
#  2. WOS:000388055800017 (Willingness to accept wind...), pos. 1 GARCIA, JORGE H.
#     CICERO (Noruega) -> CICERO (Noruega) + Pontificia Univ. Javeriana, Bogota,
#     COLOMBIA. Agrega. Tania: "Jorge Garcia tiene afiliacion en Colombia (y es
#     primer autor)". CICERO se conserva porque es la direccion del corresponsal (RP).
#  3. WOS:001541495600001 (Value of accessibility...), pos. 2 HERNANDEZ, JOSE IGNACIO
#     POPULYTICS (Paises Bajos) -> Univ. San Sebastian, Concepcion, CHILE.
#     Reemplaza. Tania: "Jose Ignacio Hernandez tiene afiliacion en Chile".
#     Populytics sigue en C1 del paper, pero queda sin asignar a un autor.
#  4. WOS:000796681800001 (Priors rule...), pos. 1 ARIAS, ERIC. Tania confirma que
#     el primer autor NO tiene afiliacion. No cambia ningun dato en este script:
#     el caso ya figura en confirmed_no_affiliation (05_descriptivos...). Se
#     deja constancia aqui y se actualiza la nota en 05.
# Ademas, el C3 del paper 2 no incluia a CIDE; se agrega a C3_manual.

correcciones_tania <- tribble(
  ~UT, ~author_position, ~autor_esperado, ~modo, ~c1_nuevo, ~c3_nuevo,
  "WOS:001204896900001", 4L, "MURR", "reemplaza",
  "CTR INVEST Y DOCENCIA ECON CIDE, DIV ESTUDIOS POLIT, MEXICO CITY, MEXICO.",
  "CENTRO DE INVESTIGACION Y DOCENCIA ECONOMICAS",
  "WOS:000388055800017", 1L, "GARCIA", "agrega",
  "PONTIFICIA UNIV JAVERIANA, DEPT ECON, VISITING FAC, BOGOTA, COLOMBIA.",
  "PONTIFICIA UNIVERSIDAD JAVERIANA",
  "WOS:001541495600001", 2L, "HERNANDEZ", "reemplaza",
  "UNIV SAN SEBASTIAN, FAC ECON, CTR ECON & SUSTAINABLE DEV, BUSINESS & GOVT, LIENTUR 1457, CONCEPCION, CHILE.",
  "UNIVERSIDAD SAN SEBASTIAN"
)

aplicar_correcciones_tania <- function(mapas, uts, correcciones) {
  for (i in seq_len(nrow(correcciones))) {
    corr <- correcciones[i, ]
    fila <- which(uts == corr$UT)
    if (length(fila) != 1L) {
      stop("Correccion de Tania: UT no encontrada o duplicada: ", corr$UT)
    }
    mapa <- mapas[[fila]]
    j <- which(mapa$author_position == corr$author_position)
    if (length(j) != 1L || !str_detect(mapa$author_name[j], corr$autor_esperado)) {
      stop(
        "Correccion de Tania: la posicion ", corr$author_position, " de ",
        corr$UT, " no corresponde a ", corr$autor_esperado
      )
    }
    antes_c1 <- mapa$c1_manual[j]
    if (corr$modo == "agrega") {
      mapa$c1_manual[j] <- str_c(
        c(antes_c1, corr$c1_nuevo)[!is.na(c(antes_c1, corr$c1_nuevo))],
        collapse = "; "
      )
      mapa$c3_manual[j] <- str_c(
        c(mapa$c3_manual[j], corr$c3_nuevo)[
          !is.na(c(mapa$c3_manual[j], corr$c3_nuevo))
        ],
        collapse = "; "
      )
    } else {
      mapa$c1_manual[j] <- corr$c1_nuevo
      mapa$c3_manual[j] <- corr$c3_nuevo
    }
    cat(
      "Correccion Tania [", corr$modo, "] ", corr$UT, " pos. ",
      corr$author_position, " ", mapa$author_name[j], "\n  C1 antes: ",
      antes_c1, "\n  C1 ahora: ", mapa$c1_manual[j], "\n", sep = ""
    )
    mapas[[fila]] <- mapa
  }
  mapas
}

author_affiliation_map <- aplicar_correcciones_tania(
  author_affiliation_map, revision_original$UT, correcciones_tania
)

revision_manual <- revision_original |>
  mutate(
    C1_manual = combinar_columnas_manuales(
      pick(all_of(c1_manual_cols)), c1_manual_cols
    ),
    C3_manual = combinar_columnas_manuales(
      pick(all_of(c3_manual_cols)), c3_manual_cols
    ),
    author_affiliation_map = author_affiliation_map
  ) |>
  transmute(
    UT,
    AB_manual = columna_o_na(revision_original, "AB_manual"),
    DE_manual = columna_o_na(revision_original, "DE_manual"),
    C1_manual,
    C3_manual,
    author_affiliation_map,
    review_status = columna_o_na(revision_original, "review_status"),
    exclude_reason = columna_o_na(revision_original, "exclude_reason"),
    review_notes = columna_o_na(revision_original, "review_notes"),
    reviewer = columna_o_na(revision_original, "reviewer"),
    comentario_revision = columna_o_na(revision_original, "comentario_revision")
  )

# Chequeo manual de Tania: C3 del paper 2 (WOS:001204896900001) no incluia a CIDE.
revision_manual <- revision_manual |>
  mutate(
    C3_manual = if_else(
      UT == "WOS:001204896900001",
      combinar_original_y_manual(
        C3_manual, "CENTRO DE INVESTIGACION Y DOCENCIA ECONOMICAS"
      ),
      C3_manual
    )
  )

candidatos_corregidos <- candidatos_originales |>
  left_join(revision_manual, by = "UT") |>
  mutate(
    C1_original = C1,
    C3_original = C3,
    AB = if_else(
      !is.na(AB_manual) & str_squish(AB_manual) != "",
      AB_manual,
      AB
    ),
    C1 = combinar_original_y_manual(C1, C1_manual),
    C3 = combinar_original_y_manual(C3, C3_manual),
    curation_c1 = if_else(
      is.na(C1_manual) | str_squish(C1_manual) == "",
      "wos_original",
      "wos_original_plus_manual"
    ),
    curation_c3 = if_else(
      is.na(C3_manual) | str_squish(C3_manual) == "",
      "wos_original",
      "wos_original_plus_manual"
    )
  )

# 3. Aplicacion de exclusiones documentadas --------------------------------

# Las exclusiones de la ayudante deben conservar una justificacion en alguna
# de las columnas de revision.
exclusiones_revision <- revision_manual |>
  filter(review_status == "exclude") |>
  mutate(
    motivo = coalesce(
      na_if(str_squish(exclude_reason), ""),
      na_if(str_squish(review_notes), ""),
      na_if(str_squish(comentario_revision), "")
    )
  ) |>
  select(UT, motivo)

if (any(is.na(exclusiones_revision$motivo))) {
  stop("Hay exclusiones manuales sin justificacion documentada.")
}

# Decisiones adicionales acordadas por el equipo.
exclusiones_equipo <- tribble(
  ~UT, ~motivo,
  "WOS:000423043100005",
  "Diseno experimental fuera del interes del paper",
  "WOS:001003209400004",
  "Gelatina y peliculas de almidon; fuera de ciencias sociales",
  "WOS:001425530000002",
  "Duplicado sobre gelatina y fuera de ciencias sociales",
  "WOS:000331604200005",
  "Resena de Roberts",
  "WOS:000412957000002",
  "Duplicado: excluir volumen 99 y conservar volumen 94",
  "WOS:001534810300002",
  "Duplicado: excluir version del 9 de mayo y conservar la del 15 de febrero"
)

papers_excluir <- union(exclusiones_revision$UT, exclusiones_equipo$UT)

if (length(papers_excluir) != 48L) {
  stop("Se esperaban 48 exclusiones unicas.")
}

candidatos_elegibles <- candidatos_corregidos |>
  filter(!UT %in% papers_excluir)

if (nrow(candidatos_elegibles) != 1089L ||
    anyDuplicated(candidatos_elegibles$UT) > 0L) {
  stop("Despues de excluir deben quedar 1.089 candidatos con UT unica.")
}

# 4. Seleccion de papers con afiliacion LatAm -------------------------------

# El corte mantiene los 14 paises de las consultas WoS. C1 permanece completo,
# por lo que despues se podran identificar todos los demas paises de coautoria.
paises_busqueda_latam <- tribble(
  ~country_raw, ~country,
  "ARGENTINA", "Argentina",
  "BOLIVIA", "Bolivia",
  "BRAZIL", "Brazil",
  "CHILE", "Chile",
  "COLOMBIA", "Colombia",
  "COSTA RICA", "Costa Rica",
  "CUBA", "Cuba",
  "DOMINICAN REP", "Dominican Rep",
  "ECUADOR", "Ecuador",
  "GUATEMALA", "Guatemala",
  "MEXICO", "Mexico",
  "PERU", "Peru",
  "URUGUAY", "Uruguay",
  "VENEZUELA", "Venezuela"
)

# El pais se extrae del final de cada direccion. Esto evita confundir
# New Mexico, USA, con Mexico.
afiliaciones_latam <- candidatos_elegibles |>
  select(UT, C1) |>
  separate_rows(C1, sep = ";") |>
  mutate(
    address = str_squish(C1),
    country_raw = address |>
      str_remove("[.;[:space:]]+$") |>
      str_extract("[^,]+$") |>
      str_squish() |>
      str_to_upper()
  ) |>
  inner_join(paises_busqueda_latam, by = "country_raw") |>
  distinct(UT, country)

paises_latam_por_paper <- afiliaciones_latam |>
  group_by(UT) |>
  summarise(
    affi_latam_countries = str_c(sort(unique(country)), collapse = "; "),
    .groups = "drop"
  )

base_oficial <- candidatos_elegibles |>
  inner_join(paises_latam_por_paper, by = "UT") |>
  mutate(affi_latam = TRUE)

if (nrow(base_oficial) != 675L || anyDuplicated(base_oficial$UT) > 0L) {
  stop("La base oficial debe contener exactamente 675 papers con UT unica.")
}

# Deben quedar 674 papers de la busqueda endogena original y un paper recuperado
# desde la otra busqueda cuya afiliacion argentina fue completada manualmente.
recuperados_fuera_busqueda_endogena <- base_oficial |>
  filter(!en_busqueda_endogena) |>
  pull(UT)

if (
  sum(base_oficial$en_busqueda_endogena) != 674L ||
    !identical(recuperados_fuera_busqueda_endogena, "WOS:001488694300001")
) {
  stop("No se reproduce la conciliacion esperada de 674 + 1 papers.")
}

# Control especifico de los cinco falsos positivos encontrados.
falsos_new_mexico <- c(
  "WOS:000560503100006",
  "WOS:001042741200001",
  "WOS:001277827200001",
  "WOS:001684128300001",
  "WOS:001740801800001"
)

if (any(falsos_new_mexico %in% base_oficial$UT)) {
  stop("Un paper afiliado solo a New Mexico fue clasificado como Mexico.")
}

# Desde este punto se trabaja exclusivamente con los 675 papers oficiales.

# 5. Abstracts y keywords ---------------------------------------------------

base_oficial <- base_oficial |>
  mutate(
    keywords_autor = case_when(
      !is.na(DE) & str_squish(DE) != "" ~ DE,
      !is.na(DE_manual) & str_squish(DE_manual) != "" ~ DE_manual,
      TRUE ~ NA_character_
    ),
    keywords_analisis = case_when(
      !is.na(keywords_autor) & str_squish(keywords_autor) != "" ~ keywords_autor,
      !is.na(KW_Merged) & str_squish(KW_Merged) != "" ~ KW_Merged,
      TRUE ~ NA_character_
    ),
    keywords_fuente = case_when(
      !is.na(DE) & str_squish(DE) != "" ~ "author_original",
      !is.na(DE_manual) & str_squish(DE_manual) != "" ~ "author_manual",
      !is.na(KW_Merged) & str_squish(KW_Merged) != "" ~ "wos_generated",
      TRUE ~ "missing"
    )
  )

if (any(is.na(base_oficial$AB) | str_squish(base_oficial$AB) == "")) {
  stop("La base oficial contiene papers sin abstract.")
}

if (any(
  is.na(base_oficial$keywords_analisis) |
    str_squish(base_oficial$keywords_analisis) == ""
)) {
  stop("La base oficial contiene papers sin keywords disponibles.")
}

# 6. Core areas -------------------------------------------------------------

# Un paper puede pertenecer a mas de una core area. Las areas centrales se
# mapean explicitamente; las demas categorias sociales quedan en Other y se
# conservan sin modificar en WC para mantener trazabilidad.
mapa_core_area <- tribble(
  ~core_area, ~WC,
  "Business/Management", "Management",
  "Business/Management", "Business",
  "Business/Management", "Business, Finance",
  "Business/Management", "Operations Research & Management Science",
  "Business/Management", "Management Science",
  "Economics", "Economics",
  "Economics", "Agricultural Economics & Policy",
  "Economics", "Economics & Policy",
  "Political Science", "Political Science",
  "Political Science", "International Relations",
  "Political Science", "Public Administration",
  "Psychology", "Psychology, Multidisciplinary",
  "Psychology", "Psychology, Clinical",
  "Psychology", "Psychology, Applied",
  "Psychology", "Psychology, Experimental",
  "Psychology", "Psychology, Developmental",
  "Psychology", "Psychology",
  "Psychology", "Psychology, Social",
  "Psychology", "Psychology, Educational",
  "Psychology", "Psychology, Psychoanalysis",
  "Psychology", "Psychology, Biological",
  "Psychology", "Psychology, Mathematical",
  "Sociology", "Sociology",
  "Other", "Education & Educational Research",
  "Other", "Social Sciences, Interdisciplinary",
  "Other", "Communication",
  "Other", "Urban Studies",
  "Other", "Women's Studies",
  "Other", "Social Issues",
  "Other", "Social Sciences, Mathematical Methods",
  "Other", "Family Studies",
  "Other", "Development Studies",
  "Other", "Cultural Studies",
  "Other", "Demography",
  "Other", "Social Sciences, Biomedical"
) |>
  mutate(WC = str_to_upper(WC))

core_area_long <- base_oficial |>
  select(UT, WC) |>
  separate_rows(WC, sep = ";") |>
  mutate(WC = str_to_upper(str_squish(WC))) |>
  filter(!is.na(WC), WC != "") |>
  left_join(mapa_core_area, by = "WC") |>
  mutate(
    core_area_mapping_status = if_else(
      is.na(core_area), "assigned_to_other", "explicit_mapping"
    ),
    core_area = replace_na(core_area, "Other")
  ) |>
  distinct(UT, WC, core_area, .keep_all = TRUE)

core_area_por_paper <- core_area_long |>
  group_by(UT) |>
  summarise(
    core_area = str_c(sort(unique(core_area)), collapse = "; "),
    core_area_mapping_status = if_else(
      any(core_area_mapping_status == "assigned_to_other"),
      "contains_other_wc",
      "fully_explicit"
    ),
    .groups = "drop"
  )

base_oficial <- base_oficial |>
  left_join(core_area_por_paper, by = "UT")

if (any(is.na(base_oficial$core_area) | str_squish(base_oficial$core_area) == "")) {
  stop("Hay papers sin core area asignada.")
}

# 7. Tipo de diseno experimental -------------------------------------------

# La clasificacion admite mas de un metodo por paper. Las categorias genericas
# solo se conservan cuando no existe un diseno mas informativo.
patrones_des_exp <- tribble(
  ~des_exp, ~patron,
  "Field experiment",
  "\\bfield experiment[a-z]*\\b|\\bfield-experiment[a-z]*\\b|\\bfield: experiment[a-z]*\\b",
  "Survey experiment",
  "\\bsurvey experiment[a-z]*\\b|\\bsurvey-experiment[a-z]*\\b",
  "Audit study",
  "\\baudit stud[a-z]*\\b",
  "Correspondence experiment/audit",
  "\\bcorrespondence experiment[a-z]*\\b|\\bcorrespondence audit[a-z]*\\b|\\bcorrespondence test[a-z]*\\b",
  "Conjoint",
  "\\bconjoint experiment[a-z]*\\b|\\bconjoint analy[a-z]*\\b|\\bconjoint-analy[a-z]*\\b|\\bconjoint stud[a-z]*\\b|\\bconjoint-study\\b|\\bconjoint study\\b",
  "List experiment",
  "\\blist experiment[a-z]*\\b",
  "Factorial experiment/survey",
  "\\bfactorial experiment[a-z]*\\b|\\bfactorial survey[a-z]*\\b",
  "Vignette experiment/survey",
  "\\bvignette experiment[a-z]*\\b|\\bvignette survey[a-z]*\\b",
  "Resume/CV audit",
  "\\bresume audit[a-z]*\\b|\\bcv audit[a-z]*\\b",
  "Choice experiment",
  "\\bchoice experiment[a-z]*\\b",
  "Online experiment",
  "\\bonline experiment[a-z]*\\b|\\bonline-experiment[a-z]*\\b",
  "Experimental methods",
  "\\bexperimental method[a-z]*\\b|\\bexperimental evidence\\b|\\bexperiment[a-z]* from\\b|\\bexperiment[a-z]* in\\b|\\bexperimental study\\b|\\bexperimental studies\\b",
  "Other experimental design",
  "\\bpreferential looking task\\b|\\blexical decision\\b|\\bsimulation\\b|\\bpractical illustration[a-z]*\\b"
)

menciones_des_exp <- base_oficial |>
  transmute(
    UT,
    texto_exp = str_to_lower(str_c(
      replace_na(TI, ""),
      replace_na(AB, ""),
      replace_na(keywords_analisis, ""),
      sep = " "
    ))
  ) |>
  crossing(patrones_des_exp) |>
  filter(str_detect(texto_exp, regex(patron, ignore_case = TRUE))) |>
  distinct(UT, des_exp)

des_exp_especifico <- c(
  "Field experiment",
  "Survey experiment",
  "Audit study",
  "Correspondence experiment/audit",
  "Conjoint",
  "List experiment",
  "Factorial experiment/survey",
  "Vignette experiment/survey",
  "Resume/CV audit",
  "Choice experiment"
)

des_exp_por_paper <- menciones_des_exp |>
  group_by(UT) |>
  mutate(
    tiene_especifico = any(des_exp %in% des_exp_especifico),
    tiene_online = any(des_exp == "Online experiment"),
    tiene_generico = any(des_exp == "Experimental methods")
  ) |>
  filter(
    des_exp %in% des_exp_especifico |
      (!tiene_especifico & des_exp == "Online experiment") |
      (!tiene_especifico & !tiene_online & des_exp == "Experimental methods") |
      (!tiene_especifico & !tiene_online & !tiene_generico &
         des_exp == "Other experimental design")
  ) |>
  summarise(
    des_exp = str_c(sort(unique(des_exp)), collapse = "; "),
    .groups = "drop"
  )

papers_survey_manual <- c("WOS:001661562700001", "WOS:001730690100001")

base_oficial <- base_oficial |>
  left_join(des_exp_por_paper, by = "UT") |>
  mutate(
    des_exp = if_else(
      UT %in% papers_survey_manual, "Survey experiment", des_exp
    ),
    des_exp_missing = is.na(des_exp) | str_squish(des_exp) == ""
  )

# 8. Variables y controles finales -----------------------------------------

# Las marcas de las dos busquedas no se guardan: solo se usaron para auditar el
# origen. La base oficial conserva los campos necesarios para descriptivos,
# autores, afiliaciones, liderazgo, metodos y topic modeling.
columnas_oficiales <- c(
  "UT", "TI", "PY", "SO", "DI", "DT", "WE", "AU", "AF", "RP",
  "C1", "C1_original", "C1_manual", "C3", "C3_original", "C3_manual",
  "author_affiliation_map",
  "curation_c1", "curation_c3", "affi_latam", "affi_latam_countries",
  "AB", "AB_manual", "DE", "DE_manual", "ID", "KW_Merged",
  "keywords_autor", "keywords_analisis", "keywords_fuente",
  "WC", "core_area", "core_area_mapping_status",
  "des_exp", "des_exp_missing", "TC", "DA", "CR",
  "review_status", "exclude_reason", "review_notes", "reviewer",
  "comentario_revision"
)

base_oficial <- base_oficial |>
  select(any_of(columnas_oficiales)) |>
  arrange(as.integer(PY), UT)

if (nrow(base_oficial) != 675L || n_distinct(base_oficial$UT) != 675L) {
  stop("La base oficial final no conserva 675 papers unicos.")
}

# Verifica que el mapa anidado conserve una posicion unica por autor y que
# ninguna afiliacion manual quede desligada de un nombre. Estos controles son
# necesarios antes de construir la base larga de autores en descriptivos.
mapa_autor_control <- base_oficial |>
  select(UT, author_affiliation_map) |>
  unnest(author_affiliation_map)

n_filas_autor_paper <- nrow(mapa_autor_control)
n_c1_manuales <- sum(
  !is.na(mapa_autor_control$c1_manual) &
    str_squish(mapa_autor_control$c1_manual) != ""
)
n_c3_manuales <- sum(
  !is.na(mapa_autor_control$c3_manual) &
    str_squish(mapa_autor_control$c3_manual) != ""
)

# Estos totales corresponden a la revision manual vigente. Si cambian, el
# script se detiene antes de sobrescribir la base oficial y obliga a revisar
# si se modifico el archivo de entrada o el vinculo por posicion de autor.
if (
  n_filas_autor_paper != 2419L ||
    n_c1_manuales != 1844L ||
    n_c3_manuales != 1900L
) {
  stop(
    "El mapa autor-afiliacion no coincide con la revision manual vigente: ",
    n_filas_autor_paper, " filas autor-paper; ",
    n_c1_manuales, " C1 manuales; ",
    n_c3_manuales, " C3 manuales."
  )
}

if (anyDuplicated(mapa_autor_control[c("UT", "author_position")]) > 0L) {
  stop("El mapa autor-afiliacion contiene posiciones duplicadas por paper.")
}

if (any(
  (is.na(mapa_autor_control$author_name) |
    str_squish(mapa_autor_control$author_name) == "") &
    (!is.na(mapa_autor_control$c1_manual) |
      !is.na(mapa_autor_control$c3_manual))
)) {
  stop("Hay afiliaciones manuales que no pueden vincularse con un autor.")
}

if (
  any(is.na(base_oficial$UT)) ||
    any(is.na(base_oficial$TI)) ||
    any(is.na(base_oficial$PY))
) {
  stop("Faltan identificadores, titulos o anios en la base oficial.")
}

doi_repetidos <- base_oficial |>
  filter(!is.na(DI), str_squish(DI) != "") |>
  count(DI) |>
  filter(n > 1L)

titulos_repetidos <- base_oficial |>
  mutate(titulo_control = TI |> str_to_upper() |> str_squish()) |>
  count(titulo_control) |>
  filter(n > 1L)

if (nrow(doi_repetidos) > 0L || nrow(titulos_repetidos) > 0L) {
  stop("Persisten DOI o titulos exactos duplicados en la base oficial.")
}

# 9. Unico output analitico de 00 ------------------------------------------

saveRDS(
  base_oficial,
  file.path(intermediate_dir, "base_oficial.rds")
)

# Resumen visible al ejecutar. No crea archivos auxiliares.
cat("\nBase oficial creada correctamente\n")
cat("Papers:", nrow(base_oficial), "\n")
cat("Filas autor-paper:", n_filas_autor_paper, "\n")
cat("Celdas C1 manuales:", n_c1_manuales, "\n")
cat("Celdas C3 manuales:", n_c3_manuales, "\n")
cat(
  "Periodo:",
  min(as.integer(base_oficial$PY)),
  "-",
  max(as.integer(base_oficial$PY)),
  "\n"
)
cat("Hasta 2025:", sum(as.integer(base_oficial$PY) <= 2025L), "\n")
cat("Papers 2026:", sum(as.integer(base_oficial$PY) == 2026L), "\n")
cat("Sin metodo especifico:", sum(base_oficial$des_exp_missing), "\n")
print(count(base_oficial, keywords_fuente, name = "n_papers"))

