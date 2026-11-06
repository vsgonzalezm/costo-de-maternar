#-----------------------------------------------
# 00 inicial - Cargar bases WoS originales
# Valentina Gonzalez Madariaga
# Agosto 2026
#-----------------------------------------------

library(tidyverse)
library(bibliometrix)
library(here)
library(writexl)
# Fijar rutas de las carpetas con archivos txt
raw_dir <- here("Paper_wos", "data", "raw")
endogeno_dir <- here("Paper_wos", "data", "raw", "endogeno")
laboratorio_dir <- here("Paper_wos", "data", "raw", "laboratorio")

# Listar los archivos txt de cada base
archivos_endogeno <- list.files(endogeno_dir, pattern = "\\.txt$", full.names = TRUE)
archivos_laboratorio <- list.files(laboratorio_dir, pattern = "\\.txt$", full.names = TRUE)

# Cargar los txt de endogeno en formato data frame
endogeno <- convert2df(
  file = archivos_endogeno,
  dbsource = "wos",
  format = "plaintext"
) |>
  as_tibble()

# Cargar los txt de laboratorio en formato data frame
laboratorio <- convert2df(
  file = archivos_laboratorio,
  dbsource = "wos",
  format = "plaintext"
) |>
  as_tibble()


# Crear base conjunta sin papers duplicados
corpus_unico <- bind_rows(
  endogeno |> mutate(base_endogeno = 1, base_laboratorio = 0),
  laboratorio |> mutate(base_endogeno = 0, base_laboratorio = 1)
) |>
  group_by(UT) |>
  mutate(
    base_endogeno = max(base_endogeno),
    base_laboratorio = max(base_laboratorio)
  ) |>
  slice(1) |>
  ungroup() |>
  mutate(
    # Estas variables conservan la pertenencia al resultado original de cada consulta WoS.
    query_endogeno = base_endogeno,
    query_laboratorio = base_laboratorio
  )

# Incorporar correcciones y decisiones de la revision manual.
revision_manual <- readxl::read_excel(
  here("Paper_wos", "data", "intermediate", "base_octubre.xlsx")
)

if (anyDuplicated(revision_manual$UT) > 0) {
  stop("La base de revision manual contiene UT duplicados.")
}

# Un paper puede tener varias afiliaciones manuales distribuidas por autor.
combinar_columnas_manuales <- function(data, columns) {
  if (length(columns) == 0) {
    return(rep(NA_character_, nrow(data)))
  }

  apply(data[, columns, drop = FALSE], 1, function(values) {
    entries <- values |>
      as.character() |>
      discard(~ is.na(.x) || str_squish(.x) == "") |>
      str_split(";", simplify = FALSE) |>
      unlist() |>
      str_squish() |>
      discard(~ .x == "") |>
      unique()

    if (length(entries) == 0) NA_character_ else str_c(entries, collapse = "; ")
  })
}

revision_manual_original <- revision_manual

# Combina WoS con correcciones manuales sin borrar afiliaciones originales.
# Las correcciones de Isi pueden ser parciales: solo agregamos entradas nuevas o corregidas.
combinar_original_y_manual <- function(original, manual) {
  purrr::map2_chr(original, manual, function(original_i, manual_i) {
    entradas <- c(original_i, manual_i) |>
      as.character() |>
      str_split(";", simplify = FALSE) |>
      unlist() |>
      str_squish() |>
      discard(~ is.na(.x) || .x == "") |>
      unique()

    if (length(entradas) == 0) NA_character_ else str_c(entradas, collapse = "; ")
  })
}

c1_manual_cols <- grep("^c1_aut[0-9]+_manual$", names(revision_manual), value = TRUE)
c3_manual_cols <- grep("^c3_aut[0-9]+_manual$", names(revision_manual), value = TRUE)

# Tabla de auditoria a nivel autor.
# Conserva la clave de WoS, el id revisado por Isi y las entradas manuales.
# Las entradas C1raw/C3raw se mantienen completas para rastrear la fuente original.
columna_o_na <- function(data, nombre) {
  if (nombre %in% names(data)) as.character(data[[nombre]]) else rep(NA_character_, nrow(data))
}

max_autores <- max(as.integer(str_extract(grep("^name_aut[0-9]+$", names(revision_manual_original), value = TRUE), "[0-9]+")), na.rm = TRUE)
autores_afiliaciones_auditoria <- purrr::map_dfr(seq_len(max_autores), function(i) {
  tibble(
    UT = revision_manual_original$UT,
    author_position = i,
    name_wos = columna_o_na(revision_manual_original, paste0("name_aut", i)),
    id_aut = columna_o_na(revision_manual_original, paste0("id_aut", i)),
    c1_entry_wos = columna_o_na(revision_manual_original, paste0("c1_entry", i)),
    c3_entry_wos = columna_o_na(revision_manual_original, paste0("c3_entry", i)),
    c1_manual_author = columna_o_na(revision_manual_original, paste0("c1_aut", i, "_manual")),
    c3_manual_author = columna_o_na(revision_manual_original, paste0("c3_aut", i, "_manual")),
    C1raw_paper = columna_o_na(revision_manual_original, "C1raw"),
    C3raw_paper = columna_o_na(revision_manual_original, "C3"),
    assignment_status = case_when(
      !is.na(c1_manual_author) & str_squish(c1_manual_author) != "" ~ "manual_author_affiliation",
      !is.na(C1raw_paper) & str_detect(C1raw_paper, "\\[") ~ "raw_brackets_available_review_assignment",
      TRUE ~ "paper_level_only_or_missing"
    )
  )
}) |>
  filter(!is.na(name_wos) & str_squish(name_wos) != "")

revision_manual <- revision_manual |>
  mutate(
    C1_manual = combinar_columnas_manuales(pick(all_of(c1_manual_cols)), c1_manual_cols),
    C3_manual = combinar_columnas_manuales(pick(all_of(c3_manual_cols)), c3_manual_cols)
  ) |>
  select(
    UT,
    AB_manual,
    DE_manual,
    C1_manual,
    C3_manual,
    review_status,
    exclude_reason,
    review_notes,
    reviewer,
    comentario_revision
  )

corpus_unico <- corpus_unico |>
  left_join(revision_manual, by = "UT") |>
  mutate(
    # Conservamos el valor original antes de integrar cualquier correccion.
    C1_original = C1,
    C3_original = C3,
    AB = if_else(!is.na(AB_manual) & str_squish(AB_manual) != "", AB_manual, AB),
    C1 = combinar_original_y_manual(C1, C1_manual),
    C3 = combinar_original_y_manual(C3, C3_manual),
    curation_c1 = case_when(
      is.na(C1_manual) | str_squish(C1_manual) == "" ~ "wos_original",
      TRUE ~ "wos_original_plus_manual"
    ),
    curation_c3 = case_when(
      is.na(C3_manual) | str_squish(C3_manual) == "" ~ "wos_original",
      TRUE ~ "wos_original_plus_manual"
    )
  )

# Excluir decisiones de la revision manual y casos resueltos por el equipo.
papers_excluir_revision <- revision_manual |>
  filter(review_status == "exclude") |>
  pull(UT)

papers_excluir_equipo <- c(
  "WOS:000423043100005", # Diseno experimental fuera del interes del paper
  "WOS:001003209400004", # Gelatina y peliculas de almidon
  "WOS:001425530000002", # Duplicado sobre gelatina y fuera de tema
  "WOS:000331604200005", # Resena de Roberts
  "WOS:000412957000002", # Duplicado: excluir version volumen 99; conservar WOS:000401207500003 (volumen 94)
  "WOS:001534810300002"  # Duplicado: excluir version 9 de mayo; conservar WOS:001475174400002 (15 de febrero)
)

papers_excluir <- unique(c(papers_excluir_revision, papers_excluir_equipo))

corpus_unico <- corpus_unico |>
  filter(!UT %in% papers_excluir) |>
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

# Contar disponibilidad y fuente de las keywords despues de las exclusiones.
resumen_keywords <- corpus_unico |>
  summarise(
    con_keywords_autor = sum(!is.na(keywords_autor)),
    sin_keywords_autor = sum(is.na(keywords_autor)),
    con_keywords_analisis = sum(!is.na(keywords_analisis)),
    sin_keywords_analisis = sum(is.na(keywords_analisis))
  )

conteo_keywords_fuente <- corpus_unico |>
  count(keywords_fuente, name = "n_papers")

# Mantener las dos bases consistentes con el corpus corregido.
endogeno <- corpus_unico |>
  filter(base_endogeno == 1)

laboratorio <- corpus_unico |>
  filter(base_laboratorio == 1)


# Dejar solo variables necesarias para revisar y trabajar
columnas_base <- c(
  "AU", "CR", "AB", "AB_manual", "C1", "C1_original", "C1_manual",
  "C3", "C3_original", "C3_manual", "curation_c1", "curation_c3",
  "DA", "KW_Merged", "DE", "DE_manual", "keywords_autor",
  "keywords_analisis", "keywords_fuente", "DI", "ID", "PY", "TC",
  "TI", "UT", "WC", "SO", "query_endogeno", "query_laboratorio",
  "base_endogeno", "base_laboratorio", "has_latam_affiliation",
  "has_latam_lab", "paper_shared", "universe_endogeno",
  "universe_laboratorio", "des_exp_missing", "review_status", "exclude_reason",
  "review_notes", "reviewer", "comentario_revision",
  "core_area_mapping_status"
)
endogeno <- endogeno |> select(any_of(columnas_base))
laboratorio <- laboratorio |> select(any_of(columnas_base))
corpus_unico <- corpus_unico |> select(any_of(c(columnas_base, "base_endogeno", "base_laboratorio")))

# Crear tabla de equivalencias entre WC y core area
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

# Crear base larga de core areas para corpus unico
corpus_unico_core_area_long <- corpus_unico |>
  select(UT, WC, base_endogeno, base_laboratorio) |>
  separate_rows(WC, sep = ";") |>
  mutate(WC = str_to_upper(str_squish(WC))) |>
  left_join(mapa_core_area, by = "WC") |>
  mutate(
    core_area_mapping_status = if_else(is.na(core_area), "unmapped_wc", "mapped_wc"),
    core_area = replace_na(core_area, "Other")
  ) |>
  distinct(UT, core_area, .keep_all = TRUE)

# Agregar core areas resumidas en una columna por paper
core_area_por_paper <- corpus_unico_core_area_long |>
  group_by(UT) |>
  summarise(
    core_area = str_c(unique(core_area), collapse = "; "),
    core_area_mapping_status = if_else(any(core_area_mapping_status == "unmapped_wc"), "contains_unmapped_wc", "fully_mapped"),
    .groups = "drop"
  )

endogeno <- endogeno |>
  left_join(core_area_por_paper, by = "UT")

laboratorio <- laboratorio |>
  left_join(core_area_por_paper, by = "UT")

corpus_unico <- corpus_unico |>
  left_join(core_area_por_paper, by = "UT")

# Control obligatorio: cada paper debe quedar en al menos una core area.
# Si aparece un paper sin mapeo, se detiene el pipeline para revisar el WC
# original antes de producir bases analiticas.
papers_sin_core_area <- corpus_unico |>
  filter(is.na(core_area) | str_squish(core_area) == "")

if (nrow(papers_sin_core_area) > 0) {
  stop(
    "Hay papers sin core area asignada. Revisar WC: ",
    str_c(papers_sin_core_area$UT, collapse = ", ")
  )
}

# Crear tabla de paises LatAm para buscar en C1
paises_latam <- tribble(
  ~pais,
  "Argentina",
  "Bolivia",
  "Brazil",
  "Chile",
  "Colombia",
  "Costa Rica",
  "Cuba",
  "Dominican Rep",
  "Ecuador",
  "Guatemala",
  "Mexico",
  "Peru",
  "Uruguay",
  "Venezuela"
)

# Crear menciones de afiliacion LatAm desde C1
menciones_afiliacion_latam <- corpus_unico |>
  select(UT, C1) |>
  crossing(paises_latam) |>
  mutate(encontrado = str_detect(str_to_lower(C1), regex(str_to_lower(pais), ignore_case = TRUE))) |>
  filter(encontrado) |>
  distinct(UT, pais)

# Agregar paises LatAm de afiliacion en una columna por paper
afiliacion_latam_por_paper <- menciones_afiliacion_latam |>
  group_by(UT) |>
  summarise(afiliacion_latam = str_c(unique(pais), collapse = "; "), .groups = "drop")

endogeno <- endogeno |>
  left_join(afiliacion_latam_por_paper, by = "UT")

laboratorio <- laboratorio |>
  left_join(afiliacion_latam_por_paper, by = "UT")

corpus_unico <- corpus_unico |>
  left_join(afiliacion_latam_por_paper, by = "UT")


# Crear tabla de terminos para identificar investigaciones sobre LatAm
patrones_lab_latam <- tribble(
  ~lab_latam, ~patron,
  "Argentina", "\\bargentina\\b|\\bargentin[a-z]*\\b",
  "Bolivia", "\\bbolivia\\b|\\bbolivian[a-z]*\\b",
  "Brazil", "\\bbrazil[a-z]*\\b|\\bbrasil[a-z]*\\b",
  "Chile", "\\bchile\\b|\\bchilean[a-z]*\\b",
  "Colombia", "\\bcolombia\\b|\\bcolombian[a-z]*\\b",
  "Costa Rica", "\\bcosta rica\\b|\\bcosta rican[a-z]*\\b",
  "Cuba", "\\bcuba\\b|\\bcuban[a-z]*\\b",
  "Dominican Rep", "\\bdominican rep[a-z]*\\b|\\bdominican[a-z]*\\b",
  "Ecuador", "\\becuador\\b|\\becuadorian[a-z]*\\b",
  "Guatemala", "\\bguatemala\\b|\\bguatemalan[a-z]*\\b",
  "Mexico", "\\bmexico\\b|\\bmexic[a-z]*\\b",
  "Peru", "\\bperu\\b|\\bperuvian[a-z]*\\b",
  "Uruguay", "\\buruguay\\b|\\buruguayan[a-z]*\\b",
  "Venezuela", "\\bvenezuela\\b|\\bvenezuelan[a-z]*\\b",
  "Latin America/Regional", "\\blatin america[a-z]*\\b|\\blatinamerican[a-z]*\\b|\\blatin american[a-z]*\\b|\\blatin-american[a-z]*\\b|\\bsouth america[a-z]*\\b|\\bsouthamerican[a-z]*\\b|\\bsouth american[a-z]*\\b|\\bcentral america[a-z]*\\b|\\bcentralamerican[a-z]*\\b|\\bcentral american[a-z]*\\b|\\blatin america and the caribbean\\b|\\blac\\b"
)

# Crear menciones de laboratorio LatAm desde titulo, abstract y keywords
menciones_lab_latam <- corpus_unico |>
  mutate(texto_lab = str_to_lower(str_c(replace_na(TI, ""), replace_na(AB, ""), replace_na(keywords_analisis, ""), sep = " "))) |>
  select(UT, texto_lab) |>
  crossing(patrones_lab_latam) |>
  mutate(encontrado = str_detect(texto_lab, regex(patron, ignore_case = TRUE))) |>
  filter(encontrado) |>
  distinct(UT, lab_latam)

# Agregar menciones LatAm de laboratorio en una columna por paper
lab_latam_por_paper <- menciones_lab_latam |>
  group_by(UT) |>
  summarise(lab_latam = str_c(unique(lab_latam), collapse = "; "), .groups = "drop")

endogeno <- endogeno |>
  left_join(lab_latam_por_paper, by = "UT")

laboratorio <- laboratorio |>
  left_join(lab_latam_por_paper, by = "UT")

corpus_unico <- corpus_unico |>
  left_join(lab_latam_por_paper, by = "UT")
# Indicadores de pertenencia conceptual a los universos.
# La consulta WoS se conserva en query_*; estas variables permiten auditar
# afiliacion LATAM y presencia tematica de LATAM despues de las correcciones.
corpus_unico <- corpus_unico |>
  mutate(
    has_latam_affiliation = !is.na(afiliacion_latam) & str_squish(afiliacion_latam) != "",
    has_latam_lab = !is.na(lab_latam) & str_squish(lab_latam) != "",
    paper_shared = query_endogeno == 1 & query_laboratorio == 1,
    universe_endogeno = has_latam_affiliation,
    universe_laboratorio = has_latam_lab
  )

# Control obligatorio: ningun paper endogeno puede quedar sin afiliacion LATAM.
# Si ocurre, el script se detiene para impedir un corpus conceptualmente incorrecto.
endogeno_sin_afiliacion_latam <- corpus_unico |>
  filter(query_endogeno == 1, !has_latam_affiliation)

if (nrow(endogeno_sin_afiliacion_latam) > 0) {
  stop(
    "Hay papers de la consulta endogena sin afiliacion LATAM detectada en C1. ",
    "Revisar antes de continuar: ",
    str_c(endogeno_sin_afiliacion_latam$UT, collapse = ", ")
  )
}

# Las bases derivadas heredan estas flags del corpus unico.
flags_universo <- corpus_unico |>
  select(UT, query_endogeno, query_laboratorio, has_latam_affiliation, has_latam_lab, paper_shared, universe_endogeno, universe_laboratorio)
endogeno <- endogeno |> left_join(flags_universo, by = "UT")
laboratorio <- laboratorio |> left_join(flags_universo, by = "UT")
# Crear tabla de terminos para identificar disenos experimentales
patrones_des_exp <- tribble(
  ~des_exp, ~patron,
  "Field experiment", "\\bfield experiment[a-z]*\\b|\\bfield-experiment[a-z]*\\b|\\bfield: experiment[a-z]*\\b",
  "Survey experiment", "\\bsurvey experiment[a-z]*\\b|\\bsurvey-experiment[a-z]*\\b",
  "Audit study", "\\baudit stud[a-z]*\\b",
  "Correspondence experiment/audit", "\\bcorrespondence experiment[a-z]*\\b|\\bcorrespondence audit[a-z]*\\b|\\bcorrespondence test[a-z]*\\b",
  "Conjoint", "\\bconjoint experiment[a-z]*\\b|\\bconjoint analy[a-z]*\\b|\\bconjoint-analy[a-z]*\\b|\\bconjoint stud[a-z]*\\b|\\bconjoint-study\\b|\\bconjoint study\\b",
  "List experiment", "\\blist experiment[a-z]*\\b",
  "Factorial experiment/survey", "\\bfactorial experiment[a-z]*\\b|\\bfactorial survey[a-z]*\\b",
  "Vignette experiment/survey", "\\bvignette experiment[a-z]*\\b|\\bvignette survey[a-z]*\\b",
  "Resume/CV audit", "\\bresume audit[a-z]*\\b|\\bcv audit[a-z]*\\b",
  "Choice experiment", "\\bchoice experiment[a-z]*\\b",
  "Online experiment", "\\bonline experiment[a-z]*\\b|\\bonline-experiment[a-z]*\\b",
  "Experimental methods", "\\bexperimental method[a-z]*\\b|\\bexperimental evidence\\b|\\bexperiment[a-z]* from\\b|\\bexperiment[a-z]* in\\b|\\bexperimental study\\b|\\bexperimental studies\\b",
  "Other experimental design", "\\bpreferential looking task\\b|\\blexical decision\\b|\\bsimulation\\b|\\bpractical illustration[a-z]*\\b"
)
# Crear menciones de diseno experimental desde titulo, abstract y keywords
menciones_des_exp <- corpus_unico |>
  mutate(texto_exp = str_to_lower(str_c(replace_na(TI, ""), replace_na(AB, ""), replace_na(keywords_analisis, ""), sep = " "))) |>
  select(UT, texto_exp) |>
  crossing(patrones_des_exp) |>
  mutate(encontrado = str_detect(texto_exp, regex(patron, ignore_case = TRUE))) |>
  filter(encontrado) |>
  distinct(UT, des_exp)

# Definir jerarquia de disenos experimentales
# Online, Experimental methods y Other solo quedan si no hay algo mas especifico
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

# Agregar disenos experimentales en una columna por paper
des_exp_por_paper <- menciones_des_exp |>
  group_by(UT) |>
  mutate(
    tiene_especifico = any(des_exp %in% des_exp_especifico),
    tiene_online = any(des_exp == "Online experiment")
  ) |>
  filter(
    des_exp %in% des_exp_especifico |
      (!tiene_especifico & des_exp == "Online experiment") |
      (!tiene_especifico & !tiene_online & des_exp == "Experimental methods") |
      (!tiene_especifico & !tiene_online & !any(des_exp == "Experimental methods") & des_exp == "Other experimental design")
  ) |>
  summarise(des_exp = str_c(unique(des_exp), collapse = "; "), .groups = "drop")

endogeno <- endogeno |>
  left_join(des_exp_por_paper, by = "UT")

laboratorio <- laboratorio |>
  left_join(des_exp_por_paper, by = "UT")

corpus_unico <- corpus_unico |>
  left_join(des_exp_por_paper, by = "UT")

# Corregir manualmente diseno experimental revisado
papers_survey_manual <- c("WOS:001661562700001", "WOS:001730690100001")

endogeno <- endogeno |>
  mutate(des_exp = if_else(UT %in% papers_survey_manual, "Survey experiment", des_exp))

laboratorio <- laboratorio |>
  mutate(des_exp = if_else(UT %in% papers_survey_manual, "Survey experiment", des_exp))

corpus_unico <- corpus_unico |>
  mutate(des_exp = if_else(UT %in% papers_survey_manual, "Survey experiment", des_exp))

# Marcar papers sin diseno experimental detectado.
# Esto no excluye papers: deja visible que el metodo no pudo inferirse
# automaticamente desde titulo, abstract y keywords.
corpus_unico <- corpus_unico |>
  mutate(des_exp_missing = is.na(des_exp) | str_squish(des_exp) == "")

endogeno <- endogeno |>
  left_join(select(corpus_unico, UT, des_exp_missing), by = "UT")

laboratorio <- laboratorio |>
  left_join(select(corpus_unico, UT, des_exp_missing), by = "UT")

# Contar papers sin diseno experimental detectado
n_sin_des_exp <- sum(corpus_unico$des_exp_missing, na.rm = TRUE)
# Crear subset de papers del corpus total que mencionan Chile
chile_corpus <- corpus_unico |>
  filter(str_detect(lab_latam, "(^|;\\s*)Chile($|;)"))
# Guardar objetos simples para volver a cargarlos despues
saveRDS(endogeno, here("Paper_wos", "data", "intermediate", "endogeno_inicial.rds"))
saveRDS(laboratorio, here("Paper_wos", "data", "intermediate", "laboratorio_inicial.rds"))
saveRDS(corpus_unico, here("Paper_wos", "data", "intermediate", "corpus_unico_inicial.rds"))
saveRDS(corpus_unico_core_area_long, here("Paper_wos", "data", "intermediate", "corpus_unico_core_area_long.rds"))
saveRDS(autores_afiliaciones_auditoria, here("Paper_wos", "data", "intermediate", "autores_afiliaciones_auditoria.rds"))
write_csv(chile_corpus, here("Paper_wos", "data", "intermediate", "chile_corpus.csv"))
# Mostrar resumen basico en consola
print(dim(endogeno))
print(dim(laboratorio))
print(dim(corpus_unico))
print(dim(corpus_unico_core_area_long))
print(n_sin_des_exp)
print(resumen_keywords)
print(conteo_keywords_fuente)
print(names(endogeno))
print(names(laboratorio))
print(names(corpus_unico))
