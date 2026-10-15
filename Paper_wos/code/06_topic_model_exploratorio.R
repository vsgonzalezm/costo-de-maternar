#---------------------------------------------------
# Topic modeling exploratorio del corpus WOS.
#
# Este script trabaja a nivel de paper (UT). Su objetivo es explorar en que
# temas se publica, como cambia esa distribucion en el tiempo y si los temas
# tienen distinta presencia en Endogena, Laboratorio y core_area.
#
# El modelo principal usa titulo + abstract. Las keywords se usan solo en una
# sensibilidad para comprobar si cambian sustantivamente los resultados.
#
# El script no modifica las bases de entrada ni crea bases intermedias.
# Solo escribe un Excel de resultados y figuras en Paper_wos/outputs.
#
# Advertencia metodologica: un topic no es una disciplina ni una categoria
# teorica definitiva. Es una agrupacion estadistica de palabras que aparecen
# juntas. Los nombres finales de los topics deben ser revisados manualmente.
#
# Valentina Gonzalez Madariaga
# Agosto 2026
#-----------------------------------------------

library(tidyverse)
library(tidytext)
library(topicmodels)
library(here)
library(writexl)

# -----------------------------------------------------------------------------
# 1. Parametros reproducibles
# -----------------------------------------------------------------------------
# Esta seccion concentra las decisiones que controlan el analisis. 

seed <- 20260929L # Semilla aleatoria: hace reproducibles los resultados.

k_candidates <- 4:12
# Valores de k que se comparan. k representa el numero de topics que el modelo
# debe encontrar. Probar varios valores evita fijar arbitrariamente un unico k.

k_exploratory <- c(4L, 12L)
# Soluciones que se exportan explicitamente para lectura manual:
# 4 topics entrega una sintesis parsimoniosa y 12 una lectura mas detallada.

n_top_terms <- 12L # Numero de palabras principales que se exportan por topic para interpretarlo.

min_document_frequency <- 3L # Un termino debe aparecer en al menos tres papers para evitar que un caso aislado defina un topic completo.

max_document_proportion <- 1.00
# No se eliminan automaticamente terminos frecuentes. Como el corpus ya esta
# restringido a ciencias sociales, America Latina y experimentos, una palabra
# comun puede ser sustantivamente importante. La frecuencia alta se reporta en
# Common_terms_audit para revisarla antes de decidir.

control_final <- list(burnin = 1000L, iter = 2000L, thin = 100L)
# Parametros del muestreo Gibbs del modelo final:
# burnin descarta iteraciones iniciales, iter define el muestreo total y thin
# guarda una iteracion cada cierto intervalo para reducir dependencia serial.

tables_dir <- here("Paper_wos", "outputs", "tables")
figures_dir <- here("Paper_wos", "outputs", "figures")



# -----------------------------------------------------------------------------
# 2. Cargar y consolidar el corpus final
# -----------------------------------------------------------------------------
# Topic modeling necesita una observacion por documento. Por eso se carga
# corpus_unico_inicial.rds, que ya contiene las correcciones de Isidora,
# exclusiones, abstracts y keywords integradas por 00.

corpus_raw <- readRDS(
  here("Paper_wos", "data", "intermediate", "corpus_unico_inicial.rds")
)

required <- c(
  "UT", "TI", "AB", "DE", "DE_manual", "KW_Merged", "PY", "SO",
  "base_endogeno", "base_laboratorio", "core_area"
)
# Se detiene el script si falta una variable necesaria para identificar el
# documento, construir el texto o comparar los topics por contexto.

missing <- setdiff(required, names(corpus_raw))
if (length(missing) > 0L) {
  stop(
    "Faltan columnas en corpus_unico_inicial.rds: ",
    paste(missing, collapse = ", ")
  )
}

base_flags <- corpus_raw |>
  transmute(
    UT = as.character(UT),
    base_endogeno = replace_na(as.integer(base_endogeno), 0L),
    base_laboratorio = replace_na(as.integer(base_laboratorio), 0L)
  ) |>
  group_by(UT) |>
  summarise(
    base_endogeno = max(base_endogeno, na.rm = TRUE),
    base_laboratorio = max(base_laboratorio, na.rm = TRUE),
    .groups = "drop"
  )
# Un paper puede estar en ambas bases. max() conserva esa pertenencia sin
# duplicar el documento.

corpus <- corpus_raw |>
  transmute(
    UT = as.character(UT),
    year = suppressWarnings(as.integer(PY)),
    title = as.character(TI),
    abstract = as.character(AB),
    journal = as.character(SO),
    DE = as.character(DE),
    DE_manual = as.character(DE_manual),
    KW_Merged = as.character(KW_Merged),
    core_area = as.character(core_area)
  ) |>
  group_by(UT) |>
  summarise(
    year = first(year),
    title = first(title),
    abstract = first(abstract),
    journal = first(journal),
    DE = first(DE),
    DE_manual = first(DE_manual),
    KW_Merged = first(KW_Merged),
    core_area = first(core_area),
    .groups = "drop"
  ) |>
  left_join(base_flags, by = "UT") |>
  mutate(
    # La jerarquia es la misma de 00: keywords originales, manuales y WoS.
    keywords_autor = coalesce(na_if(DE, ""), na_if(DE_manual, "")),
    keywords_analisis = coalesce(
      na_if(keywords_autor, ""),
      na_if(KW_Merged, "")
    )
  )

if (anyDuplicated(corpus$UT) > 0L) {
  stop("Hay UT duplicados despues de consolidar el corpus.")
}

# -----------------------------------------------------------------------------
# 3. Auditar la cobertura de texto
# -----------------------------------------------------------------------------
# Antes de modelar se verifica que los documentos tengan material textual.
# Esta auditoria permite distinguir entre papers sin abstract en los TXT
# originales y papers que ya fueron completados en la base final de 00.

is_present <- function(x) {
  # Devuelve TRUE cuando una variable contiene texto real y no solo NA o vacio.
  !is.na(x) & str_squish(as.character(x)) != ""
}

coverage <- tibble(
  measure = c(
    "Papers in corpus",
    "Papers with title",
    "Papers with abstract",
    "Papers with keywords",
    "Papers with title and abstract"
  ),
  value = c(
    nrow(corpus),
    sum(is_present(corpus$title)),
    sum(is_present(corpus$abstract)),
    sum(is_present(corpus$keywords_analisis)),
    sum(is_present(corpus$title) & is_present(corpus$abstract))
  )
)

corpus <- corpus |>
  mutate(
    text_ti_ab = str_c(
      replace_na(title, ""),
      replace_na(abstract, ""),
      sep = " "
    ),
    text_ti_ab_kw = str_c(
      text_ti_ab,
      replace_na(keywords_analisis, ""),
      sep = " "
    )
  )
# Se construyen dos versiones: la principal usa titulo + abstract y la segunda
# agrega keywords para comprobar si estas alteran la estructura encontrada.

# -----------------------------------------------------------------------------
# 4. Limpiar y tokenizar el texto
# -----------------------------------------------------------------------------
# LDA no recibe parrafos directamente. Primero transforma cada paper en tokens:
# una fila por paper y palabra, con el numero de apariciones de esa palabra.
# Tambien se eliminan stopwords y artefactos que no ayudan a distinguir temas.

# tidytext::stop_words aporta una lista estandar de stopwords en ingles.
# Esta lista adicional contiene palabras de redaccion muy general que no
# deberian definir un topic en este corpus: no son conceptos sustantivos.
# La decision queda explicitada para poder auditarla y modificarla despues.
connector_terms <- c(
  # Conectores y palabras funcionales que no representan temas sustantivos.
  "also", "although", "because", "between", "but", "despite", "either",
  "even", "further", "hence", "however", "moreover", "neither", "nor",
  "otherwise", "rather", "since", "therefore", "thereby", "though", "thus",
  "together", "versus", "whether", "while", "whereas", "which", "whose",
  "using", "used", "use"
)

methodological_terms <- c(
  # Lenguaje metodologico que describe el procedimiento, no el fenomeno.
  "analysis", "analyses", "analyze", "approach", "coefficient", "control",
  "data", "dataset", "design", "designs", "discrete", "effect", "effects",
  "empirical", "experiment", "experimental", "experiments", "estimate",
  "estimates", "evidence", "finding", "findings", "hypothesis", "hypotheses",
  "impact", "intervention", "interventions", "measure", "measures",
  "measurement", "method", "methods", "methodology", "methodological",
  "model", "models", "modeling", "modelling", "observation", "observations",
  "outcome", "outcomes", "participant", "participants", "procedure",
  "procedures", "random", "randomized", "regression", "result", "results",
  "sample", "samples", "survey", "surveys", "treatment", "treatments",
  "variable", "variables"
)

generic_adjectives <- c(
  # Adjetivos academicos generales que suelen aparecer en muchos abstracts y
  # aportan poca capacidad para distinguir un tema de otro.
  "available", "common", "different", "general", "higher", "important",
  "individual", "lower", "main", "necessary", "new", "overall", "particular",
  "possible", "previous", "recent", "relevant", "same", "several",
  "significant", "similar", "specific", "substantial", "various"
)

corpus_generic_terms <- c(
  # Terminos que siguen siendo demasiado transversales en este corpus.
  # Se retiran para que los topics se organicen por campos sustantivos y no
  # por el hecho general de que los papers son experimentales.
  "based", "behavior", "behaviour", "choice", "choices", "countries",
  "country", "evidence", "field", "factors", "global", "individuals",
  "information", "international", "policy", "policies", "preferences",
  "preference", "public", "role", "social", "support", "willingness"
)
artifact_terms <- c(
  # Artefactos HTML/XML y palabras academicas demasiado generales.
  "eacute", "aacute", "iacute", "oacute", "uacute", "ntilde",
  "nbsp", "amp", "rsquo", "ldquo", "rdquo", "ndash", "sic",
  "study", "studies", "paper", "research"
)

generic_terms <- unique(c(
  connector_terms,
  methodological_terms,
  generic_adjectives,
  corpus_generic_terms,
  artifact_terms
))
tokenize_corpus <- function(data, text_column) {
  # Recibe el corpus y el nombre de la columna textual.
  # Devuelve conteos de palabras por paper despues de tokenizar y limpiar.
  data |>
    select(UT, text = all_of(text_column)) |>
    mutate(text = str_to_lower(replace_na(text, ""))) |>
    unnest_tokens(word, text) |>
    anti_join(stop_words, by = "word") |>
    filter(
      str_detect(word, "^[a-z]+$"),
      str_length(word) > 2L,
      !word %in% generic_terms
    ) |>
    count(UT, word, name = "n")
}

filter_tokens <- function(tokens, n_corpus_documents) {
  # Calcula en cuantos documentos aparece cada palabra y aplica los filtros
  # de rareza y universalidad definidos en la seccion de parametros.
  term_frequency <- tokens |>
    count(word, name = "n_documents")

  tokens |>
    inner_join(
      term_frequency |>
        filter(
          n_documents >= min_document_frequency,
          n_documents <= max_document_proportion * n_corpus_documents
        ),
      by = "word"
    ) |>
    select(UT, word, n)
}

tokens_primary_raw <- tokenize_corpus(corpus, "text_ti_ab")
# Tokens del modelo principal antes de quitar terminos raros o universales.

common_terms_audit <- tokens_primary_raw |>
  group_by(word) |>
  summarise(
    n_documents = n_distinct(UT),
    document_proportion = n_documents / nrow(corpus),
    total_occurrences = sum(n),
    .groups = "drop"
  ) |>
  filter(document_proportion >= 0.80) |>
  arrange(desc(document_proportion), desc(total_occurrences))
# Auditoria de palabras presentes en al menos 80% de los papers. Se revisan,
# pero no se eliminan automaticamente porque pueden ser lenguaje sustantivo.

tokens_sensitivity_raw <- tokenize_corpus(corpus, "text_ti_ab_kw")
# Tokens del modelo de sensibilidad antes de aplicar los mismos filtros.

tokens_primary <- filter_tokens(tokens_primary_raw, nrow(corpus))
tokens_sensitivity <- filter_tokens(tokens_sensitivity_raw, nrow(corpus))

token_audit <- bind_rows(
  tokens_primary_raw |> mutate(model = "TI_AB_raw"),
  tokens_primary |> mutate(model = "TI_AB_filtered"),
  tokens_sensitivity_raw |> mutate(model = "TI_AB_keywords_raw"),
  tokens_sensitivity |> mutate(model = "TI_AB_keywords_filtered")
) |>
  group_by(model) |>
  summarise(
    papers_with_tokens = n_distinct(UT),
    token_rows = n(),
    vocabulary = n_distinct(word),
    total_token_count = sum(n),
    .groups = "drop"
  )
# Permite comprobar cuantas palabras y documentos quedan disponibles para LDA.

dtm_primary <- tokens_primary |>
  cast_dtm(UT, word, n)
# DTM significa Document-Term Matrix: filas = papers, columnas = palabras,
# celdas = frecuencia de la palabra en el paper.

dtm_sensitivity <- tokens_sensitivity |>
  cast_dtm(UT, word, n)

if (nrow(dtm_primary) < 10L || ncol(dtm_primary) < 10L) {
  stop("La matriz principal tiene pocos documentos o terminos.")
}

# -----------------------------------------------------------------------------
# 5. Comparar soluciones con distinto numero de topics
# -----------------------------------------------------------------------------
# No se decide k solo por intuicion. Se ajustan varias soluciones y se comparan
# perplexity, coherencia y exclusividad. Perplexity favorece ajuste; coherencia
# y exclusividad ayudan a obtener topics mas interpretables y diferenciados.

fit_lda <- function(dtm, k, seed_value = seed, final = FALSE) {
  # Ajusta un modelo LDA con k topics.
  # La semilla permite reproducir el mismo muestreo aleatorio.
  # final = FALSE usa menos iteraciones para comparar k rapidamente.
  # final = TRUE usa el muestreo completo definido en control_final.
  set.seed(seed_value + k)

  control <- if (final) {
    modifyList(control_final, list(seed = seed_value + k))
  } else {
    list(
      seed = seed_value + k,
      burnin = 500L,
      iter = 1000L,
      thin = 100L
    )
  }

  LDA(dtm, k = k, method = "Gibbs", control = control)
}

top_terms_by_topic <- function(model, n = n_top_terms) {
  # Extrae las n palabras con mayor beta dentro de cada topic.
  # beta representa la probabilidad de una palabra dentro del topic.
  tidy(model, matrix = "beta") |>
    group_by(topic) |>
    slice_max(beta, n = n, with_ties = FALSE) |>
    ungroup()
}

umass_coherence <- function(model, dtm, n = 10L) {
  # Mide si las palabras principales de un topic aparecen juntas en documentos.
  # Valores menos negativos suelen indicar mayor coherencia interna.
  beta <- posterior(model)$terms
  binary_dtm <- as.matrix(dtm > 0)

  topic_values <- map_dbl(seq_len(nrow(beta)), function(topic_id) {
    terms <- names(sort(beta[topic_id, ], decreasing = TRUE))
    terms <- head(terms, min(n, length(terms)))

    if (length(terms) < 2L) {
      return(NA_real_)
    }

    pairs <- combn(terms, 2L)

    mean(map_dbl(seq_len(ncol(pairs)), function(i) {
      wi <- pairs[1, i]
      wj <- pairs[2, i]
      denominator <- sum(binary_dtm[, wj])
      numerator <- sum(binary_dtm[, wi] & binary_dtm[, wj])
      log((numerator + 1) / max(denominator, 1))
    }))
  })

  mean(topic_values, na.rm = TRUE)
}

exclusivity_score <- function(model, n = 10L) {
  # Mide cuanto pertenecen las palabras principales a un topic y no a todos.
  # Un valor mayor significa que los terminos distinguen mejor ese topic.
  beta <- posterior(model)$terms
  relative_beta <- sweep(beta, 2, colSums(beta), "/")

  topic_values <- map_dbl(seq_len(nrow(beta)), function(topic_id) {
    terms <- names(sort(beta[topic_id, ], decreasing = TRUE))
    terms <- head(terms, min(n, length(terms)))
    mean(relative_beta[topic_id, terms])
  })

  mean(topic_values)
}

model_grid <- map(k_candidates, ~ fit_lda(dtm_primary, .x, final = FALSE))
# Ajusta todas las soluciones candidatas. Esto permite ver si el resultado cambia
# mucho entre una estructura simple y otra mas desagregada.

model_selection <- map2_dfr(
  model_grid,
  k_candidates,
  function(model, k) {
    tibble(
      k = k,
      log_likelihood = as.numeric(logLik(model)),
      perplexity = perplexity(model, newdata = dtm_primary),
      coherence_umass = umass_coherence(model, dtm_primary),
      exclusivity = exclusivity_score(model)
    )
  }
) |>
  mutate(
    # Este score es una guia equilibrando las tres metricas. No reemplaza la
    # evaluacion manual de los terminos ni la interpretacion teorica.
    selection_score = as.numeric(scale(coherence_umass)) +
      as.numeric(scale(exclusivity)) -
      as.numeric(scale(perplexity))
  ) |>
  arrange(desc(selection_score))

recommended_k <- model_selection$k[[1L]]
# Guarda el k mejor posicionado por el score combinado. Ademas se exportan
# explicitamente k=4 y k=12 para comparar una lectura simple y una amplia.

# -----------------------------------------------------------------------------
# 6. Estimar modelos finales: k=4, k=12 y k recomendado
# -----------------------------------------------------------------------------
# La primera exploracion necesita mostrar tanto una sintesis facil de leer como
# una particion mas detallada. Por eso se estiman k=4 y k=12 con mas iteraciones.
# El modelo recomendado se conserva como referencia automatica.

fit_final_cached <- function(dtm, k, seed_value = seed) {
  # Ejecuta el modelo final con los parametros completos de Gibbs.
  fit_lda(dtm, k, seed_value = seed_value, final = TRUE)
}

final_k4 <- fit_final_cached(dtm_primary, 4L, seed_value = seed + 400L)
final_k12 <- fit_final_cached(dtm_primary, 12L, seed_value = seed + 1200L)

final_recommended <- if (recommended_k == 4L) {
  final_k4
} else if (recommended_k == 12L) {
  final_k12
} else {
  fit_final_cached(dtm_primary, recommended_k, seed_value = seed + recommended_k)
}

final_keywords <- fit_final_cached(
  dtm_sensitivity,
  recommended_k,
  seed_value = seed + 10000L
)
# Este ultimo modelo comprueba si las keywords cambian la estructura elegida.

make_topic_outputs <- function(model, corpus_data, model_name) {
  # Convierte un modelo LDA en tres productos interpretables:
  # terminos principales, etiquetas provisorias y topic dominante por paper.
  terms <- top_terms_by_topic(model)

  labels <- terms |>
    group_by(topic) |>
    summarise(
      topic_label = str_c(head(term, 4L), collapse = ", "),
      .groups = "drop"
    ) |>
    mutate(
      model = model_name,
      topic_label = str_c("Topic ", topic, ": ", topic_label)
    )

  papers <- tidy(model, matrix = "gamma") |>
    group_by(document) |>
    slice_max(gamma, n = 1L, with_ties = FALSE) |>
    ungroup() |>
    transmute(
      UT = document,
      dominant_topic = topic,
      topic_probability = gamma
    ) |>
    left_join(
      labels |> select(topic, topic_label),
      by = c("dominant_topic" = "topic")
    ) |>
    left_join(corpus_data, by = "UT") |>
    mutate(model = model_name) |>
    arrange(dominant_topic, desc(topic_probability))

  list(
    terms = terms |> mutate(model = model_name),
    labels = labels,
    papers = papers
  )
}

outputs_k4 <- make_topic_outputs(final_k4, corpus, "K4_TI_AB")
outputs_k12 <- make_topic_outputs(final_k12, corpus, "K12_TI_AB")
outputs_recommended <- make_topic_outputs(
  final_recommended,
  corpus,
  str_c("Recommended_K", recommended_k, "_TI_AB")
)
outputs_keywords <- make_topic_outputs(
  final_keywords,
  corpus,
  str_c("Recommended_K", recommended_k, "_TI_AB_keywords")
)

# -----------------------------------------------------------------------------
# 7. Comparar topics por base, tiempo y disciplina
# -----------------------------------------------------------------------------
# Esta seccion no vuelve a estimar topics. Usa la asignacion del modelo
# recomendado para preguntar donde aparecen mas los temas encontrados.

paper_base <- corpus |>
  transmute(
    UT,
    base = case_when(
      base_endogeno == 1L & base_laboratorio == 1L ~ "Endogeno y Laboratorio",
      base_endogeno == 1L ~ "Endogeno",
      base_laboratorio == 1L ~ "Laboratorio",
      TRUE ~ "Sin base identificada"
    )
  )

topics_by_base <- outputs_recommended$papers |>
  select(UT, dominant_topic, topic_label) |>
  left_join(paper_base, by = "UT") |>
  count(base, dominant_topic, topic_label, name = "n_papers") |>
  group_by(base) |>
  mutate(percent = 100 * n_papers / sum(n_papers)) |>
  ungroup()

topics_by_year <- outputs_recommended$papers |>
  count(year, dominant_topic, topic_label, name = "n_papers") |>
  group_by(year) |>
  mutate(percent = 100 * n_papers / sum(n_papers)) |>
  ungroup()
# Un año aparece varias veces porque hay una fila por combinacion año-topic.
# Los años con pocos papers deben interpretarse con cautela.

core_area_long <- corpus |>
  select(UT, core_area) |>
  mutate(core_area = replace_na(core_area, "")) |>
  separate_rows(core_area, sep = "\\s*;\\s*|\\s*\\|\\s*") |>
  mutate(core_area = str_squish(core_area)) |>
  filter(core_area != "")

topics_by_core_area <- outputs_recommended$papers |>
  select(UT, dominant_topic, topic_label) |>
  left_join(core_area_long, by = "UT") |>
  count(core_area, dominant_topic, topic_label, name = "n_papers") |>
  group_by(core_area) |>
  mutate(percent = 100 * n_papers / sum(n_papers)) |>
  ungroup() |>
  arrange(core_area, dominant_topic)
# Si un paper tiene mas de una core_area, aparece en cada area declarada. Esto
# es correcto para una clasificacion superpuesta, pero debe informarse al usar
# los porcentajes.

model_summary <- tibble(
  measure = c(
    "Corpus papers",
    "Papers in primary DTM",
    "Primary vocabulary",
    "Recommended k",
    "Explicit exploratory k values",
    "Candidate k minimum",
    "Candidate k maximum",
    "Seed",
    "Primary text",
    "Sensitivity text"
  ),
  value = c(
    nrow(corpus),
    nrow(dtm_primary),
    ncol(dtm_primary),
    recommended_k,
    paste(k_exploratory, collapse = ", "),
    min(k_candidates),
    max(k_candidates),
    seed,
    "Title + abstract",
    "Title + abstract + keywords"
  )
)

# -----------------------------------------------------------------------------
# 8. Exportar el Excel de resultados
# -----------------------------------------------------------------------------
# Las hojas se organizan para separar diagnostico, seleccion de k, terminos,
# asignaciones de papers y comparaciones sustantivas.

write_xlsx(
  list(
    Model_summary = model_summary,
    Coverage = coverage,
    Token_audit = token_audit,
    Common_terms_audit = common_terms_audit,
    Model_selection = model_selection,
    Labels_k4 = outputs_k4$labels,
    Top_terms_k4 = outputs_k4$terms,
    Paper_topics_k4 = outputs_k4$papers,
    Labels_k12 = outputs_k12$labels,
    Top_terms_k12 = outputs_k12$terms,
    Paper_topics_k12 = outputs_k12$papers,
    Topic_labels_primary = outputs_recommended$labels,
    Top_terms_primary = outputs_recommended$terms,
    Paper_topics_primary = outputs_recommended$papers,
    Topics_by_base = topics_by_base,
    Topics_by_year = topics_by_year,
    Topics_by_core_area = topics_by_core_area,
    Topic_labels_keywords = outputs_keywords$labels,
    Top_terms_keywords = outputs_keywords$terms,
    Paper_topics_keywords = outputs_keywords$papers
  ),
  file.path(tables_dir, "topic_model_endogeno_laboratorio.xlsx")
)

# -----------------------------------------------------------------------------
# 9. Exportar figuras comparables
# -----------------------------------------------------------------------------
# Las figuras permiten ver rapidamente si k=4 entrega una sintesis clara y si
# k=12 separa subtemas que quedan mezclados en la solucion parsimoniosa.

plot_top_terms <- function(topic_terms, title_text, subtitle_text, file_name,
                           width = 13, height = 9) {
  # Construye y guarda un grafico horizontal de los terminos mas probables de
  # cada topic. Se usa la misma escala visual para facilitar comparaciones.
  plot_data <- topic_terms |>
    mutate(term = reorder_within(term, beta, topic))

  plot <- ggplot(plot_data, aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free_y") +
    scale_y_reordered() +
    scale_fill_hue() +
    labs(
      title = title_text,
      subtitle = subtitle_text,
      x = "Probabilidad del termino dentro del topic",
      y = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold")
    )

  ggsave(
    file.path(figures_dir, file_name),
    plot,
    width = width,
    height = height,
    dpi = 320
  )
}

plot_top_terms(
  outputs_k4$terms,
  "Terminos principales: k = 4",
  "Modelo exploratorio parsimonioso, titulo + abstract",
  "topic_model_top_terms_k4.png"
)

plot_top_terms(
  outputs_k12$terms,
  "Terminos principales: k = 12",
  "Modelo exploratorio desagregado, titulo + abstract",
  "topic_model_top_terms_k12.png"
)

plot_top_terms(
  outputs_recommended$terms,
  str_c("Terminos principales: k = ", recommended_k),
  "Modelo recomendado por metricas combinadas, titulo + abstract",
  "topic_model_top_terms.png"
)

topic_base_plot <- topics_by_base |>
  ggplot(aes(factor(dominant_topic), percent, fill = base)) +
  geom_col(position = "dodge") +
  labs(
    title = "Distribucion de topics por base",
    x = "Topic dominante",
    y = "Porcentaje de papers",
    fill = "Base"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(figures_dir, "topic_model_topics_by_base.png"),
  topic_base_plot,
  width = 11,
  height = 7,
  dpi = 320
)

print(model_summary)
print(model_selection)
print(outputs_recommended$labels)