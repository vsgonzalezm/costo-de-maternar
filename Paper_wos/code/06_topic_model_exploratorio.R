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
  "UT", "TI", "AB", "keywords_analisis", "PY", "SO",
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
    keywords_analisis = as.character(keywords_analisis),
    core_area = as.character(core_area)
  ) |>
  group_by(UT) |>
  summarise(
    year = first(year),
    title = first(title),
    abstract = first(abstract),
    journal = first(journal),
    keywords_analisis = first(keywords_analisis),
    core_area = first(core_area),
    .groups = "drop"
  ) |>
  left_join(base_flags, by = "UT") |>
  mutate(
    # 00 ya consolida la jerarquia definida para keywords: originales del autor,
    # correcciones manuales cuando existen y keywords disponibles de WoS.
    # Aqui se utiliza esa variable final sin reconstruirla nuevamente.
    keywords_analisis = na_if(str_squish(keywords_analisis), "")
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
  "field", "test", "tests", "testing", "tested", "conjoint", "attribute",
  "attributes", "respondent", "respondents", "conducted", "report", "reports",
  "implication", "implications", "variable", "variables"
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
  # Terminos identificados en la lectura de los topics que no representan un
  # campo sustantivo por si mismos.
  "affect", "average", "based", "characteristic", "characteristics",
  "factor", "factors", "global", "improve", "improved", "improvement",
  "high", "international", "level", "literature", "low", "middle", "negative", "paercived",
  "perceived", "positive", "product", "products", "set",
  # Estas formas aisladas fueron revisadas en sus bigramas y trigramas. Se
  # retiran solo despues de proteger abajo sus combinaciones sustantivas.
  "quality", "learning", "performance", "market", "strategy", "strategies",
  "social", "reserved", "lab", "laboratory", "context", "understanding",
  "decision", "process", "behavior", "behaviors", "behaviour", "behaviours",
  "identity", "intensity", "article", "randomly", "response", "responses", "receive",
  "access", "including", "online", "significantly", "share", "compared",
  "consistent", "status", "light", "question", "questions", "selection",
  "attention", "increase", "subject", "subjects", "argue", "anti", "shape",
  "developing", "importance", "influence", "identify", "local", "scale",
  "willingness", "bias", "right", "game", "games", "participation",
  "contribution", "contributions", "program", "assigned", "short", "user",
  "users", "impacts", "practice", "practices", "reduce", "reduced", "reduces",
  "reducing", "incentive", "theory", "theories", "cross", "suggest",
  "suggests", "suggested", "suggesting", "dimension", "dimensions",
  "issue", "issues", "affected", "framing", "people", "causal", "providing",
  "received", "related", "author", "authors", "type", "types", "existing",
  "levels", "difference", "differences", "prefer", "prefers", "preferred",
  "heterogeneity", "role", "roles", "delivery", "choice", "term", "terms",
  "expectation", "expectations", "expected", "found", "finally", "city",
  "cities", "alternative", "alternatives", "consequence", "consequences",
  "contexts", "real", "approaches", "south", "sur", "ios", "relative",
  "condition", "conditions", "setting", "settings", "mechanism", "mechanisms",
  "key", "experience", "experiences", "presence", "activity", "activities",
  "conditional", "chinese", "explore", "explores", "explored", "exploring",
  "explain", "explains", "explained", "explaining", "include", "includes",
  "included", "due", "statistical", "adoption", "reduction", "reductions",
  "insight", "insights", "effort", "efforts", "extent", "provide", "provides",
  "provided", "providing"
)

artifact_terms <- c(
  # Artefactos HTML/XML y palabras academicas demasiado generales.
  "eacute", "aacute", "iacute", "oacute", "uacute", "ntilde",
  "nbsp", "amp", "rsquo", "ldquo", "rdquo", "ndash", "sic", "elsevier",
  "les", "los", "las", "des", "est", "et", "study", "studies", "paper",
  "research"
)

geographic_terms <- c(
  # Los lugares describen el alcance geografico del paper, pero no su tema.
  # Se retiran para evitar que los topics reproduzcan el diseno de busqueda.
  "america", "american", "americans", "argentina", "argentine", "argentinian",
  "argentinians", "bolivia", "bolivian", "bolivians", "brasil", "brazil",
  "brazilian", "brazilians", "china", "chinese", "chile", "chilean", "chileans",
  "colombia", "colombian", "colombians", "costa_rica", "costa_rican",
  "costa_ricans", "cuba", "cuban", "cubans", "dominican_republic",
  "dominican", "dominicans", "ecuador", "ecuadorian", "ecuadorians",
  "guatemala", "guatemalan", "guatemalans", "honduras", "honduran",
  "hondurans", "latin", "latin_america", "latin_american", "latin_americans",
  "mexico", "mexican", "mexicans", "nicaragua", "nicaraguan", "nicaraguans",
  "panama", "panamanian", "panamanians", "paraguay", "paraguayan",
  "paraguayans", "peru", "peruvian", "peruvians", "puerto_rico",
  "puerto_rican", "puerto_ricans", "uruguay", "uruguayan", "uruguayans",
  "usa", "united_states", "venezuela", "venezuelan", "venezuelans",
  "country", "countries"
)

compound_terms <- tribble(
  # Las expresiones se identificaron en bigramas y trigramas del corpus.
  # TRUE conserva conceptos sustantivos; FALSE retira frases metodologicas o
  # geograficas completas antes de separar sus palabras.
  ~phrase, ~token, ~keep_in_model,
  "technology adoption", "technology_adoption", TRUE,
  "poverty reduction", "poverty_reduction", TRUE,
  "risk reductions", "risk_reduction", TRUE,
  "risk reduction", "risk_reduction", TRUE,
  "interest rate", "interest_rate", TRUE,
  "latin americans", "latin_americans", FALSE,
  "south americans", "south_americans", FALSE,
  "international organizations ios", "international_organization", TRUE,
  "political candidates", "political_candidate", TRUE,

  "global south", "global_south", FALSE,
  "south american", "south_american", FALSE,
  "mexico city", "mexico_city", FALSE,
  "recreational activities", "recreational_activity", TRUE,
  "physical activity", "physical_activity", TRUE,
  "goal setting", "goal_setting", TRUE,
  "last mile delivery", "last_mile_delivery", TRUE,
  "service delivery", "service_delivery", TRUE,
  "public attitudes", "public_attitude", TRUE,
  "all rights reserved", "all_rights_reserved", FALSE,
  "rights reserved", "rights_reserved", FALSE,
  "human rights", "human_rights", TRUE,
  "property rights", "property_rights", TRUE,
  "right-wing", "right_wing", TRUE,
  "right wing", "right_wing", TRUE,
  "electoral accountability", "electoral_accountability", TRUE,
  "resource users", "resource_user", TRUE,
  "voluntary contributions", "voluntary_contribution", TRUE,
  "business practices", "business_practice", TRUE,
  "management practices", "management_practice", TRUE,
  "prospect theory", "prospect_theory", TRUE,
  "public goods game", "public_goods_game", FALSE,
  "trust game", "trust_game", FALSE,
  "dictator game", "dictator_game", FALSE,
  "game theory", "game_theory", FALSE,
  "socioeconomic status", "socioeconomic_status", TRUE,
  "status quo", "status_quo", TRUE,
  "presidential election", "presidential_election", TRUE,
  "municipal elections", "municipal_election", TRUE,
  "municipal election", "municipal_election", TRUE,
  "immigration attitudes", "immigration_attitude", TRUE,
  "anti corruption", "anti_corruption", TRUE,
  "anti immigrants", "anti_immigration", TRUE,
  "anti immigrant", "anti_immigration", TRUE,
  "anti immigration", "anti_immigration", TRUE,
  "selection bias", "selection_bias", FALSE,
  "online survey experiment", "online_survey_experiment", FALSE,
  "social responsibility csr", "social_responsibility", TRUE,
  "social media platforms", "social_media", TRUE,
  "labor market discrimination", "market_discrimination", TRUE,
  "willingness to pay", "willingness_to_pay", TRUE,
  "willingness to accept", "willingness_to_accept", TRUE,
  "trust in government", "trust_in_government", TRUE,
  "political participation", "political_participation", TRUE,
  "policy preferences", "policy_preferences", TRUE,
  "collective action", "collective_action", TRUE,
  "public opinion", "public_opinion", TRUE,
  "public support", "public_support", TRUE,
  "labor market", "labor_market", TRUE,
  "social norms", "social_norms", TRUE,
  "vote buying", "vote_buying", TRUE,
  "climate change", "climate_change", TRUE,
  "tax compliance", "tax_compliance", TRUE,
  "risk aversion", "risk_aversion", TRUE,
  "risk preferences", "risk_preferences", TRUE,
  "voting behavior", "voting_behavior", TRUE,
  "voter turnout", "voter_turnout", TRUE,
  "health care", "health_care", TRUE,
  "food safety", "food_safety", TRUE,
  "public health", "public_health", TRUE,
  "quality of life", "quality_of_life", TRUE,
  "quality of service", "quality_of_service", TRUE,
  "market discrimination", "market_discrimination", TRUE,
  "preference heterogeneity", "preference_heterogeneity", TRUE,
  "communication strategies", "communication_strategy", TRUE,
  "incumbent party", "incumbent_party", TRUE,
  "party identification", "party_identification", TRUE,
  "party system", "party_system", TRUE,
  "political parties", "political_party", TRUE,
  "opposition parties", "opposition_party", TRUE,
  "partisan messages", "partisan_message", TRUE,
  "social media", "social_media", TRUE,
  "social capital", "social_capital", TRUE,
  "social networks", "social_network", TRUE,
  "social welfare", "social_welfare", TRUE,
  "social identity", "social_identity", TRUE,
  "social responsibility", "social_responsibility", TRUE,
  "national identity", "national_identity", TRUE,
  "political identity", "political_identity", TRUE,
  "decision making", "decision_making", TRUE,
  "investment decisions", "investment_decision", TRUE,
  "electoral process", "electoral_process", TRUE,
  "peace process", "peace_process", TRUE,
  "pro environmental behavior", "pro_environmental_behavior", TRUE,
  "pro social behavior", "pro_social_behavior", TRUE,
  "health behaviors", "health_behavior", TRUE,
  "machine learning", "machine_learning", FALSE,
  "online experiments", "online_experiments", FALSE,
  "online experiment", "online_experiment", FALSE,
  "online survey", "online_survey", FALSE,
  "laboratory experiments", "laboratory_experiments", FALSE,
  "laboratory experiment", "laboratory_experiment", FALSE,
  "lab experiments", "lab_experiments", FALSE,
  "lab experiment", "lab_experiment", FALSE,
  "open access", "open_access", FALSE,
  "access article", "access_article", FALSE,
  "randomized controlled trial", "randomized_controlled_trial", FALSE,
  "social desirability bias", "social_desirability_bias", FALSE,
  "discrete choice experiments", "discrete_choice_experiments", FALSE,
  "discrete choice experiment", "discrete_choice_experiment", FALSE,
  "choice based conjoint", "choice_based_conjoint", FALSE,
  "choice experiments", "choice_experiments", FALSE,
  "choice experiment", "choice_experiment", FALSE,
  "mixed logit models", "mixed_logit_models", FALSE,
  "mixed logit model", "mixed_logit_model", FALSE,
  "mixed logit", "mixed_logit", FALSE,
  "stated choice", "stated_choice", FALSE,
  "stated preferences", "stated_preferences", FALSE,
  "stated preference", "stated_preference", FALSE,
  "nationally representative", "nationally_representative", FALSE,
  "latin american", "latin_american", FALSE,
  "latin america", "latin_america", FALSE,
  "united states", "united_states", FALSE,
  "costa rica", "costa_rica", FALSE,
  "dominican republic", "dominican_republic", FALSE,
  "puerto rico", "puerto_rico", FALSE
)
term_normalization <- tribble(
  # Diccionario auditable para unificar variantes sin producir stems ilegibles.
  ~variant, ~normalized,
  "attitudes", "attitude",
  "beliefs", "belief",
  "benefits", "benefit",
  "choices", "choice",
  "citizens", "citizen",
  "decisions", "decision",
  "farmers", "farmer",
  "households", "household",
  "incentives", "incentive",
  "institutions", "institution",
  "candidates", "candidate",
  "leaders", "leader",
  "leadership", "leader",
  "uncertain", "uncertainty",
  "children", "child",
  "effective", "effectiveness",
  "migration", "immigration",
  "migrant", "immigration",
  "migrants", "immigration",
  "female", "woman",
  "females", "woman",
  "educational", "education",
  "individuals", "individual",
  "increased", "increase",
  "increases", "increase",
  "resources", "resource",
  "rates", "rate",
  "voting", "voter",
  "vote", "voter",
  "votes", "voter",
  "electoral", "election",
  "elections", "election",
  "immigrant", "immigration",
  "immigrants", "immigration",
  "firms", "firm",
  "parties", "party_politics",
  "party", "party_politics",
  "partisan", "party_politics",
  "markets", "market",
  "policies", "policy",
  "preferences", "preference",
  "programs", "program",
  "rights", "right",
  "services", "service",
  "students", "student",
  "taxes", "tax",
  "voters", "voter",
  "women", "woman",
  "wtp", "willingness_to_pay"
)

apply_compounds <- function(text) {
  # Sustituye primero las frases mas largas y conserva conectores internos como
  # "to" en willingness_to_pay.
  reduce2(
    compound_terms$phrase,
    compound_terms$token,
    .init = str_to_lower(replace_na(text, "")),
    .f = function(current_text, phrase, token) {
      str_replace_all(current_text, fixed(phrase), token)
    }
  )
}

generic_terms <- unique(c(
  connector_terms,
  methodological_terms,
  generic_adjectives,
  corpus_generic_terms,
  artifact_terms,
  geographic_terms,
  compound_terms |> filter(!keep_in_model) |> pull(token),
  "consumer", "consumers", "support"
))

extract_ngrams <- function(data, text_column, n_gram) {
  # Cuenta en cuantos papers y cuantas veces aparece cada frase antes de
  # eliminar stopwords. Asi se detectan conceptos dependientes del corpus.
  occurrences <- data |>
    select(UT, text = all_of(text_column)) |>
    mutate(text = str_to_lower(replace_na(text, ""))) |>
    unnest_tokens(ngram, text, token = "ngrams", n = n_gram) |>
    filter(!is.na(ngram), str_squish(ngram) != "") |>
    count(ngram, name = "occurrences")

  documents <- data |>
    select(UT, text = all_of(text_column)) |>
    mutate(text = str_to_lower(replace_na(text, ""))) |>
    unnest_tokens(ngram, text, token = "ngrams", n = n_gram) |>
    filter(!is.na(ngram), str_squish(ngram) != "") |>
    distinct(UT, ngram) |>
    count(ngram, name = "n_documents")

  occurrences |>
    left_join(documents, by = "ngram") |>
    mutate(ngram_size = n_gram, .before = 1L)
}

ngram_candidates <- bind_rows(
  extract_ngrams(corpus, "text_ti_ab", 2L),
  extract_ngrams(corpus, "text_ti_ab", 3L)
) |>
  filter(n_documents >= min_document_frequency) |>
  group_by(ngram_size) |>
  slice_max(n_documents, n = 500L, with_ties = FALSE) |>
  ungroup() |>
  arrange(ngram_size, desc(n_documents), desc(occurrences))
tokenize_corpus <- function(data, text_column) {
  # Recibe el corpus y el nombre de la columna textual. Primero conserva las
  # expresiones compuestas, luego normaliza variantes y finalmente limpia.
  data |>
    select(UT, text = all_of(text_column)) |>
    mutate(text = apply_compounds(text)) |>
    unnest_tokens(word, text) |>
    anti_join(stop_words, by = "word") |>
    left_join(term_normalization, by = c("word" = "variant")) |>
    mutate(word = coalesce(normalized, word)) |>
    filter(
      str_detect(word, "^[a-z]+(?:_[a-z]+)*$"),
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
# Esta auditoria conserva los conteos antes y despues de unificar variantes y
# expresiones compuestas. Las figuras usan los terminos consolidados, pero esta
# hoja permite rastrear cuanto aportaba cada forma original.
raw_unigram_counts <- corpus |>
  select(UT, text = text_ti_ab) |>
  unnest_tokens(original_form, text) |>
  count(UT, original_form, name = "n") |>
  group_by(original_form) |>
  summarise(
    raw_n_documents = n_distinct(UT),
    raw_occurrences = sum(n),
    .groups = "drop"
  )

model_term_counts <- tokens_primary_raw |>
  group_by(model_form = word) |>
  summarise(
    model_n_documents = n_distinct(UT),
    model_occurrences = sum(n),
    .groups = "drop"
  )

normalization_audit_map <- bind_rows(
  term_normalization |>
    transmute(original_form = variant, model_form = normalized),
  term_normalization |>
    distinct(normalized) |>
    transmute(original_form = normalized, model_form = normalized)
) |>
  distinct()

normalization_counts <- normalization_audit_map |>
  left_join(raw_unigram_counts, by = "original_form") |>
  left_join(model_term_counts, by = "model_form") |>
  mutate(
    mapping_type = "normalized word",
    keep_in_model = !model_form %in% generic_terms,
    across(where(is.numeric), ~ replace_na(.x, 0)),
    .before = 1L
  )

raw_text_for_audit <- str_to_lower(replace_na(corpus$text_ti_ab, ""))

compound_counts <- pmap_dfr(
  compound_terms,
  function(phrase, token, keep_in_model) {
    tibble(
      mapping_type = "compound phrase",
      original_form = phrase,
      model_form = token,
      keep_in_model = keep_in_model,
      raw_n_documents = sum(str_detect(raw_text_for_audit, fixed(phrase))),
      raw_occurrences = sum(str_count(raw_text_for_audit, fixed(phrase)))
    )
  }
) |>
  left_join(model_term_counts, by = "model_form") |>
  mutate(across(where(is.numeric), ~ replace_na(.x, 0)))

term_counts_audit <- bind_rows(normalization_counts, compound_counts) |>
  select(
    mapping_type, original_form, model_form, keep_in_model,
    raw_n_documents, raw_occurrences, model_n_documents, model_occurrences
  ) |>
  arrange(mapping_type, model_form, original_form)

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
    Ngram_candidates = ngram_candidates,
    Compound_terms = compound_terms,
    Term_normalization = term_normalization,
    Term_counts_audit = term_counts_audit,
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