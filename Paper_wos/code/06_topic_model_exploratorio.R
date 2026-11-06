#---------------------------------------------------
# Topic modeling exploratorio del corpus WOS.
#
# Este script trabaja a nivel de paper (UT). Su objetivo es explorar en que
# temas se publica, como cambia esa distribucion en el tiempo y si los temas
# tienen distinta presencia entre core areas del corpus affi_latam.
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

seed <- 20260929L # Semilla maestra: reproduce la particion entrenamiento-validacion.

k_candidates <- 4:10
# Valores de k que se comparan. k representa el numero de topics que el modelo
# debe encontrar. Probar varios valores evita fijar arbitrariamente un unico k.

k_exploratory <- c(4L, 10L)
# Soluciones que se exportan explicitamente para lectura manual:
# 4 topics entrega una sintesis parsimoniosa y 10 una lectura mas detallada.

selection_seeds <- c(1101L, 2202L, 3303L, 4404L, 5505L)
# Cada k se estima cinco veces. LDA puede converger a soluciones distintas
# segun la inicializacion; repetirlo permite medir estabilidad y no elegir k
# por una corrida excepcionalmente favorable.

training_proportion <- 0.80
# Se usa 80% de los papers para ajustar los modelos candidatos y 20% para
# calcular perplexity fuera de muestra. Asi perplexity mide generalizacion y
# no solamente ajuste a los mismos documentos usados para entrenar.

minimum_topic_share <- 0.03
# El 3% es una alerta descriptiva sobre topics pequenos. No elimina soluciones
# automaticamente, porque un topic poco frecuente puede ser sustantivamente
# importante y la asignacion dominante pierde la mezcla interna de cada paper.

low_text_token_threshold <- 20L
low_text_term_threshold <- 15L
# Un paper se marca como de poco texto retenido si queda bajo cualquiera de
# estos umbrales. La marca sirve para sensibilidad; el paper no se excluye.

n_top_terms <- 12L
# Numero de palabras con mayor probabilidad que se exporta para interpretar
# cada topic. Las etiquetas siguen siendo provisorias y requieren lectura.

min_document_frequency <- 3L
# Un termino debe aparecer en al menos tres papers para evitar que una palabra
# aislada defina por si sola un topic.

max_document_proportion <- 1.00
# No se eliminan automaticamente terminos frecuentes. Como el corpus ya esta
# restringido por campo, region y metodo, una palabra comun puede ser relevante.
# Las frecuencias altas se auditan y cualquier exclusion queda en el diccionario.

control_candidate <- list(burnin = 500L, iter = 1000L, thin = 100L)
# Controles breves para comparar 35 modelos candidatos (7 k por 5 semillas).

control_final <- list(burnin = 1500L, iter = 3000L, thin = 100L)
# Parametros del muestreo Gibbs de los modelos finales:
# burnin descarta iteraciones iniciales; iter cuenta las posteriores; thin
# conserva una de cada 100. El ajuste final usa mas iteraciones que la seleccion.
tables_dir <- here("Paper_wos", "outputs", "tables")
figures_dir <- here("Paper_wos", "outputs", "figures")



# -----------------------------------------------------------------------------
# 2. Cargar y consolidar el corpus final
# -----------------------------------------------------------------------------
# Topic modeling necesita una observacion por documento. Por eso se carga
# base_oficial.rds, que ya contiene las correcciones de Isidora,
# exclusiones, abstracts y keywords integradas por 00.

corpus_raw <- readRDS(
  here("Paper_wos", "data", "intermediate", "base_oficial.rds")
)

required <- c(
  "UT", "TI", "AB", "keywords_analisis", "PY", "SO",
  "core_area"
)
# Se detiene el script si falta una variable necesaria para identificar el
# documento, construir el texto o comparar los topics por contexto.

missing <- setdiff(required, names(corpus_raw))
if (length(missing) > 0L) {
  stop(
    "Faltan columnas en base_oficial.rds: ",
    paste(missing, collapse = ", ")
  )
}

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
  mutate(
    # 00 ya consolida la jerarquia definida para keywords: originales del autor,
    # correcciones manuales cuando existen y keywords disponibles de WoS.
    # Aqui se utiliza esa variable final sin reconstruirla nuevamente.
    keywords_analisis = na_if(str_squish(keywords_analisis), "")
  )

if (nrow(corpus) != 675L || anyDuplicated(corpus$UT) > 0L) {
  stop("El corpus de topic modeling debe contener 675 UT unicas.")
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
  "provided", "providing",
  # Verbos de proposito y evaluacion academica que no identifican el tema.
  "investigate", "investigates", "investigated", "investigating",
  "examine", "examines", "examined", "examining",
  "evaluate", "evaluates", "evaluated", "evaluating",
  "assess", "assesses", "assessed", "assessing",
  "aim", "aims", "aimed", "objective", "objectives", "purpose", "purposes",
  "contribute", "contributes", "contributed", "contributing", "address",
  "addresses", "addressed", "designed", "potential"
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
  "wtp", "willingness_to_pay",
  # Plurales residuales observados en la auditoria del vocabulario.
  "costs", "cost",
  "perceptions", "perception",
  "systems", "system"
)

normalize_compound_text <- function(text) {
  # Convierte guiones y signos de puntuacion internos en espacios antes de
  # buscar frases. Esto permite reconocer tanto "right-wing" como "right wing".
  replace_na(text, "") |>
    str_to_lower() |>
    str_replace_all("[\u2010\u2011\u2012\u2013\u2014\u2015-]+", " ") |>
    str_replace_all("[^a-z_\\s]", " ") |>
    str_squish()
}

compound_dictionary <- compound_terms |>
  # La frase se normaliza con la misma regla aplicada al corpus.
  mutate(
    phrase_normalized = normalize_compound_text(phrase),
    phrase_words = str_count(phrase_normalized, "\\S+")
  ) |>
  # Las frases mas largas se reemplazan primero para que un bigrama no destruya
  # previamente un trigrama que contiene las mismas palabras.
  arrange(desc(phrase_words), desc(str_length(phrase_normalized)))

apply_compounds <- function(text) {
  # Protege expresiones conceptuales como willingness_to_pay como un solo token.
  # Los limites de palabra evitan reemplazos dentro de palabras mas extensas.
  reduce2(
    compound_dictionary$phrase_normalized,
    compound_dictionary$token,
    .init = normalize_compound_text(text),
    .f = function(current_text, phrase, token) {
      flexible_phrase <- str_replace_all(phrase, "\\s+", "\\\\s+")
      pattern <- regex(str_c("\\b", flexible_phrase, "\\b"))
      str_replace_all(current_text, pattern, token)
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

normalized_text_for_audit <- normalize_compound_text(corpus$text_ti_ab)
# La auditoria usa exactamente la misma normalizacion de guiones y puntuacion
# que el modelamiento para no subcontar variantes de una misma expresion.

compound_counts <- pmap_dfr(
  compound_dictionary |> select(phrase, token, keep_in_model, phrase_normalized),
  function(phrase, token, keep_in_model, phrase_normalized) {
    flexible_phrase <- str_replace_all(phrase_normalized, "\\s+", "\\\\s+")
    pattern <- regex(str_c("\\b", flexible_phrase, "\\b"))

    tibble(
      mapping_type = "compound phrase",
      original_form = phrase,
      model_form = token,
      keep_in_model = keep_in_model,
      raw_n_documents = sum(str_detect(normalized_text_for_audit, pattern)),
      raw_occurrences = sum(str_count(normalized_text_for_audit, pattern))
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
# La sensibilidad agrega keywords, pero aplica exactamente las mismas reglas de
# compuestos, normalizacion y filtrado que el modelo principal.

tokens_primary <- filter_tokens(tokens_primary_raw, nrow(corpus))
tokens_sensitivity <- filter_tokens(tokens_sensitivity_raw, nrow(corpus))

token_audit <- bind_rows(
  tokens_primary_raw |> mutate(model = "TI_AB_before_frequency_filter"),
  tokens_primary |> mutate(model = "TI_AB_final"),
  tokens_sensitivity_raw |> mutate(model = "TI_AB_keywords_before_frequency_filter"),
  tokens_sensitivity |> mutate(model = "TI_AB_keywords_final")
) |>
  group_by(model) |>
  summarise(
    papers_with_tokens = n_distinct(UT),
    token_rows = n(),
    vocabulary = n_distinct(word),
    total_token_count = sum(n),
    .groups = "drop"
  )
# Esta tabla permite verificar cuantos documentos, palabras distintas y tokens
# sobreviven en cada version del corpus.

raw_document_counts <- corpus |>
  select(UT, text = text_ti_ab) |>
  unnest_tokens(word, text) |>
  group_by(UT) |>
  summarise(
    raw_tokens = n(),
    raw_unique_terms = n_distinct(word),
    .groups = "drop"
  )
# Cuenta el texto disponible antes de stopwords y filtros tematicos.

retained_document_counts <- tokens_primary |>
  group_by(UT) |>
  summarise(
    retained_tokens = sum(n),
    retained_unique_terms = n_distinct(word),
    .groups = "drop"
  )
# Cuenta cuanto texto aporta efectivamente cada paper a la matriz del modelo.

document_coverage <- corpus |>
  select(UT, year, title, journal, core_area) |>
  left_join(raw_document_counts, by = "UT") |>
  left_join(retained_document_counts, by = "UT") |>
  mutate(
    across(
      c(raw_tokens, raw_unique_terms, retained_tokens, retained_unique_terms),
      ~ replace_na(.x, 0L)
    ),
    token_retention_rate = if_else(
      raw_tokens > 0L,
      retained_tokens / raw_tokens,
      NA_real_
    ),
    low_text_flag = retained_tokens < low_text_token_threshold |
      retained_unique_terms < low_text_term_threshold
  ) |>
  arrange(desc(low_text_flag), retained_tokens, retained_unique_terms)
# low_text_flag identifica casos fragiles para lectura y sensibilidad, pero no
# los elimina: todos los papers permanecen en la cobertura principal.

dtm_primary <- tokens_primary |>
  cast_dtm(UT, word, n)
# DTM significa Document-Term Matrix: filas = papers, columnas = terminos y
# cada celda = numero de veces que el termino aparece en ese paper.

dtm_sensitivity <- tokens_sensitivity |>
  cast_dtm(UT, word, n)
# Esta segunda DTM agrega keywords y permite evaluar si ellas cambian los topics.

if (nrow(dtm_primary) != nrow(corpus) || ncol(dtm_primary) < 10L) {
  stop("La DTM principal debe contener los 675 papers y un vocabulario util.")
}

if (!requireNamespace("lpSolve", quietly = TRUE)) {
  stop("Se necesita el paquete lpSolve para comparar topics entre semillas.")
}

set.seed(seed)
# La particion se realiza una sola vez y se mantiene igual para todos los k.
all_document_ids <- rownames(dtm_primary)
train_ids <- sample(
  all_document_ids,
  size = floor(training_proportion * length(all_document_ids)),
  replace = FALSE
)
test_ids <- setdiff(all_document_ids, train_ids)

train_rows <- match(train_ids, rownames(dtm_primary))
test_rows <- match(test_ids, rownames(dtm_primary))
dtm_train_full <- dtm_primary[train_rows, ]

train_term_totals <- slam::col_sums(dtm_train_full)
train_terms <- names(train_term_totals[train_term_totals > 0])
# Se conservan solo terminos observados en entrenamiento. Usar vocabulario del
# conjunto de validacion filtraria informacion futura hacia el ajuste.

dtm_train <- dtm_primary[train_rows, train_terms]
dtm_test_full <- dtm_primary[test_rows, train_terms]
test_nonempty <- slam::row_sums(dtm_test_full) > 0
dtm_test <- dtm_test_full[test_nonempty, ]
# Perplexity no puede calcularse para una fila sin palabras conocidas. El numero
# de casos omitidos, si existe alguno, queda informado en Model_summary.
# -----------------------------------------------------------------------------
# 5. Comparar soluciones con distinto numero de topics
# -----------------------------------------------------------------------------
# Elegir k requiere equilibrar ajuste e interpretabilidad. Se comparan:
# (a) perplexity en papers no usados para entrenar; (b) coherencia de palabras;
# (c) exclusividad de los terminos; y (d) estabilidad entre cinco semillas.
# Ninguna metrica reemplaza la lectura sustantiva de los topics.

fit_lda <- function(dtm, k, seed_value, final = FALSE) {
  # Ajusta LDA mediante muestreo Gibbs para un k y una semilla determinados.
  # final = FALSE usa el control breve de seleccion; TRUE usa mas iteraciones.
  selected_control <- if (final) control_final else control_candidate
  selected_control$seed <- seed_value

  set.seed(seed_value)
  LDA(dtm, k = k, method = "Gibbs", control = selected_control)
}

top_terms_by_topic <- function(model, n = n_top_terms) {
  # beta es la probabilidad de cada termino condicionada al topic.
  # Se extraen los n terminos de beta mas alto para interpretar cada grupo.
  tidy(model, matrix = "beta") |>
    group_by(topic) |>
    slice_max(beta, n = n, with_ties = FALSE) |>
    ungroup()
}

umass_coherence <- function(model, dtm, n = 10L) {
  # UMass evalua si los terminos principales aparecen en los mismos documentos.
  # Valores mas altos (habitualmente menos negativos) indican mayor coherencia.
  beta <- posterior(model)$terms
  binary_dtm <- as.matrix(dtm > 0)

  topic_values <- map_dbl(seq_len(nrow(beta)), function(topic_id) {
    ordered_terms <- names(sort(beta[topic_id, ], decreasing = TRUE))
    ordered_terms <- head(ordered_terms, min(n, length(ordered_terms)))

    if (length(ordered_terms) < 2L) {
      return(NA_real_)
    }

    term_pairs <- combn(ordered_terms, 2L)

    mean(map_dbl(seq_len(ncol(term_pairs)), function(pair_id) {
      higher_ranked_term <- term_pairs[1L, pair_id]
      lower_ranked_term <- term_pairs[2L, pair_id]

      # La formula de Mimno usa en el denominador el termino anterior y mas
      # probable del par; esta orientacion corrige el calculo previo.
      denominator <- sum(binary_dtm[, higher_ranked_term])
      numerator <- sum(
        binary_dtm[, higher_ranked_term] &
          binary_dtm[, lower_ranked_term]
      )

      log((numerator + 1) / max(denominator, 1))
    }))
  })

  mean(topic_values, na.rm = TRUE)
}

exclusivity_score <- function(model, n = 10L) {
  # Exclusividad mide si los terminos principales distinguen un topic en vez de
  # repetirse con igual fuerza en todos. Valores mayores son preferibles.
  beta <- posterior(model)$terms
  relative_beta <- sweep(beta, 2L, colSums(beta), "/")

  topic_values <- map_dbl(seq_len(nrow(beta)), function(topic_id) {
    ordered_terms <- names(sort(beta[topic_id, ], decreasing = TRUE))
    selected_terms <- head(ordered_terms, min(n, length(ordered_terms)))
    mean(relative_beta[topic_id, selected_terms])
  })

  mean(topic_values)
}

topic_prevalence <- function(model) {
  # gamma contiene la mezcla de topics de cada paper. Se reporta tanto la
  # proporcion promedio de gamma como el topic dominante, porque esta ultima
  # simplifica cada documento a una sola categoria.
  gamma <- posterior(model)$topics
  expected_shares <- colMeans(gamma)
  dominant_topics <- max.col(gamma, ties.method = "first")
  dominant_counts <- tabulate(dominant_topics, nbins = ncol(gamma))
  dominant_shares <- dominant_counts / sum(dominant_counts)

  tibble(
    min_expected_topic_share = min(expected_shares),
    max_expected_topic_share = max(expected_shares),
    min_dominant_topic_papers = min(dominant_counts),
    min_dominant_topic_share = min(dominant_shares),
    max_dominant_topic_share = max(dominant_shares),
    small_topic_warning = min(expected_shares) < minimum_topic_share
  )
}

cosine_similarity_matrix <- function(beta_a, beta_b) {
  # Calcula la similitud coseno entre todos los pares de topics de dos modelos.
  # Uno significa perfiles de palabras identicos; cero, perfiles sin semejanza.
  # Una corrida de Gibbs puede dejar un topic transitoriamente vacio. En ese
  # caso sus probabilidades no finitas se convierten en cero y la estabilidad
  # recibe una similitud de cero, en vez de ignorar silenciosamente el problema.
  beta_a[!is.finite(beta_a)] <- 0
  beta_b[!is.finite(beta_b)] <- 0

  numerator <- tcrossprod(beta_a, beta_b)
  denominator <- outer(
    sqrt(rowSums(beta_a^2)),
    sqrt(rowSums(beta_b^2))
  )
  similarity <- numerator / pmax(denominator, .Machine$double.eps)
  similarity[!is.finite(similarity)] <- 0
  similarity
}

match_topic_matrices <- function(beta_a, beta_b) {
  # Dos corridas pueden numerar de forma distinta el mismo topic. El algoritmo
  # de asignacion empareja topics uno a uno maximizando la similitud total.
  common_terms <- intersect(colnames(beta_a), colnames(beta_b))

  if (length(common_terms) < 10L) {
    stop("Los modelos no comparten vocabulario suficiente para comparar topics.")
  }

  similarity <- cosine_similarity_matrix(
    beta_a[, common_terms, drop = FALSE],
    beta_b[, common_terms, drop = FALSE]
  )
  assignment_solution <- lpSolve::lp.assign(
    similarity,
    direction = "max"
  )$solution
  assignment <- which(assignment_solution > 0.5, arr.ind = TRUE)
  assignment <- assignment[order(assignment[, "row"]), , drop = FALSE]

  if (nrow(assignment) != nrow(similarity)) {
    stop("El emparejamiento no asigno exactamente un topic de cada modelo.")
  }

  tibble(
    topic_a = assignment[, "row"],
    topic_b = assignment[, "col"],
    cosine_similarity = similarity[assignment]
  )
}

compare_model_pair <- function(model_a, model_b, model_i, model_j) {
  # Compara beta para estabilidad semantica y gamma para acuerdo en el topic
  # dominante despues de corregir la permutacion de etiquetas.
  matching <- match_topic_matrices(
    posterior(model_a)$terms,
    posterior(model_b)$terms
  )

  dominant_a <- max.col(posterior(model_a)$topics, ties.method = "first")
  dominant_b <- max.col(posterior(model_b)$topics, ties.method = "first")
  map_b_to_a <- setNames(matching$topic_a, matching$topic_b)
  dominant_b_aligned <- unname(map_b_to_a[as.character(dominant_b)])

  tibble(
    model_i = model_i,
    model_j = model_j,
    mean_matched_cosine = mean(matching$cosine_similarity),
    minimum_matched_cosine = min(matching$cosine_similarity),
    dominant_topic_agreement = mean(dominant_a == dominant_b_aligned)
  )
}

summarize_model_stability <- function(models, seeds, k, stage) {
  # Compara todos los pares de semillas y elige como representante el modelo
  # con mayor similitud media respecto de los demas: la mediana empirica.
  model_pairs <- combn(seq_along(models), 2L, simplify = FALSE)

  pairwise <- map_dfr(model_pairs, function(pair) {
    compare_model_pair(
      models[[pair[1L]]],
      models[[pair[2L]]],
      model_i = pair[1L],
      model_j = pair[2L]
    )
  }) |>
    mutate(
      k = k,
      stage = stage,
      seed_i = seeds[model_i],
      seed_j = seeds[model_j],
      .before = 1L
    )

  medoid_scores <- map_dfr(seq_along(models), function(model_index) {
    relevant_pairs <- pairwise |>
      filter(model_i == model_index | model_j == model_index)

    tibble(
      model_index = model_index,
      seed = seeds[model_index],
      mean_similarity_to_others = mean(
        relevant_pairs$mean_matched_cosine
      )
    )
  })

  medoid_index <- medoid_scores |>
    arrange(desc(mean_similarity_to_others), seed) |>
    slice(1L) |>
    pull(model_index)

  list(
    pairwise = pairwise,
    medoid_scores = medoid_scores |>
      mutate(k = k, stage = stage, .before = 1L),
    medoid_index = medoid_index
  )
}

candidate_fits <- map(k_candidates, function(k_value) {
  # Para cada k se ajustan cinco modelos sobre exactamente el mismo conjunto de
  # entrenamiento; luego se evalua cada uno y se resume su estabilidad.
  models <- map(
    selection_seeds,
    ~ fit_lda(dtm_train, k_value, seed_value = .x, final = FALSE)
  )

  seed_metrics <- map2_dfr(
    models,
    selection_seeds,
    function(model, seed_value) {
      bind_cols(
        tibble(
          k = k_value,
          seed = seed_value,
          heldout_perplexity = perplexity(model, newdata = dtm_test),
          coherence_umass = umass_coherence(model, dtm_train),
          exclusivity = exclusivity_score(model)
        ),
        topic_prevalence(model)
      )
    }
  )

  stability <- summarize_model_stability(
    models,
    selection_seeds,
    k_value,
    stage = "candidate_train"
  )

  list(
    k = k_value,
    models = models,
    seed_metrics = seed_metrics,
    stability = stability
  )
})

candidate_seed_metrics <- map_dfr(candidate_fits, "seed_metrics")
candidate_stability_pairs <- map_dfr(
  candidate_fits,
  ~ .x$stability$pairwise
)
candidate_medoid_scores <- map_dfr(
  candidate_fits,
  ~ .x$stability$medoid_scores
)

model_selection <- candidate_seed_metrics |>
  group_by(k) |>
  summarise(
    heldout_perplexity_mean = mean(heldout_perplexity),
    heldout_perplexity_sd = sd(heldout_perplexity),
    coherence_umass_mean = mean(coherence_umass),
    coherence_umass_sd = sd(coherence_umass),
    exclusivity_mean = mean(exclusivity),
    exclusivity_sd = sd(exclusivity),
    min_expected_topic_share_mean = mean(min_expected_topic_share),
    min_dominant_topic_share_mean = mean(min_dominant_topic_share),
    any_small_topic_warning = any(small_topic_warning),
    .groups = "drop"
  ) |>
  left_join(
    candidate_stability_pairs |>
      group_by(k) |>
      summarise(
        stability_mean = mean(mean_matched_cosine),
        stability_minimum = min(minimum_matched_cosine),
        dominant_agreement_mean = mean(dominant_topic_agreement),
        .groups = "drop"
      ),
    by = "k"
  ) |>
  mutate(
    # Cada metrica se transforma en un ranking transparente. Perplexity se
    # minimiza; coherencia, exclusividad y estabilidad se maximizan.
    rank_perplexity = min_rank(heldout_perplexity_mean),
    rank_coherence = min_rank(desc(coherence_umass_mean)),
    rank_exclusivity = min_rank(desc(exclusivity_mean)),
    rank_stability = min_rank(desc(stability_mean)),
    selection_rank_sum = rank_perplexity + rank_coherence +
      rank_exclusivity + rank_stability
  ) |>
  arrange(selection_rank_sum, k)

recommended_k <- model_selection |>
  arrange(selection_rank_sum, k) |>
  slice(1L) |>
  pull(k)
# En caso de empate se prefiere el k menor por parsimonia. Esta recomendacion es
# estadistica y debe contrastarse con la legibilidad sustantiva de k=4 y k=10.

# -----------------------------------------------------------------------------
# 6. Estimar modelos finales y escoger una corrida estable
# -----------------------------------------------------------------------------
# Se vuelven a estimar sobre los 675 papers el k recomendado y los dos niveles
# exploratorios. Cada solucion usa cinco semillas y conserva la corrida mas
# central, evitando presentar un resultado atipico.

fit_final_ensemble <- function(dtm, k_value, stage_name) {
  # Ajusta las cinco semillas finales y selecciona el medoid segun similitud beta.
  models <- map(
    selection_seeds,
    ~ fit_lda(dtm, k_value, seed_value = .x, final = TRUE)
  )
  stability <- summarize_model_stability(
    models,
    selection_seeds,
    k_value,
    stage = stage_name
  )

  list(
    model = models[[stability$medoid_index]],
    selected_seed = selection_seeds[stability$medoid_index],
    stability = stability
  )
}

final_k_values <- sort(unique(c(k_exploratory, recommended_k)))
final_ensembles <- set_names(
  map(
    final_k_values,
    ~ fit_final_ensemble(
      dtm_primary,
      .x,
      stage_name = str_c("final_ti_ab_k", .x)
    )
  ),
  as.character(final_k_values)
)

final_k4 <- final_ensembles[["4"]]$model
final_k10 <- final_ensembles[["10"]]$model
final_recommended <- final_ensembles[[as.character(recommended_k)]]$model

final_keywords_ensemble <- fit_final_ensemble(
  dtm_sensitivity,
  recommended_k,
  stage_name = str_c("final_keywords_k", recommended_k)
)
final_keywords <- final_keywords_ensemble$model
# La sensibilidad con keywords usa la misma cantidad de topics y el mismo
# criterio de estabilidad que el modelo principal.

final_stability_pairs <- bind_rows(
  map_dfr(final_ensembles, ~ .x$stability$pairwise),
  final_keywords_ensemble$stability$pairwise
)
final_medoid_scores <- bind_rows(
  map_dfr(final_ensembles, ~ .x$stability$medoid_scores),
  final_keywords_ensemble$stability$medoid_scores
)

make_topic_outputs <- function(model, corpus_data, model_name) {
  # Convierte LDA en cuatro productos: terminos, etiquetas provisorias,
  # probabilidades gamma completas y topic dominante por paper.
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

  gamma <- tidy(model, matrix = "gamma") |>
    transmute(
      UT = document,
      topic = topic,
      gamma = gamma
    ) |>
    left_join(labels |> select(topic, topic_label), by = "topic") |>
    left_join(corpus_data, by = "UT") |>
    mutate(model = model_name)

  papers <- gamma |>
    group_by(UT) |>
    slice_max(gamma, n = 1L, with_ties = FALSE) |>
    ungroup() |>
    transmute(
      UT,
      dominant_topic = topic,
      topic_probability = gamma,
      topic_label,
      year,
      title,
      journal,
      keywords_analisis,
      core_area,
      model
    ) |>
    arrange(dominant_topic, desc(topic_probability))

  list(
    terms = terms |> mutate(model = model_name),
    labels = labels,
    gamma = gamma,
    papers = papers
  )
}

outputs_k4 <- make_topic_outputs(final_k4, corpus, "K4_TI_AB")
outputs_k10 <- make_topic_outputs(final_k10, corpus, "K10_TI_AB")
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

keyword_topic_alignment <- match_topic_matrices(
  posterior(final_recommended)$terms,
  posterior(final_keywords)$terms
) |>
  rename(
    primary_topic = topic_a,
    keyword_topic = topic_b
  ) |>
  left_join(
    outputs_recommended$labels |>
      select(primary_topic = topic, primary_label = topic_label),
    by = "primary_topic"
  ) |>
  left_join(
    outputs_keywords$labels |>
      select(keyword_topic = topic, keyword_label = topic_label),
    by = "keyword_topic"
  ) |>
  arrange(primary_topic)
# Esta tabla cuantifica si agregar keywords reproduce topics semejantes o cambia
# de manera importante su vocabulario.
# -----------------------------------------------------------------------------
# 7. Comparar topics por tiempo y disciplina
# -----------------------------------------------------------------------------
# Estas tablas no vuelven a estimar LDA. Resumen gamma del modelo recomendado
# para describir donde y cuando aparecen los temas encontrados.

topics_by_year_gamma <- outputs_recommended$gamma |>
  group_by(year, topic, topic_label) |>
  summarise(
    n_papers = n_distinct(UT),
    mean_gamma = mean(gamma),
    median_gamma = median(gamma),
    expected_papers = sum(gamma),
    .groups = "drop"
  ) |>
  mutate(percent_gamma = 100 * mean_gamma) |>
  arrange(year, topic)
# expected_papers suma probabilidades: por ejemplo 8.4 equivale al peso tematico
# agregado de aproximadamente 8.4 papers, sin obligarlos a un unico topic.

topics_by_year_dominant <- outputs_recommended$papers |>
  count(year, dominant_topic, topic_label, name = "n_papers") |>
  group_by(year) |>
  mutate(percent = 100 * n_papers / sum(n_papers)) |>
  ungroup() |>
  arrange(year, dominant_topic)
# Esta version es mas intuitiva, pero debe tratarse como complemento porque
# descarta los topics secundarios presentes en cada paper.

core_area_long <- corpus |>
  select(UT, core_area) |>
  mutate(core_area = replace_na(core_area, "")) |>
  separate_rows(core_area, sep = "\\s*;\\s*|\\s*\\|\\s*") |>
  mutate(core_area = str_squish(core_area)) |>
  filter(core_area != "") |>
  distinct(UT, core_area)
# Un paper puede pertenecer a mas de una core_area. Por eso las menciones entre
# areas se superponen y no deben sumarse como si fueran papers unicos.

core_area_sizes <- core_area_long |>
  count(core_area, name = "n_papers") |>
  mutate(
    percent_of_corpus = 100 * n_papers / nrow(corpus),
    classification_note = "Areas superpuestas: un paper puede aparecer en mas de una."
  ) |>
  arrange(desc(n_papers))

topics_by_core_gamma <- outputs_recommended$gamma |>
  select(UT, topic, topic_label, gamma) |>
  inner_join(core_area_long, by = "UT", relationship = "many-to-many") |>
  group_by(core_area, topic, topic_label) |>
  summarise(
    n_papers = n_distinct(UT),
    mean_gamma = mean(gamma),
    median_gamma = median(gamma),
    expected_papers = sum(gamma),
    .groups = "drop"
  ) |>
  mutate(percent_gamma = 100 * mean_gamma) |>
  arrange(core_area, desc(mean_gamma))

topics_by_core_dominant <- outputs_recommended$papers |>
  select(UT, dominant_topic, topic_label) |>
  inner_join(core_area_long, by = "UT", relationship = "many-to-many") |>
  count(core_area, dominant_topic, topic_label, name = "n_papers") |>
  group_by(core_area) |>
  mutate(percent = 100 * n_papers / sum(n_papers)) |>
  ungroup() |>
  arrange(core_area, desc(percent))

top3_core_gamma <- topics_by_core_gamma |>
  group_by(core_area) |>
  slice_max(mean_gamma, n = 3L, with_ties = FALSE) |>
  ungroup() |>
  arrange(core_area, desc(mean_gamma))
# Esta es la tabla principal para "top topics por disciplina", porque conserva
# toda la mezcla tematica de los papers.

top3_core_dominant <- topics_by_core_dominant |>
  group_by(core_area) |>
  slice_max(percent, n = 3L, with_ties = FALSE) |>
  ungroup() |>
  arrange(core_area, desc(percent))
# Se exporta para comprobar si la conclusion cambia al usar un solo topic por paper.

filter_dictionary <- bind_rows(
  tibble(group = "connector", original = connector_terms),
  tibble(group = "methodological", original = methodological_terms),
  tibble(group = "generic_adjective", original = generic_adjectives),
  tibble(group = "corpus_generic", original = corpus_generic_terms),
  tibble(group = "artifact", original = artifact_terms),
  tibble(group = "geographic", original = geographic_terms)
) |>
  transmute(
    rule_type = "removed_token",
    group,
    original,
    model_form = NA_character_,
    keep_in_model = FALSE
  ) |>
  bind_rows(
    term_normalization |>
      transmute(
        rule_type = "normalization",
        group = "word_variant",
        original = variant,
        model_form = normalized,
        keep_in_model = !normalized %in% generic_terms
      ),
    compound_terms |>
      transmute(
        rule_type = "compound",
        group = "multiword_expression",
        original = phrase,
        model_form = token,
        keep_in_model
      )
  ) |>
  distinct() |>
  arrange(rule_type, group, original)
# El diccionario documenta todas las decisiones de remocion, unificacion y
# proteccion de frases; permite reproducir y discutir el preprocesamiento.

dominant_probability_summary <- outputs_recommended$papers |>
  summarise(
    minimum = min(topic_probability),
    p05 = quantile(topic_probability, 0.05),
    median = median(topic_probability),
    mean = mean(topic_probability),
    p95 = quantile(topic_probability, 0.95),
    maximum = max(topic_probability),
    below_040 = sum(topic_probability < 0.40),
    below_050 = sum(topic_probability < 0.50)
  )
# Probabilidades dominantes bajas indican documentos tematicamente mixtos y
# refuerzan que los analisis principales deben usar gamma completa.

model_summary <- tibble(
  measure = c(
    "Corpus papers",
    "Papers in primary DTM",
    "Primary vocabulary",
    "Training papers",
    "Validation papers used",
    "Validation papers without known terms",
    "Papers flagged as low retained text",
    "Recommended k",
    "Exploratory k values",
    "Candidate seeds per k",
    "Selected seed for recommended model",
    "Primary text",
    "Sensitivity text",
    "Primary interpretation"
  ),
  value = as.character(c(
    nrow(corpus),
    nrow(dtm_primary),
    ncol(dtm_primary),
    nrow(dtm_train),
    nrow(dtm_test),
    sum(!test_nonempty),
    sum(document_coverage$low_text_flag),
    recommended_k,
    paste(k_exploratory, collapse = ", "),
    length(selection_seeds),
    final_ensembles[[as.character(recommended_k)]]$selected_seed,
    "Title + abstract",
    "Title + abstract + keywords",
    "Average gamma; dominant topic is complementary"
  ))
)

seed_stability <- bind_rows(
  candidate_stability_pairs |>
    mutate(record_type = "candidate_pair"),
  candidate_medoid_scores |>
    mutate(record_type = "candidate_medoid_score"),
  final_stability_pairs |>
    mutate(record_type = "final_pair"),
  final_medoid_scores |>
    mutate(record_type = "final_medoid_score")
) |>
  relocate(record_type)

# -----------------------------------------------------------------------------
# 8. Exportar un unico Excel de resultados auditables
# -----------------------------------------------------------------------------
# Se evita crear bases intermedias. El libro separa cobertura, seleccion del
# modelo, resultados principales y sensibilidades en hojas con nombres breves.

dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

write_xlsx(
  list(
    Model_summary = model_summary,
    Corpus_coverage = coverage,
    Document_coverage = document_coverage,
    Token_audit = token_audit,
    Model_selection = model_selection,
    Seed_metrics = candidate_seed_metrics,
    Seed_stability = seed_stability,
    Dominant_probability = dominant_probability_summary,
    Topic_labels = outputs_recommended$labels,
    Top_terms = outputs_recommended$terms,
    Paper_topic_gamma = outputs_recommended$gamma |>
      select(
        UT, topic, gamma, topic_label, year, title, journal,
        keywords_analisis, core_area
      ),
    Paper_dominant_topic = outputs_recommended$papers,
    Topics_year_gamma = topics_by_year_gamma,
    Topics_year_dominant = topics_by_year_dominant,
    Core_area_sizes = core_area_sizes,
    Topics_core_gamma = topics_by_core_gamma,
    Topics_core_dominant = topics_by_core_dominant,
    Top3_core_gamma = top3_core_gamma,
    Top3_core_dominant = top3_core_dominant,
    Keyword_sensitivity = keyword_topic_alignment,
    Top_terms_k4 = outputs_k4$terms,
    Top_terms_k10 = outputs_k10$terms,
    Ngram_candidates = ngram_candidates,
    Compound_audit = term_counts_audit,
    Filter_dictionary = filter_dictionary
  ),
  file.path(tables_dir, "topic_model_affi_latam.xlsx")
)

# -----------------------------------------------------------------------------
# 9. Exportar figuras para interpretar y comparar soluciones
# -----------------------------------------------------------------------------
# k=4 muestra la sintesis; k=10 muestra mayor diferenciacion; la figura principal
# usa el k recomendado por validacion y estabilidad.

plot_top_terms <- function(
  topic_terms,
  title_text,
  subtitle_text,
  file_name,
  width = 13,
  height = 9
) {
  # Ordena los terminos dentro de cada topic y guarda barras comparables.
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
  "Sintesis parsimoniosa; titulo + abstract; modelo estable entre semillas",
  "topic_model_top_terms_k4.png"
)

plot_top_terms(
  outputs_k10$terms,
  "Terminos principales: k = 10",
  "Lectura desagregada; titulo + abstract; modelo estable entre semillas",
  "topic_model_top_terms_k10.png"
)

plot_top_terms(
  outputs_recommended$terms,
  str_c("Terminos principales: k = ", recommended_k),
  "K recomendado por validacion, coherencia, exclusividad y estabilidad",
  "topic_model_top_terms.png"
)

core_heatmap_data <- topics_by_core_gamma |>
  mutate(topic_short = str_c("T", topic))

core_heatmap <- ggplot(
  core_heatmap_data,
  aes(topic_short, fct_reorder(core_area, n_papers), fill = percent_gamma)
) +
  geom_tile(color = "white", linewidth = 0.4) +
  scale_fill_viridis_c(option = "C", direction = -1) +
  labs(
    title = "Peso promedio de cada topic por core area",
    subtitle = "Promedio de gamma; las areas pueden superponerse",
    x = "Topic",
    y = NULL,
    fill = "% gamma"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank())

ggsave(
  file.path(figures_dir, "topic_model_core_area_heatmap.png"),
  core_heatmap,
  width = 10,
  height = 6,
  dpi = 320
)

print(model_summary)
print(model_selection)
print(outputs_recommended$labels)
print(dominant_probability_summary)