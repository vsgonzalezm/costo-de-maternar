############################################################
# TEST FETCH: ASR, last 10 years
# ALL VARS + COUNTRY AUDIT FROM RAW LIST OUTPUT
############################################################

suppressPackageStartupMessages({
  library(openalexR)
  library(dplyr)
  library(readr)
  library(purrr)
  library(jsonlite)
  library(tibble)
  library(stringr)
})

options(openalexR.mailto = "tania.hutt@uc.cl")

out_file_wide <- "~/Google Drive/My Drive/Paper_Metodos_Exp/OpenAlex/test_ASR_2015_2025_ALLVARS_COUNTRY_AUDIT_WIDE.csv"
out_file_long <- "~/Google Drive/My Drive/Paper_Metodos_Exp/OpenAlex/test_ASR_2015_2025_ALLVARS_COUNTRY_AUDIT_LONG.csv"

# ----------------------------
# Step 1: Look up ASR source ID
# ----------------------------
asr_source <- oa_fetch(
  entity  = "sources",
  search  = "American Sociological Review",
  verbose = TRUE
)

print(asr_source[, c("id", "display_name", "issn_l", "works_count")])

asr_id <- asr_source$id[asr_source$display_name == "American Sociological Review"][1]
message("Using source ID: ", asr_id)

# ----------------------------
# Step 2: Date window
# ----------------------------
from_date <- sprintf("%d-01-01", as.integer(format(Sys.Date(), "%Y")) - 10)
to_date   <- format(Sys.Date(), "%Y-%m-%d")

# ----------------------------
# Step 3: Fetch RAW LIST output
# ----------------------------
w_raw <- oa_fetch(
  entity                     = "works",
  primary_location.source.id = asr_id,
  from_publication_date      = from_date,
  to_publication_date        = to_date,
  per_page                   = 200,
  abstract                   = TRUE,
  output                     = "list",
  verbose                    = TRUE
)

message("Raw works fetched: ", length(w_raw))

# ----------------------------
# Helpers
# ----------------------------
collapse_unique <- function(x){
  x <- unique(as.character(x))
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NA_character_)
  paste(x, collapse = "; ")
}

pluck_or_na <- function(x, path){
  out <- purrr::pluck(x, !!!path, .default = NA)
  
  if (is.null(out) || length(out) == 0) return(NA)
  
  if (is.atomic(out) && length(out) == 1) return(out)
  
  if (is.atomic(out)) return(paste(as.character(out), collapse = "; "))
  
  NA
}

best_topic_entry <- function(topics_tbl, type_name){
  if (is.null(topics_tbl) || !is.list(topics_tbl) || length(topics_tbl) == 0) {
    return(list(id = NA_character_, name = NA_character_))
  }
  
  topic_df <- purrr::map_dfr(topics_tbl, function(x){
    tibble(
      id = purrr::pluck(x, "id", .default = NA_character_),
      display_name = purrr::pluck(x, "display_name", .default = NA_character_),
      type = purrr::pluck(x, "type", .default = NA_character_),
      score = suppressWarnings(as.numeric(purrr::pluck(x, "score", .default = NA_real_)))
    )
  })
  
  x <- topic_df %>% filter(type == type_name)
  if (nrow(x) == 0) return(list(id = NA_character_, name = NA_character_))
  
  j <- which.max(x$score)
  list(id = x$id[j], name = x$display_name[j])
}

flatten_topic_names <- function(topics_tbl){
  if (is.null(topics_tbl) || !is.list(topics_tbl) || length(topics_tbl) == 0) return(NA_character_)
  
  vals <- unlist(lapply(topics_tbl, function(tt){
    if (!is.null(tt$type) && tt$type == "topic") tt$display_name else NULL
  }), use.names = FALSE)
  
  collapse_unique(vals)
}

flatten_topic_ids <- function(topics_tbl){
  if (is.null(topics_tbl) || !is.list(topics_tbl) || length(topics_tbl) == 0) return(NA_character_)
  
  vals <- unlist(lapply(topics_tbl, function(tt){
    if (!is.null(tt$type) && tt$type == "topic") tt$id else NULL
  }), use.names = FALSE)
  
  collapse_unique(vals)
}

extract_work_summary <- function(w){
  
  auths <- w$authorships
  if (is.null(auths)) auths <- list()
  
  topics <- w$topics
  if (is.null(topics)) topics <- list()
  
  dom  <- best_topic_entry(topics, "domain")
  fld  <- best_topic_entry(topics, "field")
  subf <- best_topic_entry(topics, "subfield")
  
  auth_countries <- unlist(lapply(auths, function(a){
    if (is.null(a$countries)) return(NULL)
    a$countries
  }), use.names = FALSE)
  
  inst_countries <- unlist(lapply(auths, function(a){
    insts <- a$institutions
    if (is.null(insts)) return(NULL)
    unlist(lapply(insts, function(inst) inst$country_code), use.names = FALSE)
  }), use.names = FALSE)
  
  inst_names <- unlist(lapply(auths, function(a){
    insts <- a$institutions
    if (is.null(insts)) return(NULL)
    unlist(lapply(insts, function(inst) inst$display_name), use.names = FALSE)
  }), use.names = FALSE)
  
  inst_ids <- unlist(lapply(auths, function(a){
    insts <- a$institutions
    if (is.null(insts)) return(NULL)
    unlist(lapply(insts, function(inst) inst$id), use.names = FALSE)
  }), use.names = FALSE)
  
  tibble::tibble(
    id = as.character(pluck_or_na(w, c("id"))),
    title = as.character(pluck_or_na(w, c("display_name"))),
    publication_year = suppressWarnings(as.integer(pluck_or_na(w, c("publication_year")))),
    publication_date = as.character(pluck_or_na(w, c("publication_date"))),
    doi = as.character(pluck_or_na(w, c("doi"))),
    type = as.character(pluck_or_na(w, c("type"))),
    type_crossref = as.character(pluck_or_na(w, c("type_crossref"))),
    language = as.character(pluck_or_na(w, c("language"))),
    
    cited_by_count = suppressWarnings(as.integer(pluck_or_na(w, c("cited_by_count")))),
    countries_distinct_count = suppressWarnings(as.integer(pluck_or_na(w, c("countries_distinct_count")))),
    institutions_distinct_count = suppressWarnings(as.integer(pluck_or_na(w, c("institutions_distinct_count")))),
    authorships_count = as.integer(length(auths)),
    
    primary_source_id = as.character(pluck_or_na(w, c("primary_location", "source", "id"))),
    primary_source_name = as.character(pluck_or_na(w, c("primary_location", "source", "display_name"))),
    primary_source_issn_l = as.character(pluck_or_na(w, c("primary_location", "source", "issn_l"))),
    
    oa_domain_id = dom$id,
    oa_domain_name = dom$name,
    oa_field_id = fld$id,
    oa_field_name = fld$name,
    oa_subfield_id = subf$id,
    oa_subfield_name = subf$name,
    oa_topic_ids = flatten_topic_ids(topics),
    oa_topic_names = flatten_topic_names(topics),
    
    authorship_country_codes = collapse_unique(auth_countries),
    institution_country_codes = collapse_unique(inst_countries),
    institution_names_all = collapse_unique(inst_names),
    institution_ids_all = collapse_unique(inst_ids),
    
    abstract_text = as.character(pluck_or_na(w, c("abstract"))),
    keywords = collapse_unique(unlist(w$keywords, use.names = FALSE)),
    
    pull_from_date = from_date,
    pull_to_date = to_date,
    
    raw_json = as.character(
      jsonlite::toJSON(
        w,
        auto_unbox = TRUE,
        null = "null",
        dataframe = "rows"
      )
    )
  )
}

extract_authorship_long <- function(w){
  
  auths <- w$authorships
  
  if (is.null(auths) || length(auths) == 0) {
    return(tibble(
      work_id = as.character(pluck_or_na(w, c("id"))),
      title = as.character(pluck_or_na(w, c("display_name"))),
      authorship_position = NA_integer_,
      author_id = NA_character_,
      author_name = NA_character_,
      authorship_countries = NA_character_,
      raw_affiliation_strings = NA_character_,
      institution_id = NA_character_,
      institution_name = NA_character_,
      institution_country_code = NA_character_
    ))
  }
  
  purrr::map_dfr(seq_along(auths), function(i){
    a <- auths[[i]]
    
    author_id <- as.character(purrr::pluck(a, "author", "id", .default = NA_character_))
    author_name <- as.character(purrr::pluck(a, "author", "display_name", .default = NA_character_))
    
    auth_countries <- if (!is.null(a$countries)) collapse_unique(a$countries) else NA_character_
    raw_affil <- if (!is.null(a$raw_affiliation_strings)) collapse_unique(a$raw_affiliation_strings) else NA_character_
    
    insts <- a$institutions
    
    if (is.null(insts) || length(insts) == 0) {
      tibble(
        work_id = as.character(pluck_or_na(w, c("id"))),
        title = as.character(pluck_or_na(w, c("display_name"))),
        authorship_position = as.integer(i),
        author_id = author_id,
        author_name = author_name,
        authorship_countries = auth_countries,
        raw_affiliation_strings = raw_affil,
        institution_id = NA_character_,
        institution_name = NA_character_,
        institution_country_code = NA_character_
      )
    } else {
      purrr::map_dfr(insts, function(inst){
        tibble(
          work_id = as.character(pluck_or_na(w, c("id"))),
          title = as.character(pluck_or_na(w, c("display_name"))),
          authorship_position = as.integer(i),
          author_id = author_id,
          author_name = author_name,
          authorship_countries = auth_countries,
          raw_affiliation_strings = raw_affil,
          institution_id = as.character(purrr::pluck(inst, "id", .default = NA_character_)),
          institution_name = as.character(purrr::pluck(inst, "display_name", .default = NA_character_)),
          institution_country_code = as.character(purrr::pluck(inst, "country_code", .default = NA_character_))
        )
      })
    }
  })
}

# ----------------------------
# Step 4: Build datasets
# ----------------------------
wide_audit <- purrr::map_dfr(w_raw, extract_work_summary)
long_audit <- purrr::map_dfr(w_raw, extract_authorship_long)

# ----------------------------
# Step 5: Quick checks
# ----------------------------
message("Wide rows: ", nrow(wide_audit))
message("Long rows: ", nrow(long_audit))

print(
  wide_audit %>%
    select(id, title, authorship_country_codes, institution_country_codes) %>%
    head(10)
)

print(
  long_audit %>%
    count(institution_country_code, sort = TRUE)
)

# ----------------------------
# Step 6: Save
# ----------------------------
write_csv(wide_audit, out_file_wide)
write_csv(long_audit, out_file_long)

message("✅ Saved wide file: ", out_file_wide)
message("✅ Saved long file: ", out_file_long)