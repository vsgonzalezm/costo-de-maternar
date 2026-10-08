library(openalexR)
library(dplyr)

options(openalexR.mailto = "tania.hutt@uc.cl")

# --- Load topic sets ---
soc_ps <- read.csv("topics_sociology_polisci.csv", stringsAsFactors = FALSE)
ps_ir  <- read.csv("topics_political_science_IR.csv", stringsAsFactors = FALSE)
econ   <- read.csv("topics_economics.csv", stringsAsFactors = FALSE)
topic_ids <- unique(c(soc_ps$topic_id, ps_ir$topic_id, econ$topic_id))

# --- Latin American countries ---
latam <- c("AR","BO","BR","CL","CO","CR","CU","DO","EC",
           "GT","HN","MX","NI","PA","PE","PY","SV","UY","VE")
years <- 2000:2025

# --- Split topic IDs into smaller batches ---
split_ids <- split(topic_ids, ceiling(seq_along(topic_ids) / 5))

# --- Universal flatten helper ---
flatten_any <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (length(x) == 0) return(NA_character_)
  if (is.atomic(x)) return(paste(unique(x), collapse = "; "))
  if (is.list(x)) {
    parts <- unlist(lapply(x, function(y) {
      if (is.null(y)) return(NA_character_)
      if (is.character(y)) return(y)
      if (is.list(y)) {
        if (!is.null(y$display_name)) return(y$display_name)
        if (!is.null(y$name)) return(y$name)
        return(unlist(y))
      }
      return(as.character(y))
    }))
    return(paste(na.omit(unique(parts)), collapse = "; "))
  }
  return(as.character(x))
}

# --- Safe fetch helper with pre-check and column normalization ---
fetch_chunk <- function(ids, country, year, max_retries = 3, max_allowed = 20000) {
  Sys.sleep(1)
  out_list <- list()
  
  for (tid in ids) {
    # --- 1. Count pre-check ---
    cnt <- tryCatch({
      res <- oa_fetch(
        entity = "works",
        `topics.id` = tid,
        `authorships.institutions.country_code` = country,
        from_publication_date = sprintf("%d-01-01", year),
        to_publication_date   = sprintf("%d-12-31", year),
        count_only = TRUE
      )
      if (is.data.frame(res) && "count" %in% names(res)) {
        return(res$count[1])
      } else if (is.numeric(res)) {
        return(res[1])
      } else {
        return(NA_real_)
      }
    }, error = function(e) NA_real_)
    
    if (is.na(cnt) || cnt == 0) {
      message("Skipping ", country, "-", year, " topic ", tid, " (no records)")
      next
    }
    if (cnt > max_allowed) {
      message("Skipping ", country, "-", year, " topic ", tid, " (", cnt, " > ", max_allowed, ")")
      next
    }
    
    # --- 2. Fetch with retries ---
    for (i in seq_len(max_retries)) {
      try({
        res <- oa_fetch(
          entity = "works",
          `topics.id` = tid,
          `authorships.institutions.country_code` = country,
          from_publication_date = sprintf("%d-01-01", year),
          to_publication_date   = sprintf("%d-12-31", year),
          per_page = 200,
          abstract = TRUE,
          verbose = FALSE
        )
        
        if (!is.null(res) && nrow(res) > 0) {
          # --- 3. Guarantee required columns exist ---
          expected_cols <- c(
            "id", "display_name", "publication_year", "doi",
            "source_display_name", "abstract", "language", "keywords",
            "version", "source_id", "type", "topics", "authorships",
            "funders", "cited_by_count"
          )
          for (col in expected_cols) {
            if (!col %in% names(res)) res[[col]] <- NA
          }
          
          # --- 4. Flatten nested fields safely ---
          res_tidy <- res %>%
            mutate(
              abstract = sapply(abstract, flatten_any),
              language = sapply(language, flatten_any),
              keywords = sapply(keywords, flatten_any),
              version  = sapply(version, flatten_any),
              source_id= sapply(source_id, flatten_any),
              type     = sapply(type, flatten_any),
              topics   = sapply(topics, flatten_any),
              authors  = sapply(authorships, flatten_any),
              funders  = sapply(funders, flatten_any)
            ) %>%
            transmute(
              id,
              title    = display_name,
              year     = publication_year,
              doi,
              journal  = source_display_name,
              abstract, language, keywords, version,
              source_id, type, topics, authors, funders,
              cited_by = cited_by_count,
              country  = country
            )
          
          out_list[[length(out_list) + 1]] <- res_tidy
          message("Fetched topic ", tid, " (", nrow(res_tidy), " rows)")
          break
        }
      }, silent = TRUE)
      message("Retry ", i, " for ", country, "-", year, " topic ", tid, " ...")
      Sys.sleep(5)
    }
  }
  
  if (length(out_list) == 0) return(tibble())
  bind_rows(out_list)
}

# --- Main loop ---
for (cty in latam) {
  for (yr in years) {
    out_file <- sprintf("works_%s_%d.csv", cty, yr)
    if (file.exists(out_file)) {
      message("Skipping existing file: ", out_file)
      next
    }
    
    message("\n===== Fetching ", cty, " - ", yr, " =====")
    results_list <- lapply(split_ids, fetch_chunk, country = cty, year = yr)
    works_tidy <- bind_rows(results_list)
    
    if (nrow(works_tidy) == 0) {
      message("No results for ", cty, " - ", yr)
      next
    }
    
    cat("Fetched rows:", nrow(works_tidy), "for", cty, yr, "\n")
    write.csv(works_tidy, out_file, row.names = FALSE)
  }
}

# --- Merge all CSVs ---
all_files <- list.files(pattern = "^works_[A-Z]{2}_[0-9]{4}\\.csv$")
all_data  <- do.call(rbind, lapply(all_files, read.csv, stringsAsFactors = FALSE))

write.csv(all_data, "works_LATAM_2000_2025.csv", row.names = FALSE)
cat("Final merged dataset rows:", nrow(all_data), "\n")


/// test
test_df <- oa_fetch(
  entity = "works",
  `topics.id` = topic_ids[1],
  `authorships.institutions.country_code` = "AR",
  from_publication_date = "2000-01-01",
  to_publication_date   = "2000-12-31",
  per_page = 200,
  abstract = TRUE,
  verbose = TRUE
)
nrow(test_df)
names(test_df)