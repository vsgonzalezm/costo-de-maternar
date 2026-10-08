library(openalexR)
library(dplyr)

options(openalexR.mailto = "tania.hutt@uc.cl")

# Load topics (already saved before)
soc_ps <- read.csv("topics_sociology_polisci.csv", stringsAsFactors = FALSE)
ps_ir  <- read.csv("topics_political_science_IR.csv", stringsAsFactors = FALSE)
econ   <- read.csv("topics_economics.csv", stringsAsFactors = FALSE)
topic_ids <- unique(c(soc_ps$topic_id, ps_ir$topic_id, econ$topic_id))

# Latin American countries
latam <- c("AR","BO","BR","CL","CO","CR","CU","DO","EC",
           "GT","HN","MX","NI","PA","PE","PY","SV","UY","VE")

years <- 2000:2025  

# Split topic IDs into small chunks (avoid 429)
split_ids <- split(topic_ids, ceiling(seq_along(topic_ids) / 10))

# Helper to fetch one chunk
fetch_chunk <- function(ids, country, year) {
  Sys.sleep(1)  # polite pause
  tryCatch({
    oa_fetch(
      entity = "works",
      `topics.id` = ids,
      `authorships.institutions.country_code` = country,
      from_publication_date = sprintf("%d-01-01", year),
      to_publication_date   = sprintf("%d-12-31", year),
      per_page = 200,
      abstract = TRUE,
      verbose = FALSE
    )
  }, error = function(e) {
    message("Chunk failed for ", country, "-", year, ": ", conditionMessage(e))
    return(tibble())
  })
}

# Main loop
for (cty in latam) {
  for (yr in years) {
    out_file <- sprintf("works_%s_%d.csv", cty, yr)
    if (file.exists(out_file)) {
      message("Skipping existing file: ", out_file)
      next
    }
    
    message("Fetching ", cty, " - ", yr)
    results_list <- lapply(split_ids, fetch_chunk, country = cty, year = yr)
    works_raw <- bind_rows(results_list)
    
    # Normalise columns
    needed <- c("id", "display_name", "publication_year", "doi",
                "source_display_name", "abstract",
                "language", "keywords", "version", "source_id", "type", "topics")
    for (nm in needed) if (!nm %in% names(works_raw)) works_raw[[nm]] <- NA
    
    # Flatten list-columns safely
    flatten_col <- function(x) {
      if (is.list(x)) {
        sapply(x, function(y) {
          if (is.null(y)) return(NA_character_)
          if (length(y) == 0) return(NA_character_)
          paste(as.character(y), collapse = "; ")
        })
      } else {
        x
      }
    }
    
    works_tidy <- works_raw %>%
      transmute(
        id,
        title    = display_name,
        year     = publication_year,
        doi,
        journal  = source_display_name,
        abstract = flatten_col(abstract),
        language = flatten_col(language),
        keywords = flatten_col(keywords),
        version  = flatten_col(version),
        source_id= flatten_col(source_id),
        type     = flatten_col(type),
        topics   = flatten_col(topics), 
        country  = cty
      )
    
    cat("Fetched rows:", nrow(works_tidy), "for", cty, yr, "\n")
    write.csv(works_tidy, out_file, row.names = FALSE)
  }
}

# Merge all into one big dataset
all_files <- list.files(pattern = "^works_[A-Z]{2}_[0-9]{4}\\.csv$")
all_data  <- do.call(rbind, lapply(all_files, read.csv, stringsAsFactors = FALSE))

write.csv(all_data, "works_LATAM_2000_2025.csv", row.names = FALSE)
cat("Final merged dataset rows:", nrow(all_data), "\n")
