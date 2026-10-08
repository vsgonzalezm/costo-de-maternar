############################################################
# OpenAlex LATAM 2000–2025 overnight pull (restart-safe)
# - Fetch by country+year; auto-split to months when large
# - Keeps: domain/field/subfield (primary by max score),
#          plus all topic ids/names (topic-level),
#          plus journal/source info, abstract, keywords, etc.
# - Saves progress incrementally to CSVs and skips completed pieces
############################################################

# ----------------------------
# 0) Packages + options
# ----------------------------
suppressPackageStartupMessages({
  library(openalexR)
  library(dplyr)
  library(stringr)
  library(readr)
  library(purrr)
})

# Set your polite pool email
options(openalexR.mailto = "tania.hutt@uc.cl")
# If you have OpenAlex Premium, uncomment:
# options(openalexR.apikey = "YOUR_API_KEY")

# ----------------------------
# 1) Output folder (restart-safe)
# ----------------------------
# Change this if you want a different location
base_dir <- "~/Google Drive/My Drive/Paper_Metodos_Exp/OpenAlex/pulls_SUBFIELD"
dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)

make_outdir <- function(country){
  d <- file.path(base_dir, country)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

# ----------------------------
# 2) Helpers: dates for month split
# ----------------------------
month_start <- function(year, month){
  as.Date(sprintf("%d-%02d-01", year, as.integer(month)))
}
month_end <- function(year, month){
  ms <- month_start(year, month)
  next_ms <- seq(ms, by = "1 month", length.out = 2)[2]
  next_ms - 1
}

# ----------------------------
# 3) Helpers: topics → primary domain/field/subfield
# ----------------------------
best_topic_entry <- function(topics_tbl, type_name){
  if (is.null(topics_tbl) || !is.data.frame(topics_tbl)) return(list(id = NA_character_, name = NA_character_))
  x <- topics_tbl[topics_tbl$type == type_name, , drop = FALSE]
  if (nrow(x) == 0) return(list(id = NA_character_, name = NA_character_))
  j <- which.max(x$score)
  list(id = x$id[j], name = x$display_name[j])
}

flatten_topic_names <- function(topics_tbl){
  if (is.null(topics_tbl) || !is.data.frame(topics_tbl)) return(NA_character_)
  x <- topics_tbl[topics_tbl$type == "topic", , drop = FALSE]
  if (nrow(x) == 0) return(NA_character_)
  paste(unique(x$display_name), collapse = "; ")
}

flatten_topic_ids <- function(topics_tbl){
  if (is.null(topics_tbl) || !is.data.frame(topics_tbl)) return(NA_character_)
  x <- topics_tbl[topics_tbl$type == "topic", , drop = FALSE]
  if (nrow(x) == 0) return(NA_character_)
  paste(unique(x$id), collapse = "; ")
}

# Generic flattener for list/atomic fields
flatten_any <- function(x){
  if (is.null(x) || length(x) == 0) return(NA_character_)
  if (is.atomic(x)) return(paste(unique(x), collapse = "; "))
  if (is.list(x)) {
    parts <- unlist(lapply(x, function(y){
      if (is.null(y) || length(y) == 0) return(NA_character_)
      if (is.character(y)) return(y)
      if (is.list(y) && !is.null(y$display_name)) return(y$display_name)
      if (is.list(y) && !is.null(y$name)) return(y$name)
      return(as.character(unlist(y)))
    }))
    return(paste(unique(na.omit(parts)), collapse = "; "))
  }
  as.character(x)
}

# ----------------------------
# 4) Keep rules: your disciplines by SUBFIELD
# ----------------------------
keep_subfields_exact <- c(
  "Sociology and Political Science",
  "Political Science and International Relations"
)
keep_subfield_regex <- regex("^Economics$|Economics", ignore_case = TRUE)

# If you want to broaden a bit later, you can also include:
# "General Social Sciences", "Demography", "Development", etc.

# ----------------------------
# 5) Safe fetch wrapper: retries + exponential backoff
# ----------------------------
safe_oa_fetch <- function(..., max_tries = 6, base_sleep = 3){
  for (i in seq_len(max_tries)){
    out <- tryCatch(
      oa_fetch(...),
      error = function(e) e
    )
    if (!inherits(out, "error")) return(out)
    
    msg <- conditionMessage(out)
    wait <- base_sleep * (2^(i-1)) + runif(1, 0, 2)
    
    message("Fetch error: ", msg)
    message("Retry ", i, "/", max_tries, " in ", round(wait, 1), "s")
    Sys.sleep(wait)
  }
  return(NULL)
}

# ----------------------------
# 6) Fetch + classify + save one unit (country-year or country-year-month)
# ----------------------------
fetch_and_save <- function(country, year, month = NULL){
  
  outdir <- make_outdir(country)
  
  tag <- if (is.null(month)) {
    sprintf("%s_%d", country, year)
  } else {
    sprintf("%s_%d_%02d", country, year, as.integer(month))
  }
  
  outfile <- file.path(outdir, paste0("works_", tag, ".csv"))
  
  # Restart-safe: skip if file already exists
  if (file.exists(outfile)) {
    message("✅ Skip existing: ", basename(outfile))
    return(invisible(TRUE))
  }
  
  # If we're about to do a YEAR pull but monthly files exist, skip the yearly pull
  if (is.null(month)) {
    month_files <- list.files(outdir, pattern = paste0("^works_", country, "_", year, "_\\d{2}\\.csv$"))
    if (length(month_files) > 0) {
      message("✅ Monthly files already exist for ", country, "-", year, ". Skipping yearly file.")
      return(invisible(TRUE))
    }
  }
  
  # Date range
  if (is.null(month)) {
    from <- sprintf("%d-01-01", year)
    to   <- sprintf("%d-12-31", year)
  } else {
    from <- as.character(month_start(year, month))
    to   <- as.character(month_end(year, month))
  }
  
  message("\n===== Fetching ", tag, " (", from, " to ", to, ") =====")
  
  # Count-only precheck
  cnt_obj <- safe_oa_fetch(
    entity = "works",
    `authorships.institutions.country_code` = country,
    from_publication_date = from,
    to_publication_date   = to,
    count_only = TRUE,
    verbose = FALSE
  )
  
  cnt <- if (is.null(cnt_obj)) NA_real_ else cnt_obj$count[1]
  message("Count: ", cnt)
  
  if (is.na(cnt) || cnt == 0) {
    message("⚠️ No works. Skipping.")
    return(invisible(TRUE))
  }
  
  # Fetch full data
  w <- safe_oa_fetch(
    entity = "works",
    `authorships.institutions.country_code` = country,
    from_publication_date = from,
    to_publication_date   = to,
    per_page = 200,
    abstract = TRUE,
    verbose = FALSE
  )
  
  if (is.null(w) || !is.data.frame(w) || nrow(w) == 0) {
    message("⚠️ Empty fetch result. Skipping.")
    return(invisible(FALSE))
  }
  
  # Guarantee columns exist (openalexR output can vary)
  needed <- c("id","display_name","publication_year","doi",
              "source_display_name","source_id","type","abstract",
              "language","keywords","version","topics","cited_by_count")
  for (nm in needed) if (!nm %in% names(w)) w[[nm]] <- NA
  
  # Compute primary domain/field/subfield + flatten topic lists
  dom  <- lapply(w$topics, best_topic_entry, type_name = "domain")
  fld  <- lapply(w$topics, best_topic_entry, type_name = "field")
  subf <- lapply(w$topics, best_topic_entry, type_name = "subfield")
  
  w2 <- w %>%
    mutate(
      domain_id    = sapply(dom, `[[`, "id"),
      domain_name  = sapply(dom, `[[`, "name"),
      field_id     = sapply(fld, `[[`, "id"),
      field_name   = sapply(fld, `[[`, "name"),
      subfield_id  = sapply(subf, `[[`, "id"),
      subfield_name= sapply(subf, `[[`, "name"),
      
      topic_ids    = sapply(topics, flatten_topic_ids),
      topic_names  = sapply(topics, flatten_topic_names),
      
      abstract = sapply(abstract, flatten_any),
      language = sapply(language, flatten_any),
      keywords = sapply(keywords, flatten_any),
      version  = sapply(version, flatten_any)
    ) %>%
    # Keep only your target subfields (primary)
    filter(
      subfield_name %in% keep_subfields_exact |
        str_detect(subfield_name, keep_subfield_regex)
    ) %>%
    transmute(
      id,
      title = display_name,
      year  = publication_year,
      doi,
      journal = source_display_name,
      source_id,
      type,
      language,
      version,
      keywords,
      abstract,
      cited_by = cited_by_count,
      
      domain_id, domain_name,
      field_id, field_name,
      subfield_id, subfield_name,
      
      topic_ids,
      topic_names,
      
      country = country,
      from_date = from,
      to_date   = to
    )
  
  if (nrow(w2) == 0) {
    message("⚠️ After subfield filter: 0 rows. Not writing file.")
    return(invisible(TRUE))
  }
  
  write_csv(w2, outfile)
  message("💾 Saved ", nrow(w2), " rows → ", basename(outfile))
  
  invisible(TRUE)
}

# ----------------------------
# 7) Main loop: LATAM 2000–2025 with dynamic splitting
# ----------------------------
latam <- c("AR","BO","BR","CL","CO","CR","CU","DO","EC",
           "GT","HN","MX","NI","PA","PE","PY","SV","UY","VE")
years  <- 2000:2025
months <- 1:12

# Split if country-year count exceeds this
THRESHOLD_SPLIT <- 20000

# Optional: if extremely huge, add a bit more sleep during month pulls
HUGE_SLEEP_THRESHOLD <- 150000

for (cty in latam) {
  for (yr in years) {
    
    # Count the full year first
    cnt_obj <- safe_oa_fetch(
      entity = "works",
      `authorships.institutions.country_code` = cty,
      from_publication_date = sprintf("%d-01-01", yr),
      to_publication_date   = sprintf("%d-12-31", yr),
      count_only = TRUE,
      verbose = FALSE
    )
    
    cnt <- if (is.null(cnt_obj)) NA_real_ else cnt_obj$count[1]
    message("\n>>> ", cty, "-", yr, " total works = ", cnt)
    
    if (is.na(cnt) || cnt == 0) next
    
    # Always split Brazil; also split any large country-year above threshold
    if (cty == "BR" || cnt > THRESHOLD_SPLIT) {
      
      message("🔀 Splitting into months for ", cty, "-", yr, " (", cnt, " > ", THRESHOLD_SPLIT, ")")
      
      for (m in months) {
        fetch_and_save(cty, yr, month = m)
        
        # small sleep to reduce 429 risk
        Sys.sleep(1)
        
        # extra sleep for very large years (optional)
        if (!is.na(cnt) && cnt > HUGE_SLEEP_THRESHOLD) Sys.sleep(2)
      }
      
    } else {
      
      fetch_and_save(cty, yr, month = NULL)
      Sys.sleep(1)
      
    }
  }
}

# ----------------------------
# 8) Merge everything into one dataset (run next morning)
# ----------------------------
# This only reads what exists; no refetching.
all_csv <- list.files(base_dir, pattern = "^works_.*\\.csv$", recursive = TRUE, full.names = TRUE)
message("Found CSV parts: ", length(all_csv))

all_data <- purrr::map_dfr(all_csv, readr::read_csv, show_col_types = FALSE)
out_merged <- file.path(base_dir, "works_LATAM_2000_2025_SUBFIELD.csv")
write_csv(all_data, out_merged)

message("✅ Final merged rows: ", nrow(all_data))
message("✅ Saved merged file: ", out_merged)
