## Debugging

library(openalexR)
library(dplyr)

options(openalexR.mailto = "tania.hutt@uc.cl")

oa_fetch(
  entity="works",
  `authorships.institutions.country_code`="AR",
  from_publication_date="2000-01-01",
  to_publication_date="2000-12-31",
  count_only=TRUE
)

samp <- oa_fetch(
  entity = "works",
  `authorships.institutions.country_code` = "AR",
  from_publication_date = "2000-01-01",
  to_publication_date   = "2000-12-31",
  per_page = 50,
  abstract = FALSE,
  verbose = FALSE
)

names(samp)
head(samp$topics, 3)

table(sapply(samp$topics, function(x) any(x$type == "field")))


get_primary_field <- function(topics_tbl) {
  # topics_tbl is a tibble with a 'type' column like topic/subfield/field/domain
  if (is.null(topics_tbl) || !is.data.frame(topics_tbl)) return(NA_character_)
  f <- topics_tbl[topics_tbl$type == "field", , drop = FALSE]
  if (nrow(f) == 0) return(NA_character_)
  # pick the field with the highest score (or first if tied)
  f$display_name[which.max(f$score)][1]
}

samp$field <- sapply(samp$topics, get_primary_field)
table(samp$field, useNA = "ifany")