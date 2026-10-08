############################################################
# PROYECTO: Revisión Bibliográfica - Survey Experiments
# AUTORA:   Valentina Gonzalez
# FECHA:    Noviembre 2025
# OBJETIVO: 
############################################################

suppressPackageStartupMessages({
  library(tidyverse) 
  library(httr)    # Para hacer requests HTTP
  library(rvest)  # Para parsear HTML
  library(xml2)   # Para manejar URLs
  library(jsonlite) # Para manejar JSON
})

# Operador auxiliar: si x es NULL usa y
`%||%` <- function(x, y) if (is.null(x)) y else x  

############################################################
# Funciones de ayuda para limpiar texto y URLs.
############################################################

# Arregla URLs raras: dobles slash y dominio de scielo.cl
arreglar_url <- function(u) {
  if (length(u) == 0) return(u)
  vapply(u, function(x) {
    if (is.na(x) || x == "") return(x)
    x <- sub("//scielo.php", "/scielo.php", x)
    x <- sub("^http://www\\.scielo\\.cl", "https://www.scielo.cl", x)
    x <- sub("^http://scielo\\.cl",       "https://www.scielo.cl", x)
    x
  }, character(1))
}

# Hace absoluta una URL relativa
make_absolute <- function(href, base_url) {
  if (is.na(href) || href == "") return(NA_character_)
  if (grepl("^https?://", href)) return(href)
  xml2::url_absolute(href, base_url)
}

# Saca un año de un texto
extract_year_from_text <- function(x) {
  vapply(x, function(xx) {
    if (is.na(xx) || xx == "") return(NA_character_)
    m <- regexpr("19[0-9]{2}|20[0-9]{2}", xx)
    if (m[1] != -1) substr(xx, m[1], m[1] + attr(m, "match.length") - 1) else NA_character_
  }, character(1))
}

# Saca un año de la URL
extract_year_from_url <- function(x) {
  vapply(x, function(xx) {
    if (is.na(xx) || xx == "") return(NA_character_)
    m <- regexpr("19[0-9]{2}|20[0-9]{2}", xx)
    if (m[1] != -1) substr(xx, m[1], m[1] + attr(m, "match.length") - 1) else NA_character_
  }, character(1))
}

# Intenta extraer un DOI de un texto largo usando regex
extract_doi_from_text <- function(txt) {
  if (is.null(txt) || is.na(txt) || txt == "") return(NA_character_)
  m <- regexpr("\\b10\\.\\d{4,9}/[-._;()/:A-Za-z0-9]+", txt)
  if (m[1] == -1) {
    NA_character_
  } else {
    substr(txt, m[1], m[1] + attr(m, "match.length") - 1)
  }
}

# Lee el número de resultados totales que muestra SciELO
leer_total_hits <- function(doc_text) {
  doc <- read_html(doc_text)
  node <- html_node(doc, "#TotalHits")
  if (!is.null(node)) {
    txt <- html_text2(node)
    txt <- gsub("[^0-9]", "", txt)
    if (nzchar(txt)) return(as.integer(txt))
  }
  NA_integer_
}

# Extrae las facetas (columna izquierda)
extraer_facetas <- function(soup_busqueda) {
  facet_blocks <- html_nodes(soup_busqueda, "div.facet, aside.facet, div#facets, div.sidebar .facet")
  if (length(facet_blocks) == 0) {
    return(tibble(faceta = character(), valor = character(), conteo = integer()))
  }
  purrr::map_df(facet_blocks, function(fb) {
    titulo_faceta <- html_text2(html_node(fb, "h3, h2, .facet-title"))
    if (is.na(titulo_faceta) || titulo_faceta == "") titulo_faceta <- "facet"
    items <- html_nodes(fb, "li, div.option, div.facet-item")
    purrr::map_df(items, function(it) {
      raw <- str_trim(html_text2(it))
      m <- regexpr(" ([0-9][0-9\\.]+)$", raw)
      if (m[1] != -1) {
        cnt_txt <- substr(raw, m[1] + 1, nchar(raw))
        cnt_txt <- gsub("\\.", "", cnt_txt)
        cnt <- as.integer(cnt_txt)
        val <- str_trim(substr(raw, 1, m[1]))
      } else {
        cnt <- NA_integer_
        val <- raw
      }
      tibble(faceta = titulo_faceta, valor = val, conteo = cnt)
    })
  })
}

# Convierte todos los <meta> en tibble
metas_a_lista <- function(doc) {
  metas <- html_nodes(doc, "meta")
  tibble(
    name     = html_attr(metas, "name"),
    property = html_attr(metas, "property"),
    content  = html_attr(metas, "content")
  ) %>%
    filter(!(is.na(name) & is.na(property)))
}

# Primer meta que coincida
get_meta_first <- function(meta_df, keys) {
  cand <- meta_df %>%
    filter(name %in% keys | property %in% keys) %>%
    pull(content)
  if (length(cand) > 0) cand[1] else NA_character_
}

# Todos los meta que coincidan
get_meta_all <- function(meta_df, keys) {
  meta_df %>%
    filter(name %in% keys | property %in% keys) %>%
    pull(content)
}

# HTML sospechoso (páginas comprometidas)
es_html_sospechoso <- function(txt) {
  low <- tolower(txt)
  any(grepl("slot777|casino|gacor|bet\\b", low))
}

# Página de protección tipo Cloudflare / Hold tight
es_hold_tight <- function(txt) {
  grepl("hold tight", txt, ignore.case = TRUE) ||
    grepl("temporarily unavailable", txt, ignore.case = TRUE)
}

# Leer JSON-LD si está
leer_ld_json <- function(doc) {
  scripts <- html_nodes(doc, "script[type='application/ld+json']")
  if (length(scripts) == 0) return(NULL)
  for (s in scripts) {
    txt <- html_text2(s)
    out <- tryCatch(jsonlite::fromJSON(txt), error = function(e) NULL)
    if (!is.null(out)) return(out)
  }
  NULL
}

############################################################
# Extraer información 
############################################################

obtener_datos_paper <- function(url_paper, headers_completos, sleep_article = 2) {
  
  # Estructura base
  base_row <- tibble(
    url_final = NA_character_, titulo_completo = NA_character_,
    autores_completos = NA_character_, afiliaciones = NA_character_,
    journal = NA_character_, datos_publicacion = NA_character_,
    year = NA_character_, fecha_publicacion = NA_character_,
    volume = NA_character_, issue = NA_character_, first_page = NA_character_,
    last_page = NA_character_, doi = NA_character_, idioma = NA_character_,
    abstract_completo = NA_character_, palabras_clave = NA_character_,
    referencias = NA_character_, link_pdf = NA_character_,
    meta_json = NA_character_, status_code_error = NA_character_
  )
  
  # Normalizar URL
  full_url <- arreglar_url(url_paper)
  if (!grepl("^https?://", full_url)) {
    full_url <- xml2::url_absolute(full_url, "https://search.scielo.org/")
  }
  
  message(">>> Visitando: ", full_url)
  Sys.sleep(sleep_article)
  
  # Petición
  resp <- tryCatch({
    GET(full_url, add_headers(.headers = headers_completos), timeout(15))
  }, error = function(e) NULL)
  
  # Errores de conexión
  if (is.null(resp)) {
    base_row$url_final <- full_url; base_row$status_code_error <- "Error conexión"
    return(base_row)
  }
  
  if (resp$status_code %in% c(403, 404)) {
    base_row$url_final <- full_url; base_row$status_code_error <- as.character(resp$status_code)
    return(base_row)
  }
  
  html_txt <- content(resp, as = "text", encoding = "UTF-8")
  
  # Página de protección
  if (es_hold_tight(html_txt)) {
    doc_ht <- read_html(html_txt)
    meta_df_ht <- metas_a_lista(doc_ht)
    base_row$url_final <- resp$url
    base_row$meta_json <- as.character(toJSON(meta_df_ht, auto_unbox = TRUE))
    base_row$status_code_error <- "Bloqueado (hold tight)"
    return(base_row)
  }
  
  suspicious <- es_html_sospechoso(html_txt)
  
  doc     <- read_html(html_txt)
  meta_df <- metas_a_lista(doc)
  ld      <- leer_ld_json(doc)
  
  base_row$url_final <- resp$url
  base_row$meta_json <- as.character(toJSON(meta_df, auto_unbox = TRUE))
  if (suspicious) base_row$status_code_error <- "HTML sospechoso (usamos metadatos)"
  
  # ---------- A. TÍTULO ----------
  title_meta <- get_meta_first(meta_df, c("citation_title","dc.title","DC.Title","prism.title"))
  title_html <- if (!suspicious) html_text2(html_node(doc, "h1, h1.title, h1.article-title, header h1")) else NA_character_
  title_ld   <- if (!is.null(ld) && !is.null(ld$headline)) ld$headline else NA_character_
  base_row$titulo_completo <- dplyr::coalesce(title_meta, title_ld, title_html)
  
  # ---------- B. AUTORES ----------
  authors_meta <- get_meta_all(meta_df, c("citation_author","citation_authors","dc.creator","DC.Creator"))
  authors_meta <- authors_meta[!is.na(authors_meta)]
  if (length(authors_meta) > 0) {
    base_row$autores_completos <- paste(authors_meta, collapse = "; ")
  } else if (!is.null(ld) && !is.null(ld$author)) {
    if (is.data.frame(ld$author)) {
      base_row$autores_completos <- paste(ld$author$name, collapse = "; ")
    } else if (is.list(ld$author)) {
      nm <- vapply(ld$author, function(a) a$name %||% NA_character_, character(1))
      base_row$autores_completos <- paste(nm, collapse = "; ")
    } else {
      base_row$autores_completos <- as.character(ld$author)
    }
  } else if (!suspicious) {
    base_row$autores_completos <- html_text2(html_node(doc, "div.authors, p.authors, .authorNames, ul.authors"))
  }
  
  # ---------- C. AFILIACIONES ----------
  aff_meta <- get_meta_all(meta_df, c("citation_author_institution","dc.contributor","DC.Contributor"))
  if (length(aff_meta) > 0) {
    base_row$afiliaciones <- paste(aff_meta, collapse = "; ")
  } else if (!suspicious) {
    base_row$afiliaciones <- html_text2(html_node(doc, "div.affiliations, ul.affiliations, p.affiliation"))
  }
  
  # ---------- D. REVISTA ----------
  journal_meta <- get_meta_first(meta_df, c("citation_journal_title","dc.source","DC.Source"))
  journal_html <- if (!suspicious) html_text2(html_node(doc, "span.journal-title, p#journalTitle, div.journal")) else NA_character_
  journal_ld   <- if (!is.null(ld) && !is.null(ld$isPartOf$name)) ld$isPartOf$name else NA_character_
  base_row$journal <- dplyr::coalesce(journal_meta, journal_ld, journal_html)
  
  # ---------- E. FECHA / AÑO ----------
  date_meta <- get_meta_first(meta_df, c("citation_publication_date","citation_date","dc.date","DC.Date","prism.publicationDate","citation_year"))
  date_ld <- if (!is.null(ld) && !is.null(ld$datePublished)) ld$datePublished else NA_character_
  base_row$fecha_publicacion <- dplyr::coalesce(date_meta, date_ld)
  base_row$year <- extract_year_from_text(base_row$fecha_publicacion)
  if (is.na(base_row$year) || base_row$year == "") {
    base_row$year <- extract_year_from_url(base_row$url_final)
  }
  
  # ---------- F. VOLUMEN / ISSUE / PÁGINAS ----------
  base_row$volume      <- dplyr::coalesce(
    get_meta_first(meta_df, c("citation_volume","prism.volume")),
    if (!is.null(ld) && !is.null(ld$isPartOf$volumeNumber)) as.character(ld$isPartOf$volumeNumber) else NA_character_
  )
  base_row$issue       <- dplyr::coalesce(
    get_meta_first(meta_df, c("citation_issue","prism.number")),
    if (!is.null(ld) && !is.null(ld$isPartOf$issueNumber)) as.character(ld$isPartOf$issueNumber) else NA_character_
  )
  base_row$first_page <- get_meta_first(meta_df, c("citation_firstpage"))
  base_row$last_page  <- get_meta_first(meta_df, c("citation_lastpage"))
  
  # ---------- G. DOI ----------
  doi_meta  <- get_meta_first(meta_df, c("citation_doi", "dc.identifier", "DC.Identifier", "prism.doi"))
  doi_ld    <- if (!is.null(ld) && !is.null(ld$identifier)) ld$identifier else NA_character_
  doi_link  <- NA_character_
  if (!suspicious) {
    doi_a <- html_node(doc, "a[href*='doi.org']")
    if (!is.null(doi_a)) {
      href_doi <- html_attr(doi_a, "href")
      doi_link <- extract_doi_from_text(href_doi)
      if (is.na(doi_link)) doi_link <- href_doi
    }
  }
  doi_regex <- extract_doi_from_text(html_txt)
  base_row$doi <- dplyr::coalesce(doi_meta, doi_ld, doi_link, doi_regex)
  
  # ---------- H. IDIOMA ----------
  base_row$idioma <- get_meta_first(meta_df, c("dc.language","DC.Language","citation_language"))
  
  # ---------- I. ABSTRACT (Lógica Original Completa) ----------
  # Esta es la parte crítica que mantenemos idéntica al original
  abs_meta <- get_meta_first(meta_df, c("citation_abstract"))
  
  if (!is.na(abs_meta)) {
    base_row$abstract_completo <- abs_meta
    
  } else if (!is.null(ld) && !is.null(ld$description)) {
    base_row$abstract_completo <- ld$description
    
  } else if (!suspicious) {
    # 1) intentos directos
    abs_node <- html_node(doc,"div.abstract, section.abstract, div#abstract, div#resumen, div#resumo, section#abstract")
    
    if (is.null(abs_node)) {
      # 2) Búsqueda agresiva en secciones por título
      cand_sections <- html_nodes(doc, "div.articleSection, section, div.section")
      if (length(cand_sections) > 0) {
        found_txt <- NA_character_
        for (sec in cand_sections) {
          heading <- html_text2(html_node(sec, "h2, h3, strong, p.subtitle"))
          if (!is.na(heading) && heading != "") {
            heading_low <- tolower(heading)
            if (grepl("resumen", heading_low) || grepl("abstract", heading_low) || grepl("resumo",  heading_low)) {
              found_txt <- html_text2(sec)
              break
            }
          }
        }
        if (!is.na(found_txt)) {
          base_row$abstract_completo <- found_txt
        }
      }
    } else {
      base_row$abstract_completo <- html_text2(abs_node)
    }
    
    # 3) último intento: párrafos dentro de divs conocidos
    if (is.na(base_row$abstract_completo) || base_row$abstract_completo == "") {
      base_row$abstract_completo <- html_text2(html_node(doc, "div#resumen p, div#resumo p, div#abstract p"))
    }
  }
  
  # ---------- J. PALABRAS CLAVE ----------
  kw_meta <- get_meta_all(meta_df, c("citation_keywords"))
  if (length(kw_meta) > 0) {
    base_row$palabras_clave <- paste(kw_meta, collapse = "; ")
  } else if (!suspicious) {
    base_row$palabras_clave <- html_text2(html_node(doc, "div.keywords, section.keywords, p.keywords, div#keywords"))
  }
  
  # ---------- K. REFERENCIAS ----------
  if (!suspicious) {
    refs_nodes <- html_nodes(doc, "div.ref-list li, div#references li, ol.references li, ul.references li")
    if (length(refs_nodes) > 0) {
      base_row$referencias <- paste(html_text2(refs_nodes), collapse = " || ")
    }
  }
  
  # ---------- L. PDF ----------
  pdf_meta <- get_meta_first(meta_df, c("citation_pdf_url"))
  if (!is.na(pdf_meta)) {
    base_row$link_pdf <- pdf_meta
  } else if (!suspicious) {
    pdf_node <- html_node(doc, "a.pdf, a[href$='.pdf'], a[href*='pdf']")
    if (!is.null(pdf_node)) {
      base_row$link_pdf <- make_absolute(html_attr(pdf_node, "href"), resp$url)
    }
  }
  
  base_row
}

############################################################
# Construir query
############################################################

build_query <- function(terms, field_mode = "subject") {
  quoted <- paste0('"', terms, '"')
  ors <- paste(quoted, collapse = " OR ")
  
  if (field_mode == "subject")           return(paste0("subject:(", ors, ")"))
  if (field_mode == "abstract")          return(paste0("ab:(", ors, ")"))
  if (field_mode == "title_or_abstract") return(paste0("(ti:(", ors, ") OR ab:(", ors, "))"))
  ors
}

############################################################
# Función de búsqueda
############################################################

scrap_scielo_labor <- function(
    terms,
    field_mode = "subject",
    max_pages = 2,
    max_articles = 20,
    results_per_page = 50,
    sleep_between_pages = 1,
    sleep_between_articles = 2,
    apply_term_filter = FALSE
) {
  
  base_url <- "https://search.scielo.org/"
  
  headers_completos <- c(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/118.0 Safari/537.36",
    `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8",
    `Accept-Language` = "es-ES,es;q=0.9,en;q=0.8,pt;q=0.7"
  )
  
  q_text <- build_query(terms, field_mode)
  message("Buscando en SciELO con la consulta (", field_mode, "): ", q_text)
  
  todas_paginas <- list()
  links_vistos  <- character(0)
  facetas_guardadas <- NULL
  pagina <- 1
  
  repeat {
    message("=== Página ", pagina, " ===")
    resp <- tryCatch({
      GET(url = base_url,
          query = list(q = q_text, lang = "es", count = results_per_page, page = pagina),
          add_headers(.headers = headers_completos),
          timeout(12))
    }, error = function(e) NULL)
    
    if (is.null(resp) || resp$status_code != 200) break
    
    html_txt <- content(resp, as = "text", encoding = "UTF-8")
    doc <- read_html(html_txt)
    
    if (pagina == 1) {
      tot <- leer_total_hits(html_txt)
      if (!is.na(tot)) {
        message("→ SciELO indica ~", tot, " resultados.")
        max_articles <- min(max_articles, tot)
      }
      facetas_guardadas <- extraer_facetas(doc)
    }
    
    nodos <- html_nodes(doc, "div.item")
    if (length(nodos) == 0) break
    
    df_page <- purrr::map_df(nodos, ~{
      tibble(
        titulo_snippet  = html_text2(html_node(.x, "h1, h2, strong, .item-title")),
        revista_snippet = html_text2(html_node(.x, "div.source, div.journal, p.journal")),
        fecha_snippet   = html_text2(html_node(.x, "div.date, span.date")),
        link_articulo   = {
          links <- html_nodes(.x, "a")
          if (length(links) == 0) {
            NA_character_
          } else {
            found <- NA_character_
            for (i in seq_along(links)) {
              txt  <- html_text2(links[[i]])
              href <- html_attr(links[[i]], "href")
              if (!is.null(href) && (
                grepl("sci_arttext", href, fixed = TRUE) ||
                grepl("Texto", txt, ignore.case = TRUE) ||
                grepl("Text",  txt, ignore.case = TRUE)
              )) {
                found <- href
                break
              }
            }
            found
          }
        }
      )
    })
    
    df_page$link_articulo <- arreglar_url(df_page$link_articulo)
    nuevos <- setdiff(df_page$link_articulo[!is.na(df_page$link_articulo)], links_vistos)
    todas_paginas[[length(todas_paginas) + 1]] <- df_page
    links_vistos <- c(links_vistos, nuevos)
    
    if (length(links_vistos) >= max_articles) {
      message("Se alcanzó max_articles = ", max_articles, ". Fin.")
      break
    }
    
    pagina <- pagina + 1
    if (pagina > max_pages) break
    Sys.sleep(sleep_between_pages)
  }
  
  if (length(todas_paginas) == 0) {
    return(list(articulos = tibble(), facetas = facetas_guardadas))
  }
  
  links_all <- bind_rows(todas_paginas) %>%
    filter(!is.na(link_articulo)) %>%
    distinct(link_articulo, .keep_all = TRUE) %>%
    slice_head(n = max_articles)
  
  message("Total de artículos únicos encontrados con link: ", nrow(links_all))
  
  # Descargar detalles
  papers <- purrr::map(
    links_all$link_articulo,
    ~ obtener_datos_paper(.x, headers_completos, sleep_article = sleep_between_articles)
  ) %>% bind_rows()
  
  # Unir resultados
  articulos <- papers %>%
    bind_cols(links_all %>% select(titulo_snippet, revista_snippet, fecha_snippet)) %>%
    mutate(
      titulo_completo = dplyr::case_when(
        is.na(titulo_completo) | titulo_completo == "" ~ titulo_snippet,
        grepl("(?i)hold tight", titulo_completo) ~ titulo_snippet,
        grepl("(?i)slot777",    titulo_completo) ~ titulo_snippet,
        TRUE ~ titulo_completo
      ),
      journal = ifelse(is.na(journal) | journal == "", revista_snippet, journal),
      year    = ifelse(is.na(year) | year == "",
                       dplyr::coalesce(
                         extract_year_from_text(fecha_snippet),
                         extract_year_from_url(url_final)
                       ),
                       year)
    )
  
  # Filtro opcional post-procesamiento (del código original)
  if (apply_term_filter) {
    # Nota: Aquí se implementaría el filtro adicional si fuera necesario, 
    # pero para research note suele ser mejor filtrar después en el CSV.
    message("Filtro secundario desactivado en esta limpieza para conservar datos.")
  }
  
  list(articulos = articulos, facetas = facetas_guardadas)
}

############################################################
# Ejecutar con inputs
############################################################

mis_terminos <- c(
  # --- Inglés ---
  "field experiment", "survey experiment", "audit study",
  "correspondence experiment","correspondence audit", "conjoint",
  "list experiment", "factorial experiment","factorial survey",
  "vignette experiment", "vignette survey",
  
  # --- Español ---
  "experimento de campo", "encuesta experimental",
  "estudio de correspondencia", "experimento de correspondencia", "análisis de conjunto",
  "experimento de lista", "experimento factorial", "encuesta factorial",
  "experimento de viñeta","encuesta de viñeta",
  
  # --- Portugués ---
  "experimento de campo", "inquérito experimental", 
  "estudo de correspondência", "experimento de correspondência",
  "análise conjunta", "experimento de lista", 
  "experimento fatorial", "inquérito fatorial",
  "experimento de vinheta","inquérito de vinheta"
)

# EJECUTAR
res <- scrap_scielo_labor(
  terms = mis_terminos,
  field_mode = "subject", 
  max_pages = 41,          
  max_articles = 2100,      
  results_per_page = 50,
  sleep_between_pages = runif(1, 8, 15), # Pausa aleatoria
  sleep_between_articles = runif(1, 3, 6),
  apply_term_filter = FALSE
)

# GUARDAR
nombre_archivo <- paste0("scielo_survey_experiments_", format(Sys.Date(), "%Y%m%d"), ".csv")
write_csv(res$articulos, nombre_archivo)
message("Guardado en: ", nombre_archivo)
