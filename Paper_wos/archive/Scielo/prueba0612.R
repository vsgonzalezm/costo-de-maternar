###############################################################################
# PROYECTO: Revisión Bibliográfica - Survey Experiments
# SCRIPT:   scrap_scielo_V9_STEALTH.R
# AUTORA:   Valentina Gonzalez

###############################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(httr)
  library(rvest)
  library(stringr)
})

# =============================================================================
# 1. CONFIGURACIÓN DE NAVEGADOR (LO MÁS IMPORTANTE AHORA)
# =============================================================================

# Estos headers imitan exactamente a un humano navegando en Chrome
headers_stealth <- c(
  `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
  `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8",
  `Accept-Language` = "es-ES,es;q=0.9,en;q=0.8",
  `Connection` = "keep-alive",
  `Upgrade-Insecure-Requests` = "1",
  `Sec-Fetch-Dest` = "document",
  `Sec-Fetch-Mode` = "navigate",
  `Sec-Fetch-Site` = "same-origin",
  `Referer` = "https://search.scielo.org/"
)

# =============================================================================
# 2. FUNCIONES AUXILIARES
# =============================================================================

clean_txt <- function(x) str_trim(str_squish(x))

parse_citation_line <- function(txt) {
  if (is.na(txt) || txt == "") return(list(journal=NA, year=NA))
  journal_match <- str_split(txt, "Métricas|SciELO|Sobre o|20[0-2][0-9]|19[9][0-9]")[[1]][1]
  journal_match <- str_remove_all(journal_match, ",") %>% str_trim()
  year_match <- str_extract(txt, "20[0-2][0-9]|19[9][0-9]")
  list(journal = journal_match, year = year_match)
}

get_abstract_stealth <- function(url_paper) {
  if (is.na(url_paper) || url_paper == "") return(NA_character_)
  
  tryCatch({
    resp <- GET(
      url_paper, 
      add_headers(.headers = headers_stealth), 
      config(ssl_verifypeer = FALSE),
      timeout(15)
    )
    
    if (status_code(resp) != 200) return(NA_character_)
    
    doc <- read_html(content(resp, as="text", encoding="UTF-8"))
    
    # ESTRATEGIA 1: Metadato citation_abstract
    abs_meta <- html_attr(html_node(doc, "meta[name='citation_abstract']"), "content")
    if (! is.na(abs_meta) && abs_meta != "") return(abs_meta)
    
    # ESTRATEGIA 2: Metadato DC.Description
    abs_meta <- html_attr(html_node(doc, "meta[name='DC.Description']"), "content")
    if (!is.na(abs_meta) && abs_meta != "") return(abs_meta)
    
    # ESTRATEGIA 3: Buscar en el cuerpo HTML
    selectores <- c(
      "div.abstract", "section.abstract", "div#abstract", 
      "div#resumen", "div#resumo", "div.resumen", "div.resumo"
    )
    
    for (selector in selectores) {
      nodo <- html_node(doc, selector)
      if (! is.na(nodo)) {
        texto <- html_text2(nodo) %>% clean_txt()
        if (!is.na(texto) && nchar(texto) > 50) return(texto)
      }
    }
    
    # ESTRATEGIA 4: Buscar después de encabezados "Resumen"/"Abstract"
    encabezados <- html_nodes(doc, "h2, h3, h4, strong, b")
    for (h in encabezados) {
      texto_header <- clean_txt(html_text2(h))
      if (str_detect(texto_header, regex("^(resumen|abstract|resumo)$", ignore_case=TRUE))) {
        siguiente <- html_node(h, xpath="following-sibling::p[1]")
        if (!is.na(siguiente)) {
          texto <- html_text2(siguiente) %>% clean_txt()
          if (!is.na(texto) && nchar(texto) > 50) return(texto)
        }
      }
    }
    
    # Si nada funcionó, retornar NA
    return(NA_character_)
    
  }, error = function(e) NA_character_)
}
# =============================================================================
# 3. MOTOR DE BÚSQUEDA ROBUSTO
# =============================================================================

scrap_keyword_v9 <- function(keyword) {
  
  base_url <- "https://search.scielo.org/"
  # Codificamos la URL correctamente para evitar espacios raros
  q_encoded <- URLencode(paste0('subject:("', keyword, '")'))
  
  message(paste0("\n>>> INTENTANDO: '", keyword, "'"))
  
  all_data <- list()
  MAX_PAGES <- 30 
  
  for (page_num in 1:MAX_PAGES) {
    
    # Construcción manual de URL para control total
    url_target <- paste0(
      base_url, "?",
      "q=", q_encoded,
      "&lang=es&count=50",
      "&from=", (page_num - 1) * 50 + 1,
      "&page=", page_num
    )
    
    # BUCLE DE INTENTOS CON REPORTE DE ERROR REAL
    resp <- NULL
    err_msg <- ""
    
    for(i in 1:3) {
      tryCatch({
        resp <- GET(
          url_target, 
          add_headers(.headers = headers_stealth),
          config(ssl_verifypeer = FALSE), # CLAVE: Evita bloqueos por certificado
          timeout(30)
        )
      }, error = function(e) { err_msg <<- e$message })
      
      if(!is.null(resp) && status_code(resp) == 200) break
      
      # Si falló, esperamos más tiempo (Pausas exponenciales: 5s, 10s, 15s)
      wait_time <- i * 5
      message(paste0("   ...fallo conexión (intento ", i, "). Esperando ", wait_time, "s..."))
      Sys.sleep(wait_time)
    }
    
    # DIAGNÓSTICO FINAL SI FALLA
    if (is.null(resp)) {
      message(paste0("   [XXX] Error Fatal de Red: ", err_msg))
      break
    }
    if (status_code(resp) != 200) {
      message(paste0("   [XXX] Bloqueo del Servidor. Código HTTP: ", status_code(resp)))
      # Si es 403 o 429, es bloqueo de IP. Paramos todo.
      if (status_code(resp) %in% c(403, 429)) {
        stop("!!! TU IP HA SIDO BLOQUEADA TEMPORALMENTE POR SCIELO. CAMBIA DE RED O ESPERA 1 HORA.")
      }
      break
    }
    
    # Procesar si hubo éxito
    doc <- read_html(content(resp, as="text", encoding="UTF-8"))
    items <- html_nodes(doc, "div.item")
    
    if (length(items) == 0) {
      if(page_num == 1) message("   -> Sin resultados (0 papers).")
      else message("   -> Fin de paginación.")
      break
    }
    
    message(paste0("   -> Éxito Pág ", page_num, ": ", length(items), " papers. Descargando abstracts..."))
    
    df_page <- map_df(items, function(node) {
      titulo <- clean_txt(html_text2(html_node(node, ".title")))
      autores <- clean_txt(html_text2(html_node(node, ".authors")))
      parsed <- parse_citation_line(html_text2(html_node(node, ".line.source")))
      
      link_node <- html_node(node, "a[href*='sci_arttext']")
      url_paper <- html_attr(link_node, "href")
      doi_clean <- str_extract(html_attr(html_node(node, "a[href*='doi.org']"), "href"), "10\\..+")
      
      # Abstract
      abs_text <- get_abstract_stealth(url_paper)
      
      tibble(
        Keyword = keyword,
        Titulo = titulo,
        Autores = autores,
        Revista = parsed$journal,
        Anio = parsed$year,
        Abstract = abs_text,
        URL = url_paper
      )
    })
    
    all_data[[page_num]] <- df_page
    
    # PAUSA ALEATORIA LARGA ENTRE PÁGINAS (Seguridad)
    Sys.sleep(runif(1, 4, 8)) 
  }
  
  if(length(all_data) > 0) bind_rows(all_data) else NULL
}

# =============================================================================
# 4. EJECUCIÓN
# =============================================================================

mis_terminos <- c(
  "survey experiment", "conjoint experiment", "list experiment", 
  "vignette experiment", "factorial survey", "audit study", 
  "experimento de encuesta", "encuesta experimental",
  "experimento de campo", "experimento de lista", 
  "experimento de viñeta", "encuesta de viñeta",
  "experimento de correspondencia", "estudio de correspondencia",
  "inquérito experimental", "experimento fatorial", "analise conjunta"
)

# LISTA NEGRA LOCAL (Filtrado en R, no en URL)
revistas_ruido <- c("Agronomía", "Agrícola", "Agropecuária", "Zootecnia", "Biology", 
                    "Biología", "Forestry", "Floresta", "Soil", "Suelo", "Solo", 
                    "Horticultura", "Veterinaria", "Seed", "Planta", "Crop", "Weed")

resultados_totales <- list()

for (k in mis_terminos) {
  df_k <- scrap_keyword_v9(k)
  if (!is.null(df_k)) {
    resultados_totales[[k]] <- df_k
  }
  # PAUSA ENTRE KEYWORDS
  message("...Enfriando conexión (esperando 5s)...")
  Sys.sleep(5)
}

# =============================================================================
# 5. GUARDADO
# =============================================================================

if (length(resultados_totales) > 0) {
  df_final <- bind_rows(resultados_totales) %>%
    mutate(Anio = as.numeric(Anio)) %>%
    filter(!is.na(Anio) & Anio >= 2000 & Anio <= 2025) %>%
    distinct(Titulo, .keep_all = TRUE) %>%
    # Filtro de áreas local
    filter(!str_detect(Revista, regex(paste(revistas_ruido, collapse = "|"), ignore_case = TRUE)))
  
  message(paste0("\n>>> ÉXITO: Se guardaron ", nrow(df_final), " papers."))
  write_csv(df_final, paste0("scielo_V9_STEALTH_", Sys.Date(), ".csv"))
} else {
  message(">>> No se pudieron descargar datos. Revisa si tu IP sigue bloqueada.")
}
