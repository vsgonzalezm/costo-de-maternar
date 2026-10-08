###############################################################################
# PROYECTO: Revisión Bibliográfica - Survey Experiments
# AUTORA:   Valentina Gonzalez
# FECHA:    Noviembre/Diciembre 2025
#
# 1. Estrategia: Surface Scraping.
# 2. Motor: Búsqueda Booleana en metadatos ('subject:') .
# 3. Cobertura: Revisa página por página hasta que el servidor indica "0 resultados".
# 4. Seguridad: Incluye pausas aleatorias para respetar el servidor de SciELO.
###############################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(httr)
  library(rvest)
  library(stringr)
})

# =============================================================================
# 1. PARSEADORES (LIMPIEZA DE DATOS CRUDOS)
# =============================================================================
# SciELO entrega la información de la revista en una sola línea de texto sucia.
# Esta funciónmordenar los datos.
parse_citation_line <- function(txt) {
  if (is.na(txt) || txt == "") return(list(journal=NA, year=NA, vol=NA, issue=NA))
  
  # Limpieza del nombre de la revista (borrando palabras técnicas de la web)
  journal_match <- str_split(txt, "Métricas|SciELO|Sobre o|20[0-2][0-9]")[[1]][1]
  journal_match <- str_remove_all(journal_match, ",") %>% str_trim()
  
  # Extracción de Año (4 dígitos)
  year_match <- str_extract(txt, "20[0-2][0-9]|19[9][0-9]")
  
  # Extracción de Volumen (busca "vol." o "v." seguido de números)
  vol_match  <- str_extract(txt, "(?i)(vol\\.|volumen|v\\.|Vol\\.)\\s*([0-9]+)") %>% 
    str_remove_all("(?i)(vol\\.|volumen|v\\.|Vol\\.)\\s*")
  
  # Extracción de Número/Issue
  issue_match <- str_extract(txt, "(?i)(no\\.|número|num\\.|n\\.|Número)\\s*([0-9]+)") %>% 
    str_remove_all("(?i)(no\\.|número|num\\.|n\\.|Número)\\s*")
  
  list(journal = journal_match, year = year_match, vol = vol_match, issue = issue_match)
}

# =============================================================================
# 2. MOTOR DE BÚSQUEDA
# =============================================================================
scrap_scielo_infinito <- function(terms) {
  
  # --- CONFIGURACIÓN DE SEGURIDAD ---
  
  MAX_PAGES_SAFETY <- 2000   # se puede adaptar 
  
  base_url <- "https://search.scielo.org/"
  
  # Construcción de la consulta Booleana (OR)
  quoted <- paste0('"', terms, '"')
  ors <- paste(quoted, collapse = " OR ")
  
  
  # Usamos 'subject:' en lugar de búsqueda libre. 
  # Esto obliga a SciELO a buscar en los índices temáticos 
  q_text <- paste0("subject:(", ors, ")")
  
  # Headers para simular un navegador real y evitar bloqueos
  headers_ok <- c(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/118.0 Safari/537.36",
    `Referer` = "https://search.scielo.org/"
  )
  
  message(">>> INICIANDO BÚSQUEDA EXHAUSTIVA")
  message(">>> Estrategia: Búsqueda en 'subject' (Metadatos controlados)")
  message(">>> Criterio de parada: El script revisará TODAS las páginas hasta que no queden resultados.")
  
  all_data <- list()
  
  # Bucle principal: Recorre páginas 1, 2, 3... hasta el final.
  for (page_num in 1:MAX_PAGES_SAFETY) {
    message("\n--- Descargando Página ", page_num, " ---")
    
    # --- SISTEMA DE  REINTENTOS ---
    # Si el internet falla o SciELO se demora, reintentamos 3 veces.
    # por si hay error de conexión.
    resp <- NULL
    for(i in 1:3) {
      try({
        resp <- GET(
          url = base_url,
          query = list(q = q_text, lang = "es", count = 50, from = (page_num - 1) * 50 + 1, page = page_num),
          add_headers(.headers = headers_ok), timeout(45) # Timeout largo por seguridad
        )
      }, silent = TRUE)
      
      if(!is.null(resp) && status_code(resp) == 200) break
      message("... conexión lenta detectada, reintentando (intento ", i, ")...")
      Sys.sleep(3)
    }
    
    # Si después de 3 intentos falla, saltamos la página (pero no paramos todo)
    if (is.null(resp) || status_code(resp) != 200) {
      message("!!! Error crítico al cargar página ", page_num, ". Saltando este bloque.")
      next 
    }
    
    # Procesamiento del HTML recibido
    html <- content(resp, as = "text", encoding = "UTF-8")
    doc <- read_html(html)
    items <- html_nodes(doc, "div.item") # Cada 'item' es un paper en la lista
    
    # --- VALIDACIÓN  ---
    # Si SciELO devuelve 0 items, significa que SE ACABARON DE VERDAD.
    # Aquí es donde el script decide detenerse automáticamente.
    if (length(items) == 0) {
      message(">>> FIN DE LA BÚSQUEDA: SciELO indica que no hay más resultados.")
      message(">>> Se han revisado exitosamente ", page_num - 1, " páginas completas.")
      break
    }
    
    # Extracción de variables de cada paper encontrado
    df_page <- map_df(items, function(node) {
      titulo <- html_text2(html_node(node, ".title"))
      autores_raw <- html_text2(html_node(node, ".authors"))
      source_raw <- html_text2(html_node(node, ".line.source"))
      
      # Usamos nuestra función de limpieza para separar año y revista
      parsed <- parse_citation_line(source_raw)
      
      # Extraemos el LINK en lugar del texto del abstract (Estrategia Surface)
      url_paper <- html_attr(html_node(node, "a[href*='sci_arttext']"), "href")
      doi_clean <- str_extract(html_attr(html_node(node, "a[href*='doi.org']"), "href"), "10\\..+")
      
      tibble(
        Titulo = titulo,
        Autores = autores_raw,
        Revista = parsed$journal,
        Anio = parsed$year,
        Volumen = parsed$vol,
        Numero = parsed$issue,
        DOI = doi_clean,
        URL = url_paper
      )
    })
    
    all_data[[page_num]] <- df_page
    message("-> Papers encontrados en esta página: ", nrow(df_page))
    
    # --- PAUSA  ---
    # Esperamos entre 2 y 4 segundos antes de pedir la siguiente página.
    # Esto evita que SciELO nos bloquee por "comportamiento robótico".
    Sys.sleep(runif(1, 2, 4))
  }
  
  if(length(all_data) > 0) bind_rows(all_data) else NULL
}

# =============================================================================
# 3. EJECUCIÓN (INPUTS)
# =============================================================================
# Lista completa de términos en 3 idiomas para asegurar cobertura regional.
mis_terminos <- c(
  "survey experiment", "conjoint experiment", "list experiment", 
  "vignette experiment", "factorial survey", "audit study", "correspondence study",
  "experimento de encuesta", "encuesta experimental",
  "experimento de campo", "experimento de lista", 
  "experimento de viñeta", "encuesta de viñeta",
  "experimento de correspondencia", "estudio de correspondencia",
  "analisis conjunto", "experimento factorial",
  "inquérito experimental", "experimento fatorial", "analise conjunta",
  "experimento de vinheta", "experimento de lista"
)

# CORRER LA BÚSQUEDA
datos_raw <- scrap_scielo_infinito(mis_terminos)

# =============================================================================
# 4. FILTRADO FINAL Y GUARDADO
# =============================================================================
if (!is.null(datos_raw)) {
  
  datos_final <- datos_raw %>%
    mutate(Anio = as.numeric(Anio)) %>%
    # Filtro Temporal: Mantenemos solo lo que está entre 2000 y 2025.
    # Hacemos esto DESPUÉS de descargar para evitar errores en el filtro web.
    filter(!is.na(Anio) & Anio >= 2000 & Anio <= 2025) %>%
    distinct(Titulo, .keep_all = TRUE) # Eliminamos duplicados exactos
  
  message("\n=============================================")
  message("INFORME DE MISIÓN CUMPLIDA")
  message("Total Papers Recolectados (2000-2025): ", nrow(datos_final))
  message("=============================================")
  
  # Guardamos con el nombre solicitado
  nombre_archivo <- paste0("scielo_total_diciembre_", Sys.Date(), ".csv")
  write_csv(datos_final, nombre_archivo)
  message("Base de datos guardada en: ", nombre_archivo)
}
