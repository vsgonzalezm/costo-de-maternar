###############################################################################
# PROYECTO:Survey Experiments en Latinoamérica
# AUTORA:   Valentina Gonzalez
# OBJETIVO: Web scraping  SciELO América Latina (1990-2025)
###############################################################################

library(tidyverse)
library(httr)
library(rvest)
library(stringr)

# =============================================================================
#  CONFIGURACIÓN DE NAVEGADOR Y PARÁMETROS DE BÚSQUEDA
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
# FILTROS TEMÁTICOS CONFIGURABLES
# =============================================================================
SCIELO_AREAS_TEMATICAS <- c(
  "Human Sciences",           # Ciencias Humanas
  "Applied Social Sciences"   # Ciencias Sociales Aplicadas
)

# =============================================================================
# PARÁMETROS DE VALIDACIÓN
# =============================================================================
# Longitud mínima aceptable para un abstract válido (en caracteres) porque abstract académico típico tiene al menos 100 caracteres
# =============================================================================
MIN_ABSTRACT_LENGTH <- 100

# =============================================================================
#  FUNCIONES AUXILIARES DE PROCESAMIENTO DE TEXTO
# =============================================================================

# -----------------------------------------------------------------------------
# Función: clean_txt()
# para limpiar espacios extras, saltos de línea y caracteres especiales
# -----------------------------------------------------------------------------
clean_txt <- function(x) str_trim(str_squish(x))


# -----------------------------------------------------------------------------
# Función: parse_citation_line()
# Extrae nombre de revista y año de publicación de la línea de citación
# -----------------------------------------------------------------------------
parse_citation_line <- function(txt) {
  if (is.na(txt) || txt == "") return(list(journal=NA, year=NA))    # si no hay texto devuelve na 
  
  # Separar por palabras clave que indican fin del nombre de revista
  journal_match <- str_split(txt, "Métricas|SciELO|Sobre o|20[0-2][0-9]|19[9][0-9]")[[1]][1]   #Quédate con el primer trozo (que suele ser la revista)
  journal_match <- str_remove_all(journal_match, ",") %>% str_trim()   #Quitar comas y espacios
   
  # Extraer año 
  year_match <- str_extract(txt, "20[0-2][0-9]|19[9][0-9]") #buscar cualquier año entre 2000 y 2029 
  
  list(journal = journal_match, year = year_match)  # devolver lista con journal y year
}

# -----------------------------------------------------------------------------
# Función auxiliar: toggle_plural()
# -----------------------------------------------------------------------------
#toggle_plural <- function(term) {
#  if (grepl("s$", term)) {
#    # Si termina en 's', remover la 's' (plural -> singular)
#    gsub("s$", "", term)
#  } else {
#    # Si no termina en 's', agregar 's' (singular -> plural)
#    paste0(term, "s")
#  }
#}

# -----------------------------------------------------------------------------
# Función: expand_keywords()
# Genera variantes automáticas de keywords (singular/plural, guiones/espacios)
# -----------------------------------------------------------------------------
#expand_keywords <- function(keyword) {
#  variants <- c(keyword)  # Siempre incluir el original
#  
#  # Generar variante singular/plural del original
#  variants <- c(variants, toggle_plural(keyword))
#  
#  # Generar variante con guión si tiene espacios
#  if (grepl(" ", keyword)) {
#    with_hyphen <- gsub(" ", "-", keyword)
#    variants <- c(variants, with_hyphen)
#    # También crear versión plural/singular de la variante con guión
#    variants <- c(variants, toggle_plural(with_hyphen))
#  }
#  
#  # Generar variante con espacio si tiene guiones
#  if (grepl("-", keyword)) {
    with_space <- gsub("-", " ", keyword)
    variants <- c(variants, with_space)
    # También crear versión plural/singular de la variante con espacio
    variants <- c(variants, toggle_plural(with_space))
#  }
#  
#  # Eliminar duplicados y retornar
#  return(unique(variants))
#}

# =============================================================================
#  Extracción de Abstract con Estrategia de Cascada
# =============================================================================
# Esta función implementa un sistema de "fallback" en 3 pasos para maximizar

# PASO 1: Buscar en meta tag "citation_abstract" (estándar académico)
# PASO 2: Si falla, buscar en meta tag "DC.Description" (Dublin Core)
# PASO 3: Si falla, buscar en el HTML visible usando múltiples selectores
#         comunes para abstracts en español/portugués
# =============================================================================

get_abstract_stealth <- function(url_paper) {
  if (is.na(url_paper) || url_paper == "") return(NA_character_)
  
  tryCatch({
    # Petición HTTP con headers stealth para evitar bloqueos
    resp <- GET(
      url_paper, 
      add_headers(.headers = headers_stealth), 
      config(ssl_verifypeer = FALSE), # Ignorar errores SSL
      timeout(15)
    )
    
    if (status_code(resp) != 200) return(NA_character_)
    
    doc <- read_html(content(resp, as="text", encoding="UTF-8"))
    
    # --------------------------------------------------
    # PASO 1: Intentar extraer de meta tag citation_abstract
    # --------------------------------------------------
    meta_node <- html_node(doc, "meta[name='citation_abstract']")
    abs_meta <- if (!is.null(meta_node)) html_attr(meta_node, "content") else NA_character_
    
    # --------------------------------------------------
    # PASO 2: Si falla, intentar meta tag DC.Description 
    # --------------------------------------------------
    if (is.na(abs_meta)) {
      meta_node <- html_node(doc, "meta[name='DC.Description']")
      abs_meta <- if (!is.null(meta_node)) html_attr(meta_node, "content") else NA_character_
    }
    
    # --------------------------------------------------
    # PASO 3: Si aún no, buscar en HTML visible
    # --------------------------------------------------

    if (is.na(abs_meta)) {
      # Lista de selectores CSS comunes para abstracts en SciELO
      # Cubre español, portugués e inglés
      selectores_abstract <- c(
        "div.abstract",              # Selector genérico común
        "div.resumo",                # Portugués
        "div.resumen",               # Español
        "section#abstract",          # HTML5 semántico
        "section#resumen",           # Español semántico
        "section#resumo",            # Portugués semántico
        "div[class*='abstract']",    # Cualquier clase que contenga "abstract"
        "div[class*='resumen']",     # Cualquier clase que contenga "resumen"
        "div[class*='resumo']",      # Cualquier clase que contenga "resumo"
        "div.trans-abstract",        # Abstracts traducidos
        "p.sec",                     # Formato alternativo de SciELO
        ".article-abstract",         # Clase específica de artículos
        "#articleText .abstract"     # Abstract dentro del texto del artículo
      )
      
      for (selector in selectores_abstract) {
        nodo_abstract <- html_node(doc, selector)
        # html_node() retorna NULL (no NA) cuando no encuentra el elemento
        if (!is.null(nodo_abstract)) {
          texto_extraido <- html_text2(nodo_abstract)
          # Validación completa: no NULL, no NA, es character, y longitud >= MIN_ABSTRACT_LENGTH
          if (!is.null(texto_extraido) && 
              !is.na(texto_extraido) && 
              is.character(texto_extraido) && 
              length(texto_extraido) == 1 &&
              nchar(texto_extraido, allowNA = TRUE) > MIN_ABSTRACT_LENGTH) {
            abs_meta <- texto_extraido
            break  # Salimos del loop una vez encontrado
          }
        }
      }
    }
    
    # Limpiar el texto final
    if (!is.na(abs_meta)) {
      abs_meta <- clean_txt(abs_meta)
    }
    
    return(abs_meta)
  }, error = function(e) NA_character_)
}

# =============================================================================
#MOTOR DE BÚSQUEDA CON FILTROS TEMÁTICOS
# =============================================================================

scrap_keyword_v9 <- function(keyword, use_variants = TRUE) {
  base_url <- "https://search.scielo.org/"
  
  # =============================================================================
  # EXPANSIÓN DE KEYWORDS: Buscar variantes singular/plural  (esto ya no lo uso)
  # =============================================================================
  # Genera variantes del keyword para maximizar recuperación
  # Ejemplo: "survey experiment" -> ["survey experiment", "survey experiments"]
  # =============================================================================
  
  keywords_to_search <- if (use_variants) expand_keywords(keyword) else c(keyword)
  message(paste0("\n>>> PROCESANDO: '", keyword, "'"))
  message(paste0("    Variantes a buscar: ", paste(keywords_to_search, collapse=", ")))
  
  all_data_variants <- list()
  
  # Iterar sobre cada variante del keyword
  for (variant in keywords_to_search) {
    
    message(paste0("\n>>> BUSCANDO VARIANTE: '", variant, "'"))
    
    
    
    
### Cambiar por esto porque no usare las variantes: 

#    scrap_keyword_v9 <- function(keyword) {
#     base_url <- "https://search.scielo.org/"   
#    keywords_to_search <- c(keyword)
    
# message(paste0("\n>>> PROCESANDO TÉRMINO EXACTO: '", keyword, "'"))
    
#    all_data_variants <- list()
    
    # Mantenemos este bucle "for" para no tener que borrar ni mover
    # todo el código que viene abajo. Simplemente correrá 1 sola vez.
   # for (variant in keywords_to_search) { }
      
      
      
    # =============================================================================
    # FILTROS TEMÁTICOS: Estructura correcta según URLs reales de SciELO
    # =============================================================================
    
    all_data <- list()    # da los resultados de a 50 en scielo por eso necesita esto. 
    MAX_PAGES <- 30 
    
    for (page_num in 1:MAX_PAGES) {
      
      # =============================================================================
      # CONSTRUCCIÓN DE URL CORRECTA (según URLs reales de SciELO)
      # =============================================================================
      
      # Keyword con comillas para búsqueda exacta
      q_value <- paste0('"', variant, '"')
      
      # Construcción manual de URL con TODOS los parámetros requeridos
      # NOTA: filter[subject_area][] debe aparecer UNA VEZ POR CADA ÁREA
      url_target <- paste0(
        base_url, "?",
        "q=", URLencode(q_value),    
        "&lang=pt",  # Portugués para mejor cobertura de SciELO
        "&count=50",  # 50 resultados por página (máximo permitido)
        "&from=", (page_num - 1) * 50 + 1, 
        "&output=site",
        "&sort=",  # Vacío pero requerido
        "&format=summary",
        "&fb=",  # Vacío pero requerido
        "&page=", page_num,
        "&filter%5Bsubject_area%5D%5B%5D=", URLencode("Human Sciences"),    # filtrar por area 
        "&filter%5Bsubject_area%5D%5B%5D=", URLencode("Applied Social Sciences")
      )
      
      # BUCLE DE INTENTOS: por si se me cae el internet o algo similar
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
        
        # Si fallo, esperamos más tiempo 
        wait_time <- i * 5
        message(paste0("   ...fallo conexión (intento ", i, "). ", wait_time, "s..."))
        Sys.sleep(wait_time)
      }
      
      # DIAGNÓSTICO FINAL SI FALLA
      if (is.null(resp)) {
        message(paste0("   [XXX] Error: ", err_msg))
        break
      }
      if (status_code(resp) != 200) {
        message(paste0("   [XXX] Bloqueo . Código HTTP: ", status_code(resp)))
  
        if (status_code(resp) %in% c(403, 429)) {
          stop("!!! se bloqueo ")
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
      
      # =============================================================================
      # EXTRACCIÓN DE METADATOS POR CADA PAPER
      # =============================================================================
      # 1. Metadatos básicos (título, autores, revista, año)
      # 2. URLs y DOI
      # 3. Abstract completo
      # =============================================================================
      
      df_page <- map_df(items, function(node) {    
        # AQUÍ EXTRAEMOS LOS METADATOS BÁSICOS DEL HTML DE RESULTADOS
        title_node <- html_node(node, ".title")
        titulo <- if (!is.null(title_node)) clean_txt(html_text2(title_node)) else NA_character_
        
        authors_node <- html_node(node, ".authors")
        autores <- if (!is.null(authors_node)) clean_txt(html_text2(authors_node)) else NA_character_
        
        source_node <- html_node(node, ".line.source")
        source_text <- if (!is.null(source_node)) html_text2(source_node) else ""
        parsed <- parse_citation_line(source_text)  #usar la función construida 
        
        # AQUÍ OBTENEMOS LA URL DEL PAPER Y EL DOI
        link_node <- html_node(node, "a[href*='sci_arttext']")
        url_paper <- if (!is.null(link_node)) html_attr(link_node, "href") else NA_character_
        
        doi_node <- html_node(node, "a[href*='doi.org']")
        doi_href <- if (!is.null(doi_node)) html_attr(doi_node, "href") else NA_character_
        doi_clean <- if (!is.na(doi_href)) str_extract(doi_href, "10\\..+") else NA_character_
        
        # AQUÍ DESCARGAMOS EL ABSTRACT 
        abs_text <- get_abstract_stealth(url_paper)
        
        # =============================================================================
        # ESTRUCTURA DE DATOS: Trazabilidad 1 a 1 por Keyword
        # =============================================================================
        # Keyword_Origen: Identifica la palabra clave que encontró cada paper
        # Variant_Used: Variante específica que encontró el paper (para análisis)
        # DOI: Identificador único del documento (si está disponible)
        # =============================================================================
        tibble(
          Keyword_Origen = keyword,
          Variant_Used = variant,
          Titulo = titulo,
          Autores = autores,
          Revista = parsed$journal,
          Anio = parsed$year,
          Abstract = abs_text,
          URL = url_paper,
          DOI = doi_clean
        )
      })
      
      all_data[[page_num]] <- df_page
      
      # PAUSA ALEATORIA LARGA ENTRE PÁGINAS
      Sys.sleep(runif(1, 4, 8)) 
    }
    
    # Consolidar datos de esta variante
    if(length(all_data) > 0) {
      all_data_variants[[variant]] <- bind_rows(all_data)
    }
    
    # PAUSA ENTRE VARIANTES DEL MISMO KEYWORD
    if (variant != keywords_to_search[length(keywords_to_search)]) {
      message("   ...esperar...")
      Sys.sleep(3)
    }
  }
  
  # =============================================================================
  # CONSOLIDACIÓN Y DEDUPLICACIÓN DE RESULTADOS
  # =============================================================================
  # Combinar resultados de todas las variantes y eliminar duplicados por título
  # =============================================================================
  
  
  # editar ya que no usaremos los plurales 
  if(length(all_data_variants) > 0) {
    combined <- bind_rows(all_data_variants)
    
    # Deduplicar por título manteniendo la primera ocurrencia
    # (que corresponde a la primera variante que encontró el paper)
    deduplicated <- combined %>%
      distinct(Titulo, .keep_all = TRUE)
    
    n_total <- nrow(combined)
    n_unique <- nrow(deduplicated)
    n_duplicated <- n_total - n_unique
    
    if (n_duplicated > 0) {
      message(paste0("\n>>> DEDUPLICACIÓN: ", n_duplicated, " papers duplicados removidos"))
      message(paste0("    Total único: ", n_unique, " papers"))
    }
    
    return(deduplicated)
  } else {
    return(NULL)
  }
}

# =============================================================================
# EJECUCIÓN PRINCIPAL
# =============================================================================

# =============================================================================
# LISTA DE TÉRMINOS DE BÚSQUEDA (Keywords)
# =============================================================================
# Se mantiene estructura keyword por keyword para TRAZABILIDAD COMPLETA

mis_terminos <- c(
  # INGLÉS - SINGULARES
  "field experiment", "survey experiment", "audit study",
  "correspondence experiment", "correspondence audit", "conjoint",
  "list experiment", "factorial experiment", "factorial survey",
  "vignette experiment", "vignette survey",
  
  # INGLÉS - PLURALES
  "field experiments", "survey experiments", "audit studies", 
  "correspondence experiments", "correspondence audits", "conjoints",
  "list experiments", "factorial experiments", "factorial surveys",
  "vignette experiments", "vignette surveys",
  
  # INGLÉS - CON GUIÓN SINGULARES
  "field-experiment", "survey-experiment", "audit-study",
  "correspondence-experiment", "correspondence-audit", 
  "list-experiment", "factorial-experiment", "factorial-survey",
  "vignette-experiment", "vignette-survey",
  
  # INGLÉS - CON GUIÓN PLURALES
  "field-experiments", "survey-experiments", "audit-studies",
  "correspondence-experiments", "correspondence-audits",
  "list-experiments", "factorial-experiments", "factorial-surveys", 
  "vignette-experiments", "vignette-surveys",
  
  # ESPAÑOL
  "experimento de campo", "encuesta experimental", "estudio de correspondencia", 
  "experimento de correspondencia", "análisis de conjunto", "experimento de lista", 
  "experimento factorial", "encuesta factorial", "experimento de viñeta", "encuesta de viñeta",
  "experimentos de campo", "encuestas experimentales", "estudios de correspondencia",
  "experimentos de correspondencia", "análisis de conjuntos", "experimentos de lista",
  "experimentos factoriales", "encuestas factoriales", "experimentos de viñeta", "encuestas de viñeta",
  
  # PORTUGUÉS
  "experimento de campo", "inquérito experimental", "estudo de correspondência",
  "experimento de correspondência", "análise conjunta", "experimento de lista",
  "experimento fatorial", "inquérito fatorial", "experimento de vinheta", "inquérito de vinheta",
  "experimentos de campo", "inquéritos experimentais", "estudos de correspondência", 
  "experimentos de correspondência", "análises conjuntas", "experimentos de lista",
  "experimentos fatoriais", "inquéritos fatoriais", "experimentos de vinheta", "inquéritos de vinheta"
)


# =============================================================================
# filtro secundario para eliminar revistas claramente irrelevantes

# =============================================================================

revistas_ruido <- c("Agronomía", "Agrícola", "Agropecuária", "Zootecnia", "Biology", 
                    "Biología", "Forestry", "Floresta", "Soil", "Suelo", "Solo", 
                    "Horticultura", "Veterinaria", "Seed", "Planta", "Crop", "Weed")

# =============================================================================
# BUCLE PRINCIPAL: Iteración Keyword por Keyword
# =============================================================================
# Cada paper quedará marcado con su Keyword_Origen
# =============================================================================

resultados_totales <- list()

for (k in mis_terminos) {
  df_k <- scrap_keyword_v9(k)
  if (!is.null(df_k)) {
    resultados_totales[[k]] <- df_k
  }
  # PAUSA ENTRE KEYWORDS PARA EVITAR BLOQUEO
  message("   ... (espera 5s)...")
  Sys.sleep(5)
}

# =============================================================================
#  POST-PROCESAMIENTO Y GUARDADO
# =============================================================================

if (length(resultados_totales) > 0) {
  
  
  # AQUÍ CONSOLIDAMOS TODOS LOS RESULTADOS 
  df_final <- bind_rows(resultados_totales) %>%
    
    # 1. FILTRO TEMPORAL: Solo papers entre 1990-2025 
    mutate(Anio = as.numeric(Anio)) %>%
    filter(!is.na(Anio) & Anio >= 1990 & Anio <= 2025) %>%
    
    # 2. ELIMINACIÓN DE DUPLICADOS:Por título (pueden aparecer en múltiples keywords)
    distinct(Titulo, .keep_all = TRUE) %>%
    
    # 3. FILTRO SECUNDARIO DE REVISTAS: Eliminar áreas claramente irrelevantes
    filter(!str_detect(Revista, regex(paste(revistas_ruido, collapse = "|"), ignore_case = TRUE)))
  
  # =============================================================================
  # ESTRUCTURA CSV FINAL
  # =============================================================================
  # Columnas incluidas:
  # - Keyword_Origen: Palabra clave original que encontró el paper (TRAZABILIDAD)
  # - Variant_Used: Variante específica usada (singular/plural/con guión)
  # - Titulo: Título completo del artículo
  # - Autores: Lista de autores
  # - Revista: Nombre de la revista
  # - Anio: Año de publicación
  # - Abstract: Resumen completo (con fallback mejorado)
  # - URL: Enlace directo al artículo en SciELO
  # - DOI: Identificador único (si está disponible)
  # =============================================================================
  
  # Calcular estadísticas 
  n_total <- nrow(df_final)
  n_con_abstract <- sum(!is.na(df_final$Abstract))
  n_con_doi <- sum(!is.na(df_final$DOI))
  
  # Calcular porcentajes con protección contra división por cero
  pct_abstract <- if (n_total > 0) round(100 * n_con_abstract / n_total, 1) else 0
  pct_doi <- if (n_total > 0) round(100 * n_con_doi / n_total, 1) else 0
  
  message(paste0("    - Total de papers : ", n_total))
  message(paste0("    - Papers con abstract: ", n_con_abstract, " (", pct_abstract, "%)"))
  message(paste0("    - Papers con DOI: ", n_con_doi, " (", pct_doi, "%)"))
  
  # Guardar archivo CSV con fecha
  nombre_archivo <- paste0("scielo_", Sys.Date(), ".csv")
  write_csv(df_final, nombre_archivo)
 


# ==============================================================================
# POST-PROCESAMIENTO Y VISUALIZACIÓN
# ==============================================================================

library(tidyverse)
library(stringr)
library(wesanderson) 
library(scales)
# ------------------------------------------------------------------------------
# 1. CARGAR DATOS
# ------------------------------------------------------------------------------

df <- read_csv("scielo_2026-01-02.csv")

# 2. LIMPIEZA Y CATEGORIZACIÓN
# ------------------------------------------------------------------------------
# A. Filtro de Ruido 
palabras_ruido <- c("soil", "suelo", "solo ", "agronom", "harvest", 
                    "colheita", "weed", "crop", "bovino", "porcino")

# Función para detectar si es ruido (busca en título y abstract)
es_ruido <- function(titulo, abstract) {
  texto <- str_to_lower(paste(titulo, abstract))
  any(str_detect(texto, palabras_ruido))
}

# Aplicamos el filtro
df_clean <- df %>%
  rowwise() %>%
  filter(!es_ruido(Titulo, Abstract)) %>%
  ungroup()



# B. Clasificación de Disciplinas por Nombre de Revista
categorizar_disciplina <- function(revista) {
  r <- str_to_lower(revista)
  
  case_when(
    str_detect(r, "politic|política|opinião pública|saap|governo|internacional") ~ "Ciencia Política",
    str_detect(r, "sociol|social|sociedade|caderno crh|human|sociais") ~ "Sociología",
    str_detect(r, "econom|econôm|desarrollo|trimestre|finance") ~ "Economía",
    str_detect(r, "administra|management|gestão|business|contaduría|innovar|marketing|rausp|bar |ram |contábil|comercio|negocios") ~ "Negocios & Admin",  # RAUSP Management Journal,BAR - Brazilian Administration Review,RAM. Revista de Administração Mackenzie.
    str_detect(r, "psico|psychol") ~ "Psicología",
    str_detect(r, "educa|educare|enseñanza|formación") ~ "Educación",
    TRUE ~ "Otras Cs. Sociales"
  )
}
 # las que quedaron como otras son algunas de sociologia, relacion con antropologia, ciudad, algunas con turismo. 

# C. Extracción de País desde la URL
extraer_pais <- function(url) {
  case_when(
    str_detect(url, "\\.br") ~ "Brasil",
    str_detect(url, "\\.cl") ~ "Chile",
    str_detect(url, "\\.mx") ~ "México",
    str_detect(url, "\\.co") ~ "Colombia",
    str_detect(url, "\\.ar") ~ "Argentina",
    str_detect(url, "\\.pe") ~ "Perú",
    str_detect(url, "\\.uy") ~ "Uruguay",
    str_detect(url, "\\.ve") ~ "Venezuela",
    str_detect(url, "\\.ec") ~ "Ecuador",
    str_detect(url, "\\.cr") ~ "Costa Rica",
    str_detect(url, "\\.cu") ~ "Cuba",
    TRUE ~ "Otros"
  )
}

# Aplicamos las nuevas columnas
df_final <- df_clean %>%
  mutate(
    Disciplina = categorizar_disciplina(Revista),
    Pais = extraer_pais(URL)
  )

# paleta de colores 
paleta_disciplinas <- colorRampPalette(wes_palette("Zissou1", 5, type = "discrete"))(7)
paleta_paises <- colorRampPalette(wes_palette("Darjeeling1", 5, type = "discrete"))(10)

# 2. GRÁFICO A: EVOLUCIÓN TEMPORAL (APILADA)
# ------------------------------------------------------------------------------
g1 <- ggplot(df_final, aes(x = factor(Anio), fill = Disciplina)) +
  geom_bar(position = "stack", alpha = 0.9, width = 0.8) + # Barra apilada para ser más "decidor"
  scale_fill_manual(values = paleta_disciplinas) +
  theme_minimal() +
  # TUS TÍTULOS EXACTOS:
  labs(title = "Evolución LatAm (SciELO)",
       subtitle = paste("Total paperss:", nrow(df_final)),
       x = "Año", y = "Cantidad de Artículos",
       fill = "Disciplina") + # Etiqueta de la leyenda
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# 3. GRÁFICO B: POR DISCIPLINA
# ------------------------------------------------------------------------------
g2 <- ggplot(df_final, aes(y = fct_infreq(Disciplina), fill = Disciplina)) +
  geom_bar(alpha = 0.9) +
  scale_fill_manual(values = paleta_disciplinas) +
  theme_minimal() +
  # TUS TÍTULOS EXACTOS:
  labs(title = "Distribución por Disciplina",
       subtitle = "Por palabra revista",
       x = "Cantidad", y = "") +
  theme(legend.position = "none")

# 4. GRÁFICO C: POR PAÍS
# ------------------------------------------------------------------------------
g3 <- ggplot(df_final, aes(y = fct_infreq(Pais), fill = Pais)) +
  geom_bar(alpha = 0.9) +
  scale_fill_manual(values = paleta_paises) +
  theme_minimal() +
  # TUS TÍTULOS EXACTOS:
  labs(title = " País",
       x = "Cantidad", y = "") +
  theme(legend.position = "none")

# 5. GUARDAR
# ------------------------------------------------------------------------------
ggsave("grafico_evolucion.png", g1, width = 10, height = 6, dpi = 300)
ggsave("grafico_disciplinas.png", g2, width = 8, height = 5, dpi = 300)
ggsave("grafico_paises.png", g3, width = 8, height = 5, dpi = 300)


# Muestra qué país está publicando más en cada año
g4 <- ggplot(df_final, aes(x = factor(Anio), fill = Pais)) +
  geom_bar(position = "stack", alpha = 0.9, width = 0.8) +
  scale_fill_manual(values = paleta_paises) + # Usamos la misma paleta de países
  theme_minimal() +
  labs(title = "Ev.Producción por País",
       subtitle = "por pais",
       x = "Año", y = "Cantidad articulos",
       fill = "País") +
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# 2. GRÁFICO E: EVOLUCIÓN DE DISCIPLINAS POR AÑO
# ------------------------------------------------------------------------------
g5 <- ggplot(df_final, aes(x = factor(Anio), fill = Disciplina)) +
  geom_bar(position = "stack", alpha = 0.9, width = 0.8) +
  scale_fill_manual(values = paleta_disciplinas) +
  theme_minimal() +
  labs(title = "Evolución Temática ",
       subtitle = "Cambio en el tiempo",
       x = "Año", y = "Cantidad de Artículos",
       fill = "Disciplina") +
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# 3. GUARDAR
# ------------------------------------------------------------------------------
ggsave("grafico_evolucion_paises.png", g4, width = 10, height = 6, dpi = 300)
ggsave("grafico_evolucion_temas.png", g5, width = 10, height = 6, dpi = 300)
