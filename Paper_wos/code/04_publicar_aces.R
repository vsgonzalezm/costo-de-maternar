#-----------------------------------------------
# 04 - Publicar presentacion ACES
# Valentina Gonzalez Madariaga
# Agosto 2026
#-----------------------------------------------

library(here)

# Renderizar la presentacion desde el QMD.
quarto::quarto_render(here("Paper_wos", "aces", "aces_experimental_latam.qmd"))

# Funcion pequena para agregar noindex al HTML.
add_noindex <- function(html_path) {
  html <- readLines(html_path, warn = FALSE, encoding = "UTF-8")
  html <- gsub("`r`n", "", html, fixed = TRUE)
  html <- gsub("`n", "", html, fixed = TRUE)
  if (!any(grepl('name="robots"', html, fixed = TRUE))) {
    html <- sub("<head>", paste0("<head>", "\n", '<meta name="robots" content="noindex, nofollow">'), html, fixed = TRUE)
  }
  writeLines(html, html_path, useBytes = TRUE)
}

# Copia local para GitHub Pages.
dir.create(here("Paper_wos", "docs", "aces"), recursive = TRUE, showWarnings = FALSE)
file.copy(
  from = here("Paper_wos", "aces", "aces_experimental_latam.html"),
  to = here("Paper_wos", "docs", "aces", "index.html"),
  overwrite = TRUE
)
file.create(here("Paper_wos", "docs", ".nojekyll"))
writeLines("User-agent: *\nDisallow: /", here("Paper_wos", "docs", "robots.txt"))
add_noindex(here("Paper_wos", "docs", "aces", "index.html"))

# Copia al repo del sitio personal, si existe el clon local.
site_dir <- here("Paper_wos", "publish_site_tmp")
if (dir.exists(site_dir)) {
  dir.create(file.path(site_dir, "static", "aces"), recursive = TRUE, showWarnings = FALSE)
  file.copy(
    from = here("Paper_wos", "aces", "aces_experimental_latam.html"),
    to = file.path(site_dir, "static", "aces", "index.html"),
    overwrite = TRUE
  )
  add_noindex(file.path(site_dir, "static", "aces", "index.html"))
}

message("Ready: Paper_wos/docs/aces/index.html")
if (dir.exists(site_dir)) message("Ready: Paper_wos/publish_site_tmp/static/aces/index.html")
message("Public URL after commit + push in publish_site_tmp:")
message("https://vsgonzalezm.github.io/aces/")
