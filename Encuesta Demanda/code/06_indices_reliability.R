# ============================================================================
# 06 - Reliability (Cronbach's alpha) of the constructs behind the composite indices
# Author: Isi
# Modified: Tania Hutt - separado de 07_Indices.R (octubre 2026)
# Purpose: Documents the pre-registered decision rule (alpha >= .70 -> items are
#          averaged into an index; otherwise they are analysed individually).
#          The indices themselves are built by add_indices() in 00_setup.R
#          (constructos list); this script only reports/justifies them.
# Outputs: outputs/06_indices_reliability/
# ============================================================================
proj <- if (dir.exists(here::here("Encuesta Demanda", "data"))) here::here("Encuesta Demanda") else here::here()
source(file.path(proj, "code", "00_setup.R"))
library(psych)
init_output("06_indices_reliability")

base <- read_csv(analysis_ready_path, show_col_types = FALSE) %>% add_indices()

calcular_alpha <- function(datos, items, nombre) {
  a <- psych::alpha(as.data.frame(datos[, items]), check.keys = TRUE)
  raw_alpha <- a$total$raw_alpha
  cat(sprintf("  %-12s alpha = %.3f -> %s\n", nombre, raw_alpha,
              ifelse(raw_alpha >= .70, "SE AGREGA como indice", "se analiza por item individual")))
  list(alpha_obj = a, raw_alpha = raw_alpha, agregar = raw_alpha >= .70)
}

# long_term_commitment_score ya entra invertido (_rev) en constructos$riesgo (ver add_indices()).
cat("Confiabilidad por constructo:\n")
resultados_alpha <- imap(constructos, ~ calcular_alpha(base, .x, .y))

tabla_alpha <- imap_dfr(resultados_alpha, function(res, nombre) {
  tibble(
    constructo = nombre,
    items      = paste(constructos[[nombre]], collapse = ", "),
    n_items    = length(constructos[[nombre]]),
    raw_alpha  = round(res$raw_alpha, 3),
    decision   = ifelse(res$agregar, "Se agrega como indice", "Se analiza por item individual")
  )
})
write_csv(tabla_alpha, out_path("alpha_por_constructo.csv"))

# Chequeo EXPLORATORIO: alpha de costo incluyendo salary_costly_score y "buena inversion".
# No reemplaza la definicion confirmatoria de costo (3 items).
costo_con_inversion <- c("salary_costly_score", "operational_costly_score",
                         "expected_benefits_use", "expected_accommodations", "good_investment_score")
alpha_expl <- calcular_alpha(base, costo_con_inversion, "costo_con_inversion (EXPLORATORIO)")
tibble(
  constructo = "costo_con_inversion",
  items      = paste(costo_con_inversion, collapse = ", "),
  n_items    = length(costo_con_inversion),
  raw_alpha  = round(alpha_expl$raw_alpha, 3),
  decision   = "EXPLORATORIO -- no reemplaza la definicion confirmatoria de 'costo' (3 items)"
) %>% write_csv(out_path("alpha_exploratorio_costo_con_inversion.csv"))

# salary_costly_score queda fuera de todo constructo (revertirlo no arregla la direccion:
# no mide el mismo latente que costo/investment/evaluation; alpha <= .16-.41 en todas las pruebas).
finish_output()
