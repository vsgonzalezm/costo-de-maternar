#-----------------------------------------------
# 07 - Figuras
# Este script genera las FIGURAS 2 Y 3 del paper de metodos experimentales
# (ver Outline_current, seccion Results):
#   - Figura 2: produccion relativa por region de afiliacion (Latam, USA, Europa)
#   - Figura 3: produccion relativa por disciplina (Latam)
# Borrador basado en archive/pipeline_pre_affi_latam_2026-11-05/code/
#   (01_denominadores_disciplina_anio.R y 02_graficos.R), adaptado a la
#   estructura actual de data/raw y a base_oficial.rds.
# Noviembre 2026
#-----------------------------------------------
#
# Figura 2. Papers experimentales por cada 1.000 papers de ciencias sociales
#           en WoS, por region de afiliacion (Latam, USA, Europa) y anio,
#           1990-2025. No mutuamente excluyente entre regiones.
# Figura 3. Papers experimentales con afiliacion latinoamericana por cada
#           1.000 papers de la disciplina (Latam), por disciplina y anio.
#
# Entradas (Paper_wos/data/):
#   raw/experimental/{latam,usa,europa}_exp.txt          -> numeradores Fig. 2
#   raw/totales denominador/por region-año/*_gral.txt    -> denominadores Fig. 2
#   intermediate/base_oficial.rds                        -> numeradores Fig. 3
#   raw/totales denominador/por disciplina-pais-año/     -> denominadores Fig. 3
# No se usan raw/endogeno ni raw/laboratorio.
#
# Salidas (Paper_wos/outputs/):
#   figures/fig2_produccion_relativa_region.png (+ _suavizada)
#   figures/fig3_produccion_relativa_disciplina_latam.png
#   tables/fig2_produccion_relativa_region.csv
#   tables/fig3_produccion_relativa_disciplina_latam.csv
#-----------------------------------------------

library(tidyverse)
library(here)
library(scales)

# Leer bien rutas con acentos ("region-año"): forzar UTF-8 segun sistema
if (!l10n_info()$`UTF-8`) {
  invisible(if (.Platform$OS.type == "windows") {
    Sys.setlocale("LC_CTYPE", "Spanish_Chile.utf8")
  } else {
    Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
  })
}

anio_min <- 1990
anio_max <- 2025   # 2026 es parcial: se excluye de las figuras principales

data_dir <- here("Paper_wos", "data")
raw_dir  <- file.path(data_dir, "raw")
fig_dir  <- here("Paper_wos", "outputs", "figures")
tab_dir  <- here("Paper_wos", "outputs", "tables")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)

# La ruta usa un nombre valido en Windows y debe ser identica en todos los
# computadores para que el flujo sea reproducible.
dir_totales <- file.path(raw_dir, "totales denominador")
if (!dir.exists(dir_totales)) {
  stop("No existe la carpeta de denominadores: ", dir_totales)
}
dir_region     <- list.dirs(dir_totales, recursive = FALSE) |> keep(~ str_detect(basename(.x), "region"))
dir_disciplina <- list.dirs(dir_totales, recursive = FALSE) |> keep(~ str_detect(basename(.x), "disciplina"))

# Lee una exportacion "Analyze results" de WoS (anio <tab> conteo).
# Ignora encabezado, notas (#) y lineas como "478 record(s) do not contain...".
leer_conteo_wos <- function(path) {
  lineas <- read_lines(path)
  lineas <- lineas[str_detect(lineas, "^\\d{4}\\t\\d+")]
  tibble(x = lineas) |>
    separate(x, into = c("year", "n"), sep = "\t", extra = "drop") |>
    mutate(year = as.integer(year), n = as.numeric(n))
}

# Media movil centrada de 3 anios (en extremos usa los anios disponibles)
mm3 <- function(x) {
  (coalesce(lag(x), x) + x + coalesce(lead(x), x)) / 3
}

paleta_region <- c("Latin America" = "#D55E00", "USA" = "#0072B2", "Europe" = "#009E73")

#-----------------------------------------------
# FIGURA 2
#-----------------------------------------------
regiones <- tribble(
  ~region,         ~num_file,                                        ~den_file,
  "Latin America", file.path(raw_dir, "experimental", "latam_exp.txt"),  file.path(dir_region, "latam_gral.txt"),
  "USA",           file.path(raw_dir, "experimental", "usa_exp.txt"),    file.path(dir_region, "usa_gral.txt"),
  "Europe",        file.path(raw_dir, "experimental", "europa_exp.txt"), file.path(dir_region, "europa_gral.txt")
)

fig2_data <- regiones |>
  mutate(
    num = map(num_file, leer_conteo_wos),
    den = map(den_file, leer_conteo_wos)
  ) |>
  mutate(datos = map2(num, den, ~ full_join(
    rename(.x, experimental_n = n), rename(.y, total_n = n), by = "year"
  ))) |>
  select(region, datos) |>
  unnest(datos) |>
  # Completar anios sin papers experimentales con 0
  complete(region, year = anio_min:anio_max, fill = list(experimental_n = 0)) |>
  filter(between(year, anio_min, anio_max)) |>
  mutate(rate_per_1000 = if_else(total_n > 0, experimental_n / total_n * 1000, NA_real_)) |>
  group_by(region) |>
  arrange(year, .by_group = TRUE) |>
  mutate(rate_per_1000_mm3 = mm3(rate_per_1000)) |>
  ungroup() |>
  mutate(region = factor(region, levels = names(paleta_region)))

write_csv(fig2_data, file.path(tab_dir, "fig2_produccion_relativa_region.csv"))

tema_base <- theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title.position = "plot",
    plot.caption = element_text(hjust = 0, color = "grey40", size = 9)
  )

nota_fig2 <- paste(
  "Note: Web of Science Core Collection, social sciences. Regions by author affiliation; a paper can count in more than one region.",
  "\nLatin America numerator rebuilt from the validated corpus (unique UT); USA and Europe numerators are direct WoS counts. 2026 excluded (partial)."
)

fig2_plot <- ggplot(fig2_data, aes(year, rate_per_1000, color = region)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.4) +
  scale_color_manual(values = paleta_region) +
  scale_x_continuous(breaks = seq(anio_min, anio_max, 5)) +
  labs(
    title = "Figure 2. Relative production of papers using experimental methods",
    subtitle = "Experimental papers per 1,000 social science papers, by region of affiliation, 1990-2025",
    x = NULL, y = "Experimental papers\nper 1,000 papers", caption = nota_fig2
  ) +
  tema_base

ggsave(file.path(fig_dir, "fig2_produccion_relativa_region.png"), fig2_plot,
       width = 11, height = 6.4, dpi = 320, bg = "white")

# Variante suavizada (media movil 3 anios), por si los primeros anios distorsionan
fig2_plot_mm3 <- fig2_plot +
  aes(y = rate_per_1000_mm3) +
  labs(subtitle = "Experimental papers per 1,000 social science papers, 3-year moving average, 1990-2025")

ggsave(file.path(fig_dir, "fig2_produccion_relativa_region_suavizada.png"), fig2_plot_mm3,
       width = 11, height = 6.4, dpi = 320, bg = "white")

#-----------------------------------------------
# FIGURA 3 (Latin America)
#-----------------------------------------------
paises_latam <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica",
                  "Cuba", "Dominican Rep", "Ecuador", "Guatemala", "Mexico", "Peru",
                  "Uruguay", "Venezuela")

disciplinas <- tribble(
  ~patron,             ~discipline,
  "business",          "Business/Management",
  "economics",         "Economics",
  "other",             "Other",
  "political science", "Political Science",
  "psychology",        "Psychology",
  "sociology",         "Sociology"
)

# Denominador: suma de los 14 paises latinoamericanos por disciplina y anio.
# Ojo: un paper con autores de varios paises latinoamericanos se suma mas de
# una vez (no hay un archivo deduplicado "Latam" por disciplina).
den_disciplina <- tibble(carpeta = list.dirs(dir_disciplina, recursive = FALSE)) |>
  mutate(patron = map_chr(basename(carpeta), ~ disciplinas$patron[str_detect(.x, fixed(disciplinas$patron))][1])) |>
  left_join(disciplinas, by = "patron") |>
  mutate(file = map(carpeta, ~ list.files(.x, pattern = "\\.txt$", full.names = TRUE))) |>
  unnest(file) |>
  mutate(country = str_remove(basename(file), "\\.txt$") |> str_squish() |>
           recode("Brasil" = "Brazil")) |>
  filter(country %in% paises_latam) |>
  mutate(data = map(file, leer_conteo_wos)) |>
  unnest(data) |>
  group_by(discipline, year) |>
  summarise(total_n = sum(n), n_paises = n_distinct(country), .groups = "drop")

# Chequeo: cada disciplina debe tener los 14 paises
faltantes <- den_disciplina |> group_by(discipline) |> summarise(max_paises = max(n_paises))
if (any(faltantes$max_paises < 14)) {
  warning("Faltan paises en algun denominador por disciplina:\n",
          paste(capture.output(print(faltantes)), collapse = "\n"))
}

# Numerador: papers del corpus validado por disciplina (core_area) y anio.
# Un paper puede estar en mas de una disciplina; se cuenta una vez en cada una.
base_oficial <- readRDS(file.path(data_dir, "intermediate", "base_oficial.rds"))
stopifnot(n_distinct(base_oficial$UT) == nrow(base_oficial))

num_disciplina <- base_oficial |>
  transmute(UT, year = as.integer(PY), core_area) |>
  separate_rows(core_area, sep = ";") |>
  mutate(discipline = str_squish(core_area)) |>
  filter(discipline != "") |>
  distinct(UT, year, discipline) |>
  count(discipline, year, name = "experimental_n")

fig3_data <- den_disciplina |>
  full_join(num_disciplina, by = c("discipline", "year")) |>
  complete(discipline, year = anio_min:anio_max, fill = list(experimental_n = 0)) |>
  filter(between(year, anio_min, anio_max)) |>
  mutate(rate_per_1000 = if_else(total_n > 0, experimental_n / total_n * 1000, NA_real_)) |>
  group_by(discipline) |>
  arrange(year, .by_group = TRUE) |>
  mutate(rate_per_1000_mm3 = mm3(rate_per_1000)) |>
  ungroup()

write_csv(fig3_data, file.path(tab_dir, "fig3_produccion_relativa_disciplina_latam.csv"))

totales_disc <- fig3_data |> group_by(discipline) |> summarise(n = sum(experimental_n))
etiquetas_disc <- setNames(paste0(totales_disc$discipline, " (n = ", totales_disc$n, ")"),
                           totales_disc$discipline)

nota_fig3 <- paste(
  "Note: Latin American-affiliated experimental papers (validated corpus) per 1,000 WoS papers in each discipline by authors in 14 Latin American countries.",
  "\nPoints: annual rate; line: 3-year moving average. Disciplines overlap (WoS categories). Denominator sums countries, so multi-country papers count more than once.",
  "\n2026 excluded (partial)."
)

fig3_plot <- ggplot(fig3_data, aes(year)) +
  geom_point(aes(y = rate_per_1000), color = "grey60", size = 1) +
  geom_line(aes(y = rate_per_1000_mm3), color = "#D55E00", linewidth = 1) +
  facet_wrap(~ discipline, ncol = 3, labeller = as_labeller(etiquetas_disc)) +
  scale_x_continuous(breaks = seq(anio_min, anio_max, 10)) +
  labs(
    title = "Figure 3. Relative production of experimental papers by discipline, Latin America",
    subtitle = "Experimental papers per 1,000 papers in the discipline, 1990-2025",
    x = NULL, y = "Experimental papers\nper 1,000 papers", caption = nota_fig3
  ) +
  tema_base +
  theme(strip.text = element_text(face = "bold", hjust = 0))

ggsave(file.path(fig_dir, "fig3_produccion_relativa_disciplina_latam.png"), fig3_plot,
       width = 12, height = 7.6, dpi = 320, bg = "white")

message("Listo: figuras y tablas en Paper_wos/outputs/")
