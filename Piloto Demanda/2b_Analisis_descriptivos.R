
# LIBRERÍAS

library(tidyverse)
library(haven)
library(here)
library(flextable)
library(officer)



# CARGA DE DATOS

df_raw <- read_sav(here("data/procesado/base_pretest_long_final.sav"))

df_enc <- df_raw %>% filter(pantalla_orden == 1)

cat("N encuestados:", nrow(df_enc), "\n")

dir.create(here("figuras/descriptivos"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("tablas/descriptivos"),  recursive = TRUE, showWarnings = FALSE)



# ETIQUETAS


etiquetas_empresa <- c(
  "empresa_disp_fuera_horario" = "Disponible fuera\ndel horario laboral",
  "empresa_priorizar_trabajo"  = "Priorizar trabajo\nsobre vida personal",
  "empresa_evitar_ausencias"   = "Evitar ausencias por\nresponsabilidades familiares"
)

etiquetas_acuerdo <- c(
  "acuerdo_progresar_priorizar" = "Razonable priorizar trabajo\nsobre vida personal/familiar",
  "acuerdo_buen_trabaja_disp"   = "Buen/a trabajador/a debe estar\ndisponible fuera de jornada",
  "acuerdo_postnatal_1ano"      = "Postnatal debería\nextenderse a un año",
  "acuerdo_salacuna_todos"      = "Empleadores deben proveer\nsala cuna a todos"
)

etiquetas_escala_empresa <- c(
  "1" = "1\nPara nada",
  "2" = "2", "3" = "3", "4" = "4", "5" = "5", "6" = "6",
  "7" = "7\nExtremadamente"
)

etiquetas_escala_acuerdo <- c(
  "1" = "1\nTotalmente\nen desacuerdo",
  "2" = "2", "3" = "3", "4" = "4", "5" = "5", "6" = "6",
  "7" = "7\nTotalmente\nde acuerdo"
)

# Paleta divergente 7 colores: rojo → amarillo → gris → azul claro → azul
paleta7 <- c(
  "1" = "#d73027",
  "2" = "#fc8d59",
  "3" = "#fee090",
  "4" = "#d9d9d9",
  "5" = "#91bfdb",
  "6" = "#4393c3",
  "7" = "#2166ac"
)



# HELPERS

preparar_dist <- function(df, vars, etiquetas_vars) {
  map_dfr(vars, function(v) {
    df %>%
      count(valor = as.numeric(.data[[v]])) %>%
      filter(!is.na(valor)) %>%
      mutate(
        pct      = n / sum(n) * 100,
        variable = etiquetas_vars[v],
        valor    = factor(valor, levels = 1:7)
      )
  }) %>%
    mutate(variable = factor(variable, levels = etiquetas_vars))
}

tabla_descriptivos <- function(df, vars, etiquetas_vars) {
  map_dfr(vars, function(v) {
    x <- as.numeric(df[[v]])
    tibble(
      Variable      = etiquetas_vars[v],
      N             = sum(!is.na(x)),
      Media         = round(mean(x, na.rm = TRUE), 2),
      DE            = round(sd(x,   na.rm = TRUE), 2),
      Mín           = min(x, na.rm = TRUE),
      Mediana       = median(x, na.rm = TRUE),
      Máx           = max(x, na.rm = TRUE),
      `% valor 6-7` = round(mean(x >= 6, na.rm = TRUE) * 100, 1)
    )
  })
}

exportar_docx <- function(df_tabla, ruta, titulo = NULL, nota = NULL) {
  ft <- df_tabla %>%
    flextable() %>%
    theme_booktabs() %>%
    bg(bg = "#2E74B5", part = "header") %>%
    color(color = "white", part = "header") %>%
    bold(part = "header") %>%
    fontsize(size = 11, part = "all") %>%
    font(fontname = "Arial", part = "all") %>%
    autofit()
  
  ft <- bg(ft,
           i    = seq(2, nrow(df_tabla), by = 2),
           bg   = "#F2F7FC",
           part = "body")
  
  doc <- read_docx() %>% body_add_par("", style = "Normal")
  if (!is.null(titulo))
    doc <- doc %>% body_add_par(titulo, style = "heading 2")
  if (!is.null(nota))
    doc <- doc %>% body_add_par(nota, style = "Normal")
  doc <- doc %>% body_add_flextable(ft)
  print(doc, target = ruta)
  cat("✓ Tabla guardada:", ruta, "\n")
}



# BLOQUE 1: EMPRESA

vars_empresa  <- names(etiquetas_empresa)
dist_empresa  <- preparar_dist(df_enc, vars_empresa, etiquetas_empresa)

fig_empresa <- ggplot(dist_empresa,
                      aes(x = valor, y = pct, fill = valor)) +
  geom_col(width = 0.75, color = "white") +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            vjust = -0.4, size = 3.2, fontface = "bold") +
  facet_wrap(~ variable, ncol = 3) +
  scale_fill_manual(values = paleta7, guide = "none") +
  scale_x_discrete(labels = etiquetas_escala_empresa) +
  scale_y_continuous(limits = c(0, 55),
                     breaks = seq(0, 50, 10),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Importancia de atributos laborales en la empresa actual",
    subtitle = "Escala 1 (para nada) a 7 (extremadamente) — % de respuestas por categoría",
    x        = NULL,
    y        = "Porcentaje (%)",
    caption  = paste0("N = ", nrow(df_enc), " encuestados.")
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10, color = "gray40"),
    strip.background = element_rect(fill = "#f0f0f0"),
    strip.text       = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(size = 8)
  )

ggsave(here("figuras/descriptivos/dist_empresa.png"),
       fig_empresa, width = 13, height = 5, dpi = 150, bg = "white")
cat("✓ Figura empresa guardada\n")

tab_empresa <- tabla_descriptivos(df_enc, vars_empresa, etiquetas_empresa)
print(tab_empresa)

exportar_docx(
  tab_empresa,
  here("tablas/descriptivos/dist_empresa.docx"),
  titulo = "Importancia de atributos laborales en la empresa actual",
  nota   = "Nota: Escala 1 (para nada) a 7 (extremadamente). % valor 6-7 = proporción que respondió 6 o 7."
)


# BLOQUE 2: ACUERDO


vars_acuerdo <- names(etiquetas_acuerdo)
dist_acuerdo <- preparar_dist(df_enc, vars_acuerdo, etiquetas_acuerdo)

fig_acuerdo <- ggplot(dist_acuerdo,
                      aes(x = valor, y = pct, fill = valor)) +
  geom_col(width = 0.75, color = "white") +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            vjust = -0.4, size = 3.2, fontface = "bold") +
  facet_wrap(~ variable, ncol = 2) +
  scale_fill_manual(values = paleta7, guide = "none") +
  scale_x_discrete(labels = etiquetas_escala_acuerdo) +
  scale_y_continuous(limits = c(0, 55),
                     breaks = seq(0, 50, 10),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Acuerdo con afirmaciones sobre trabajo y familia",
    subtitle = "Escala 1 (totalmente en desacuerdo) a 7 (totalmente de acuerdo) — % de respuestas por categoría",
    x        = NULL,
    y        = "Porcentaje (%)",
    caption  = paste0("N = ", nrow(df_enc), " encuestados.")
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10, color = "gray40"),
    strip.background = element_rect(fill = "#f0f0f0"),
    strip.text       = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(size = 8)
  )

ggsave(here("figuras/descriptivos/dist_acuerdo.png"),
       fig_acuerdo, width = 13, height = 8, dpi = 150, bg = "white")
cat("✓ Figura acuerdo guardada\n")

tab_acuerdo <- tabla_descriptivos(df_enc, vars_acuerdo, etiquetas_acuerdo)
print(tab_acuerdo)

exportar_docx(
  tab_acuerdo,
  here("tablas/descriptivos/dist_acuerdo.docx"),
  titulo = "Acuerdo con afirmaciones sobre trabajo y familia",
  nota   = "Nota: Escala 1 (totalmente en desacuerdo) a 7 (totalmente de acuerdo). % valor 6-7 = proporción que respondió 6 o 7."
)

cat("\n✓ Proceso completado.\n")
cat("  Figuras en: figuras/descriptivos/\n")
cat("  Tablas en:  tablas/descriptivos/\n")