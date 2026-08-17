
library(tidyverse)
library(haven)
library(estimatr)
library(modelsummary)
library(broom)
library(ggplot2)
library(here)

df_raw <- read_sav(here("data/procesado/base_pretest_long_final.sav"))


# ============================================================
# PREPARACIÓN DE VARIABLES
# ============================================================

df <- df_raw %>%
  mutate(
    perfil_genero       = relevel(as_factor(perfil_genero),       ref = "Hombre"),
    perfil_estado_civil = relevel(as_factor(perfil_estado_civil), ref = "soltero/a"),
    perfil_parental     = factor(as_factor(perfil_parental),
                                 levels = c("actualmente no tiene hijos",
                                            "tiene un hijo",
                                            "tiene dos hijos")),
    perfil_plan_futuro  = relevel(as_factor(perfil_plan_futuro),  ref = "no quiere"),
    perfil_ocupacion    = as_factor(perfil_ocupacion),
    perfil_exp          = as_factor(perfil_exp),
    perfil_desempeno    = as_factor(perfil_desempeno),
    pantalla_orden      = relevel(factor(pantalla_orden),         ref = "1"),
    template_contexto   = relevel(as_factor(template_contexto),   ref = "B")
  )


# ============================================================
# OUTCOMES Y ETIQUETAS
# ============================================================

outcomes_p1             <- "ans_p1_global"
outcomes_evaluacion     <- paste0("ans_p2_", 1:7)
outcomes_comportamiento <- paste0("ans_p3_", 1:8)
all_outcomes            <- c(outcomes_p1, outcomes_evaluacion, outcomes_comportamiento)

labels_outcomes <- c(
  "ans_p1_global" = "Recomendación de contratación",
  "ans_p2_1"      = "Confiable",
  "ans_p2_2"      = "Capaz",
  "ans_p2_3"      = "Comprometido/a largo plazo",
  "ans_p2_4"      = "Prometedor/a",
  "ans_p2_5"      = "Ambicioso/a",
  "ans_p2_6"      = "Riesgoso/a para la empresa",
  "ans_p2_7"      = "Costoso/a para la empresa",
  "ans_p3_1"      = "Usará beneficios empresa",
  "ans_p3_2"      = "Solicitará facilidades/permisos",
  "ans_p3_3"      = "Llegará tarde o se ausentará",
  "ans_p3_4"      = "Renunciará en próximos años",
  "ans_p3_5"      = "Tomará mayores responsabilidades",
  "ans_p3_6"      = "Será ascendido/a o promovido/a",
  "ans_p3_7"      = "Generará costos económicos adicionales",
  "ans_p3_8"      = "Generará costos operativos/organizacionales"
)

controles_base <- "perfil_genero + perfil_estado_civil + perfil_parental +
                   perfil_plan_futuro + perfil_exp + perfil_desempeno +
                   template_contexto"


# ============================================================
#  EFECTO DE OCUPACIÓN EN EL PRIMER PERFIL
# ============================================================

df_perfil1 <- df %>% filter(pantalla_orden == "1")

cat("N observaciones en primer perfil:", nrow(df_perfil1), "\n")

formula_p1 <- paste("perfil_ocupacion +", controles_base)

modelos_ocupacion_p1 <- map(all_outcomes, function(y) {
  fml <- as.formula(paste(y, "~", formula_p1))
  lm_robust(fml,
            data     = df_perfil1,
            clusters = numericalId,
            se_type  = "CR2")
}) %>% set_names(all_outcomes)

cat("✓ Modelos ocupación (solo primer perfil) corridos\n")


# ============================================================
# EFECTO DE OCUPACIÓN SEGÚN COMBINACIÓN DE PERFILES
# construi la variable combo_ocupacion (homogénea vs heterogénea: cuando les mostraban la misma ocupación vs distintas ocupaciones)
# ============================================================

df <- df %>%
  group_by(numericalId) %>%
  mutate(
    n_tipos_ocupacion = n_distinct(as.character(perfil_ocupacion)),
    combo_ocupacion   = case_when(
      n_tipos_ocupacion == 1 ~ "homogénea",
      n_tipos_ocupacion == 2 ~ "heterogénea"
    ),
    combo_ocupacion = factor(combo_ocupacion,
                             levels = c("homogénea", "heterogénea"))
  ) %>%
  ungroup()

# Verificar distribución de combinaciones
cat("\nDistribución de combinaciones de ocupación por encuestado:\n")
df %>%
  distinct(numericalId, combo_ocupacion) %>%
  count(combo_ocupacion) %>%
  print()

# Submuestra solo homogénea (evaluaron dos perfiles del mismo tipo)
df_homogenea <- df %>% filter(combo_ocupacion == "homogénea")

cat("\nN observaciones en combinación homogénea:", nrow(df_homogenea), "\n")

# --- Modelo 2a: efecto de ocupación en submuestra homogénea ---

modelos_ocupacion_homogenea <- map(all_outcomes, function(y) {
  fml <- as.formula(paste(y, "~ perfil_ocupacion +", controles_base))
  lm_robust(fml,
            data     = df_homogenea,
            clusters = numericalId,
            se_type  = "CR2")
}) %>% set_names(all_outcomes)

cat("✓ Modelos ocupación (combinación homogénea) corridos\n")

# --- Modelo 2b: interacción ocupación × combo en muestra completa ---
# Permite testear directamente si el efecto de ocupación difiere
# según si el evaluador tuvo o no contraste disponible

formula_combo <- paste(
  "perfil_ocupacion * combo_ocupacion +",
  controles_base
)

modelos_ocupacion_combo <- map(all_outcomes, function(y) {
  fml <- as.formula(paste(y, "~", formula_combo))
  lm_robust(fml,
            data     = df,
            clusters = numericalId,
            se_type  = "CR2")
}) %>% set_names(all_outcomes)

cat("✓ Modelos ocupación × combo corridos\n")


# ============================================================
# EXPORTAR TABLAS
# ============================================================

exportar_tabla <- function(lista_modelos, outcomes, nombre_archivo, titulo = NULL) {
  modelos_sel        <- lista_modelos[outcomes]
  names(modelos_sel) <- labels_outcomes[outcomes]
  modelsummary(
    modelos_sel,
    stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    gof_map = c("nobs", "r.squared", "adj.r.squared"),
    title   = titulo,
    output  = nombre_archivo
  )
}

#solo primer perfil
exportar_tabla(
  modelos_ocupacion_p1, outcomes_p1,
  here("tablas/ocup_primerperfil_contratacion.docx"),
  "Efecto ocupación — solo primer perfil evaluado"
)
exportar_tabla(
  modelos_ocupacion_p1, outcomes_evaluacion,
  here("tablas/ocup_primerperfil_evaluaciones.docx"),
  "Efecto ocupación — solo primer perfil evaluado"
)
exportar_tabla(
  modelos_ocupacion_p1, outcomes_comportamiento,
  here("tablas/ocup_primerperfil_comportamiento.docx"),
  "Efecto ocupación — solo primer perfil evaluado"
)

# 2a: submuestra homogénea
exportar_tabla(
  modelos_ocupacion_homogenea, outcomes_p1,
  here("tablas/ocup_homogenea_contratacion.docx"),
  "Efecto ocupación — combinación homogénea (sin contraste)"
)
exportar_tabla(
  modelos_ocupacion_homogenea, outcomes_evaluacion,
  here("tablas/ocup_homogenea_evaluaciones.docx"),
  "Efecto ocupación — combinación homogénea (sin contraste)"
)
exportar_tabla(
  modelos_ocupacion_homogenea, outcomes_comportamiento,
  here("tablas/ocup_homogenea_comportamiento.docx"),
  "Efecto ocupación — combinación homogénea (sin contraste)"
)

# 2b: interacción ocupación × combo
exportar_tabla(
  modelos_ocupacion_combo, outcomes_p1,
  here("tablas/ocup_combo_contratacion.docx"),
  "Efecto ocupación × tipo de combinación"
)
exportar_tabla(
  modelos_ocupacion_combo, outcomes_evaluacion,
  here("tablas/ocup_combo_evaluaciones.docx"),
  "Efecto ocupación × tipo de combinación"
)
exportar_tabla(
  modelos_ocupacion_combo, outcomes_comportamiento,
  here("tablas/ocup_combo_comportamiento.docx"),
  "Efecto ocupación × tipo de combinación"
)

cat("✓ Todas las tablas exportadas\n")


# ============================================================
# EXTRAER Y PREPARAR RESULTADOS PARA FIGURAS
# ============================================================

preparar_df_figura <- function(lista_modelos, nombre_modelo) {
  map_dfr(all_outcomes, ~ tidy(lista_modelos[[.x]]) %>%
            mutate(outcome = .x)) %>%
    filter(!str_detect(term, "Intercept"), !is.na(estimate)) %>%
    mutate(
      modelo = nombre_modelo,
      
      grupo_outcome = case_when(
        outcome == "ans_p1_global"           ~ "Contratación",
        outcome %in% outcomes_evaluacion     ~ "Evaluación",
        outcome %in% outcomes_comportamiento ~ "Comportamiento futuro"
      ),
      
      outcome_label = labels_outcomes[outcome],
      
      term_label = case_when(
        term == "perfil_generoMujer"                                          ~ "Género: Mujer",
        term == "perfil_estado_civilcasado/a"                                 ~ "Estado civil: Casado/a",
        term == "perfil_estado_civilconviviendo"                              ~ "Estado civil: Conviviendo",
        term == "perfil_parentaltiene un hijo"                                ~ "Parental: Un hijo",
        term == "perfil_parentaltiene dos hijos"                              ~ "Parental: Dos hijos",
        term == "perfil_plan_futuroen 2 años"                                 ~ "Plan: en 2 años",
        term == "perfil_plan_futuroen 5 años"                                 ~ "Plan: en 5 años",
        term == "perfil_ocupacionasociado/a financiero/a"                     ~ "Ocupación: asociado/a financiero/a",
        term == "combo_ocupaciónheterogénea"                                  ~ "Combo: heterogénea",
        term == "perfil_ocupacionasociado/a financiero/a:combo_ocupaciónheterogénea" ~ "Asociado/a financiero/a × heterogénea",
        term == "perfil_exp6 años"                                            ~ "Experiencia: 6 años",
        term == "perfil_exp8 años"                                            ~ "Experiencia: 8 años",
        term == "perfil_desempeno6.0"                                         ~ "Desempeño: 6.0",
        term == "perfil_desempeno6.3"                                         ~ "Desempeño: 6.3",
        term == "template_contextoA"                                          ~ "Template: A",
        TRUE ~ term
      ),
      
      term_grupo = case_when(
        term == "perfil_generoMujer"                                          ~ "Género",
        term %in% c("perfil_estado_civilcasado/a",
                    "perfil_estado_civilconviviendo")                         ~ "Estado civil",
        term %in% c("perfil_parentaltiene un hijo",
                    "perfil_parentaltiene dos hijos")                         ~ "Estado parental",
        term %in% c("perfil_plan_futuroen 2 años",
                    "perfil_plan_futuroen 5 años")                            ~ "Plan reproductivo",
        term == "perfil_ocupacionasociado/a financiero/a"                     ~ "Ocupación",
        term %in% c("combo_ocupaciónheterogénea",
                    "perfil_ocupacionasociado/a financiero/a:combo_ocupaciónheterogénea") ~ "Interacción combo",
        term %in% c("perfil_exp6 años", "perfil_exp8 años",
                    "perfil_desempeno6.0", "perfil_desempeno6.3")             ~ "Controles perfil",
        term %in% c("template_contextoA", "template_contextoB")              ~ "Controles diseño",
        TRUE ~ "Otros"
      ),
      
      sig = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        p.value < 0.1   ~ ".",
        TRUE            ~ ""
      )
    )
}

df_ocup_p1        <- preparar_df_figura(modelos_ocupacion_p1,        "Solo primer perfil")
df_ocup_homogenea <- preparar_df_figura(modelos_ocupacion_homogenea, "Combinación homogénea")
df_ocup_combo     <- preparar_df_figura(modelos_ocupacion_combo,     "Ocupación × Combo")


# ============================================================
# FIGURAS
# ============================================================

colores <- c(
  "Género"             = "#e41a1c",
  "Estado civil"       = "#377eb8",
  "Estado parental"    = "#4daf4a",
  "Plan reproductivo"  = "#ff7f00",
  "Ocupación"          = "#a65628",
  "Interacción combo"  = "#984ea3",
  "Controles perfil"   = "#808080",
  "Controles diseño"   = "#cccccc"
)

graficar <- function(datos, titulo, prefijo_archivo) {
  
  grupos <- unique(datos$grupo_outcome)
  
  walk(grupos, function(grupo) {
    
    datos_grupo <- datos %>%
      filter(grupo_outcome == grupo) %>%
      mutate(
        term_grupo  = factor(term_grupo, levels = names(colores)),
        term_label  = factor(term_label,
                             levels = unique(term_label[order(term_grupo)]))
      )
    
    n_terms    <- n_distinct(datos_grupo$term_label)
    n_outcomes <- n_distinct(datos_grupo$outcome_label)
    n_cols     <- 2
    n_rows     <- ceiling(n_outcomes / n_cols)
    alto       <- n_rows * (n_terms * 0.23 + 1.5)
    ancho      <- 7 * n_cols
    
    p <- ggplot(datos_grupo,
                aes(x = estimate, y = term_label, color = term_grupo)) +
      geom_vline(xintercept = 0, linetype = "dashed",
                 color = "gray50", linewidth = 0.5) +
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                     height = 0.25, linewidth = 0.7, alpha = 0.8) +
      geom_point(size = 3) +
      geom_text(data = datos_grupo %>% filter(p.value < 0.1),
                aes(label = sig, x = conf.high + 0.05),
                size = 4, fontface = "bold", color = "black") +
      facet_wrap(~ outcome_label, ncol = n_cols, scales = "free_x") +
      scale_color_manual(
        values = colores,
        breaks = levels(datos_grupo$term_grupo)[
          levels(datos_grupo$term_grupo) %in% unique(datos_grupo$term_grupo)
        ]
      ) +
      labs(
        title    = paste0(titulo, ": ", grupo),
        subtitle = "Coeficientes OLS clusterizados (IC 95%)\nRef: Hombre, Soltero/a, Sin hijos, Sin planes, Analista contable",
        x        = "Coeficiente estimado",
        y        = NULL,
        color    = "Tipo de variable",
        caption  = ". p < 0.1   * p < 0.05   ** p < 0.01   *** p < 0.001"
      ) +
      theme_bw(base_size = 12) +
      theme(
        strip.background = element_rect(fill = "#f0f0f0"),
        strip.text       = element_text(face = "bold", size = 10),
        legend.position  = "bottom",
        legend.title     = element_text(face = "bold"),
        plot.title       = element_text(face = "bold", size = 13),
        plot.subtitle    = element_text(size = 9, color = "gray40"),
        axis.text.y      = element_text(size = 10),
        panel.spacing    = unit(1.2, "cm")
      )
    
    nombre_fig <- here(paste0(
      "figuras/", prefijo_archivo, "_",
      tolower(str_replace_all(grupo, " ", "_")),
      ".png"
    ))
    
    ggsave(nombre_fig, plot = p,
           width = ancho, height = alto, dpi = 150, bg = "white")
    cat("✓ Figura guardada:", nombre_fig, "\n")
  })
}

# efecto de ocupación solo en primer perfil
graficar(df_ocup_p1,        "Ocupación — solo primer perfil", "ocup_primerperfil")

# efecto de ocupación en combinación homogénea
graficar(df_ocup_homogenea, "Ocupación — combinación homogénea", "ocup_homogenea")

#  ocupación × tipo de combinación
graficar(df_ocup_combo,     "Ocupación × tipo de combinación", "ocup_combo")


# ============================================================
# TABLA RESUMEN: comparación directa de los tres modelos
# ============================================================

resumen_ocupacion <- bind_rows(
  df_ocup_p1,
  df_ocup_homogenea,
  df_ocup_combo
) %>%
  filter(term == "perfil_ocupacionasociado/a financiero/a") %>%
  select(modelo, grupo_outcome, outcome_label, estimate, std.error,
         conf.low, conf.high, p.value, sig) %>%
  arrange(grupo_outcome, outcome_label, modelo)

cat("\n=== Comparación del efecto de ocupación entre los tres modelos ===\n")
print(as.data.frame(resumen_ocupacion))

write_csv(resumen_ocupacion,
          here("tablas/resumen_comparacion_ocupacion.csv"))

cat("\n✓ Proceso completado.\n")
cat("   Tablas en:  tablas/\n")
cat("   Figuras en: figuras/\n")


