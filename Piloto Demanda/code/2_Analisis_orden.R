library(tidyverse)
library(haven)
library(estimatr)
library(modelsummary)
library(broom)
library(ggplot2)
library(here)

df_raw <- read_sav(here("data/procesado/base_pretest_long_final.sav"))


# PREPARACIÓN DE VARIABLES

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


# DEFINICIÓN DE OUTCOMES Y ETIQUETAS

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


# CONTROLES

controles <- "perfil_genero + perfil_estado_civil + perfil_parental +
              perfil_exp + perfil_desempeno + template_contexto"

sets_formulas <- list(
  
  "orden_x_plan" = paste(
    "perfil_plan_futuro * pantalla_orden + perfil_ocupacion +",
    controles),
  
  "orden_x_ocupacion" = paste(
    "perfil_ocupacion * pantalla_orden + perfil_plan_futuro +",
    controles)
)

# MODELOS

correr_modelos <- function(formula_str, outcomes, datos) {
  map(outcomes, function(y) {
    fml <- as.formula(paste(y, "~", formula_str))
    lm_robust(fml,
              data     = datos,
              clusters = numericalId,
              se_type  = "CR2")
  }) %>% set_names(outcomes)
}

resultados_plan      <- correr_modelos(sets_formulas[["orden_x_plan"]],      all_outcomes, df)
resultados_ocupacion <- correr_modelos(sets_formulas[["orden_x_ocupacion"]], all_outcomes, df)

cat("✓ Todos los modelos corridos\n")


# TABLAS

exportar_tabla <- function(lista_modelos, outcomes, nombre_archivo) {
  modelos_sel        <- lista_modelos[outcomes]
  names(modelos_sel) <- labels_outcomes[outcomes]
  modelsummary(
    modelos_sel,
    stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    gof_map = c("nobs", "r.squared", "adj.r.squared"),
    output  = nombre_archivo
  )
}

# Orden × Plan de familia
exportar_tabla(resultados_plan, outcomes_p1,
               here("tablas/orden_plan_contratacion.docx"))
exportar_tabla(resultados_plan, outcomes_evaluacion,
               here("tablas/orden_plan_evaluaciones.docx"))
exportar_tabla(resultados_plan, outcomes_comportamiento,
               here("tablas/orden_plan_comportamiento.docx"))

# Orden × Ocupación
exportar_tabla(resultados_ocupacion, outcomes_p1,
               here("tablas/orden_ocup_contratacion.docx"))
exportar_tabla(resultados_ocupacion, outcomes_evaluacion,
               here("tablas/orden_ocup_evaluaciones.docx"))
exportar_tabla(resultados_ocupacion, outcomes_comportamiento,
               here("tablas/orden_ocup_comportamiento.docx"))

cat("✓ Todas las tablas exportadas\n")


#EXTRAER Y ETIQUETAR RESULTADOS


preparar_resultados <- function(lista_modelos, nombre_modelo) {
  map_dfr(all_outcomes, ~ tidy(lista_modelos[[.x]]) %>% mutate(outcome = .x)) %>%
    filter(!str_detect(term, "Intercept"), !is.na(estimate)) %>%
    mutate(
      modelo = nombre_modelo,
      
      grupo_outcome = case_when(
        outcome == "ans_p1_global"               ~ "Contratación",
        outcome %in% outcomes_evaluacion         ~ "Evaluación",
        outcome %in% outcomes_comportamiento     ~ "Comportamiento futuro"
      ),
      
      outcome_label = case_when(
        outcome == "ans_p1_global" ~ "Recomendación contratación",
        outcome == "ans_p2_1"      ~ "Confiable",
        outcome == "ans_p2_2"      ~ "Capaz",
        outcome == "ans_p2_3"      ~ "Comprometido/a largo plazo",
        outcome == "ans_p2_4"      ~ "Prometedor/a",
        outcome == "ans_p2_5"      ~ "Ambicioso/a",
        outcome == "ans_p2_6"      ~ "Riesgoso/a para empresa",
        outcome == "ans_p2_7"      ~ "Costoso/a financieramente",
        outcome == "ans_p3_1"      ~ "Uso de beneficios",
        outcome == "ans_p3_2"      ~ "Requerirá facilidades/permisos",
        outcome == "ans_p3_3"      ~ "Llegará tarde / se ausentará",
        outcome == "ans_p3_4"      ~ "Renunciará en próximos años",
        outcome == "ans_p3_5"      ~ "Tomará mayores responsabilidades",
        outcome == "ans_p3_6"      ~ "Será ascendido/a",
        outcome == "ans_p3_7"      ~ "Generará costos económicos",
        outcome == "ans_p3_8"      ~ "Generará costos operativos",
        TRUE ~ outcome
      ),
      
      term_label = case_when(
        term == "perfil_generoMujer"                                       ~ "Género: Mujer",
        term == "perfil_estado_civilcasado/a"                              ~ "Estado civil: Casado/a",
        term == "perfil_estado_civilconviviendo"                           ~ "Estado civil: Conviviendo",
        term == "perfil_parentaltiene un hijo"                             ~ "Parental: Un hijo",
        term == "perfil_parentaltiene dos hijos"                           ~ "Parental: Dos hijos",
        term == "perfil_plan_futuroen 2 años"                              ~ "Plan: en 2 años",
        term == "perfil_plan_futuroen 5 años"                              ~ "Plan: en 5 años",
        term == "perfil_ocupacionasociado/a financiero/a"                  ~ "Ocupación: asociado/a financiero/a",
        term == "perfil_exp6 años"                                         ~ "Experiencia: 6 años",
        term == "perfil_exp8 años"                                         ~ "Experiencia: 8 años",
        term == "perfil_desempeno6.0"                                      ~ "Desempeño: 6.0",
        term == "perfil_desempeno6.3"                                      ~ "Desempeño: 6.3",
        term == "pantalla_orden2"                                          ~ "Orden: 2do perfil",
        term == "template_contextoA"                                       ~ "Template: A",
        term == "perfil_plan_futuroen 2 años:pantalla_orden2"              ~ "Plan 2 años × 2do perfil",
        term == "perfil_plan_futuroen 5 años:pantalla_orden2"              ~ "Plan 5 años × 2do perfil",
        term == "perfil_ocupacionasociado/a financiero/a:pantalla_orden2"  ~ "Asociado/a financiero/a × 2do perfil",
        TRUE ~ term
      ),
      
      term_grupo = case_when(
        term == "perfil_generoMujer"                                       ~ "Género",
        term %in% c("perfil_estado_civilcasado/a",
                    "perfil_estado_civilconviviendo")                      ~ "Estado civil",
        term %in% c("perfil_parentaltiene un hijo",
                    "perfil_parentaltiene dos hijos")                      ~ "Estado parental",
        term %in% c("perfil_plan_futuroen 2 años",
                    "perfil_plan_futuroen 5 años")                         ~ "Plan reproductivo",
        term == "perfil_ocupacionasociado/a financiero/a"                  ~ "Ocupación",
        term %in% c("perfil_plan_futuroen 2 años:pantalla_orden2",
                    "perfil_plan_futuroen 5 años:pantalla_orden2",
                    "perfil_ocupacionasociado/a financiero/a:pantalla_orden2") ~ "Interacción con orden",
        term %in% c("perfil_exp6 años", "perfil_exp8 años",
                    "perfil_desempeno6.0", "perfil_desempeno6.3")          ~ "Controles perfil",
        term %in% c("pantalla_orden2", "template_contextoA",
                    "template_contextoB")                                  ~ "Controles diseño",
        TRUE                                                               ~ "Otros"
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

df_plan      <- preparar_resultados(resultados_plan,      "Orden × Plan familia")
df_ocupacion <- preparar_resultados(resultados_ocupacion, "Orden × Ocupación")


# FIGURAS

orden_labels <- c(
  "Género: Mujer",
  "Estado civil: Casado/a", "Estado civil: Conviviendo",
  "Parental: Un hijo", "Parental: Dos hijos",
  "Plan: en 2 años", "Plan: en 5 años",
  "Ocupación: asociado/a financiero/a",
  "Plan 2 años × 2do perfil", "Plan 5 años × 2do perfil",
  "Asociado/a financiero/a × 2do perfil",
  "Experiencia: 6 años", "Experiencia: 8 años",
  "Desempeño: 6.0", "Desempeño: 6.3",
  "Orden: 2do perfil", "Template: A", "Template: B"
)

graficar_grupo <- function(datos, grupo, titulo_modelo, prefijo_archivo) {
  
  datos_grupo <- datos %>%
    filter(grupo_outcome == grupo) %>%
    mutate(
      term_grupo = factor(term_grupo, levels = c(
        "Género", "Estado civil", "Estado parental", "Plan reproductivo",
        "Ocupación", "Interacción con orden",
        "Controles perfil", "Controles diseño"
      )),
      term_label = factor(term_label,
                          levels = orden_labels[orden_labels %in% unique(term_label)])
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
    geom_hline(
      data = datos_grupo %>%
        group_by(term_grupo) %>%
        summarise(y_max = max(as.numeric(term_label)), .groups = "drop") %>%
        filter(y_max < max(as.numeric(datos_grupo$term_label))),
      aes(yintercept = y_max + 0.5),
      color = "gray80", linewidth = 0.4, linetype = "dotted",
      inherit.aes = FALSE
    ) +
    facet_wrap(~ outcome_label, ncol = n_cols, scales = "free_x") +
    scale_color_manual(
      values = c(
        "Género"                = "#e41a1c",
        "Estado civil"          = "#377eb8",
        "Estado parental"       = "#4daf4a",
        "Plan reproductivo"     = "#ff7f00",
        "Ocupación"             = "#a65628",
        "Interacción con orden" = "#984ea3",
        "Controles perfil"      = "#808080",
        "Controles diseño"      = "#cccccc"
      ),
      breaks = levels(datos_grupo$term_grupo)[
        levels(datos_grupo$term_grupo) %in% unique(datos_grupo$term_grupo)
      ]
    ) +
    labs(
      title    = paste0(titulo_modelo, ": ", grupo),
      subtitle = "Coeficientes OLS clusterizados (IC 95%)\nRef: Hombre, Soltero/a, Sin hijos, Sin planes, Analista contable, 1er perfil",
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
  
  nombre_archivo <- here(paste0(
    "figuras/", prefijo_archivo, "_",
    tolower(str_replace_all(grupo, " ", "_")),
    ".png"
  ))
  
  ggsave(nombre_archivo, plot = p,
         width = ancho, height = alto, dpi = 150, bg = "white")
  cat("✓ Figura guardada:", nombre_archivo, "\n")
}


# GENERAR FIGS

grupos <- c("Contratación", "Evaluación", "Comportamiento futuro")

# Orden × Plan familia
walk(grupos, ~ graficar_grupo(df_plan, .x,
                              "Orden × Plan familia",
                              paste0("orden_plan_",
                                     tolower(str_replace_all(.x, " ", "_")))))

# Orden × Ocupación
walk(grupos, ~ graficar_grupo(df_ocupacion, .x,
                              "Orden × Ocupación",
                              paste0("orden_ocup_",
                                     tolower(str_replace_all(.x, " ", "_")))))

cat("\n✓ Proceso completado.\n")
cat("   Tablas en:  tablas/\n")
cat("   Figuras en: figuras/\n")
