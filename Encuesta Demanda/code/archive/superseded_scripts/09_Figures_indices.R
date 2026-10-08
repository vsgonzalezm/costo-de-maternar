# ============================================================================
# 09 - Figures for composite-index outcomes (coefficient plots + predicted values)
# Author: valentinagonzalez
# Modified: Tania Hutt - arreglo de errores, deps faltantes, consistencia de rutas
# Date: agosto 2026
# Purpose: Coefficient plots (section A) and marginal predicted-value plots
#          (section B) for the index-outcome regression models in 07_Indices.R.
# ============================================================================

library(tidyverse)
library(here)
library(broom)
# Necesario para que broom::tidy()/marginaleffects reconozcan objetos lm_robust
# (registran metodos S3 al cargarse); sin este load, tidy() cae a tidy.default() y falla.
library(estimatr)
library(marginaleffects)

project_dir <- if (dir.exists(here("Encuesta Demanda", "Data"))) {
  here("Encuesta Demanda")
} else {
  here()
}

models_dir  <- file.path(project_dir, "Outputs", "models")
figures_dir <- file.path(project_dir, "Outputs", "figures")

#OUTCOME LABELS Y GRUPOS PARA INDICES (mismos nombres que 07_Indices.R;
#repetidos aqui para que este script pueda correr de forma independiente)
outcome_labels_indices <- c(
  "recommendation_probability" = "Hiring recommendation",
  "idx_riesgo"      = "Perceived risk (index)",
  "idx_crecimiento" = "Growth potential (index)",
  "idx_competencia" = "Competence (index)",
  "salary_costly_score"      = "Costly (salary)",
  "operational_costly_score" = "Costly (operational)",
  "expected_benefits_use"    = "Expected: use benefits",
  "expected_accommodations"  = "Expected: request accommodations",
  "good_investment_score"    = "Good investment"
)

outcome_groups_indices <- list(
  hiring    = "recommendation_probability",
  indices   = c("idx_riesgo", "idx_crecimiento", "idx_competencia"),
  costo     = c("salary_costly_score", "operational_costly_score", "expected_benefits_use", "expected_accommodations"),
  inversion = "good_investment_score"
)

#DICCIONARIO DE TERMINOS
term_dictionary <- tribble(
  ~term,                             ~term_label,               ~term_group,
  "perfil_generoMujer",              "Gender: Woman",           "Gender",
  "perfil_estado_civilcasado/a",     "Marital status: Married", "Marital status",
  "perfil_parentaltiene un hijo",    "Parental: 1 child",       "Parental status",
  "perfil_parentaltiene dos hijos",  "Parental: 2 children",    "Parental status",
  # Sin tilde: perfil_plan_futuro pasa por normalizar_texto() en 01_..., que convierte
  # "años" -> "anos"; el nombre del coeficiente sale sin tilde (ver revision de 04_...).
  "perfil_plan_futuroen 2 anos",     "Family plans: 2 years",   "Family plans",
  "perfil_plan_futuroen 5 anos",     "Family plans: 5 years",   "Family plans",
  "perfil_exp7 años",                "Experience: 7 years",     "Design controls",
  "perfil_exp8 años",                "Experience: 8 years",     "Design controls",
  "perfil_desempeno6",               "Performance: 6.0",        "Design controls",
  "perfil_desempeno6.3",             "Performance: 6.3",        "Design controls",
  "pantalla_orden2",                 "Order: 2nd profile shown","Presentation controls",
  "template_contextoB",              "Template: B",             "Presentation controls",
  "condicion_ocupacionaltecnico",    "Occupation: Technical",   "Occupational level"
)

colores_grupos <- c(
  "Gender"                  = "#e41a1c",
  "Marital status"          = "#377eb8",
  "Parental status"         = "#4daf4a",
  "Family plans"            = "#ff7f00",
  "Occupational level"      = "#a6d854",
  "Design controls"         = "#808080",
  "Presentation controls"   = "#cccccc",
  "Interaction: Gender ×"   = "#a65628",
  "Interaction: Other"      = "#984ea3",
  "Triple interaction"      = "#f781bf"
)

resolve_terms <- function(term_vec) {
  map_dfr(unique(term_vec), function(term) {
    parts <- str_split(term, ":")[[1]]

    part_info <- map(parts, function(p) {
      m <- term_dictionary %>% filter(term == p)
      if (nrow(m) == 1) list(label = m$term_label, group = m$term_group)
      else list(label = p, group = "Other")
    })

    label <- paste(map_chr(part_info, "label"), collapse = " × ")
    groups <- map_chr(part_info, "group")

    if (length(parts) == 1) {
      group <- groups[1]
    } else if (length(parts) >= 3) {
      group <- "Triple interaction"
    } else if ("Gender" %in% groups) {
      group <- "Interaction: Gender ×"
    } else {
      group <- "Interaction: Other"
    }

    tibble(term = term, term_label = label, term_group = group)
  })
}

extract_tidy_all <- function(models_list) {
  map_dfr(names(models_list), function(outcome_name) {
    model <- models_list[[outcome_name]]
    if (is.null(model)) return(NULL)
    broom::tidy(model, conf.int = TRUE) %>%
      mutate(outcome = outcome_name)
  })
}

plot_coefficients <- function(tidy_all, outcomes, labels, set_label,
                               file_prefix, condicion_label, group_name,
                               out_dir, modo = "todos") {

  datos_viz <- tidy_all %>%
    filter(outcome %in% outcomes, term != "(Intercept)")

  if (modo == "principales") datos_viz <- datos_viz %>% filter(!str_detect(term, ":"))
  if (modo == "interacciones") datos_viz <- datos_viz %>% filter(str_detect(term, ":"))

  term_info <- resolve_terms(datos_viz$term)

  datos_viz <- datos_viz %>%
    left_join(term_info, by = "term") %>%
    mutate(
      outcome_label = factor(labels[outcome], levels = unname(labels[outcomes])),
      sig = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        p.value < 0.1   ~ ".",
        TRUE ~ ""
      )
    )

  n_terms <- n_distinct(datos_viz$term_label)
  alto <- max(6, n_terms * 0.4)

  sufijo_titulo <- if (modo == "principales") " (main effects)"
                    else if (modo == "interacciones") " (interactions)"
                    else ""
  sufijo_archivo <- if (modo == "principales") "_principales"
                      else if (modo == "interacciones") "_interacciones"
                      else ""

  p <- ggplot(datos_viz, aes(x = estimate, y = term_label, color = term_group)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                  orientation = "y", width = 0.2, linewidth = 0.7, alpha = 0.8) +
    geom_point(size = 3) +
    geom_text(data = datos_viz %>% filter(p.value < 0.1),
              aes(label = sig, x = conf.high + 0.02),
              size = 4, fontface = "bold", color = "black") +
    facet_wrap(~ outcome_label, ncol = 2, scales = "free_x") +
    scale_color_manual(values = colores_grupos) +
    labs(
      title    = paste0(set_label, ": ", str_to_title(group_name),
                        " — ", condicion_label, sufijo_titulo),
      x       = "Estimated coefficient (95% CI)",
      y       = NULL,
      color   = "Variable type",
      caption = ". p<0.1   * p<0.05   ** p<0.01   *** p<0.001"
    ) +
    theme_bw(base_size = 11) +
    theme(
      strip.background = element_rect(fill = "#f0f0f0"),
      strip.text       = element_text(face = "bold", size = 9),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold"),
      plot.title       = element_text(face = "bold", size = 13),
      axis.text.y      = element_text(size = 9)
    )

  n_facets <- n_distinct(datos_viz$outcome_label)
  filename <- file.path(
    out_dir,
    paste0(file_prefix, "_", condicion_label, "_", group_name, sufijo_archivo, "_coefficients.png")
  )

  ggsave(filename, p, width = 11, height = ceiling(n_facets / 2) * alto / 2 + 2,
         dpi = 150, bg = "white", limitsize = FALSE)
  cat("  done:", filename, "\n")
}

#SETS POR CONDICION

#SET1 -- sin interaccion
out_dir_set1 <- file.path(figures_dir, "set1_idx_models")
dir.create(out_dir_set1, recursive = TRUE, showWarnings = FALSE)

set1_idx_models_prof <- readRDS(file.path(models_dir, "set1_idx_models_prof.rds"))
set1_idx_models_tec  <- readRDS(file.path(models_dir, "set1_idx_models_tec.rds"))

condiciones_set1 <- list(profesional = set1_idx_models_prof, tecnico = set1_idx_models_tec)

iwalk(condiciones_set1, function(models_cond, nombre_cond) {
  tidy_all <- extract_tidy_all(models_cond)
  iwalk(outcome_groups_indices, function(outcomes_vec, group_name) {
    plot_coefficients(tidy_all, outcomes_vec, outcome_labels_indices, "SET1 (main effects, index outcomes)",
                       "SET1_IDX", nombre_cond, group_name, out_dir_set1, modo = "todos")
  })
})

cat("Figuras (índices) listas para set1\n")

#SET2a, SET2b, SET3 -- interaccion doble
sets_interaccion_doble <- tribble(
  ~set_name, ~set_label,
  "set2a",   "SET2a (Gender × Parental status, index outcomes)",
  "set2b",   "SET2b (Gender × Family plans, index outcomes)",
  "set3",    "SET3 (Gender × Marital status, index outcomes)"
)

for (i in seq_len(nrow(sets_interaccion_doble))) {
  set_name  <- sets_interaccion_doble$set_name[i]
  set_label <- sets_interaccion_doble$set_label[i]

  out_dir_set <- file.path(figures_dir, paste0(set_name, "_idx_models"))
  dir.create(out_dir_set, recursive = TRUE, showWarnings = FALSE)

  models_prof <- readRDS(file.path(models_dir, paste0(set_name, "_idx_models_prof.rds")))
  models_tec  <- readRDS(file.path(models_dir, paste0(set_name, "_idx_models_tec.rds")))

  condiciones <- list(profesional = models_prof, tecnico = models_tec)

  iwalk(condiciones, function(models_cond, nombre_cond) {
    tidy_all <- extract_tidy_all(models_cond)
    iwalk(outcome_groups_indices, function(outcomes_vec, group_name) {
      plot_coefficients(tidy_all, outcomes_vec, outcome_labels_indices, set_label,
                         paste0(toupper(set_name), "_IDX"), nombre_cond, group_name,
                         out_dir_set, modo = "todos")
    })
  })

  cat("Figuras (índices) listas para", set_name, "\n")
}

#SET4a, SET4b -- interaccion triple
sets_interaccion_triple <- tribble(
  ~set_name, ~set_label,
  "set4a",   "SET4a (Gender × Marital status × Parental status, index outcomes)",
  "set4b",   "SET4b (Gender × Parental status × Family plans, index outcomes)"
)

for (i in seq_len(nrow(sets_interaccion_triple))) {
  set_name  <- sets_interaccion_triple$set_name[i]
  set_label <- sets_interaccion_triple$set_label[i]

  out_dir_set <- file.path(figures_dir, paste0(set_name, "_idx_models"))
  dir.create(out_dir_set, recursive = TRUE, showWarnings = FALSE)

  models_prof <- readRDS(file.path(models_dir, paste0(set_name, "_idx_models_prof.rds")))
  models_tec  <- readRDS(file.path(models_dir, paste0(set_name, "_idx_models_tec.rds")))

  condiciones <- list(profesional = models_prof, tecnico = models_tec)

  iwalk(condiciones, function(models_cond, nombre_cond) {
    tidy_all <- extract_tidy_all(models_cond)
    iwalk(outcome_groups_indices, function(outcomes_vec, group_name) {
      plot_coefficients(tidy_all, outcomes_vec, outcome_labels_indices, set_label,
                         paste0(toupper(set_name), "_IDX"), nombre_cond, group_name,
                         out_dir_set, modo = "principales")
      plot_coefficients(tidy_all, outcomes_vec, outcome_labels_indices, set_label,
                         paste0(toupper(set_name), "_IDX"), nombre_cond, group_name,
                         out_dir_set, modo = "interacciones")
    })
  })

  cat("Figuras (índices) listas para", set_name, "\n")
}

#SETS POOLED (SET5)

out_dir_set5 <- file.path(figures_dir, "set5_idx_models")
dir.create(out_dir_set5, recursive = TRUE, showWarnings = FALSE)

set5_idx_models <- readRDS(file.path(models_dir, "set5_idx_models.rds"))
tidy_set5 <- extract_tidy_all(set5_idx_models)

iwalk(outcome_groups_indices, function(outcomes_vec, group_name) {
  plot_coefficients(tidy_set5, outcomes_vec, outcome_labels_indices,
                     "SET5 (main effects, pooled, index outcomes)", "SET5_IDX", "pooled",
                     group_name, out_dir_set5, modo = "todos")
})

cat("Figuras (índices) listas para set5\n")

sets_pooled_doble <- tribble(
  ~set_name, ~set_label,
  "set5a",   "SET5a (Occupation × Gender, index outcomes)",
  "set5b",   "SET5b (Occupation × Parental status, index outcomes)",
  "set5c",   "SET5c (Occupation × Family plans, index outcomes)"
)

for (i in seq_len(nrow(sets_pooled_doble))) {
  set_name  <- sets_pooled_doble$set_name[i]
  set_label <- sets_pooled_doble$set_label[i]

  out_dir_set <- file.path(figures_dir, paste0(set_name, "_idx_models"))
  dir.create(out_dir_set, recursive = TRUE, showWarnings = FALSE)

  models_pooled <- readRDS(file.path(models_dir, paste0(set_name, "_idx_models.rds")))
  tidy_all <- extract_tidy_all(models_pooled)

  iwalk(outcome_groups_indices, function(outcomes_vec, group_name) {
    plot_coefficients(tidy_all, outcomes_vec, outcome_labels_indices, set_label,
                       paste0(toupper(set_name), "_IDX"), "pooled", group_name,
                       out_dir_set, modo = "todos")
  })

  cat("Figuras (índices) listas para", set_name, "\n")
}

sets_pooled_triple <- tribble(
  ~set_name, ~set_label,
  "set5d",   "SET5d (Occupation × Gender × Parental status, index outcomes)",
  "set5e",   "SET5e (Occupation × Gender × Family plans, index outcomes)"
)

for (i in seq_len(nrow(sets_pooled_triple))) {
  set_name  <- sets_pooled_triple$set_name[i]
  set_label <- sets_pooled_triple$set_label[i]

  out_dir_set <- file.path(figures_dir, paste0(set_name, "_idx_models"))
  dir.create(out_dir_set, recursive = TRUE, showWarnings = FALSE)

  models_pooled <- readRDS(file.path(models_dir, paste0(set_name, "_idx_models.rds")))
  tidy_all <- extract_tidy_all(models_pooled)

  iwalk(outcome_groups_indices, function(outcomes_vec, group_name) {
    plot_coefficients(tidy_all, outcomes_vec, outcome_labels_indices, set_label,
                       paste0(toupper(set_name), "_IDX"), "pooled", group_name,
                       out_dir_set, modo = "principales")
    plot_coefficients(tidy_all, outcomes_vec, outcome_labels_indices, set_label,
                       paste0(toupper(set_name), "_IDX"), "pooled", group_name,
                       out_dir_set, modo = "interacciones")
  })

  cat("Figuras (índices) listas para", set_name, "\n")
}

# ============================================================================
# SECTION B - Predicted values (marginal effects on the original 1-7 scale)
# ============================================================================
# Nota: models_dir/figures_dir, y las librerias, ya quedaron definidos arriba
# (antes esta seccion los redefinia de nuevo con here("outputs", ...); se elimino
# esa duplicacion).

#OUTCOMES A GRAFICAR: indices + contratacion (los mas interpretables en escala real)
outcomes_predichos <- c("recommendation_probability", "idx_riesgo", "idx_crecimiento", "idx_competencia")
# Subconjunto de outcome_labels_indices (sin los items de costo) -- nombre distinto
# para no pisar la version completa usada en la Seccion A.
outcome_labels_predichos <- outcome_labels_indices[outcomes_predichos]

#NIVELES DE CADA VARIABLE (mismos que en sum contrasts de 07_Indices.R)
niveles_vars <- list(
  perfil_genero          = c("Hombre", "Mujer"),
  perfil_estado_civil    = c("soltero/a", "casado/a"),
  perfil_parental        = c("actualmente no tiene hijos", "tiene un hijo", "tiene dos hijos"),
  # Sin tilde (ver nota de term_dictionary mas arriba).
  perfil_plan_futuro     = c("no quiere", "en 2 anos", "en 5 anos"),
  condicion_ocupacional  = c("profesional", "tecnico")
)

etiquetas_niveles <- c(
  "Hombre" = "Man", "Mujer" = "Woman",
  "soltero/a" = "Single", "casado/a" = "Married",
  "actualmente no tiene hijos" = "No children", "tiene un hijo" = "1 child", "tiene dos hijos" = "2 children",
  "no quiere" = "No plans", "en 2 anos" = "In 2 years", "en 5 anos" = "In 5 years",
  "profesional" = "Professional", "tecnico" = "Technical"
)

etiquetas_variables <- c(
  perfil_genero = "Gender", perfil_estado_civil = "Marital status",
  perfil_parental = "Parental status", perfil_plan_futuro = "Family plans",
  condicion_ocupacional = "Occupational level"
)

#CALCULAR VALORES PREDICHOS PARA UNA COMBINACION DE 2 VARIABLES, EN UN OUTCOME
predict_grid <- function(model, x_var, color_var) {
  grid_args <- list(model = model)
  grid_args[[x_var]] <- niveles_vars[[x_var]]
  grid_args[[color_var]] <- niveles_vars[[color_var]]
  newdata_grid <- do.call(marginaleffects::datagrid, grid_args)
  marginaleffects::predictions(model, newdata = newdata_grid)
}

#GRAFICAR VALORES PREDICHOS PARA UN SET (varios outcomes, facetado)
plot_predicted_values <- function(models_list, outcomes, labels, x_var, color_var,
                                   set_label, file_prefix, condicion_label, out_dir) {

  pred_all <- map_dfr(outcomes, function(outcome_name) {
    model <- models_list[[outcome_name]]
    if (is.null(model)) return(NULL)
    predict_grid(model, x_var, color_var) %>%
      mutate(outcome = outcome_name)
  })

  if (nrow(pred_all) == 0) return(invisible(NULL))

  pred_all <- pred_all %>%
    mutate(
      outcome_label = factor(labels[outcome], levels = unname(labels[outcomes])),
      x_label = factor(etiquetas_niveles[as.character(.data[[x_var]])], levels = unname(etiquetas_niveles[niveles_vars[[x_var]]])),
      color_label = factor(etiquetas_niveles[as.character(.data[[color_var]])], levels = unname(etiquetas_niveles[niveles_vars[[color_var]]]))
    )

  p <- ggplot(pred_all, aes(x = x_label, y = estimate, color = color_label, group = color_label)) +
    geom_line(linewidth = 0.8, position = position_dodge(width = 0.15)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width = 0.15, linewidth = 0.6, position = position_dodge(width = 0.15)) +
    geom_point(size = 3, position = position_dodge(width = 0.15)) +
    facet_wrap(~ outcome_label, scales = "free_y") +
    scale_color_brewer(palette = "Set1") +
    labs(
      title = paste0(set_label, " — predicted values (", condicion_label, ")"),
      x     = etiquetas_variables[[x_var]],
      y     = "Predicted value (1-7 scale)",
      color = etiquetas_variables[[color_var]]
    ) +
    theme_bw(base_size = 11) +
    theme(
      strip.background = element_rect(fill = "#f0f0f0"),
      strip.text       = element_text(face = "bold", size = 9),
      legend.position  = "bottom",
      plot.title       = element_text(face = "bold", size = 12)
    )

  filename <- file.path(out_dir, paste0(file_prefix, "_", condicion_label, "_predicted.png"))
  ggsave(filename, p, width = 9, height = 6.5, dpi = 150, bg = "white")
  cat("  done:", filename, "\n")
}

#SETS CON INTERACCION DOBLE, POR CONDICION (SET2a, SET2b, SET3)
sets_doble_condicion <- tribble(
  ~set_name, ~set_label,              ~x_var,               ~color_var,
  "set2a",   "SET2a (Parental × Gender)", "perfil_parental",    "perfil_genero",
  "set2b",   "SET2b (Family plans × Gender)", "perfil_plan_futuro", "perfil_genero",
  "set3",    "SET3 (Marital status × Gender)", "perfil_estado_civil", "perfil_genero"
)

for (i in seq_len(nrow(sets_doble_condicion))) {
  set_name  <- sets_doble_condicion$set_name[i]
  set_label <- sets_doble_condicion$set_label[i]
  x_var     <- sets_doble_condicion$x_var[i]
  color_var <- sets_doble_condicion$color_var[i]

  out_dir_set <- file.path(figures_dir, paste0(set_name, "_idx_predicted"))
  dir.create(out_dir_set, recursive = TRUE, showWarnings = FALSE)

  models_prof <- readRDS(file.path(models_dir, paste0(set_name, "_idx_models_prof.rds")))
  models_tec  <- readRDS(file.path(models_dir, paste0(set_name, "_idx_models_tec.rds")))

  plot_predicted_values(models_prof, outcomes_predichos, outcome_labels_predichos,
                         x_var, color_var, set_label,
                         paste0(toupper(set_name), "_IDX"), "profesional", out_dir_set)
  plot_predicted_values(models_tec, outcomes_predichos, outcome_labels_predichos,
                         x_var, color_var, set_label,
                         paste0(toupper(set_name), "_IDX"), "tecnico", out_dir_set)

  cat("Predicted values listos para", set_name, "\n")
}

#SETS CON INTERACCION DOBLE, POOLED (SET5a, SET5b, SET5c)
sets_doble_pooled <- tribble(
  ~set_name, ~set_label,                          ~x_var,                    ~color_var,
  "set5a",   "SET5a (Gender × Occupation)",       "perfil_genero",           "condicion_ocupacional",
  "set5b",   "SET5b (Parental × Occupation)",     "perfil_parental",         "condicion_ocupacional",
  "set5c",   "SET5c (Family plans × Occupation)", "perfil_plan_futuro",      "condicion_ocupacional"
)

for (i in seq_len(nrow(sets_doble_pooled))) {
  set_name  <- sets_doble_pooled$set_name[i]
  set_label <- sets_doble_pooled$set_label[i]
  x_var     <- sets_doble_pooled$x_var[i]
  color_var <- sets_doble_pooled$color_var[i]

  out_dir_set <- file.path(figures_dir, paste0(set_name, "_idx_predicted"))
  dir.create(out_dir_set, recursive = TRUE, showWarnings = FALSE)

  models_pooled <- readRDS(file.path(models_dir, paste0(set_name, "_idx_models.rds")))

  plot_predicted_values(models_pooled, outcomes_predichos, outcome_labels_predichos,
                         x_var, color_var, set_label,
                         paste0(toupper(set_name), "_IDX"), "pooled", out_dir_set)

  cat("Predicted values listos para", set_name, "\n")
}
