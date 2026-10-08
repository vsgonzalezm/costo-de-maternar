# ============================================================================
# 06 - Regression model figures
# 1st version: Isi
# Modified: Tania Hutt 
# Date: agosto 2026
# Purpose: Create figures from regression models in 04.
# ============================================================================


library(tidyverse)
library(here)
library(broom)
# Necesario para que broom::tidy() reconozca objetos lm_robust (registra
# tidy.lm_robust via S3); sin este load, tidy() cae a tidy.default() y falla.
library(estimatr)

project_dir <- if (dir.exists(here("Encuesta Demanda", "Data"))) {
  here("Encuesta Demanda")
} else {
  here()
}

models_dir  <- file.path(project_dir, "Outputs", "models")
figures_dir <- file.path(project_dir, "Outputs", "figures")

# Outcome labels y groupings (mismos nombres que 04_analysis_cluster_robust_models.R /
# 05_Tables.R: base_demanda_analysis_ready.csv ya renombrada, no los ans_p* crudos)
outcome_labels <- c(
  "recommendation_probability" = "Hiring recommendation",
  "ambitious_score" = "Ambitious", "trustworthy_score" = "Trustworthy",
  "competent_score" = "Competent", "risky_continuity_score" = "Risky (job continuity)",
  "salary_costly_score" = "Costly (salary)", "operational_costly_score" = "Costly (operational)",
  "long_term_commitment_score" = "Committed long-term", "promising_score" = "Promising",
  "good_investment_score" = "Good investment",
  "expected_late_absence" = "Expected: late/absent", "expected_benefits_use" = "Expected: use benefits",
  "expected_accommodations" = "Expected: request accommodations", "expected_quit_work" = "Expected: quit",
  "expected_new_challenges" = "Expected: take on new challenges", "expected_promotion" = "Expected: be promoted"
)

outcome_groups <- list(
  hiring     = "recommendation_probability",
  evaluation = c(
    "ambitious_score", "trustworthy_score", "competent_score",
    "risky_continuity_score", "salary_costly_score", "operational_costly_score",
    "long_term_commitment_score", "promising_score", "good_investment_score"
  ),
  behavior = c(
    "expected_late_absence", "expected_benefits_use", "expected_accommodations",
    "expected_quit_work", "expected_new_challenges", "expected_promotion"
  )
)

#DICCIONARIO DE TERMINOS BASE
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

#Rlabel/group para CUALQUIER termino
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

#EXTRAER TIDY() 
extract_tidy_all <- function(models_list) {
  map_dfr(names(models_list), function(outcome_name) {
    model <- models_list[[outcome_name]]
    if (is.null(model)) return(NULL)
    broom::tidy(model, conf.int = TRUE) %>%
      mutate(outcome = outcome_name)
  })
}

#GRAFICAR UN GRUPO DE OUTCOMES 
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

#SETS POR CONDICION (SET1-SET4b)

#SET1 -- sin interaccion, un grafico por condicion x grupo
out_dir_set1 <- file.path(figures_dir, "set1_models")
dir.create(out_dir_set1, recursive = TRUE, showWarnings = FALSE)

set1_models_prof <- readRDS(file.path(models_dir, "set1_models_prof.rds"))
set1_models_tec  <- readRDS(file.path(models_dir, "set1_models_tec.rds"))

condiciones_set1 <- list(profesional = set1_models_prof, tecnico = set1_models_tec)

iwalk(condiciones_set1, function(models_cond, nombre_cond) {
  tidy_all <- extract_tidy_all(models_cond)
  iwalk(outcome_groups, function(outcomes_vec, group_name) {
    plot_coefficients(tidy_all, outcomes_vec, outcome_labels, "SET1 (main effects)",
                       "SET1", nombre_cond, group_name, out_dir_set1, modo = "todos")
  })
})

cat("Figuras listas para set1\n")

#SET2a, SET2b, SET3 -- interaccion doble, un grafico por condicion x grupo
sets_interaccion_doble <- tribble(
  ~set_name, ~set_label,
  "set2a",   "SET2a (Gender × Parental status)",
  "set2b",   "SET2b (Gender × Family plans)",
  "set3",    "SET3 (Gender × Marital status)"
)

for (i in seq_len(nrow(sets_interaccion_doble))) {
  set_name  <- sets_interaccion_doble$set_name[i]
  set_label <- sets_interaccion_doble$set_label[i]

  out_dir_set <- file.path(figures_dir, paste0(set_name, "_models"))
  dir.create(out_dir_set, recursive = TRUE, showWarnings = FALSE)

  models_prof <- readRDS(file.path(models_dir, paste0(set_name, "_models_prof.rds")))
  models_tec  <- readRDS(file.path(models_dir, paste0(set_name, "_models_tec.rds")))

  condiciones <- list(profesional = models_prof, tecnico = models_tec)

  iwalk(condiciones, function(models_cond, nombre_cond) {
    tidy_all <- extract_tidy_all(models_cond)
    iwalk(outcome_groups, function(outcomes_vec, group_name) {
      plot_coefficients(tidy_all, outcomes_vec, outcome_labels, set_label,
                         toupper(set_name), nombre_cond, group_name,
                         out_dir_set, modo = "todos")
    })
  })

  cat("Figuras listas para", set_name, "\n")
}

#SET4a, SET4b -- interaccion triple, dos graficos por condicion x grupo
#(principales / interacciones separados, igual que en el piloto)
sets_interaccion_triple <- tribble(
  ~set_name, ~set_label,
  "set4a",   "SET4a (Gender × Marital status × Parental status)",
  "set4b",   "SET4b (Gender × Parental status × Family plans)"
)

for (i in seq_len(nrow(sets_interaccion_triple))) {
  set_name  <- sets_interaccion_triple$set_name[i]
  set_label <- sets_interaccion_triple$set_label[i]

  out_dir_set <- file.path(figures_dir, paste0(set_name, "_models"))
  dir.create(out_dir_set, recursive = TRUE, showWarnings = FALSE)

  models_prof <- readRDS(file.path(models_dir, paste0(set_name, "_models_prof.rds")))
  models_tec  <- readRDS(file.path(models_dir, paste0(set_name, "_models_tec.rds")))

  condiciones <- list(profesional = models_prof, tecnico = models_tec)

  iwalk(condiciones, function(models_cond, nombre_cond) {
    tidy_all <- extract_tidy_all(models_cond)
    iwalk(outcome_groups, function(outcomes_vec, group_name) {
      plot_coefficients(tidy_all, outcomes_vec, outcome_labels, set_label,
                         toupper(set_name), nombre_cond, group_name,
                         out_dir_set, modo = "principales")
      plot_coefficients(tidy_all, outcomes_vec, outcome_labels, set_label,
                         toupper(set_name), nombre_cond, group_name,
                         out_dir_set, modo = "interacciones")
    })
  })

  cat("Figuras listas para", set_name, "\n")
}

#SETS 5 (SET5-SET5e)

#SET5 
out_dir_set5 <- file.path(figures_dir, "set5_models")
dir.create(out_dir_set5, recursive = TRUE, showWarnings = FALSE)

set5_models <- readRDS(file.path(models_dir, "set5_models.rds"))
tidy_set5 <- extract_tidy_all(set5_models)

iwalk(outcome_groups, function(outcomes_vec, group_name) {
  plot_coefficients(tidy_set5, outcomes_vec, outcome_labels,
                     "SET5 (main effects, pooled)", "SET5", "pooled",
                     group_name, out_dir_set5, modo = "todos")
})

cat("Figuras listas para set5\n")

#SET5a, SET5b, SET5c 
sets_pooled_doble <- tribble(
  ~set_name, ~set_label,
  "set5a",   "SET5a (Occupation × Gender)",
  "set5b",   "SET5b (Occupation × Parental status)",
  "set5c",   "SET5c (Occupation × Family plans)"
)

for (i in seq_len(nrow(sets_pooled_doble))) {
  set_name  <- sets_pooled_doble$set_name[i]
  set_label <- sets_pooled_doble$set_label[i]

  out_dir_set <- file.path(figures_dir, paste0(set_name, "_models"))
  dir.create(out_dir_set, recursive = TRUE, showWarnings = FALSE)

  models_pooled <- readRDS(file.path(models_dir, paste0(set_name, "_models.rds")))
  tidy_all <- extract_tidy_all(models_pooled)

  iwalk(outcome_groups, function(outcomes_vec, group_name) {
    plot_coefficients(tidy_all, outcomes_vec, outcome_labels, set_label,
                       toupper(set_name), "pooled", group_name,
                       out_dir_set, modo = "todos")
  })

  cat("Figuras listas para", set_name, "\n")
}

#SET5d, SET5e 
sets_pooled_triple <- tribble(
  ~set_name, ~set_label,
  "set5d",   "SET5d (Occupation × Gender × Parental status)",
  "set5e",   "SET5e (Occupation × Gender × Family plans)"
)

for (i in seq_len(nrow(sets_pooled_triple))) {
  set_name  <- sets_pooled_triple$set_name[i]
  set_label <- sets_pooled_triple$set_label[i]

  out_dir_set <- file.path(figures_dir, paste0(set_name, "_models"))
  dir.create(out_dir_set, recursive = TRUE, showWarnings = FALSE)

  models_pooled <- readRDS(file.path(models_dir, paste0(set_name, "_models.rds")))
  tidy_all <- extract_tidy_all(models_pooled)

  iwalk(outcome_groups, function(outcomes_vec, group_name) {
    plot_coefficients(tidy_all, outcomes_vec, outcome_labels, set_label,
                       toupper(set_name), "pooled", group_name,
                       out_dir_set, modo = "principales")
    plot_coefficients(tidy_all, outcomes_vec, outcome_labels, set_label,
                       toupper(set_name), "pooled", group_name,
                       out_dir_set, modo = "interacciones")
  })

  cat("Figuras listas para", set_name, "\n")
}