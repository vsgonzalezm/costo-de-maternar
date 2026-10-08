# ============================================================================
# helpers_tables_figures.R - funciones compartidas por las tablas y figuras de
# coeficientes (scripts 08, 09, 16, 17). Se carga con source() DESPUES de
# 00_setup.R y de init_output(); todo archivo se escribe con out_path().
# ============================================================================
suppressPackageStartupMessages({ library(modelsummary); library(broom); library(marginaleffects) })

# Cuantos "*" tiene la formula: >= 2 => interaccion triple (figuras separadas)
is_triple_set <- function(formula_rhs) str_count(formula_rhs, fixed("*")) >= 2

# ---- TABLAS -----------------------------------------------------------------
# Convierte cada lm_robust en tidy/glance para que modelsummary no use su handler (con bug)
to_modelsummary_list <- function(model) {
  out <- list(tidy = broom::tidy(model, conf.int = TRUE), glance = broom::glance(model))
  class(out) <- "modelsummary_list"
  out
}

export_table <- function(models_list, outcomes, labels, filename) {
  selected <- models_list[outcomes]
  selected <- selected[!sapply(selected, is.null)]
  if (length(selected) == 0) return(invisible(NULL))
  names(selected) <- labels[names(selected)]
  modelsummary(
    lapply(selected, to_modelsummary_list),
    stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
    fmt = 3,
    notes = c(
      "OLS with cluster-robust standard errors (CR2, clustered by respondent).",
      "Sum contrasts: coefficients reflect deviation from the design mean, not a reference category.",
      "Controls for presentation order and template version."
    ),
    output = filename
  )
  cat("  done:", filename, "\n")
}

# ---- FIGURAS DE COEFICIENTES -------------------------------------------------
term_dictionary <- tribble(
  ~term,                             ~term_label,               ~term_group,
  "perfil_generoMujer",              "Gender: Woman",           "Gender",
  "perfil_estado_civilcasado/a",     "Marital status: Married", "Marital status",
  "perfil_parentaltiene un hijo",    "Parental: 1 child",       "Parental status",
  "perfil_parentaltiene dos hijos",  "Parental: 2 children",    "Parental status",
  # Sin tilde: normalizar_texto() en 01_... convierte "años" -> "anos"
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
  "Gender" = "#e41a1c", "Marital status" = "#377eb8", "Parental status" = "#4daf4a",
  "Family plans" = "#ff7f00", "Occupational level" = "#a6d854", "Design controls" = "#808080",
  "Presentation controls" = "#cccccc", "Interaction: Gender ×" = "#a65628",
  "Interaction: Other" = "#984ea3", "Triple interaction" = "#f781bf"
)

resolve_terms <- function(term_vec) {
  map_dfr(unique(term_vec), function(term) {
    parts <- str_split(term, ":")[[1]]
    part_info <- map(parts, function(p) {
      m <- term_dictionary %>% filter(term == p)
      if (nrow(m) == 1) list(label = m$term_label, group = m$term_group) else list(label = p, group = "Other")
    })
    label  <- paste(map_chr(part_info, "label"), collapse = " × ")
    groups <- map_chr(part_info, "group")
    group <- if (length(parts) == 1) groups[1]
             else if (length(parts) >= 3) "Triple interaction"
             else if ("Gender" %in% groups) "Interaction: Gender ×"
             else "Interaction: Other"
    tibble(term = term, term_label = label, term_group = group)
  })
}

extract_tidy_all <- function(models_list) {
  map_dfr(names(models_list), function(outcome_name) {
    model <- models_list[[outcome_name]]
    if (is.null(model)) return(NULL)
    broom::tidy(model, conf.int = TRUE) %>% mutate(outcome = outcome_name)
  })
}

# subdir: subcarpeta dentro de outputs/<script>/ ; file_prefix p.ej. "set2a_pooled"
plot_coefficients <- function(tidy_all, outcomes, labels, set_label, file_prefix,
                              condicion_label, group_name, subdir, modo = "todos") {
  datos_viz <- tidy_all %>% filter(outcome %in% outcomes, term != "(Intercept)")
  if (modo == "principales")   datos_viz <- datos_viz %>% filter(!str_detect(term, ":"))
  if (modo == "interacciones") datos_viz <- datos_viz %>% filter(str_detect(term, ":"))
  if (nrow(datos_viz) == 0) return(invisible(NULL))

  datos_viz <- datos_viz %>%
    left_join(resolve_terms(datos_viz$term), by = "term") %>%
    mutate(
      outcome_label = factor(labels[outcome], levels = unname(labels[outcomes])),
      sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**", p.value < 0.05 ~ "*",
                      p.value < 0.1 ~ ".", TRUE ~ "")
    )

  alto <- max(6, n_distinct(datos_viz$term_label) * 0.4)
  sufijo_titulo  <- c(todos = "", principales = " (main effects)", interacciones = " (interactions)")[[modo]]
  sufijo_archivo <- c(todos = "", principales = "_principales", interacciones = "_interacciones")[[modo]]

  p <- ggplot(datos_viz, aes(x = estimate, y = term_label, color = term_group)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high), orientation = "y",
                  width = 0.2, linewidth = 0.7, alpha = 0.8) +
    geom_point(size = 3) +
    geom_text(data = datos_viz %>% filter(p.value < 0.1),
              aes(label = sig, x = conf.high + 0.02), size = 4, fontface = "bold", color = "black") +
    facet_wrap(~ outcome_label, ncol = 2, scales = "free_x") +
    scale_color_manual(values = colores_grupos) +
    labs(title = paste0(set_label, ": ", str_to_title(group_name), " — ", condicion_label, sufijo_titulo),
         x = "Estimated coefficient (95% CI)", y = NULL, color = "Variable type",
         caption = ". p<0.1   * p<0.05   ** p<0.01   *** p<0.001") +
    theme_bw(base_size = 11) +
    theme(strip.background = element_rect(fill = "#f0f0f0"),
          strip.text = element_text(face = "bold", size = 9), legend.position = "bottom",
          legend.title = element_text(face = "bold"), plot.title = element_text(face = "bold", size = 13),
          axis.text.y = element_text(size = 9))

  filename <- out_path(subdir, paste0(file_prefix, "_", condicion_label, "_", group_name,
                                      sufijo_archivo, "_coefficients.png"))
  ggsave(filename, p, width = 11, height = ceiling(n_distinct(datos_viz$outcome_label) / 2) * alto / 2 + 2,
         dpi = 150, bg = "white", limitsize = FALSE)
  cat("  done:", filename, "\n")
}

# ---- FIGURAS DE VALORES PREDICHOS ---------------------------------------------
outcomes_predichos <- c("recommendation_probability", "idx_riesgo", "idx_crecimiento", "idx_competencia")

niveles_vars <- list(
  perfil_genero         = c("Hombre", "Mujer"),
  perfil_estado_civil   = c("soltero/a", "casado/a"),
  perfil_parental       = c("actualmente no tiene hijos", "tiene un hijo", "tiene dos hijos"),
  perfil_plan_futuro    = c("no quiere", "en 2 anos", "en 5 anos"),
  condicion_ocupacional = c("profesional", "tecnico")
)
etiquetas_niveles <- c(
  "Hombre" = "Man", "Mujer" = "Woman", "soltero/a" = "Single", "casado/a" = "Married",
  "actualmente no tiene hijos" = "No children", "tiene un hijo" = "1 child", "tiene dos hijos" = "2 children",
  "no quiere" = "No plans", "en 2 anos" = "In 2 years", "en 5 anos" = "In 5 years",
  "profesional" = "Professional", "tecnico" = "Technical"
)
etiquetas_variables <- c(
  perfil_genero = "Gender", perfil_estado_civil = "Marital status", perfil_parental = "Parental status",
  perfil_plan_futuro = "Family plans", condicion_ocupacional = "Occupational level"
)

predict_grid <- function(model, x_var, color_var) {
  grid_args <- list(model = model)
  grid_args[[x_var]] <- niveles_vars[[x_var]]
  grid_args[[color_var]] <- niveles_vars[[color_var]]
  marginaleffects::predictions(model, newdata = do.call(marginaleffects::datagrid, grid_args))
}

plot_predicted_values <- function(models_list, outcomes, labels, x_var, color_var,
                                  set_label, file_prefix, condicion_label, subdir) {
  pred_all <- map_dfr(outcomes, function(o) {
    m <- models_list[[o]]
    if (is.null(m)) return(NULL)
    predict_grid(m, x_var, color_var) %>% mutate(outcome = o)
  })
  if (nrow(pred_all) == 0) return(invisible(NULL))
  pred_all <- pred_all %>% mutate(
    outcome_label = factor(labels[outcome], levels = unname(labels[outcomes])),
    x_label = factor(etiquetas_niveles[as.character(.data[[x_var]])], levels = unname(etiquetas_niveles[niveles_vars[[x_var]]])),
    color_label = factor(etiquetas_niveles[as.character(.data[[color_var]])], levels = unname(etiquetas_niveles[niveles_vars[[color_var]]]))
  )
  p <- ggplot(pred_all, aes(x = x_label, y = estimate, color = color_label, group = color_label)) +
    geom_line(linewidth = 0.8, position = position_dodge(width = 0.15)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15, linewidth = 0.6,
                  position = position_dodge(width = 0.15)) +
    geom_point(size = 3, position = position_dodge(width = 0.15)) +
    facet_wrap(~ outcome_label, scales = "free_y") +
    scale_color_brewer(palette = "Set1") +
    labs(title = paste0(set_label, " — predicted values (", condicion_label, ")"),
         x = etiquetas_variables[[x_var]], y = "Predicted value (1-7 scale)", color = etiquetas_variables[[color_var]]) +
    theme_bw(base_size = 11) +
    theme(strip.background = element_rect(fill = "#f0f0f0"), strip.text = element_text(face = "bold", size = 9),
          legend.position = "bottom", plot.title = element_text(face = "bold", size = 12))
  filename <- out_path(subdir, paste0(file_prefix, "_", condicion_label, "_predicted.png"))
  ggsave(filename, p, width = 9, height = 6.5, dpi = 150, bg = "white")
  cat("  done:", filename, "\n")
}

# Etiquetas de cada set para titulos de figuras
set_title <- function(set_id) {
  r <- model_sets %>% filter(set_id == !!set_id)
  paste0(toupper(set_id), " (", r$label, ")")
}
