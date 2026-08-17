# ============================================================
# SCRIPT: 3_VE_260626.R
#
# Valores esperados (VE) para grupos sustantivos
# Outcomes: idx_evaluacion_pos, idx_costo_percibido, idx_riesgo_conductual
# Carpeta de salida: figuras/VE_260626/
#
# GRUPOS:
#   VE1: Género (H vs M) — efectos principales
#   VE2: Maternidad × Género (sin hijos vs con hijos, H y M)
#   VE3: Fertilidad futura — solo Mujeres (3 planes)
#   VE4: Fertilidad futura — solo Hombres (3 planes)
#   VE5: Maternidad × plan — Mujeres sin hijos (3 planes)
#   VE6: Maternidad × plan — Hombres sin hijos (3 planes)
#   VE7: Maternidad × plan — Madres (sin hijos vs 1+2 hijos) × plan
#   VE8: Maternidad × plan — Padres (sin hijos vs 1+2 hijos) × plan
#
# Modelo: SET4b (género × parental × plan, intercepto aleatorio)
# Para VE1: SET1 (efectos principales)
# Covariables no focales fijadas en su media (sum contrasts → 0)
# ============================================================

# ---- 0. Librerías ----
pkgs <- c("tidyverse", "lme4", "lmerTest", "emmeans", "ggplot2", "haven")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

# ---- 1. Directorio de salida ----
dir.create("figuras/VE_260626", recursive = TRUE, showWarnings = FALSE)
out_dir <- "figuras/VE_260626"
cat("✓ Carpeta de salida:", out_dir, "\n")

# ---- 2. Helper: sum contrasts ----
aplicar_contr_sum <- function(fac, ref_level = NULL) {
  if (!is.factor(fac)) fac <- as.factor(fac)
  lvls <- levels(fac)
  if (is.null(ref_level)) ref_level <- lvls[1]
  stopifnot(ref_level %in% lvls)
  lvls_nuevo <- c(setdiff(lvls, ref_level), ref_level)
  fac        <- factor(fac, levels = lvls_nuevo)
  n          <- length(lvls_nuevo)
  C          <- contr.sum(n)
  colnames(C) <- lvls_nuevo[-n]
  contrasts(fac) <- C
  fac
}

# ---- 3. Datos ----
# Usa df en memoria si existe; si no, carga el archivo
if (!exists("df") || !all(c("idx_evaluacion_pos", "idx_costo_percibido",
                              "idx_riesgo_conductual") %in% names(df))) {

  cat("Cargando datos...\n")

  # Buscar el archivo sav
  posibles <- c(
    "data/procesado/base_pretest_long_final.sav",
    "base_pretest_long_final.sav"
  )
  sav_path <- posibles[file.exists(posibles)][1]
  if (is.na(sav_path)) stop("No se encontró base_pretest_long_final.sav")

  base_long_final <- read_sav(sav_path)

  df <- base_long_final %>%
    mutate(
      perfil_genero = aplicar_contr_sum(
        as_factor(perfil_genero), ref_level = "Hombre"),
      perfil_estado_civil = aplicar_contr_sum(
        as_factor(perfil_estado_civil), ref_level = "soltero/a"),
      perfil_parental = aplicar_contr_sum(
        factor(perfil_parental,
               levels = c("actualmente no tiene hijos",
                          "tiene un hijo", "tiene dos hijos")),
        ref_level = "actualmente no tiene hijos"),
      perfil_plan_futuro = aplicar_contr_sum(
        as_factor(perfil_plan_futuro), ref_level = "no quiere"),
      perfil_ocupacion  = aplicar_contr_sum(as_factor(perfil_ocupacion)),
      perfil_exp        = aplicar_contr_sum(as_factor(perfil_exp)),
      perfil_desempeno  = aplicar_contr_sum(as.factor(perfil_desempeno)),
      pantalla_orden    = aplicar_contr_sum(as.factor(pantalla_orden)),
      template_contexto = aplicar_contr_sum(as.factor(template_contexto))
    ) %>%
    rowwise() %>%
    mutate(
      idx_evaluacion_pos = mean(c_across(all_of(
        c("ans_p2_1","ans_p2_2","ans_p2_3","ans_p2_4","ans_p3_5","ans_p3_6"))),
        na.rm = TRUE),
      idx_riesgo_conductual = mean(c_across(all_of(
        c("ans_p3_2","ans_p3_3","ans_p3_4"))),
        na.rm = TRUE),
      idx_costo_percibido = mean(c_across(all_of(
        c("ans_p2_6","ans_p2_7","ans_p3_7","ans_p3_8"))),
        na.rm = TRUE)
    ) %>%
    ungroup()

  cat("✓ Datos cargados y preparados\n")
} else {
  cat("✓ Usando df ya en memoria\n")
}

# ---- 4. Outcomes ----
outcomes_ve <- c("idx_evaluacion_pos", "idx_costo_percibido", "idx_riesgo_conductual")

labels_ve <- c(
  "idx_evaluacion_pos"    = "Evaluación positiva (índice)",
  "idx_costo_percibido"   = "Costo percibido (índice)",
  "idx_riesgo_conductual" = "Riesgo conductual (índice)"
)

# ---- 5. Modelos ----
controles <- "pantalla_orden + template_contexto"

# Revisar si los modelos ya están en memoria (desde 2_Analisis_RI_sum_contrasts.R)
if (exists("resultados_idx")) {
  cat("✓ Usando modelos de resultados_idx ya en memoria\n")
  modelos_4b <- setNames(
    lapply(outcomes_ve,
           function(y) resultados_idx[["SET4b_gen_x_parental_x_plan"]][[y]]),
    outcomes_ve)
  modelos_1 <- setNames(
    lapply(outcomes_ve,
           function(y) resultados_idx[["SET1_principales"]][[y]]),
    outcomes_ve)
} else {
  cat("Corriendo modelos (no están en memoria)...\n")

  formula_4b <- paste(
    "perfil_genero * perfil_parental * perfil_plan_futuro +",
    "perfil_estado_civil +",
    "perfil_ocupacion + perfil_exp + perfil_desempeno +", controles)

  formula_1 <- paste(
    "perfil_genero + perfil_estado_civil + perfil_parental +",
    "perfil_plan_futuro + perfil_ocupacion +",
    "perfil_exp + perfil_desempeno +", controles)

  correr <- function(formula_str, y) {
    fml <- as.formula(paste(y, "~", formula_str, "+ (1 | numericalId)"))
    tryCatch(
      lmer(fml, data = df, REML = TRUE),
      error   = function(e) { message("Error en ", y, ": ", e$message); NULL },
      warning = function(w) suppressWarnings(lmer(fml, data = df, REML = TRUE))
    )
  }

  modelos_4b <- setNames(lapply(outcomes_ve, function(y) correr(formula_4b, y)),
                         outcomes_ve)
  modelos_1  <- setNames(lapply(outcomes_ve, function(y) correr(formula_1,  y)),
                         outcomes_ve)
  cat("✓ Modelos corridos\n")
}

# ---- 6. Helper emmeans ----
calc_ve <- function(modelos, specs, at_vals, outs = outcomes_ve) {
  map_dfr(outs, function(y) {
    m <- modelos[[y]]
    if (is.null(m)) return(NULL)
    tryCatch({
      em <- emmeans(m, specs = specs, at = at_vals, type = "response")
      as.data.frame(em) %>%
        mutate(outcome = y, outcome_label = labels_ve[y])
    }, error = function(e) {
      message("emmeans error en ", y, ": ", e$message); NULL
    })
  })
}

# ---- Tema y paletas ----
theme_ve <- theme_bw(base_size = 12) +
  theme(
    strip.background = element_rect(fill = "#f0f0f0"),
    strip.text       = element_text(face = "bold", size = 10),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold"),
    axis.text.x      = element_text(size = 10),
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 8, color = "gray40"),
    plot.caption     = element_text(size = 8, color = "gray50")
  )

col_genero  <- c("Hombre" = "#377eb8", "Mujer" = "#e41a1c")
col_plan    <- c("Sin planes"   = "#4daf4a",
                 "Plan: 2 años" = "#ff7f00",
                 "Plan: 5 años" = "#e41a1c")
col_parental <- c("Sin hijos"      = "#4daf4a",
                  "Con hijos (1-2)" = "#984ea3")

sub_4b <- paste0(
  "Covariables no focales fijadas en su media | IC 95%\n",
  "Modelo RI: género × parental × plan familiar (SET4b, intercepto por respondente)"
)
sub_1 <- paste0(
  "Covariables no focales fijadas en su media | IC 95%\n",
  "Modelo RI: efectos principales (SET1, intercepto aleatorio por respondente)"
)

# ============================================================
# VE1: Género — Hombres vs Mujeres
# ============================================================
cat("\n--- VE1: Género ---\n")

pred_1 <- calc_ve(
  modelos_1,
  specs   = ~ perfil_genero,
  at_vals = list(perfil_genero = c("Hombre", "Mujer"))
) %>%
  mutate(
    Genero = factor(perfil_genero, levels = c("Hombre", "Mujer")),
    outcome_label = factor(outcome_label, levels = labels_ve[outcomes_ve])
  )

if (!is.null(pred_1) && nrow(pred_1) > 0) {
  fig1 <- ggplot(pred_1,
                 aes(x = Genero, y = emmean,
                     color = Genero, group = Genero)) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                  width = 0.15, linewidth = 1) +
    facet_wrap(~ outcome_label, ncol = 3, scales = "free_y") +
    scale_color_manual(values = col_genero) +
    labs(
      title    = "Valores esperados por género del perfil",
      subtitle = sub_1,
      x = NULL, y = "Valor esperado (IC 95%)",
      color = "Género del perfil",
      caption = "Basado en emmeans | Resto de covariables en su media"
    ) +
    theme_ve +
    theme(legend.position = "none")

  ggsave(file.path(out_dir, "VE1_genero.png"),
         fig1, width = 13, height = 5, dpi = 150, bg = "white")
  cat("  ✓ VE1_genero.png\n")
}

# ============================================================
# VE2: Maternidad × Género
# H y M × sin hijos / con hijos (1+2 promediados)
# ============================================================
cat("\n--- VE2: Maternidad × Género ---\n")

pred_2_raw <- calc_ve(
  modelos_4b,
  specs   = ~ perfil_genero * perfil_parental,
  at_vals = list(
    perfil_genero   = c("Hombre", "Mujer"),
    perfil_parental = c("actualmente no tiene hijos",
                        "tiene un hijo", "tiene dos hijos")
  )
)

pred_2 <- pred_2_raw %>%
  mutate(
    parental_cat = if_else(
      perfil_parental == "actualmente no tiene hijos",
      "Sin hijos", "Con hijos (1-2)"),
    Genero = factor(perfil_genero, levels = c("Hombre", "Mujer")),
    outcome_label = factor(outcome_label, levels = labels_ve[outcomes_ve])
  ) %>%
  group_by(outcome, outcome_label, Genero, parental_cat) %>%
  summarise(
    emmean   = mean(emmean,   na.rm = TRUE),
    SE       = sqrt(sum(SE^2, na.rm = TRUE)) / n(),
    lower.CL = mean(lower.CL, na.rm = TRUE),
    upper.CL = mean(upper.CL, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  mutate(parental_cat = factor(parental_cat,
                               levels = c("Sin hijos", "Con hijos (1-2)")))

if (!is.null(pred_2) && nrow(pred_2) > 0) {
  fig2 <- ggplot(pred_2,
                 aes(x = parental_cat, y = emmean,
                     color = Genero, group = Genero)) +
    geom_line(linewidth = 1, position = position_dodge(width = 0.2)) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                  width = 0.15, linewidth = 0.8,
                  position = position_dodge(width = 0.2)) +
    geom_point(size = 3.5, position = position_dodge(width = 0.2)) +
    facet_wrap(~ outcome_label, ncol = 3, scales = "free_y") +
    scale_color_manual(values = col_genero) +
    labs(
      title    = "Valores esperados por género y maternidad/paternidad",
      subtitle = sub_4b,
      x = "Estado parental del perfil",
      y = "Valor esperado (IC 95%)",
      color = "Género del perfil",
      caption = "Con hijos = promedio de 1 hijo y 2 hijos | Basado en emmeans"
    ) +
    theme_ve

  ggsave(file.path(out_dir, "VE2_maternidad_x_genero.png"),
         fig2, width = 13, height = 5, dpi = 150, bg = "white")
  cat("  ✓ VE2_maternidad_x_genero.png\n")
}

# ============================================================
# VE3: Fertilidad futura — Solo Mujeres
# ============================================================
cat("\n--- VE3: Fertilidad futura — Mujeres ---\n")

pred_3 <- calc_ve(
  modelos_4b,
  specs   = ~ perfil_genero * perfil_plan_futuro,
  at_vals = list(
    perfil_genero      = "Mujer",
    perfil_plan_futuro = c("no quiere", "en 2 años", "en 5 años")
  )
) %>%
  mutate(
    plan_label = case_when(
      perfil_plan_futuro == "no quiere" ~ "Sin planes",
      perfil_plan_futuro == "en 2 años" ~ "Plan: 2 años",
      perfil_plan_futuro == "en 5 años" ~ "Plan: 5 años"
    ),
    plan_label    = factor(plan_label,
                           levels = c("Sin planes","Plan: 2 años","Plan: 5 años")),
    outcome_label = factor(outcome_label, levels = labels_ve[outcomes_ve])
  )

if (!is.null(pred_3) && nrow(pred_3) > 0) {
  fig3 <- ggplot(pred_3,
                 aes(x = plan_label, y = emmean,
                     color = plan_label, group = 1)) +
    geom_line(linewidth = 0.9, color = "#e41a1c", alpha = 0.5) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                  width = 0.15, linewidth = 0.8) +
    geom_point(size = 4) +
    facet_wrap(~ outcome_label, ncol = 3, scales = "free_y") +
    scale_color_manual(values = col_plan) +
    labs(
      title    = "Valores esperados — Perfiles MUJER: plan reproductivo futuro",
      subtitle = sub_4b,
      x = "Plan reproductivo del perfil",
      y = "Valor esperado (IC 95%)",
      color = "Plan familiar",
      caption = "Solo perfiles femeninos | Basado en emmeans"
    ) +
    theme_ve +
    theme(legend.position = "none")

  ggsave(file.path(out_dir, "VE3_plan_mujeres.png"),
         fig3, width = 13, height = 5, dpi = 150, bg = "white")
  cat("  ✓ VE3_plan_mujeres.png\n")
}

# ============================================================
# VE4: Fertilidad futura — Solo Hombres
# ============================================================
cat("\n--- VE4: Fertilidad futura — Hombres ---\n")

pred_4 <- calc_ve(
  modelos_4b,
  specs   = ~ perfil_genero * perfil_plan_futuro,
  at_vals = list(
    perfil_genero      = "Hombre",
    perfil_plan_futuro = c("no quiere", "en 2 años", "en 5 años")
  )
) %>%
  mutate(
    plan_label = case_when(
      perfil_plan_futuro == "no quiere" ~ "Sin planes",
      perfil_plan_futuro == "en 2 años" ~ "Plan: 2 años",
      perfil_plan_futuro == "en 5 años" ~ "Plan: 5 años"
    ),
    plan_label    = factor(plan_label,
                           levels = c("Sin planes","Plan: 2 años","Plan: 5 años")),
    outcome_label = factor(outcome_label, levels = labels_ve[outcomes_ve])
  )

if (!is.null(pred_4) && nrow(pred_4) > 0) {
  fig4 <- ggplot(pred_4,
                 aes(x = plan_label, y = emmean,
                     color = plan_label, group = 1)) +
    geom_line(linewidth = 0.9, color = "#377eb8", alpha = 0.5) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                  width = 0.15, linewidth = 0.8) +
    geom_point(size = 4) +
    facet_wrap(~ outcome_label, ncol = 3, scales = "free_y") +
    scale_color_manual(values = col_plan) +
    labs(
      title    = "Valores esperados — Perfiles HOMBRE: plan reproductivo futuro",
      subtitle = sub_4b,
      x = "Plan reproductivo del perfil",
      y = "Valor esperado (IC 95%)",
      color = "Plan familiar",
      caption = "Solo perfiles masculinos | Basado en emmeans"
    ) +
    theme_ve +
    theme(legend.position = "none")

  ggsave(file.path(out_dir, "VE4_plan_hombres.png"),
         fig4, width = 13, height = 5, dpi = 150, bg = "white")
  cat("  ✓ VE4_plan_hombres.png\n")
}

# ============================================================
# VE5: Maternidad × plan — Mujeres SIN HIJOS
# ============================================================
cat("\n--- VE5: Maternidad × plan — Mujeres sin hijos ---\n")

pred_5 <- calc_ve(
  modelos_4b,
  specs   = ~ perfil_genero * perfil_parental * perfil_plan_futuro,
  at_vals = list(
    perfil_genero      = "Mujer",
    perfil_parental    = "actualmente no tiene hijos",
    perfil_plan_futuro = c("no quiere", "en 2 años", "en 5 años")
  )
) %>%
  mutate(
    plan_label = case_when(
      perfil_plan_futuro == "no quiere" ~ "Sin planes",
      perfil_plan_futuro == "en 2 años" ~ "Plan: 2 años",
      perfil_plan_futuro == "en 5 años" ~ "Plan: 5 años"
    ),
    plan_label    = factor(plan_label,
                           levels = c("Sin planes","Plan: 2 años","Plan: 5 años")),
    outcome_label = factor(outcome_label, levels = labels_ve[outcomes_ve])
  )

if (!is.null(pred_5) && nrow(pred_5) > 0) {
  fig5 <- ggplot(pred_5,
                 aes(x = plan_label, y = emmean,
                     color = plan_label, group = 1)) +
    geom_line(linewidth = 0.9, color = "#e41a1c", alpha = 0.5) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                  width = 0.15, linewidth = 0.8) +
    geom_point(size = 4) +
    facet_wrap(~ outcome_label, ncol = 3, scales = "free_y") +
    scale_color_manual(values = col_plan) +
    labs(
      title    = "Valores esperados — Perfiles MUJER SIN HIJOS: plan reproductivo futuro",
      subtitle = sub_4b,
      x = "Plan reproductivo del perfil",
      y = "Valor esperado (IC 95%)",
      color = "Plan familiar",
      caption = "Solo perfiles femeninos sin hijos | Basado en emmeans"
    ) +
    theme_ve +
    theme(legend.position = "none")

  ggsave(file.path(out_dir, "VE5_plan_mujeres_sin_hijos.png"),
         fig5, width = 13, height = 5, dpi = 150, bg = "white")
  cat("  ✓ VE5_plan_mujeres_sin_hijos.png\n")
}

# ============================================================
# VE6: Maternidad × plan — Hombres SIN HIJOS
# ============================================================
cat("\n--- VE6: Maternidad × plan — Hombres sin hijos ---\n")

pred_6 <- calc_ve(
  modelos_4b,
  specs   = ~ perfil_genero * perfil_parental * perfil_plan_futuro,
  at_vals = list(
    perfil_genero      = "Hombre",
    perfil_parental    = "actualmente no tiene hijos",
    perfil_plan_futuro = c("no quiere", "en 2 años", "en 5 años")
  )
) %>%
  mutate(
    plan_label = case_when(
      perfil_plan_futuro == "no quiere" ~ "Sin planes",
      perfil_plan_futuro == "en 2 años" ~ "Plan: 2 años",
      perfil_plan_futuro == "en 5 años" ~ "Plan: 5 años"
    ),
    plan_label    = factor(plan_label,
                           levels = c("Sin planes","Plan: 2 años","Plan: 5 años")),
    outcome_label = factor(outcome_label, levels = labels_ve[outcomes_ve])
  )

if (!is.null(pred_6) && nrow(pred_6) > 0) {
  fig6 <- ggplot(pred_6,
                 aes(x = plan_label, y = emmean,
                     color = plan_label, group = 1)) +
    geom_line(linewidth = 0.9, color = "#377eb8", alpha = 0.5) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                  width = 0.15, linewidth = 0.8) +
    geom_point(size = 4) +
    facet_wrap(~ outcome_label, ncol = 3, scales = "free_y") +
    scale_color_manual(values = col_plan) +
    labs(
      title    = "Valores esperados — Perfiles HOMBRE SIN HIJOS: plan reproductivo futuro",
      subtitle = sub_4b,
      x = "Plan reproductivo del perfil",
      y = "Valor esperado (IC 95%)",
      color = "Plan familiar",
      caption = "Solo perfiles masculinos sin hijos | Basado en emmeans"
    ) +
    theme_ve +
    theme(legend.position = "none")

  ggsave(file.path(out_dir, "VE6_plan_hombres_sin_hijos.png"),
         fig6, width = 13, height = 5, dpi = 150, bg = "white")
  cat("  ✓ VE6_plan_hombres_sin_hijos.png\n")
}

# ============================================================
# VE7: Maternidad × plan — MADRES (sin hijos vs 1+2 hijos) × plan
# ============================================================
cat("\n--- VE7: Maternidad × plan — Madres (sin hijos vs con hijos) ---\n")

pred_7_raw <- calc_ve(
  modelos_4b,
  specs   = ~ perfil_genero * perfil_parental * perfil_plan_futuro,
  at_vals = list(
    perfil_genero      = "Mujer",
    perfil_parental    = c("actualmente no tiene hijos",
                           "tiene un hijo", "tiene dos hijos"),
    perfil_plan_futuro = c("no quiere", "en 2 años", "en 5 años")
  )
)

pred_7 <- pred_7_raw %>%
  mutate(
    maternidad_cat = if_else(
      perfil_parental == "actualmente no tiene hijos",
      "Sin hijos", "Madres (1-2 hijos)"),
    plan_label = case_when(
      perfil_plan_futuro == "no quiere" ~ "Sin planes",
      perfil_plan_futuro == "en 2 años" ~ "Plan: 2 años",
      perfil_plan_futuro == "en 5 años" ~ "Plan: 5 años"
    )
  ) %>%
  group_by(outcome, outcome_label, maternidad_cat, plan_label) %>%
  summarise(
    emmean   = mean(emmean,   na.rm = TRUE),
    SE       = sqrt(sum(SE^2, na.rm = TRUE)) / n(),
    lower.CL = mean(lower.CL, na.rm = TRUE),
    upper.CL = mean(upper.CL, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  mutate(
    maternidad_cat = factor(maternidad_cat,
                            levels = c("Sin hijos", "Madres (1-2 hijos)")),
    plan_label     = factor(plan_label,
                            levels = c("Sin planes","Plan: 2 años","Plan: 5 años")),
    outcome_label  = factor(outcome_label, levels = labels_ve[outcomes_ve])
  )

if (!is.null(pred_7) && nrow(pred_7) > 0) {
  fig7 <- ggplot(pred_7,
                 aes(x = maternidad_cat, y = emmean,
                     color = plan_label, group = plan_label)) +
    geom_line(linewidth = 1, position = position_dodge(width = 0.25)) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                  width = 0.15, linewidth = 0.8,
                  position = position_dodge(width = 0.25)) +
    geom_point(size = 3.5, position = position_dodge(width = 0.25)) +
    facet_wrap(~ outcome_label, ncol = 3, scales = "free_y") +
    scale_color_manual(values = col_plan) +
    labs(
      title    = "Valores esperados — MUJERES: sin hijos vs madres (1-2 hijos) × plan",
      subtitle = paste0(sub_4b, "\nMadres = promedio de perfiles con 1 hijo y 2 hijos"),
      x = "Estado parental del perfil",
      y = "Valor esperado (IC 95%)",
      color = "Plan familiar",
      caption = "Solo perfiles femeninos | Basado en emmeans"
    ) +
    theme_ve

  ggsave(file.path(out_dir, "VE7_madres_sin_vs_con_hijos_x_plan.png"),
         fig7, width = 13, height = 5.5, dpi = 150, bg = "white")
  cat("  ✓ VE7_madres_sin_vs_con_hijos_x_plan.png\n")
}

# ============================================================
# VE8: Maternidad × plan — PADRES (sin hijos vs 1+2 hijos) × plan
# ============================================================
cat("\n--- VE8: Maternidad × plan — Padres (sin hijos vs con hijos) ---\n")

pred_8_raw <- calc_ve(
  modelos_4b,
  specs   = ~ perfil_genero * perfil_parental * perfil_plan_futuro,
  at_vals = list(
    perfil_genero      = "Hombre",
    perfil_parental    = c("actualmente no tiene hijos",
                           "tiene un hijo", "tiene dos hijos"),
    perfil_plan_futuro = c("no quiere", "en 2 años", "en 5 años")
  )
)

pred_8 <- pred_8_raw %>%
  mutate(
    paternidad_cat = if_else(
      perfil_parental == "actualmente no tiene hijos",
      "Sin hijos", "Padres (1-2 hijos)"),
    plan_label = case_when(
      perfil_plan_futuro == "no quiere" ~ "Sin planes",
      perfil_plan_futuro == "en 2 años" ~ "Plan: 2 años",
      perfil_plan_futuro == "en 5 años" ~ "Plan: 5 años"
    )
  ) %>%
  group_by(outcome, outcome_label, paternidad_cat, plan_label) %>%
  summarise(
    emmean   = mean(emmean,   na.rm = TRUE),
    SE       = sqrt(sum(SE^2, na.rm = TRUE)) / n(),
    lower.CL = mean(lower.CL, na.rm = TRUE),
    upper.CL = mean(upper.CL, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  mutate(
    paternidad_cat = factor(paternidad_cat,
                            levels = c("Sin hijos", "Padres (1-2 hijos)")),
    plan_label     = factor(plan_label,
                            levels = c("Sin planes","Plan: 2 años","Plan: 5 años")),
    outcome_label  = factor(outcome_label, levels = labels_ve[outcomes_ve])
  )

if (!is.null(pred_8) && nrow(pred_8) > 0) {
  fig8 <- ggplot(pred_8,
                 aes(x = paternidad_cat, y = emmean,
                     color = plan_label, group = plan_label)) +
    geom_line(linewidth = 1, position = position_dodge(width = 0.25)) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                  width = 0.15, linewidth = 0.8,
                  position = position_dodge(width = 0.25)) +
    geom_point(size = 3.5, position = position_dodge(width = 0.25)) +
    facet_wrap(~ outcome_label, ncol = 3, scales = "free_y") +
    scale_color_manual(values = col_plan) +
    labs(
      title    = "Valores esperados — HOMBRES: sin hijos vs padres (1-2 hijos) × plan",
      subtitle = paste0(sub_4b, "\nPadres = promedio de perfiles con 1 hijo y 2 hijos"),
      x = "Estado parental del perfil",
      y = "Valor esperado (IC 95%)",
      color = "Plan familiar",
      caption = "Solo perfiles masculinos | Basado en emmeans"
    ) +
    theme_ve

  ggsave(file.path(out_dir, "VE8_padres_sin_vs_con_hijos_x_plan.png"),
         fig8, width = 13, height = 5.5, dpi = 150, bg = "white")
  cat("  ✓ VE8_padres_sin_vs_con_hijos_x_plan.png\n")
}

# ============================================================
# RESUMEN
# ============================================================
cat("\n================================================\n")
cat("  FIGURAS VE_260626 — COMPLETADAS\n")
cat("================================================\n")
cat("Guardadas en: figuras/VE_260626/\n\n")
cat("  VE1_genero.png\n")
cat("    → Hombres vs Mujeres (efectos principales)\n\n")
cat("  VE2_maternidad_x_genero.png\n")
cat("    → H y M × sin hijos vs con hijos (1+2)\n\n")
cat("  VE3_plan_mujeres.png\n")
cat("    → Mujeres: sin planes / plan 2 años / plan 5 años\n\n")
cat("  VE4_plan_hombres.png\n")
cat("    → Hombres: sin planes / plan 2 años / plan 5 años\n\n")
cat("  VE5_plan_mujeres_sin_hijos.png\n")
cat("    → Mujeres SIN HIJOS: sin planes / plan 2 años / plan 5 años\n\n")
cat("  VE6_plan_hombres_sin_hijos.png\n")
cat("    → Hombres SIN HIJOS: sin planes / plan 2 años / plan 5 años\n\n")
cat("  VE7_madres_sin_vs_con_hijos_x_plan.png\n")
cat("    → Mujeres: sin hijos vs madres (1+2) × plan\n\n")
cat("  VE8_padres_sin_vs_con_hijos_x_plan.png\n")
cat("    → Hombres: sin hijos vs padres (1+2) × plan\n\n")
cat("Modelo: SET4b (género × parental × plan, RI por respondente)\n")
cat("================================================\n")
