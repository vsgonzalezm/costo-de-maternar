
library(tidyverse)
library(haven)
library(estimatr)
library(broom)
library(ggplot2)
library(here)
library(flextable)
library(officer)

df_raw <- read_sav(here("data/procesado/base_pretest_long_final.sav"))

# Subcarpetas de output
dir.create(here("tablas/valores_esperados"),  recursive = TRUE, showWarnings = FALSE)
dir.create(here("figuras/valores_esperados"), recursive = TRUE, showWarnings = FALSE)

out_tablas  <- here("tablas/valores_esperados")
out_figuras <- here("figuras/valores_esperados")


# HELPER: aplicar sum contrasts 

aplicar_contr_sum <- function(fac, ref_level = NULL) {
  if (!is.factor(fac)) fac <- as.factor(fac)
  lvls <- levels(fac)
  if (is.null(ref_level)) ref_level <- lvls[length(lvls)]
  stopifnot(ref_level %in% lvls)
  lvls_nuevo <- c(setdiff(lvls, ref_level), ref_level)
  fac <- factor(fac, levels = lvls_nuevo)
  n <- length(lvls_nuevo)
  C <- contr.sum(n)
  colnames(C) <- lvls_nuevo[-n]
  contrasts(fac) <- C
  fac
}

# PREPARACIÓN: sum contrasts en todas las variables

df <- df_raw %>%
  mutate(
    perfil_genero = aplicar_contr_sum(
      as_factor(perfil_genero), ref_level = "Hombre"),
    
    perfil_estado_civil = aplicar_contr_sum(
      as_factor(perfil_estado_civil), ref_level = "soltero/a"),
    
    perfil_parental = aplicar_contr_sum(
      factor(as_factor(perfil_parental),
             levels = c("actualmente no tiene hijos",
                        "tiene un hijo", "tiene dos hijos")),
      ref_level = "actualmente no tiene hijos"),
    
    perfil_plan_futuro = aplicar_contr_sum(
      as_factor(perfil_plan_futuro), ref_level = "no quiere"),
    
    perfil_ocupacion = aplicar_contr_sum(
      as_factor(perfil_ocupacion), ref_level = "analista contable"),
    
    perfil_exp       = aplicar_contr_sum(as_factor(perfil_exp)),
    perfil_desempeno = aplicar_contr_sum(as_factor(perfil_desempeno)),
    
    pantalla_orden = aplicar_contr_sum(
      factor(pantalla_orden), ref_level = "2"),
    
    template_contexto = aplicar_contr_sum(
      as_factor(template_contexto), ref_level = "B")
  )

cat("✓ Sum contrasts aplicados\n")
cat("  Intercepto = media general del diseño experimental\n")
cat("  Cada coeficiente = desviación del promedio del diseño\n\n")


# ============================================================
# HELPER: valor modal de un factor / media de un numérico

modal_factor <- function(x) {
  if (is.factor(x)) {
    lvl <- names(sort(table(as.character(x)), decreasing = TRUE))[1]
    factor(lvl, levels = levels(x))
  } else {
    mean(x, na.rm = TRUE)
  }
}

# Covariables fijadas en promedio/moda del diseño

covariables_base <- function(datos, excluir = c()) {
  vars <- setdiff(
    c("perfil_genero", "perfil_estado_civil", "perfil_parental",
      "perfil_plan_futuro", "perfil_ocupacion",
      "perfil_exp", "perfil_desempeno",
      "pantalla_orden", "template_contexto"),
    excluir
  )
  map_dfc(vars, ~ {
    v <- datos[[.x]]
    val <- modal_factor(v)
    tibble(!!.x := val)
  })
}


# HELPER
aplicar_contrastes <- function(newdata, df_ref) {
  for (v in names(df_ref)) {
    if (v %in% names(newdata) && is.factor(df_ref[[v]])) {
      newdata[[v]] <- factor(
        as.character(newdata[[v]]),
        levels = levels(df_ref[[v]])
      )
      contrasts(newdata[[v]]) <- contrasts(df_ref[[v]])
    }
  }
  newdata
}


# ==========================================================
exportar_docx <- function(df_tabla, ruta, titulo = NULL, nota = NULL) {
  
  ft <- df_tabla %>%
    flextable() %>%
    theme_booktabs() %>%
    bg(bg = "#2E74B5", part = "header") %>%
    color(color = "white", part = "header") %>%
    bold(part = "header") %>%
    fontsize(size = 11, part = "all") %>%
    font(fontname = "Arial", part = "all") %>%
    autofit() %>%
    set_table_properties(width = 1, layout = "autofit")
  
  # Filas alternas
  ft <- bg(ft,
           i   = seq(2, nrow(df_tabla), by = 2),
           bg  = "#F2F7FC",
           part = "body")
  
  doc <- read_docx() %>%
    body_add_par("", style = "Normal")
  
  if (!is.null(titulo)) {
    doc <- doc %>%
      body_add_par(titulo, style = "heading 2")
  }
  
  if (!is.null(nota)) {
    doc <- doc %>%
      body_add_par(nota, style = "Normal")
  }
  
  doc <- doc %>%
    body_add_flextable(ft)
  
  print(doc, target = ruta)
  cat("✓ Tabla guardada:", ruta, "\n")
}


# CONTROLES BASE para fórmulas
  
controles <- "perfil_genero + perfil_estado_civil + perfil_parental +
              perfil_exp + perfil_desempeno + template_contexto"



# MODELOS PRINCIPALES

#ocupación*orden
m_ocup_orden <- lm(
  as.formula(paste(
    "ans_p1_global ~ perfil_ocupacion * pantalla_orden +",
    "perfil_plan_futuro +", controles
  )),
  data = df
)

#pan familia*orden
m_plan_orden <- lm(
  as.formula(paste(
    "ans_p1_global ~ perfil_plan_futuro * pantalla_orden +",
    "perfil_ocupacion +", controles
  )),
  data = df
)

cat("✓ Modelos ocupación × orden y plan × orden estimados\n")



# VE 1: OCUPACIÓN × ORDEN  (4 celdas)
# Técnico/Profesional × 1er perfil/2do perfil


base_ocup <- covariables_base(df, excluir = c("perfil_ocupacion", "pantalla_orden"))

grilla_ocup_orden <- expand.grid(
  perfil_ocupacion = levels(df$perfil_ocupacion),
  pantalla_orden   = levels(df$pantalla_orden),
  stringsAsFactors = FALSE
) %>%
  mutate(
    perfil_ocupacion = factor(perfil_ocupacion, levels = levels(df$perfil_ocupacion)),
    pantalla_orden   = factor(pantalla_orden,   levels = levels(df$pantalla_orden))
  )

contrasts(grilla_ocup_orden$perfil_ocupacion) <- contrasts(df$perfil_ocupacion)
contrasts(grilla_ocup_orden$pantalla_orden)   <- contrasts(df$pantalla_orden)

newdata_ocup_orden <- grilla_ocup_orden %>%
  bind_cols(base_ocup[rep(1, nrow(grilla_ocup_orden)), ]) %>%
  aplicar_contrastes(df)

preds_ocup_orden <- predict(m_ocup_orden, newdata = newdata_ocup_orden, se.fit = TRUE)

ve_ocup_orden <- grilla_ocup_orden %>%
  mutate(
    ve      = preds_ocup_orden$fit,
    se      = preds_ocup_orden$se.fit,
    ci_low  = ve - 1.96 * se,
    ci_high = ve + 1.96 * se,
    ocupacion_label = ifelse(
      as.character(perfil_ocupacion) == "analista contable",
      "Técnico (analista contable)",
      "Profesional (asociado/a financiero/a)"),
    orden_label = ifelse(pantalla_orden == "1", "1er perfil", "2do perfil")
  )

cat("\n=== VE: OCUPACIÓN × ORDEN ===\n")
ve_ocup_orden %>%
  select(ocupacion_label, orden_label, ve, ci_low, ci_high) %>%
  mutate(across(c(ve, ci_low, ci_high), ~ round(., 3))) %>%
  as.data.frame() %>% print()

# Exportar .docx
exportar_docx(
  df_tabla = ve_ocup_orden %>%
    select(
      "Ocupación"   = ocupacion_label,
      "Posición"    = orden_label,
      "VE"          = ve,
      "SE"          = se,
      "IC inf. 95%" = ci_low,
      "IC sup. 95%" = ci_high
    ) %>%
    mutate(across(c(VE, SE, `IC inf. 95%`, `IC sup. 95%`), ~ round(., 3))),
  ruta   = file.path(out_tablas, "ve_ocupacion_orden.docx"),
  titulo = "Valores esperados de contratación: Ocupación × Orden de presentación",
  nota   = "Nota: VE = valor esperado en escala 1–7. Covariables fijadas en promedio del diseño (sum contrasts). IC 95% aproximado."
)


# VE 2: PLAN FAMILIAR × ORDEN  (6 celdas)
# No quiere / En 2 años / En 5 años × 1er perfil / 2do perfil


base_plan <- covariables_base(df, excluir = c("perfil_plan_futuro", "pantalla_orden"))

grilla_plan_orden <- expand.grid(
  perfil_plan_futuro = levels(df$perfil_plan_futuro),
  pantalla_orden     = levels(df$pantalla_orden),
  stringsAsFactors   = FALSE
) %>%
  mutate(
    perfil_plan_futuro = factor(perfil_plan_futuro, levels = levels(df$perfil_plan_futuro)),
    pantalla_orden     = factor(pantalla_orden,     levels = levels(df$pantalla_orden))
  )

contrasts(grilla_plan_orden$perfil_plan_futuro) <- contrasts(df$perfil_plan_futuro)
contrasts(grilla_plan_orden$pantalla_orden)     <- contrasts(df$pantalla_orden)

newdata_plan_orden <- grilla_plan_orden %>%
  bind_cols(base_plan[rep(1, nrow(grilla_plan_orden)), ]) %>%
  aplicar_contrastes(df)

preds_plan_orden <- predict(m_plan_orden, newdata = newdata_plan_orden, se.fit = TRUE)

ve_plan_orden <- grilla_plan_orden %>%
  mutate(
    ve      = preds_plan_orden$fit,
    se      = preds_plan_orden$se.fit,
    ci_low  = ve - 1.96 * se,
    ci_high = ve + 1.96 * se,
    plan_label = case_when(
      as.character(perfil_plan_futuro) == "no quiere" ~ "No quiere hijos",
      as.character(perfil_plan_futuro) == "en 2 años" ~ "En 2 años",
      as.character(perfil_plan_futuro) == "en 5 años" ~ "En 5 años"
    ),
    plan_label  = factor(plan_label,
                         levels = c("No quiere hijos", "En 2 años", "En 5 años")),
    orden_label = ifelse(pantalla_orden == "1", "1er perfil", "2do perfil")
  )

cat("\n=== VE: PLAN FAMILIAR × ORDEN ===\n")
ve_plan_orden %>%
  select(plan_label, orden_label, ve, ci_low, ci_high) %>%
  mutate(across(c(ve, ci_low, ci_high), ~ round(., 3))) %>%
  as.data.frame() %>% print()

exportar_docx(
  df_tabla = ve_plan_orden %>%
    select(
      "Plan reproductivo" = plan_label,
      "Posición"          = orden_label,
      "VE"                = ve,
      "SE"                = se,
      "IC inf. 95%"       = ci_low,
      "IC sup. 95%"       = ci_high
    ) %>%
    mutate(across(c(VE, SE, `IC inf. 95%`, `IC sup. 95%`), ~ round(., 3))),
  ruta   = file.path(out_tablas, "ve_plan_orden.docx"),
  titulo = "Valores esperados de contratación: Plan familiar × Orden de presentación",
  nota   = "Nota: VE = valor esperado en escala 1–7. Covariables fijadas en promedio del diseño (sum contrasts). IC 95% aproximado."
)



# VE 3: OCUPACIÓN EN VIÑETAS HOMOGÉNEAS
# Encuestados que vieron dos perfiles del mismo tipo de cargo

df <- df %>%
  group_by(numericalId) %>%
  mutate(
    n_tipos_ocup = n_distinct(as.character(perfil_ocupacion)),
    combo_ocup   = ifelse(n_tipos_ocup == 1, "homogénea", "heterogénea"),
    combo_ocup   = factor(combo_ocup, levels = c("homogénea", "heterogénea"))
  ) %>%
  ungroup()

df_homo <- df %>% filter(combo_ocup == "homogénea")

cat("\nN en submuestra homogénea:", nrow(df_homo),
    "(~", n_distinct(df_homo$numericalId), "encuestados)\n")
cat("ADVERTENCIA: N pequeño — resultados indicativos, no concluyentes\n\n")

m_ocup_homo <- lm(
  as.formula(paste(
    "ans_p1_global ~ perfil_ocupacion + pantalla_orden +",
    "perfil_plan_futuro +", controles
  )),
  data = df_homo
)

base_homo <- covariables_base(df_homo,
                              excluir = c("perfil_ocupacion", "pantalla_orden"))

grilla_homo <- expand.grid(
  perfil_ocupacion = levels(df_homo$perfil_ocupacion),
  pantalla_orden   = levels(df_homo$pantalla_orden),
  stringsAsFactors = FALSE
) %>%
  mutate(
    perfil_ocupacion = factor(perfil_ocupacion, levels = levels(df_homo$perfil_ocupacion)),
    pantalla_orden   = factor(pantalla_orden,   levels = levels(df_homo$pantalla_orden))
  )

contrasts(grilla_homo$perfil_ocupacion) <- contrasts(df_homo$perfil_ocupacion)
contrasts(grilla_homo$pantalla_orden)   <- contrasts(df_homo$pantalla_orden)

newdata_homo <- grilla_homo %>%
  bind_cols(base_homo[rep(1, nrow(grilla_homo)), ]) %>%
  aplicar_contrastes(df_homo)

preds_homo <- predict(m_ocup_homo, newdata = newdata_homo, se.fit = TRUE)

ve_homo <- grilla_homo %>%
  mutate(
    ve      = preds_homo$fit,
    se      = preds_homo$se.fit,
    ci_low  = ve - 1.96 * se,
    ci_high = ve + 1.96 * se,
    ocupacion_label = ifelse(
      as.character(perfil_ocupacion) == "analista contable",
      "Técnico", "Profesional"),
    orden_label = ifelse(pantalla_orden == "1", "1er perfil", "2do perfil"),
    subgrupo    = "Solo viñetas homogéneas (sin contraste entre cargos)"
  )

ve_completa <- ve_ocup_orden %>%
  mutate(
    ocupacion_label = ifelse(str_detect(ocupacion_label, "Técnico"),
                             "Técnico", "Profesional"),
    subgrupo = "Muestra completa (con y sin contraste)"
  )

ve_ocup_subgrupos <- bind_rows(
  ve_completa %>% select(ocupacion_label, orden_label, ve, se, ci_low, ci_high, subgrupo),
  ve_homo     %>% select(ocupacion_label, orden_label, ve, se, ci_low, ci_high, subgrupo)
)

cat("\n=== VE: OCUPACIÓN — MUESTRA COMPLETA vs HOMOGÉNEA ===\n")
ve_ocup_subgrupos %>%
  select(subgrupo, ocupacion_label, orden_label, ve, ci_low, ci_high) %>%
  mutate(across(c(ve, ci_low, ci_high), ~ round(., 3))) %>%
  as.data.frame() %>% print()

exportar_docx(
  df_tabla = ve_ocup_subgrupos %>%
    select(
      "Subgrupo"    = subgrupo,
      "Ocupación"   = ocupacion_label,
      "Posición"    = orden_label,
      "VE"          = ve,
      "SE"          = se,
      "IC inf. 95%" = ci_low,
      "IC sup. 95%" = ci_high
    ) %>%
    mutate(across(c(VE, SE, `IC inf. 95%`, `IC sup. 95%`), ~ round(., 3))),
  ruta   = file.path(out_tablas, "ve_ocupacion_subgrupos.docx"),
  titulo = "Valores esperados de contratación: Ocupación — Muestra completa vs. Viñetas homogéneas",
  nota   = "Nota: Viñetas homogéneas = encuestados que evaluaron dos perfiles del mismo tipo de cargo (sin contraste técnico-profesional). N pequeño en subgrupo homogéneo — resultados indicativos."
)


# COEFS: VARIACIÓN DEL EFECTO DE OCUPACIÓN SEGÚN
#         GÉNERO, ESTADO PARENTAL Y PLAN FAMILIAR

m_ocup_interacciones <- lm_robust(
  ans_p1_global ~
    perfil_ocupacion * perfil_genero +
    perfil_ocupacion * perfil_parental +
    perfil_ocupacion * perfil_plan_futuro +
    pantalla_orden + perfil_estado_civil +
    perfil_exp + perfil_desempeno + template_contexto,
  data     = df,
  clusters = numericalId,
  se_type  = "CR2"
)

coefs_interacciones <- tidy(m_ocup_interacciones) %>%
  filter(str_detect(term, "perfil_ocupacion"),
         !str_detect(term, "Intercept")) %>%
  mutate(
    term_label = case_when(
      !str_detect(term, ":")                  ~ "Ocupación: profesional (efecto principal)",
      str_detect(term, "Mujer")               ~ "Profesional × Mujer",
      str_detect(term, "tiene un hijo")       ~ "Profesional × Un hijo",
      str_detect(term, "tiene dos hijos")     ~ "Profesional × Dos hijos",
      str_detect(term, "en 2 años")           ~ "Profesional × Plan 2 años",
      str_detect(term, "en 5 años")           ~ "Profesional × Plan 5 años",
      TRUE ~ term
    ),
    tipo = ifelse(str_detect(term, ":"),
                  "Interacción con ocupación",
                  "Efecto principal ocupación"),
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ ".",
      TRUE            ~ ""
    )
  )

cat("\n=== COEFICIENTES: OCUPACIÓN × GÉNERO/PARENTAL/PLAN ===\n")
coefs_interacciones %>%
  select(term_label, tipo, estimate, std.error, p.value, sig) %>%
  mutate(across(c(estimate, std.error), ~ round(., 3))) %>%
  as.data.frame() %>% print()

exportar_docx(
  df_tabla = coefs_interacciones %>%
    select(
      "Término"          = term_label,
      "Tipo"             = tipo,
      "Coeficiente"      = estimate,
      "Error estándar"   = std.error,
      "IC inf. 95%"      = conf.low,
      "IC sup. 95%"      = conf.high,
      "p"                = p.value,
      "Sig."             = sig
    ) %>%
    mutate(across(c(Coeficiente, `Error estándar`,
                    `IC inf. 95%`, `IC sup. 95%`, p), ~ round(., 3))),
  ruta   = file.path(out_tablas, "coefs_ocupacion_interacciones.docx"),
  titulo = "¿Varía el efecto de ocupación según género, estado parental y plan familiar?",
  nota   = "Nota: Coeficientes OLS clusterizados (CR2). Sum contrasts — cada coeficiente = desviación del promedio del diseño. Si las interacciones son cercanas a cero, ocupación no interfiere con las variables de interés principal."
)

cat("✓ Todas las tablas exportadas a", out_tablas, "\n")



# FIGURAS


col_ocup  <- c(
  "Técnico (analista contable)"           = "#377eb8",
  "Profesional (asociado/a financiero/a)" = "#e41a1c",
  "Técnico"                               = "#377eb8",
  "Profesional"                           = "#e41a1c"
)
col_orden <- c("1er perfil" = "#e41a1c", "2do perfil" = "#377eb8")


# ---- Figura 1

fig1 <- ggplot(ve_ocup_orden,
               aes(x = ve, y = orden_label,
                   color = ocupacion_label,
                   shape = orden_label)) +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high),
                 height = 0.12, linewidth = 0.9,
                 position = position_dodge(width = 0.5)) +
  geom_point(size = 4,
             position = position_dodge(width = 0.5)) +
  scale_color_manual(values = col_ocup) +
  scale_shape_manual(values = c("1er perfil" = 16, "2do perfil" = 17),
                     guide  = "none") +
  scale_x_continuous(limits = c(4, 7), breaks = seq(4, 7, 0.5)) +
  labs(
    title    = "Valores esperados de contratación: Ocupación × Orden",
    subtitle = paste0(
    ),
    x       = "Recomendación de contratación esperada (escala 1–7)",
    y       = "Posición en el diseño",
    color   = "Tipo de ocupación",
    caption = "IC 95% aproximado. Covariables fijadas en promedio del diseño."
  ) +
  theme_bw(base_size = 13) +
  theme(plot.title       = element_text(face = "bold"),
        plot.subtitle    = element_text(size = 10, color = "gray40"),
        legend.position  = "bottom",
        legend.title     = element_text(face = "bold"),
        panel.grid.minor = element_blank())

ggsave(file.path(out_figuras, "ve_ocupacion_orden.png"),
       fig1, width = 9, height = 5, dpi = 150, bg = "white")
cat("✓ Figura 1 guardada\n")


# ---- Figura 2
fig2 <- ggplot(ve_plan_orden,
               aes(x = plan_label, y = ve,
                   color = orden_label, group = orden_label)) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high, fill = orden_label),
              alpha = 0.12, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.07, linewidth = 0.8, alpha = 0.8) +
  scale_color_manual(values = col_orden) +
  scale_fill_manual(values  = col_orden, guide = "none") +
  scale_y_continuous(limits = c(4, 7), breaks = seq(4, 7, 0.5)) +
  labs(
    title    = "Valores esperados de contratación: Plan familiar × Orden",
    subtitle = paste0(
    ),
    x       = "Plan reproductivo",
    y       = "Recomendación de contratación esperada (escala 1–7)",
    color   = "Posición en el diseño",
    caption = "IC 95% aproximado. Covariables fijadas en promedio del diseño."
  ) +
  theme_bw(base_size = 13) +
  theme(plot.title       = element_text(face = "bold"),
        plot.subtitle    = element_text(size = 10, color = "gray40"),
        legend.position  = "bottom",
        legend.title     = element_text(face = "bold"),
        panel.grid.minor = element_blank())

ggsave(file.path(out_figuras, "ve_plan_orden.png"),
       fig2, width = 9, height = 6, dpi = 150, bg = "white")
cat("✓ Figura 2 guardada\n")


# ---- Figura 3

fig3 <- ggplot(ve_ocup_subgrupos,
               aes(x = ve, y = ocupacion_label,
                   color = ocupacion_label,
                   shape = orden_label)) +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high),
                 height = 0.15, linewidth = 0.8,
                 position = position_dodge(width = 0.5)) +
  geom_point(size = 4,
             position = position_dodge(width = 0.5)) +
  facet_wrap(~ subgrupo, ncol = 2) +
  scale_color_manual(values = col_ocup) +
  scale_shape_manual(values = c("1er perfil" = 16, "2do perfil" = 17)) +
  scale_x_continuous(limits = c(3, 7), breaks = seq(3, 7, 0.5)) +
  labs(
    title    = "Efecto de ocupación: muestra completa vs. viñetas homogéneas",
    subtitle = paste0(
    ),
    x       = "Recomendación de contratación esperada (escala 1–7)",
    y       = NULL,
    color   = "Tipo de ocupación",
    shape   = "Posición",
    caption = "IC 95% aproximado. Covariables fijadas en promedio del diseño."
  ) +
  theme_bw(base_size = 13) +
  theme(plot.title        = element_text(face = "bold"),
        plot.subtitle     = element_text(size = 10, color = "gray40"),
        legend.position   = "bottom",
        legend.title      = element_text(face = "bold"),
        strip.background  = element_rect(fill = "#f0f0f0"),
        strip.text        = element_text(face = "bold"),
        panel.grid.minor  = element_blank())

ggsave(file.path(out_figuras, "ve_ocupacion_subgrupos.png"),
       fig3, width = 12, height = 5, dpi = 150, bg = "white")
cat("✓ Figura 3 guardada\n")


# ---- Figura 4

fig4 <- ggplot(coefs_interacciones,
               aes(x = estimate,
                   y = reorder(term_label, estimate),
                   color = tipo)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "gray50", linewidth = 0.5) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2, linewidth = 0.8, alpha = 0.8) +
  geom_point(size = 3.5) +
  geom_text(data = coefs_interacciones %>% filter(p.value < 0.1),
            aes(label = sig, x = conf.high + 0.05),
            size = 4, fontface = "bold", color = "black") +
  scale_color_manual(values = c(
    "Efecto principal ocupación" = "#a65628",
    "Interacción con ocupación"  = "#984ea3"
  )) +
  labs(
    title    = "Variación del efecto de ocupación según género, parental y plan",
    subtitle = paste0(
    ),
    x       = "Coeficiente estimado (desviación del promedio del diseño)",
    y       = NULL,
    color   = "Tipo de término",
    caption = ". p < 0.1   * p < 0.05   ** p < 0.01   *** p < 0.001"
  ) +
  theme_bw(base_size = 13) +
  theme(plot.title       = element_text(face = "bold"),
        plot.subtitle    = element_text(size = 10, color = "gray40"),
        legend.position  = "bottom",
        legend.title     = element_text(face = "bold"),
        axis.text.y      = element_text(size = 11),
        panel.grid.minor = element_blank())

ggsave(file.path(out_figuras, "coefs_ocupacion_interacciones.png"),
       fig4, width = 9, height = 6, dpi = 150, bg = "white")
cat("✓ Figura 4 guardada\n")


# ---- Figura 5 -> 1er perfil vs muestra 

# ---- Modelo solo primer perfil ----
m_ocup_int_p1 <- lm_robust(
  ans_p1_global ~
    perfil_ocupacion * perfil_genero +
    perfil_ocupacion * perfil_parental +
    perfil_ocupacion * perfil_plan_futuro +
    perfil_estado_civil +
    perfil_exp + perfil_desempeno + template_contexto,
  data     = df %>% filter(pantalla_orden == "1"),
  clusters = numericalId,
  se_type  = "CR2"
)

coefs_int_p1 <- tidy(m_ocup_int_p1) %>%
  filter(str_detect(term, "perfil_ocupacion"),
         !str_detect(term, "Intercept")) %>%
  mutate(
    term_label = case_when(
      !str_detect(term, ":")              ~ "Ocupación: profesional (efecto principal)",
      str_detect(term, "Mujer")           ~ "Profesional × Mujer",
      str_detect(term, "tiene un hijo")   ~ "Profesional × Un hijo",
      str_detect(term, "tiene dos hijos") ~ "Profesional × Dos hijos",
      str_detect(term, "en 2 años")       ~ "Profesional × Plan 2 años",
      str_detect(term, "en 5 años")       ~ "Profesional × Plan 5 años",
      TRUE ~ term
    ),
    tipo = ifelse(str_detect(term, ":"),
                  "Interacción con ocupación",
                  "Efecto principal ocupación"),
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ ".",
      TRUE            ~ ""
    )
  )

coefs_comparacion <- bind_rows(
  coefs_interacciones %>% mutate(muestra = "Muestra completa"),
  coefs_int_p1        %>% mutate(muestra = "Solo 1er perfil")
) %>%
  mutate(
    muestra    = factor(muestra, levels = c("Muestra completa", "Solo 1er perfil")),
    term_label = factor(term_label, levels = c(
      "Ocupación: profesional (efecto principal)",
      "Profesional × Mujer",
      "Profesional × Un hijo",
      "Profesional × Dos hijos",
      "Profesional × Plan 2 años",
      "Profesional × Plan 5 años"
    ))
  )

fig5 <- ggplot(coefs_comparacion,
               aes(x = estimate, y = term_label,
                   color = tipo, shape = muestra)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "gray50", linewidth = 0.5) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2, linewidth = 0.7, alpha = 0.7,
                 position = position_dodge(width = 0.6)) +
  geom_point(size = 3.5,
             position = position_dodge(width = 0.6)) +
  geom_text(
    data = coefs_comparacion %>% filter(p.value < 0.1),
    aes(label = sig, x = conf.high + 0.02),
    size = 4, fontface = "bold", color = "black",
    position = position_dodge(width = 0.6)
  ) +
  scale_color_manual(values = c(
    "Efecto principal ocupación" = "#a65628",
    "Interacción con ocupación"  = "#984ea3"
  )) +
  scale_shape_manual(values = c(
    "Muestra completa" = 16,
    "Solo 1er perfil"  = 17
  )) +
  facet_wrap(~ tipo, ncol = 1, scales = "free_y") +
  labs(
    title    = "Variación efecto de ocupación -> primer perfil (vs all muestra)",
    subtitle = paste0(
    ),
    x       = "Coeficiente estimado (desviación del promedio del diseño)",
    y       = NULL,
    color   = "Tipo de término",
    shape   = "Muestra",
    caption = ". p < 0.1   * p < 0.05   ** p < 0.01   *** p < 0.001"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(size = 10, color = "gray40"),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold"),
    axis.text.y      = element_text(size = 11),
    strip.background = element_rect(fill = "#f0f0f0"),
    strip.text       = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(out_figuras, "coefs_ocupacion_int_vs_p1.png"),
       fig5, width = 10, height = 7, dpi = 150, bg = "white")
cat("✓ Figura guardada:", file.path(out_figuras, "coefs_ocupacion_int_vs_p1.png"), "\n")



# ---- Figura 6 -> 1er perfil vs muestra valores esperados
#  Valores esperados contratación
#         Ocupación × Estado parental — solo 1er perfil


# ---- Modelo: ocupación × parental, solo 1er perfil ----
m_ocup_parental_p1 <- lm(
  ans_p1_global ~
    perfil_ocupacion * perfil_parental +
    perfil_genero + perfil_estado_civil +
    perfil_plan_futuro +
    perfil_exp + perfil_desempeno + template_contexto,
  data = df %>% filter(pantalla_orden == "1")
)

cat("✓ Modelo ocupación × parental (1er perfil) estimado\n")


# ---- Grilla: 6 celdas (2 ocupaciones × 3 niveles parentales) ----
base_ocup_parental <- covariables_base(
  df %>% filter(pantalla_orden == "1"),
  excluir = c("perfil_ocupacion", "perfil_parental")
)

grilla_ocup_parental <- expand.grid(
  perfil_ocupacion = levels(df$perfil_ocupacion),
  perfil_parental  = levels(df$perfil_parental),
  stringsAsFactors = FALSE
) %>%
  mutate(
    perfil_ocupacion = factor(perfil_ocupacion,
                              levels = levels(df$perfil_ocupacion)),
    perfil_parental  = factor(perfil_parental,
                              levels = levels(df$perfil_parental))
  )

contrasts(grilla_ocup_parental$perfil_ocupacion) <- contrasts(df$perfil_ocupacion)
contrasts(grilla_ocup_parental$perfil_parental)  <- contrasts(df$perfil_parental)

newdata_ocup_parental <- grilla_ocup_parental %>%
  bind_cols(base_ocup_parental[rep(1, nrow(grilla_ocup_parental)), ]) %>%
  aplicar_contrastes(df %>% filter(pantalla_orden == "1"))

preds_ocup_parental <- predict(m_ocup_parental_p1,
                               newdata = newdata_ocup_parental,
                               se.fit  = TRUE)

ve_ocup_parental <- grilla_ocup_parental %>%
  mutate(
    ve      = preds_ocup_parental$fit,
    se      = preds_ocup_parental$se.fit,
    ci_low  = ve - 1.96 * se,
    ci_high = ve + 1.96 * se,
    ocupacion_label = ifelse(
      as.character(perfil_ocupacion) == "analista contable",
      "Técnico", "Profesional"),
    parental_label = case_when(
      as.character(perfil_parental) == "actualmente no tiene hijos" ~ "Sin hijos",
      as.character(perfil_parental) == "tiene un hijo"              ~ "Un hijo",
      as.character(perfil_parental) == "tiene dos hijos"            ~ "Dos hijos"
    ),
    parental_label = factor(parental_label,
                            levels = c("Sin hijos", "Un hijo", "Dos hijos"))
  )

cat("\n=== VE: OCUPACIÓN × PARENTAL (solo 1er perfil) ===\n")
ve_ocup_parental %>%
  select(ocupacion_label, parental_label, ve, ci_low, ci_high) %>%
  mutate(across(c(ve, ci_low, ci_high), ~ round(., 3))) %>%
  arrange(parental_label, ocupacion_label) %>%
  as.data.frame() %>% print()


# ---- Figura ----

col_ocup2 <- c("Técnico" = "#377eb8", "Profesional" = "#e41a1c")

fig6 <- ggplot(ve_ocup_parental,
               aes(x = parental_label, y = ve,
                   color = ocupacion_label,
                   group = ocupacion_label)) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high,
                  fill = ocupacion_label),
              alpha = 0.12, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.07, linewidth = 0.8, alpha = 0.8) +
  scale_color_manual(values = col_ocup2) +
  scale_fill_manual(values  = col_ocup2, guide = "none") +
  scale_y_continuous(limits = c(4, 7), breaks = seq(4, 7, 0.5)) +
  labs(
    title    = "Valores esperados de contratación: Ocupación × Estado parental",
    subtitle = paste0(
    ),
    x       = "Estado parental",
    y       = "Recomendación de contratación esperada (escala 1–7)",
    color   = "Tipo de ocupación",
    caption = "IC 95% aproximado. Solo primer perfil. Covariables fijadas en promedio del diseño."
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(size = 10, color = "gray40"),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(out_figuras, "ve_ocupacion_parental_p1.png"),
       fig6, width = 9, height = 6, dpi = 150, bg = "white")
cat("✓ Figura guardada:", file.path(out_figuras, "ve_ocupacion_parental_p1.png"), "\n")


# ---- Tabla ----
exportar_docx(
  df_tabla = ve_ocup_parental %>%
    arrange(parental_label, ocupacion_label) %>%
    select(
      "Estado parental" = parental_label,
      "Ocupación"       = ocupacion_label,
      "VE"              = ve,
      "SE"              = se,
      "IC inf. 95%"     = ci_low,
      "IC sup. 95%"     = ci_high
    ) %>%
    mutate(across(c(VE, SE, `IC inf. 95%`, `IC sup. 95%`), ~ round(., 3))),
  ruta   = file.path(out_tablas, "ve_ocupacion_parental_p1.docx"),
  titulo = "Valores esperados de contratación: Ocupación × Estado parental (solo 1er perfil)",
  nota   = "Nota: VE = valor esperado en escala 1–7. Solo primer perfil evaluado. Covariables fijadas en promedio del diseño (sum contrasts). IC 95% aproximado."
)



#Figura 6


# BLOQUE: Valores esperados contratación

col_ocup2 <- c("Técnico" = "#377eb8", "Profesional" = "#e41a1c")


# ---- Modelo base: ocupación × todas las interacciones, 1er perfil ----
# Un solo modelo para las tres comparaciones

m_base_p1 <- lm(
  ans_p1_global ~
    perfil_ocupacion * perfil_genero +
    perfil_ocupacion * perfil_parental +
    perfil_ocupacion * perfil_plan_futuro +
    perfil_estado_civil +
    perfil_exp + perfil_desempeno + template_contexto,
  data = df %>% filter(pantalla_orden == "1")
)

cat("✓ Modelo base (1er perfil) estimado\n")

df_p1 <- df %>% filter(pantalla_orden == "1")



# HELPER

calcular_ve_interaccion <- function(modelo, var_interaccion,
                                    niveles_label, df_ref) {
  
  base <- covariables_base(df_ref, excluir = c("perfil_ocupacion",
                                               var_interaccion))
  
  grilla <- expand.grid(
    perfil_ocupacion   = levels(df_ref$perfil_ocupacion),
    var_interes        = levels(df_ref[[var_interaccion]]),
    stringsAsFactors   = FALSE
  ) %>%
    rename(!!var_interaccion := var_interes) %>%
    mutate(
      perfil_ocupacion = factor(perfil_ocupacion,
                                levels = levels(df_ref$perfil_ocupacion)),
      !!var_interaccion := factor(.data[[var_interaccion]],
                                  levels = levels(df_ref[[var_interaccion]]))
    )
  
  contrasts(grilla$perfil_ocupacion)    <- contrasts(df_ref$perfil_ocupacion)
  contrasts(grilla[[var_interaccion]])  <- contrasts(df_ref[[var_interaccion]])
  
  newdata <- grilla %>%
    bind_cols(base[rep(1, nrow(grilla)), ]) %>%
    aplicar_contrastes(df_ref)
  
  preds <- predict(modelo, newdata = newdata, se.fit = TRUE)
  
  grilla %>%
    mutate(
      ve      = preds$fit,
      se      = preds$se.fit,
      ci_low  = ve - 1.96 * se,
      ci_high = ve + 1.96 * se,
      ocupacion_label = ifelse(
        as.character(perfil_ocupacion) == "analista contable",
        "Técnico", "Profesional"),
      x_label = niveles_label[as.character(.data[[var_interaccion]])]
    )
}



# VE 1: OCUPACIÓN × GÉNERO


niveles_genero <- c("Hombre" = "Hombre", "Mujer" = "Mujer")

ve_gen <- calcular_ve_interaccion(
  m_base_p1, "perfil_genero", niveles_genero, df_p1
) %>%
  mutate(x_label = factor(x_label, levels = c("Hombre", "Mujer")))

cat("\n=== VE: OCUPACIÓN × GÉNERO (1er perfil) ===\n")
ve_gen %>%
  select(ocupacion_label, x_label, ve, ci_low, ci_high) %>%
  mutate(across(c(ve, ci_low, ci_high), ~ round(., 3))) %>%
  arrange(x_label, ocupacion_label) %>%
  as.data.frame() %>% print()

fig_gen <- ggplot(ve_gen,
                  aes(x = x_label, y = ve,
                      color = ocupacion_label,
                      group = ocupacion_label)) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high, fill = ocupacion_label),
              alpha = 0.12, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.07, linewidth = 0.8, alpha = 0.8) +
  scale_color_manual(values = col_ocup2) +
  scale_fill_manual(values  = col_ocup2, guide = "none") +
  scale_y_continuous(limits = c(4, 7), breaks = seq(4, 7, 0.5)) +
  labs(
    title    = "Valores esperados de contratación: Ocupación × Género",
    subtitle = paste0(
    ),
    x       = "Género",
    y       = "Recomendación de contratación esperada (escala 1–7)",
    color   = "Tipo de ocupación",
    caption = "IC 95% aproximado. Solo primer perfil. Covariables fijadas en promedio del diseño."
  ) +
  theme_bw(base_size = 13) +
  theme(plot.title       = element_text(face = "bold"),
        plot.subtitle    = element_text(size = 10, color = "gray40"),
        legend.position  = "bottom",
        legend.title     = element_text(face = "bold"),
        panel.grid.minor = element_blank())

ggsave(file.path(out_figuras, "ve_ocupacion_genero_p1.png"),
       fig_gen, width = 8, height = 6, dpi = 150, bg = "white")
cat("✓ Figura ocupación × género guardada\n")

exportar_docx(
  df_tabla = ve_gen %>%
    arrange(x_label, ocupacion_label) %>%
    select("Género" = x_label, "Ocupación" = ocupacion_label,
           "VE" = ve, "SE" = se,
           "IC inf. 95%" = ci_low, "IC sup. 95%" = ci_high) %>%
    mutate(across(c(VE, SE, `IC inf. 95%`, `IC sup. 95%`), ~ round(., 3))),
  ruta   = file.path(out_tablas, "ve_ocupacion_genero_p1.docx"),
  titulo = "Valores esperados: Ocupación × Género (solo 1er perfil)",
  nota   = "Nota: VE = valor esperado escala 1–7. Solo primer perfil. Sum contrasts."
)



# VE 2: OCUPACIÓN × ESTADO PARENTAL

niveles_parental <- c(
  "actualmente no tiene hijos" = "Sin hijos",
  "tiene un hijo"              = "Un hijo",
  "tiene dos hijos"            = "Dos hijos"
)

ve_par <- calcular_ve_interaccion(
  m_base_p1, "perfil_parental", niveles_parental, df_p1
) %>%
  mutate(x_label = factor(x_label,
                          levels = c("Sin hijos", "Un hijo", "Dos hijos")))

cat("\n=== VE: OCUPACIÓN × PARENTAL (1er perfil) ===\n")
ve_par %>%
  select(ocupacion_label, x_label, ve, ci_low, ci_high) %>%
  mutate(across(c(ve, ci_low, ci_high), ~ round(., 3))) %>%
  arrange(x_label, ocupacion_label) %>%
  as.data.frame() %>% print()

fig_par <- ggplot(ve_par,
                  aes(x = x_label, y = ve,
                      color = ocupacion_label,
                      group = ocupacion_label)) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high, fill = ocupacion_label),
              alpha = 0.12, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.07, linewidth = 0.8, alpha = 0.8) +
  scale_color_manual(values = col_ocup2) +
  scale_fill_manual(values  = col_ocup2, guide = "none") +
  scale_y_continuous(limits = c(4, 7), breaks = seq(4, 7, 0.5)) +
  labs(
    title    = "Valores esperados de contratación: Ocupación × Estado parental",
    subtitle = paste0(
    ),
    x       = "Estado parental",
    y       = "Recomendación de contratación esperada (escala 1–7)",
    color   = "Tipo de ocupación",
    caption = "IC 95% aproximado. Solo primer perfil. Covariables fijadas en promedio del diseño."
  ) +
  theme_bw(base_size = 13) +
  theme(plot.title       = element_text(face = "bold"),
        plot.subtitle    = element_text(size = 10, color = "gray40"),
        legend.position  = "bottom",
        legend.title     = element_text(face = "bold"),
        panel.grid.minor = element_blank())

ggsave(file.path(out_figuras, "ve_ocupacion_parental_p1.png"),
       fig_par, width = 9, height = 6, dpi = 150, bg = "white")
cat("✓ Figura ocupación × parental guardada\n")

exportar_docx(
  df_tabla = ve_par %>%
    arrange(x_label, ocupacion_label) %>%
    select("Estado parental" = x_label, "Ocupación" = ocupacion_label,
           "VE" = ve, "SE" = se,
           "IC inf. 95%" = ci_low, "IC sup. 95%" = ci_high) %>%
    mutate(across(c(VE, SE, `IC inf. 95%`, `IC sup. 95%`), ~ round(., 3))),
  ruta   = file.path(out_tablas, "ve_ocupacion_parental_p1.docx"),
  titulo = "Valores esperados: Ocupación × Estado parental (solo 1er perfil)",
  nota   = "Nota: VE = valor esperado escala 1–7. Solo primer perfil. Sum contrasts."
)



# VE 3: OCUPACIÓN × PLAN FAMILIAR


niveles_plan <- c(
  "no quiere"  = "No quiere hijos",
  "en 2 años"  = "En 2 años",
  "en 5 años"  = "En 5 años"
)

ve_plan <- calcular_ve_interaccion(
  m_base_p1, "perfil_plan_futuro", niveles_plan, df_p1
) %>%
  mutate(x_label = factor(x_label,
                          levels = c("No quiere hijos", "En 2 años", "En 5 años")))

cat("\n=== VE: OCUPACIÓN × PLAN FAMILIAR (1er perfil) ===\n")
ve_plan %>%
  select(ocupacion_label, x_label, ve, ci_low, ci_high) %>%
  mutate(across(c(ve, ci_low, ci_high), ~ round(., 3))) %>%
  arrange(x_label, ocupacion_label) %>%
  as.data.frame() %>% print()

fig_plan <- ggplot(ve_plan,
                   aes(x = x_label, y = ve,
                       color = ocupacion_label,
                       group = ocupacion_label)) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high, fill = ocupacion_label),
              alpha = 0.12, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.07, linewidth = 0.8, alpha = 0.8) +
  scale_color_manual(values = col_ocup2) +
  scale_fill_manual(values  = col_ocup2, guide = "none") +
  scale_y_continuous(limits = c(4, 7), breaks = seq(4, 7, 0.5)) +
  labs(
    title    = "Valores esperados de contratación: Ocupación × Plan familiar",
    subtitle = paste0(
    ),
    x       = "Plan reproductivo",
    y       = "Recomendación de contratación esperada (escala 1–7)",
    color   = "Tipo de ocupación",
    caption = "IC 95% aproximado. Solo primer perfil. Covariables fijadas en promedio del diseño."
  ) +
  theme_bw(base_size = 13) +
  theme(plot.title       = element_text(face = "bold"),
        plot.subtitle    = element_text(size = 10, color = "gray40"),
        legend.position  = "bottom",
        legend.title     = element_text(face = "bold"),
        panel.grid.minor = element_blank())

ggsave(file.path(out_figuras, "ve_ocupacion_plan_p1.png"),
       fig_plan, width = 9, height = 6, dpi = 150, bg = "white")
cat("✓ Figura ocupación × plan familiar guardada\n")

exportar_docx(
  df_tabla = ve_plan %>%
    arrange(x_label, ocupacion_label) %>%
    select("Plan reproductivo" = x_label, "Ocupación" = ocupacion_label,
           "VE" = ve, "SE" = se,
           "IC inf. 95%" = ci_low, "IC sup. 95%" = ci_high) %>%
    mutate(across(c(VE, SE, `IC inf. 95%`, `IC sup. 95%`), ~ round(., 3))),
  ruta   = file.path(out_tablas, "ve_ocupacion_plan_p1.docx"),
  titulo = "Valores esperados: Ocupación × Plan familiar (solo 1er perfil)",
  nota   = "Nota: VE = valor esperado escala 1–7. Solo primer perfil. Sum contrasts."
)

