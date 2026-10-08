### Considerar reduccion de outcomes
# Instalar si no está
if (!requireNamespace("broom.mixed", quietly = TRUE)) {
  install.packages("broom.mixed")
}
library(broom.mixed)
# Diagnóstico: correlación entre outcomes (piloto)
library(corrr)

df %>%
  select(all_of(all_outcomes)) %>%
  correlate(quiet = TRUE) %>%
  shave() %>%
  fashion() %>%
  as.data.frame() %>%
  print()

# Alpha por cluster propuesto
# (excluyendo ans_p2_5 y ans_p3_1 que no cargan limpiamente)

clusters_propuestos <- list(
  
  "Evaluación positiva" = c(
    "ans_p2_1", "ans_p2_2", "ans_p2_3", "ans_p2_4",
    "ans_p3_5", "ans_p3_6"
  ),
  
  "Riesgo conductual" = c(
    "ans_p3_2", "ans_p3_3", "ans_p3_4"
  ),
  
  "Costo percibido" = c(
    "ans_p2_6", "ans_p2_7", "ans_p3_7", "ans_p3_8"
  )
)

cat("=== Alpha de Cronbach por cluster ===\n\n")

alphas <- map_dfr(names(clusters_propuestos), function(nombre) {
  items <- clusters_propuestos[[nombre]]
  a     <- psych::alpha(df %>% select(all_of(items)), check.keys = TRUE)
  
  tibble(
    cluster        = nombre,
    n_items        = length(items),
    alpha          = round(a$total$raw_alpha, 3),
    alpha_std      = round(a$total$std.alpha, 3),
    interpretacion = case_when(
      a$total$raw_alpha >= 0.80 ~ "✓ Buena consistencia",
      a$total$raw_alpha >= 0.65 ~ "✓ Aceptable",
      a$total$raw_alpha >= 0.50 ~ "⚠ Moderada — usar con cautela",
      TRUE                      ~ "✗ Baja — no agregar"
    )
  )
})

as.data.frame(alphas) %>% print()


# --- Crear índices si alphas son aceptables ---

cat("\n=== Creando índices como media de ítems ===\n")

df <- df %>%
  rowwise() %>%
  mutate(
    idx_evaluacion_pos = mean(c_across(all_of(
      clusters_propuestos[["Evaluación positiva"]])),
      na.rm = TRUE),
    
    idx_riesgo_conductual = mean(c_across(all_of(
      clusters_propuestos[["Riesgo conductual"]])),
      na.rm = TRUE),
    
    idx_costo_percibido = mean(c_across(all_of(
      clusters_propuestos[["Costo percibido"]])),
      na.rm = TRUE)
  ) %>%
  ungroup()

cat("✓ Índices creados\n\n")

# Verificar distribución de índices
df %>%
  select(starts_with("idx_")) %>%
  summary() %>%
  print()


# --- ¿Qué hacer con Ambicioso/a y Usará beneficios? ---

cat("\n=== Decisión sobre ítems sin cluster claro ===\n\n")

items_sin_cluster <- tribble(
  ~item,         ~label,                   ~correlacion_max, ~decision_sugerida,
  "ans_p2_5",    "Ambicioso/a",            ".38",
  "Mantener separado — mide dimensión distinta (agencia)",
  "ans_p3_1",    "Usará beneficios empresa", ".44",
  "Incluir en riesgo conductual o eliminar — baja correlación con los otros dos"
)

as.data.frame(items_sin_cluster) %>% print()




# Verificar techo/piso en cada índice

techo_piso <- df %>%
  select(starts_with("idx_")) %>%
  pivot_longer(everything(),
               names_to  = "indice",
               values_to = "valor") %>%
  group_by(indice) %>%
  summarise(
    media       = round(mean(valor,  na.rm = TRUE), 2),
    sd          = round(sd(valor,    na.rm = TRUE), 2),
    pct_maximo  = round(100 * mean(valor == max(valor, na.rm = TRUE),
                                   na.rm = TRUE), 1),
    pct_minimo  = round(100 * mean(valor == min(valor, na.rm = TRUE),
                                   na.rm = TRUE), 1),
    asimetria   = round(psych::skew(valor, na.rm = TRUE), 2),
    alerta      = case_when(
      pct_maximo > 15 ~ "⚠ Efecto techo",
      pct_minimo > 15 ~ "⚠ Efecto piso",
      TRUE            ~ "✓ OK"
    ),
    .groups = "drop"
  )

as.data.frame(techo_piso) %>% print()


# Histogramas de los índices
df %>%
  select(starts_with("idx_")) %>%
  pivot_longer(everything(),
               names_to  = "indice",
               values_to = "valor") %>%
  mutate(indice = recode(indice,
                         "idx_evaluacion_pos"    = "Evaluación positiva",
                         "idx_riesgo_conductual" = "Riesgo conductual",
                         "idx_costo_percibido"   = "Costo percibido"
  )) %>%
  ggplot(aes(x = valor)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue",
                 color = "white", alpha = 0.85) +
  facet_wrap(~ indice, ncol = 1, scales = "free_y") +
  labs(
    title    = "Distribución de índices agregados",
    subtitle = "Verificación de efectos techo/piso",
    x = "Valor del índice", y = "Frecuencia"
  ) +
  theme_bw(base_size = 11)

ggsave("figuras/diagnosticos/distribucion_indices.png",
       width = 8, height = 8, dpi = 150, bg = "white")
cat("✓ Figura guardada\n")








# ============================================================
# REEMPLAZAR sección 3 (DEFINICIÓN DE OUTCOMES) en tu script
# ============================================================

# Crear índices (agregar después de sección 2 - preparación de variables)
df <- df %>%
  rowwise() %>%
  mutate(
    idx_evaluacion_pos    = mean(c_across(all_of(
      c("ans_p2_1", "ans_p2_2", "ans_p2_3",
        "ans_p2_4", "ans_p3_5", "ans_p3_6"))),
      na.rm = TRUE),
    
    idx_riesgo_conductual = mean(c_across(all_of(
      c("ans_p3_2", "ans_p3_3", "ans_p3_4"))),
      na.rm = TRUE),
    
    idx_costo_percibido   = mean(c_across(all_of(
      c("ans_p2_6", "ans_p2_7",
        "ans_p3_7", "ans_p3_8"))),
      na.rm = TRUE)
  ) %>%
  ungroup()


# ============================================================
# NUEVA sección 3: outcomes reducidos
# ============================================================

# Outcomes principales del estudio
outcomes_principales <- c(
  "ans_p1_global",        # Recomendación contratación
  "idx_evaluacion_pos",   # Competencia y potencial
  "idx_riesgo_conductual",# Mecanismo FCR: ausentismo/renuncia
  "idx_costo_percibido",  # Mecanismo FCR: costos
  "ans_p2_5"              # Agencia percibida (Ambicioso/a)
)

# Outcomes secundarios (apéndice)
# ans_p3_1 va aquí o al índice de riesgo según alpha
outcomes_apendice <- c(
  "ans_p3_1",   # Usará beneficios — decisión pendiente
  # Los 16 outcomes individuales para verificación de robustez
  all_outcomes
)

labels_outcomes_red <- c(
  "ans_p1_global"         = "Recomendación de contratación",
  "idx_evaluacion_pos"    = "Evaluación positiva (índice)",
  "idx_riesgo_conductual" = "Riesgo conductual (índice)",
  "idx_costo_percibido"   = "Costo percibido (índice)",
  "ans_p2_5"              = "Ambicioso/a"
)


# ============================================================
# NUEVA sección 4: sets de modelos simplificados
# ============================================================

sets_formulas_principal <- list(
  
  # M1: Efectos principales
  # Pregunta: ¿hay penalización por género, parental, plan?
  "M1_principales" = paste(
    "perfil_genero + perfil_estado_civil + perfil_parental +",
    "perfil_plan_futuro + perfil_exp + perfil_desempeno +",
    "pantalla_orden + template_contexto"
  ),
  
  # M2: Interacciones de dos vías género × FCR
  # Pregunta: ¿la penalización opera diferente para mujeres?
  "M2_genero_x_fcr" = paste(
    "perfil_genero * perfil_parental +",
    "perfil_genero * perfil_plan_futuro +",
    "perfil_estado_civil + perfil_exp + perfil_desempeno +",
    "pantalla_orden + template_contexto"
  )
)

# Sets de apéndice (tus sets 3, 4a, 4b originales)
sets_formulas_apendice <- list(
  
  "A1_gen_x_ecivil" = paste(
    "perfil_genero * perfil_estado_civil +",
    "perfil_parental + perfil_plan_futuro +",
    "perfil_exp + perfil_desempeno +",
    "pantalla_orden + template_contexto"
  ),
  
  "A2_gen_x_ecivil_x_parental" = paste(
    "perfil_genero * perfil_estado_civil * perfil_parental +",
    "perfil_plan_futuro +",
    "perfil_exp + perfil_desempeno +",
    "pantalla_orden + template_contexto"
  ),
  
  "A3_gen_x_parental_x_plan" = paste(
    "perfil_genero * perfil_parental * perfil_plan_futuro +",
    "perfil_estado_civil +",
    "perfil_exp + perfil_desempeno +",
    "pantalla_orden + template_contexto"
  )
)


# ============================================================
# NUEVA sección 5: correr modelos
# ============================================================

# Modelos principales con RI
correr_modelos_ri <- function(formula_str, outcomes, datos) {
  map(outcomes, function(y) {
    fml <- as.formula(paste(y, "~", formula_str, "+ (1 | numericalId)"))
    tryCatch(
      lmer(fml, data = datos, REML = TRUE),
      error   = function(e) {
        message(sprintf("Error en %s: %s", y, e$message))
        NULL
      }
    )
  }) %>% set_names(outcomes)
}

# Correr principales
resultados_principales <- map(
  sets_formulas_principal,
  ~ correr_modelos_ri(.x, outcomes_principales, df)
)

# Correr apéndice (clustered SE es suficiente para robustez)
resultados_apendice <- map(
  sets_formulas_apendice,
  ~ map(outcomes_principales, function(y) {
    fml <- as.formula(paste(y, "~", .x))
    lm_robust(fml, data = df, clusters = numericalId, se_type = "CR2")
  }) %>% set_names(outcomes_principales)
)

cat("✓ Modelos principales corridos:",
    length(sets_formulas_principal), "sets ×",
    length(outcomes_principales), "outcomes =",
    length(sets_formulas_principal) * length(outcomes_principales),
    "modelos\n")

cat("✓ Modelos apéndice corridos:",
    length(sets_formulas_apendice), "sets ×",
    length(outcomes_principales), "outcomes =",
    length(sets_formulas_apendice) * length(outcomes_principales),
    "modelos\n")


# ============================================================
# NUEVA sección 6: exportar tablas
# ============================================================

exportar_tabla_ri <- function(lista_modelos, outcomes,
                              labels, nombre_archivo) {
  modelos_sel        <- lista_modelos[outcomes]
  names(modelos_sel) <- labels[outcomes]
  
  coef_map <- c(
    "perfil_generoMujer"             = "Género: Mujer",
    "perfil_estado_civilcasado/a"    = "E. civil: Casado/a",
    "perfil_estado_civilconviviendo" = "E. civil: Conviviendo",
    "perfil_parentaltiene un hijo"   = "Parental: 1 hijo",
    "perfil_parentaltiene dos hijos" = "Parental: 2 hijos",
    "perfil_plan_futuroen 2 años"    = "Plan: en 2 años",
    "perfil_plan_futuroen 5 años"    = "Plan: en 5 años",
    "perfil_generoMujer:perfil_parentaltiene un hijo"   = "Mujer × 1 hijo",
    "perfil_generoMujer:perfil_parentaltiene dos hijos" = "Mujer × 2 hijos",
    "perfil_generoMujer:perfil_plan_futuroen 2 años"    = "Mujer × Plan 2 años",
    "perfil_generoMujer:perfil_plan_futuroen 5 años"    = "Mujer × Plan 5 años",
    "perfil_exp6 años"                                  = "Experiencia: 6 años",
    "perfil_exp8 años"                                  = "Experiencia: 8 años",
    "perfil_desempeno6.0"                               = "Desempeño: 6.0",
    "perfil_desempeno6.3"                               = "Desempeño: 6.3",
    "pantalla_orden2"                                   = "Orden: 2do perfil",
    "template_contextoB"                                = "Template: B"
  )
  
  modelsummary(
    modelos_sel,
    coef_map = coef_map,
    stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    gof_map  = list(
      list(raw = "nobs",   clean = "N observaciones", fmt = 0),
      list(raw = "icc",    clean = "ICC (ρ)",          fmt = 3),
      list(raw = "logLik", clean = "Log-likelihood",   fmt = 1)
    ),
    notes  = c(
      "Modelos de intercepto aleatorio (lme4/lmerTest).",
      "Ref: Hombre, Soltero/a, Sin hijos, Sin planes.",
      "Índices: media simple de ítems constitutivos."
    ),
    output = nombre_archivo
  )
  cat("✓ Tabla guardada:", nombre_archivo, "\n")
}

# Exportar M1 y M2
exportar_tabla_ri(resultados_principales[["M1_principales"]],
                  outcomes_principales,
                  labels_outcomes_red,
                  "tablas/M1_principales.docx")

exportar_tabla_ri(resultados_principales[["M2_genero_x_fcr"]],
                  outcomes_principales,
                  labels_outcomes_red,
                  "tablas/M2_genero_x_fcr.docx")

cat("✓ Tablas principales exportadas\n")



# ============================================================
# DIAGNÓSTICO: ¿Los efectos en M2 enmascaran heterogeneidad
# por ocupación?
# ============================================================

formula_m2_str <- paste(
  "perfil_genero * perfil_parental +",
  "perfil_genero * perfil_plan_futuro +",
  "perfil_estado_civil + perfil_exp + perfil_desempeno +",
  "pantalla_orden + template_contexto"
)

outcomes_red <- c(
  "ans_p1_global",
  "idx_evaluacion_pos",
  "idx_riesgo_conductual",
  "idx_costo_percibido",
  "ans_p2_5"
)

terminos_fcr <- c(
  "perfil_generoMujer",
  "perfil_parentaltiene un hijo",
  "perfil_parentaltiene dos hijos",
  "perfil_plan_futuroen 2 años",
  "perfil_plan_futuroen 5 años",
  "perfil_generoMujer:perfil_parentaltiene un hijo",
  "perfil_generoMujer:perfil_parentaltiene dos hijos",
  "perfil_generoMujer:perfil_plan_futuroen 2 años",
  "perfil_generoMujer:perfil_plan_futuroen 5 años"
)

etiquetas_fcr <- c(
  "perfil_generoMujer"                                = "Género: Mujer",
  "perfil_parentaltiene un hijo"                      = "Parental: 1 hijo",
  "perfil_parentaltiene dos hijos"                    = "Parental: 2 hijos",
  "perfil_plan_futuroen 2 años"                       = "Plan: 2 años",
  "perfil_plan_futuroen 5 años"                       = "Plan: 5 años",
  "perfil_generoMujer:perfil_parentaltiene un hijo"   = "Mujer × 1 hijo",
  "perfil_generoMujer:perfil_parentaltiene dos hijos" = "Mujer × 2 hijos",
  "perfil_generoMujer:perfil_plan_futuroen 2 años"    = "Mujer × Plan 2 años",
  "perfil_generoMujer:perfil_plan_futuroen 5 años"    = "Mujer × Plan 5 años"
)


# --- 1. Correr M2 separado por ocupación ---
correr_m2_subgrupo <- function(ocup_nivel) {
  df_sub <- df %>%
    filter(str_detect(as.character(perfil_ocupacion), ocup_nivel))
  
  cat(sprintf("\nSubgrupo: %s | N obs = %d | N resp = %d\n",
              ocup_nivel, nrow(df_sub), n_distinct(df_sub$numericalId)))
  
  map_dfr(outcomes_red, function(y) {
    fml <- as.formula(paste(y, "~", formula_m2_str, "+ (1 | numericalId)"))
    
    m <- tryCatch(
      lmer(fml, data = df_sub, REML = TRUE),
      error   = function(e) NULL,
      warning = function(w) suppressWarnings(
        lmer(fml, data = df_sub, REML = TRUE))
    )
    if (is.null(m)) return(NULL)
    
    broom.mixed::tidy(m) %>%          # <-- cambio aquí
      filter(term %in% terminos_fcr) %>%
      mutate(
        ocupacion     = ocup_nivel,
        outcome       = y,
        outcome_label = labels_outcomes_red[y],
        term_label    = etiquetas_fcr[term],
        sig = case_when(
          p.value < .05 ~ "*",
          p.value < .10 ~ ".",
          TRUE          ~ ""
        )
      )
  })
}

# Y para el poolado:
res_poolado <- map_dfr(outcomes_red, function(y) {
  fml <- as.formula(paste(y, "~", formula_m2_str, "+ (1 | numericalId)"))
  m   <- tryCatch(
    lmer(fml, data = df, REML = TRUE),
    error   = function(e) NULL,
    warning = function(w) suppressWarnings(lmer(fml, data = df, REML = TRUE))
  )
  if (is.null(m)) return(NULL)
  
  broom.mixed::tidy(m) %>%           # <-- cambio aquí
    filter(term %in% terminos_fcr) %>%
    mutate(
      ocupacion     = "Poolado",
      outcome       = y,
      outcome_label = labels_outcomes_red[y],
      term_label    = etiquetas_fcr[term],
      sig = case_when(
        p.value < .05 ~ "*",
        p.value < .10 ~ ".",
        TRUE          ~ ""
      )
    )
})

# Volver a correr
res_tec  <- correr_m2_subgrupo(nivel_tecnico)
res_prof <- correr_m2_subgrupo(nivel_profesional)

cat(sprintf("Filas técnico: %d | Filas profesional: %d | Filas poolado: %d\n",
            nrow(res_tec), nrow(res_prof), nrow(res_poolado)))



# Test rápido
m_test <- lmer(ans_p1_global ~ perfil_genero + (1 | numericalId),
               data = df, REML = TRUE)
cat("Clase del modelo:", class(m_test), "\n")
broom.mixed::tidy(m_test) %>% head() %>% print()





# --- 2. Comparar con modelo poolado ---

res_poolado <- map_dfr(outcomes_red, function(y) {
  fml <- as.formula(paste(y, "~", formula_m2_str, "+ (1 | numericalId)"))
  m   <- tryCatch(
    lmer(fml, data = df, REML = TRUE),
    error   = function(e) NULL,
    warning = function(w) suppressWarnings(lmer(fml, data = df, REML = TRUE))
  )
  if (is.null(m)) return(NULL)
  
  broom::tidy(m) %>%
    filter(term %in% terminos_fcr) %>%
    mutate(
      ocupacion     = "Poolado",
      outcome       = y,
      outcome_label = labels_outcomes_red[y],
      term_label    = etiquetas_fcr[term],
      sig = case_when(
        p.value < .05 ~ "*",
        p.value < .10 ~ ".",
        TRUE          ~ ""
      )
    )
})


# --- 3. Tabla de los tres modelos lado a lado ---

cat("\n=== Comparación: Técnico vs Profesional vs Poolado ===\n\n")

res_tec_sel <- res_tec %>%
  select(outcome_label, term_label,
         est_tec  = estimate,
         se_tec   = std.error,
         sig_tec  = sig,
         pval_tec = p.value)

res_prof_sel <- res_prof %>%
  select(outcome_label, term_label,
         est_prof  = estimate,
         se_prof   = std.error,
         sig_prof  = sig,
         pval_prof = p.value)

res_pool_sel <- res_poolado %>%
  select(outcome_label, term_label,
         est_pool  = estimate,
         se_pool   = std.error,
         sig_pool  = sig,
         pval_pool = p.value)

tabla_comparacion <- res_tec_sel %>%
  left_join(res_prof_sel, by = c("outcome_label", "term_label")) %>%
  left_join(res_pool_sel, by = c("outcome_label", "term_label")) %>%
  mutate(
    # ¿Signos opuestos entre ocupaciones?
    signos_opuestos = sign(est_tec) != sign(est_prof),
    # ¿El poolado está entre los dos subgrupos?
    poolado_enmascarado = signos_opuestos &
      abs(est_pool) < pmax(abs(est_tec), abs(est_prof)) * 0.5,
    alerta = case_when(
      signos_opuestos & poolado_enmascarado ~ "🔴 Enmascaramiento severo",
      signos_opuestos                        ~ "🟡 Signos opuestos",
      abs(est_tec - est_prof) > 0.30        ~ "🟡 Magnitud muy diferente",
      TRUE                                   ~ "✓ Consistente"
    ),
    across(where(is.numeric), ~ round(.x, 3))
  )

# Imprimir casos problemáticos primero
cat("--- Casos con posible enmascaramiento ---\n\n")
tabla_comparacion %>%
  filter(str_detect(alerta, "🔴|🟡")) %>%
  select(outcome_label, term_label,
         est_tec, sig_tec,
         est_prof, sig_prof,
         est_pool, sig_pool,
         alerta) %>%
  arrange(alerta, outcome_label) %>%
  as.data.frame() %>%
  print()

cat("\n--- Casos consistentes entre ocupaciones ---\n\n")
tabla_comparacion %>%
  filter(str_detect(alerta, "✓")) %>%
  select(outcome_label, term_label,
         est_tec, sig_tec,
         est_prof, sig_prof,
         est_pool, sig_pool) %>%
  arrange(outcome_label, term_label) %>%
  as.data.frame() %>%
  print()

write_csv(tabla_comparacion,
          "tablas/enmascaramiento_por_ocupacion.csv")
cat("\n✓ Guardado en tablas/enmascaramiento_por_ocupacion.csv\n")


# --- 4. Figura: los tres modelos en paralelo ---

datos_fig <- bind_rows(
  res_tec     %>% mutate(modelo = "Técnico"),
  res_prof    %>% mutate(modelo = "Profesional"),
  res_poolado %>% mutate(modelo = "Poolado")
) %>%
  mutate(
    modelo = factor(modelo,
                    levels = c("Técnico", "Poolado", "Profesional")),
    term_label = factor(term_label, levels = etiquetas_fcr),
    outcome_label = factor(outcome_label,
                           levels = labels_outcomes_red)
  )

for (out in outcomes_red) {
  
  datos_out <- datos_fig %>%
    filter(outcome == out)
  
  # Añadir franja de fondo para términos con signos opuestos
  alertas_out <- tabla_comparacion %>%
    filter(outcome_label == labels_outcomes_red[out],
           str_detect(alerta, "🔴|🟡")) %>%
    pull(term_label)
  
  fig <- ggplot(datos_out,
                aes(x = estimate, y = term_label,
                    color = modelo, shape = modelo)) +
    # Franja de fondo para términos problemáticos
    {if (length(alertas_out) > 0)
      geom_rect(
        data = tibble(term_label = factor(alertas_out,
                                          levels = etiquetas_fcr)),
        aes(ymin = as.numeric(term_label) - 0.4,
            ymax = as.numeric(term_label) + 0.4,
            xmin = -Inf, xmax = Inf),
        fill = "lightyellow", alpha = 0.5,
        inherit.aes = FALSE
      )} +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0.2, linewidth = 0.7,
                   position = position_dodgev(height = 0.6)) +
    geom_point(size = 3,
               position = position_dodgev(height = 0.6)) +
    geom_text(
      data = datos_out %>% filter(p.value < .10),
      aes(label = sig, x = conf.high + 0.08),
      size = 3.5, color = "black", fontface = "bold",
      position = position_dodgev(height = 0.6)
    ) +
    scale_color_manual(values = c(
      "Técnico"     = "#377eb8",
      "Poolado"     = "gray40",
      "Profesional" = "#e41a1c"
    )) +
    scale_shape_manual(values = c(16, 4, 17)) +
    labs(
      title    = paste0("Enmascaramiento por ocupación — ",
                        labels_outcomes_red[out]),
      subtitle = paste0(
        "Fondo amarillo = signos opuestos entre ocupaciones\n",
        "Ref: Hombre, Soltero/a, Sin hijos, Sin planes"
      ),
      x      = "Coeficiente estimado (IC 95%)",
      y      = NULL,
      color  = "Modelo",
      shape  = "Modelo",
      caption = ". p<0.1  * p<0.05"
    ) +
    theme_bw(base_size = 11) +
    theme(legend.position = "bottom")
  
  nombre_fig <- paste0(
    "figuras/diagnosticos/enmascaramiento_",
    out, ".png"
  )
  ggsave(nombre_fig, fig,
         width = 10, height = 7, dpi = 150, bg = "white")
  cat("✓ Figura guardada:", nombre_fig, "\n")
}


# --- 5. Resumen ejecutivo ---

cat("\n=== RESUMEN: ¿Cuántos efectos están enmascarados? ===\n\n")

tabla_comparacion %>%
  count(alerta) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n)) %>%
  as.data.frame() %>%
  print()

cat("\n=== Por outcome: número de efectos problemáticos ===\n\n")

tabla_comparacion %>%
  group_by(outcome_label) %>%
  summarise(
    n_total         = n(),
    n_enmascarados  = sum(str_detect(alerta, "🔴"), na.rm = TRUE),
    n_opuestos      = sum(str_detect(alerta, "🟡"), na.rm = TRUE),
    n_consistentes  = sum(str_detect(alerta, "✓"), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  as.data.frame() %>%
  print()


# Ver exactamente cuáles términos están enmascarados
# para entender el patrón sustantivo

cat("=== Detalle de enmascaramiento severo (🔴) ===\n\n")

tabla_comparacion %>%
  filter(str_detect(alerta, "🔴")) %>%
  select(outcome_label, term_label,
         est_tec, sig_tec,
         est_prof, sig_prof,
         est_pool, sig_pool) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  arrange(outcome_label, term_label) %>%
  as.data.frame() %>%
  print()

# ¿Hay algún término que sea consistente en TODOS los outcomes?
cat("\n=== Términos robustos entre ocupaciones y outcomes ===\n\n")

tabla_comparacion %>%
  group_by(term_label) %>%
  summarise(
    n_consistentes     = sum(str_detect(alerta, "✓"), na.rm = TRUE),
    n_outcomes         = n_distinct(outcome_label),
    pct_consistente    = round(100 * n_consistentes / n_outcomes, 0),
    est_medio_tec      = round(mean(est_tec,  na.rm = TRUE), 3),
    est_medio_prof     = round(mean(est_prof, na.rm = TRUE), 3),
    mismo_signo        = sign(mean(est_tec, na.rm=TRUE)) ==
      sign(mean(est_prof, na.rm=TRUE)),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_consistente)) %>%
  as.data.frame() %>%
  print()





# ============================================================
# DIAGNÓSTICO DE OUTCOMES: SOLO PERFILES PROFESIONALES
# Evaluar cuáles outcomes aportan información única
# ============================================================

df_prof <- df %>%
  filter(str_detect(as.character(perfil_ocupacion), nivel_profesional))

cat(sprintf("N obs: %d | N respondentes: %d\n\n",
            nrow(df_prof), n_distinct(df_prof$numericalId)))


# --- 1. Correlaciones entre outcomes (solo profesional) ---

cat("=== Correlaciones entre outcomes — perfiles profesionales ===\n\n")

cor_prof <- df_prof %>%
  select(all_of(all_outcomes)) %>%
  cor(use = "pairwise.complete.obs")

rownames(cor_prof) <- colnames(cor_prof) <- labels_outcomes[all_outcomes]

# Imprimir matriz simplificada
corrplot::corrplot(
  cor_prof,
  method   = "color",
  order    = "hclust",
  addrect  = 4,
  tl.cex   = 0.7,
  tl.col   = "black",
  addCoef.col = "black",
  number.cex  = 0.45,
  col = colorRampPalette(c("#d01c8b", "white", "#4dac26"))(200),
  title = "Correlaciones entre outcomes — Perfiles profesionales",
  mar   = c(0, 0, 2, 0)
)

ggsave <- function(...) {} # placeholder por si corrplot no usa ggsave
png("figuras/diagnosticos/correlaciones_prof.png",
    width = 1400, height = 1200, res = 150)
corrplot::corrplot(
  cor_prof,
  method   = "color",
  order    = "hclust",
  addrect  = 4,
  tl.cex   = 0.7,
  tl.col   = "black",
  addCoef.col = "black",
  number.cex  = 0.45,
  col = colorRampPalette(c("#d01c8b", "white", "#4dac26"))(200),
  title = "Correlaciones — Perfiles profesionales",
  mar   = c(0, 0, 2, 0)
)
dev.off()
cat("✓ Figura guardada: figuras/diagnosticos/correlaciones_prof.png\n\n")


# ============================================================
# VERSIÓN CORREGIDA: Alpha por cluster en profesional
# ============================================================

alpha_prof_resumen   <- list()
alpha_prof_items     <- list()
alpha_prof_drop      <- list()

for (nombre in names(clusters_propuestos)) {
  items <- clusters_propuestos[[nombre]]
  a     <- psych::alpha(df_prof %>% select(all_of(items)),
                        check.keys = TRUE)
  
  # Resumen del cluster
  alpha_prof_resumen[[nombre]] <- tibble(
    cluster        = nombre,
    n_items        = length(items),
    alpha          = round(a$total$raw_alpha, 3),
    alpha_drop_min = round(min(a$alpha.drop[, "raw_alpha"]), 3),
    item_min_r     = rownames(a$item.stats)[which.min(a$item.stats$r.drop)],
    r_min          = round(min(a$item.stats$r.drop), 3)
  )
  
  # Item-total correlations
  alpha_prof_items[[nombre]] <- as.data.frame(a$item.stats) %>%
    tibble::rownames_to_column("item") %>%
    mutate(
      cluster            = nombre,
      label              = labels_outcomes[item],
      r_drop             = round(r.drop, 3),
      candidato_eliminar = r_drop < 0.40
    ) %>%
    select(cluster, item, label, r_drop, candidato_eliminar)
  
  # Alpha si se elimina cada ítem
  alpha_prof_drop[[nombre]] <- as.data.frame(a$alpha.drop) %>%
    tibble::rownames_to_column("item") %>%
    mutate(
      cluster        = nombre,
      label          = labels_outcomes[item],
      alpha_sin_item = round(raw_alpha, 3),
      alpha_actual   = round(a$total$raw_alpha, 3),
      mejora         = round(raw_alpha - a$total$raw_alpha, 3),
      decision       = case_when(
        mejora > 0.02  ~ "⚠ Eliminar — mejora alpha",
        mejora > 0     ~ "~ Marginal",
        TRUE           ~ "✓ Mantener"
      )
    ) %>%
    select(cluster, item, label,
           alpha_sin_item, alpha_actual, mejora, decision)
}

# --- Imprimir resultados ---

cat("=== Resumen alpha por cluster — perfiles profesionales ===\n\n")
bind_rows(alpha_prof_resumen) %>%
  as.data.frame() %>%
  print()

cat("\n=== Correlación ítem-total (r.drop) ===\n")
cat("r.drop < 0.40 = candidato a eliminar\n\n")
bind_rows(alpha_prof_items) %>%
  arrange(cluster, r_drop) %>%
  as.data.frame() %>%
  print()

cat("\n=== Alpha si se elimina cada ítem ===\n")
cat("mejora > 0.02 → el ítem perjudica el índice\n\n")
bind_rows(alpha_prof_drop) %>%
  arrange(cluster, desc(mejora)) %>%
  select(cluster, label, alpha_sin_item, alpha_actual, mejora, decision) %>%
  as.data.frame() %>%
  print()

# Guardar para referencia
write_csv(bind_rows(alpha_prof_items),
          "tablas/alpha_items_profesional.csv")
write_csv(bind_rows(alpha_prof_drop),
          "tablas/alpha_drop_profesional.csv")
cat("\n✓ Tablas guardadas\n")





# Resumen por cluster
cat("--- Resumen alpha por cluster ---\n")
map_dfr(alpha_prof, "resumen") %>%
  as.data.frame() %>%
  print()

# Item-total correlations
cat("\n--- Correlación ítem-total (r.drop) por cluster ---\n")
cat("r.drop < 0.40 = candidato a eliminar\n\n")
map_dfr(alpha_prof, "items") %>%
  arrange(cluster, r_drop) %>%
  as.data.frame() %>%
  print()

# Alpha si se elimina cada ítem
cat("\n--- Alpha si se elimina cada ítem ---\n")
cat("Si alpha_sin_item > alpha actual → el ítem perjudica la escala\n\n")

alpha_actual <- map_dfr(alpha_prof, "resumen") %>%
  select(cluster, alpha)

map_dfr(alpha_prof, "drop") %>%
  left_join(alpha_actual, by = "cluster") %>%
  mutate(
    mejora    = round(alpha_sin_item - alpha, 3),
    decision  = case_when(
      mejora > 0.02  ~ "⚠ Eliminar — mejora alpha",
      mejora > 0     ~ "~ Marginal",
      TRUE           ~ "✓ Mantener"
    )
  ) %>%
  arrange(cluster, desc(mejora)) %>%
  select(cluster, label, alpha_sin_item, alpha, mejora, decision) %>%
  as.data.frame() %>%
  print()


# --- 3. Efectos de M2 en profesional por outcome individual ---
# Para ver si los outcomes dentro de cada índice
# cuentan historias distintas o redundantes

cat("\n=== Efectos de M2 por outcome individual — profesional ===\n")
cat("Foco en términos FCR\n\n")

resultados_prof_ind <- map_dfr(all_outcomes, function(y) {
  fml <- as.formula(paste(y, "~", formula_m2_str, "+ (1 | numericalId)"))
  
  m <- tryCatch(
    lmer(fml, data = df_prof, REML = TRUE),
    error   = function(e) NULL,
    warning = function(w) suppressWarnings(
      lmer(fml, data = df_prof, REML = TRUE))
  )
  if (is.null(m)) return(NULL)
  
  tidy_lmer(m) %>%
    filter(term %in% terminos_fcr) %>%
    mutate(
      outcome       = y,
      outcome_label = labels_outcomes[y],
      cluster       = case_when(
        y %in% clusters_propuestos[["Evaluación positiva"]] ~ "Evaluación positiva",
        y %in% clusters_propuestos[["Riesgo conductual"]]   ~ "Riesgo conductual",
        y %in% clusters_propuestos[["Costo percibido"]]     ~ "Costo percibido",
        y == "ans_p1_global"                                 ~ "Contratación",
        y == "ans_p2_5"                                      ~ "Ambicioso/a",
        y == "ans_p3_1"                                      ~ "Sin cluster",
        TRUE                                                  ~ "Otro"
      ),
      term_label = etiquetas_fcr[term],
      sig = case_when(
        p.value < .05 ~ "*",
        p.value < .10 ~ ".",
        TRUE          ~ ""
      )
    )
})

# Tabla: variación de efectos DENTRO de cada cluster
# Si todos los ítems de un cluster cuentan la misma historia → índice OK
# Si hay ítems con efectos opuestos → el cluster es heterogéneo

cat("--- Variación dentro de clusters (desviación de la media del cluster) ---\n\n")

variacion_interna <- resultados_prof_ind %>%
  filter(cluster %in% names(clusters_propuestos)) %>%
  group_by(cluster, term_label) %>%
  summarise(
    est_medio   = round(mean(estimate, na.rm = TRUE), 3),
    est_min     = round(min(estimate,  na.rm = TRUE), 3),
    est_max     = round(max(estimate,  na.rm = TRUE), 3),
    rango       = round(est_max - est_min, 3),
    n_positivos = sum(estimate > 0, na.rm = TRUE),
    n_negativos = sum(estimate < 0, na.rm = TRUE),
    n_sig       = sum(p.value < .10, na.rm = TRUE),
    heterogeneo = n_positivos > 0 & n_negativos > 0,
    alerta      = case_when(
      heterogeneo & rango > 0.30 ~ "🔴 Ítems con efectos opuestos",
      rango > 0.50               ~ "🟡 Alta variación interna",
      TRUE                       ~ "✓ Cluster homogéneo"
    ),
    .groups = "drop"
  )

variacion_interna %>%
  arrange(cluster, alerta, term_label) %>%
  as.data.frame() %>%
  print()

# Figura: heatmap de efectos por ítem y término FCR

datos_heat <- resultados_prof_ind %>%
  filter(cluster %in% names(clusters_propuestos),
         str_detect(term_label, "hijo|Plan|Mujer")) %>%
  mutate(
    outcome_label = factor(outcome_label),
    term_label    = factor(term_label, levels = rev(etiquetas_fcr))
  )

fig_heat <- ggplot(datos_heat,
                   aes(x = outcome_label,
                       y = term_label,
                       fill = estimate)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(round(estimate, 2), sig)),
            size = 2.8, color = "black") +
  facet_wrap(~ cluster, scales = "free_x", ncol = 1) +
  scale_fill_gradient2(
    low      = "#d01c8b",
    mid      = "white",
    high     = "#4dac26",
    midpoint = 0,
    limits   = c(-1.5, 1.5),
    name     = "Coeficiente"
  ) +
  labs(
    title    = "Efectos FCR por outcome individual — Perfiles profesionales",
    subtitle = "Fondo verde = efecto positivo, rosa = negativo\nNúmero = coeficiente, * = p<.10",
    x = NULL, y = NULL
  ) +
  theme_bw(base_size = 9) +
  theme(
    axis.text.x      = element_text(angle = 35, hjust = 1, size = 8),
    strip.background = element_rect(fill = "#f0f0f0"),
    strip.text       = element_text(face = "bold"),
    legend.position  = "right"
  )

ggsave("figuras/diagnosticos/heatmap_efectos_prof.png",
       fig_heat,
       width = 14, height = 10, dpi = 150, bg = "white")
cat("✓ Figura guardada: figuras/diagnosticos/heatmap_efectos_prof.png\n")


# --- 4. Resumen de decisión por outcome ---

cat("\n=== RESUMEN: decisión por outcome individual ===\n\n")

decision_outcomes <- resultados_prof_ind %>%
  group_by(outcome, outcome_label, cluster) %>%
  summarise(
    n_sig_fcr   = sum(p.value < .10, na.rm = TRUE),
    n_terminos  = n(),
    pct_sig     = round(100 * n_sig_fcr / n_terminos, 0),
    est_max_abs = round(max(abs(estimate), na.rm = TRUE), 3),
    .groups     = "drop"
  ) %>%
  left_join(
    map_dfr(alpha_prof, "items") %>%
      select(item, r_drop, candidato_eliminar),
    by = c("outcome" = "item")
  ) %>%
  mutate(
    decision = case_when(
      # Fuera de clusters — decidir por separado
      is.na(r_drop) & n_sig_fcr >= 2    ~ "✓ Mantener (sin cluster, informativo)",
      is.na(r_drop) & n_sig_fcr < 2     ~ "~ Revisar (sin cluster, poco informativo)",
      # Dentro de clusters
      candidato_eliminar & pct_sig < 20  ~ "⚠ Candidato a eliminar del índice",
      candidato_eliminar                  ~ "~ Revisar — baja r.drop pero efectos presentes",
      pct_sig >= 30                       ~ "✓ Mantener — informativo",
      TRUE                                ~ "✓ Mantener en índice"
    )
  ) %>%
  arrange(cluster, decision, outcome_label)

decision_outcomes %>%
  select(cluster, outcome_label, r_drop, n_sig_fcr,
         pct_sig, est_max_abs, decision) %>%
  as.data.frame() %>%
  print()

write_csv(decision_outcomes,
          "tablas/decision_outcomes_profesional.csv")
cat("✓ Guardado en tablas/decision_outcomes_profesional.csv\n")