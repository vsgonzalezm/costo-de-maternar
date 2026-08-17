# ============================================================
# DIAGNÓSTICO: EFECTO DE OCUPACIÓN EN LOS OUTCOMES
# ============================================================

# --- 1. Verificar niveles de ocupación ---

cat("=== Niveles de perfil_ocupacion ===\n")
niveles_ocup <- if (is.factor(df$perfil_ocupacion)) {
  levels(df$perfil_ocupacion)
} else {
  sort(unique(as.character(df$perfil_ocupacion)))
}
cat(paste0("  ", seq_along(niveles_ocup), ". '", niveles_ocup, "'"), sep = "\n")

# Ajustar con los nombres exactos del output anterior
nivel_tecnico     <- "analista contable"
nivel_profesional <- "asociado/a financiero/a"


# --- 2. Diferencia bruta en outcomes por ocupación ---

cat("\n=== Medias por ocupación (efecto bruto) ===\n")

medias_ocupacion <- df %>%
  mutate(perfil_ocupacion = as.character(perfil_ocupacion)) %>%
  group_by(perfil_ocupacion) %>%
  summarise(
    across(all_of(all_outcomes), ~ mean(.x, na.rm = TRUE)),
    n = n(),
    .groups = "drop"
  ) %>%
  pivot_longer(-c(perfil_ocupacion, n),
               names_to  = "outcome",
               values_to = "media") %>%
  mutate(outcome_label = labels_outcomes[outcome]) %>%
  pivot_wider(names_from  = perfil_ocupacion,
              values_from = media) %>%
  # Renombrar usando sym() para manejar nombres con caracteres especiales
  rename(
    media_tecnico     = !!sym(nivel_tecnico),
    media_profesional = !!sym(nivel_profesional)
  ) %>%
  mutate(
    diferencia     = round(media_profesional - media_tecnico, 3),
    dif_pct_escala = round(100 * diferencia / 6, 1),
    direccion      = case_when(
      diferencia >  0.10 ~ "Profesional > Técnico",
      diferencia < -0.10 ~ "Técnico > Profesional",
      TRUE               ~ "Similar"
    )
  )

medias_ocupacion %>%
  select(outcome_label, media_tecnico, media_profesional,
         diferencia, dif_pct_escala, direccion) %>%
  arrange(desc(abs(diferencia))) %>%
  as.data.frame() %>%   # evita problemas de print con tibbles y NAs
  print()


# --- 3. Fórmula SET1 local ---

formula_set1_str <- paste(
  "perfil_genero + perfil_estado_civil + perfil_parental + perfil_plan_futuro +",
  "perfil_ocupacion + perfil_exp + perfil_desempeno +",
  "pantalla_orden + template_contexto"
)


# --- 4. Interacción ocupación × género ---

cat("\n=== Interacción ocupación x género (outcome: contratación) ===\n")

m_interaccion <- lm_robust(
  as.formula(paste(
    "ans_p1_global ~ perfil_ocupacion * perfil_genero +",
    "perfil_estado_civil + perfil_parental + perfil_plan_futuro +",
    "perfil_exp + perfil_desempeno +",
    "pantalla_orden + template_contexto"
  )),
  data     = df,
  clusters = numericalId,
  se_type  = "CR2"
)

broom::tidy(m_interaccion) %>%
  filter(str_detect(term, "ocupacion|genero")) %>%
  mutate(
    sig = case_when(
      p.value < .05 ~ "*",
      p.value < .10 ~ ".",
      TRUE          ~ ""
    ),
    across(where(is.numeric), ~ round(.x, 4))
  ) %>%
  select(term, estimate, std.error, p.value, sig) %>%
  as.data.frame() %>%
  print()


# --- 5. Tamaño de efectos: todas las dimensiones en contratación ---

cat("\n=== Tamaño de efectos por dimensión (outcome: contratación) ===\n")

m_set1_p1 <- lm_robust(
  as.formula(paste("ans_p1_global ~", formula_set1_str)),
  data     = df,
  clusters = numericalId,
  se_type  = "CR2"
)

broom::tidy(m_set1_p1) %>%
  filter(!str_detect(term, "Intercept|pantalla|template")) %>%
  mutate(
    term_label = case_when(
      str_detect(term, "ocupacion")   ~ "★ Ocupación: profesional",
      str_detect(term, "generoMujer") ~ "★ Género: Mujer",
      TRUE                            ~ term
    ),
    sig = case_when(
      p.value < .05 ~ "*",
      p.value < .10 ~ ".",
      TRUE          ~ ""
    ),
    across(where(is.numeric), ~ round(.x, 4))
  ) %>%
  arrange(desc(abs(estimate))) %>%
  select(term_label, estimate, std.error, sig) %>%
  as.data.frame() %>%
  print()


# --- 6. Correlación de ocupación con otras dimensiones ---

cat("\n=== Correlación de ocupación con otras dimensiones ===\n")

cors_ocupacion <- df %>%
  mutate(across(c(perfil_ocupacion, perfil_genero, perfil_parental,
                  perfil_plan_futuro, perfil_estado_civil),
                ~ as.numeric(as.factor(.x)))) %>%
  summarise(
    r_ocup_genero   = cor(perfil_ocupacion, perfil_genero,       use = "complete.obs"),
    r_ocup_parental = cor(perfil_ocupacion, perfil_parental,     use = "complete.obs"),
    r_ocup_plan     = cor(perfil_ocupacion, perfil_plan_futuro,  use = "complete.obs"),
    r_ocup_ecivil   = cor(perfil_ocupacion, perfil_estado_civil, use = "complete.obs")
  ) %>%
  pivot_longer(everything(),
               names_to  = "par",
               values_to = "correlacion") %>%
  mutate(
    correlacion = round(correlacion, 3),
    alerta      = if_else(abs(correlacion) > .10,
                          "⚠ Correlación no trivial", "✓ OK")
  )

as.data.frame(cors_ocupacion) %>% print()


# --- 7. Efecto de ocupación en TODOS los outcomes ---

cat("\n=== Efecto de ocupación por outcome (SET1) ===\n")

efectos_ocupacion <- map_dfr(all_outcomes, function(y) {
  fml <- as.formula(paste(y, "~", formula_set1_str))
  m   <- lm_robust(fml, data = df, clusters = numericalId, se_type = "CR2")
  broom::tidy(m) %>%
    filter(str_detect(term, "ocupacion")) %>%
    mutate(outcome       = y,
           outcome_label = labels_outcomes[y])
})

if (nrow(efectos_ocupacion) == 0) {
  cat("⚠ No se encontró término de ocupación en los modelos. Verificar nombre de variable.\n")
} else {
  efectos_ocupacion <- efectos_ocupacion %>%
    mutate(
      grupo_outcome = case_when(
        outcome == "ans_p1_global"           ~ "Contratación",
        outcome %in% outcomes_evaluacion     ~ "Evaluación persona",
        outcome %in% outcomes_comportamiento ~ "Riesgo/costo percibido"
      ),
      sig = case_when(
        p.value < .05 ~ "*",
        p.value < .10 ~ ".",
        TRUE          ~ ""
      )
    )
  
  # Resumen numérico
  efectos_ocupacion %>%
    select(outcome_label, estimate, conf.low, conf.high, p.value, sig) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
    arrange(desc(estimate)) %>%
    as.data.frame() %>%
    print()
  
  # Figura
  fig_ocup <- ggplot(efectos_ocupacion,
                     aes(x = estimate,
                         y = reorder(outcome_label, estimate),
                         color = grupo_outcome)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0.3, linewidth = 0.7) +
    geom_point(size = 3) +
    geom_text(
      data = efectos_ocupacion %>% filter(p.value < .10),
      aes(label = sig, x = conf.high + 0.05),
      size = 4, color = "black", fontface = "bold"
    ) +
    scale_color_manual(values = c(
      "Contratación"           = "#2166ac",
      "Evaluación persona"     = "#4dac26",
      "Riesgo/costo percibido" = "#d01c8b"
    )) +
    labs(
      title    = "Efecto de ocupación (profesional vs. técnico) en todos los outcomes",
      subtitle = "Controlando por todas las demás dimensiones del perfil\nRef: analista contable",
      x        = "Coeficiente estimado",
      y        = NULL,
      color    = "Grupo de outcome",
      caption  = ". p<0.1  * p<0.05"
    ) +
    theme_bw(base_size = 11) +
    theme(legend.position = "bottom")
  
  ggsave("figuras/diagnosticos/efecto_ocupacion.png",
         fig_ocup, width = 10, height = 7, dpi = 150, bg = "white")
  cat("✓ Figura guardada: figuras/diagnosticos/efecto_ocupacion.png\n")
}




# ============================================================
# DIAGNÓSTICO: INTERACCIÓN OCUPACIÓN × GÉNERO × PARENTAL
# Para todos los outcomes
# ============================================================

# Términos de interacción que nos interesan:
# - ocupacion × genero (ya visto, pero ahora para todos los outcomes)
# - ocupacion × parental
# - ocupacion × genero × parental (la clave)

formula_interaccion_str <- paste(
  "perfil_ocupacion * perfil_genero * perfil_parental +",
  "perfil_estado_civil + perfil_plan_futuro +",
  "perfil_exp + perfil_desempeno +",
  "pantalla_orden + template_contexto"
)

# --- Correr modelo con triple interacción para cada outcome ---

resultados_interaccion_ocup <- map_dfr(all_outcomes, function(y) {
  fml <- as.formula(paste(y, "~", formula_interaccion_str))
  
  m <- lm_robust(fml,
                 data     = df,
                 clusters = numericalId,
                 se_type  = "CR2")
  
  broom::tidy(m) %>%
    filter(str_detect(term, "ocupacion")) %>%
    mutate(
      outcome       = y,
      outcome_label = labels_outcomes[y],
      tipo_termino  = case_when(
        str_detect(term, "genero") & str_detect(term, "parental") ~ "Triple: Ocup × Género × Parental",
        str_detect(term, "genero")                                 ~ "Doble: Ocup × Género",
        str_detect(term, "parental")                               ~ "Doble: Ocup × Parental",
        TRUE                                                        ~ "Principal: Ocupación"
      ),
      sig = case_when(
        p.value < .05 ~ "*",
        p.value < .10 ~ ".",
        TRUE          ~ ""
      )
    )
})

# --- Resumen: solo interacciones con género y/o parental ---

cat("=== Interacciones con ocupación: resumen por outcome ===\n")
cat("(Solo términos con p < .20 para facilitar lectura)\n\n")

resultados_interaccion_ocup %>%
  filter(tipo_termino != "Principal: Ocupación",
         p.value < .20) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  select(outcome_label, tipo_termino, term, estimate, std.error, p.value, sig) %>%
  arrange(p.value) %>%
  as.data.frame() %>%
  print()

# Si no hay nada con p < .20, mostrar el mínimo p por outcome
cat("\n=== p-value mínimo de interacciones por outcome ===\n")

resultados_interaccion_ocup %>%
  filter(tipo_termino != "Principal: Ocupación") %>%
  group_by(outcome_label) %>%
  slice_min(p.value, n = 1) %>%
  ungroup() %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 3)),
    alerta = if_else(p.value < .10, "⚠ Revisar", "✓ OK")
  ) %>%
  select(outcome_label, tipo_termino, estimate, p.value, alerta) %>%
  arrange(p.value) %>%
  as.data.frame() %>%
  print()


# --- Figura: interacciones con ocupación por outcome y tipo ---

# Filtrar solo los términos de interacción (no el efecto principal de ocupación)
datos_fig <- resultados_interaccion_ocup %>%
  filter(tipo_termino != "Principal: Ocupación") %>%
  mutate(
    grupo_outcome = case_when(
      outcome == "ans_p1_global"           ~ "Contratación",
      outcome %in% outcomes_evaluacion     ~ "Evaluación persona",
      outcome %in% outcomes_comportamiento ~ "Riesgo/costo percibido"
    ),
    tipo_termino = factor(tipo_termino, levels = c(
      "Doble: Ocup × Género",
      "Doble: Ocup × Parental",
      "Triple: Ocup × Género × Parental"
    )),
    # Etiqueta corta del término para el eje
    term_corto = term %>%
      str_remove("perfil_ocupacionasociado/a financiero/a:") %>%
      str_remove("perfil_ocupacionasociado/a financiero/a") %>%
      str_replace("perfil_generoMujer", "Mujer") %>%
      str_replace("perfil_parentaltiene un hijo", "1 hijo") %>%
      str_replace("perfil_parentaltiene dos hijos", "2 hijos") %>%
      str_replace_all(":", " × ")
  )

fig_interaccion_ocup <- ggplot(datos_fig,
                               aes(x = estimate,
                                   y = reorder(outcome_label, estimate),
                                   color = tipo_termino,
                                   shape = tipo_termino)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.3, linewidth = 0.6, alpha = 0.7) +
  geom_point(size = 2.5) +
  geom_text(
    data = datos_fig %>% filter(p.value < .10),
    aes(label = sig, x = conf.high + 0.05),
    size = 4, color = "black", fontface = "bold"
  ) +
  facet_wrap(~ tipo_termino, ncol = 1, scales = "free_x") +
  scale_color_manual(values = c(
    "Doble: Ocup × Género"             = "#e41a1c",
    "Doble: Ocup × Parental"           = "#377eb8",
    "Triple: Ocup × Género × Parental" = "#984ea3"
  )) +
  scale_shape_manual(values = c(16, 17, 15)) +
  labs(
    title    = "Interacciones con ocupación: ¿varía el efecto de género/parental según ocupación?",
    subtitle = "Ref: analista contable, Hombre, Sin hijos",
    x        = "Coeficiente de interacción",
    y        = NULL,
    color    = "Tipo de interacción",
    shape    = "Tipo de interacción",
    caption  = ". p<0.1  * p<0.05\nSi los IC cruzan el cero en todos los outcomes → ocupación no modera el efecto de género/parental"
  ) +
  theme_bw(base_size = 10) +
  theme(
    legend.position  = "bottom",
    strip.background = element_rect(fill = "#f0f0f0"),
    strip.text       = element_text(face = "bold", size = 9)
  )

ggsave("figuras/diagnosticos/interaccion_ocupacion_genero_parental.png",
       fig_interaccion_ocup,
       width = 12, height = 10, dpi = 150, bg = "white")
cat("✓ Figura guardada: figuras/diagnosticos/interaccion_ocupacion_genero_parental.png\n")


# --- Test conjunto: ¿son todas las interacciones con ocupación = 0? ---
# F-test para cada outcome

cat("\n=== F-test conjunto: todas las interacciones con ocupación = 0 ===\n")

library(car)

f_tests_ocup <- map_dfr(all_outcomes, function(y) {
  fml <- as.formula(paste(y, "~", formula_interaccion_str))
  # OLS simple para poder usar linearHypothesis (car)
  m   <- lm(fml, data = df)
  
  # Términos de interacción con ocupación
  terminos_interaccion <- names(coef(m)) %>%
    keep(~ str_detect(.x, "ocupacion") & str_detect(.x, "genero|parental"))
  
  if (length(terminos_interaccion) == 0) {
    return(tibble(outcome = y, outcome_label = labels_outcomes[y],
                  F_stat = NA, p_valor = NA))
  }
  
  resultado <- tryCatch({
    lh <- linearHypothesis(m, terminos_interaccion)
    tibble(
      outcome       = y,
      outcome_label = labels_outcomes[y],
      n_terminos    = length(terminos_interaccion),
      F_stat        = round(lh$F[2], 3),
      p_valor       = round(lh$`Pr(>F)`[2], 3),
      conclusion    = if_else(lh$`Pr(>F)`[2] < .05,
                              "⚠ Interacciones conjuntamente significativas",
                              "✓ No significativo → ocupación no modera")
    )
  }, error = function(e) {
    tibble(outcome = y, outcome_label = labels_outcomes[y],
           n_terminos = NA, F_stat = NA, p_valor = NA,
           conclusion = "Error en F-test")
  })
  resultado
})


f_tests_corregidos <- f_tests_ocup %>%
  filter(!is.na(p_valor)) %>%
  mutate(
    p_ajustado_BH  = p.adjust(p_valor, method = "BH"),
    p_ajustado_bon = p.adjust(p_valor, method = "bonferroni"),
    sig_original   = p_valor    < .05,
    sig_BH         = p_ajustado_BH  < .05,
    sig_bonferroni = p_ajustado_bon < .05
  ) %>%
  select(outcome_label, p_valor, p_ajustado_BH, p_ajustado_bon,
         sig_original, sig_BH, sig_bonferroni) %>%
  arrange(p_valor)

cat("=== F-tests con corrección por comparaciones múltiples ===\n")
as.data.frame(f_tests_corregidos) %>% print()

f_tests_ocup %>%
  select(outcome_label, F_stat, p_valor, conclusion) %>%
  arrange(p_valor) %>%
  as.data.frame() %>%
  print()

write_csv(f_tests_ocup, "tablas/ftest_interaccion_ocupacion.csv")
cat("✓ F-tests guardados en tablas/ftest_interaccion_ocupacion.csv\n")





# ============================================================
# DIAGNÓSTICO: EFECTOS DE GÉNERO, PARENTAL Y PLAN
# POR SUBGRUPO DE OCUPACIÓN
# ============================================================

formula_subgrupo_str <- paste(
  "perfil_genero + perfil_estado_civil + perfil_parental +",
  "perfil_plan_futuro + perfil_exp + perfil_desempeno +",
  "pantalla_orden + template_contexto"
)

# Términos de interés (los que van a los gráficos y tabla)
terminos_interes <- c(
  "perfil_generoMujer",
  "perfil_parentaltiene un hijo",
  "perfil_parentaltiene dos hijos",
  "perfil_plan_futuroen 2 años",
  "perfil_plan_futuroen 5 años"
)

etiquetas_terminos <- c(
  "perfil_generoMujer"             = "Género: Mujer",
  "perfil_parentaltiene un hijo"   = "Parental: 1 hijo",
  "perfil_parentaltiene dos hijos" = "Parental: 2 hijos",
  "perfil_plan_futuroen 2 años"    = "Plan: en 2 años",
  "perfil_plan_futuroen 5 años"    = "Plan: en 5 años"
)

# Niveles de ocupación
df <- df %>%
  mutate(perfil_ocupacion_chr = as.character(perfil_ocupacion))

ocupaciones <- sort(unique(df$perfil_ocupacion_chr))
cat("Subgrupos:\n"); print(ocupaciones)


# --- 1. Modelos separados por ocupación ---

resultados_subgrupos <- map_dfr(ocupaciones, function(ocup) {
  df_sub <- df %>% filter(perfil_ocupacion_chr == ocup)
  
  map_dfr(all_outcomes, function(y) {
    fml <- as.formula(paste(y, "~", formula_subgrupo_str))
    
    m <- tryCatch(
      lm_robust(fml, data = df_sub, clusters = numericalId, se_type = "CR2"),
      error = function(e) NULL
    )
    if (is.null(m)) return(NULL)
    
    broom::tidy(m) %>%
      filter(term %in% terminos_interes) %>%
      mutate(
        ocupacion     = ocup,
        outcome       = y,
        outcome_label = labels_outcomes[y],
        term_label    = etiquetas_terminos[term],
        grupo_outcome = case_when(
          outcome == "ans_p1_global"           ~ "Contratación",
          outcome %in% outcomes_evaluacion     ~ "Evaluación persona",
          outcome %in% outcomes_comportamiento ~ "Riesgo/costo percibido"
        ),
        sig = case_when(
          p.value < .05 ~ "*",
          p.value < .10 ~ ".",
          TRUE          ~ ""
        )
      )
  })
})


# --- 2. Tabla comparativa de coeficientes ---

cat("\n=== Coeficientes por ocupación: términos de interés ===\n")
cat("(Solo outcomes principales para legibilidad)\n\n")

resultados_subgrupos %>%
  filter(outcome %in% c(outcomes_p1, outcomes_evaluacion[1:4],
                        outcomes_comportamiento[c(1,2,4)])) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  select(ocupacion, outcome_label, term_label, estimate, std.error, sig) %>%
  pivot_wider(
    names_from  = ocupacion,
    values_from = c(estimate, std.error, sig),
    names_glue  = "{ocupacion}_{.value}"
  ) %>%
  arrange(outcome_label, term_label) %>%
  as.data.frame() %>%
  print()


# --- 3. Diferencia entre coeficientes (profesional - técnico) ---
# Equivalente al coeficiente de interacción en modelo poolado

cat("\n=== Diferencia de efectos: profesional vs técnico ===\n")
cat("Positivo = efecto más fuerte en profesional\n\n")

diferencias <- resultados_subgrupos %>%
  select(ocupacion, outcome, outcome_label, term, term_label,
         grupo_outcome, estimate, std.error) %>%
  pivot_wider(names_from  = ocupacion,
              values_from = c(estimate, std.error),
              names_glue  = "{.value}_{ocupacion}") %>%
  # Renombrar usando los niveles reales
  rename(
    est_prof = paste0("estimate_", nivel_profesional),
    est_tec  = paste0("estimate_", nivel_tecnico),
    se_prof  = paste0("std.error_", nivel_profesional),
    se_tec   = paste0("std.error_", nivel_tecnico)
  ) %>%
  mutate(
    dif           = round(est_prof - est_tec, 3),
    # SE de la diferencia (asumiendo independencia entre subgrupos)
    se_dif        = round(sqrt(se_prof^2 + se_tec^2), 3),
    z_dif         = round(dif / se_dif, 3),
    p_dif         = round(2 * pnorm(-abs(z_dif)), 3),
    sig_dif       = case_when(
      p_dif < .05 ~ "*",
      p_dif < .10 ~ ".",
      TRUE        ~ ""
    ),
    direccion = case_when(
      dif >  0.05 ~ "Más fuerte en profesional",
      dif < -0.05 ~ "Más fuerte en técnico",
      TRUE        ~ "Similar en ambos"
    )
  )

diferencias %>%
  select(outcome_label, term_label, est_tec, est_prof,
         dif, se_dif, p_dif, sig_dif, direccion) %>%
  arrange(p_dif) %>%
  as.data.frame() %>%
  print(n = 40)

write_csv(diferencias, "tablas/diferencias_efectos_por_ocupacion.csv")
cat("✓ Guardado en tablas/diferencias_efectos_por_ocupacion.csv\n")


# --- 4. Figura comparativa ---

# Preparar datos para figura
datos_fig_subgrupo <- resultados_subgrupos %>%
  mutate(
    term_label  = factor(term_label, levels = etiquetas_terminos),
    ocupacion   = factor(ocupacion,
                         levels = c(nivel_tecnico, nivel_profesional),
                         labels = c("Técnico\n(analista contable)",
                                    "Profesional\n(asoc. financiero/a)"))
  )

# Una figura por grupo de outcome
for (grp in c("Contratación", "Evaluación persona", "Riesgo/costo percibido")) {
  
  datos_grp <- datos_fig_subgrupo %>% filter(grupo_outcome == grp)
  
  fig <- ggplot(datos_grp,
                aes(x = estimate, y = term_label,
                    color = ocupacion, shape = ocupacion)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0.25, linewidth = 0.7,
                   position = position_dodgev(height = 0.5)) +
    geom_point(size = 3,
               position = position_dodgev(height = 0.5)) +
    geom_text(
      data = datos_grp %>% filter(p.value < .10),
      aes(label = sig, x = conf.high + 0.05),
      size = 3.5, color = "black", fontface = "bold",
      position = position_dodgev(height = 0.5)
    ) +
    facet_wrap(~ outcome_label, ncol = 2, scales = "free_x") +
    scale_color_manual(values = c(
      "Técnico\n(analista contable)"      = "#377eb8",
      "Profesional\n(asoc. financiero/a)" = "#e41a1c"
    )) +
    scale_shape_manual(values = c(16, 17)) +
    labs(
      title    = paste0("Efectos de género, parental y plan familiar — ", grp),
      subtitle = "Por subgrupo de ocupación del perfil (IC 95%)\nRef: Hombre, Soltero/a, Sin hijos, Sin planes",
      x        = "Coeficiente estimado",
      y        = NULL,
      color    = "Ocupación del perfil",
      shape    = "Ocupación del perfil",
      caption  = ". p<0.1  * p<0.05"
    ) +
    theme_bw(base_size = 10) +
    theme(
      legend.position  = "bottom",
      strip.background = element_rect(fill = "#f0f0f0"),
      strip.text       = element_text(face = "bold", size = 8),
      axis.text.y      = element_text(size = 9)
    )
  
  nombre_fig <- paste0(
    "figuras/diagnosticos/efectos_por_ocupacion_",
    tolower(str_replace_all(grp, "[/ ]", "_")),
    ".png"
  )
  
  alto <- if (grp == "Contratación") 5 else if (grp == "Evaluación persona") 10 else 10
  
  ggsave(nombre_fig, fig,
         width = 12, height = alto, dpi = 150, bg = "white")
  cat("✓ Figura guardada:", nombre_fig, "\n")
}


# --- 5. Resumen ejecutivo ---

cat("\n=== RESUMEN: ¿Son más fuertes los efectos en perfiles profesionales? ===\n\n")

diferencias %>%
  group_by(term_label) %>%
  summarise(
    n_outcomes          = n(),
    n_mas_fuerte_prof   = sum(dif > 0.05,  na.rm = TRUE),
    n_mas_fuerte_tec    = sum(dif < -0.05, na.rm = TRUE),
    n_similar           = sum(abs(dif) <= 0.05, na.rm = TRUE),
    n_sig_diferencia    = sum(p_dif < .10, na.rm = TRUE),
    dif_media           = round(mean(dif, na.rm = TRUE), 3)
  ) %>%
  mutate(
    patron = case_when(
      n_mas_fuerte_prof > n_mas_fuerte_tec ~ "Efectos mayores en profesional",
      n_mas_fuerte_tec > n_mas_fuerte_prof ~ "Efectos mayores en técnico",
      TRUE                                  ~ "Sin patrón claro"
    )
  ) %>%
  as.data.frame() %>%
  print()



# ============================================================
# DIAGNÓSTICO: EFECTOS DE ESTADO PARENTAL Y PLAN
# SOLO PARA PERFILES MUJER, POR OCUPACIÓN
# ============================================================

formula_mujer_str <- paste(
  "perfil_parental + perfil_plan_futuro +",
  "perfil_estado_civil + perfil_exp + perfil_desempeno +",
  "pantalla_orden + template_contexto"
)

terminos_mujer <- c(
  "perfil_parentaltiene un hijo",
  "perfil_parentaltiene dos hijos",
  "perfil_plan_futuroen 2 años",
  "perfil_plan_futuroen 5 años"
)

etiquetas_mujer <- c(
  "perfil_parentaltiene un hijo"   = "Parental: 1 hijo",
  "perfil_parentaltiene dos hijos" = "Parental: 2 hijos",
  "perfil_plan_futuroen 2 años"    = "Plan: en 2 años",
  "perfil_plan_futuroen 5 años"    = "Plan: en 5 años"
)

# Filtrar solo perfiles mujer
df_mujer <- df %>%
  filter(as.character(perfil_genero) == "Mujer") %>%
  mutate(perfil_ocupacion_chr = as.character(perfil_ocupacion))

cat(sprintf("N observaciones (perfiles mujer): %d\n", nrow(df_mujer)))
cat(sprintf("N respondentes únicos:            %d\n", 
            n_distinct(df_mujer$numericalId)))
cat("\nDistribución por ocupación:\n")
print(table(df_mujer$perfil_ocupacion_chr))


# --- 1. Modelos por ocupación, solo perfiles mujer ---

resultados_mujer <- map_dfr(ocupaciones, function(ocup) {
  df_sub <- df_mujer %>% filter(perfil_ocupacion_chr == ocup)
  
  cat(sprintf("\n  Subgrupo: %s | N = %d obs, %d respondentes\n",
              ocup, nrow(df_sub), n_distinct(df_sub$numericalId)))
  
  map_dfr(all_outcomes, function(y) {
    fml <- as.formula(paste(y, "~", formula_mujer_str))
    
    m <- tryCatch(
      lm_robust(fml, data = df_sub, clusters = numericalId, se_type = "CR2"),
      error = function(e) {
        message(sprintf("    Error en %s: %s", y, e$message))
        NULL
      }
    )
    if (is.null(m)) return(NULL)
    
    broom::tidy(m) %>%
      filter(term %in% terminos_mujer) %>%
      mutate(
        ocupacion     = ocup,
        outcome       = y,
        outcome_label = labels_outcomes[y],
        term_label    = etiquetas_mujer[term],
        grupo_outcome = case_when(
          outcome == "ans_p1_global"           ~ "Contratación",
          outcome %in% outcomes_evaluacion     ~ "Evaluación persona",
          outcome %in% outcomes_comportamiento ~ "Riesgo/costo percibido"
        ),
        sig = case_when(
          p.value < .05 ~ "*",
          p.value < .10 ~ ".",
          TRUE          ~ ""
        )
      )
  })
})


# --- 2. Tabla comparativa (versión corregida) ---

cat("\n=== Efectos de parental y plan en perfiles MUJER por ocupación ===\n\n")




# Versión sin asumir nombres de columna

# Calcular diferencias uniendo los dos subgrupos directamente
resultados_tec <- resultados_mujer %>%
  filter(str_detect(ocupacion, nivel_tecnico)) %>%
  select(outcome, outcome_label, grupo_outcome, term, term_label,
         est_tec  = estimate,
         se_tec   = std.error,
         lo_tec   = conf.low,
         hi_tec   = conf.high,
         pval_tec = p.value,
         sig_tec  = sig)

resultados_prof <- resultados_mujer %>%
  filter(str_detect(ocupacion, nivel_profesional)) %>%
  select(outcome, outcome_label, grupo_outcome, term, term_label,
         est_prof  = estimate,
         se_prof   = std.error,
         lo_prof   = conf.low,
         hi_prof   = conf.high,
         pval_prof = p.value,
         sig_prof  = sig)

# Join por las variables de identificación
tabla_mujer <- resultados_tec %>%
  left_join(resultados_prof,
            by = c("outcome", "outcome_label", "grupo_outcome",
                   "term", "term_label")) %>%
  mutate(
    dif     = round(est_prof - est_tec, 3),
    se_dif  = round(sqrt(se_prof^2 + se_tec^2), 3),
    z_dif   = round(dif / se_dif, 3),
    p_dif   = round(2 * pnorm(-abs(z_dif)), 3),
    sig_dif = case_when(
      p_dif < .05 ~ "*",
      p_dif < .10 ~ ".",
      TRUE        ~ ""
    )
  )

# Verificar que el join funcionó
cat(sprintf("Filas resultantes: %d (esperado: %d)\n",
            nrow(tabla_mujer),
            length(all_outcomes) * length(terminos_mujer)))

# Imprimir tabla
cat("\n=== Efectos de parental y plan en perfiles MUJER por ocupación ===\n\n")

tabla_mujer %>%
  arrange(term_label, outcome_label) %>%
  select(term_label, outcome_label,
         est_tec, sig_tec,
         est_prof, sig_prof,
         dif, p_dif, sig_dif) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  as.data.frame() %>%
  print(n = 40)

write_csv(tabla_mujer, "tablas/efectos_parental_mujer_por_ocupacion.csv")
cat("✓ Guardado en tablas/efectos_parental_mujer_por_ocupacion.csv\n")


# --- Figura (usa tabla_mujer ya construida) ---

datos_fig_mujer <- resultados_mujer %>%
  mutate(
    term_label = factor(term_label,
                        levels = c("Parental: 1 hijo", "Parental: 2 hijos",
                                   "Plan: en 2 años",  "Plan: en 5 años")),
    ocupacion_label = factor(
      case_when(
        str_detect(ocupacion, nivel_tecnico)     ~ "Técnico\n(analista contable)",
        str_detect(ocupacion, nivel_profesional) ~ "Profesional\n(asoc. financiero/a)",
        TRUE ~ ocupacion
      ),
      levels = c("Técnico\n(analista contable)",
                 "Profesional\n(asoc. financiero/a)")
    ),
    grupo_outcome = factor(grupo_outcome,
                           levels = c("Contratación",
                                      "Evaluación persona",
                                      "Riesgo/costo percibido"))
  )

for (grp in levels(datos_fig_mujer$grupo_outcome)) {
  
  datos_grp <- datos_fig_mujer %>% filter(grupo_outcome == grp)
  
  fig <- ggplot(datos_grp,
                aes(x = estimate,
                    y = term_label,
                    color = ocupacion_label,
                    shape = ocupacion_label)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0.25, linewidth = 0.7,
                   position = position_dodgev(height = 0.55)) +
    geom_point(size = 3,
               position = position_dodgev(height = 0.55)) +
    geom_text(
      data = datos_grp %>% filter(p.value < .10),
      aes(label = sig, x = conf.high + 0.08),
      size = 3.5, color = "black", fontface = "bold",
      position = position_dodgev(height = 0.55)
    ) +
    facet_wrap(~ outcome_label, ncol = 2, scales = "free_x") +
    scale_color_manual(values = c(
      "Técnico\n(analista contable)"      = "#377eb8",
      "Profesional\n(asoc. financiero/a)" = "#e41a1c"
    )) +
    scale_shape_manual(values = c(16, 17)) +
    labs(
      title    = paste0("Parental y plan familiar en mujeres — ", grp),
      subtitle = "Solo perfiles MUJER | Por ocupación (IC 95%)\nRef: Sin hijos, Sin planes",
      x        = "Coeficiente estimado",
      y        = NULL,
      color    = "Ocupación",
      shape    = "Ocupación",
      caption  = ". p<0.1  * p<0.05"
    ) +
    theme_bw(base_size = 10) +
    theme(
      legend.position  = "bottom",
      strip.background = element_rect(fill = "#f0f0f0"),
      strip.text       = element_text(face = "bold", size = 8),
      axis.text.y      = element_text(size = 9)
    )
  
  alto <- if (grp == "Contratación") 5 else 10
  
  nombre_fig <- paste0(
    "figuras/diagnosticos/mujer_parental_",
    tolower(str_replace_all(grp, "[/ ]", "_")),
    ".png"
  )
  
  ggsave(nombre_fig, fig,
         width = 12, height = alto, dpi = 150, bg = "white")
  cat("✓ Figura guardada:", nombre_fig, "\n")
}


# --- Resumen ejecutivo ---

cat("\n=== RESUMEN ejecutivo ===\n\n")

tabla_mujer %>%
  arrange(term_label, outcome_label) %>%
  select(term_label, outcome_label,
         est_tec, sig_tec,
         est_prof, sig_prof,
         dif, p_dif, sig_dif) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  as.data.frame() %>%
  print()



# Visualizar el patrón de forma sintética
resumen_patron_mujer <- tabla_mujer %>%
  mutate(
    grupo_outcome = case_when(
      outcome == "ans_p1_global"           ~ "Contratación",
      outcome %in% outcomes_evaluacion     ~ "Evaluación persona",
      outcome %in% outcomes_comportamiento ~ "Riesgo/costo percibido"
    ),
    dimension = case_when(
      str_detect(term_label, "hijo")  ~ "Estado parental",
      str_detect(term_label, "Plan")  ~ "Plan familiar"
    )
  ) %>%
  group_by(dimension, grupo_outcome) %>%
  summarise(
    # Técnico
    est_medio_tec  = round(mean(est_tec,  na.rm = TRUE), 3),
    n_neg_tec      = sum(est_tec < 0,     na.rm = TRUE),
    n_sig_tec      = sum(pval_tec < .10,  na.rm = TRUE),
    # Profesional  
    est_medio_prof = round(mean(est_prof, na.rm = TRUE), 3),
    n_pos_prof     = sum(est_prof > 0,    na.rm = TRUE),
    n_sig_prof     = sum(pval_prof < .10, na.rm = TRUE),
    # Diferencia
    n_dif_sig      = sum(p_dif < .10,    na.rm = TRUE),
    n_outcomes     = n(),
    .groups = "drop"
  )

cat("=== Resumen sintético del patrón ===\n\n")
as.data.frame(resumen_patron_mujer) %>% print()





# ============================================================
# VERIFICACIÓN: ¿El patrón técnico vs profesional se debe
# a efectos de orden de presentación?
# ============================================================

# La lógica: si fuera efecto de orden (comparación con la viñeta anterior),
# los efectos de ocupación deberían variar según si el perfil
# fue visto primero o segundo, no según su ocupación per se.

# --- 1. Distribución de ocupación por orden de pantalla ---
cat("Clase de pantalla_orden:", class(df_mujer$pantalla_orden), "\n")
cat("Valores únicos:\n")
print(unique(df_mujer$pantalla_orden))

# Ver si df_mujer tiene obs en cada orden
df_mujer %>%
  count(pantalla_orden) %>%
  as.data.frame() %>%
  print()
cat("=== ¿La ocupación está balanceada entre posiciones? ===\n")

df %>%
  mutate(perfil_ocupacion_chr = as.character(perfil_ocupacion)) %>%
  count(pantalla_orden, perfil_ocupacion_chr) %>%
  group_by(pantalla_orden) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  as.data.frame() %>%
  print()

# Si la distribución es ~50/50 en cada posición → randomización exitosa
# → el patrón de resultados no puede deberse a orden


# --- 2. Efectos de parental en mujeres, separado por orden ---
# Si hubiera efecto de orden, los coeficientes deberían
# diferir según pantalla_orden, no según ocupación

cat("\n=== Efectos de parental en mujeres por ORDEN de presentación ===\n")
cat("(Independiente de ocupación)\n\n")

resultados_orden <- map_dfr(c(1, 2), function(orden) {
  df_sub <- df_mujer %>%
    filter(pantalla_orden == factor(orden, levels = c(1, 2)))
  
  cat(sprintf("Orden %d: %d obs, %d respondentes\n",
              orden, nrow(df_sub), n_distinct(df_sub$numericalId)))
  
  map_dfr(c(outcomes_p1, outcomes_evaluacion[1:4],
            outcomes_comportamiento[c(1,2,4)]), function(y) {
              fml <- as.formula(paste(y, "~", formula_mujer_str))
              m <- tryCatch(
                lm_robust(fml, data = df_sub, clusters = numericalId, se_type = "CR2"),
                error = function(e) NULL
              )
              if (is.null(m)) return(NULL)
              
              broom::tidy(m) %>%
                filter(term %in% c("perfil_parentaltiene un hijo",
                                   "perfil_parentaltiene dos hijos")) %>%
                mutate(
                  orden         = paste0("Posición ", orden),
                  outcome       = y,
                  outcome_label = labels_outcomes[y],
                  term_label    = etiquetas_mujer[term],
                  sig = case_when(
                    p.value < .05 ~ "*",
                    p.value < .10 ~ ".",
                    TRUE          ~ ""
                  )
                )
            })
})

cat(sprintf("\nTotal filas en resultados_orden: %d\n", nrow(resultados_orden)))

# Tabla: ¿cambian los efectos según orden?
resultados_tec_orden <- resultados_orden %>%
  filter(orden == "Posición 1") %>%
  select(outcome_label, term_label,
         est_pos1 = estimate, se_pos1 = std.error,
         sig_pos1 = sig, pval_pos1 = p.value)

resultados_prof_orden <- resultados_orden %>%
  filter(orden == "Posición 2") %>%
  select(outcome_label, term_label,
         est_pos2 = estimate, se_pos2 = std.error,
         sig_pos2 = sig, pval_pos2 = p.value)

resultados_pos1 <- resultados_orden %>%
  filter(orden == "Posición 1") %>%
  select(outcome_label, term_label,
         est_pos1  = estimate,
         se_pos1   = std.error,
         sig_pos1  = sig,
         pval_pos1 = p.value)

resultados_pos2 <- resultados_orden %>%
  filter(orden == "Posición 2") %>%
  select(outcome_label, term_label,
         est_pos2  = estimate,
         se_pos2   = std.error,
         sig_pos2  = sig,
         pval_pos2 = p.value)

tabla_orden <- resultados_pos1 %>%
  left_join(resultados_pos2,
            by = c("outcome_label", "term_label")) %>%
  mutate(
    dif_orden   = round(est_pos2 - est_pos1, 3),
    se_dif      = round(sqrt(se_pos1^2 + se_pos2^2), 3),
    z_dif       = round(dif_orden / se_dif, 3),
    p_dif_orden = round(2 * pnorm(-abs(z_dif)), 3),
    sig_dif     = if_else(p_dif_orden < .10, "⚠", "✓")
  )

cat("\n=== Efectos de parental por orden de presentación ===\n")
cat("Si diferencias no significativas → patrón técnico/profesional no es efecto de orden\n\n")

tabla_orden %>%
  arrange(term_label, outcome_label) %>%
  select(term_label, outcome_label,
         est_pos1, sig_pos1,
         est_pos2, sig_pos2,
         dif_orden, p_dif_orden, sig_dif) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  as.data.frame() %>%
  print()






# ============================================================
# COMPARACIÓN: Clustered SE vs RI
# Para los efectos de parental en mujeres por ocupación
# ============================================================

library(lme4)
library(lmerTest)

comparar_modelos_subgrupo <- function(ocup_nivel, outcomes_subset) {
  
  df_sub <- df_mujer %>%
    filter(str_detect(as.character(perfil_ocupacion), ocup_nivel))
  
  cat(sprintf("\n--- Subgrupo: %s | N obs = %d | N resp = %d ---\n",
              ocup_nivel, nrow(df_sub), n_distinct(df_sub$numericalId)))
  
  map_dfr(outcomes_subset, function(y) {
    
    fml_ols <- as.formula(paste(y, "~", formula_mujer_str))
    fml_ri  <- as.formula(paste(y, "~", formula_mujer_str,
                                "+ (1 | numericalId)"))
    
    # Modelo clustered SE
    m_cls <- tryCatch(
      lm_robust(fml_ols, data = df_sub,
                clusters = numericalId, se_type = "CR2"),
      error = function(e) NULL
    )
    
    # Modelo RI
    m_ri <- tryCatch(
      lmer(fml_ri, data = df_sub, REML = TRUE),
      error  = function(e) NULL,
      warning = function(w) {
        cat(sprintf("  ⚠ Warning en RI para %s: %s\n", y, w$message))
        suppressWarnings(lmer(fml_ri, data = df_sub, REML = TRUE))
      }
    )
    
    # Extraer coeficientes de parental
    coef_cls <- if (!is.null(m_cls)) {
      broom::tidy(m_cls) %>%
        filter(term %in% terminos_mujer) %>%
        select(term, est_cls = estimate, se_cls = std.error, pval_cls = p.value)
    } else NULL
    
    coef_ri <- if (!is.null(m_ri)) {
      broom::tidy(m_ri) %>%
        filter(term %in% terminos_mujer) %>%
        select(term, est_ri = estimate, se_ri = std.error, pval_ri = p.value)
    } else NULL
    
    if (is.null(coef_cls) || is.null(coef_ri)) return(NULL)
    
    coef_cls %>%
      left_join(coef_ri, by = "term") %>%
      mutate(
        ocupacion     = ocup_nivel,
        outcome       = y,
        outcome_label = labels_outcomes[y],
        term_label    = etiquetas_mujer[term],
        # ¿Cambia la significancia?
        sig_cls = case_when(pval_cls < .05 ~ "*", pval_cls < .10 ~ ".", TRUE ~ ""),
        sig_ri  = case_when(pval_ri  < .05 ~ "*", pval_ri  < .10 ~ ".", TRUE ~ ""),
        cambio_sig = case_when(
          sig_cls == sig_ri                          ~ "✓ Igual",
          sig_cls != "" & sig_ri == ""              ~ "⚠ Pierde sig en RI",
          sig_cls == "" & sig_ri != ""              ~ "⚠ Gana sig en RI",
          TRUE                                       ~ "~ Cambia nivel"
        ),
        dif_se = round(se_ri - se_cls, 4)
      )
  })
}

# Correr para ambas ocupaciones
outcomes_diagnostico <- c(outcomes_p1, outcomes_evaluacion,
                          outcomes_comportamiento)

comp_tec  <- comparar_modelos_subgrupo(nivel_tecnico,     outcomes_diagnostico)
comp_prof <- comparar_modelos_subgrupo(nivel_profesional, outcomes_diagnostico)

comp_total <- bind_rows(comp_tec, comp_prof)


# --- Tabla resumen de cambios ---

cat("\n=== ¿Cambia la significancia al usar RI? ===\n\n")

comp_total %>%
  filter(str_detect(term_label, "hijo")) %>%  # foco en parental
  select(ocupacion, outcome_label, term_label,
         est_cls, se_cls, sig_cls,
         est_ri,  se_ri,  sig_ri,
         cambio_sig, dif_se) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  arrange(ocupacion, term_label, outcome_label) %>%
  as.data.frame() %>%
  print()


# --- Resumen de cuántos outcomes cambian ---

cat("\n=== Resumen: frecuencia de cambios en significancia ===\n\n")

comp_total %>%
  group_by(ocupacion, term_label) %>%
  count(cambio_sig) %>%
  pivot_wider(names_from = cambio_sig,
              values_from = n,
              values_fill = 0) %>%
  as.data.frame() %>%
  print()


# --- ICC por subgrupo ---
# Para entender si el RI tiene sentido en estos subgrupos pequeños

cat("\n=== ICC por subgrupo (modelo nulo) ===\n\n")

icc_subgrupos <- map_dfr(
  list(
    list(label = "Técnico",     data = df_mujer %>% filter(str_detect(as.character(perfil_ocupacion), nivel_tecnico))),
    list(label = "Profesional", data = df_mujer %>% filter(str_detect(as.character(perfil_ocupacion), nivel_profesional)))
  ),
  function(sub) {
    map_dfr(outcomes_diagnostico, function(y) {
      m <- tryCatch(
        lmer(as.formula(paste(y, "~ 1 + (1 | numericalId)")),
             data = sub$data, REML = TRUE),
        error = function(e) NULL
      )
      if (is.null(m)) return(NULL)
      vc    <- as.data.frame(VarCorr(m))
      var_u <- vc$vcov[1]
      var_e <- vc$vcov[2]
      tibble(
        subgrupo      = sub$label,
        outcome       = y,
        outcome_label = labels_outcomes[y],
        icc           = round(var_u / (var_u + var_e), 3),
        n_obs         = nrow(sub$data),
        n_resp        = n_distinct(sub$data$numericalId)
      )
    })
  }
)

icc_subgrupos %>%
  group_by(subgrupo) %>%
  summarise(
    n_obs      = first(n_obs),
    n_resp     = first(n_resp),
    icc_medio  = round(mean(icc, na.rm = TRUE), 3),
    icc_min    = round(min(icc,  na.rm = TRUE), 3),
    icc_max    = round(max(icc,  na.rm = TRUE), 3),
    .groups    = "drop"
  ) %>%
  as.data.frame() %>%
  print()

write_csv(comp_total, "tablas/comparacion_cls_ri_subgrupos.csv")
cat("\n✓ Guardado en tablas/comparacion_cls_ri_subgrupos.csv\n")


