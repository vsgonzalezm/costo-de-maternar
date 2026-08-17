# ============================================================
# DIAGNÓSTICO: ¿APORTA ESTADO CIVIL INFORMACIÓN ÚNICA
# SOBRE RIESGO REPRODUCTIVO PERCIBIDO?
# Solo perfiles profesionales
# ============================================================

# ============================================================
# PAQUETES
# ============================================================

# Instalar una sola vez si hace falta:
# install.packages(c(
#   "tidyverse",
#   "lme4",
#   "lmerTest",
#   "broom.mixed"
# ))

library(tidyverse)
library(lme4)
library(lmerTest)
library(broom.mixed)

# ============================================================
# 1. ¿Estado civil predice outcomes FCR
#    más allá de parental y plan?
# ============================================================

cat("=== Estado civil como predictor de riesgo FCR ===\n")
cat("Pregunta: ¿agrega info más allá de parental y plan familiar?\n\n")

outcomes_fcr <- c(
  "idx_riesgo_conductual",
  "idx_costo_percibido",
  "ans_p1_global",
  "idx_evaluacion_pos"
)

comparar_con_sin_ecivil <- map_dfr(outcomes_fcr, function(y) {
  
  fml_sin <- as.formula(paste(
    y, "~",
    "perfil_genero +",
    "perfil_parental +",
    "perfil_plan_futuro +",
    "perfil_exp +",
    "perfil_desempeno +",
    "pantalla_orden +",
    "template_contexto +",
    "(1 | numericalId)"
  ))
  
  fml_con <- as.formula(paste(
    y, "~",
    "perfil_genero +",
    "perfil_estado_civil +",
    "perfil_parental +",
    "perfil_plan_futuro +",
    "perfil_exp +",
    "perfil_desempeno +",
    "pantalla_orden +",
    "template_contexto +",
    "(1 | numericalId)"
  ))
  
  m_sin <- tryCatch(
    lmer(fml_sin, data = df_prof, REML = FALSE),
    error = function(e) NULL,
    warning = function(w) suppressWarnings(
      lmer(fml_sin, data = df_prof, REML = FALSE)
    )
  )
  
  m_con <- tryCatch(
    lmer(fml_con, data = df_prof, REML = FALSE),
    error = function(e) NULL,
    warning = function(w) suppressWarnings(
      lmer(fml_con, data = df_prof, REML = FALSE)
    )
  )
  
  if (is.null(m_sin) | is.null(m_con)) return(NULL)
  
  # ----------------------------------------------------------
  # LR test
  # ----------------------------------------------------------
  
  lr <- anova(m_con, m_sin)
  
  # ----------------------------------------------------------
  # Coeficientes estado civil
  # ----------------------------------------------------------
  
  coefs_ecivil <- broom.mixed::tidy(
    m_con,
    effects = "fixed"
  ) %>%
    filter(str_detect(term, "estado_civil")) %>%
    mutate(
      sig = case_when(
        p.value < .05 ~ "*",
        p.value < .10 ~ ".",
        TRUE ~ ""
      )
    )
  
  tibble(
    outcome       = y,
    outcome_label = labels_outcomes_red[y],
    
    # LR test
    chi2          = round(lr$Chisq[2], 3),
    df_lr         = lr$Df[2],
    p_lr          = round(lr$`Pr(>Chisq)`[2], 3),
    ecivil_mejora = lr$`Pr(>Chisq)`[2] < .10,
    
    # coeficientes
    b_casado = round(
      coefs_ecivil$estimate[
        str_detect(coefs_ecivil$term, "casado")
      ][1],
      3
    ),
    
    sig_casado = coefs_ecivil$sig[
      str_detect(coefs_ecivil$term, "casado")
    ][1],
    
    b_conviviendo = round(
      coefs_ecivil$estimate[
        str_detect(coefs_ecivil$term, "conviviendo")
      ][1],
      3
    ),
    
    sig_conviviendo = coefs_ecivil$sig[
      str_detect(coefs_ecivil$term, "conviviendo")
    ][1]
  )
})

cat("--- LR test: ¿estado civil mejora el modelo? ---\n\n")

comparar_con_sin_ecivil %>%
  select(
    outcome_label,
    chi2,
    p_lr,
    ecivil_mejora,
    b_casado,
    sig_casado,
    b_conviviendo,
    sig_conviviendo
  ) %>%
  mutate(across(where(is.numeric), round, 3)) %>%
  as.data.frame() %>%
  print()


# ============================================================
# 2. Interacción estado civil × género
# ============================================================

cat("\n=== Interacción estado civil × género ===\n\n")

interaccion_ecivil_genero <- map_dfr(outcomes_fcr, function(y) {
  
  fml <- as.formula(paste(
    y, "~",
    "perfil_genero * perfil_estado_civil +",
    "perfil_parental +",
    "perfil_plan_futuro +",
    "perfil_exp +",
    "perfil_desempeno +",
    "pantalla_orden +",
    "template_contexto +",
    "(1 | numericalId)"
  ))
  
  m <- tryCatch(
    lmer(fml, data = df_prof, REML = TRUE),
    error = function(e) NULL,
    warning = function(w) suppressWarnings(
      lmer(fml, data = df_prof, REML = TRUE)
    )
  )
  
  if (is.null(m)) return(NULL)
  
  broom.mixed::tidy(
    m,
    effects = "fixed"
  ) %>%
    filter(str_detect(term, "estado_civil|genero")) %>%
    mutate(
      outcome       = y,
      outcome_label = labels_outcomes_red[y],
      
      sig = case_when(
        p.value < .05 ~ "*",
        p.value < .10 ~ ".",
        TRUE ~ ""
      ),
      
      term_label = case_when(
        term == "perfil_generoMujer" ~
          "Género: Mujer",
        
        term == "perfil_estado_civilcasado/a" ~
          "E. civil: Casado/a",
        
        term == "perfil_estado_civilconviviendo" ~
          "E. civil: Conviviendo",
        
        term == "perfil_generoMujer:perfil_estado_civilcasado/a" ~
          "Mujer × Casado/a",
        
        term == "perfil_generoMujer:perfil_estado_civilconviviendo" ~
          "Mujer × Conviviendo",
        
        TRUE ~ term
      )
    )
})

interaccion_ecivil_genero %>%
  select(
    outcome_label,
    term_label,
    estimate,
    std.error,
    p.value,
    sig
  ) %>%
  mutate(across(where(is.numeric), round, 3)) %>%
  arrange(outcome_label, term_label) %>%
  as.data.frame() %>%
  print()


# ============================================================
# 3. Correlación estado civil y plan familiar
# ============================================================

cat("\n=== Correlación entre estado civil y plan familiar ===\n\n")

df_prof %>%
  mutate(
    ecivil_num    = as.numeric(as.factor(perfil_estado_civil)),
    plan_num      = as.numeric(as.factor(perfil_plan_futuro)),
    parental_num  = as.numeric(as.factor(perfil_parental))
  ) %>%
  summarise(
    r_ecivil_plan = round(
      cor(ecivil_num, plan_num, use = "complete.obs"),
      3
    ),
    
    r_ecivil_parental = round(
      cor(ecivil_num, parental_num, use = "complete.obs"),
      3
    )
  ) %>%
  as.data.frame() %>%
  print()


# ============================================================
# 4. Distribución cruzada
# ============================================================

cat("\n=== Distribución cruzada ===\n\n")

df_prof %>%
  mutate(
    ecivil = as.character(perfil_estado_civil),
    plan   = as.character(perfil_plan_futuro)
  ) %>%
  count(ecivil, plan) %>%
  group_by(ecivil) %>%
  mutate(
    pct = round(100 * n / sum(n), 1)
  ) %>%
  ungroup() %>%
  select(ecivil, plan, n, pct) %>%
  arrange(ecivil, plan) %>%
  as.data.frame() %>%
  print()


# ============================================================
# 5. Medias por combinación
# ============================================================

cat("\n=== Medias FCR por estado civil × género ===\n")
cat("Controlando: sin hijos y sin planes\n\n")

df_prof %>%
  filter(
    as.character(perfil_parental) ==
      "actualmente no tiene hijos",
    
    as.character(perfil_plan_futuro) ==
      "no quiere"
  ) %>%
  mutate(
    genero = as.character(perfil_genero),
    ecivil = as.character(perfil_estado_civil)
  ) %>%
  group_by(genero, ecivil) %>%
  summarise(
    n = n(),
    
    contratacion = round(
      mean(ans_p1_global, na.rm = TRUE),
      2
    ),
    
    evaluacion_pos = round(
      mean(idx_evaluacion_pos, na.rm = TRUE),
      2
    ),
    
    riesgo_conductual = round(
      mean(idx_riesgo_conductual, na.rm = TRUE),
      2
    ),
    
    costo_percibido = round(
      mean(idx_costo_percibido, na.rm = TRUE),
      2
    ),
    
    .groups = "drop"
  ) %>%
  arrange(genero, ecivil) %>%
  as.data.frame() %>%
  print()


# ============================================================
# 6. Resumen decisión
# ============================================================

cat("\n=== RESUMEN: ¿Fijar estado civil? ===\n\n")

cat("Criterios para mantener estado civil:\n")
cat("  1. LR test significativo (p < .10)\n")
cat("  2. Interacción Mujer × Casado/a significativa\n")
cat("  3. Diferencias sustantivas en medias\n\n")

cat("Criterios para eliminar estado civil:\n")
cat("  1. LR tests no significativos\n")
cat("  2. Sin interacción con género\n")
cat("  3. Correlación alta con plan familiar\n")
cat("  4. Medias similares\n\n")

n_sig_lr <- sum(
  comparar_con_sin_ecivil$ecivil_mejora,
  na.rm = TRUE
)

n_sig_int <- interaccion_ecivil_genero %>%
  filter(
    str_detect(term_label, "Mujer ×"),
    p.value < .10
  ) %>%
  nrow()

cat(sprintf(
  "LR tests significativos: %d de %d outcomes\n",
  n_sig_lr,
  nrow(comparar_con_sin_ecivil)
))

cat(sprintf(
  "Interacciones Mujer × E.civil significativas: %d\n\n",
  n_sig_int
))

if (n_sig_lr == 0 & n_sig_int == 0) {
  
  cat("→ RECOMENDACIÓN: Fijar estado civil.\n")
  cat("  Plan familiar captura la señal principal.\n")
  cat("  Estado civil no aporta información adicional.\n")
  
} else if (n_sig_lr >= 2 | n_sig_int >= 2) {
  
  cat("→ RECOMENDACIÓN: Mantener estado civil.\n")
  cat("  Aporta señal independiente del plan familiar.\n")
  
} else {
  
  cat("→ RECOMENDACIÓN: Evidencia mixta.\n")
  cat("  Decidir principalmente por criterio teórico.\n")
  
  
  
  
  
  # ============================================================
  # MODELO FOCALIZADO:
  # Mujeres SIN hijos y SIN planes de tenerlos
  #
  # Pregunta:
  # ¿El estado civil sigue activando penalización
  # incluso cuando el riesgo reproductivo explícito
  # ya está eliminado?
  # ============================================================
  
  cat("\n====================================================\n")
  cat("MUJERES SIN HIJOS Y SIN PLANES\n")
  cat("¿Estado civil activa riesgo residual?\n")
  cat("====================================================\n\n")
  
  # ------------------------------------------------------------
  # Filtrar subgrupo teórico clave
  # ------------------------------------------------------------
  
  df_sub <- df_prof %>%
    filter(
      as.character(perfil_genero) == "Mujer",
      
      as.character(perfil_parental) ==
        "actualmente no tiene hijos",
      
      as.character(perfil_plan_futuro) ==
        "no quiere"
    )
  
  cat("Tamaño muestra subgrupo:\n")
  print(table(df_sub$perfil_estado_civil))
  cat("\n")
  
  # ------------------------------------------------------------
  # Outcomes
  # ------------------------------------------------------------
  
  outcomes_fcr <- c(
    "idx_riesgo_conductual",
    "idx_costo_percibido",
    "ans_p1_global",
    "idx_evaluacion_pos"
  )
  
  # ------------------------------------------------------------
  # Modelos
  # ------------------------------------------------------------
  # ============================================================
# MODELOS
# ============================================================

modelos_subgrupo <- map_dfr(outcomes_fcr, function(y) {

  cat("Corriendo:", y, "\n")

  fml <- as.formula(paste(
    y, "~",
    "perfil_estado_civil +",
    "perfil_exp +",
    "perfil_desempeno +",
    "pantalla_orden +",
    "template_contexto +",
    "(1 | numericalId)"
  ))

  m <- tryCatch(

    lmer(fml, data = df_sub, REML = TRUE),

    error = function(e) {
      cat("ERROR EN MODELO:", y, "\n")
      return(NULL)
    },

    warning = function(w) {
      suppressWarnings(
        lmer(fml, data = df_sub, REML = TRUE)
      )
    }
  )

  if (is.null(m)) return(NULL)

  td <- tryCatch(

    broom.mixed::tidy(
      m,
      effects = "fixed"
    ),

    error = function(e) {
      cat("ERROR EN TIDY:", y, "\n")
      return(NULL)
    }
  )

  if (is.null(td)) return(NULL)

  td %>%
    filter(str_detect(term, "estado_civil")) %>%
    mutate(
      outcome = y,
      outcome_label = labels_outcomes_red[y],

      sig = case_when(
        p.value < .05 ~ "*",
        p.value < .10 ~ ".",
        TRUE ~ ""
      ),

      term_label = case_when(
        str_detect(term, "casado") ~
          "Casada vs soltera",

        str_detect(term, "conviviendo") ~
          "Conviviendo vs soltera",

        TRUE ~ term
      )
    )
})

# ============================================================
# RESULTADOS
# ============================================================

cat("\n=== RESULTADOS SUBGRUPO TEÓRICO ===\n\n")

if (nrow(modelos_subgrupo) == 0) {

  cat("No se pudieron estimar modelos.\n")
  cat("Probablemente el subgrupo es demasiado pequeño.\n")

} else {

  modelos_subgrupo %>%
    select(
      outcome_label,
      term_label,
      estimate,
      std.error,
      p.value,
      sig
    ) %>%
    mutate(
      across(
        where(is.numeric),
        \(x) round(x, 3)
      )
    ) %>%
    arrange(outcome_label, term_label) %>%
    as.data.frame() %>%
    print()
}