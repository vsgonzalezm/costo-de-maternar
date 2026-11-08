
install.packages(c("estimatr", "modelsummary", "censReg"))
install.packages("data.table")
install.packages("pandoc")
install.packages("patchwork")
install.packages("corrr")
install.packages("psych")
install.packages("rlang")
install.packages("corrplot")
install.packages("tidy_lmer")
install.packages("tidyverse")
install.packages("lme4")
install.packages("lmerTest")


# SCRIPT COMPLETO
# INSTALACIÓN DE PAQUETES 
# LIBRERÍAS
library(tidyverse)
library(haven)
library(estimatr)
library(modelsummary)
library(broom)
library(ggplot2)
library(lme4)
library(lmerTest)
library(censReg)
library(patchwork)
library(readr)
library(corrplot)
library(tidy_lmer)


# DIRECTORIO
setwd("~/Desktop/Diseno_demanda")

# CARPETAS DE SALIDA
dir.create("tablas",  showWarnings = FALSE)
dir.create("figuras", showWarnings = FALSE)

# 1. CARGA DE DATOS

base_long_final <- read_sav("base_pretest_long_final.sav")

# 2. PREPARACIÓN DE VARIABLES

df <- base_long_final %>%
  mutate(
    perfil_genero       = relevel(as_factor(perfil_genero),       ref = "Hombre"),
    perfil_estado_civil = relevel(as_factor(perfil_estado_civil), ref = "soltero/a"),
    perfil_parental     = factor(perfil_parental,
                                 levels = c("actualmente no tiene hijos",
                                            "tiene un hijo",
                                            "tiene dos hijos")),
    perfil_plan_futuro  = relevel(as_factor(perfil_plan_futuro),  ref = "no quiere"),
    perfil_ocupacion    = as_factor(perfil_ocupacion),
    perfil_exp          = as_factor(perfil_exp),
    perfil_desempeno    = as_factor(perfil_desempeno),
    pantalla_orden      = as.factor(pantalla_orden),
    template_contexto   = as.factor(template_contexto)
  )


# 3. DEFINICIÓN DE OUTCOMES Y ETIQUETAS

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


# 3.1: Chequeo de outcomes
# Objetivo: detectar censura (picos en extremos) o asimetría severa

dir.create("figuras/diagnosticos", showWarnings = FALSE)

plot_distribucion <- function(outcome_var, label) {
  ggplot(df, aes(x = .data[[outcome_var]])) +
    geom_histogram(binwidth = 1, fill = "steelblue",
                   color = "white", alpha = 0.85) +
    geom_vline(xintercept = c(min(df[[outcome_var]], na.rm = TRUE),
                              max(df[[outcome_var]], na.rm = TRUE)),
               linetype = "dashed", color = "red", alpha = 0.6) +
    labs(title = label,
         x = "Valor", y = "Frecuencia",
         caption = "Líneas rojas = extremos de escala") +
    theme_bw(base_size = 10)
}

# Graficar los 16 outcomes en grillas de 4
grupos_outcomes <- list(
  "p1"     = outcomes_p1,
  "p2_1_4" = outcomes_evaluacion[1:4],
  "p2_5_7" = outcomes_evaluacion[5:7],
  "p3_1_4" = outcomes_comportamiento[1:4],
  "p3_5_8" = outcomes_comportamiento[5:8]
)

for (nombre_grp in names(grupos_outcomes)) {
  vars <- grupos_outcomes[[nombre_grp]]
  plots <- map2(vars, labels_outcomes[vars], plot_distribucion)
  fig <- wrap_plots(plots, ncol = 2)
  ggsave(paste0("figuras/diagnosticos/dist_", nombre_grp, ".png"),
         fig, width = 10, height = length(vars) * 1.8 + 1,
         dpi = 150, bg = "white")
  cat("✓ Distribuciones guardadas: figuras/diagnosticos/dist_", nombre_grp, ".png\n")
}


# --- 3.2 Detección de response sets ---
# Respondentes que dieron el mismo valor a ambas viñetas en todos los outcomes

response_sets <- df %>%
  group_by(numericalId) %>%
  summarise(
    across(all_of(all_outcomes),
           ~ var(.x, na.rm = TRUE),
           .names = "var_{.col}"),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    n_cero = sum(c_across(starts_with("var_")) == 0, na.rm = TRUE),
    posible_response_set = n_cero >= (length(all_outcomes) * 0.75)
  ) %>%
  ungroup()

n_rs <- sum(response_sets$posible_response_set, na.rm = TRUE)
cat(sprintf("\n=== Response sets ===\n"))
cat(sprintf("Respondentes con varianza 0 en ≥75%% de outcomes: %d (%.1f%%)\n\n",
            n_rs, 100 * n_rs / nrow(response_sets)))

# Tabla resumen de varianza within por outcome
var_within_summary <- df %>%
  group_by(numericalId) %>%
  summarise(across(all_of(all_outcomes), ~ var(.x, na.rm = TRUE)),
            .groups = "drop") %>%
  summarise(across(all_of(all_outcomes), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "outcome", values_to = "var_within_media") %>%
  mutate(outcome_label = labels_outcomes[outcome])

cat("=== Varianza within-respondente media por outcome ===\n")
print(var_within_summary %>% select(outcome_label, var_within_media) %>% arrange(var_within_media))



#### 3.3. ICC por outcome

calcular_icc <- function(outcome_var) {
  fml    <- as.formula(paste(outcome_var, "~ 1 + (1 | numericalId)"))
  modelo <- lmer(fml, data = df, REML = TRUE)
  vc     <- as.data.frame(VarCorr(modelo))
  var_u  <- vc$vcov[1]   # varianza entre respondentes
  var_e  <- vc$vcov[2]   # varianza residual
  icc    <- var_u / (var_u + var_e)
  tibble(
    outcome     = outcome_var,
    label       = labels_outcomes[outcome_var],
    var_u       = round(var_u, 4),
    var_e       = round(var_e, 4),
    icc         = round(icc, 4),
    recomendacion = case_when(
      icc >= 0.10 ~ "RI claramente necesario",
      icc >= 0.05 ~ "RI recomendable",
      TRUE        ~ "Clustered SE puede ser suficiente"
    )
  )
}

tabla_icc <- map_dfr(all_outcomes, calcular_icc)

cat("\n--- ICC por outcome ---\n")
print(tabla_icc %>% select(label, icc, recomendacion), n = 30)

write_csv(tabla_icc, "tablas/diagnostico_icc.csv")
cat("✓ Tabla ICC guardada en tablas/diagnostico_icc.csv\n\n")

# Figura resumen de ICC
fig_icc <- ggplot(tabla_icc,
                  aes(x = icc,
                      y = reorder(label, icc),
                      color = recomendacion)) +
  geom_point(size = 3) +
  geom_vline(xintercept = c(0.05, 0.10),
             linetype = c("dashed", "solid"),
             color = c("orange", "red"), alpha = 0.7) +
  annotate("text", x = 0.052, y = 0.5,
           label = "ICC=0.05", size = 3, color = "orange",
           hjust = 0, vjust = 0) +
  annotate("text", x = 0.102, y = 0.5,
           label = "ICC=0.10", size = 3, color = "red",
           hjust = 0, vjust = 0) +
  scale_color_manual(values = c(
    "RI claramente necesario"         = "#e41a1c",
    "RI recomendable"                 = "#ff7f00",
    "Clustered SE puede ser suficiente" = "#4daf4a"
  )) +
  labs(title = "ICC por outcome (modelo nulo con intercepto aleatorio)",
       subtitle = "Proporción de varianza atribuible a diferencias entre respondentes",
       x = "ICC (ρ)", y = NULL,
       color = "Recomendación") +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

ggsave("figuras/diagnosticos/icc_por_outcome.png",
       fig_icc, width = 10, height = 7, dpi = 150, bg = "white")
cat("✓ Figura ICC guardada\n\n")


# Diagnóstico de efecto de contraste en el piloto

contraste_check <- df %>%
  group_by(numericalId) %>%
  summarise(
    generos_distintos = n_distinct(perfil_genero) > 1,
    dif_contratacion  = diff(ans_p1_global),    # viñeta2 - viñeta1
    .groups = "drop"
  )

# ¿La diferencia en evaluación es mayor cuando los géneros son distintos?
contraste_check %>%
  group_by(generos_distintos) %>%
  summarise(
    dif_media   = mean(abs(dif_contratacion), na.rm = TRUE),
    dif_sd      = sd(abs(dif_contratacion),   na.rm = TRUE),
    n           = n()
  )


# -------------------------------------------------------
# PASO 3.4: COMPARACIÓN DE MODELOS (OLS / Clustered SE / RI)
# Solo para SET1 (efectos principales), un outcome a la vez
# Esto es para diagnóstico, no para reemplazar tus análisis principales
# -------------------------------------------------------

cat("=== PASO 4: Comparación OLS vs Clustered SE vs RI (SET1) ===\n\n")

formula_set1 <- sets_formulas[["SET1_principales"]]

correr_tres_modelos <- function(outcome_var, formula_str) {
  
  fml <- as.formula(paste(outcome_var, "~", formula_str))
  
  # M1: OLS simple
  m1 <- lm(fml, data = df)
  
  # M2: Clustered SE (lo que usas actualmente)
  m2 <- lm_robust(fml,
                  data     = df,
                  clusters = numericalId,
                  se_type  = "CR2")
  
  # M3: Random intercept
  # lmerTest para p-values; REML=FALSE para poder comparar con LR test
  fml_ri <- as.formula(paste(outcome_var, "~",
                             formula_str, "+ (1 | numericalId)"))
  m3 <- lmer(fml_ri, data = df, REML = FALSE)
  
  list(OLS = m1, `Clustered SE` = m2, `Random Intercept` = m3)
}

# Correr para todos los outcomes
modelos_comparacion <- map(all_outcomes,
                           ~ correr_tres_modelos(.x, formula_set1)) %>%
  set_names(all_outcomes)

cat("✓ Tres modelos corridos para cada outcome\n\n")


# --- LR test: ¿el RI mejora significativamente sobre OLS? ---

cat("=== LR test: OLS vs Random Intercept ===\n")

lr_tests <- map_dfr(all_outcomes, function(y) {
  fml_ols <- as.formula(paste(y, "~", formula_set1))
  fml_ri  <- as.formula(paste(y, "~", formula_set1, "+ (1 | numericalId)"))
  
  m_ols <- lmer(update(fml_ols, . ~ . + (1 | numericalId)),
                data = df, REML = FALSE)
  # Modelo nulo (solo RI, sin predictores de viñeta)
  m_nulo <- lmer(as.formula(paste(y, "~ 1 + (1 | numericalId)")),
                 data = df, REML = FALSE)
  
  # anova() compara el RI con predictores vs solo el intercepto aleatorio
  lr <- anova(m_ols, m_nulo)
  
  tibble(
    outcome    = y,
    label      = labels_outcomes[y],
    chi2       = round(lr$Chisq[2], 3),
    df_lr      = lr$Df[2],   # grados de libertad del test
    p_lr       = round(lr$`Pr(>Chisq)`[2], 4),
    icc        = tabla_icc$icc[tabla_icc$outcome == y],
    conclusion = if_else(lr$`Pr(>Chisq)`[2] < .05,
                         "RI mejora el ajuste (p<.05)",
                         "RI no mejora significativamente")
  )
})

print(lr_tests %>% select(label, chi2, p_lr, icc, conclusion), n = 30)
write_csv(lr_tests, "tablas/lr_tests_ri_vs_ols.csv")
cat("✓ LR tests guardados en tablas/lr_tests_ri_vs_ols.csv\n\n")


# -------------------------------------------------------
# PASO 5: TABLA COMPARATIVA OLS / CLUSTERED / RI
# Para los 3 grupos de outcomes, usando SET1
# -------------------------------------------------------

cat("=== PASO 5: Exportar tablas comparativas de modelos ===\n")

exportar_comparacion <- function(outcomes_subset, nombre_archivo) {
  
  # Para cada outcome: 3 modelos con sufijo en el nombre
  lista_modelos <- map(outcomes_subset, function(y) {
    mods  <- modelos_comparacion[[y]]
    nms   <- paste0(labels_outcomes[y], "\n[",
                    c("OLS", "Clust-SE", "RI"), "]")
    set_names(mods, nms)
  }) %>%
    flatten()
  
  # Solo variables de viñeta (excluir controles) en la tabla
  coefs_mostrar <- c(
    "perfil_generoMujer",
    "perfil_estado_civilcasado/a",
    "perfil_estado_civilconviviendo",
    "perfil_parentaltiene un hijo",
    "perfil_parentaltiene dos hijos",
    "perfil_plan_futuroen 2 años",
    "perfil_plan_futuroen 5 años",
    "perfil_exp6 años",
    "perfil_exp8 años",
    "perfil_desempeno6.0",
    "perfil_desempeno6.3",
    "perfil_ocupacionasociado/a financiero/a"
  )
  
  coef_map <- c(
    "perfil_generoMujer"                      = "Género: Mujer",
    "perfil_estado_civilcasado/a"             = "E. civil: Casado/a",
    "perfil_estado_civilconviviendo"          = "E. civil: Conviviendo",
    "perfil_parentaltiene un hijo"            = "Parental: 1 hijo",
    "perfil_parentaltiene dos hijos"          = "Parental: 2 hijos",
    "perfil_plan_futuroen 2 años"             = "Plan: 2 años",
    "perfil_plan_futuroen 5 años"             = "Plan: 5 años",
    "perfil_exp6 años"                        = "Experiencia: 6 años",
    "perfil_exp8 años"                        = "Experiencia: 8 años",
    "perfil_desempeno6.0"                     = "Desempeño: 6.0",
    "perfil_desempeno6.3"                     = "Desempeño: 6.3",
    "perfil_ocupacionasociado/a financiero/a" = "Ocupación: Financiero/a",
    "pantalla_orden2"                         = "Orden: 2do perfil",
    "template_contextoB"                      = "Template: B"
  )
  
  modelsummary(
    lista_modelos,
    coef_map = coef_map,
    stars     = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    gof_map   = list(
      list(raw = "nobs",      clean = "N observaciones",  fmt = 0),
      list(raw = "r.squared", clean = "R²",               fmt = 3),
      list(raw = "icc",       clean = "ICC (ρ)",          fmt = 3)
    ),
    notes     = c(
      "OLS: mínimos cuadrados ordinarios sin ajuste.",
      "Clust-SE: clustered standard errors por respondente (CR2).",
      "RI: modelo de intercepto aleatorio (lme4). p-values con lmerTest.",
      "Ref: Hombre, Soltero/a, Sin hijos, Sin planes, Exp. 4 años, Desempeño 5.7."
    ),
    output = nombre_archivo
  )
  cat("✓ Tabla guardada:", nombre_archivo, "\n")
}

exportar_comparacion(outcomes_p1,             "tablas/COMP_contratacion.docx")
exportar_comparacion(outcomes_evaluacion,     "tablas/COMP_evaluaciones.docx")
exportar_comparacion(outcomes_comportamiento, "tablas/COMP_comportamiento.docx")









# 4. FÓRMULAS POR SET

controles <- "pantalla_orden + template_contexto"

sets_formulas <- list(
  
  "SET1_principales" = paste(
    "perfil_genero + perfil_estado_civil + perfil_parental + perfil_plan_futuro +",
    "perfil_ocupacion + perfil_exp + perfil_desempeno +", controles),
  
  "SET2a_gen_x_parental" = paste(
    "perfil_genero * perfil_parental +",
    "perfil_estado_civil + perfil_plan_futuro +",
    "perfil_ocupacion + perfil_exp + perfil_desempeno +", controles),
  
  "SET2b_gen_x_planfam" = paste(
    "perfil_genero * perfil_plan_futuro +",
    "perfil_estado_civil + perfil_parental +",
    "perfil_ocupacion + perfil_exp + perfil_desempeno +", controles),
  
  "SET3_gen_x_ecivil" = paste(
    "perfil_genero * perfil_estado_civil +",
    "perfil_parental + perfil_plan_futuro +",
    "perfil_ocupacion + perfil_exp + perfil_desempeno +", controles),
  
  "SET4a_gen_x_ecivil_x_parental" = paste(
    "perfil_genero * perfil_estado_civil * perfil_parental +",
    "perfil_plan_futuro +",
    "perfil_ocupacion + perfil_exp + perfil_desempeno +", controles),
  
  "SET4b_gen_x_parental_x_plan" = paste(
    "perfil_genero * perfil_parental * perfil_plan_futuro +",
    "perfil_estado_civil +",
    "perfil_ocupacion + perfil_exp + perfil_desempeno +", controles)
)


# 5. CORRER MODELOS


correr_modelos <- function(formula_str, outcomes, datos) {
  map(outcomes, function(y) {
    fml <- as.formula(paste(y, "~", formula_str))
    lm_robust(fml,
              data     = datos,
              clusters = numericalId,
              se_type  = "CR2")
  }) %>% set_names(outcomes)
}

resultados <- map(sets_formulas, ~ correr_modelos(., all_outcomes, df))
cat("✓ Todos los modelos corridos\n")


# 6. EXPORTAR TABLAS


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

# SET 1
exportar_tabla(resultados[["SET1_principales"]], outcomes_p1,             "tablas/SET1_contratacion.docx")
exportar_tabla(resultados[["SET1_principales"]], outcomes_evaluacion,     "tablas/SET1_evaluaciones.docx")
exportar_tabla(resultados[["SET1_principales"]], outcomes_comportamiento, "tablas/SET1_comportamiento.docx")

# SET 2a
exportar_tabla(resultados[["SET2a_gen_x_parental"]], outcomes_p1,             "tablas/SET2a_contratacion.docx")
exportar_tabla(resultados[["SET2a_gen_x_parental"]], outcomes_evaluacion,     "tablas/SET2a_evaluaciones.docx")
exportar_tabla(resultados[["SET2a_gen_x_parental"]], outcomes_comportamiento, "tablas/SET2a_comportamiento.docx")

# SET 2b
exportar_tabla(resultados[["SET2b_gen_x_planfam"]], outcomes_p1,             "tablas/SET2b_contratacion.docx")
exportar_tabla(resultados[["SET2b_gen_x_planfam"]], outcomes_evaluacion,     "tablas/SET2b_evaluaciones.docx")
exportar_tabla(resultados[["SET2b_gen_x_planfam"]], outcomes_comportamiento, "tablas/SET2b_comportamiento.docx")

# SET 3
exportar_tabla(resultados[["SET3_gen_x_ecivil"]], outcomes_p1,             "tablas/SET3_contratacion.docx")
exportar_tabla(resultados[["SET3_gen_x_ecivil"]], outcomes_evaluacion,     "tablas/SET3_evaluaciones.docx")
exportar_tabla(resultados[["SET3_gen_x_ecivil"]], outcomes_comportamiento, "tablas/SET3_comportamiento.docx")

# SET 4a
exportar_tabla(resultados[["SET4a_gen_x_ecivil_x_parental"]], outcomes_p1,             "tablas/SET4a_contratacion.docx")
exportar_tabla(resultados[["SET4a_gen_x_ecivil_x_parental"]], outcomes_evaluacion,     "tablas/SET4a_evaluaciones.docx")
exportar_tabla(resultados[["SET4a_gen_x_ecivil_x_parental"]], outcomes_comportamiento, "tablas/SET4a_comportamiento.docx")

# SET 4b
exportar_tabla(resultados[["SET4b_gen_x_parental_x_plan"]], outcomes_p1,             "tablas/SET4b_contratacion.docx")
exportar_tabla(resultados[["SET4b_gen_x_parental_x_plan"]], outcomes_evaluacion,     "tablas/SET4b_evaluaciones.docx")
exportar_tabla(resultados[["SET4b_gen_x_parental_x_plan"]], outcomes_comportamiento, "tablas/SET4b_comportamiento.docx")

cat("✓ Todas las tablas exportadas a carpeta tablas/\n")


# 7. EXTRAER RESULTADOS PARA FIGURAS


resultados_set1  <- map_dfr(all_outcomes, ~tidy(resultados[["SET1_principales"]][[.x]])               %>% mutate(outcome = .x))
resultados_set2a <- map_dfr(all_outcomes, ~tidy(resultados[["SET2a_gen_x_parental"]][[.x]])           %>% mutate(outcome = .x))
resultados_set2b <- map_dfr(all_outcomes, ~tidy(resultados[["SET2b_gen_x_planfam"]][[.x]])            %>% mutate(outcome = .x))
resultados_set3  <- map_dfr(all_outcomes, ~tidy(resultados[["SET3_gen_x_ecivil"]][[.x]])              %>% mutate(outcome = .x))
resultados_set4a <- map_dfr(all_outcomes, ~tidy(resultados[["SET4a_gen_x_ecivil_x_parental"]][[.x]]) %>% mutate(outcome = .x))
resultados_set4b <- map_dfr(all_outcomes, ~tidy(resultados[["SET4b_gen_x_parental_x_plan"]][[.x]])   %>% mutate(outcome = .x))


#ver términos 

cat("\n--- Términos: SET1 ---\n");  resultados_set1  %>% distinct(term) %>% pull(term) %>% sort() %>% print()
cat("\n--- Términos: SET2a ---\n"); resultados_set2a %>% distinct(term) %>% pull(term) %>% sort() %>% print()
cat("\n--- Términos: SET2b ---\n"); resultados_set2b %>% distinct(term) %>% pull(term) %>% sort() %>% print()
cat("\n--- Términos: SET3 ---\n");  resultados_set3  %>% distinct(term) %>% pull(term) %>% sort() %>% print()
cat("\n--- Términos: SET4a ---\n"); resultados_set4a %>% distinct(term) %>% pull(term) %>% sort() %>% print()
cat("\n--- Términos: SET4b ---\n"); resultados_set4b %>% distinct(term) %>% pull(term) %>% sort() %>% print()


# 8. FUNCIÓN DE ETIQUETAS


agregar_etiquetas <- function(df, nombre_set) {
  df %>%
    mutate(
      set = nombre_set,
      
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
        
        # -------------------------------------------------------
        # EFECTOS PRINCIPALES
        # -------------------------------------------------------
        term == "perfil_generoMujer"                          ~ "Género: Mujer",
        term == "perfil_estado_civilcasado/a"                 ~ "Estado civil: Casado/a",
        term == "perfil_estado_civilconviviendo"              ~ "Estado civil: Conviviendo",
        term == "perfil_parentaltiene un hijo"                ~ "Parental: Un hijo",
        term == "perfil_parentaltiene dos hijos"              ~ "Parental: Dos hijos",
        term == "perfil_plan_futuroen 2 años"                 ~ "Plan: en 2 años",
        term == "perfil_plan_futuroen 5 años"                 ~ "Plan: en 5 años",
        term == "perfil_exp6 años"                            ~ "Experiencia: 6 años",
        term == "perfil_exp8 años"                            ~ "Experiencia: 8 años",
        term == "perfil_desempeno6.0"                         ~ "Desempeño: 6.0",
        term == "perfil_desempeno6.3"                         ~ "Desempeño: 6.3",
        term == "perfil_ocupacionasociado/a financiero/a"     ~ "Ocupación: asociado/a financiero/a",
        term == "pantalla_orden2"                             ~ "Orden: 2do perfil",
        term == "template_contextoB"                          ~ "Template: B",
        
        # -------------------------------------------------------
        # INTERACCIONES TRIPLES SET 4a: género × estado civil × parental
        # -------------------------------------------------------
        term == "perfil_generoMujer:perfil_estado_civilcasado/a:perfil_parentaltiene dos hijos"    ~ "Mujer × Casado/a × Dos hijos",
        term == "perfil_generoMujer:perfil_estado_civilcasado/a:perfil_parentaltiene un hijo"      ~ "Mujer × Casado/a × Un hijo",
        term == "perfil_generoMujer:perfil_estado_civilconviviendo:perfil_parentaltiene dos hijos" ~ "Mujer × Conviviendo × Dos hijos",
        term == "perfil_generoMujer:perfil_estado_civilconviviendo:perfil_parentaltiene un hijo"   ~ "Mujer × Conviviendo × Un hijo",
        
        # -------------------------------------------------------
        # INTERACCIONES TRIPLES SET 4b: género × parental × plan
        # -------------------------------------------------------
        term == "perfil_generoMujer:perfil_parentaltiene dos hijos:perfil_plan_futuroen 2 años"    ~ "Mujer × Dos hijos × Plan en 2 años",
        term == "perfil_generoMujer:perfil_parentaltiene dos hijos:perfil_plan_futuroen 5 años"    ~ "Mujer × Dos hijos × Plan en 5 años",
        term == "perfil_generoMujer:perfil_parentaltiene un hijo:perfil_plan_futuroen 2 años"      ~ "Mujer × Un hijo × Plan en 2 años",
        term == "perfil_generoMujer:perfil_parentaltiene un hijo:perfil_plan_futuroen 5 años"      ~ "Mujer × Un hijo × Plan en 5 años",
        
        # -------------------------------------------------------
        # INTERACCIONES DE DOS VÍAS: género × estado civil
        # -------------------------------------------------------
        term == "perfil_generoMujer:perfil_estado_civilcasado/a"    ~ "Mujer × Casado/a",
        term == "perfil_generoMujer:perfil_estado_civilconviviendo" ~ "Mujer × Conviviendo",
        
        # -------------------------------------------------------
        # INTERACCIONES DE DOS VÍAS: género × parental
        # -------------------------------------------------------
        term == "perfil_generoMujer:perfil_parentaltiene un hijo"   ~ "Mujer × Un hijo",
        term == "perfil_generoMujer:perfil_parentaltiene dos hijos" ~ "Mujer × Dos hijos",
        
        # -------------------------------------------------------
        # INTERACCIONES DE DOS VÍAS: género × plan
        # -------------------------------------------------------
        term == "perfil_generoMujer:perfil_plan_futuroen 2 años"    ~ "Mujer × Plan en 2 años",
        term == "perfil_generoMujer:perfil_plan_futuroen 5 años"    ~ "Mujer × Plan en 5 años",
        
        # -------------------------------------------------------
        # INTERACCIONES DE DOS VÍAS: estado civil × parental (SET 4a)
        # -------------------------------------------------------
        term == "perfil_estado_civilcasado/a:perfil_parentaltiene dos hijos"    ~ "Casado/a × Dos hijos",
        term == "perfil_estado_civilcasado/a:perfil_parentaltiene un hijo"      ~ "Casado/a × Un hijo",
        term == "perfil_estado_civilconviviendo:perfil_parentaltiene dos hijos" ~ "Conviviendo × Dos hijos",
        term == "perfil_estado_civilconviviendo:perfil_parentaltiene un hijo"   ~ "Conviviendo × Un hijo",
        
        # -------------------------------------------------------
        # INTERACCIONES DE DOS VÍAS: parental × plan (SET 4b)
        # -------------------------------------------------------
        term == "perfil_parentaltiene dos hijos:perfil_plan_futuroen 2 años"    ~ "Dos hijos × Plan en 2 años",
        term == "perfil_parentaltiene dos hijos:perfil_plan_futuroen 5 años"    ~ "Dos hijos × Plan en 5 años",
        term == "perfil_parentaltiene un hijo:perfil_plan_futuroen 2 años"      ~ "Un hijo × Plan en 2 años",
        term == "perfil_parentaltiene un hijo:perfil_plan_futuroen 5 años"      ~ "Un hijo × Plan en 5 años",
        
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
        # Interacciones triples (antes que las de dos vías)
        term %in% c(
          "perfil_generoMujer:perfil_estado_civilcasado/a:perfil_parentaltiene dos hijos",
          "perfil_generoMujer:perfil_estado_civilcasado/a:perfil_parentaltiene un hijo",
          "perfil_generoMujer:perfil_estado_civilconviviendo:perfil_parentaltiene dos hijos",
          "perfil_generoMujer:perfil_estado_civilconviviendo:perfil_parentaltiene un hijo",
          "perfil_generoMujer:perfil_parentaltiene dos hijos:perfil_plan_futuroen 2 años",
          "perfil_generoMujer:perfil_parentaltiene dos hijos:perfil_plan_futuroen 5 años",
          "perfil_generoMujer:perfil_parentaltiene un hijo:perfil_plan_futuroen 2 años",
          "perfil_generoMujer:perfil_parentaltiene un hijo:perfil_plan_futuroen 5 años"
        )                                                                     ~ "Interacción triple",
        # Interacciones de dos vías con género
        term %in% c(
          "perfil_generoMujer:perfil_estado_civilcasado/a",
          "perfil_generoMujer:perfil_estado_civilconviviendo",
          "perfil_generoMujer:perfil_parentaltiene un hijo",
          "perfil_generoMujer:perfil_parentaltiene dos hijos",
          "perfil_generoMujer:perfil_plan_futuroen 2 años",
          "perfil_generoMujer:perfil_plan_futuroen 5 años"
        )                                                                     ~ "Interacción: Género",
        # Interacciones de dos vías sin género
        term %in% c(
          "perfil_estado_civilcasado/a:perfil_parentaltiene dos hijos",
          "perfil_estado_civilcasado/a:perfil_parentaltiene un hijo",
          "perfil_estado_civilconviviendo:perfil_parentaltiene dos hijos",
          "perfil_estado_civilconviviendo:perfil_parentaltiene un hijo",
          "perfil_parentaltiene dos hijos:perfil_plan_futuroen 2 años",
          "perfil_parentaltiene dos hijos:perfil_plan_futuroen 5 años",
          "perfil_parentaltiene un hijo:perfil_plan_futuroen 2 años",
          "perfil_parentaltiene un hijo:perfil_plan_futuroen 5 años"
        )                                                                     ~ "Interacción: Otras",
        # Controles
        term %in% c("perfil_exp6 años", "perfil_exp8 años",
                    "perfil_desempeno6.0", "perfil_desempeno6.3",
                    "perfil_ocupacionasociado/a financiero/a")                ~ "Controles perfil",
        term %in% c("pantalla_orden2", "template_contextoB")                 ~ "Controles diseño",
        TRUE                                                                   ~ "Otros"
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

# 9. FUNCIÓN PARA GRÁFICOS

graficar_set <- function(resultados_etiquetados, nombre_set, prefijo_archivo,
                         modo = "todos") {
  orden_term_label <- c(
    # Género
    "Género: Mujer",
    # Estado civil
    "Estado civil: Casado/a", "Estado civil: Conviviendo",
    # Estado parental
    "Parental: Un hijo", "Parental: Dos hijos",
    # Plan reproductivo
    "Plan: en 2 años", "Plan: en 5 años",
    # Interacciones género × estado civil
    "Mujer × Casado/a", "Mujer × Conviviendo",
    # Interacciones género × parental
    "Mujer × Un hijo", "Mujer × Dos hijos",
    # Interacciones género × plan
    "Mujer × Plan en 2 años", "Mujer × Plan en 5 años",
    # Interacciones otras: estado civil × parental
    "Casado/a × Un hijo", "Casado/a × Dos hijos",
    "Conviviendo × Un hijo", "Conviviendo × Dos hijos",
    # Interacciones otras: parental × plan
    "Un hijo × Plan en 2 años", "Un hijo × Plan en 5 años",
    "Dos hijos × Plan en 2 años", "Dos hijos × Plan en 5 años",
    # Interacciones triples SET 4a
    "Mujer × Casado/a × Un hijo", "Mujer × Casado/a × Dos hijos",
    "Mujer × Conviviendo × Un hijo", "Mujer × Conviviendo × Dos hijos",
    # Interacciones triples SET 4b
    "Mujer × Un hijo × Plan en 2 años", "Mujer × Un hijo × Plan en 5 años",
    "Mujer × Dos hijos × Plan en 2 años", "Mujer × Dos hijos × Plan en 5 años",
    # Controles perfil
    "Experiencia: 6 años", "Experiencia: 8 años",
    "Desempeño: 6.0", "Desempeño: 6.3",
    "Ocupación: asociado/a financiero/a",
    # Controles diseño
    "Orden: 2do perfil", "Template: B"
  )
  
  datos_viz <- resultados_etiquetados %>%
    filter(!str_detect(term, "Intercept"), !is.na(estimate)) %>%
    { if (modo == "interacciones") filter(., str_detect(term, ":"))
      else if (modo == "principales") filter(., !str_detect(term, ":"))
      else . } %>%
    mutate(
      term_grupo = factor(term_grupo, levels = c(
        "Género", "Estado civil", "Estado parental", "Plan reproductivo",
        "Interacción: Género", "Interacción: Otras", "Interacción triple",
        "Controles perfil", "Controles diseño"
      )),
      term_label = factor(term_label, 
                          levels = orden_term_label[orden_term_label %in% unique(term_label)])
    )
  
  sufijo <- if (modo == "interacciones") "_interacciones"
  else if (modo == "principales") "_principales"
  else ""
  
  for (grupo in unique(datos_viz$grupo_outcome)) {
    
    datos_grupo <- datos_viz %>% filter(grupo_outcome == grupo)
    n_terms     <- n_distinct(datos_grupo$term_label)
    alto        <- max(8, n_terms * 0.35)
    
    p <- ggplot(datos_grupo,
                aes(x = estimate, y = term_label, color = term_grupo)) +
      geom_vline(xintercept = 0, linetype = "dashed",
                 color = "gray50", linewidth = 0.5) +
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                     height = 0.2, linewidth = 0.7, alpha = 0.8) +
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
      facet_wrap(~ outcome_label, ncol = 2, scales = "free_x") +
      scale_color_manual(
        values = c(
          "Género"               = "#e41a1c",
          "Estado civil"         = "#377eb8",
          "Estado parental"      = "#4daf4a",
          "Plan reproductivo"    = "#ff7f00",
          "Interacción: Género"  = "#a65628",
          "Interacción: Otras"   = "#984ea3",
          "Interacción triple"   = "#f781bf",
          "Controles perfil"     = "#808080",
          "Controles diseño"     = "#cccccc"
        ),
        breaks = levels(datos_grupo$term_grupo)[
          levels(datos_grupo$term_grupo) %in% unique(datos_grupo$term_grupo)
        ]
      ) +
      labs(
        title    = paste0(nombre_set, ": ", grupo,
                          if (modo == "interacciones") " (Interacciones)"
                          else if (modo == "principales") " (Efectos principales)"
                          else ""),
        subtitle = "Coeficientes OLS clusterizados (IC 95%)\nRef: Hombre, Soltero/a, Sin hijos, Sin planes de tener hijos",
        x        = "Coeficiente estimado",
        y        = NULL,
        color    = "Tipo de variable",
        caption  = ". p < 0.1   * p < 0.05   ** p < 0.01   *** p < 0.001"
      ) +
      theme_bw(base_size = 11) +
      theme(
        strip.background = element_rect(fill = "#f0f0f0"),
        strip.text       = element_text(face = "bold", size = 9),
        legend.position  = "bottom",
        legend.title     = element_text(face = "bold"),
        plot.title       = element_text(face = "bold", size = 13),
        plot.subtitle    = element_text(size = 9, color = "gray40"),
        axis.text.y      = element_text(size = 9)
      )
    
    nombre_archivo <- paste0(
      "figuras/",
      prefijo_archivo, sufijo, "_",
      tolower(str_replace_all(grupo, " ", "_")),
      ".png"
    )
    
    ggsave(nombre_archivo, plot = p,
           width = 14, height = alto, dpi = 150, bg = "white")
    cat("✓ Gráfico guardado:", nombre_archivo, "\n")
  }
}


# 10. GENERAR FIGURAS


# SET 1, 2a, 2b, 3 → modo "todos"
resultados_set1 %>%
  agregar_etiquetas("SET1: Efectos principales") %>%
  graficar_set("SET1 — Efectos principales", "grafico_set1", modo = "todos")

resultados_set2a %>%
  agregar_etiquetas("SET2a: Género × Estado parental") %>%
  graficar_set("SET2a — Género × Estado parental", "grafico_set2a", modo = "todos")

resultados_set2b %>%
  agregar_etiquetas("SET2b: Género × Plan familia") %>%
  graficar_set("SET2b — Género × Plan familia", "grafico_set2b", modo = "todos")

resultados_set3 %>%
  agregar_etiquetas("SET3: Género × Estado civil") %>%
  graficar_set("SET3 — Género × Estado civil", "grafico_set3", modo = "todos")

# SET 4a → dos figuras separadas
resultados_set4a_etiq <- resultados_set4a %>%
  agregar_etiquetas("SET4a: Género × Estado civil × Parental")

resultados_set4a_etiq %>%
  graficar_set("SET4a — Género × Estado civil × Parental",
               "grafico_set4a", modo = "principales")

resultados_set4a_etiq %>%
  graficar_set("SET4a — Género × Estado civil × Parental",
               "grafico_set4a", modo = "interacciones")

# SET 4b → dos figuras separadas
resultados_set4b_etiq <- resultados_set4b %>%
  agregar_etiquetas("SET4b: Género × Parental × Plan familia")

resultados_set4b_etiq %>%
  graficar_set("SET4b — Género × Parental × Plan familia",
               "grafico_set4b", modo = "principales")

resultados_set4b_etiq %>%
  graficar_set("SET4b — Género × Parental × Plan familia",
               "grafico_set4b", modo = "interacciones")

cat("\n✓ Proceso completado.\n")
cat("   Tablas en:  ~/Desktop/Diseno_demanda/tablas/\n")
cat("   Figuras en: ~/Desktop/Diseno_demanda/figuras/\n")
