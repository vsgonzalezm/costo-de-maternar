
# ============================================================
# SCRIPT: 1b_Analisis_sum_contrasts.R
#
# Análisis con codificación de efectos (sum / deviation contrasts)
# Todas las variables categóricas se centran en su promedio:
#   - Cada coeficiente = desviación de ese nivel respecto al
#     promedio de TODOS los niveles del diseño (no vs. categoría base)
#   - El intercepto = media general del diseño experimental
#   - Interpretación más limpia en diseños factoriales aleatorizados
#
# Salidas:
#   tablas/analisis covariables en promedio/
#   figuras/analisis covariables en promedio/
# ============================================================

# LIBRERÍAS
library(tidyverse)
library(haven)
library(estimatr)
library(modelsummary)
library(broom)
library(ggplot2)
library(marginaleffects)

# DIRECTORIO: detecta automáticamente desde la ubicación del script
args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("--file=", "", args[grep("--file=", args)])
if (length(script_path) > 0) {
  setwd(dirname(dirname(normalizePath(script_path))))
} else if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(dirname(normalizePath(rstudioapi::getActiveDocumentContext()$path))))
}
cat("✓ Directorio de trabajo:", getwd(), "\n")

# CARPETAS DE SALIDA
dir.create("tablas/analisis covariables en promedio",  recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/analisis covariables en promedio", recursive = TRUE, showWarnings = FALSE)
out_tablas  <- "tablas/analisis covariables en promedio"
out_figuras <- "figuras/analisis covariables en promedio"


# ============================================================
# HELPER: aplicar contr.sum con nombres de niveles legibles
#
# El nivel 'ref_level' se mueve al ÚLTIMO lugar → queda implícito
# (no aparece como coeficiente propio; su efecto = -(suma del resto))
# Los demás niveles reciben su nombre como nombre del coeficiente,
# igual que en dummy coding, así los labels del script original siguen funcionando.
# ============================================================
aplicar_contr_sum <- function(fac, ref_level = NULL) {
  if (!is.factor(fac)) fac <- as.factor(fac)
  lvls <- levels(fac)
  if (is.null(ref_level)) ref_level <- lvls[1]
  stopifnot(ref_level %in% lvls)
  lvls_nuevo <- c(setdiff(lvls, ref_level), ref_level)
  fac <- factor(fac, levels = lvls_nuevo)
  n <- length(lvls_nuevo)
  C <- contr.sum(n)
  colnames(C) <- lvls_nuevo[-n]   # nombres legibles → términos = "varNivel"
  contrasts(fac) <- C
  fac
}


# ============================================================
# 1. CARGA DE DATOS
# ============================================================
base_long_final <- read_sav("data/procesado/base_pretest_long_final.sav")


# ============================================================
# 2. PREPARACIÓN DE VARIABLES CON SUM CONTRASTS
#    Nivel omitido = el mismo "nivel base" del script original
#    → los nombres de los términos en la salida son IDÉNTICOS al script 1_Analisis.R
#    → solo cambia la interpretación (desv. del promedio, no vs. categoría ref.)
# ============================================================
df <- base_long_final %>%
  mutate(
    perfil_genero = aplicar_contr_sum(
      as_factor(perfil_genero),
      ref_level = "Hombre"),

    perfil_estado_civil = aplicar_contr_sum(
      as_factor(perfil_estado_civil),
      ref_level = "soltero/a"),

    perfil_parental = aplicar_contr_sum(
      factor(perfil_parental,
             levels = c("actualmente no tiene hijos",
                        "tiene un hijo",
                        "tiene dos hijos")),
      ref_level = "actualmente no tiene hijos"),

    perfil_plan_futuro = aplicar_contr_sum(
      as_factor(perfil_plan_futuro),
      ref_level = "no quiere"),

    # Controles de perfil — centrados en su promedio
    perfil_ocupacion  = aplicar_contr_sum(as_factor(perfil_ocupacion)),
    perfil_exp        = aplicar_contr_sum(as_factor(perfil_exp)),
    perfil_desempeno  = aplicar_contr_sum(as_factor(perfil_desempeno)),

    # Controles de diseño — centrados en su promedio
    pantalla_orden    = aplicar_contr_sum(as.factor(pantalla_orden)),
    template_contexto = aplicar_contr_sum(as.factor(template_contexto))
  )

cat("✓ Sum contrasts aplicados. Niveles omitidos (implícitos):\n")
cat("    perfil_genero:        Hombre\n")
cat("    perfil_estado_civil:  soltero/a\n")
cat("    perfil_parental:      actualmente no tiene hijos\n")
cat("    perfil_plan_futuro:   no quiere\n")
cat("    perfil_ocupacion:    ", tail(levels(df$perfil_ocupacion),  1), "\n")
cat("    perfil_exp:          ", tail(levels(df$perfil_exp),        1), "\n")
cat("    perfil_desempeno:    ", tail(levels(df$perfil_desempeno),  1), "\n")
cat("    pantalla_orden:      ", tail(levels(df$pantalla_orden),    1), "\n")
cat("    template_contexto:   ", tail(levels(df$template_contexto), 1), "\n")


# ============================================================
# 3. DEFINICIÓN DE OUTCOMES Y ETIQUETAS
# ============================================================
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


# ============================================================
# 4. FÓRMULAS POR SET  (idénticas al script original)
# ============================================================
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


# ============================================================
# 5. CORRER MODELOS
# ============================================================
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


# ============================================================
# 6. EXPORTAR TABLAS
# ============================================================
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
exportar_tabla(resultados[["SET1_principales"]], outcomes_p1,
               file.path(out_tablas, "SET1_contratacion.docx"))
exportar_tabla(resultados[["SET1_principales"]], outcomes_evaluacion,
               file.path(out_tablas, "SET1_evaluaciones.docx"))
exportar_tabla(resultados[["SET1_principales"]], outcomes_comportamiento,
               file.path(out_tablas, "SET1_comportamiento.docx"))

# SET 2a
exportar_tabla(resultados[["SET2a_gen_x_parental"]], outcomes_p1,
               file.path(out_tablas, "SET2a_contratacion.docx"))
exportar_tabla(resultados[["SET2a_gen_x_parental"]], outcomes_evaluacion,
               file.path(out_tablas, "SET2a_evaluaciones.docx"))
exportar_tabla(resultados[["SET2a_gen_x_parental"]], outcomes_comportamiento,
               file.path(out_tablas, "SET2a_comportamiento.docx"))

# SET 2b
exportar_tabla(resultados[["SET2b_gen_x_planfam"]], outcomes_p1,
               file.path(out_tablas, "SET2b_contratacion.docx"))
exportar_tabla(resultados[["SET2b_gen_x_planfam"]], outcomes_evaluacion,
               file.path(out_tablas, "SET2b_evaluaciones.docx"))
exportar_tabla(resultados[["SET2b_gen_x_planfam"]], outcomes_comportamiento,
               file.path(out_tablas, "SET2b_comportamiento.docx"))

# SET 3
exportar_tabla(resultados[["SET3_gen_x_ecivil"]], outcomes_p1,
               file.path(out_tablas, "SET3_contratacion.docx"))
exportar_tabla(resultados[["SET3_gen_x_ecivil"]], outcomes_evaluacion,
               file.path(out_tablas, "SET3_evaluaciones.docx"))
exportar_tabla(resultados[["SET3_gen_x_ecivil"]], outcomes_comportamiento,
               file.path(out_tablas, "SET3_comportamiento.docx"))

# SET 4a
exportar_tabla(resultados[["SET4a_gen_x_ecivil_x_parental"]], outcomes_p1,
               file.path(out_tablas, "SET4a_contratacion.docx"))
exportar_tabla(resultados[["SET4a_gen_x_ecivil_x_parental"]], outcomes_evaluacion,
               file.path(out_tablas, "SET4a_evaluaciones.docx"))
exportar_tabla(resultados[["SET4a_gen_x_ecivil_x_parental"]], outcomes_comportamiento,
               file.path(out_tablas, "SET4a_comportamiento.docx"))

# SET 4b
exportar_tabla(resultados[["SET4b_gen_x_parental_x_plan"]], outcomes_p1,
               file.path(out_tablas, "SET4b_contratacion.docx"))
exportar_tabla(resultados[["SET4b_gen_x_parental_x_plan"]], outcomes_evaluacion,
               file.path(out_tablas, "SET4b_evaluaciones.docx"))
exportar_tabla(resultados[["SET4b_gen_x_parental_x_plan"]], outcomes_comportamiento,
               file.path(out_tablas, "SET4b_comportamiento.docx"))

cat("✓ Todas las tablas exportadas a", out_tablas, "\n")


# ============================================================
# 7. EXTRAER RESULTADOS PARA FIGURAS
# ============================================================
resultados_set1  <- map_dfr(all_outcomes, ~tidy(resultados[["SET1_principales"]][[.x]])               %>% mutate(outcome = .x))
resultados_set2a <- map_dfr(all_outcomes, ~tidy(resultados[["SET2a_gen_x_parental"]][[.x]])           %>% mutate(outcome = .x))
resultados_set2b <- map_dfr(all_outcomes, ~tidy(resultados[["SET2b_gen_x_planfam"]][[.x]])            %>% mutate(outcome = .x))
resultados_set3  <- map_dfr(all_outcomes, ~tidy(resultados[["SET3_gen_x_ecivil"]][[.x]])              %>% mutate(outcome = .x))
resultados_set4a <- map_dfr(all_outcomes, ~tidy(resultados[["SET4a_gen_x_ecivil_x_parental"]][[.x]]) %>% mutate(outcome = .x))
resultados_set4b <- map_dfr(all_outcomes, ~tidy(resultados[["SET4b_gen_x_parental_x_plan"]][[.x]])   %>% mutate(outcome = .x))

# Diagnóstico: ver términos para verificar que los nombres son los esperados
cat("\n--- Términos SET1 ---\n")
resultados_set1 %>% distinct(term) %>% pull(term) %>% sort() %>% print()


# ============================================================
# 8. FUNCIÓN DE ETIQUETAS
#    Idéntica al script original: los nombres de términos son los mismos
#    porque usamos colnames() para nombrar las columnas del contraste.
# ============================================================
agregar_etiquetas <- function(df_res, nombre_set) {
  df_res %>%
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


# ============================================================
# 9. FUNCIÓN PARA GRÁFICOS
#    Subtitle actualizado: refleja codificación de efectos
# ============================================================
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
        subtitle = paste0(
          "Coeficientes OLS clusterizados (IC 95%) — Codificación de efectos (sum contrasts)\n",
          "Cada coeficiente = desviación respecto al promedio del diseño (no vs. categoría de referencia)"
        ),
        x        = "Coeficiente estimado (desviación del promedio)",
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

    nombre_archivo <- file.path(
      out_figuras,
      paste0(prefijo_archivo, sufijo, "_",
             tolower(str_replace_all(grupo, " ", "_")),
             ".png")
    )

    ggsave(nombre_archivo, plot = p,
           width = 14, height = alto, dpi = 150, bg = "white")
    cat("✓ Gráfico guardado:", nombre_archivo, "\n")
  }
}


# ============================================================
# 10. GENERAR FIGURAS
# ============================================================

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

# SET 4a → dos figuras separadas (efectos principales e interacciones)
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
cat("   Tablas en:  tablas/analisis covariables en promedio/\n")
cat("   Figuras en: figuras/analisis covariables en promedio/\n")

# ============================================================
# 11. FIGURAS DE VALORES ESPERADOS — SET 4b
#     Predicciones promedio marginalizadas (avg_predictions)
# ============================================================

# ============================================================
# 11.1 FUNCIÓN AUXILIAR: construir perfiles
#      Conserva explícitamente los contrasts del dataframe original
# ============================================================

hacer_perfil <- function(genero, parental, plan,
                         estado_civil = "soltero/a") {
  
  out <- tibble(
    perfil_genero = factor(
      genero,
      levels = levels(df$perfil_genero)
    ),
    
    perfil_parental = factor(
      parental,
      levels = levels(df$perfil_parental)
    ),
    
    perfil_plan_futuro = factor(
      plan,
      levels = levels(df$perfil_plan_futuro)
    ),
    
    perfil_estado_civil = factor(
      estado_civil,
      levels = levels(df$perfil_estado_civil)
    )
  )
  
  contrasts(out$perfil_genero)       <- contrasts(df$perfil_genero)
  contrasts(out$perfil_parental)     <- contrasts(df$perfil_parental)
  contrasts(out$perfil_plan_futuro)  <- contrasts(df$perfil_plan_futuro)
  contrasts(out$perfil_estado_civil) <- contrasts(df$perfil_estado_civil)
  
  out
}

# ============================================================
# 11.2 FUNCIÓN: predicciones promedio marginalizadas
#      Usa marginaleffects::avg_predictions()
# ============================================================

obtener_pred_set4b <- function(grupos) {
  
  modelos <- resultados[["SET4b_gen_x_parental_x_plan"]]
  
  map_dfr(names(grupos), function(grupo_nombre) {
    
    perfil <- grupos[[grupo_nombre]]
    
    # dataset empírico sobre el cual se marginaliza
    nd <- df %>%
      mutate(
        perfil_genero       = perfil$perfil_genero,
        perfil_parental     = perfil$perfil_parental,
        perfil_plan_futuro  = perfil$perfil_plan_futuro,
        perfil_estado_civil = perfil$perfil_estado_civil
      )
    
    map_dfr(all_outcomes, function(y) {
      
      ap <- avg_predictions(
        modelos[[y]],
        newdata = nd,
        vcov = ~numericalId
      )
      
      tibble(
        grupo    = grupo_nombre,
        outcome  = y,
        estimate = ap$estimate,
        ci_low   = ap$conf.low,
        ci_high  = ap$conf.high,
        std.error = ap$std.error
      )
    })
  })
}


# ============================================================
# 11.3 FUNCIÓN: graficar valores esperados
# ============================================================

graficar_valores_esperados <- function(datos_pred,
                                       titulo,
                                       prefijo_archivo,
                                       orden_grupos) {
  
  datos_viz <- datos_pred %>%
    mutate(
      outcome_label = recode(outcome, !!!labels_outcomes),
      
      grupo_outcome = case_when(
        outcome == "ans_p1_global"           ~ "Contratación",
        outcome %in% outcomes_evaluacion     ~ "Evaluación",
        outcome %in% outcomes_comportamiento ~ "Comportamiento futuro"
      ),
      
      genero = if_else(
        str_starts(grupo, "Mujer"),
        "Mujer",
        "Hombre"
      ),
      
      grupo = factor(grupo, levels = rev(orden_grupos))
    )
  
  for (go in c("Contratación",
               "Evaluación",
               "Comportamiento futuro")) {
    
    datos_go <- datos_viz %>%
      filter(grupo_outcome == go)
    
    n_grupos <- n_distinct(datos_go$grupo)
    alto     <- max(7, n_grupos * 0.55 + 2)
    
    n_hombre <- sum(str_starts(orden_grupos, "Hombre"))
    sep_y    <- n_hombre + 0.5
    
    p <- ggplot(
      datos_go,
      aes(
        x     = estimate,
        y     = grupo,
        color = genero,
        shape = genero
      )
    ) +
      
      geom_hline(
        yintercept = sep_y,
        color = "gray70",
        linewidth = 0.5,
        linetype = "dotted"
      ) +
      
      geom_errorbarh(
        aes(
          xmin = ci_low,
          xmax = ci_high
        ),
        height = 0.25,
        linewidth = 0.7,
        alpha = 0.8
      ) +
      
      geom_point(size = 3.5) +
      
      facet_wrap(
        ~ outcome_label,
        ncol = 2,
        scales = "free_x"
      ) +
      
      scale_color_manual(
        values = c(
          "Hombre" = "#377eb8",
          "Mujer"  = "#e41a1c"
        )
      ) +
      
      scale_shape_manual(
        values = c(
          "Hombre" = 16,
          "Mujer"  = 17
        )
      ) +
      
      labs(
        title = paste0(titulo, ": ", go),
        
        subtitle = paste0(
          "Valores esperados promedio (IC 95%)\n",
          "Marginalizados sobre la distribución empírica del resto del diseño"
        ),
        
        x = "Valor esperado",
        y = NULL,
        color = "Género",
        shape = "Género"
      ) +
      
      theme_bw(base_size = 11) +
      
      theme(
        strip.background = element_rect(fill = "#f0f0f0"),
        
        strip.text = element_text(
          face = "bold",
          size = 9
        ),
        
        legend.position = "bottom",
        
        legend.title = element_text(
          face = "bold"
        ),
        
        plot.title = element_text(
          face = "bold",
          size = 13
        ),
        
        plot.subtitle = element_text(
          size = 9,
          color = "gray40"
        ),
        
        axis.text.y = element_text(size = 9)
      )
    
    nombre_archivo <- file.path(
      out_figuras,
      paste0(
        prefijo_archivo,
        "_",
        tolower(str_replace_all(go, " ", "_")),
        ".png"
      )
    )
    
    ggsave(
      nombre_archivo,
      plot = p,
      width = 14,
      height = alto,
      dpi = 150,
      bg = "white"
    )
    
    cat("✓ Gráfico guardado:", nombre_archivo, "\n")
  }
}


# ============================================================
# 11.4 DEFINICIÓN DE GRUPOS Y GENERACIÓN DE FIGURAS
# ============================================================

cat("\n--- Generando figuras de valores esperados (SET 4b) ---\n")

# ------------------------------------------------------------
# SET A: Soltero/a, sin hijos
# ------------------------------------------------------------

grupos_sin_hijos <- list(
  
  "Hombre · Sin hijos · Sin plan" =
    hacer_perfil(
      "Hombre",
      "actualmente no tiene hijos",
      "no quiere"
    ),
  
  "Hombre · Sin hijos · Plan en 2 años" =
    hacer_perfil(
      "Hombre",
      "actualmente no tiene hijos",
      "en 2 años"
    ),
  
  "Hombre · Sin hijos · Plan en 5 años" =
    hacer_perfil(
      "Hombre",
      "actualmente no tiene hijos",
      "en 5 años"
    ),
  
  "Mujer · Sin hijos · Sin plan" =
    hacer_perfil(
      "Mujer",
      "actualmente no tiene hijos",
      "no quiere"
    ),
  
  "Mujer · Sin hijos · Plan en 2 años" =
    hacer_perfil(
      "Mujer",
      "actualmente no tiene hijos",
      "en 2 años"
    ),
  
  "Mujer · Sin hijos · Plan en 5 años" =
    hacer_perfil(
      "Mujer",
      "actualmente no tiene hijos",
      "en 5 años"
    )
)

pred_sin_hijos <- obtener_pred_set4b(grupos_sin_hijos)

graficar_valores_esperados(
  datos_pred      = pred_sin_hijos,
  titulo          = "SET4b — Valores esperados: Soltero/a, Sin hijos",
  prefijo_archivo = "ve_set4b_sin_hijos",
  orden_grupos    = names(grupos_sin_hijos)
)


# ------------------------------------------------------------
# SET B: Soltero/a, con hijos
# ------------------------------------------------------------

grupos_con_hijos <- list(
  
  "Hombre · Un hijo · Sin plan" =
    hacer_perfil(
      "Hombre",
      "tiene un hijo",
      "no quiere"
    ),
  
  "Hombre · Un hijo · Plan en 2 años" =
    hacer_perfil(
      "Hombre",
      "tiene un hijo",
      "en 2 años"
    ),
  
  "Hombre · Un hijo · Plan en 5 años" =
    hacer_perfil(
      "Hombre",
      "tiene un hijo",
      "en 5 años"
    ),
  
  "Hombre · Dos hijos · Sin plan" =
    hacer_perfil(
      "Hombre",
      "tiene dos hijos",
      "no quiere"
    ),
  
  "Hombre · Dos hijos · Plan en 2 años" =
    hacer_perfil(
      "Hombre",
      "tiene dos hijos",
      "en 2 años"
    ),
  
  "Hombre · Dos hijos · Plan en 5 años" =
    hacer_perfil(
      "Hombre",
      "tiene dos hijos",
      "en 5 años"
    ),
  
  "Mujer · Un hijo · Sin plan" =
    hacer_perfil(
      "Mujer",
      "tiene un hijo",
      "no quiere"
    ),
  
  "Mujer · Un hijo · Plan en 2 años" =
    hacer_perfil(
      "Mujer",
      "tiene un hijo",
      "en 2 años"
    ),
  
  "Mujer · Un hijo · Plan en 5 años" =
    hacer_perfil(
      "Mujer",
      "tiene un hijo",
      "en 5 años"
    ),
  
  "Mujer · Dos hijos · Sin plan" =
    hacer_perfil(
      "Mujer",
      "tiene dos hijos",
      "no quiere"
    ),
  
  "Mujer · Dos hijos · Plan en 2 años" =
    hacer_perfil(
      "Mujer",
      "tiene dos hijos",
      "en 2 años"
    ),
  
  "Mujer · Dos hijos · Plan en 5 años" =
    hacer_perfil(
      "Mujer",
      "tiene dos hijos",
      "en 5 años"
    )
)

pred_con_hijos <- obtener_pred_set4b(grupos_con_hijos)

graficar_valores_esperados(
  datos_pred      = pred_con_hijos,
  titulo          = "SET4b — Valores esperados: Soltero/a, Con hijos",
  prefijo_archivo = "ve_set4b_con_hijos",
  orden_grupos    = names(grupos_con_hijos)
)


# ------------------------------------------------------------
# SET C: Casado/a, sin hijos
# ------------------------------------------------------------

grupos_casados_sin_hijos <- list(
  
  "Hombre · Casado/a · Sin hijos · Sin plan" =
    hacer_perfil(
      "Hombre",
      "actualmente no tiene hijos",
      "no quiere",
      "casado/a"
    ),
  
  "Hombre · Casado/a · Sin hijos · Plan en 2 años" =
    hacer_perfil(
      "Hombre",
      "actualmente no tiene hijos",
      "en 2 años",
      "casado/a"
    ),
  
  "Hombre · Casado/a · Sin hijos · Plan en 5 años" =
    hacer_perfil(
      "Hombre",
      "actualmente no tiene hijos",
      "en 5 años",
      "casado/a"
    ),
  
  "Mujer · Casado/a · Sin hijos · Sin plan" =
    hacer_perfil(
      "Mujer",
      "actualmente no tiene hijos",
      "no quiere",
      "casado/a"
    ),
  
  "Mujer · Casado/a · Sin hijos · Plan en 2 años" =
    hacer_perfil(
      "Mujer",
      "actualmente no tiene hijos",
      "en 2 años",
      "casado/a"
    ),
  
  "Mujer · Casado/a · Sin hijos · Plan en 5 años" =
    hacer_perfil(
      "Mujer",
      "actualmente no tiene hijos",
      "en 5 años",
      "casado/a"
    )
)

pred_casados_sin_hijos <- obtener_pred_set4b(
  grupos_casados_sin_hijos
)

graficar_valores_esperados(
  datos_pred      = pred_casados_sin_hijos,
  titulo          = "SET4b — Valores esperados: Casado/a, Sin hijos",
  prefijo_archivo = "ve_set4b_casados_sin_hijos",
  orden_grupos    = names(grupos_casados_sin_hijos)
)

cat("\n✓ Proceso completado. Figuras en figuras/\n")


# ============================================================
# TEST SIMPLE:
# ¿Casado/a vs Conviviendo realmente difieren?
# ============================================================

library(marginaleffects)

outcomes_clave <- c(
  "ans_p1_global",
  "ans_p2_3",
  "ans_p3_4",
  "ans_p2_7",
  "ans_p3_7"
)

comparaciones_ecivil <- map_dfr(outcomes_clave, function(y) {
  
  mod <- resultados[["SET3_gen_x_ecivil"]][[y]]
  
  avg_comparisons(
    mod,
    variables = "perfil_estado_civil",
    vcov = ~numericalId
  ) %>%
    mutate(outcome = y)
  
})

comparaciones_ecivil %>%
  select(
    outcome,
    contrast,
    estimate,
    conf.low,
    conf.high,
    p.value
  ) %>%
  arrange(outcome)


##### solo entre sin hijos y sin planes, para descartar efectos #######

library(marginaleffects)

# ============================================================
# SUBGRUPO: sin hijos + sin plan
# ============================================================

nd_sub <- df %>%
  filter(
    perfil_parental == "actualmente no tiene hijos",
    perfil_plan_futuro == "no quiere"
  )

outcomes_clave <- c(
  "ans_p1_global",
  "ans_p2_3",
  "ans_p3_4",
  "ans_p2_7",
  "ans_p3_7"
)

resultados_subgrupo <- map_dfr(outcomes_clave, function(y) {
  
  mod <- resultados[["SET3_gen_x_ecivil"]][[y]]
  
  avg_comparisons(
    mod,
    variables = "perfil_estado_civil",
    newdata   = nd_sub,
    vcov      = ~numericalId
  ) %>%
    mutate(outcome = y)
  
})

resultados_subgrupo %>%
  select(outcome, contrast, estimate, conf.low, conf.high, p.value) %>%
  arrange(outcome)


interaccion_parental_ecivil <- map_dfr(outcomes_clave, function(y) {
  
  mod <- resultados[["SET3_gen_x_ecivil"]][[y]]
  
  avg_comparisons(
    mod,
    variables = "perfil_estado_civil",
    by = "perfil_parental",
    vcov = ~numericalId
  ) %>%
    mutate(outcome = y)
  
})

names(interaccion_parental_ecivil)

interaccion_parental_ecivil %>%
  select(outcome, perfil_parental, contrast, estimate, conf.low, conf.high, p.value)