# ==============================================================================
# SCRIPT FINAL: PRETEST EXPERIMENTO MATERNIDAD
# ==============================================================================

library(haven)
library(tidyverse)
library(here)
library(sjPlot)

#  CARGA DE BASE ORIGINAL
datos_pretest <- read_sav(here("data/UCXXCL_289723_20260421.Sav"))   # Isi, aqui poner la ruta donde esté tu base de datos

# Ver libro de códigos
# view_df(datos_pretest)

# ==============================================================================
# TRANSFORMACIÓN A FORMATO LARGO 
# ==============================================================================

# --- POSICIÓN 1 (Primer perfil) ---
df_pos1 <- datos_pretest %>%
  mutate(
    pantalla_orden = 1L,
    bloque_id = POSTULANTE_1_BLOCK,
    template_contexto = POSTULANTE_1_POSTULANTE, 
    
    # Atributos de la viñeta
    perfil_nombre = POSTULANTE_1_PLACEHOLDER_NOMBRE,
    perfil_genero = as_factor(POSTULANTE_1_TEXTO_GENERO),
    perfil_ocupacion = POSTULANTE_1_PLACEHOLDER_OCUPACION,
    perfil_educacion = POSTULANTE_1_PLACEHOLDER_EDUCACION,
    perfil_exp = POSTULANTE_1_PLACEHOLDER_EXP,
    perfil_desempeno = POSTULANTE_1_PLACEHOLDER_DESEMPENO,
    perfil_familia = POSTULANTE_1_TEXTO_FAMILIA_COMPLETO,
    
    # Comportamiento 
    time_screen_perfil = TIME_SCREEN_POSTULANTE_1,
    click_popit = coalesce(CLICK_POPIT_P1_TA_P1, CLICK_POPIT_P1_TB_P1),
    time_popit = coalesce(TIME_POPIT_P1_TA_P1, TIME_POPIT_P1_TB_P1),
    
    # Respuestas a la viñeta
    ans_p1_global = coalesce(P1_TA_P1, P1_TB_P1),
    ans_p2_1 = coalesce(P2_TA_P1_1, P2_TB_P1_1),
    ans_p2_2 = coalesce(P2_TA_P1_2, P2_TB_P1_2),
    ans_p2_3 = coalesce(P2_TA_P1_3, P2_TB_P1_3),
    ans_p2_4 = coalesce(P2_TA_P1_4, P2_TB_P1_4),
    ans_p2_5 = coalesce(P2_TA_P1_5, P2_TB_P1_5),
    ans_p2_6 = coalesce(P2_TA_P1_6, P2_TB_P1_6),
    ans_p2_7 = coalesce(P2_TA_P1_7, P2_TB_P1_7),
    ans_p3_1 = coalesce(P3_TA_P1_1, P3_TB_P1_1),
    ans_p3_2 = coalesce(P3_TA_P1_2, P3_TB_P1_2),
    ans_p3_3 = coalesce(P3_TA_P1_3, P3_TB_P1_3),
    ans_p3_4 = coalesce(P3_TA_P1_4, P3_TB_P1_4),
    ans_p3_5 = coalesce(P3_TA_P1_5, P3_TB_P1_5),
    ans_p3_6 = coalesce(P3_TA_P1_6, P3_TB_P1_6),
    ans_p3_7 = coalesce(P3_TA_P1_7, P3_TB_P1_7),
    ans_p3_8 = coalesce(P3_TA_P1_8, P3_TB_P1_8)
  )

# --- POSICIÓN 2 (Segundo perfil) ---
df_pos2 <- datos_pretest %>%
  mutate(
    pantalla_orden = 2L,
    bloque_id = POSTULANTE_2_BLOCK,
    template_contexto = POSTULANTE_2_POSTULANTE,
    
    # Atributos de la viñeta
    perfil_nombre = POSTULANTE_2_PLACEHOLDER_NOMBRE,
    perfil_genero = as_factor(POSTULANTE_2_TEXTO_GENERO),
    perfil_ocupacion = POSTULANTE_2_PLACEHOLDER_OCUPACION,
    perfil_educacion = POSTULANTE_2_PLACEHOLDER_EDUCACION,
    perfil_exp = POSTULANTE_2_PLACEHOLDER_EXP,
    perfil_desempeno = POSTULANTE_2_PLACEHOLDER_DESEMPENO,
    perfil_familia = POSTULANTE_2_TEXTO_FAMILIA_COMPLETO,
    
    # Comportamiento
    time_screen_perfil = TIME_SCREEN_POSTULANTE_2,
    click_popit = coalesce(CLICK_POPIT_P1_TA_P2, CLICK_POPIT_P1_TB_P2),
    time_popit = coalesce(TIME_POPIT_P1_TA_P2, TIME_POPIT_P1_TB_P2),
    
    # Respuestas a la viñeta
    ans_p1_global = coalesce(P1_TA_P2, P1_TB_P2),
    ans_p2_1 = coalesce(P2_TA_P2_1, P2_TB_P2_1),
    ans_p2_2 = coalesce(P2_TA_P2_2, P2_TB_P2_2),
    ans_p2_3 = coalesce(P2_TA_P2_3, P2_TB_P2_3),
    ans_p2_4 = coalesce(P2_TA_P2_4, P2_TB_P2_4),
    ans_p2_5 = coalesce(P2_TA_P2_5, P2_TB_P2_5),
    ans_p2_6 = coalesce(P2_TA_P2_6, P2_TB_P2_6),
    ans_p2_7 = coalesce(P2_TA_P2_7, P2_TB_P2_7),
    ans_p3_1 = coalesce(P3_TA_P2_1, P3_TB_P2_1),
    ans_p3_2 = coalesce(P3_TA_P2_2, P3_TB_P2_2),
    ans_p3_3 = coalesce(P3_TA_P2_3, P3_TB_P2_3),
    ans_p3_4 = coalesce(P3_TA_P2_4, P3_TB_P2_4),
    ans_p3_5 = coalesce(P3_TA_P2_5, P3_TB_P2_5),
    ans_p3_6 = coalesce(P3_TA_P2_6, P3_TB_P2_6),
    ans_p3_7 = coalesce(P3_TA_P2_7, P3_TB_P2_7),
    ans_p3_8 = coalesce(P3_TA_P2_8, P3_TB_P2_8)
  )

# ==============================================================================
# UNIÓN, SEPARACIÓN DE ATRIBUTOS Y ORDENAMIENTO DE COLUMNAS
# ==============================================================================
base_long_final <- bind_rows(df_pos1, df_pos2) %>%
  arrange(numericalId, pantalla_orden) %>%
  
  # SEPARACIÓN DEL ATRIBUTO FAMILIA
  separate(
    perfil_familia, 
    into = c("perfil_estado_civil", "resto_familia"), 
    sep = ", ", 
    remove = FALSE
  ) %>%
  separate(
    resto_familia, 
    into = c("perfil_parental", "perfil_plan_futuro"), 
    sep = " y ", 
    remove = TRUE
  ) %>%
  
  # RENOMBRAR PREGUNTAS DEL CUESTIONARIO (Abiertas y Filtros)
  rename(
    abierta_costoso              = P2,              
    abierta_riesgoso             = P3,              
    abierta_intencion_estudio    = P4,              
    check_atencion_memoria       = P5,              
    enc_estado_civil             = P6,
    enc_tiene_hijos              = P7,
    enc_numero_hijos             = P8,
    enc_educacion                = P9,
    
    filtro_cargo_actual          = P10,
    filtro_cargo_otro            = `P10#98#value`,
    filtro_anos_experiencia      = P11,
    filtro_rubro_empresa         = P12,
    filtro_rubro_otro            = `P12#98#value`,
    filtro_decision_contratacion = P13,
    filtro_personal_cargo        = P14,
    filtro_horas_semana_abierta  = P15,
    
    empresa_disp_fuera_horario   = P16_1,
    empresa_priorizar_trabajo    = P16_2,
    empresa_evitar_ausencias     = P16_3,
    acuerdo_progresar_priorizar  = P17_1,
    acuerdo_buen_trabaja_disp    = P17_2,
    acuerdo_postnatal_1ano       = P17_3,
    acuerdo_salacuna_todos       = P17_4
  ) %>%
  
  # ORDEN
  select(
    #  Bloque de Imagen (Metadata y Demográficos Base)
    key, numericalId, accessCount, startTime, endTime, duration, status, type,
    CodPanelista, DEVICE, SEXO, EDAD, ppi_tp, EDADR, NSE, ZONA, TARGET, CONSENT, VISITAS_1,
    
    # Bloque, Template y Orden
    bloque_id, template_contexto, pantalla_orden,
    
    # Características del Perfil Evaluado
    perfil_nombre, perfil_genero, perfil_ocupacion, perfil_educacion, 
    perfil_exp, perfil_desempeno, perfil_familia, perfil_estado_civil, 
    perfil_parental, perfil_plan_futuro,
    
    # Evaluaciones Likert al Perfil (Recomendación, Confiable, etc.)
    ans_p1_global, starts_with("ans_p2_"), starts_with("ans_p3_"),
    
    # Preguntas Abiertas y Check de Atención
    abierta_costoso, abierta_riesgoso, abierta_intencion_estudio, check_atencion_memoria,
    
    #  Resto de encuestas y variables de la empresa
    starts_with("enc_"), starts_with("filtro_"), starts_with("empresa_"), starts_with("acuerdo_"),
    
    #  Comportamiento, Tiempos y Clics 
    time_screen_perfil, click_popit, time_popit, TIME_SCREEN_CARGO, TIME_SCREEN_TAREA,
    
    #  Variables originales de Tiempos y Clics (Para no olvidar ninguna combinación)
    starts_with("TIME_SCREEN_POSTULANTE"), starts_with("CLICK_POPIT_P1"), starts_with("TIME_POPIT_P1")
  )

table(base_long_final$filtro_rubro_otro, useNA = "always")
# ==============================================================================
# 0. RECODIFICACIÓN 
# ==============================================================================
# NOTA: fct_collapse para factores fijos y case_when para homogeneizar el texto libre de los campos "otro".

base_long_final <- base_long_final %>%
  mutate(
    # 1. Agrupación de Ocupación (2 categorías de tratamiento)
    perfil_ocupacion = fct_collapse(as.character(perfil_ocupacion),
                                    "analista contable" = c("analista contable"),
                                    "asociado/a financiero/a" = c("asociada financiera", "asociado financiero")
    ),
    
    # 2. Agrupación de Estado Civil (3 cat)
    perfil_estado_civil = fct_collapse(as.character(perfil_estado_civil),
                                       "casado/a" = c("se encuentra casada", "se encuentra casado"),
                                       "conviviendo" = c("se encuentra conviviendo"),
                                       "soltero/a" = c("se encuentra soltera", "se encuentra soltero")
    ),
    
    # 3. Agrupación de Planes Futuros de Fertilidad (3 categorías de riesgo)
    perfil_plan_futuro = fct_collapse(as.character(perfil_plan_futuro),
                                      "no quiere" = c("no quiere tener más", "no tiene planes de tenerlos en el futuro"),
                                      "en 2 años" = c("le gustaría tener en los próximos 2 años", "le gustaría tener otro en los próximos 2 años"),
                                      "en 5 años" = c("le gustaría tener en los próximos 5 años", "le gustaría tener otro en los próximos 5 años")
    ),

    # 4. Limpieza del campo abierto: Rubro de la empresa
    filtro_rubro_otro = case_when(
      str_detect(tolower(filtro_rubro_otro), "no trabajo|cesante") ~ "Sin trabajo",
      str_detect(tolower(filtro_rubro_otro), "arte|diseño|impresion|imprenta|gráficos") ~ "Arte, Diseño e Impresión",
      str_detect(tolower(filtro_rubro_otro), "turismo|eventos|deportivas") ~ "Turismo y Eventos",
      str_detect(tolower(filtro_rubro_otro), "consultoría|derecho|jurídica|recursos humanos|contabilidad") ~ "Consultoría, RRHH y Asesorías",
      str_detect(tolower(filtro_rubro_otro), "independiente") ~ "Independiente",
      str_detect(tolower(filtro_rubro_otro), "carnicería|alimentos|reppsteria") ~ "Comercio y Alimentos",
      str_detect(tolower(filtro_rubro_otro), "aseo|mantención|piscinas|seguridad") ~ "Mantenimiento y Servicios",
      str_detect(tolower(filtro_rubro_otro), "minería|minero|acuícolas") ~ "Minería y Recursos Naturales",
      str_detect(tolower(filtro_rubro_otro), "ingenieria|ingeneria|software") ~ "Ingeniería y Tecnología",
      str_detect(tolower(filtro_rubro_otro), "municipalidad|mutualidades") ~ "Sector Público / Mutualidades",
      TRUE ~ as.character(filtro_rubro_otro)
    ),
    
    # 5. Limpieza del campo abierto: Cargo
    filtro_cargo_otro = case_when(
      str_detect(tolower(filtro_cargo_otro), "independiente") ~ "Independiente",
      str_detect(tolower(filtro_cargo_otro), "no trabajo|cesante|no tengo|no estoy trabajando") ~ "Sin trabajo / Cesante",
      str_detect(tolower(filtro_cargo_otro), "dueña de casa") ~ "Trabajo doméstico no remunerado",
      str_detect(tolower(filtro_cargo_otro), "seguridad|técnico|auxiliar|mantencion|produccion") ~ "Operativo y Mantenimiento",
      str_detect(tolower(filtro_cargo_otro), "supervisor|director") ~ "Jefatura y Dirección",
      str_detect(tolower(filtro_cargo_otro), "ingeniera|psicólogo|empleado|ventas") ~ "Profesional y Administrativo",
      TRUE ~ as.character(filtro_cargo_otro)
    )
  )

table(base_long_final$filtro_rubro_otro, useNA = "always")
table(base_long_final$filtro_cargo_otro, useNA = "always")

# ==============================================================================
#  EXPORTACIÓN
# ==============================================================================
write_sav(base_long_final, here("data/base_pretest_long_final.sav"))
write_csv(base_long_final, here("data/base_pretest_long_final.csv"))


# ==============================================================================
# CHEQUEOS DE CALIDAD Y DESCRIPTIVOS 
# ==============================================================================

# --- 0. REVISIÓN DE RECODIFICACION --- 
table(base_long_final$perfil_ocupacion)
table(base_long_final$perfil_estado_civil)
table(base_long_final$perfil_plan_futuro)


# --- 1. ATENCIÓN Y FILTROS ---

# Chequeo de atención ¿Qué información sobre el postulante  NO  estaba en los perfiles que revisaste?
print(table(as_factor(base_long_final$check_atencion_memoria), useNA = "always"))

# Generar el flag de quienes NO pasaron el attention check
# (1 = Falló el chequeo / 0 = Pasó el chequeo)
base_long_final <- base_long_final %>%
  mutate(flag_atencion_fallida = case_when(
      # Si la respuesta contiene "rut", pasaron la prueba (No hay flag)
      str_detect(tolower(as_factor(check_atencion_memoria)), "rut") ~ 0,
      # Si dejaron la pregunta en blanco, se considera fallo
      is.na(check_atencion_memoria) ~ 1,
      # Cualquier otra respuesta se considera fallo (Se levanta el flag)
      TRUE ~ 1 
    )
  )

# Verificar cuántos casos fueron flageados
print(table(base_long_final$flag_atencion_fallida, useNA = "always"))


# ¿Con qué frecuencia participas en decisiones de contratación de personas?
print(table(as_factor(base_long_final$filtro_decision_contratacion), useNA = "always"))

## ¿En que rubro o sector se desempeña la empresa en la que trabaja?
print(table(as_factor(base_long_final$filtro_rubro_empresa), useNA = "always"))

## ¿En que rubro o sector (otro) se desempeña la empresa en la que trabaja?
print(table(as.factor(base_long_final$filtro_rubro_otro), useNA = "always"))

# ¿Cuál es tu cargo actual en la empresa en la que trabajas actualmente?
print(table(as_factor(base_long_final$filtro_cargo_actual), useNA = "always"))

# ¿Cuál es tu cargo (otro) actual en la empresa en la que trabajas actualmente?
print(table(as.factor(base_long_final$filtro_cargo_otro), useNA = "always"))


# --- 2. SOCIODEMOGRÁFICOS ---

# sexo
print(table(as_factor(base_long_final$SEXO), useNA = "always"))

#nse
print(table(as_factor(base_long_final$NSE), useNA = "always"))

#zona
print(table(as_factor(base_long_final$ZONA), useNA = "always"))

#edad
print(summary(base_long_final$EDAD))

#ecivil
print(table(as_factor(base_long_final$enc_estado_civil), useNA = "always"))

#tiene hijos 
print(table(as_factor(base_long_final$enc_tiene_hijos), useNA = "always"))


# --- 3. TIEMPOS ---

#tiempo total en la encuesta (segundos)
print(summary(base_long_final$duration))
#valores extremos
print(quantile(base_long_final$duration, probs = c(0.01, 0.05, 0.95, 0.99), na.rm = TRUE))

#tiempo total en la encuesta transformado a minutos
print(summary(base_long_final$duration / 60))
# alguien se demoró 75 min, quizás netquest debería dejar límite de tiempo ? 

#los time pop it no lo convertí a minutos porque casi todos duran segundos, no hay tiempos superiores.

# --- 4. CLICS EN PERFIL (POP-IT) ---
#click Nota: 1 = Hizo click, NA = No hizo click o no disponible
print(table(base_long_final$click_popit, useNA = "always"))


