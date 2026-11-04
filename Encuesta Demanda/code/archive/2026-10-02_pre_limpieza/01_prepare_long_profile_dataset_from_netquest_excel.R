# ============================================================================
# 01 - Prepare long profile dataset from Netquest Excel
# Author: valentina gonzalez
# Modified: Tania Hutt
  # ampliar categorias de cargo y rubro
  # arreglar recodificacion attention check
  # ampliar categorias de preguntas abiertas
# Date: agosto 2026
# Purpose: Reads the updated Netquest Excel file, builds the respondent-profile long dataset, cleans profile
# ============================================================================

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(forcats)
library(here)
library(readxl)

project_dir <- if (dir.exists(here("Encuesta Demanda", "Data"))) {
  here("Encuesta Demanda")
} else {
  here()
}

clean_netquest_nulls <- function(data) {
  data %>%
    mutate(across(where(is.character), ~ na_if(str_squish(.x), "#NULL!"))) %>%
    mutate(across(where(is.character), ~ na_if(str_squish(.x), "NULL!")))
}

# ==============================================================================
# 1. CARGA DE BASE ORIGINAL
# ==============================================================================
datos_demanda <- read_excel(
  file.path(project_dir, "Data", "Raw", "UCXXCL_289723_cMain_20260730.xlsx"),
  sheet = "Datos"
) %>%
  clean_netquest_nulls()

# ==============================================================================
# 2. TRANSFORMACION A FORMATO LARGO (VIGNETTE LEVEL)
# ==============================================================================

# --- POSICION 1 (Primer perfil evaluado) ---
df_pos1 <- datos_demanda %>%
  mutate(
    pantalla_orden = 1L,
    bloque_id = POSTULANTE_1_BLOCK,
    template_contexto = POSTULANTE_1_POSTULANTE, 
    
    # Atributos de la viÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â±eta
    perfil_nombre = POSTULANTE_1_PLACEHOLDER_NOMBRE,
    perfil_genero = forcats::as_factor(POSTULANTE_1_TEXTO_GENERO),
    perfil_ocupacion = POSTULANTE_1_PLACEHOLDER_OCUPACION,
    perfil_educacion = POSTULANTE_1_PLACEHOLDER_EDUCACION,
    perfil_exp = POSTULANTE_1_PLACEHOLDER_EXP,
    perfil_desempeno = POSTULANTE_1_PLACEHOLDER_DESEMPENO,
    perfil_familia = POSTULANTE_1_TEXTO_FAMILIA_COMPLETO,
    
    # Comportamiento 
    time_screen_perfil = TIME_SCREEN_POSTULANTE_1,
    click_popit = coalesce(CLICK_POPIT_P1_TA_P1, CLICK_POPIT_P1_TB_P1),
    time_popit = coalesce(TIME_POPIT_P1_TA_P1, TIME_POPIT_P1_TB_P1),
    
    # Escala global de recomendacion
    ans_p1_global = coalesce(P1_TA_P1, P1_TB_P1),

    # Escala Likert de percepciones del perfil
    # 1 = nada, 7 = extremadamente
    # Ambicioso
    ans_p2_1 = coalesce(P2_TA_P1_1, P2_TB_P1_1),
    # Confiable
    ans_p2_2 = coalesce(P2_TA_P1_2, P2_TB_P1_2),
    # Competente
    ans_p2_3 = coalesce(P2_TA_P1_3, P2_TB_P1_3),
    # Riesgoso en continuidad laboral
    ans_p2_4 = coalesce(P2_TA_P1_4, P2_TB_P1_4),
    # Costoso en salario
    ans_p2_5 = coalesce(P2_TA_P1_5, P2_TB_P1_5),
    # Costoso operativo / gestion
    ans_p2_6 = coalesce(P2_TA_P1_6, P2_TB_P1_6),
    # Comprometido a largo plazo
    ans_p2_7 = coalesce(P2_TA_P1_7, P2_TB_P1_7),
    # Prometedor laboralmente
    ans_p2_8 = coalesce(P2_TA_P1_8, P2_TB_P1_8),
    # Buena inversion
    ans_p2_9 = coalesce(P2_TA_P1_9, P2_TB_P1_9),
    
    # Escala Likert de conductas esperadas
    # 1 = nada, 7 = extremadamente
    # Llegue tarde o se ausente
    ans_p3_1 = coalesce(P3_TA_P1_1, P3_TB_P1_1),
    # Haga uso de beneficios
    ans_p3_2 = coalesce(P3_TA_P1_2, P3_TB_P1_2),
    # Solicite facilidades o permisos
    ans_p3_3 = coalesce(P3_TA_P1_3, P3_TB_P1_3),
    # Renuncie o cese actividades
    ans_p3_4 = coalesce(P3_TA_P1_4, P3_TB_P1_4),
    # Asuma nuevos desafios
    ans_p3_5 = coalesce(P3_TA_P1_5, P3_TB_P1_5),
    # Sea ascendido o promovido
    ans_p3_6 = coalesce(P3_TA_P1_6, P3_TB_P1_6)
  )

# --- POSICION 2 (Segundo perfil evaluado) ---
df_pos2 <- datos_demanda %>%
  mutate(
    pantalla_orden = 2L,
    bloque_id = POSTULANTE_2_BLOCK,
    template_contexto = POSTULANTE_2_POSTULANTE,
    
    # Atributos de la viÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â±eta
    perfil_nombre = POSTULANTE_2_PLACEHOLDER_NOMBRE,
    perfil_genero = forcats::as_factor(POSTULANTE_2_TEXTO_GENERO),
    perfil_ocupacion = POSTULANTE_2_PLACEHOLDER_OCUPACION,
    perfil_educacion = POSTULANTE_2_PLACEHOLDER_EDUCACION,
    perfil_exp = POSTULANTE_2_PLACEHOLDER_EXP,
    perfil_desempeno = POSTULANTE_2_PLACEHOLDER_DESEMPENO,
    perfil_familia = POSTULANTE_2_TEXTO_FAMILIA_COMPLETO,
    
    # Comportamiento
    time_screen_perfil = TIME_SCREEN_POSTULANTE_2,
    click_popit = coalesce(CLICK_POPIT_P1_TA_P2, CLICK_POPIT_P1_TB_P2),
    time_popit = coalesce(TIME_POPIT_P1_TA_P2, TIME_POPIT_P1_TB_P2),
    
    # Escala global de recomendacion
    ans_p1_global = coalesce(P1_TA_P2, P1_TB_P2),

    # Escala Likert de percepciones del perfil
    # 1 = nada, 7 = extremadamente
    # Ambicioso
    ans_p2_1 = coalesce(P2_TA_P2_1, P2_TB_P2_1),
    # Confiable
    ans_p2_2 = coalesce(P2_TA_P2_2, P2_TB_P2_2),
    # Competente
    ans_p2_3 = coalesce(P2_TA_P2_3, P2_TB_P2_3),
    # Riesgoso en continuidad laboral
    ans_p2_4 = coalesce(P2_TA_P2_4, P2_TB_P2_4),
    # Costoso en salario
    ans_p2_5 = coalesce(P2_TA_P2_5, P2_TB_P2_5),
    # Costoso operativo / gestion
    ans_p2_6 = coalesce(P2_TA_P2_6, P2_TB_P2_6),
    # Comprometido a largo plazo
    ans_p2_7 = coalesce(P2_TA_P2_7, P2_TB_P2_7),
    # Prometedor laboralmente
    ans_p2_8 = coalesce(P2_TA_P2_8, P2_TB_P2_8),
    # Buena inversion
    ans_p2_9 = coalesce(P2_TA_P2_9, P2_TB_P2_9),
    
    # Escala Likert de conductas esperadas
    # 1 = nada, 7 = extremadamente
    # Llegue tarde o se ausente
    ans_p3_1 = coalesce(P3_TA_P2_1, P3_TB_P2_1),
    # Haga uso de beneficios
    ans_p3_2 = coalesce(P3_TA_P2_2, P3_TB_P2_2),
    # Solicite facilidades o permisos
    ans_p3_3 = coalesce(P3_TA_P2_3, P3_TB_P2_3),
    # Renuncie o cese actividades
    ans_p3_4 = coalesce(P3_TA_P2_4, P3_TB_P2_4),
    # Asuma nuevos desafios
    ans_p3_5 = coalesce(P3_TA_P2_5, P3_TB_P2_5),
    # Sea ascendido o promovido
    ans_p3_6 = coalesce(P3_TA_P2_6, P3_TB_P2_6)
  )

# ==============================================================================
# 3. UNION, SEPARACION DE ATRIBUTOS Y ORDENAMIENTO DE COLUMNAS
# ==============================================================================
base_long_final <- bind_rows(df_pos1, df_pos2) %>%
  arrange(numericalId, pantalla_orden) %>%
  
  # SEPARACION DEL ATRIBUTO FAMILIA
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
  
  # RENOMBRAR PREGUNTAS DEL CUESTIONARIO (Se ajusta sintaxis de 'Otro' y se quita P4)
  rename(
    abierta_costoso              = P2,              
    abierta_inversion           = P3,              
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
    
    # P16_2/P16_3 estaban invertidos respecto al texto real de la pregunta (ver revision)
    empresa_disp_fuera_horario   = P16_1,
    empresa_evitar_ausencias     = P16_2,
    empresa_priorizar_trabajo    = P16_3,
    acuerdo_progresar_priorizar  = P17_1,
    acuerdo_buen_trabaja_disp    = P17_2,
    acuerdo_postnatal_1ano       = P17_3,
    acuerdo_salacuna_todos       = P17_4
  ) %>%
  
  # ORDEN Y SELECCION DE COLUMNAS RELEVANTES
  select(
    key, numericalId, accessCount, startTime, endTime, duration, status, type,
    CodPanelista, DEVICE, SEXO, EDAD, ppi_tp, EDADR, NSE, ZONA, TARGET, CONSENT, VISITAS_1, VISITAS_1_BRAZO,
    
    bloque_id, template_contexto, pantalla_orden,
    
    perfil_nombre, perfil_genero, perfil_ocupacion, perfil_educacion, 
    perfil_exp, perfil_desempeno, perfil_familia, perfil_estado_civil, 
    perfil_parental, perfil_plan_futuro,
    
    ans_p1_global, starts_with("ans_p2_"), starts_with("ans_p3_"),
    
    abierta_costoso, abierta_inversion, check_atencion_memoria,
    
    starts_with("enc_"), starts_with("filtro_"), starts_with("empresa_"), starts_with("acuerdo_"),
    
    time_screen_perfil, click_popit, time_popit, TIME_SCREEN_CARGO, TIME_SCREEN_TAREA,
    
    starts_with("TIME_SCREEN_POSTULANTE"), starts_with("CLICK_POPIT_P1"), starts_with("TIME_POPIT_P1")
  )

# ==============================================================================
# Normaliza texto para clasificar campos abiertos sin depender de acentos.
normalizar_texto <- function(x) {
  x <- tolower(as.character(x))
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- str_squish(x)
  x
}

# 4. RECODIFICACION Y LIMPIEZA
# ==============================================================================
base_long_final <- base_long_final %>%
  mutate(
    # 1. Agrupacion Ocupacion 
    perfil_ocupacion = tolower(as.character(perfil_ocupacion)),
    perfil_ocupacion = fct_collapse(perfil_ocupacion,
                                    "analista contable" = c("analista contable"),
                                    "asociado/a financiero/a" = c("asociada financiera", "asociado financiero")
    ),
    
    # 2. Agrupacion Estado Civil
    perfil_estado_civil = str_squish(tolower(as.character(perfil_estado_civil))),
    perfil_estado_civil = case_when(
      str_detect(perfil_estado_civil, "conviv") ~ "conviviendo",
      str_detect(perfil_estado_civil, "casad") ~ "casado/a",
      str_detect(perfil_estado_civil, "solter") ~ "soltero/a",
      TRUE ~ as.character(perfil_estado_civil)
    ),

    # 3. Agrupacion Planes Futuros de Fertilidad
    # Se normaliza texto para evitar problemas de acentos/codificacion.
    perfil_plan_futuro = normalizar_texto(str_remove(as.character(perfil_plan_futuro), "\\.$")),
    perfil_plan_futuro = fct_collapse(perfil_plan_futuro,
                                      "no quiere" = c("no quiere tener mas", "no tiene planes de tenerlos en el futuro"),
                                      "en 2 anos" = c("le gustaria tener en 2 anos mas", "le gustaria tener otro en 2 anos mas"),
                                      "en 5 anos" = c("le gustaria tener en 5 anos mas", "le gustaria tener otro en 5 anos mas")
    ),

    # 4. Limpieza del campo abierto: Rubro de la empresa
    filtro_rubro_otro_clean = tolower(as.character(filtro_rubro_otro)),
    filtro_rubro_otro_clean = case_when(
      str_detect(filtro_rubro_otro_clean, "no trabajo|cesante") ~ "Sin trabajo",
      # (bare "arte" se saco de aqui: capturaba falsamente "artesania/artesano" como Arte/Diseno)
      str_detect(normalizar_texto(filtro_rubro_otro_clean), "diseno|impresion|imprenta|graficos") ~ "Arte, Diseno e Impresion",
      str_detect(filtro_rubro_otro_clean, "turismo|eventos|deportivas") ~ "Turismo y Eventos",
      str_detect(normalizar_texto(filtro_rubro_otro_clean), "consultoria|derecho|juridica|recursos humanos|contabilidad") ~ "Consultoria, RRHH y Asesorias",
      str_detect(filtro_rubro_otro_clean, "independiente") ~ "Independiente",
      str_detect(normalizar_texto(filtro_rubro_otro_clean), "carniceria|alimentos|reposteria|comercio") ~ "Comercio y Alimentos",
      str_detect(normalizar_texto(filtro_rubro_otro_clean), "aseo|mantencion|piscinas|seguridad") ~ "Mantenimiento y Servicios",
      str_detect(normalizar_texto(filtro_rubro_otro_clean), "mineria|minero|acuicolas") ~ "Mineria y Recursos Naturales",
      str_detect(normalizar_texto(filtro_rubro_otro_clean), "ingenieria|ingeneria|software|tecnologia") ~ "Ingenieria y Tecnologia",
      str_detect(normalizar_texto(filtro_rubro_otro_clean), "municipalidad|mutualidades|publico") ~ "Sector Publico / Mutualidades",
      TRUE ~ as.character(filtro_rubro_otro)
    ),
    
    # 5. Limpieza del campo abierto: Cargo
    filtro_cargo_otro_clean = tolower(as.character(filtro_cargo_otro)),
    filtro_cargo_otro_clean = case_when(
      str_detect(filtro_cargo_otro_clean, "independiente") ~ "Independiente",
      str_detect(filtro_cargo_otro_clean, "no trabajo|cesante|no tengo|no estoy trabajando") ~ "Sin trabajo / Cesante",
      str_detect(normalizar_texto(filtro_cargo_otro_clean), "duena de casa") ~ "Trabajo domestico no remunerado",
      str_detect(normalizar_texto(filtro_cargo_otro_clean), "seguridad|tecnico|auxiliar|mantencion|produccion") ~ "Operativo y Mantenimiento",
      str_detect(normalizar_texto(filtro_cargo_otro_clean), "supervisor|director|jefatura") ~ "Jefatura y Direccion",
      str_detect(normalizar_texto(filtro_cargo_otro_clean), "ingeniera|psicologo|empleado|ventas|profesional") ~ "Profesional y Administrativo",
      TRUE ~ as.character(filtro_cargo_otro)
    ),

    # 6. Flag Atencion
    # Codigo "1" es la respuesta correcta observada en los datos (valores posibles: 1-4).
    # Se elimino la rama "RUT" (codigo muerto de un formato de pregunta no usado en esta ola).
    flag_atencion_fallida = case_when(
      as.character(check_atencion_memoria) == "1" ~ 0,
      TRUE ~ 1
    )
  )

# ==============================================================================

# ==============================================================================
# 4B. SIMPLIFICACION DE PREGUNTAS ABIERTAS Y CAMPOS OTRO
# ==============================================================================
# Normaliza texto 


base_long_final <- base_long_final %>%
  mutate(
    abierta_costoso_simple = case_when(
      is.na(abierta_costoso) | str_squish(as.character(abierta_costoso)) == "" ~ NA_character_,
      str_detect(normalizar_texto(abierta_costoso), "licenc|permis|ausenc|matern|post natal|pre y post natal|embaraz|hij|madr|famili") ~ "Licencias/ausencias/maternidad",
      str_detect(normalizar_texto(abierta_costoso), "capacit|experienc|estudi|conocim|formacion|aprendiz|ensen|entrena|titulo|universitari|magister|doctorado") ~ "Capacitacion/experiencia",
      str_detect(normalizar_texto(abierta_costoso), "productiv|desempen|rendim|motiv|eficienc") ~ "Desempeno/productividad",
      str_detect(normalizar_texto(abierta_costoso), "cost|car|sueldo|remuner|inversion|gasto|viatic|hora extra|bono") ~ "Costo/sueldo",
      str_detect(normalizar_texto(abierta_costoso), "confiab|leal|compromis|responsab|seriedad|estabil|riesgo|incierto|rotacion|inestab") ~ "Confianza/compromiso",
      TRUE ~ "Otro"
    ),
    abierta_inversion_simple = case_when(
      is.na(abierta_inversion) | str_squish(as.character(abierta_inversion)) == "" ~ NA_character_,
      str_detect(normalizar_texto(abierta_inversion), "habilid|competenc|capacit|experienc|conocim|titulo|profesional") ~ "Habilidades/experiencia",
      str_detect(normalizar_texto(abierta_inversion), "productiv|aporte|benefic|rentab|inversion|resultado|crecimiento|crecer|proyeccion|ambicio|proactiv|desafio|ganan|utilidad") ~ "Aporte/productividad",
      str_detect(normalizar_texto(abierta_inversion), "responsab|compromis|actitud|confiab|seriedad|estabil") ~ "Compromiso/confianza",
      str_detect(normalizar_texto(abierta_inversion), "carga|complej|tarea|funcion|trabajo") ~ "Carga/complejidad",
      TRUE ~ "Otro"
    ),
    # NOTA: los patrones de filtro_cargo_simple / filtro_rubro_simple se ampliaron con
    # sinonimos observados en los datos (ver revision), manteniendo los mismos 6/8 buckets
    # originales. Todo lo que no calce con un bucket existente queda como "Otro".
    filtro_cargo_simple = case_when(
      is.na(filtro_cargo_otro_clean) | str_squish(as.character(filtro_cargo_otro_clean)) == "" ~ NA_character_,
      str_detect(normalizar_texto(filtro_cargo_otro_clean), "sin trabajo|cesante|sesante|no trabajo|desempleo|jubilad|pensionad|\\bnada\\b") ~ "Sin trabajo/Cesante",
      str_detect(normalizar_texto(filtro_cargo_otro_clean), "indepen|freelance|emprend|microempresari|contratista|constratista|forma liberal|\\bpyme\\b|\\bpropia\\b") ~ "Independiente",
      str_detect(normalizar_texto(filtro_cargo_otro_clean), "jefatura|direccion|director|directiv|supervisor|coordinador|gerencia|gerente|\\bjef|encargad|responsable de|inspector|manager|administrador") ~ "Jefatura/Direccion",
      str_detect(normalizar_texto(filtro_cargo_otro_clean), "profesional|analista|ingenier|psicolog|contabil|actuaria|academi|empleado|vendedor|secretari|consultor|asesor|tecnolog|trabajador|compliance|arquitect|auditor|docente") ~ "Profesional/Administrativo",
      str_detect(normalizar_texto(filtro_cargo_otro_clean), "tecnico|operari|operador|operativ|auxiliar|seguridad|manten|produccion|chofer|cajer|conserje|ayudante|capata|obrero|miner|mecanic|pintor|prevencionista|\\btens\\b") ~ "Tecnico/Operativo",
      TRUE ~ "Otro"
    ),
    filtro_rubro_simple = case_when(
      is.na(filtro_rubro_otro_clean) | str_squish(as.character(filtro_rubro_otro_clean)) == "" ~ NA_character_,
      str_detect(normalizar_texto(filtro_rubro_otro_clean), "sin trabajo|cesante|sesante|no trabajo|desemple|jubilad|pension|incapacidad para trabajar|no aplica|no tengo|ninguno|\\bn/a\\b") ~ "Sin trabajo",
      str_detect(normalizar_texto(filtro_rubro_otro_clean), "turismo|evento|entreten|espectacul|\\bcasino\\b|deporte") ~ "Turismo/Eventos",
      str_detect(normalizar_texto(filtro_rubro_otro_clean), "comercio|alimento|gastronom|carnicer|restaurant|reposter|panader|comerci|venta|retail") ~ "Comercio/Alimentos",
      str_detect(normalizar_texto(filtro_rubro_otro_clean), "consultor|derech|rrhh|contabil|asesori|publicidad|arquitect|investig|banc|factoring|capacitacion|comunicacion|cumplimiento|diseno|impresion|imprenta|grafic|outsourcing|legal") ~ "Servicios profesionales",
      str_detect(normalizar_texto(filtro_rubro_otro_clean), "ingenier|tecnolog|software|informat|miner|agric|acuic|industrial|construccion|logistic|aeronautic|aviacion|ganad|metalurg") ~ "Industria/Tecnologia/Agro",
      str_detect(normalizar_texto(filtro_rubro_otro_clean), "seguridad|aseo|manten|automotriz|automocion|mecanic|vehicul|motociclet|conserje") ~ "Mantenimiento/Seguridad",
      str_detect(normalizar_texto(filtro_rubro_otro_clean), "municipal|mutual|publico|embajada|tribunal") ~ "Sector publico/Mutualidades",
      str_detect(normalizar_texto(filtro_rubro_otro_clean), "indepen|\\bpyme\\b|rentista|emprend") ~ "Independiente",
      TRUE ~ "Otro"
    )
  )
# 5. EXPORTACION
# ==============================================================================

write_csv(
  base_long_final,
  file.path(project_dir, "Data", "processed", "base_demanda_long_final.csv")
)



