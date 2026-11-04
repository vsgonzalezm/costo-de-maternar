# ==============================================================================
# DISEÑO PILOTO DE ELECCIÓN FORZADA — OFERTA
# Fecha: 2026-10-28
#
# Estructura del piloto:
#   - 2 grupos: 50 personas sin hijos y 50 personas con hijos.
#   - 3 pares de ofertas por persona.
#   - 2 alternativas por par (A/B), sin opción de salida (forced choice).
#   - 6 perfiles distintos por persona.
#   - Los mismos 50 bloques se replican en ambos grupos para que las diferencias
#     entre grupos no se confundan con diferencias en los pares mostrados.
#
# JUSTIFICACIÓN METODOLÓGICA
# En una elección A/B la información no proviene de cada perfil por separado,
# sino de la diferencia entre sus vectores de atributos: x_A - x_B. Por ello,
# este script optimiza pares y no perfiles individuales. Como aún estamos en el
# piloto y no contamos con parámetros previos sólidos, se utiliza un diseño
# local D-eficiente en beta = 0 (p_A = p_B = 0.5). Los parámetros obtenidos en
# este piloto deberán emplearse como priors —idealmente con incertidumbre— para
# construir el diseño definitivo local o bayesiano.
#
# ==============================================================================

paquetes <- c("AlgDesign", "dplyr", "tidyr", "openxlsx", "tibble")
faltantes <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltantes) > 0) {
  stop("Faltan paquetes. Instálelos antes de ejecutar: ",
       paste(faltantes, collapse = ", "))
}

library(AlgDesign)
library(dplyr)
library(tidyr)
library(openxlsx)
library(tibble)

options(contrasts = c("contr.sum", "contr.poly"))

# ==============================================================================
# 0. PARÁMETROS DEL PILOTO
# ==============================================================================
# Hay 50 bloques. Cada bloque contiene tres pares y se aplica una vez en cada
# grupo. Así, cada grupo responde exactamente el mismo diseño experimental.

set.seed(20261028)

grupos <- c("Sin_hijos", "Con_hijos")
encuestados_por_grupo <- 50L
pares_por_persona <- 3L
alternativas_por_par <- 2L
total_bloques <- encuestados_por_grupo
total_pares_diseno <- total_bloques * pares_por_persona # 150 pares
encuestados_totales <- length(grupos) * encuestados_por_grupo # 100

# Restricción de complejidad: cada par difiere en 5 o 6 de los 7 atributos.
# NO es una exigencia matemática: es una decisión entre información y facilidad
# de respuesta. Con este modelo, diagnósticos previos dieron aproximadamente:
#   3 atributos distintos: 54% de la información del óptimo irrestricto.
#   4 atributos distintos: 72%.
#   5 atributos distintos: 87%.
#   6 atributos distintos: 98%.
#   7 atributos distintos: no permite identificar los 10 parámetros del modelo.
# El óptimo dentro del rango 5-6 selecciona finalmente 6 diferencias: deja un
# atributo igual dentro de cada par y conserva casi toda la información. Si el
# piloto muestra fatiga o dificultad, basta cambiar ambos límites a 5 para crear
# pares más simples, aceptando una pérdida moderada de eficiencia.
min_atributos_diferentes <- 5L
max_atributos_diferentes <- 6L

stopifnot(
  total_pares_diseno == 150L,
  encuestados_totales == 100L,
  min_atributos_diferentes >= 1L,
  max_atributos_diferentes <= 7L
)

# ==============================================================================
# 1. DEFINIR LOS NIVELES, CANDIDATOS Y MODELO
# ==============================================================================
niveles_oferta <- c(2, 2, 2, 3, 2, 3, 2)
nombres_oferta <- c(
  "Modalidad", "Flexibilidad", "Carga", "Liderazgo",
  "Ben_desempeno", "Ben_parentales", "Cuidado"
)

# Factorial completo: 2*2*2*3*2*3*2 = 288 perfiles posibles.
cand_uni <- gen.factorial(
  levels = niveles_oferta,
  varNames = nombres_oferta,
  factors = "all"
)

frm_final <- ~ Modalidad * Flexibilidad + Carga + Liderazgo +
  Ben_desempeno + Ben_parentales + Cuidado

# En el modelo de perfiles hay intercepto + 10 coeficientes. En un forced choice
# no etiquetado el intercepto común se cancela al calcular x_A - x_B. Por tanto,
# el diseño debe identificar 10 parámetros de preferencias.
X_perfiles <- model.matrix(frm_final, cand_uni)
X_eleccion <- X_perfiles[, colnames(X_perfiles) != "(Intercept)", drop = FALSE]
k_eleccion <- ncol(X_eleccion)

stopifnot(nrow(cand_uni) == 288L, k_eleccion == 10L)

cat("\n[REPORTE DEL PILOTO]\n")
cat("Perfiles candidatos:", nrow(cand_uni), "\n")
cat("Parámetros identificables en elección forzada:", k_eleccion, "\n")
cat("Respondentes:", encuestados_totales,
    "(", encuestados_por_grupo, "por grupo )\n")
cat("Pares por grupo:", total_pares_diseno, "\n")

# ==============================================================================
# 2. CONSTRUIR EL UNIVERSO DE PARES A/B
# ==============================================================================
# combn() genera cada par no ordenado una sola vez. Con 288 perfiles hay
# choose(288, 2) = 41.328 pares candidatos. Se excluyen pares que difieren en
# menos de 5 o en los 7 atributos, de acuerdo con la regla de complejidad.

indices_pares_todos <- combn(nrow(cand_uni), 2L)

diferencias_atributos_todos <-
  as.matrix(cand_uni[indices_pares_todos[1, ], nombres_oferta]) !=
  as.matrix(cand_uni[indices_pares_todos[2, ], nombres_oferta])

n_atributos_diferentes_todos <- rowSums(diferencias_atributos_todos)

pares_admisibles <-
  n_atributos_diferentes_todos >= min_atributos_diferentes &
  n_atributos_diferentes_todos <= max_atributos_diferentes

indices_pares <- indices_pares_todos[, pares_admisibles, drop = FALSE]
n_atributos_diferentes <- n_atributos_diferentes_todos[pares_admisibles]

# Matriz de diferencias. El signo inicial es arbitrario porque luego se asignan
# posiciones A/B balanceadas para cada grupo.
X_diferencias <-
  X_eleccion[indices_pares[1, ], , drop = FALSE] -
  X_eleccion[indices_pares[2, ], , drop = FALSE]

datos_diferencias <- as.data.frame(X_diferencias)
names(datos_diferencias) <- paste0("x", seq_len(ncol(datos_diferencias)))

cat("Pares candidatos sin restricción:", ncol(indices_pares_todos), "\n")
cat("Pares candidatos admisibles (5-6 atributos distintos):",
    ncol(indices_pares), "\n")

# ==============================================================================
# 3. OPTIMIZACIÓN D-EFICIENTE DE LOS 150 PARES
# ==============================================================================
# Bajo beta = 0, p_A = p_B = 0.5 y la contribución de un par a la matriz de
# información del logit condicional es 0.25 * (x_A-x_B)(x_A-x_B)'. El factor
# 0.25 es constante, de modo que optFederov puede optimizar directamente las
# filas de diferencias sin intercepto.

set.seed(12345)
opt_pares <- optFederov(
  frml = ~ . - 1,
  data = datos_diferencias,
  nTrials = total_pares_diseno,
  criterion = "D",
  nRepeats = 100,
  nullify = 1
)

filas_seleccionadas <- opt_pares$rows
indices_pares_seleccionados <- indices_pares[, filas_seleccionadas, drop = FALSE]
X_diferencias_seleccionadas <- X_diferencias[filas_seleccionadas, , drop = FALSE]
n_diferencias_seleccionadas <- n_atributos_diferentes[filas_seleccionadas]

# Matriz de información media del MNL binario local en beta = 0.
M_local <- crossprod(X_diferencias_seleccionadas) /
  (4 * nrow(X_diferencias_seleccionadas))

log_det_M <- as.numeric(determinant(M_local, logarithm = TRUE)$modulus)
D_error_local <- exp(-log_det_M / k_eleccion)
rango_diseno <- qr(X_diferencias_seleccionadas)$rank
numero_condicion <- kappa(M_local, exact = TRUE)
autovalor_minimo <- min(eigen(M_local, symmetric = TRUE, only.values = TRUE)$values)

# Benchmark irrestricto: sirve para cuantificar cuánto cuesta la restricción
# cognitiva de permitir solo 5-6 atributos diferentes por par.
datos_diferencias_todos <- as.data.frame(
  X_eleccion[indices_pares_todos[1, ], , drop = FALSE] -
    X_eleccion[indices_pares_todos[2, ], , drop = FALSE]
)
names(datos_diferencias_todos) <- names(datos_diferencias)

set.seed(12345)
opt_pares_irrestricto <- optFederov(
  frml = ~ . - 1,
  data = datos_diferencias_todos,
  nTrials = total_pares_diseno,
  criterion = "D",
  nRepeats = 50,
  nullify = 1
)

eficiencia_relativa_irrestricto <- opt_pares$D / opt_pares_irrestricto$D

cat(sprintf(
  paste0(
    "[MÉTRICAS DE PARES] D-root: %.4f | G-efficiency: %.3f | ",
    "D-efficiency bound: %.3f | D-error local: %.4f\n"
  ),
  opt_pares$D, opt_pares$Ge, opt_pares$Dea, D_error_local
))
cat(sprintf(
  "[RESTRICCIÓN COGNITIVA] Eficiencia relativa al óptimo irrestricto: %.3f\n",
  eficiencia_relativa_irrestricto
))
cat("[IDENTIFICACIÓN] Rango:", rango_diseno, "de", k_eleccion,
    "| Número de condición:", round(numero_condicion, 2),
    "| Autovalor mínimo:", round(autovalor_minimo, 5), "\n")

# ==============================================================================
# 4. FORMAR 50 BLOQUES DE 3 PARES (6 PERFILES DISTINTOS)
# ==============================================================================
# La optimización selecciona pares; este paso los distribuye entre personas.
# Se buscan bloques donde ningún perfil aparezca dos veces para que cada persona
# vea exactamente seis ofertas distintas. Como la asignación de pares a bloques
# no altera la matriz global de información, puede resolverse después de la
# optimización D.

pares_maestros <- tibble(
  Design_Pair_ID = seq_len(total_pares_diseno),
  Profile_1 = indices_pares_seleccionados[1, ],
  Profile_2 = indices_pares_seleccionados[2, ],
  N_atributos_diferentes = n_diferencias_seleccionadas
)

formar_bloques_sin_repetidos <- function(
    pares, n_bloques, pares_por_bloque, intentos = 50000L, semilla = 999L) {
  set.seed(semilla)

  for (intento in seq_len(intentos)) {
    orden <- sample(seq_len(nrow(pares)))
    matriz_bloques <- matrix(
      orden,
      nrow = n_bloques,
      ncol = pares_por_bloque,
      byrow = TRUE
    )

    bloque_valido <- apply(matriz_bloques, 1, function(filas) {
      perfiles <- c(pares$Profile_1[filas], pares$Profile_2[filas])
      length(unique(perfiles)) == 2L * pares_por_bloque
    })

    if (all(bloque_valido)) {
      return(tibble(
        Design_Pair_ID = pares$Design_Pair_ID[as.vector(t(matriz_bloques))],
        Block = rep(seq_len(n_bloques), each = pares_por_bloque),
        Pair_in_Block = rep(seq_len(pares_por_bloque), times = n_bloques)
      ))
    }
  }

  stop("No fue posible formar bloques sin perfiles repetidos. ",
       "Aumente 'intentos' o revise las restricciones.")
}

asignacion_bloques <- formar_bloques_sin_repetidos(
  pares = pares_maestros,
  n_bloques = total_bloques,
  pares_por_bloque = pares_por_persona
)

pares_maestros <- pares_maestros %>%
  left_join(asignacion_bloques, by = "Design_Pair_ID") %>%
  arrange(Block, Pair_in_Block)

# ==============================================================================
# 5. ASIGNAR LOS MISMOS BLOQUES A LOS DOS GRUPOS
# ==============================================================================
# Cada bloque aparece una vez en Sin_hijos y una vez en Con_hijos. El orden de
# los tres pares se aleatoriza de manera independiente dentro de cada persona.
# Las posiciones A/B se balancean separadamente en cada grupo para reducir la
# posibilidad de que un sesgo izquierda/derecha se confunda con los atributos.

balancear_posiciones <- function(X_diff, semilla, intentos = 10000L) {
  set.seed(semilla)
  n <- nrow(X_diff)
  mejor_signo <- rep(1L, n)
  mejor_score <- Inf

  for (i in seq_len(intentos)) {
    # Con 150 pares, 75 mantienen orientación y 75 se invierten.
    signo <- sample(rep(c(-1L, 1L), each = n / 2L))
    score <- sum(colMeans(X_diff * signo)^2)

    if (score < mejor_score) {
      mejor_score <- score
      mejor_signo <- signo
    }
  }

  mejor_signo
}

orientacion <- bind_rows(lapply(seq_along(grupos), function(g) {
  tibble(
    Group = grupos[g],
    # Los signos están en el orden original de las 150 filas optimizadas,
    # que coincide con Design_Pair_ID = 1,...,150 (no con el orden por bloque).
    Design_Pair_ID = seq_len(total_pares_diseno),
    Signo_AB = balancear_posiciones(
      X_diferencias_seleccionadas,
      semilla = 20261028L + g
    )
  )
}))

asignacion_personas <- crossing(
  Group = grupos,
  Block = seq_len(total_bloques)
) %>%
  arrange(Group, Block) %>%
  group_by(Group) %>%
  mutate(
    ID_Grupo = row_number(),
    Respondent_ID = paste0(
      if_else(Group == "Sin_hijos", "SH", "CH"),
      "_", sprintf("%03d", ID_Grupo)
    )
  ) %>%
  ungroup()

set.seed(24680)
asignacion_pares_personas <- asignacion_personas %>%
  inner_join(pares_maestros, by = "Block", relationship = "many-to-many") %>%
  left_join(orientacion, by = c("Group", "Design_Pair_ID")) %>%
  group_by(Group, Respondent_ID) %>%
  mutate(Pair_Order = sample(seq_len(pares_por_persona))) %>%
  ungroup() %>%
  arrange(Group, Respondent_ID, Pair_Order)

# Convertir cada par en dos filas: alternativa A y alternativa B.
df_long_codigos <- bind_rows(
  asignacion_pares_personas %>%
    transmute(
      Respondent_ID, Group, ID_Grupo, Block, Pair_Order,
      Design_Pair_ID, Alternative = "A",
      Profile_ID = if_else(Signo_AB == 1L, Profile_1, Profile_2)
    ),
  asignacion_pares_personas %>%
    transmute(
      Respondent_ID, Group, ID_Grupo, Block, Pair_Order,
      Design_Pair_ID, Alternative = "B",
      Profile_ID = if_else(Signo_AB == 1L, Profile_2, Profile_1)
    )
) %>%
  mutate(
    Alternative = factor(Alternative, levels = c("A", "B")),
    Choice_Set_ID = paste(Respondent_ID, paste0("P", Pair_Order), sep = "_"),
    Chosen = NA_integer_ # Netquest: completar 1 para elegida y 0 para no elegida
  ) %>%
  left_join(
    cand_uni %>% mutate(Profile_ID = row_number()),
    by = "Profile_ID"
  ) %>%
  arrange(Group, Respondent_ID, Pair_Order, Alternative)

# ==============================================================================
# 6. INYECCIÓN DE TEXTOS EXACTOS
# ==============================================================================
# Se conserva la lógica y los textos del script original. A diferencia de la
# versión anterior, también se mantienen los códigos de los niveles, necesarios
# para analizar posteriormente el logit condicional.

procesar_textos_oferta <- function(df_input) {
  df_input %>%
    mutate(
      Texto_Modalidad = if_else(
        Modalidad == "1",
        "Híbrido, algunos días remoto y otros presencial.",
        "Todos los días presencial."
      ),
      Texto_Flexibilidad = if_else(
        Flexibilidad == "1",
        "Fijo (de 9 a.m a 6 p.m).",
        "Flexible (puedes modificar el horario según tu preferencia)."
      ),
      Texto_Carga = if_else(
        Carga == "1",
        "Nunca deberás trabajar horas extra.",
        "Frecuentemente deberás trabajar horas extra."
      ),
      Texto_Liderazgo = case_when(
        Liderazgo == "1" ~ "Tu jefe se enfoca en potenciar las fortalezas de tu equipo.",
        Liderazgo == "2" ~ "Tu jefe se enfoca en los resultados de sus trabajadores.",
        Liderazgo == "3" ~ "Tu jefe se enfoca en la organización de los procesos en el equipo."
      ),
      Texto_Desempeno = if_else(
        Ben_desempeno == "1",
        "No hay bonos por desempeño.",
        "Hay bonos anuales por desempeño."
      ),
      Texto_Parentales = case_when(
        Ben_parentales == "1" ~ "5 meses y medio para las madres y 5 días para los padres.",
        Ben_parentales == "2" ~ "12 meses para las madres y 1 mes para los padres.",
        Ben_parentales == "3" ~ "6 meses para las madres y 6 meses para los padres."
      ),
      Texto_Cuidado = if_else(
        Cuidado == "1",
        "Sala cuna para menores de 2 años, solo para las madres.",
        "Sala cuna para menores de 2 años, para madres y padres."
      )
    )
}

df_long_final <- procesar_textos_oferta(df_long_codigos)

# Base maestra de pares sin replicación por grupo. Es útil para documentar el
# diseño y revisar qué perfiles forman cada choice set.
df_pares_maestros <- bind_rows(
  pares_maestros %>%
    transmute(
      Block, Pair_in_Block, Design_Pair_ID,
      N_atributos_diferentes, Alternative_Base = "Perfil_1",
      Profile_ID = Profile_1
    ),
  pares_maestros %>%
    transmute(
      Block, Pair_in_Block, Design_Pair_ID,
      N_atributos_diferentes, Alternative_Base = "Perfil_2",
      Profile_ID = Profile_2
    )
) %>%
  left_join(
    cand_uni %>% mutate(Profile_ID = row_number()),
    by = "Profile_ID"
  ) %>%
  procesar_textos_oferta() %>%
  arrange(Block, Pair_in_Block, Alternative_Base)

# ==============================================================================
# 7. AUDITORÍA DEL DISEÑO EXPERIMENTAL
# ==============================================================================

# (a) Cada persona debe tener tres choice sets y dos alternativas por set.
chk_choice_sets <- df_long_final %>%
  count(Group, Respondent_ID, Choice_Set_ID, name = "alternativas")

# (b) Cada persona debe ver seis perfiles diferentes.
chk_perfiles_persona <- df_long_final %>%
  group_by(Group, Respondent_ID) %>%
  summarise(
    pares = n_distinct(Choice_Set_ID),
    perfiles = n(),
    perfiles_unicos = n_distinct(Profile_ID),
    .groups = "drop"
  )

# (c) Los mismos pares deben aparecer una vez en cada grupo.
chk_replicacion_grupos <- df_long_final %>%
  distinct(Group, Design_Pair_ID) %>%
  count(Design_Pair_ID, name = "n_grupos")

# (d) Balance A/B en la codificación del modelo dentro de cada grupo.
calcular_desbalance_posicion <- function(datos_grupo) {
  perfiles_A <- datos_grupo %>%
    filter(Alternative == "A") %>%
    arrange(Design_Pair_ID)
  perfiles_B <- datos_grupo %>%
    filter(Alternative == "B") %>%
    arrange(Design_Pair_ID)

  XA <- model.matrix(frm_final, perfiles_A)[, -1, drop = FALSE]
  XB <- model.matrix(frm_final, perfiles_B)[, -1, drop = FALSE]

  max(abs(colMeans(XA - XB)))
}

chk_desbalance_posicion <- df_long_final %>%
  group_by(Group) %>%
  group_modify(~ tibble(
    Max_abs_media_diferencia_AB = calcular_desbalance_posicion(.x)
  )) %>%
  ungroup()

# (e) Balance marginal de niveles entre las 300 apariciones de perfiles del
# diseño maestro. En atributos de 2 niveles se busca aproximadamente 50/50; en
# atributos de 3 niveles, aproximadamente un tercio por nivel. Como el mismo
# diseño se replica en los dos grupos, este balance es idéntico en ambos.
chk_balance_niveles <- df_pares_maestros %>%
  select(all_of(nombres_oferta)) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Atributo",
    values_to = "Nivel"
  ) %>%
  count(Atributo, Nivel, name = "N_apariciones") %>%
  group_by(Atributo) %>%
  mutate(Proporcion = N_apariciones / sum(N_apariciones)) %>%
  ungroup() %>%
  arrange(match(Atributo, nombres_oferta), Nivel)

stopifnot(
  n_distinct(df_long_final$Respondent_ID) == encuestados_totales,
  nrow(df_long_final) == encuestados_totales * pares_por_persona * alternativas_por_par,
  all(chk_choice_sets$alternativas == alternativas_por_par),
  all(chk_perfiles_persona$pares == pares_por_persona),
  all(chk_perfiles_persona$perfiles == 6L),
  all(chk_perfiles_persona$perfiles_unicos == 6L),
  all(chk_replicacion_grupos$n_grupos == length(grupos)),
  rango_diseno == k_eleccion,
  autovalor_minimo > 0
)

cat("\n[AUDITORÍA]\n")
cat("Respondentes por grupo:\n")
print(df_long_final %>% distinct(Group, Respondent_ID) %>% count(Group))
cat("Distribución del número de atributos diferentes por par:\n")
print(table(pares_maestros$N_atributos_diferentes))
cat("Máximo desbalance medio A/B por grupo:\n")
print(chk_desbalance_posicion)
cat("Balance marginal de niveles en los perfiles seleccionados:\n")
print(chk_balance_niveles)
cat("Todos los controles estructurales fueron superados.\n")

# ==============================================================================
# 8. OUTPUT ANCHO PARA PROGRAMACIÓN Y FORMATO LARGO PARA ANÁLISIS
# ==============================================================================

cols_texto <- c(
  "Texto_Modalidad", "Texto_Flexibilidad", "Texto_Carga",
  "Texto_Liderazgo", "Texto_Desempeno", "Texto_Parentales",
  "Texto_Cuidado"
)

# Una fila por persona. Los prefijos P1_A, P1_B, ..., P3_B identifican de forma
# inequívoca los tres pares y las dos alternativas de cada uno.
df_wide_final <- df_long_final %>%
  select(
    Respondent_ID, Group, ID_Grupo, Block, Pair_Order, Alternative,
    Design_Pair_ID, Profile_ID, all_of(cols_texto)
  ) %>%
  pivot_wider(
    id_cols = c(Respondent_ID, Group, ID_Grupo, Block),
    names_from = c(Pair_Order, Alternative),
    values_from = c(Design_Pair_ID, Profile_ID, all_of(cols_texto)),
    names_glue = "P{Pair_Order}_{Alternative}_{.value}"
  ) %>%
  arrange(Group, ID_Grupo)

# Tabla compacta de métricas para dejar trazabilidad del diseño generado.
metricas_diseno <- tibble(
  Fecha = "2026-10-28",
  Perfiles_candidatos = nrow(cand_uni),
  Pares_candidatos_admisibles = ncol(indices_pares),
  Pares_seleccionados = total_pares_diseno,
  Parametros_eleccion = k_eleccion,
  Rango = rango_diseno,
  D_root = opt_pares$D,
  G_efficiency = opt_pares$Ge,
  D_efficiency_bound = opt_pares$Dea,
  D_error_local = D_error_local,
  Eficiencia_relativa_irrestricto = eficiencia_relativa_irrestricto,
  Numero_condicion = numero_condicion,
  Autovalor_minimo = autovalor_minimo
)

# ==============================================================================
# 9. EXPORTACIÓN
# ==============================================================================
# Los archivos se escriben en la carpeta desde la cual se ejecuta el script,
# igual que en la versión original. Se recomienda ejecutarlo desde Piloto Oferta.

write.csv(
  df_long_final,
  "Netquest_Oferta_Piloto_Pares_Long_100.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8",
  na = ""
)

write.csv(
  df_wide_final,
  "Netquest_Oferta_Piloto_Pares_Wide_100.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8",
  na = ""
)

write.csv(
  df_pares_maestros,
  "Netquest_Oferta_Piloto_Pares_Maestro_150.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  metricas_diseno,
  "Netquest_Oferta_Piloto_Pares_Metricas.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  chk_balance_niveles,
  "Netquest_Oferta_Piloto_Pares_Balance_Niveles.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.xlsx(
  list(
    Programacion_Wide = df_wide_final,
    Analisis_Long = df_long_final,
    Pares_Maestros = df_pares_maestros,
    Metricas = metricas_diseno,
    Balance_Niveles = chk_balance_niveles,
    Auditoria_AB = chk_desbalance_posicion
  ),
  "Netquest_Oferta_Piloto_Pares_100.xlsx",
  overwrite = TRUE
)

cat("\n[EXPORTACIÓN COMPLETA]\n")
cat("Se generaron bases para 100 respondentes, 300 choice sets y 600 alternativas.\n")

