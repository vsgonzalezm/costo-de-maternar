# ==============================================================================
# DISEÑO ALEATORIO — EXPERIMENTO DE OFERTA (ELECCIÓN FORZADA + CALIFICACIÓN)
# Fecha: 2026-11-16
# Archivo: 20261116_diseno_oferta_aleatorio.R
#
# FUNDAMENTO DEL DISEÑO (versión breve)
# - Resultado principal: probabilidad de tener la intención de un hijo, calificada
#   para cada escenario por separado (escala 0-4 o 0-10). Secundario: momento
#   (timing, ordinal). La elección forzada A/B es un chequeo de robustez.
# - Siguiendo a Aassve et al. (2026, PNAS), cada nivel de cada atributo se asigna
#   de forma independiente y aleatoria, SIN restringir cuántos atributos difieren
#   dentro de un par. Esto:
#   (a) mantiene insesgados los efectos marginales de las calificaciones por perfil;
#   (b) hace que los atributos de cada perfil sean estadísticamente independientes
#       de los de su pareja y de los del perfil calificado justo antes, de modo que
#       los efectos de anclaje se promedian y pueden testearse agregando al modelo
#       los atributos de la pareja;
#   (c) permite estimar por diseño el ACP de Ganter (2023) en la elección forzada.
# - Reemplaza el diseño local D-óptimo de pares (20261028_piloto_oferta_pares.R),
#   que forzaba 5-6 atributos distintos por par e inducía una fuerte correlación
#   negativa (~ -0,7) entre los niveles propios y los de la pareja.
# - La asignación completa se genera antes del terreno con semilla fija y se
#   audita antes de enviarla a Netquest (Su & Steiner 2020; Kleinewiese 2022).
#
# ESTRUCTURA POR PERSONA
# - 3 pares y 6 perfiles distintos por persona.
# - Secuencia de pantallas por par: (1) A y B juntos + elección forzada;
#   (2) escenario A solo + probabilidad + timing; (3) escenario B solo + ídem.
#   Orden de calificación dentro de la persona: 1..6 = P1A, P1B, P2A, P2B, P3A, P3B.
#
# ALGORITMO (se aplica por separado dentro de cada estrato)
# 1. Reparto aleatorio balanceado: cada uno de los 288 perfiles entra al pozo
#    floor(n*6/288) veces y el resto (n*6 mod 288) se completa con perfiles
#    distintos sorteados sin reposición; luego el pozo se baraja. Así, dentro del
#    estrato, las frecuencias de los perfiles difieren a lo más en 1.
# 2. El pozo barajado se reparte en mazos de 6 IDs consecutivos (uno por persona).
# 3. Si un mazo tiene un perfil repetido, se intercambia ese elemento con uno
#    aleatorio de otro mazo, solo si el intercambio no crea duplicados en ninguno
#    de los dos. Se repite hasta que no queden duplicados (con tope de iteraciones).
# 4. Orden del mazo = orden de presentación: posiciones (1,2) = par 1 (A, B),
#    (3,4) = par 2 y (5,6) = par 3. El barajado ya aleatoriza el orden de los
#    pares y la posición A/B.
# 5. NO se impone ninguna regla sobre el número de atributos distintos por par y
#    NO se vuelve a aleatorizar para mejorar la auditoría: las alertas se
#    informan, no se corrigen.
#
# USO
#   Desde la carpeta "Piloto Oferta/code":
#     Rscript 20261116_diseno_oferta_aleatorio.R            # usa 'modo' de abajo
#     Rscript 20261116_diseno_oferta_aleatorio.R principal  # fuerza el modo
# ==============================================================================

# ==============================================================================
# 0. PARÁMETROS — CONFIRMAR ANTES DE EJECUTAR
# ==============================================================================

# CONFIRMAR: "piloto" o "principal". Un argumento por línea de comandos lo reemplaza.
modo <- "piloto"

# CONFIRMAR: semilla única de toda la aleatorización.
semilla <- 20261116L

# CONFIRMAR: tarea por persona.
perfiles_por_persona <- 6L
pares_por_persona    <- 3L

# CONFIRMAR: estratos de aleatorización. Prefijo_ID se usa para Respondent_ID.
estratos_por_modo <- list(
  piloto = tibble::tibble(
    Estrato    = c("Sin_hijos", "Con_hijos"),
    Prefijo_ID = c("SH", "CH"),
    n          = c(50L, 50L)
  ),
  principal = tibble::tibble(
    Estrato    = "Total",
    Prefijo_ID = "R",
    n          = 1470L
  )
  # Ejemplo para dividir la muestra principal en género x paridad (sin hijos /
  # un hijo / dos o más) cuando se confirmen los tamaños. Reemplace el bloque
  # 'principal' de arriba por este y complete los n (deben sumar 1470):
  # principal = tibble::tibble(
  #   Estrato    = c("Mujer_sin_hijos", "Mujer_un_hijo", "Mujer_dos_o_mas",
  #                  "Hombre_sin_hijos", "Hombre_un_hijo", "Hombre_dos_o_mas"),
  #   Prefijo_ID = c("MS", "M1", "M2", "HS", "H1", "H2"),
  #   n          = c(NA, NA, NA, NA, NA, NA)
  # )
)

# CONFIRMAR: carpeta de salida (relativa al directorio de trabajo; se asume que
# el script se ejecuta desde "Piloto Oferta/code"). Se crea si no existe.
dir_salida <- file.path("..", "outputs", "diseno_aleatorio")

# Si es FALSE, el script se detiene (o pregunta, en sesión interactiva) antes de
# sobrescribir cualquier archivo de salida que ya exista.
permitir_sobrescribir <- FALSE

# Tope de iteraciones para la reparación de mazos con perfiles repetidos.
max_iter_reparacion <- 100000L

# ==============================================================================
# 1. PAQUETES Y OPCIONES
# ==============================================================================
paquetes <- c("dplyr", "tidyr", "tibble", "openxlsx")
faltantes <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltantes) > 0) {
  stop("Faltan paquetes. Instálelos antes de ejecutar: ",
       paste(faltantes, collapse = ", "))
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(openxlsx)
})

options(contrasts = c("contr.sum", "contr.poly"))

# Se fija explícitamente el generador para que la asignación sea reproducible
# aunque cambien los valores por defecto de R en versiones futuras.
RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion",
        sample.kind = "Rejection")

args_cli <- commandArgs(trailingOnly = TRUE)
if (length(args_cli) >= 1L) modo <- args_cli[1]

if (!modo %in% names(estratos_por_modo)) {
  stop("'modo' debe ser uno de: ", paste(names(estratos_por_modo), collapse = ", "))
}
estratos <- estratos_por_modo[[modo]]

stopifnot(
  perfiles_por_persona == 2L * pares_por_persona,
  !anyDuplicated(estratos$Estrato),
  !anyDuplicated(estratos$Prefijo_ID),
  all(!is.na(estratos$n)),
  all(estratos$n == round(estratos$n)),
  all(estratos$n >= 2L)
)
estratos$n <- as.integer(estratos$n)

n_total <- sum(estratos$n)
sufijo  <- paste0(modo, "_", n_total)

# ==============================================================================
# 2. CONTROL DE SOBRESCRITURA (antes de cualquier cálculo)
# ==============================================================================
nombre_salida <- function(base, ext) {
  file.path(dir_salida, paste0("20261116_Oferta_", base, "_", sufijo, ".", ext))
}

archivos_salida <- c(
  long         = nombre_salida("Long", "csv"),
  wide         = nombre_salida("Wide", "csv"),
  catalogo     = nombre_salida("Catalogo_Perfiles", "csv"),
  balance      = nombre_salida("Auditoria_Balance", "csv"),
  correlacion  = nombre_salida("Auditoria_Correlaciones", "csv"),
  metricas     = nombre_salida("Metricas", "csv"),
  xlsx         = nombre_salida("Diseno", "xlsx"),
  reproducible = nombre_salida("Reproducibilidad", "txt")
)

existentes <- archivos_salida[file.exists(archivos_salida)]
if (length(existentes) > 0L && !permitir_sobrescribir) {
  mensaje <- paste0(
    "Ya existen estos archivos de salida:\n  ",
    paste(existentes, collapse = "\n  ")
  )
  if (interactive()) {
    respuesta <- utils::menu(c("Sí, sobrescribir", "No, detener"),
                             title = paste0(mensaje, "\n¿Desea sobrescribirlos?"))
    if (respuesta != 1L) stop("Ejecución detenida: no se sobrescribió nada.")
  } else {
    stop(mensaje, "\nNo se sobrescribió nada. Para continuar, bórrelos, cambie ",
         "'dir_salida' o fije permitir_sobrescribir <- TRUE.")
  }
}

# ==============================================================================
# 3. ATRIBUTOS, CATÁLOGO DE 288 PERFILES Y TEXTOS
# ==============================================================================
niveles_oferta <- c(2, 2, 2, 3, 2, 3, 2)
nombres_oferta <- c("Modalidad", "Flexibilidad", "Carga", "Liderazgo",
                    "Ben_desempeno", "Ben_parentales", "Cuidado")
names(niveles_oferta) <- nombres_oferta

# Orden fijo y documentado del factorial completo: expand.grid hace variar más
# rápido el primer atributo (Modalidad) y más lento el último (Cuidado).
# Profile_ID = número de fila en ese orden. Es el mismo orden que
# AlgDesign::gen.factorial(factors = "all") de los scripts anteriores, por lo que
# los Profile_ID coinciden con los del piloto de pares del 2026-10-28.
grilla <- expand.grid(
  lapply(niveles_oferta, function(L) factor(seq_len(L), levels = seq_len(L))),
  KEEP.OUT.ATTRS = FALSE
)

if (requireNamespace("AlgDesign", quietly = TRUE)) {
  grilla_algdesign <- AlgDesign::gen.factorial(
    levels = unname(niveles_oferta), varNames = nombres_oferta, factors = "all"
  )
  stopifnot(all(vapply(nombres_oferta, function(a) {
    identical(as.character(grilla[[a]]), as.character(grilla_algdesign[[a]]))
  }, logical(1))))
}

# Textos exactos (copiados de procesar_textos_oferta() del script de pares).
textos_niveles <- list(
  Modalidad = c(
    "Híbrido, algunos días remoto y otros presencial.",
    "Todos los días presencial."
  ),
  Flexibilidad = c(
    "Fijo (9 a 18:00 hrs.).",
    "Flexible (puedes modificar el horario según tu preferencia)."
  ),
  Carga = c(
    "Nunca deberás trabajar horas extra.",
    "Frecuentemente deberás trabajar horas extra."
  ),
  Liderazgo = c(
    "Tu jefe se enfoca en potenciar las fortalezas de tu equipo.",
    "Tu jefe se enfoca en los resultados de sus trabajadores.",
    "Tu jefe se enfoca en la organización de los procesos en el equipo."
  ),
  Ben_desempeno = c(
    "No hay bonos por desempeño.",
    "Hay bonos anuales por desempeño."
  ),
  Ben_parentales = c(
    "5 meses y medio para las madres y 5 días para los padres.",
    "12 meses para las madres y 1 mes para los padres.",
    "6 meses para las madres y 6 meses para los padres."
  ),
  Cuidado = c(
    "Sala cuna para menores de 2 años, solo para las madres.",
    "Sala cuna para menores de 2 años, para madres y padres."
  )
)

# Mismos nombres de columnas de texto que en el script del piloto.
cols_texto <- c(
  Modalidad      = "Texto_Modalidad",
  Flexibilidad   = "Texto_Flexibilidad",
  Carga          = "Texto_Carga",
  Liderazgo      = "Texto_Liderazgo",
  Ben_desempeno  = "Texto_Desempeno",
  Ben_parentales = "Texto_Parentales",
  Cuidado        = "Texto_Cuidado"
)

stopifnot(
  identical(names(textos_niveles), nombres_oferta),
  all(lengths(textos_niveles) == niveles_oferta)
)

catalogo_perfiles <- as_tibble(grilla) %>%
  mutate(Profile_ID = row_number(), .before = 1)
for (a in nombres_oferta) {
  catalogo_perfiles[[cols_texto[[a]]]] <-
    textos_niveles[[a]][as.integer(catalogo_perfiles[[a]])]
}

n_perfiles_ff <- nrow(catalogo_perfiles)
stopifnot(
  n_perfiles_ff == prod(niveles_oferta),
  n_perfiles_ff == 288L,
  !anyNA(catalogo_perfiles),
  !anyDuplicated(catalogo_perfiles[nombres_oferta])
)

# Códigos numéricos de los niveles (288 x 7), útiles para comparar perfiles.
codigos_mat <- vapply(nombres_oferta, function(a) {
  as.integer(catalogo_perfiles[[a]])
}, integer(n_perfiles_ff))

# ==============================================================================
# 4. MATRICES CON CODIFICACIÓN DE EFECTOS (CATÁLOGO)
# ==============================================================================
# Se calculan una sola vez sobre los 288 perfiles; luego basta indexarlas por
# Profile_ID. Sin intercepto, con contr.sum (codificación de efectos).
frm_principales <- as.formula(paste("~", paste(nombres_oferta, collapse = " + ")))
frm_M1 <- ~ Modalidad * Flexibilidad + Carga + Liderazgo +
  Ben_desempeno + Ben_parentales + Cuidado
frm_M2 <- as.formula(paste("~ (", paste(nombres_oferta, collapse = " + "), ")^2"))

matriz_sin_intercepto <- function(frm, datos) {
  mm <- model.matrix(frm, datos)
  asignacion <- attr(mm, "assign")
  X <- mm[, asignacion != 0L, drop = FALSE]
  list(
    X = X,
    termino = attr(terms(frm), "term.labels")[asignacion[asignacion != 0L]]
  )
}

ef_principales <- matriz_sin_intercepto(frm_principales, catalogo_perfiles)
X_cat_ef       <- ef_principales$X
atributo_col   <- ef_principales$termino   # atributo al que pertenece cada columna
X_cat_M1       <- matriz_sin_intercepto(frm_M1, catalogo_perfiles)$X
X_cat_M2       <- matriz_sin_intercepto(frm_M2, catalogo_perfiles)$X

stopifnot(ncol(X_cat_ef) == 9L, ncol(X_cat_M1) == 10L, ncol(X_cat_M2) == 43L)

# ==============================================================================
# 5. ALEATORIZACIÓN POR ESTRATO
# ==============================================================================
repartir_estrato <- function(n, m_por_persona, n_perfiles, max_iter, estrato) {
  m <- n * m_por_persona

  # Paso 1: pozo balanceado (frecuencias dentro de +-1) y barajado.
  pozo <- c(
    rep(seq_len(n_perfiles), m %/% n_perfiles),
    sample.int(n_perfiles, m %% n_perfiles)
  )
  pozo <- pozo[sample.int(length(pozo))]

  # Paso 2: mazos de 6 IDs consecutivos, uno por persona.
  mazos <- matrix(pozo, nrow = n, ncol = m_por_persona, byrow = TRUE)

  # Paso 3: reparación de mazos con perfiles repetidos.
  tiene_duplicado <- function(fila) anyDuplicated(fila) > 0L
  mazos_con_dup <- which(apply(mazos, 1, tiene_duplicado))
  mazos_con_dup_inicial <- length(mazos_con_dup)
  iteraciones <- 0L
  intercambios <- 0L

  while (length(mazos_con_dup) > 0L) {
    iteraciones <- iteraciones + 1L
    if (iteraciones > max_iter) {
      stop("Estrato '", estrato, "': no se pudieron reparar los mazos con ",
           "perfiles repetidos en ", max_iter, " iteraciones. Quedan ",
           length(mazos_con_dup), " mazos con duplicados. Aumente ",
           "'max_iter_reparacion' o revise el tamaño del estrato.")
    }
    d <- mazos_con_dup[sample.int(length(mazos_con_dup), 1L)]
    i <- anyDuplicated(mazos[d, ])          # posición del elemento repetido
    otros <- setdiff(seq_len(n), d)
    e <- otros[sample.int(length(otros), 1L)]
    j <- sample.int(m_por_persona, 1L)
    v <- mazos[d, i]
    w <- mazos[e, j]

    # Solo se acepta el intercambio si no crea duplicados en ninguno de los mazos.
    if (v != w && !(v %in% mazos[e, -j]) && !(w %in% mazos[d, -i])) {
      mazos[d, i] <- w
      mazos[e, j] <- v
      intercambios <- intercambios + 1L
      mazos_con_dup <- which(apply(mazos, 1, tiene_duplicado))
    }
  }

  list(
    mazos = mazos,
    registro = tibble(
      Estrato = estrato,
      Mazos_con_duplicados_inicial = mazos_con_dup_inicial,
      Intercambios_reparacion = intercambios,
      Iteraciones_reparacion = iteraciones
    )
  )
}

set.seed(semilla)

asignaciones <- vector("list", nrow(estratos))
registro_reparacion <- vector("list", nrow(estratos))

for (s in seq_len(nrow(estratos))) {
  est <- estratos[s, ]
  reparto <- repartir_estrato(
    n = est$n,
    m_por_persona = perfiles_por_persona,
    n_perfiles = n_perfiles_ff,
    max_iter = max_iter_reparacion,
    estrato = est$Estrato
  )
  ancho_id <- max(3L, nchar(est$n))
  ids <- paste0(est$Prefijo_ID, "_",
                formatC(seq_len(est$n), width = ancho_id, flag = "0"))

  asignaciones[[s]] <- tibble(
    Respondent_ID = rep(ids, each = perfiles_por_persona),
    Estrato = est$Estrato,
    Orden_Calificacion = rep(seq_len(perfiles_por_persona), times = est$n),
    Profile_ID = as.vector(t(reparto$mazos))   # fila por fila: persona 1, 2, ...
  )
  registro_reparacion[[s]] <- reparto$registro
}

registro_reparacion <- bind_rows(registro_reparacion)

# ==============================================================================
# 6. BASE LARGA (una fila por persona x perfil)
# ==============================================================================
codigos_propios <- catalogo_perfiles %>%
  select(Profile_ID, all_of(nombres_oferta))

codigos_pareja <- codigos_propios %>%
  rename_with(~ paste0("Pareja_", .x), all_of(nombres_oferta)) %>%
  rename(Partner_Profile_ID = Profile_ID)

textos_perfil <- catalogo_perfiles %>%
  select(Profile_ID, all_of(unname(cols_texto)))

df_long <- bind_rows(asignaciones) %>%
  mutate(
    Modo = modo,
    Pair_Order = (Orden_Calificacion + 1L) %/% 2L,
    Alternative = if_else(Orden_Calificacion %% 2L == 1L, "A", "B"),
    Choice_Set_ID = paste0(Respondent_ID, "_P", Pair_Order)
  ) %>%
  group_by(Choice_Set_ID) %>%
  # Cada choice set tiene exactamente dos filas: la pareja es "la otra".
  mutate(Partner_Profile_ID = sum(Profile_ID) - Profile_ID) %>%
  group_by(Respondent_ID) %>%
  mutate(Prev_Profile_ID = lag(Profile_ID, order_by = Orden_Calificacion)) %>%
  ungroup() %>%
  mutate(
    N_atributos_diferentes = rowSums(
      codigos_mat[Profile_ID, , drop = FALSE] !=
        codigos_mat[Partner_Profile_ID, , drop = FALSE]
    )
  ) %>%
  left_join(codigos_propios, by = "Profile_ID") %>%
  left_join(codigos_pareja, by = "Partner_Profile_ID") %>%
  left_join(textos_perfil, by = "Profile_ID") %>%
  mutate(
    Chosen     = NA_integer_,  # 1 = elegida en el par, 0 = no elegida
    Likelihood = NA_real_,     # probabilidad de intención de un hijo
    Timing     = NA_integer_   # momento (ordinal)
  ) %>%
  select(
    Respondent_ID, Estrato, Modo, Pair_Order, Alternative, Orden_Calificacion,
    Choice_Set_ID, Profile_ID, Partner_Profile_ID, Prev_Profile_ID,
    all_of(nombres_oferta), all_of(paste0("Pareja_", nombres_oferta)),
    N_atributos_diferentes, all_of(unname(cols_texto)),
    Chosen, Likelihood, Timing
  ) %>%
  arrange(match(Estrato, estratos$Estrato), Respondent_ID, Orden_Calificacion)

# ==============================================================================
# 7. BASE ANCHA PARA PROGRAMACIÓN EN NETQUEST (una fila por persona)
# ==============================================================================
df_wide <- df_long %>%
  select(Respondent_ID, Estrato, Pair_Order, Alternative, Profile_ID,
         all_of(unname(cols_texto))) %>%
  pivot_wider(
    id_cols = c(Respondent_ID, Estrato),
    names_from = c(Pair_Order, Alternative),
    values_from = c(Profile_ID, all_of(unname(cols_texto))),
    names_glue = "P{Pair_Order}_{Alternative}_{.value}",
    names_vary = "slowest"
  ) %>%
  arrange(match(Estrato, estratos$Estrato), Respondent_ID)

# ==============================================================================
# 8. AUDITORÍA ESTRUCTURAL (si falla, el script se detiene)
# ==============================================================================
# (a) Personas por estrato, pares, filas y perfiles por persona, pares válidos.
chk_estratos <- df_long %>%
  distinct(Estrato, Respondent_ID) %>%
  count(Estrato, name = "N_personas")

chk_personas <- df_long %>%
  group_by(Respondent_ID) %>%
  summarise(
    filas = n(),
    pares = n_distinct(Choice_Set_ID),
    perfiles_unicos = n_distinct(Profile_ID),
    .groups = "drop"
  )

chk_pares <- df_long %>%
  group_by(Choice_Set_ID) %>%
  summarise(
    n_A = sum(Alternative == "A"),
    n_B = sum(Alternative == "B"),
    perfiles_distintos = n_distinct(Profile_ID),
    .groups = "drop"
  )

# (b) Frecuencia de perfiles dentro de cada estrato (incluye perfiles con 0).
chk_frecuencias <- df_long %>%
  group_by(Estrato) %>%
  summarise(
    Frec_min = min(tabulate(Profile_ID, nbins = n_perfiles_ff)),
    Frec_max = max(tabulate(Profile_ID, nbins = n_perfiles_ff)),
    .groups = "drop"
  )

stopifnot(
  # (a)
  all(chk_estratos$N_personas[match(estratos$Estrato, chk_estratos$Estrato)] ==
        estratos$n),
  nrow(df_long) == n_total * perfiles_por_persona,
  all(chk_personas$filas == perfiles_por_persona),
  all(chk_personas$pares == pares_por_persona),
  all(chk_personas$perfiles_unicos == perfiles_por_persona),
  all(chk_pares$n_A == 1L),
  all(chk_pares$n_B == 1L),
  all(chk_pares$perfiles_distintos == 2L),
  all(df_long$N_atributos_diferentes >= 1L),
  all(is.na(df_long$Prev_Profile_ID) == (df_long$Orden_Calificacion == 1L)),
  !anyNA(df_long[, c(nombres_oferta, paste0("Pareja_", nombres_oferta),
                     unname(cols_texto))]),
  nrow(df_wide) == n_total,
  # (b)
  all(chk_frecuencias$Frec_max - chk_frecuencias$Frec_min <= 1L)
)

cat("\n==============================================================\n")
cat(" DISEÑO ALEATORIO OFERTA — modo:", modo, "| N =", n_total,
    "| semilla =", semilla, "\n")
cat("==============================================================\n")
cat("\n[ESTRUCTURA] Todos los controles estructurales (a) y (b) fueron superados.\n")
print(as.data.frame(
  estratos %>%
    left_join(chk_frecuencias, by = "Estrato") %>%
    left_join(registro_reparacion, by = "Estrato")
), row.names = FALSE)

# ==============================================================================
# 9. DIAGNÓSTICOS (se informan y se marcan alertas, pero NO detienen el script)
# ==============================================================================
# Regla de alerta para (d)-(f): |valor| > 3 / sqrt(número de filas usadas).

# Distribución esperada del número de atributos distintos si los dos perfiles
# del par fueran independientes y uniformes sobre los 288 perfiles.
prob_dif <- 1
for (L in niveles_oferta) {
  q <- 1 - 1 / L
  prob_dif <- c(prob_dif * (1 - q), 0) + c(0, prob_dif * q)
}
names(prob_dif) <- 0:length(niveles_oferta)
prob_dif_cond <- prob_dif
prob_dif_cond[1] <- 0
prob_dif_cond <- prob_dif_cond / sum(prob_dif_cond)  # condicional a perfiles distintos
media_dif_esperada <- sum(as.integer(names(prob_dif_cond)) * prob_dif_cond)

d_eficiencia <- function(X, X_ff) {
  k <- ncol(X)
  rango <- qr(X)$rank
  if (rango < k) return(list(D_eff = 0, rango = rango, k = k))
  ld    <- as.numeric(determinant(crossprod(X) / nrow(X), logarithm = TRUE)$modulus)
  ld_ff <- as.numeric(determinant(crossprod(X_ff) / nrow(X_ff), logarithm = TRUE)$modulus)
  list(D_eff = 100 * exp((ld - ld_ff) / k), rango = rango, k = k)
}

cor_columna_a_columna <- function(X1, X2) {
  vapply(seq_len(ncol(X1)), function(k) {
    suppressWarnings(cor(X1[, k], X2[, k]))
  }, numeric(1))
}

auditar_ambito <- function(d, ambito) {
  pares_A <- d %>% filter(Alternative == "A")
  con_prev <- d %>% filter(!is.na(Prev_Profile_ID))
  prev_entre_pares <- con_prev %>% filter(Alternative == "A")  # órdenes 3 y 5

  # (c) Balance de niveles por atributo.
  balance <- d %>%
    select(all_of(nombres_oferta)) %>%
    mutate(across(everything(), as.character)) %>%
    pivot_longer(everything(), names_to = "Atributo", values_to = "Nivel") %>%
    count(Atributo, Nivel, name = "N") %>%
    group_by(Atributo) %>%
    mutate(
      Proporcion = N / sum(N),
      Proporcion_esperada = 1 / niveles_oferta[Atributo]
    ) %>%
    ungroup() %>%
    mutate(
      Ambito = ambito,
      Texto_nivel = mapply(function(a, l) textos_niveles[[a]][as.integer(l)],
                           Atributo, Nivel, USE.NAMES = FALSE)
    ) %>%
    arrange(match(Atributo, nombres_oferta), Nivel) %>%
    select(Ambito, Atributo, Nivel, Texto_nivel, N, Proporcion, Proporcion_esperada)

  # (d) Correlaciones entre columnas de atributos DISTINTOS (nivel perfil).
  X <- X_cat_ef[d$Profile_ID, , drop = FALSE]
  R <- suppressWarnings(cor(X))
  idx <- which(upper.tri(R), arr.ind = TRUE)
  idx <- idx[atributo_col[idx[, 1]] != atributo_col[idx[, 2]], , drop = FALSE]
  tabla_d <- tibble(
    Diagnostico = "d_entre_atributos",
    Columna_1 = colnames(X)[idx[, 1]],
    Columna_2 = colnames(X)[idx[, 2]],
    Valor = R[idx],
    N_filas = nrow(X)
  )

  # (e) Independencia propio-pareja y propio-perfil previo (misma columna).
  XA <- X_cat_ef[pares_A$Profile_ID, , drop = FALSE]
  XB <- X_cat_ef[pares_A$Partner_Profile_ID, , drop = FALSE]
  tabla_e_pareja <- tibble(
    Diagnostico = "e_pareja",
    Columna_1 = colnames(X), Columna_2 = paste0("Pareja_", colnames(X)),
    Valor = cor_columna_a_columna(XA, XB),
    N_filas = nrow(XA)
  )
  tabla_e_previo <- tibble(
    Diagnostico = "e_previo_todas",
    Columna_1 = colnames(X), Columna_2 = paste0("Previo_", colnames(X)),
    Valor = cor_columna_a_columna(
      X_cat_ef[con_prev$Profile_ID, , drop = FALSE],
      X_cat_ef[con_prev$Prev_Profile_ID, , drop = FALSE]
    ),
    N_filas = nrow(con_prev)
  )
  tabla_e_previo_entre <- tibble(
    Diagnostico = "e_previo_entre_pares",
    Columna_1 = colnames(X), Columna_2 = paste0("Previo_", colnames(X)),
    Valor = cor_columna_a_columna(
      X_cat_ef[prev_entre_pares$Profile_ID, , drop = FALSE],
      X_cat_ef[prev_entre_pares$Prev_Profile_ID, , drop = FALSE]
    ),
    N_filas = nrow(prev_entre_pares)
  )

  # (f) Balance de posición A/B: medias de (x_A - x_B).
  dif_AB <- XA - XB
  tabla_f <- tibble(
    Diagnostico = "f_media_xA_menos_xB",
    Columna_1 = colnames(X), Columna_2 = NA_character_,
    Valor = colMeans(dif_AB),
    N_filas = nrow(dif_AB)
  )

  correlaciones <- bind_rows(tabla_d, tabla_e_pareja, tabla_e_previo,
                             tabla_e_previo_entre, tabla_f) %>%
    mutate(
      Ambito = ambito,
      Umbral = 3 / sqrt(N_filas),
      Alerta = !is.na(Valor) & abs(Valor) > Umbral
    ) %>%
    select(Ambito, Diagnostico, Columna_1, Columna_2, Valor, N_filas, Umbral, Alerta)

  # (g) Número de atributos distintos por par y empates por atributo.
  n_dif <- pares_A$N_atributos_diferentes
  obs_dif <- table(factor(n_dif, levels = 0:length(niveles_oferta)))
  tabla_g_dist <- tibble(
    Ambito = ambito,
    Tabla = "Distribucion_N_atributos_diferentes",
    Categoria = names(obs_dif),
    N_pares = as.integer(obs_dif),
    Prop_observada = as.numeric(obs_dif) / length(n_dif),
    Prop_esperada = as.numeric(prob_dif),
    Prop_esperada_condicional = as.numeric(prob_dif_cond)
  )
  empates <- codigos_mat[pares_A$Profile_ID, , drop = FALSE] ==
    codigos_mat[pares_A$Partner_Profile_ID, , drop = FALSE]
  tabla_g_empates <- tibble(
    Ambito = ambito,
    Tabla = "Empates_por_atributo",
    Categoria = nombres_oferta,
    N_pares = colSums(empates),
    Prop_observada = colMeans(empates),
    Prop_esperada = 1 / niveles_oferta,
    Prop_esperada_condicional =
      (1 / niveles_oferta - 1 / n_perfiles_ff) / (1 - 1 / n_perfiles_ff)
  )
  diferencias <- bind_rows(tabla_g_dist, tabla_g_empates)

  # (h) D-eficiencia a nivel de perfil, relativa al factorial completo.
  h_M1 <- d_eficiencia(X_cat_M1[d$Profile_ID, , drop = FALSE], X_cat_M1)
  h_M2 <- d_eficiencia(X_cat_M2[d$Profile_ID, , drop = FALSE], X_cat_M2)

  frecuencias <- tabulate(d$Profile_ID, nbins = n_perfiles_ff)
  max_abs <- function(diag) {
    v <- correlaciones$Valor[correlaciones$Diagnostico == diag]
    max(abs(v), na.rm = TRUE)
  }

  metricas <- tibble(
    Ambito = ambito,
    Modo = modo,
    Semilla = semilla,
    N_personas = n_distinct(d$Respondent_ID),
    N_perfiles_mostrados = nrow(d),
    N_pares = nrow(pares_A),
    Perfiles_distintos_usados = sum(frecuencias > 0),
    Frec_perfil_min = min(frecuencias),
    Frec_perfil_max = max(frecuencias),
    D_eff_M1 = h_M1$D_eff, Rango_M1 = h_M1$rango, k_M1 = h_M1$k,
    D_eff_M2 = h_M2$D_eff, Rango_M2 = h_M2$rango, k_M2 = h_M2$k,
    Max_abs_r_entre_atributos = max_abs("d_entre_atributos"),
    Max_abs_r_pareja = max_abs("e_pareja"),
    Max_abs_r_previo_todas = max_abs("e_previo_todas"),
    Max_abs_r_previo_entre_pares = max_abs("e_previo_entre_pares"),
    Max_abs_media_xA_menos_xB = max_abs("f_media_xA_menos_xB"),
    Media_N_atributos_diferentes = mean(n_dif),
    Media_N_atributos_diferentes_esperada = media_dif_esperada,
    N_alertas_d_a_f = sum(correlaciones$Alerta)
  )

  list(balance = balance, correlaciones = correlaciones,
       diferencias = diferencias, metricas = metricas)
}

# Ámbitos: cada estrato y, si hay más de un estrato, el conjunto global.
ambitos <- split(df_long, factor(df_long$Estrato, levels = estratos$Estrato))
if (length(ambitos) > 1L) ambitos[["Global"]] <- df_long

auditorias <- lapply(names(ambitos), function(a) auditar_ambito(ambitos[[a]], a))

auditoria_balance       <- bind_rows(lapply(auditorias, `[[`, "balance"))
auditoria_correlaciones <- bind_rows(lapply(auditorias, `[[`, "correlaciones"))
auditoria_diferencias   <- bind_rows(lapply(auditorias, `[[`, "diferencias"))
metricas <- bind_rows(lapply(auditorias, `[[`, "metricas")) %>%
  left_join(registro_reparacion, by = c("Ambito" = "Estrato"))

# Alertas: (d)-(f) por umbral y (h) si alguna matriz no tiene rango completo.
alertas <- bind_rows(
  auditoria_correlaciones %>%
    filter(Alerta) %>%
    transmute(Ambito, Diagnostico,
              Detalle = if_else(is.na(Columna_2), Columna_1,
                                paste(Columna_1, "vs", Columna_2)),
              Valor, Umbral),
  metricas %>%
    filter(Rango_M1 < k_M1) %>%
    transmute(Ambito, Diagnostico = "h_rango_M1",
              Detalle = paste("Rango", Rango_M1, "de", k_M1),
              Valor = Rango_M1, Umbral = k_M1),
  metricas %>%
    filter(Rango_M2 < k_M2) %>%
    transmute(Ambito, Diagnostico = "h_rango_M2",
              Detalle = paste("Rango", Rango_M2, "de", k_M2),
              Valor = Rango_M2, Umbral = k_M2)
)

# ------------------------------------------------------------------------------
# Reporte legible
# ------------------------------------------------------------------------------
fmt <- function(x, d = 3) formatC(x, format = "f", digits = d)

cat("\n[(c) BALANCE DE NIVELES] Proporción observada (esperada)\n")
tabla_balance_print <- auditoria_balance %>%
  mutate(Valor = paste0(fmt(Proporcion), " (", fmt(Proporcion_esperada), ")")) %>%
  select(Ambito, Atributo, Nivel, Valor) %>%
  pivot_wider(names_from = Ambito, values_from = Valor)
print(as.data.frame(tabla_balance_print), row.names = FALSE)

cat("\n[(d)-(f) CORRELACIONES Y BALANCE A/B] máximo |valor| (umbral 3/sqrt(n))\n")
resumen_df <- auditoria_correlaciones %>%
  group_by(Ambito, Diagnostico) %>%
  summarise(
    Max_abs = max(abs(Valor), na.rm = TRUE),
    Umbral = first(Umbral),
    N_filas = first(N_filas),
    Alertas = sum(Alerta),
    .groups = "drop"
  ) %>%
  arrange(match(Ambito, names(ambitos)), Diagnostico) %>%
  mutate(Max_abs = fmt(Max_abs), Umbral = fmt(Umbral))
print(as.data.frame(resumen_df), row.names = FALSE)

cat("\n[(g) N° DE ATRIBUTOS DISTINTOS POR PAR] proporción observada",
    "(esperada condicional a perfiles distintos)\n")
tabla_g_print <- auditoria_diferencias %>%
  filter(Tabla == "Distribucion_N_atributos_diferentes") %>%
  mutate(Valor = paste0(fmt(Prop_observada), " (",
                        fmt(Prop_esperada_condicional), ")")) %>%
  select(Ambito, Categoria, Valor) %>%
  pivot_wider(names_from = Ambito, values_from = Valor)
print(as.data.frame(tabla_g_print), row.names = FALSE)

cat("\n[(g) EMPATES POR ATRIBUTO] proporción observada (esperada 1/L)\n")
tabla_emp_print <- auditoria_diferencias %>%
  filter(Tabla == "Empates_por_atributo") %>%
  mutate(Valor = paste0(fmt(Prop_observada), " (", fmt(Prop_esperada), ")")) %>%
  select(Ambito, Categoria, Valor) %>%
  pivot_wider(names_from = Ambito, values_from = Valor)
print(as.data.frame(tabla_emp_print), row.names = FALSE)

cat("\n[(h) D-EFICIENCIA RELATIVA AL FACTORIAL COMPLETO Y RANGO]\n")
print(as.data.frame(
  metricas %>%
    transmute(Ambito,
              D_eff_M1 = fmt(D_eff_M1, 2), Rango_M1 = paste0(Rango_M1, "/", k_M1),
              D_eff_M2 = fmt(D_eff_M2, 2), Rango_M2 = paste0(Rango_M2, "/", k_M2))
), row.names = FALSE)

cat("\n[RESUMEN]\n")
print(as.data.frame(
  metricas %>%
    transmute(Ambito, N_personas,
              Frec_perfil = paste0(Frec_perfil_min, "-", Frec_perfil_max),
              Max_r_pareja = fmt(Max_abs_r_pareja),
              Max_r_previo = fmt(Max_abs_r_previo_todas),
              Max_dif_AB = fmt(Max_abs_media_xA_menos_xB),
              Media_N_dif = fmt(Media_N_atributos_diferentes, 2),
              Esperada = fmt(Media_N_atributos_diferentes_esperada, 2))
), row.names = FALSE)

if (nrow(alertas) == 0L) {
  cat("\n[ALERTAS] Ninguna: ningún diagnóstico supera su umbral.\n")
} else {
  cat("\n[ALERTAS] Los siguientes diagnósticos superan su umbral.",
      "NO se re-aleatorizó; informe y revise:\n")
  print(as.data.frame(alertas %>% mutate(Valor = fmt(Valor), Umbral = fmt(Umbral))),
        row.names = FALSE)
}

# ==============================================================================
# 10. EXPORTACIÓN
# ==============================================================================
if (!dir.exists(dir_salida)) dir.create(dir_salida, recursive = TRUE)

escribir_csv <- function(datos, ruta) {
  write.csv(datos, ruta, row.names = FALSE, fileEncoding = "UTF-8", na = "")
}

escribir_csv(df_long,                 archivos_salida[["long"]])
escribir_csv(df_wide,                 archivos_salida[["wide"]])
escribir_csv(catalogo_perfiles,       archivos_salida[["catalogo"]])
escribir_csv(auditoria_balance,       archivos_salida[["balance"]])
escribir_csv(auditoria_correlaciones, archivos_salida[["correlacion"]])
escribir_csv(metricas,                archivos_salida[["metricas"]])

write.xlsx(
  list(
    Programacion_Wide  = df_wide,
    Analisis_Long      = df_long,
    Catalogo_Perfiles  = catalogo_perfiles,
    Balance_Niveles    = auditoria_balance,
    Correlaciones      = auditoria_correlaciones,
    Diferencias_Pares  = auditoria_diferencias,
    Metricas           = metricas
  ),
  archivos_salida[["xlsx"]],
  overwrite = TRUE
)

# Registro de reproducibilidad: semilla, parámetros, huellas MD5 y sessionInfo().
huellas <- tools::md5sum(archivos_salida[c("long", "wide", "catalogo")])
writeLines(c(
  "REPRODUCIBILIDAD — 20261116_diseno_oferta_aleatorio.R",
  paste("Fecha de ejecución:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  paste("Modo:", modo),
  paste("Semilla:", semilla),
  paste("RNGkind:", paste(RNGkind(), collapse = " / ")),
  paste("Perfiles por persona:", perfiles_por_persona,
        "| Pares por persona:", pares_por_persona),
  "Estratos (en el orden en que se aleatorizan; cambiar el orden cambia el diseño):",
  paste0("  ", estratos$Estrato, " | prefijo ", estratos$Prefijo_ID,
         " | n = ", estratos$n),
  "MD5 de las bases generadas:",
  paste0("  ", basename(names(huellas)), "  ", unname(huellas)),
  "",
  "sessionInfo():",
  capture.output(sessionInfo())
), archivos_salida[["reproducible"]], useBytes = TRUE)

cat("\n[EXPORTACIÓN COMPLETA] Archivos en", normalizePath(dir_salida), ":\n")
cat(paste0("  ", basename(archivos_salida), collapse = "\n"), "\n")
