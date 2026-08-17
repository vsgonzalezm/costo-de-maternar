# =========================================================================
# DISEÑO FACTORIAL
# =========================================================================

# 1. CONFIGURACIÓN DEL DISEÑO (Nivel 1: Viñetas)
vignette_universe <- list(
  Genero = c("Mujer", "Hombre"),                           # 2 niveles
  Estado_Familiar = c("Soltero/a sin hijos", 
                      "Conviviendo sin hijos (quiere mp)", 
                      "Casado/a sin hijos (quiere mp)", 
                      "Conviviendo 1 hijo (quiere cp)", 
                      "Casado/a 1 hijo (quiere cp)", 
                      "Conviviendo 2 hijos (no quiere más)", 
                      "Casado/a 2 hijos (no quiere más)"), # 7 niveles
  Experiencia = c("6-7 años", "8-10 años"),               # 2 niveles
  Expectativa_Sueldo = c("+10%", "+15%", "-10%", "-15%")   # 4 niveles
)

total_combinations <- prod(sapply(vignette_universe, length))
total_combinations
# Universo total: 2 * 7 * 2 * 4 = 112 combinaciones


# 2. CONFIGURACIÓN DE LA MUESTRA (Nivel 2: Encuestados)
nd <- 2                    # Viñetas (o perfiles) por respondente
ns_vinetas_unicas <- 112   # El universo completo
n_mazos <- ns_vinetas_unicas / nd # División de la muestra para cubrir todo el universo de 112 combinaciones
n_respondentes <- 1000 

cat("RESUMEN DEL DISEÑO")
cat("Universo total de combinaciones:", total_combinations, "\n")
cat("Viñetas únicas a utilizar (ns):", ns_vinetas_unicas, "\n")
cat("Número de masos:", n_mazos, "\n")
cat("Número de encuestados que verán cada mazo):", round(n_respondentes / (ns_vinetas_unicas/nd), 0), "\n")



# =========================================================================
# PODER ESTADÍSTICO
# =========================================================================

# 1. PARÁMETROS DE EFECTO (Basado en la fórmula del libro)
rho <- 0.2          # Correlación intraclase (ajustar según pretest).
                    # El grado en que las respuestas de un mismo individuo son similares (asumiendo que el 20% de la varianza se debe a características fijas del encuestado).
p_esperada <- 0.10  # Proporción (p) de "éxito" o de respuestas en una categoría específica que se espera encontrar.
error_t <- 0.04     # Margen de error (t) 
alpha <- 0.05             


# 2. CÁLCULO DE PODER ESTADÍSTICO (1 - Beta)
deff <- 1 + (nd - 1) * rho # Efecto de diseño: mide la pérdida de eficiencia estadística (independencia de las evaluaciones).
n_total_evaluaciones <- n_respondentes * nd
n_efectivo <- n_total_evaluaciones / deff # Muestra efectiva: corrige el hecho de que las respuestas de una misma persona no son totalmente independientes.
                                          
z_alpha <- qnorm(1 - alpha/2)
termino_varianza <- (p_esperada * (1 - p_esperada)) / error_t^2

z_beta <- sqrt(n_efectivo / termino_varianza) - z_alpha
poder_final <- pnorm(z_beta)


cat("RESULTADOS DE PODER")
cat("Efecto de diseño (deff):", round(deff, 2), "\n")
# deff = 1.0: Es el escenario ideal (muestreo aleatorio simple). Cada evaluación es 100% independiente.
# deff entre 1.5 y 3.0: Es el rango estándar y aceptable. 
# deff > 5.0: Diseño ineficiente.
cat("Muestra efectiva:", round(n_efectivo, 0), "\n")
# Significa que el experimento de 1000 personas viendo 2 candidatos cada una,
# tiene la misma precisión que un experimento donde encuestaras a 1667 personas y cada una viera a solo 1 candidato.
cat("Poder estadístico:", poder_final, "\n")
cat("Poder para detectar error del", error_t*100, "%:", round(poder_final * 100, 2), "%\n")