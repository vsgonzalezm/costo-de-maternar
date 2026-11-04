# ==============================================================================
# SCRIPT DE EVALUACIÓN DE DISEÑO EXPERIMENTAL - FONDECYT
# Objetivo: Validar solvencia estadística
# ==============================================================================

if(!require(AlgDesign)) install.packages("AlgDesign"); library(AlgDesign)

# ==============================================================================
# ESCENARIO 1: LÍNEA BASE 
# Estructura: 2(Genero) x 7(Familia) x 2(Exp) x 4(Sueldo)
# Interacciones: Genero:Familia, Genero:Sueldo, Familia:Sueldo
# ==============================================================================

# 1. Definir Universo de Candidatos (112 perfiles posibles)
# ------------------------------------------------------------------------------
# F1=Genero(2), F2=Familia(7), F3=Exp(2), F4=Sueldo(4)
cand_base <- gen.factorial(
  levels   = c(2, 7, 2, 4),
  varNames = c("F1", "F2", "F3", "F4"),
  factors  = "all" # Importante: asegura que R trate todo como categórico
)

# 2. Definir Modelo y Calcular Grados de Libertad 
# ------------------------------------------------------------------------------
# Incluimos las interacciones.
# OJO: Aquí calculamos el "p" real (número de parámetros).
frm_base <- ~ F1 + F2 + F3 + F4 + F1:F2 + F1:F4 + F2:F4

# Creamos la matriz teórica solo para contar columnas (parámetros)
X_base <- model.matrix(frm_base, cand_base)
p_base <- ncol(X_base) 

# El cálculo de p=39 sale de:
# Intercepto(1) + F1(1) + F2(6) + F3(1) + F4(3) + 
# F1:F2(1*6=6) + F1:F4(1*3=3) + F2:F4(6*3=18) = 39 parámetros.
print(paste("ESCENARIO 1 - Parámetros a estimar (p):", p_base))
print(paste("Ratio de Eficiencia (60/39):", round(60/p_base, 2))) 
# Si el ratio es > 1.5, el diseño es sólido. 1.54 es ACEPTABLE (Justo, pero pasa).

# 3. Generar Diseño Óptimo (60 viñetas)
# ------------------------------------------------------------------------------
set.seed(123)
opt_base <- optFederov(
  frml      = frm_base,
  data      = cand_base,
  nTrials   = 60,    # N definido por presupuesto y regla n > p
  criterion = "D",   # Maximizar información
  nRepeats  = 1000   # Alta repetición para asegurar el mejor óptimo global
)

# 4. Diagnóstico de Solidez 
# ------------------------------------------------------------------------------
des_base <- opt_base$design
eval_base <- eval.design(frm_base, des_base, X = cand_base)

print(">>> DIAGNÓSTICO ESCENARIO 1 (BASE) <<<")
print(paste("G-Efficiency:", round(eval_base$Ge, 3))) 
# G-Efficiency > 0.80 indica un diseño muy estable. Si da ~0.90 es excelente.
# Rank debe ser 39. Si es menor, el modelo está roto.
print(paste("Rank (debe ser 39):", eval_base$rank))

# Revisión de Balance (Cómo afecta la variable de 7 niveles)
# Muestra cuántas veces aparece cada combinación Genero x Familia.
# Deben ser números parecidos (ej. 4 y 5). No debe haber ceros.
print("Tabla Cruzada Genero x Familia:")
print(table(des_base$F1, des_base$F2))


# ==============================================================================
# ESCENARIO 2: 5TA DIMENSIÓN
# Qué pasa si añadimos Educación (2 niveles)
# Estructura: 2 x 7 x 2 x 4 x 2
# ==============================================================================

# 1. Nuevo Universo (224 perfiles)
cand_edu <- gen.factorial(
  levels   = c(2, 7, 2, 4, 2), # Agregamos el 2 al final
  varNames = c("F1", "F2", "F3", "F4", "F5"), # F5 = Educación
  factors  = "all"
)

# 2. Nueva Fórmula (Solo efecto principal de F5 por ahora)
# Si solo agregamos la variable sin interacciones nuevas, el costo es bajo.
frm_edu <- ~ F1 + F2 + F3 + F4 + F5 + F1:F2 + F1:F4 + F2:F4

X_edu <- model.matrix(frm_edu, cand_edu)
p_edu <- ncol(X_edu) # Debería subir de 39 a 40.

print(">>> DIAGNÓSTICO ESCENARIO 2 (+ EDUCACIÓN) <<<")
print(paste("Nuevos Parámetros (p):", p_edu))
print(paste("Nuevo Ratio (60/40):", round(60/p_edu, 2))) 
# Con ratio 1.50 seguimos en zona segura.

# 3. Optimizar
set.seed(123)
opt_edu <- optFederov(frm_edu, cand_edu, nTrials = 60, criterion = "D", nRepeats = 500)
eval_edu <- eval.design(frm_edu, opt_edu$design, X = cand_edu)

print(paste("G-Efficiency con Educación:", round(eval_edu$Ge, 3)))



# ==============================================================================
# ESCENARIO 3: STRESS TEST (INTERACCIONES COMPLEJAS)
# Qué pasa si añadimos una interacción Triple (Genero x Familia x Educ)
# ==============================================================================

# 1. Fórmula Agresiva
frm_tri <- ~ F1 + F2 + F3 + F4 + F5 + F1:F2:F5

# Calcular costo
X_tri <- model.matrix(frm_tri, cand_edu)
p_tri <- ncol(X_tri)

print(">>> DIAGNÓSTICO ESCENARIO 3 (TRIPLE INTERACCIÓN) <<<")
print(paste("Parámetros necesarios:", p_tri))
# Si p_tri > 60, el script fallará o dará error.
if(p_tri > 60) {
  print("ALERTA: El número de parámetros supera el N muestral. IMPOSIBLE EJECUTAR.")
} else {
  print(paste("Ratio (60/p):", round(60/p_tri, 2)))
  # Si el ratio baja de 1.0 o 1.2, es muy riesgoso.
}

