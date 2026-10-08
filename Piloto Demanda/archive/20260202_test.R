# ==============================================================================
# PROYECTO FONDECYT: (DEMANDA)
# ==============================================================================

# Cargar librería para diseño algorítmico. Si no está, instalarla.
if(!require(AlgDesign)) install.packages("AlgDesign"); library(AlgDesign)

# Definir función auxiliar para redondear N a decenas (facilita la logística)
round10 <- function(x) {
  as.integer(ceiling(x / 10) * 10)
}

# Configurar contrastes suma-cero (recomendación de Wheeler para ortogonalidad)
options(contrasts = c("contr.sum", "contr.poly"))

# Fijar semilla para asegurar que los resultados sean replicables en el futuro
set.seed(12345)

# Definir repeticiones del algoritmo para evitar óptimos locales
nRepeats_default <- 50 # Número de repeticiones para optFederov

# Crear lista vacía para ir guardando los objetos de cada modelo
results_list <- list()

# ==============================================================================
# MODELO PRETEST (Limitado a 200 Encuestados)
# ==============================================================================
# Contexto: Tenemos 200 personas x 2 viñetas = 400 observaciones totales.
# Objetivo: Maximizar la diversidad (que vean la mayor cantidad de viñetas únicas).

# 1. DEFINIR EL MODELO COMPLETO (Usamos M2mas complejo)
# Niveles: Genero(2), Familia(7), Ocupacion(2), Exp(2), Desempeno(3)
niveles_pretest <- c(2, 7, 2, 2, 3)
nombres_pretest <- c("Genero", "Familia", "Ocupacion", "Exp", "Desempeno")

# Generar el UNIVERSO TOTAL (Candidate Set)
cand_uni_pretest <- gen.factorial(levels = niveles_pretest, varNames = nombres_pretest, factors = "all")

# Calcular tamaño del universo
n_universo <- nrow(cand_uni_pretest)
cat("Tamaño del Universo de Viñetas posibles:", n_universo, "\n")
cat("Capacidad del Pretest (200 x 2): 400 observaciones\n")

# 2. ESTRATEGIA: "FULL DIVERSITY"
# Como tenemos 400 espacios y solo 168 viñetas únicas, NO usar optFederov para reducir.
# Usar el UNIVERSO COMPLETO para garantizar que se pruebe TODO.
# El diseño base será el universo mismo.

# Fórmula del Modelo (para asegurar que el bloqueo respete las interacciones)
frm_pretest <- ~ Genero + Familia + Ocupacion + Exp + Desempeno +
  Genero:Familia + Genero:Ocupacion + Ocupacion:Familia + Genero:Ocupacion:Familia +
  Genero:Desempeno + Familia:Desempeno + Genero:Desempeno:Familia

# 3. GENERACIÓN DE BLOQUES (DECKS)
# Vamos a dividir el universo entero (168) en pares óptimos.
# 168 viñetas / 2 = 84 Bloques Únicos.


set.seed(12345)
bloqueo_pretest <- tryCatch({
  AlgDesign::optBlock(frml = frm_pretest, 
                      withinData = cand_uni_pretest,
                      blocksizes = rep(2, n_universo / 2), 
                      criterion = "D") 
}, error = function(e) {
  warning("Error en el bloqueo: ", e$message)
  NULL
})
print(bloqueo_pretest$diagonality)
if(!is.null(bloqueo_pretest)){
  # 4. ASIGNACIÓN A LOS 200 ENCUESTADOS
  # Tenemos 84 Decks únicos. Tenemos 200 encuestados.
  # Estrategia: Repetir los 84 decks las veces que quepan.
  

  # 200 encuestados / 84 decks = 2.38 vueltas.
  # Esto significa que TODOS los decks se verán al menos 2 veces y algunos 3.
  
  decks_unicos <- bloqueo_pretest$Blocks
  n_decks_unicos <- length(decks_unicos)
  
  # Creamos el vector de asignación para los 200 usuarios
  # Repetimos la secuencia 1:84 hasta llenar 200
  asignacion_decks <- rep(1:n_decks_unicos, length.out = 200)
  

  cat("Total de Decks Únicos generados:", n_decks_unicos, "\n")
  cat("Total de Encuestados:", 200, "\n")
  cat("Cobertura: El universo completo de viñetas se testeará al menos 2 veces.\n")
  cat("Eficiencia del Bloqueo (Diagonality):", round(bloqueo_pretest$diagonality, 3), "\n")
  
  # Mostrar un ejemplo para verificar
  cat("\nEjemplo: encuestado 1\n")
  print(decks_unicos[[1]])
  
  cat("\nEjemplo: El Encuestado #85 vera el 1:\n")
  # Esto confirma la replicación
}

# ==============================================================================
# SIMULACIÓN RÁPIDA 
# ==============================================================================
# Expandimos los datos según la asignación real de 200 personas
if(!is.null(bloqueo_pretest)){
  datos_pretest <- do.call(rbind, decks_unicos[asignacion_decks])
  
  # Creamos respuesta ficticia
  set.seed(999)
  contrasts(datos_pretest$Genero) <- contr.treatment(2) # Para lectura fácil
  
  # Efecto simple: Mujer (-0.5)
  efecto_G <- ifelse(datos_pretest$Genero == "2", -0.5, 0)
  Y_sim <- 7 + efecto_G + rnorm(nrow(datos_pretest), 0, 2)
  datos_pretest$Y <- Y_sim
  
  # Correr modelo
  mod_check <- lm(update(frm_pretest, Y ~ .), data = datos_pretest)
  cat("\nChequeo de Potencia (Genero2 debería ser aprox -0.5):\n")
  print(summary(mod_check)$coefficients["Genero2", ])
}



# ==============================================================================
# Tabla
# ==============================================================================




# 1. Extraer los 84 Decks (Bloques) únicos
lista_de_decks <- bloqueo_pretest$Blocks

# 2. Crear una tabla larga con el contenido de cada Deck
#    Columna 1: ID del Deck (1 a 84)
#    Columna 2: Posición en el Deck (Viñeta 1 o Viñeta 2)
#    Columnas 3-7: Los atributos (Genero, Familia, etc.)
tabla_maestra_decks <- do.call(rbind, lapply(seq_along(lista_de_decks), function(i) {
  df <- lista_de_decks[[i]]
  df$Deck_ID <- i
  df$Vignette_Position <- 1:nrow(df) # 1 o 2
  return(df)
}))

# Reordenar columnas para que Deck_ID quede primero
tabla_maestra_decks <- tabla_maestra_decks[, c("Deck_ID", "Vignette_Position", names(cand_uni_pretest))]

# 3. Asignar Decks a los 200 Participantes (Logística)
#    Queremos que los participantes 1-84 vean los decks 1-84.
#    Los 85-168 repiten los decks 1-84.
#    Los 169-200 ven los decks 1-32.
n_participantes <- 200
n_decks <- length(lista_de_decks) # Debería ser 84

# Creamos un vector que repite la secuencia 1, 2... 84, 1, 2... hasta llegar a 200
asignacion_participantes <- data.frame(
  Participante_ID = 1:n_participantes,
  Deck_Asignado = rep(1:n_decks, length.out = n_participantes)
)

# 4. Unir todo en un archivo final
#    Por cada participante, pegamos la información de las 2 viñetas que le tocan.
archivo_final <- merge(asignacion_participantes, tabla_maestra_decks, 
                       by.x = "Deck_Asignado", by.y = "Deck_ID")

# Ordenar por participante y posición de viñeta
archivo_final <- archivo_final[order(archivo_final$Participante_ID, archivo_final$Vignette_Position), ]

# 5. Exportar a CSV para enviar por correo
write.csv(archivo_final, "ASIGNACION_PRETEST_FONDECYT.csv", row.names = FALSE)

