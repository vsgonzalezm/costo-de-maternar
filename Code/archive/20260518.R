setwd("/Users/isidorajeria/Google Drive/Mi unidad/Costo de Maternar/Code")
# D-EFFIENCY — DOS SUB-DISEÑOS: UNO PROFESIONAL - UNO TÉCNICO

#   Genero      : 2 niveles (Mujer, Hombre)
#   Ecivil      : 2 niveles (Soltero/a, Casado/a)
#   Eparental   : 3 niveles (sin hijos, un hijo, dos hijos)
#   Planfamilia : 3 niveles (quiere en 2 años, quiere en 5 años, no quiere)
#   Exp         : 3 niveles (6, 7, 8 años)  — control
#   Desempeno   : 3 niveles (5.8, 6.0, 6.3) — control
#
# Modelo: Genero * Ecivil * Eparental * Planfamilia + Exp + Desempeno


if (!require(AlgDesign)) install.packages("AlgDesign"); library(AlgDesign)
if (!require(dplyr))     install.packages("dplyr");     library(dplyr)

round10 <- function(x) as.integer(ceiling(x / 10) * 10)

options(contrasts = c("contr.sum", "contr.poly"))



#  1: DEFINIR LOS NIVELES Y GENERAR EL ESPACIO CANDIDATO
# Ocupación eliminada del espacio factorial (será between-subjects)


niveles_nuevos <- c(2, 2, 3, 3, 3, 3)
nombres_nuevos <- c("Genero", "Ecivil", "Eparental", "Planfamilia", "Exp", "Desempeno")

cand_uni <- gen.factorial(levels = niveles_nuevos,
                          varNames = nombres_nuevos,
                          factors = "all")



#  2: DEFINIR EL MODELO

frm_final <- ~ Genero * Ecivil * Eparental * Planfamilia + Exp + Desempeno



#  3: CALCULAR k Y DERIVAR N

k_final  <- ncol(model.matrix(frm_final, cand_uni))
N_final  <- round10(3 * k_final)

cat("\n[REPORTE] Parámetros (k):", k_final, "| N sugerido:", N_final, "\n")

# N se deriva de la regla 3k; no se sobreescribe manualmente.
# Ambas ramas (Técnico y Profesional) comparten este mismo diseño base.

cat("[REPORTE] N del estudio:", N_final,
    sprintf("(3x el mínimo sugerido por regla 3k)\n"))



#  4: D-EFFICIENCY — UN ÚNICO DISEÑO COMPARTIDO
# Se genera un solo diseño D-óptimo; ambas ramas parten de él

set.seed(12345)

opt_diseno <- optFederov(frml      = frm_final,
                         data      = cand_uni,
                         nTrials   = N_final,
                         criterion = "D",
                         nRepeats  = 100,
                         nullify   = 1)

diseno_final <- as.data.frame(opt_diseno$design)



#  5: EVALUAR LAS MÉTRICAS DEL DISEÑO
# Se evalúa contra cand_uni (espacio candidato sin replicar).

eval_final <- eval.design(frm_final, diseno_final, X = cand_uni)

cat(sprintf("\n[MÉTRICAS] D-Efficiency: %.3f | G-Efficiency: %.3f\n",
            eval_final$De, eval_final$Ge))



#  6: CONSTRUCCIÓN DE BLOQUES — optBlock()

N_bloques <- N_final / 2   # bloques de 2 viñetas cada uno

set.seed(999)
bloques_opt <- optBlock(frml       = frm_final,
                        withinData = diseno_final,
                        blocksizes = rep(2, N_bloques),
                        criterion  = "D")



#  7: CONSTRUIR EL DATA FRAME CON BLOQUES Y POSTULANTE A/B

df_con_bloques            <- as.data.frame(bloques_opt$design)
df_con_bloques$Block      <- rep(1:N_bloques, each = 2)
df_con_bloques$Postulante <- rep(c("A", "B"), N_bloques)

# Validación de bloques
cat("\n[VALIDACIÓN DE BLOQUES]\n")
cat("Total de viñetas:", nrow(df_con_bloques), "\n")
cat("Total de bloques:", length(unique(df_con_bloques$Block)), "\n")
cat("Viñetas por bloque (deben ser 2):\n")
print(table(table(df_con_bloques$Block)))



#  8: MAPEO DE TEXTOS



procesar_textos <- function(df_input, tipo_perfil) {
  
  nombres_mujer  <- c("Marcela", "Natalia")
  nombres_hombre <- c("Claudio", "Patricio")
  
  df_output <- as.data.frame(df_input) %>%
    mutate(
      # Género
      Texto_Genero = if_else(Genero == "1", "Mujer", "Hombre"),
      
      # Estado civil con concordancia de género (2 niveles: soltero/a, casado/a)
      Texto_Civil = case_when(
        Texto_Genero == "Mujer"  & Ecivil == "1" ~ "soltera",
        Texto_Genero == "Mujer"  & Ecivil == "2" ~ "casada",
        Texto_Genero == "Hombre" & Ecivil == "1" ~ "soltero",
        Texto_Genero == "Hombre" & Ecivil == "2" ~ "casado"
      ),
      
      # Ocupación inyectada como between-subjects según tipo_perfil
      Placeholder_Ocupacion = case_when(
        tipo_perfil == "Profesional" & Texto_Genero == "Mujer"  ~ "Asociada financiera",
        tipo_perfil == "Profesional" & Texto_Genero == "Hombre" ~ "Asociado financiero",
        TRUE ~ "Analista contable"   # nivel Técnico; término neutro en género
      ),
      
      # Educación fija por tipo de perfil
      Placeholder_Educacion = if_else(
        tipo_perfil == "Profesional",
        "Título profesional en Ingeniería Comercial",
        "Título técnico en Contabilidad"
      ),
      
      # Situación familiar: combinación de Eparental × Planfamilia
      Texto_Situacion = case_when(
        Eparental == "1" & Planfamilia == "1" ~ "actualmente no tiene hijos y le gustaría tener en los próximos 2 años.",
        Eparental == "1" & Planfamilia == "2" ~ "actualmente no tiene hijos y le gustaría tener en los próximos 5 años.",
        Eparental == "1" & Planfamilia == "3" ~ "actualmente no tiene hijos y no tiene planes de tenerlos en el futuro.",
        Eparental == "2" & Planfamilia == "1" ~ "tiene un hijo y le gustaría tener otro en los próximos 2 años.",
        Eparental == "2" & Planfamilia == "2" ~ "tiene un hijo y le gustaría tener otro en los próximos 5 años.",
        Eparental == "2" & Planfamilia == "3" ~ "tiene un hijo y no quiere tener más.",
        Eparental == "3" & Planfamilia == "1" ~ "tiene dos hijos y le gustaría tener otro en los próximos 2 años.",
        Eparental == "3" & Planfamilia == "2" ~ "tiene dos hijos y le gustaría tener otro en los próximos 5 años.",
        Eparental == "3" & Planfamilia == "3" ~ "tiene dos hijos y no quiere tener más."
      ),
      
      # Experiencia y desempeño
      Placeholder_Exp       = case_when(Exp == "1" ~ "6 años",
                                        Exp == "2" ~ "7 años",
                                        Exp == "3" ~ "8 años"),
      Placeholder_Desempeno = case_when(Desempeno == "1" ~ "5.8",
                                        Desempeno == "2" ~ "6.0",
                                        Desempeno == "3" ~ "6.3")
    ) %>%
    # Nombres asignados por posición dentro del bloque
    arrange(Block) %>%
    group_by(Block) %>%
    mutate(
      idx_bloque         = row_number(),
      Placeholder_Nombre = if_else(Texto_Genero == "Mujer",
                                   nombres_mujer[idx_bloque],
                                   nombres_hombre[idx_bloque])
    ) %>%
    select(-idx_bloque) %>%
    ungroup() %>%
    # Texto familiar completo que va al placeholder [TEXTO_ESTADO_FAMILIAR]
    mutate(
      Texto_Familia_Completo = paste0("se encuentra ", Texto_Civil, ", ", Texto_Situacion)
    )
  
  return(df_output)
}



#  9: CREAR LAS DOS RAMAS (SPLIT-SAMPLE BETWEEN-SUBJECTS)
# Ambas ramas parten del mismo df_con_bloques (diseño compartido).

df_rama_profesional <- procesar_textos(df_con_bloques, tipo_perfil = "Profesional")
df_rama_tecnica     <- procesar_textos(df_con_bloques, tipo_perfil = "Tecnico")

# Validación de bloques
for (info in list(list(df = df_rama_tecnica,     nombre = "Técnico"),
                  list(df = df_rama_profesional, nombre = "Profesional"))) {
  cat(sprintf("\n[VALIDACIÓN DE BLOQUES] Diseño: %s\n", info$nombre))
  cat("Total de viñetas:", nrow(info$df), "\n")
  cat("Total de bloques:", length(unique(info$df$Block)), "\n")
  cat("Viñetas por bloque (deben ser 2):\n")
  print(table(table(info$df$Block)))
}



#  10: VALIDACIÓN FINAL

cat("\n[VALIDACIÓN FINAL] Primeros 5 bloques — Técnico\n")
print(df_rama_tecnica %>%
        filter(Block %in% 1:5) %>%
        select(Block, Postulante, Texto_Genero, Placeholder_Nombre, Texto_Civil))

cat("\n[VALIDACIÓN FINAL] Primeros 5 bloques — Profesional\n")
print(df_rama_profesional %>%
        filter(Block %in% 1:5) %>%
        select(Block, Postulante, Texto_Genero, Placeholder_Nombre, Texto_Civil))



#EVALUACIÓN
#Calidad del d-efficiency
eval_final <- eval.design(frm_final, diseno_final, X = cand_uni)

cat(sprintf("\n[MÉTRICAS] D-Efficiency: %.3f | G-Efficiency: %.3f\n",
            eval_final$De, eval_final$Ge))

#BALANCE ATRIBUTOS
for (nombre in c("Técnico", "Profesional")) {
  df <- if (nombre == "Técnico") df_rama_tecnica else df_rama_profesional
  cat(sprintf("\n--- Balance: %s ---\n", nombre))
  cat("Género:\n");      print(prop.table(table(df$Texto_Genero)))
  cat("Estado civil:\n"); print(prop.table(table(df$Texto_Civil)))
  cat("Eparental:\n");    print(prop.table(table(df$Eparental)))
  cat("Planfamilia:\n");  print(prop.table(table(df$Planfamilia)))
  cat("Exp:\n");          print(prop.table(table(df$Exp)))
  cat("Desempeno:\n");    print(prop.table(table(df$Desempeno)))
}

#BALANCE DENTRO DE LOS BLOQUES
for (nombre in c("Técnico", "Profesional")) {
  df <- if (nombre == "Técnico") df_rama_tecnica else df_rama_profesional
  cat(sprintf("\n--- Balance intra-bloque: %s ---\n", nombre))
  resumen <- df %>%
    group_by(Block) %>%
    summarise(n_generos     = n_distinct(Texto_Genero),
              n_civil       = n_distinct(Texto_Civil),
              n_eparental   = n_distinct(Eparental),
              n_planfamilia = n_distinct(Planfamilia))
  cat("Bloques con género distinto (ideal = 2):\n")
  print(prop.table(table(resumen$n_generos)))
  cat("Bloques con estado civil distinto (ideal = 2):\n")
  print(prop.table(table(resumen$n_civil)))
  cat("Bloques con Eparental distinto (ideal = 2):\n")
  print(prop.table(table(resumen$n_eparental)))
  cat("Bloques con Planfamilia distinto (ideal = 2):\n")
  print(prop.table(table(resumen$n_planfamilia)))
}



#  11: EXPORTACIÓN

cols_export <- c("Block", "Postulante", "Texto_Genero", "Placeholder_Nombre",
                 "Texto_Familia_Completo", "Placeholder_Ocupacion",
                 "Placeholder_Educacion", "Placeholder_Exp", "Placeholder_Desempeno")

archivo_vinetas_tecnico <- df_rama_tecnica %>%
  select(all_of(cols_export)) %>%
  arrange(Block)

archivo_vinetas_profesional <- df_rama_profesional %>%
  select(all_of(cols_export)) %>%
  arrange(Block)

write.csv(archivo_vinetas_tecnico,     "Base_Vinetas_Tecnico.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(archivo_vinetas_profesional, "Base_Vinetas_Profesional.csv",
          row.names = FALSE, fileEncoding = "UTF-8")

cat("\nArchivos exportados:\n")
cat(sprintf("  Base_Vinetas_Tecnico.csv      — %d viñetas | %d bloques\n", N_final, N_bloques))
cat(sprintf("  Base_Vinetas_Profesional.csv  — %d viñetas | %d bloques\n", N_final, N_bloques))

