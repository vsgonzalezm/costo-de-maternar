# ===========================================================================
# SIMULACIÓN DE DISEÑO CONJOINT Y VIÑETAS PARA SESGO DE FERTILIDAD POTENCIAL
# ===========================================================================

library(DeclareDesign) #Paquete especializado para diseño de experimentos
library(dplyr)
library(estimatr)
library(ggplot2)

# =========================================================================
# 1. DISEÑO DE CONJOINT
# =========================================================================
# Parámetros del diseño
# -------------------------------------------------------------------------
n_sujetos <- 2000
n_tareas <- 2
n_perfiles <- 2
total_obs <- n_sujetos * n_tareas * n_perfiles

# -------------------------------------------------------------------------
# Definición del modelo
# -------------------------------------------------------------------------
conjoint <- declare_model(
  sujetos = add_level(N = n_sujetos, u_i = rnorm(n_sujetos)),
  observaciones = add_level(N = n_tareas * n_perfiles, u_ij = rnorm(total_obs))
) +
  declare_assignment(
    gender = factor(sample(c("Man", "Woman"), total_obs, replace = TRUE)),
    age = factor(sample(c("27", "31", "35"), total_obs, replace = TRUE)),
    civil = factor(sample(c("Single", "Cohabiting", "Married"), total_obs, replace = TRUE)),
    children = factor(sample(c("No children", "1 child (preschool)", "2 children (school)"), total_obs, replace = TRUE)),
    education = factor(sample(c("Technical degree", "University degree"), total_obs, replace = TRUE)),
    occupation = factor(sample(c("Sales analyst", "Accounting assistant", "Legal assistant", "Administrative analyst"), total_obs, replace = TRUE)),
    motivation = factor(sample(c("Highly motivated", "Strong commitment"), total_obs, replace = TRUE))
  ) +
  declare_step(handler = function(data) {
    # Supuestos de dirección/magnitud
    # Castigo fuerte por "Fertilidad Potencial": Mujer joven (27) sin hijos
    data$score <- 0.5 + 
      (data$gender == "Woman") * -0.2 + 
      (data$education == "University degree") * 0.6 +
      (data$gender == "Woman" & data$age == "27" & data$children == "No children") * -1.2 + 
      (data$gender == "Woman" & data$age == "31" & data$children == "No children") * -0.8 +
      data$u_i + data$u_ij
    
    # Transformación Logística a Probabilidad 0-100
    data$prob_contratacion <- 1 / (1 + exp(-data$score))
    data$hiring <- round(data$prob_contratacion * 100)
    return(data)
  })

# -------------------------------------------------------------------------
# Simulación y poder estadístico
# -------------------------------------------------------------------------
set.seed(2025)

simulacion <- function(n_reps = 30) {
  lista_resultados <- replicate(n_reps, {
    dat <- draw_data(conjoint)
    
    # Utilizamos Man / 35 / 2 children como cateogria de base para que el riesgo de la mujer joven aparezca
    dat$gender <- relevel(factor(dat$gender), ref = "Man")
    dat$age <- relevel(factor(dat$age), ref = "35")
    dat$children <- relevel(factor(dat$children), ref = "2 children (school)")
    
    mod <- lm_robust(hiring ~ gender * age * children + civil + education + 
                       occupation + motivation, data = dat, clusters = sujetos)
    
    res <- as.data.frame(summary(mod)$coefficients)
    res$term <- rownames(res)
    return(res)
  }, simplify = FALSE)
  
  df_consolidado <- bind_rows(lista_resultados) %>%
    group_by(term) %>%
    summarize(
      estimate = mean(Estimate),
      std_error = mean(`Std. Error`),
      poder = mean(`Pr(>|t|)` < 0.05)
    )
  return(df_consolidado)
}

df_resultados_final <- simulacion(30)

# -------------------------------------------------------------------------
# Gráfico Forest plot (magnitud, significancia y poder)
# -------------------------------------------------------------------------
# Limpieza de etiquetas 
df_plot <- df_resultados_final %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = gsub(" \\(preschool\\)", "", term),
    term = gsub(" \\(school\\)", "", term),
    term = gsub("genderWoman", "Woman", term),
    term = gsub("age", "Age:", term),
    term = gsub("children", "", term),
    term = gsub(":", " x ", term),
    lower = estimate - (1.96 * std_error),
    upper = estimate + (1.96 * std_error)
  ) %>%
  # Filtramos solo términos relevantes (principales e interacciones de interés)
  # para evitar un gráfico saturado con 50 líneas
  filter(poder > 0.1 | grepl("Woman", term)) 

ggplot(df_plot, aes(x = estimate, y = reorder(term, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.2, color = "gray30") +
  geom_point(aes(size = poder, color = poder >= 0.8), alpha = 0.8) +
  scale_color_manual(values = c("tomato", "steelblue"), 
                     labels = c("Bajo (<0.8)", "Alto (>=0.8)"),
                     name = "Poder Estadístico") +
  scale_size_continuous(range = c(2, 6), name = "Poder (0-1)") +
  labs(
    title = "Resultados Conjoint: Sesgo por Fertilidad Potencial",
    subtitle = "Triple Interacción (Gender x Age x Children). Baseline: Man, 35 years, 2 children.",
    x = "Cambio en Probabilidad de Recomendación (Puntos Porcentuales 0-100)",
    y = "Niveles de Atributos e Interacciones",
    caption = "Nota: Valores negativos indican penalización en la contratación."
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.y = element_text(size = 9))



# =========================================================================
# 2. DISEÑO DE VIÑETAS (2 viñetas por persona, N=1000 por tipo de perfil)
# =========================================================================
# Función para crear el diseño según el tipo de perfil
# -------------------------------------------------------------------------
  vignette <- function(n_sujetos, tipo_perfil) {
  declare_model(
    sujetos = add_level(N = n_sujetos, u_i = rnorm(n_sujetos)),
    vignettes = add_level(N = 2, u_ij = rnorm(n_sujetos * 2)) # 2 viñetas por persona
  ) +
    declare_assignment(
      gender = factor(sample(c("Man", "Woman"), n_sujetos * 2, replace = TRUE)),
      age = factor(sample(c("27", "31", "35"), n_sujetos * 2, replace = TRUE)),
      children = factor(sample(c("No children", "1 child", "2 children"), n_sujetos * 2, replace = TRUE)),
      perfil = tipo_perfil
    ) +
    declare_step(handler = function(data) {
      # Supuesto:
      # El castigo es mayor en el perfil Profesional que en el Técnico
      penalty <- if(tipo_perfil == "Professional") -1.1 else -0.7
      
      data$score <- 0.5 + 
        (data$gender == "Woman") * -0.2 + 
        (data$gender == "Woman" & data$age == "27" & data$children == "No children") * penalty + 
        data$u_i + data$u_ij
      
      data$hiring <- round((1 / (1 + exp(-data$score))) * 100)
      return(data)
    })
  }

# -------------------------------------------------------------------------
# Simulación y análisis por submuestras
# -------------------------------------------------------------------------
set.seed(2025)

# Muestras separadas según tipo de perfil
diseno_prof <- vignette(2000, "Professional")
diseno_tech <- vignette(2000, "Technical")

dat_prof <- draw_data(diseno_prof)
dat_tech <- draw_data(diseno_tech)

# Función para procesar modelos con referencias consistentes
  modelo_vignette <- function(data) {
  data$gender <- relevel(factor(data$gender), ref = "Man")
  data$age <- relevel(factor(data$age), ref = "35")
  data$children <- relevel(factor(data$children), ref = "2 children")
  
  lm_robust(hiring ~ gender * age * children, data = data, clusters = sujetos)
}

mod_prof <- modelo_vignette(dat_prof)
mod_tech <- modelo_vignette(dat_tech)

# -------------------------------------------------------------------------
# Comparación de resultados
# -------------------------------------------------------------------------
  tabla <- function(model, label) {
  res <- as.data.frame(summary(model)$coefficients)
  res$term <- rownames(res)
  res$experiment <- label
  return(res)
}

df_comparativo <- bind_rows(
  tabla(mod_prof, "Professional Profile"),
  tabla(mod_tech, "Technical Profile")
)

# Limpieza de nombres para el gráfico
df_plot_vignette <- df_comparativo %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = gsub("genderWoman", "Woman", term),
    term = gsub("age", "Age:", term),
    term = gsub("children", "", term),
    term = gsub(":", " x ", term),
    lower = Estimate - (1.96 * `Std. Error`),
    upper = Estimate + (1.96 * `Std. Error`)
  )

ggplot(df_plot_vignette %>% filter(grepl("Woman", term)), 
       aes(x = Estimate, y = reorder(term, Estimate), color = experiment)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.3, position = position_dodge(0.5)) +
  geom_point(position = position_dodge(0.5), size = 3) +
  labs(
    title = "Experimento de Viñetas: Penalización por Fertilidad",
    subtitle = "Comparación entre Perfil Profesional vs. Técnico (N=1000 cada uno)",
    x = "Efecto en Probabilidad de Contratación (0-100)",
    y = "Interacciones de Género, Edad e Hijos",
    color = "Tipo de Experimento"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "bottom")


# -------------------------------------------------------------------------
# Poder para ambos experimentos de viñetas
# -------------------------------------------------------------------------
set.seed(2025)

poder_comparativo <- function(n_reps = 100) {
  
  resultados <- replicate(n_reps, {
    
    # Simulación Sub-muestra Profesional (N=1000)
    diseno_prof <- vignette(2000, "Professional")
    dat_prof <- draw_data(diseno_prof)
    mod_prof <- modelo_vignette(dat_prof)
    p_prof <- summary(mod_prof)$coefficients[, "Pr(>|t|)"] < 0.05
    
    # Simulación Sub-muestra Técnica (N=1000)
    diseno_tech <- vignette(2000, "Technical")
    dat_tech <- draw_data(diseno_tech)
    mod_tech <- modelo_vignette(dat_tech)
    p_tech <- summary(mod_tech)$coefficients[, "Pr(>|t|)"] < 0.05
    
    # Unimos resultados
    list(prof = p_prof, tech = p_tech)
    
  }, simplify = FALSE)
  
  # Extraer nombres de los coeficientes
  nombres <- names(resultados[[1]]$prof)
  
  # Calcular promedio de éxitos (poder) para cada grupo
  poder_prof <- rowMeans(do.call(cbind, lapply(resultados, function(x) x$prof)))
  poder_tech <- rowMeans(do.call(cbind, lapply(resultados, function(x) x$tech)))
  
  data.frame(
    Atributo = nombres,
    Professional = poder_prof,
    Technical = poder_tech
  )
}

  # Ejecutar la simulación
  df_poder_vignette <- simular_poder_comparativo(100)

# -------------------------------------------------------------------------
# Gráfico de comparación de poder (Lollipop Plot)
# -------------------------------------------------------------------------
df_poder_long <- df_poder_vignette %>%
  filter(Atributo != "(Intercept)") %>%
  pivot_longer(cols = c(Professional, Technical), names_to = "Experimento", values_to = "Poder") %>%
  mutate(Atributo = gsub("genderWoman", "Woman", Atributo),
         Atributo = gsub("age", "Age:", Atributo),
         Atributo = gsub("children", "", Atributo),
         Atributo = gsub(":", " x ", Atributo))

ggplot(df_poder_long, aes(x = Poder, y = reorder(Atributo, Poder), color = Experimento)) +
  geom_segment(aes(x = 0, xend = Poder, yend = Atributo), color = "grey", alpha = 0.5) +
  geom_point(size = 4) +
  geom_vline(xintercept = 0.8, linetype = "dashed", color = "red") +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Comparación de Poder Estadístico: Viñetas",
    subtitle = "Muestra: 1000 sujetos por experimento (2 viñetas c/u)",
    x = "Poder (Probabilidad de detectar efecto significativo)",
    y = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")



# =========================================================================
# PODER DE CONJOINT VS VIÑETAS
# =========================================================================
set.seed(2025)

comparar_poder <- function(n_reps = 50) {
  
  resultados <- replicate(n_reps, {
    
    # Conjoint
    dat_conj <- draw_data(diseno_conjoint_triple)
    # Forzamos referencias para que la interacción sea visible y no sea la base de referencia
    dat_conj$gender <- relevel(factor(dat_conj$gender), ref = "Man")
    dat_conj$age <- relevel(factor(dat_conj$age), ref = "35")
    dat_conj$children <- relevel(factor(dat_conj$children), ref = "2 children (school)")
    
    mod_conj <- lm_robust(hiring ~ gender * age * children, data = dat_conj, clusters = sujetos)
    
    # Buscamos el término de "Fertilidad Potencial" de forma segura
    coefs_conj <- rownames(summary(mod_conj)$coefficients)
    target <- coefs_conj[grepl("Woman", coefs_conj) & grepl("27", coefs_conj) & grepl("No children", coefs_conj)]
    p_conj <- summary(mod_conj)$coefficients[target, "Pr(>|t|)"] < 0.05
    
    # Viñetas
    # Usamos el diseño global de 2000 sujetos para comparar peras con peras
    vignette_total <- vignette(2000, "Global") 
    dat_vign <- draw_data(vignette_total)
    dat_vign$gender <- relevel(factor(dat_vign$gender), ref = "Man")
    dat_vign$age <- relevel(factor(dat_vign$age), ref = "35")
    dat_vign$children <- relevel(factor(dat_vign$children), ref = "2 children")
    
    mod_vign <- lm_robust(hiring ~ gender * age * children, data = dat_vign, clusters = sujetos)
    
    coefs_vign <- rownames(summary(mod_vign)$coefficients)
    target_vign <- coefs_vign[grepl("Woman", coefs_vign) & grepl("27", coefs_vign) & grepl("No children", coefs_vign)]
    p_vign <- summary(mod_vign)$coefficients[target_vign, "Pr(>|t|)"] < 0.05
    
    list(conj = p_conj, vign = p_vign)
  }, simplify = FALSE)
  
  # Poder promedio
  poder_final <- data.frame(
    Metodo = c("Conjoint", "Vignettes"),
    Poder = c(mean(sapply(resultados, function(x) x$conj)),
              mean(sapply(resultados, function(x) x$vign)))
  )
  return(poder_final)
}

df_poder_comparado <- comparar_metodos_poder(30)
print(df_poder_comparado)


# -------------------------------------------------------------------------
# Gráfico de comparación de poder conjoint vs viñetas
# -------------------------------------------------------------------------
# Función para extraer y limpiar resultados
obtener_stats <- function(model, label) {
  res <- as.data.frame(summary(model)$coefficients)
  res$term <- rownames(res)
  res$experimento <- label
  # Calculamos el poder aproximado basado en el SE actual (para el gráfico)
  # Un valor p < 0.05 indica que ese hallazgo específico es significativo
  res$es_sig <- res$`Pr(>|t|)` < 0.05
  return(res)
}

# Ejecución de modelos
df_total <- bind_rows(
  obtener_stats(mod_conj, "1. Conjoint (N=2000)"),
  obtener_stats(mod_prof, "2. Viñeta Prof. (N=1000)"),
  obtener_stats(mod_tech, "3. Viñeta Técn. (N=1000)")
)

# Limpieza técnica de los nombres de las variables
df_final_limpio <- df_total %>%
  filter(grepl("Woman", term)) %>% 
  mutate(
    term = gsub(" \\(preschool\\)", "", term),
    term = gsub(" \\(school\\)", "", term),
    term = gsub("genderWoman", "Woman", term),
    term = gsub("age", "Age:", term),
    term = gsub("children", "", term),
    term = gsub(":", " x ", term),
    lower = Estimate - (1.96 * `Std. Error`),
    upper = Estimate + (1.96 * `Std. Error`)
  )


ggplot(df_final_limpio, aes(x = Estimate, y = experimento, color = experimento)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2, linewidth = 0.8) +
  geom_point(aes(alpha = es_sig), size = 4) +
  facet_wrap(~term, scales = "free_x", ncol = 1) +
  scale_color_brewer(palette = "Set1") +
  scale_alpha_manual(values = c(0.4, 1), labels = c("No Sig.", "Sig. (p<0.05)"), name = "Resultado") +
  labs(
    title = "Comparación de Resultados: Conjoint vs. Viñetas",
    subtitle = "Triple interacción y efectos de género (Baseline: Man, 35 years, 2 Children)",
    x = "Efecto Estimado (Puntos Porcentuales 0-100)",
    y = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 10),
    panel.background = element_rect(fill = "gray98", color = NA)
  )


# -------------------------------------------------------------------------
# Gráfico de precisión para la interacción de interés
# -------------------------------------------------------------------------

df_precision <- data.frame(
  Metodo = c("Conjoint", "Viñeta Profesional", "Viñeta Técnica"),
  Estimacion = c(
    summary(mod_conj)$coefficients["genderWoman:age27:childrenNo children", "Estimate"],
    summary(mod_prof)$coefficients["genderWoman:age27:childrenNo children", "Estimate"],
    summary(mod_tech)$coefficients["genderWoman:age27:childrenNo children", "Estimate"]
  ),
  Error_Estandar = c(
    summary(mod_conj)$coefficients["genderWoman:age27:childrenNo children", "Std. Error"],
    summary(mod_prof)$coefficients["genderWoman:age27:childrenNo children", "Std. Error"],
    summary(mod_tech)$coefficients["genderWoman:age27:childrenNo children", "Std. Error"]
  )
)

# Gráfico de Precisión: mientras más estrecha la barra, mejor es el experimento
ggplot(df_precision, aes(x = Estimacion, y = Metodo, fill = Metodo)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = Estimacion - 1.96*Error_Estandar, 
                     xmax = Estimacion + 1.96*Error_Estandar), height = 0.2) +
  geom_point(size = 5, shape = 21, color = "white") +
  labs(title = "¿Cuál experimento es más preciso?",
       subtitle = "Comparación de la Triple Interacción (Mujer x 27 x Sin Hijos)",
       x = "Efecto Estimado (Puntos Porcentuales)",
       caption = "Barras más cortas = Mayor precisión y poder estadístico.") +
  theme_minimal() + theme(legend.position = "none")
