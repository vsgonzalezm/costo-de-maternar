### CHECK VIÑETAS PILOTO DEMANDA ###

df <- read.csv("Base_Vinetas_Netquest_Final.csv")


# 2. Every block has exactly 2 rows
block_counts <- table(df$Block)
stopifnot(all(block_counts == 2))
stopifnot(length(block_counts) == N_final / 2)

# 3. No NAs anywhere
stopifnot(sum(is.na(df)) == 0)

# 4. Gender values are only "Mujer" / "Hombre"
stopifnot(all(df$Texto_Genero %in% c("Mujer", "Hombre")))

# 5. Names are consistent with gender
stopifnot(all(df$Placeholder_Nombre[df$Texto_Genero == "Mujer"] %in% c("Marcela", "Natalia")))
stopifnot(all(df$Placeholder_Nombre[df$Texto_Genero == "Hombre"] %in% c("Claudio", "Patricio")))

# 6. Within each block, names are never repeated
name_check <- df %>% group_by(Block) %>% 
  summarise(unique_names = n_distinct(Placeholder_Nombre))
stopifnot(all(name_check$unique_names == 2))  # Each block has 2 distinct names

# 7. Occupation matches gender (no "Asociada" for Hombre)
stopifnot(!any(df$Texto_Genero == "Hombre" & grepl("Asociada", df$Placeholder_Ocupacion)))
stopifnot(!any(df$Texto_Genero == "Mujer" & grepl("Asociado financiero$", df$Placeholder_Ocupacion)))

# 8. Civil status concordance
stopifnot(!any(df$Texto_Genero == "Mujer" & grepl("soltero|casado", df$Texto_Familia_Completo) & 
                 !grepl("soltera|casada", df$Texto_Familia_Completo)))

# 9. Exp values are only 6, 7, or 8 años
stopifnot(all(df$Placeholder_Exp %in% c("6 años", "7 años", "8 años")))

# 10. Desempeño values are valid
stopifnot(all(df$Placeholder_Desempeno %in% c("5.8", "6.0", "6.3")))

# 11. Check D-efficiency was acceptable (already printed, but log it)
cat("D-Efficiency:", eval_final$De, "\n")
cat("G-Efficiency:", eval_final$Ge, "\n")

# 12. Each attribute level should appear roughly equally
df_orig <- df_con_bloques  # Before text mapping
for(col in nombres_nuevos) {
  cat(col, ":\n"); print(table(df_orig[[col]])); cat("\n")
}


# Count how many attributes DIFFER within each block
attr_cols <- c("Genero", "Ecivil", "Eparental", "Planfamilia", "Ocupacion", "Exp", "Desempeno")

block_diversity <- df_con_bloques %>%
  group_by(Block) %>%
  summarise(
    n_diff = sum(sapply(attr_cols, function(col) n_distinct(.data[[col]]) > 1)),
    attrs_diff = paste(attr_cols[sapply(attr_cols, function(col) n_distinct(.data[[col]]) > 1)], collapse = ", "),
    attrs_same = paste(attr_cols[sapply(attr_cols, function(col) n_distinct(.data[[col]]) == 1)], collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(n_diff)

# Summary of how many blocks differ on k attributes
cat("Distribution of attribute differences per block:\n")
print(table(block_diversity$n_diff))

# Flag blocks that differ on 2 or fewer attributes (out of 7) — adjust threshold as needed
problematic <- block_diversity %>% filter(n_diff == 0)
cat("\nProblematic blocks (≤2 attributes differ):", nrow(problematic), "\n")
print(problematic)