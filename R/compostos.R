scale_indicadores <- function(df_indicadores) {
    df_indicadores |> 
        group_by(indicador) |> 
        arrange(indicador) |> 
        replace_na(list(valor = 1)) |> 
        mutate(valor_padronizado = scale(valor)[, 1]) |> 
        ungroup()
}

invert_indicadores <- function(df_scaled) {
    indicadores_invertidos <- c(
        "i.2", "ii.1", "ii.1", "ii.2", "ii.3", "ii.4", "iii.1", "iii.2",
        "iv.2", "iv.3", "iv.4", "iv.5", "iv.6", "iv.7", "vi.01", "vi.06",
        "0.1", "0.2", "0.3"
    )
    
    df_scaled |> 
        group_by(indicador) |> 
        mutate(
            min = min(valor_padronizado, na.rm = TRUE),
            max = max(valor_padronizado, na.rm = TRUE)
        ) |> 
        ungroup() |> 
        mutate(
            valor_padronizado = if_else(
                indicador %in% indicadores_invertidos,
                max - (valor_padronizado - min),
                valor_padronizado
            )
        )
}

arrange_wide_df <- function(df_scaled) {
    df_scaled |> 
        select(nome_uf, indicador, valor_padronizado) |> 
        pivot_wider(names_from = indicador, values_from = valor_padronizado)
}

calc_pca <- function(df_indicadores_wide, pilar) {
    df_pca <- select(df_indicadores_wide, starts_with(pilar), -nome_uf)
    prcomp(df_pca)
}

extract_pca_var <- function(pca_results) {
    pca_summary <- summary(pca_results)
    pca_var <- pca_summary$importance |> 
        as_tibble() |> 
        slice(2) |> 
        rename_with(\(x) paste0("var_", x))
    
    eigen_criteria <- sum(pca_results$sdev > 1)
    
    valid_pca_var <- select(pca_var, 1:eigen_criteria)
    
    # if (eigen_criteria > 2) {
    #     valid_pca_var <- select(pca_var, 1:eigen_criteria)
    # } else {
    #     valid_pca_var <- select(pca_var, 1:2)
    # }
    return(valid_pca_var)
}

extract_pca_values <- function(pca_results) {
    eigen_criteria <- sum(pca_results$sdev > 1)
    
    pca_values <- as_tibble(pca_results$x)
    
    valid_pca_values <- select(pca_values, 1:eigen_criteria)
    
    # if (eigen_criteria > 2) {
    #     valid_pca_values <- select(pca_values, 1:eigen_criteria)
    # } else {
    #     valid_pca_values <- select(pca_values, 1:2)
    # }
    
    return(valid_pca_values)
    
}

join_pc_values_var <- function(df_indicadores_wide, pca_values, pca_var) {
    df_indicadores_wide |> 
        select(nome_uf) |> 
        bind_cols(pca_values, pca_var)
}

calc_indicador_composto <- function(df_joined_pc, pca_stats) {
    
    pc_count <- df_joined_pc |> 
        select(starts_with("PC")) |>
        ncol()
    
    if (pc_count == 1) {
        pc1_signal <- pca_stats |> 
            filter(!indicador %in% c("eigenvalue", "variancia")) |> 
            pull(PC1) |> 
            sum()
        
        pc1_pos = pc1_signal > 0
        
        df_results <- df_joined_pc |> 
            mutate(
                pc1_pos = pc1_pos,
                indicador_composto = if_else(
                    pc1_pos,
                    var_PC1 * PC1,
                    var_PC1 * -PC1
                )
            ) |> 
            arrange(-indicador_composto) |> 
            select(nome_uf, PC1, indicador_composto)
    } else if (pc_count == 2) {
        pc1_signal <- pca_stats |> 
            filter(!indicador %in% c("eigenvalue", "variancia")) |> 
            pull(PC1) |> 
            sum()
        
        pc2_signal <- pca_stats |>
            filter(!indicador %in% c("eigenvalue", "variancia")) |> 
            pull(PC2) |> 
            sum()
        
        pc1_pos = pc1_signal > 0
        pc2_pos = pc2_signal > 0
        
        df_results <- df_joined_pc |> 
            mutate(
                pc1_pos = pc1_pos,
                pc2_pos = pc2_pos,
                pre_pc1 = if_else(
                    pc1_pos,
                    var_PC1 * PC1,
                    var_PC1 * -PC1
                ),
                pre_pc2 = if_else(
                    pc2_pos,
                    var_PC2 * PC2,
                    var_PC2 * -PC2
                ),
                indicador_composto = pre_pc1 + pre_pc2
            ) |> 
            arrange(-indicador_composto) |> 
            select(nome_uf, PC1, PC2, indicador_composto)
    } else {
        pc1_signal <- pca_stats |> 
            filter(!indicador %in% c("eigenvalue", "variancia")) |> 
            pull(PC1) |> 
            sum()
        
        pc2_signal <- pca_stats |>
            filter(!indicador %in% c("eigenvalue", "variancia")) |> 
            pull(PC2) |> 
            sum()
        
        pc3_signal <- pca_stats |>
            filter(!indicador %in% c("eigenvalue", "variancia")) |> 
            pull(PC3) |> 
            sum()
        
        pc1_pos = pc1_signal > 0
        pc2_pos = pc2_signal > 0
        pc3_pos = pc3_signal > 0
        
        df_results <- df_joined_pc |> 
            mutate(
                pc1_pos = pc1_pos,
                pc2_pos = pc2_pos,
                pc3_pos = pc3_pos,
                pre_pc1 = if_else(
                    pc1_pos,
                    var_PC1 * PC1,
                    var_PC1 * -PC1
                ),
                pre_pc2 = if_else(
                    pc2_pos,
                    var_PC2 * PC2,
                    var_PC2 * -PC2
                ),
                pre_pc3 = if_else(
                    pc3_pos,
                    var_PC3 * PC3,
                    var_PC3 * -PC3
                ),
                indicador_composto = pre_pc1 + pre_pc2 + pre_pc3
            ) |> 
            arrange(-indicador_composto) |> 
            select(nome_uf, PC1, PC2, PC3, indicador_composto)
    }
    
    return(df_results)
    
}

arrange_df_composto <- function(indicadores_compostos, pilar_input) {
    indicadores_compostos |> 
        mutate(pilar = pilar_input)
}

calc_notas <- function(df_indicadores_compostos) {
    df_indicadores_compostos |> 
        group_by(pilar) |> 
        mutate(
            max = max(indicador_composto),
            min = min(indicador_composto),
            nota = (indicador_composto - min) / (max - min) * 10
        ) |> 
        ungroup() |> 
        select(nome_uf, pilar, nota)
}

create_classificacao <- function(df_indicadores_notas) {
    df_indicadores_notas |> 
        group_by(pilar) |> 
        mutate(
            classificacao = cut(
                nota,
                breaks = quantile(
                    nota, 
                    probs = seq(0, 1, length.out = 6), 
                    na.rm = TRUE
                ),
                include.lowest = TRUE,
                labels = c(
                    "*",
                    "**",
                    "***",
                    "****",
                    "*****"
                )
            ),
            classificacao_numeric = case_match(
                classificacao,
                "*" ~ 1,
                "**" ~ 2,
                "***" ~ 3,
                "****" ~ 4,
                "*****" ~ 5
            )
        ) |> 
        ungroup()
}

