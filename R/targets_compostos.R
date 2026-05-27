targets_compostos <- list(
    tar_target(indicadores_scaled, scale_indicadores(indicadores)),
    tar_target(
        indicadores_scaled_inverted, 
        invert_indicadores(indicadores_scaled)
    ),
    tar_target(indicadores_wide, arrange_wide_df(indicadores_scaled_inverted)),
    ## PCA
    tar_target(
        list_pca_results,
        map(
            c("i.", "ii.", "iii.", "iv.", "v.", "vi.", "0."),
            calc_pca,
            df_indicadores_wide = indicadores_wide
        )
    ),
    tar_target(list_pca_var, map(list_pca_results, extract_pca_var)),
    tar_target(list_pca_values, map(list_pca_results, extract_pca_values)),
    ## Indicadores compostos
    tar_target(
        list_joined_pc,
        map2(
            list_pca_values,
            list_pca_var,
            join_pc_values_var,
            df_indicadores_wide = indicadores_wide
        )
    ),
    tar_target(
        list_indicadores_compostos,
        map2(
            list_joined_pc,
            list_pca_stats,
            calc_indicador_composto
        )
    ),
    tar_target(
        indicadores_compostos,
        map2(
            list_indicadores_compostos,
            c(
                "Pilar I", "Pilar II", "Pilar III", "Pilar IV", "Pilar V",
                "Pilar VI", "Resultado final"
            ),
            arrange_df_composto
        ) |> 
            reduce(bind_rows) |> 
            select(nome_uf, pilar, PC1:PC3, indicador_composto)
    ),
    ## Notas
    tar_target(indicadores_notas, calc_notas(indicadores_compostos)),
    ## Classificacao
    tar_target(
        classificacao,
        create_classificacao(indicadores_notas)
    )
)
