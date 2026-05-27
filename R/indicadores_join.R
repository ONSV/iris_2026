join_all_indicadores <- function(
    df_pilar1, df_pilar2, df_pilar3, df_pilar4, df_pilar5, df_pilar6,
    df_resultado
) {
    bind_rows(
        df_pilar1, df_pilar2, df_pilar3, df_pilar4, df_pilar5, df_pilar6, 
        df_resultado
    ) |> 
        arrange(cod_uf, indicador) |>
        mutate(
            regiao_uf = case_match(
                str_sub(cod_uf, 1, 1),
                "1" ~ "Norte",
                "2" ~ "Nordeste",
                "3" ~ "Sudeste",
                "4" ~ "Sul",
                "5" ~ "Centro-Oeste"
            )
        )
}

