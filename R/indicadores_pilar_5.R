join_v <- function(df_datasus_cnes, df_ibge_pop) {
    df_datasus_cnes |> 
        filter(ano == 2023) |> 
        left_join(
            df_ibge_pop |> filter(ano == 2023) |> select(cod_uf, populacao),
            by = "cod_uf"
        )
}

calc_indicadores_pilar5 <- function(df_joined) {
    df_joined |> 
        mutate(
            v.1 = n_profissionais / populacao * 1000,
            v.2 = n_leitos / populacao * 1000,
            v.3 = n_leitos_sus / populacao * 1000,
            v.4 = n_leitos_nao_sus / populacao * 1000
        ) |> 
        select(cod_uf, nome_uf, v.1:v.4) |> 
        pivot_longer(
            cols = v.1:v.4,
            names_to = "indicador",
            values_to = "valor"
        )
}