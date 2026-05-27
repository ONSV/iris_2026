join_01 <- function(df_renavam, df_datasus_sim, df_uf) {
    df_renavam |> 
        filter(ano == 2023) |> 
        group_by(uf) |> 
        summarise(total_frota = sum(frota)) |> 
        left_join(df_uf, by = c("uf" = "sigla_uf")) |> 
        left_join(df_datasus_sim, by = "cod_uf")
}


calc_01 <- function(joined_df) {
    joined_df |>     
        mutate(
            valor = obitos / total_frota * 10000,
            indicador = "0.1"
        ) |> 
        select(cod_uf, nome_uf, indicador, valor)
}

join_02 <- function(df_ibge_pop, df_datasus_sim) {
    df_ibge_pop |> 
        filter(ano == 2023) |> 
        left_join(df_datasus_sim, by = "cod_uf")
}

calc_02 <- function(df_joined) {
    df_joined |>    
        mutate(
            valor = obitos / populacao * 100000,
            indicador = "0.2"
        ) |> 
        select(cod_uf, nome_uf, indicador, valor)
}

calc_03 <- function(df_uf, df_taxa) {
    df_taxa |> 
        left_join(df_uf, by = "nome_uf") |> 
        mutate(indicador = "0.3") |> 
        select(cod_uf, nome_uf, indicador, valor = taxa_bilhao)
}