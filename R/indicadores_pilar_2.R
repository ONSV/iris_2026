calc_indicadores_cnt <- function(df_cnt, input_indicador) {
  df_cnt |> 
    mutate(
      valor = (ruim + pessimo) / total,
      indicador = input_indicador
    ) |> 
    select(nome_uf = uf, indicador, valor)
}

calc_ii5 <- function(df_dnit) {
  df_dnit |> 
    filter(ano == 2024) |> 
    mutate(
      valor = pista_dupla / total_pavimentado,
      indicador = "ii.5"
    ) |> 
    select(nome_uf, indicador, valor)
}

join_indicadores_pilar2 <- function(list_cnt, df_ii5, df_uf) {
  list_cnt |> 
    reduce(bind_rows) |> 
    bind_rows(df_ii5) |> 
    left_join(df_uf, by = "nome_uf") |> 
    select(cod_uf, nome_uf, indicador, valor)
}