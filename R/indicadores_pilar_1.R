calc_i1 <- function(df_snt) {
  df_snt |> 
    mutate(
      valor = n_integrados / n_municipios,
      indicador = "i.1"
    ) |> 
    select(nome_uf, indicador, valor) |> 
    add_row(nome_uf = "DISTRITO FEDERAL", indicador = "i.1", valor = NA)
}

calc_i2 <- function(df_renaest) {
  df_renaest |> 
    mutate(valor = na_count / total_count, indicador = "i.2") |> 
    select(sigla_uf = uf_acidente, valor, indicador)
}

arrange_i2 <- function(raw_i2, uf_df) {
  uf_df |> 
    left_join(raw_i2, by = "sigla_uf") |> 
    replace_na(list(valor = 1, indicador = "i.2")) |> 
    select(cod_uf, nome_uf, indicador, valor)
}

calc_i3 <- function(pib_df, pop_df) {
  pib_df |>  
    left_join(pop_df, by = c("nome_uf", "ano")) |> 
    filter(ano == 2023) |> 
    mutate(valor = pib / populacao, indicador = "i.3") |> 
    drop_na() |> 
    select(cod_uf, nome_uf, indicador, valor)
}

arrange_i4 <- function(df_detrans) {
  df_detrans |> 
    rename(valor = nota_media) |> 
    mutate(indicador = "i.4")
}

join_indicadores_pilar1 <- function(df_i1, df_i2, df_i3, df_i4, df_uf) {
  fixed_i1 <- df_i1 |> 
    left_join(df_uf, by = c("nome_uf" = "nome_uf_upper")) |> 
    select(cod_uf, nome_uf = nome_uf.y, indicador, valor)
  
  fixed_i4 <- df_i4 |> 
    left_join(df_uf, by = "sigla_uf") |> 
    select(cod_uf, nome_uf, indicador, valor)
  
  bind_rows(fixed_i1, df_i2, df_i3, fixed_i4)
}