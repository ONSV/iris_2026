# OS ARQUIVOS ORIGINAIS DO RENAEST SAO MUITO GRANDES PARA DEIXAR NO REPOSITORIO
# O SCRIPT data-raw/renaest/raw_renaest.R é responsável por importar,
# filtrar para o ano de 2023 e salvar no formato .parquet
arrange_renaest <- function(df) {
  df |> 
    mutate(
      data_acidente = as.character(data_acidente),
      across(
        -uf_acidente,
         \(x) if_else(
          is.na(x) | x == "NAO INFORMADO" | x == "00000000" |
            x == "999999" | x == "00000",
          1,
          0
        )
      ), 
      na_count = rowSums(across(where(is.numeric))),
      total_count = ncol(df)
    ) |> 
    group_by(uf_acidente) |> 
    summarise(na_count = sum(na_count), total_count = sum(total_count)) |> 
    ungroup()
}

join_renaest <- function(df_list, arrange_func) {
  map(df_list, arrange_func) |> 
    reduce(bind_rows) |> 
    group_by(uf_acidente) |> 
    summarise(na_count = sum(na_count), total_count = sum(total_count)) |> 
    ungroup()
}