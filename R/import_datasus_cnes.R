extract_cnes_path <- function(pattern) {
  list.files(
    "data-raw/datasus_cnes",
    pattern = pattern,
    full.names = TRUE
  )
}

read_cnes_files <- function(paths) {
  read_csv2(
    paths,
    locale = locale(encoding = "latin1"), 
    skip = 3, 
    n_max = 27
  )
}

arrange_cnes_df <- function(df) {
  clean_df <- df |> 
    clean_names() |> 
    separate(
      unidade_da_federacao,
      into = c("cod_uf", "nome_uf"), 
      sep = " ",
      extra = "merge"
    )
  
  clean_df$ano <- c(rep(2022, 27), rep(2023, 27))
  return(clean_df)
}

join_cnes_df <- function(clean_list) {
  clean_list |> 
    reduce(left_join, by = c("cod_uf", "nome_uf", "ano")) |> 
    rename(
      n_leitos_nao_sus = quantidade_nao_sus,
      n_leitos_sus = quantidade_sus,
      n_leitos = quantidade_existente,
      n_profissionais = quantidade
    ) |> 
    select(cod_uf, nome_uf, ano, starts_with("n"))
}
