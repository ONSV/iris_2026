read_dnit_df <- function(path_list) {
  readxl::read_excel(
    path_list,
    sheet = 2,
    skip = 4,
    n_max = 31
  )
}

arrange_dnit_df <- function(raw_df, ano_input) {
  raw_df |> 
    clean_names() |> 
    select(sigla_uf = x2, nome_uf = x3, pista_simples, pista_dupla) |> 
    filter(sigla_uf != "Sub-Total") |> 
    mutate(
      total_pavimentado = pista_simples + pista_dupla, ano = ano_input,
      nome_uf = if_else(nome_uf == "Espirito Santo", "Espírito Santo", nome_uf)
    )
}