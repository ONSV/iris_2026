clean_ibge_pib_df <- function(raw_df) {
  raw_df |> 
    pivot_longer(-...1, names_to = "ano", values_to = "pib") |> 
    rename(nome_uf = ...1)
}