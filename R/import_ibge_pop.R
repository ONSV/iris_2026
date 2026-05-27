arrange_ibge_pop <- function(raw_df) {
  raw_df |> 
    separate(
      `Unidade da Federação`,
      into = c("cod_uf", "nome_uf"), 
      sep = " ",
      extra = "merge"
    ) |> 
    pivot_longer(`2018`:`2023`, names_to = "ano", values_to = "populacao")
}

