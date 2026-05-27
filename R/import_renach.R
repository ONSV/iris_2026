arrange_renach <- function(pkg_df) {
  pkg_df |> 
    drop_na() |> 
    filter(ano > 2018, categoria_cnh %in% c("categoria_a", "categoria_ab")) |> 
    group_by(uf, ano, categoria_cnh) |> 
    summarise(n_condutores = sum(condutores))
}