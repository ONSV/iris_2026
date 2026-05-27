import_datasus_sim <- function(pkg_df) {
  pkg_df |> 
    mutate(cod_uf = str_sub(cod_municipio_ocor, 1, 2)) |> 
    filter(ano_ocorrencia == 2023) |> 
    select(cod_uf, nome_uf_ocor) |> 
    count(cod_uf, nome_uf_ocor, name = "obitos") |> 
    drop_na()
}