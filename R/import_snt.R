read_snt <- function(url_snt) {
  read_html(url_snt) |> 
    html_node("table") |> 
    html_table(header = TRUE) |> 
    clean_names() |> 
    rename(n_integrados = no_de_municipios_integrados, nome_uf = estado) |> 
    filter(nome_uf != "BRASIL") |> 
    mutate(n_integrados = as.numeric(n_integrados))
}

arrange_municipios <- function(raw_df) {
  raw_df |> 
    clean_names() |> 
    count(nome_uf, name = "n_municipios") |> 
    filter(nome_uf != "Distrito Federal") |> 
    mutate(nome_uf = toupper(nome_uf))
}