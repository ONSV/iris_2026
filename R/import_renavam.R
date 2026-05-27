read_renavam_2011_2023 <- function(pkg_df) {
  pkg_df |> 
    filter(mes == 12, modal != "TOTAL") |> 
    select(-mes)
}

arrange_old_renavam <- function(df_old_renavam, ano_input) {
  df_old_renavam |> 
    rename(nome_uf = `Grandes Regiões e\nUnidades da Federação`) |> 
    filter(!nome_uf %in% c(
      "Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste"
    )) |> 
    select(-TOTAL, -nome_uf) |> 
    mutate(uf = c(
      "AC", "AP", "AM", "PA", "RO", "RR", "TO", 
      "AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE", 
      "ES", "MG", "RJ", "SP", "PR", "RS", "SC", 
      "DF", "GO", "MT", "MS"
    )) |> 
    pivot_longer(
      -uf,
      names_to = "modal",
      values_to = "frota"
    ) |> 
    mutate(
      modal = stri_trans_general(modal, "Latin-ASCII"),
      modal = case_match(
        modal,
        "CHASSI PLATAFORMA" ~ "CHASSI PLATAF",
        "MICROONIBUS" ~ "MICRO-ONIBUS",
        "TRATOR ESTEIRA" ~ "TRATOR ESTEI",
        .default = modal
      ),
      ano = ano_input
    )
}

arrange_renavam_idade <- function(df) {
  df |> 
    clean_names() |> 
    group_by(uf, ano_modelo) |> 
    summarise(n_veiculos = sum(qtd_veiculos)) |>
    filter(ano_modelo != "Sem Informação") |> 
    ungroup()
}