download_renainf_2024 <- function(url) {
    data_urls <-
        read_html(url) |>
        html_nodes(".internal-link") |>
        html_attr("href")
    
    csv_urls <- grep(".csv", data_urls, value = TRUE)
    
    csv_2024_urls <- grep("2024", csv_urls, value = TRUE)
    
    suppressMessages(
        raw_infracoes <-
            map(csv_2024_urls, \(link) read_csv(link, locale = locale(encoding = "UTF-16LE"))) |>
            reduce(bind_rows) |> 
            pivot_longer(
                cols = -1, 
                names_to = "UF",
                values_to = "Quantidade"
            ) |> filter(`Quantidade` != "Qt Infração c/ NP") |>
            rename(Cod_Infração = `UF Jurisdição Veículo (Desc)`) |> 
            relocate(`UF`, .before = `Cod_Infração`) |>
            mutate(
                `Cod_Infração` = as.double(`Cod_Infração`),
                `Quantidade` = as.numeric(str_remove_all(`Quantidade`, "\\.")),
                `Cod _Infração` = NA,
                Cod_Infracao = NA
            ) 
    )
    return(raw_infracoes)
}

arrange_renainf <- function(raw_df) {

  list_cod_inf <- c(
    "5169", "5185", "5193", "6262", "6270", "6289", "6297", "6300", "6319",
    "6327", "6335", "6343", "6351", "6360", "6378", "6386", "6394", "7030",
    "7048", "7072", "7137", "7366", "7455", "7463", "7471"
  )

  infracoes <- raw_df |>
    clean_names() |>
    mutate(
      cod_infracao = case_when(
        is.na(cod_infracao) & is.na(cod_infracao_2) ~ cod_infracao_3,
        is.na(cod_infracao) & is.na(cod_infracao_3) ~ cod_infracao_2,
        TRUE ~ cod_infracao
      )
    ) |>
    mutate(cod_infracao = as.character(cod_infracao)) |>
    select(uf, cod_infracao, quantidade) |>
    drop_na()

  infracoes_df <- infracoes |>
    group_by(uf, cod_infracao) |>
    summarise(quantidade = sum(quantidade)) |>
    ungroup() |> 
    filter(cod_infracao %in% list_cod_inf)

  return(infracoes_df)
}