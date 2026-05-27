download_renainf_2023 <- function(url) {
  data_urls <-
    read_html(url) |>
    html_nodes(".internal-link") |>
    html_attr("href")

  csv_urls <- grep(".csv", data_urls, value = TRUE)

  csv_2023_urls <- grep("2023", csv_urls, value = TRUE)

  ago_url <- "https://www.gov.br/transportes/pt-br/assuntos/transito/arquivos-senatran/estatisticas/renainf/csv/2023_08_infracoes_com_np.csv"

  csv_2023_urls[12] <- ago_url

  suppressMessages(
    raw_infracoes <-
      map(csv_2023_urls, read_csv2) |>
      reduce(bind_rows)
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