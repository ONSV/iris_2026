create_table_list <- function() {
  list(
    ano_2023 = list(
      estado_geral_2023 = "TABELA 72",
      pavimento_2023 = "TABELA 73",
      sinalizacao_2023 = "TABELA 74",
      geometria_2023 = "TABELA 75"
    ),
    ano_2024 = list(
      estado_geral_2024 = "TABELA 72",
      pavimento_2024 = "TABELA 73",
      sinalizacao_2024 = "TABELA 74",
      geometria_2024 = "TABELA 75"
    )
  )
}

get_raw_table <- function(raw_text, page_number) {
  page_split <- strsplit(raw_text[page_number], "\n", )[[1]]
  tbl_inicio <- grep(pattern = "Ótimo", x = page_split)
  tbl_fim <- grep(pattern = "Distrito Federal", x = page_split)
  data_vec <- page_split[tbl_inicio:tbl_fim]
  data_vec[1] <- paste("uf", data_vec[1])
  return(data_vec)
}

cnt_transform_to_df <- function(raw_text) {
  matrix <- str_split(
    str_trim(raw_text[-1]),
    "\\s{2,}",
    simplify = TRUE
  )
  df <- as_tibble(matrix)
  colnames(df) <- 
    c("uf", "otimo", "bom", "regular", "ruim", "pessimo", "total")
  return(df)
}

arrange_cnt_df <- function(raw_df, ano_input) {
  raw_df |> 
    mutate(
      across(-uf, \(x) gsub("\\.", "", x)),
      across(everything(), \(x) ifelse(x == "-", 0, x)),
      across(-uf, \(x) as.numeric(x)),
      ano = ano_input
    ) |> 
    filter(
      !uf %in% c(
        "Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste"
      )
    )
}