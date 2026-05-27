create_uf <- function(df_municipios) {
  tibble(
    cod_uf = as.character(unique(df_municipios$UF)),
    nome_uf = unique(df_municipios$Nome_UF),
    sigla_uf = c(
      "RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI", "CE", 
      "RN", "PB", "PE", "AL", "SE", "BA", "MG", "ES", "RJ", "SP", 
      "PR", "SC", "RS", "MS", "MT", "GO", "DF"
    )
  ) |> 
    mutate(
      nome_uf_upper = toupper(nome_uf),
      nome_uf_sem_acento = stri_trans_general(nome_uf_upper, "Latin-ASCII"),
      regiao_uf = case_match(
        sigla_uf,
        c("RO", "AC", "AM", "RR", "PA", "AP", "TO") ~ "Norte",
        c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA") ~ "Nordeste",
        c("MG", "ES", "RJ", "SP") ~ "Sudeste",
        c("PR", "SC", "RS") ~ "Sul",
        c("MS", "MT", "GO", "DF") ~ "Centro-Oeste"
      )
    )
}