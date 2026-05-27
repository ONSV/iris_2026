calc_iii1 <- function(df_renavam) {
  df_renavam |> 
    filter(ano == 2023) |> 
    select(-ano) |> 
    pivot_wider(names_from = modal, values_from = frota) |> 
    mutate(
      moto = MOTOCICLETA + MOTONETA,
      frota_desconsiderada = BONDE + `CHASSI PLATAF` + REBOQUE +
        `SEMI-REBOQUE` + `SIDE-CAR` + `TRATOR ESTEI` + `TRATOR RODAS`,
      frota_reduzida = rowSums(across(AUTOMOVEL:UTILITARIO)) - 
        frota_desconsiderada,
      valor = moto / frota_reduzida,
      indicador = "iii.1"
    ) |> 
    select(sigla_uf = uf, indicador, valor)
}

calc_iii2 <- function(df_renavam_idade) {
  df_renavam_idade |> 
    filter(
      !uf %in% c("Não se Aplica", "Sem Informação"),
      !ano_modelo %in% c("Não se Aplica", "Não Identificado")
    ) |> 
    mutate(
      idade = 2024 - as.numeric(ano_modelo),
      acima_10_anos = idade >= 10
    ) |> 
    group_by(uf, acima_10_anos) |> 
    summarise(n_veiculos = sum(n_veiculos)) |> 
    pivot_wider(names_from = acima_10_anos, values_from = n_veiculos) |> 
    mutate(valor = `TRUE` / (`FALSE` + `TRUE`), indicador = "iii.2") |> 
    select(nome_uf = uf, indicador, valor)
}

calc_iii3 <- function(df_renavam) {
  df_renavam |> 
    filter(modal %in% c("AUTOMOVEL", "CAMINHONETE")) |> 
    group_by(uf, ano) |> 
    summarise(frota_total = sum(frota)) |> 
    mutate(
      adesao = case_match(
        ano,
        2009 ~ 0,
        2010 ~ 0.08,
        2011 ~ 0.15,
        2012 ~ 0.30,
        2013 ~ 0.60,
        .default = 1
      ),
      novos = if_else(ano == 2009, 0, frota_total - lag(frota_total)),
      novos_equipados = novos * adesao,
      novos_equipados_acum = cumsum(novos_equipados),
      valor = novos_equipados_acum / frota_total,
      indicador = "iii.3"
    ) |> 
    filter(ano == 2023) |> 
    ungroup() |> 
    select(sigla_uf = uf, indicador, valor)
}

calc_iii4 <- function(df_renavam) {
  df_renavam |> 
    filter(
      ano > 2018,
      modal %in% c("AUTOMOVEL", "CAMINHONETE", "UTILITARIO", "CAMIONETA")
    ) |> 
    group_by(uf, ano) |> 
    summarise(frota_total = sum(frota)) |> 
    mutate(
      novos = if_else(ano == 2019, 0, frota_total - lag(frota_total)),
      novos_acum = cumsum(novos),
      valor = novos_acum / frota_total,
      indicador = "iii.4"
    ) |> 
    filter(ano == 2023) |> 
    ungroup() |> 
    select(sigla_uf = uf, indicador, valor)
}


join_indicadores_pilar3 <- function(df_iii1, df_iii2, df_iii3, df_iii4, df_uf) {
  clean_indicadores <- 
    bind_rows(df_iii1, df_iii3, df_iii4) |> 
    left_join(df_uf, by = "sigla_uf") |> 
    select(cod_uf, nome_uf, indicador, valor)
  
  fixed_iii2 <- df_uf |> 
    mutate(
      fixed_nome = stringi::stri_trans_general(nome_uf_upper, "Latin-ASCII")
    ) |> 
    right_join(df_iii2, by = c("fixed_nome" = "nome_uf")) |> 
    select(cod_uf, nome_uf, indicador, valor)
  
  bind_rows(clean_indicadores, fixed_iii2)
}