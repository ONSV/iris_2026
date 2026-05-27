filter_frota_motos <- function(df_renavam) {
    df_motos <- df_renavam |>
        filter(ano == 2023, modal %in% c("MOTOCICLETA", "MOTONETA")) |>
        group_by(uf) |>
        summarise(frota = sum(frota))
    return(df_motos)   
}

join_iv1 <- function(df_renach, df_motos) {
    df_condutores <- df_renach |>
        filter(ano == 2023) |>
        group_by(uf) |>
        summarise(condutores = sum(n_condutores))
    
    left_join(df_condutores, df_motos, by = "uf")
}

calc_iv1 <- function(df_joined) {
    df_joined |> 
        mutate(
            valor = condutores / frota * 100,
            indicador = "iv.1"
        ) |>
        select(sigla_uf = uf, indicador, valor)
}

calc_frota_reduzida <- function(df_renavam) {
    df_renavam |> 
        filter(ano == 2023) |> 
        select(-ano) |> 
        pivot_wider(names_from = modal, values_from = frota) |> 
        mutate(
            frota_desconsiderada = BONDE + `CHASSI PLATAF` + REBOQUE +
                `SEMI-REBOQUE` + `SIDE-CAR` + `TRATOR ESTEI` + `TRATOR RODAS`,
            frota_reduzida = rowSums(across(AUTOMOVEL:UTILITARIO)) - 
                frota_desconsiderada
        ) |> 
        select(uf, frota_reduzida)
}

join_iv2 <- function(df_uf, df_renainf, df_frota_reduzida) {
    df_renainf |> 
        filter(cod_infracao == 5169) |> 
        right_join(df_uf, by = c("uf" = "nome_uf_sem_acento")) |> 
        left_join(df_frota_reduzida, by = c("sigla_uf" = "uf"))
}

calc_iv2 <- function(df_joined) {
    df_joined |> 
        mutate(
            valor = quantidade / frota_reduzida * 10000,
            indicador = "iv.2"
        ) |> 
        select(cod_uf, nome_uf, indicador, valor)
}

filter_infracoes_iv3 <- function(df_renainf) {
    infracoes_vel <- c(
        "6262", "6270", "6289", "6297", "6300", "6319", "6327",
        "6335", "6343", "6351", "6360", "6378", "6386", "6394", "7455",
        "7463", "7471"
    )
    
    infracoes_vel_df <- df_renainf |> 
        filter(cod_infracao %in% infracoes_vel) |> 
        group_by(uf) |> 
        summarise(infracoes = sum(quantidade))
    
    return(infracoes_vel_df)
}

join_iv3 <- function(df_frota_reduzida, df_uf, df_infracoes_iv3) {
    df_infracoes_iv3 |> 
        left_join(df_uf, by = c("uf" = "nome_uf_sem_acento")) |> 
        left_join(df_frota_reduzida, by = c("sigla_uf" = "uf"))
}

calc_iv3 <- function(df_joined) {
    df_joined |> 
        mutate(
            valor = infracoes / frota_reduzida * 10000,
            indicador = "iv.3"
        ) |> 
        select(cod_uf, nome_uf, indicador, valor)
}

join_iv4 <- function(df_renavam, df_renainf, df_uf) {
    df_renavam |> 
        filter(
            ano == 2023, 
            modal %in% c(
                "AUTOMOVEL", "CAMINHONETE", "CAMIONETA", "CAMINHAO",
                "CAMINHAO TRATOR", "UTILITARIO", "MICRO-ONIBUS", "ONIBUS"
            )
        ) |> 
        group_by(uf) |> 
        summarise(frota = sum(frota)) |> 
        left_join(df_uf, by = c("uf" = "sigla_uf")) |> 
        left_join(
            df_renainf |> filter(cod_infracao == "5185"),
            by = c("nome_uf_sem_acento" = "uf")
        )
}

calc_iv4 <- function(df_joined) {
    df_joined |> 
        mutate(
            valor = quantidade / frota * 10000,
            indicador = "iv.4"
        ) |> 
        select(cod_uf, nome_uf, indicador, valor)
}

filter_infracoes_iv5 <- function(df_renainf) {
    infracoes_capacete <- df_renainf |> 
        filter(cod_infracao %in% c("7030", "7048")) |> 
        group_by(uf) |> 
        summarise(infracoes = sum(quantidade))
    return(infracoes_capacete)
}

join_iv5 <- function(df_infracoes_iv5, df_frota_motos, df_uf) {
    df_infracoes_iv5 |> 
        left_join(df_uf, by = c("uf" = "nome_uf_sem_acento")) |> 
        left_join(df_frota_motos, by = c("sigla_uf" = "uf")) 
}

calc_iv5 <- function(df_joined) {
    df_joined |> 
        mutate(
            valor = infracoes / frota * 10000,
            indicador = "iv.5"
        ) |> 
        select(cod_uf, nome_uf, indicador, valor)
}

join_iv6 <- function(df_frota_reduzida, df_renainf, df_uf) {
    df_frota_reduzida |> 
        left_join(df_uf, by = c("uf" = "sigla_uf")) |> 
        left_join(
            df_renainf |> filter(cod_infracao == "7366"),
            by = c("nome_uf_sem_acento" = "uf")
        )
}

calc_iv6 <- function(df_joined) {
    df_joined |>     
        mutate(
            valor = quantidade / frota_reduzida * 10000,
            indicador = "iv.6"
        ) |> 
        select(cod_uf, nome_uf, indicador, valor)
}

join_iv7 <- function(df_renavam, df_renainf, df_uf) {
    df_renavam |> 
        filter(
            ano == 2023,
            modal %in% c("AUTOMOVEL", "CAMINHONETE", "CAMIONETA", "UTILITARIO")
        ) |> 
        group_by(uf) |> 
        summarise(frota = sum(frota)) |> 
        left_join(df_uf, by = c("uf" = "sigla_uf")) |> 
        left_join(
            df_renainf |> filter(cod_infracao == "5193"),
            by = c("nome_uf_sem_acento" = "uf")
        )
}

calc_iv7 <- function(df_joined) {
    df_joined |> 
        mutate(
            valor = quantidade / frota * 10000,
            indicador = "iv.7"
        ) |> 
        select(cod_uf, nome_uf, indicador, valor)
}

join_indicadores_pilar4 <- function(
    df_iv1, df_iv2, df_iv3, df_iv4, df_iv5, df_iv6, df_iv7, df_uf
) {
    df_iv1 |> 
        left_join(df_uf, by = "sigla_uf") |> 
        select(cod_uf, nome_uf, indicador, valor) |> 
        bind_rows(
            df_iv2,
            df_iv3,
            df_iv4,
            df_iv5,
            df_iv6,
            df_iv7,
        )
}

