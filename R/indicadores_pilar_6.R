calc_frota_total_uf <- function(df_renavam) {
    df_renavam |> 
        filter(ano == 2023) |> 
        group_by(uf) |> 
        summarise(frota_total = sum(frota))
}

calc_inf_total <- function(df_renainf) {
    df_infracoes <- df_renainf |> 
        group_by(uf) |> 
        summarise(infracoes = sum(quantidade))
    
    return(df_infracoes)
}

join_vi1 <- function(df_frota_total, df_infracoes, df_uf) {
    df_infracoes |> 
        left_join(df_uf, by = c("uf" = "nome_uf_sem_acento")) |> 
        left_join(df_frota_total, by = c("sigla_uf" = "uf"))
}

calc_vi1 <- function(df_joined) {
    df_joined |> 
        mutate(
            valor = infracoes / frota_total * 10000,
            indicador = "vi.1"
        ) |> 
        select(cod_uf, nome_uf, indicador, valor)
}

arrange_cameras <- function(df_cameras) {
    df_cameras |> 
        select(-estado) |> 
        drop_na() |> 
        pivot_wider(
            names_from = "tipo", 
            values_from = n_cameras, 
            names_prefix = "cameras_",
            values_fill = 0
        ) |> 
        mutate(cameras_total = cameras_Rodovia + `cameras_Via Urbana`)
}

join_cameras_frota <- function(df_frota, df_cameras_arranged) {
    df_cameras_arranged |> 
        left_join(df_frota, by = c("sigla_uf" = "uf"))
}

calc_cameras_frota <- function(df_joined) {
   df_joined |> 
        mutate(
            vi.2 = cameras_total / frota_total * 10000,
            vi.3 = cameras_Rodovia / frota_total * 10000,
            vi.4 = `cameras_Via Urbana` / frota_total * 10000
        ) |> 
        select(sigla_uf, vi.2:vi.4) |> 
        pivot_longer(-sigla_uf, values_to = "valor", names_to = "indicador")
}

calc_extensao <- function(df_dnit) {
    df_extensao <- df_dnit |> 
        filter(ano == 2023) |> 
        select(nome_uf, total_pavimentado)
    
    return(df_extensao)
}

join_vi5 <- function(df_extensao, df_cameras) {
    df_radares <- df_cameras |> 
        filter(tipo == "Rodovia") |> 
        select(nome_uf = estado, n_cameras)
    
    df_extensao |> 
        left_join(df_radares, by = "nome_uf")
}

calc_vi5 <- function(df_joined) {
    df_joined |> 
        mutate(
            valor = n_cameras / total_pavimentado * 100,
            indicador = "vi.5"
        ) |> 
        select(nome_uf, indicador, valor)
}

arrange_indicadores_cameras <- function(df_uf, df_ind_cameras) {
    df_uf |> 
        left_join(df_ind_cameras, by = "sigla_uf") |> 
        select(cod_uf, nome_uf, indicador, valor) |> 
        replace_na(list(indicador = "vi.4", valor = 0))
    
}

calc_total_cameras <- function(df_cameras){
    cameras_uf <- df_cameras |> 
        group_by(sigla_uf) |> 
        summarise(n_cameras = sum(n_cameras))
    
    return(cameras_uf)
}

calc_infracoes_vi6 <- function(df_renainf) {
    df_infracoes <- df_renainf |> 
        filter(cod_infracao %in% c("7455", "7463", "7471")) |> 
        group_by(uf) |> 
        summarise(n_infracoes = sum(quantidade))
    
    return(df_infracoes)
}

join_vi6 <- function(df_total_cameras, df_uf, df_infracoes_vi6) {
    df_total_cameras |> 
        left_join(df_uf, by = "sigla_uf") |> 
        left_join(df_infracoes_vi6, by = c("nome_uf_sem_acento" = "uf"))
}

calc_vi6 <- function(df_joined) {
    df_joined |> 
        mutate(valor = n_infracoes / n_cameras, indicador = "vi.6") |> 
        select(cod_uf, nome_uf, indicador, valor)
}

join_indicadores_pilar6 <- function(
    df_vi1, df_vi5, df_vi6, df_vi7, df_uf, df_cameras
) {
    df_results <- df_vi5 |> 
        left_join(df_uf, by = "nome_uf") |> 
        select(cod_uf, nome_uf, indicador, valor) |> 
        bind_rows(
            df_cameras,
            df_vi1,
            df_vi6,
            df_vi7
        )
    
    return(df_results)
}

count_cameras_cidades <- function(df_cameras_raw) {
    df_cameras_raw |> 
        janitor::clean_names() |> 
        filter(ultimo_resultado != "Reprovado", tipo_medidor == "Fixo") |> 
        select(estado, municipio) |> 
        count(estado, municipio)
}

join_vi7 <- function(df_osm, df_cameras_cidades) {
    df_osm |> 
        mutate(nome_mun = toupper(nome_mun)) |> 
        left_join(
            df_cameras_cidades, 
            by = c("nome_mun" = "municipio", "nome_uf" = "estado")
        ) |> 
        mutate(nome_mun = str_to_title(nome_mun)) |> 
        select(nome_uf, nome_mun, dist_vias, cameras = n)
}

calc_vi7 <- function(df_joined_vi7, df_uf) {
    df_joined_vi7 |> 
        mutate(
            valor = cameras / (units::drop_units(dist_vias) / 1000) * 100,
            indicador = "vi.7"
        ) |> 
        left_join(df_uf, by = "nome_uf") |> 
        select(cod_uf, nome_uf, indicador, valor)
}

join_pontos_fisc <- function(df_pontos_fisc_uf, df_renavam_total_uf) {
    df_pontos_fisc_uf |> 
        mutate(total_pontos = Rodovia + `Via Urbana`) |>
        left_join(df_renavam_total_uf, by = c("sigla_uf" = "uf"))
}

calc_indicadores_pontos <- function(df_joined_pontos_fisc_uf) {
    df_joined_pontos_fisc_uf |> 
        mutate(
            vi.08 = total_pontos / frota_total * 10000,
            vi.09 = Rodovia / frota_total * 10000,
            vi.10 = `Via Urbana` / frota_total * 10000
        ) |> 
        select(sigla_uf, vi.08:vi.10) |> 
        pivot_longer(-sigla_uf, values_to = "valor", names_to = "indicador")
}

join_vi11 <- function(df_pontos_fisc_uf, df_dnit) {
    df_pontos_fisc_uf |> 
        left_join(df_dnit |> filter(ano == 2023), by = "sigla_uf") |> 
        select(sigla_uf, pontos_fisc = Rodovia, total_pavimentado)
}

calc_vi11 <- function(df_joined) {
    df_joined |> 
        mutate(
            valor = pontos_fisc / total_pavimentado * 100,
            indicador = "vi.11"
        ) |> 
        select(sigla_uf, indicador, valor)
}

join_vi12 <- function(df_pontos_fisc_mun, df_osm) {
    df_pontos_fisc_mun |> 
        select(nome_mun, pontos_fiscalizacao) |>
        left_join(
            df_osm |> mutate(nome_mun = str_to_title(nome_mun)),
            by = "nome_mun"
        ) |> 
        select(nome_uf, nome_mun, pontos_fiscalizacao, dist_vias)
}

calc_vi12 <- function(df_joined) {
    df_joined |> 
        mutate(
            valor = pontos_fiscalizacao /
                (units::drop_units(dist_vias) / 1000) * 100,
            indicador = "vi.12"
        ) |> 
        select(nome_uf, indicador, valor)
}



