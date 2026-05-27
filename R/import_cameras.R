arrange_cameras_df <- function(raw_df) {
  raw_df |> 
    clean_names() |> 
    filter(ultimo_resultado != "Reprovado", tipo_medidor == "Fixo") |> 
    group_by(sigla_uf, estado, tipo) |> 
    summarise(n_cameras = n()) |> 
    ungroup()
}

calc_pontos_fiscalizacao <- function(raw_cameras_df) {
    raw_cameras_df |> 
        janitor::clean_names() |> 
        filter(ultimo_resultado != "Reprovado", tipo_medidor == "Fixo") |> 
        distinct(local_verificacao, .keep_all = TRUE) |> 
        group_by(sigla_uf, estado, municipio, tipo) |> 
        summarise(pontos_fiscalizacao = n(), .groups = "drop")
}

calc_pontos_fisc_uf <- function(df_pontos_fiscalizacao) {
    df_pontos_fiscalizacao |> 
        group_by(sigla_uf, tipo) |> 
        summarise(
            pontos_fiscalizacao = sum(pontos_fiscalizacao),
            .groups = "drop"
        ) |> 
        drop_na() |> 
        pivot_wider(
            names_from = tipo,
            values_from = pontos_fiscalizacao,
            values_fill = 0
        )
}

calc_pontos_fisc_mun <- function(df_pontos_fiscalizacao, df_osm) {
    
    capital_df <- df_osm |> 
        mutate(
            nome_mun = str_to_title(nome_mun),
            nome_uf_mun = paste0(nome_uf, " - ", nome_mun)
        )
    
    df_pontos <- df_pontos_fiscalizacao |>
        mutate(
            municipio = str_to_title(municipio),
            nome_uf_mun = paste0(estado, " - ", municipio)
        ) |> 
        group_by(nome_uf_mun) |> 
        summarise(
            pontos_fiscalizacao = sum(pontos_fiscalizacao),
            .groups = "drop"
        )
    
    capital_df |> 
        left_join(df_pontos, by = "nome_uf_mun") |> 
        select(nome_uf, nome_mun, pontos_fiscalizacao)
}

