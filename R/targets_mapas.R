targets_mapas <- list(
    tar_target(
        list_map_indicadores,
        map(
            unique(indicadores$indicador), 
            plot_indicadores_map, 
            sf_estados = sf_uf, 
            df_indicadores = indicadores
        )
    ),
    tar_target(
        map_indicadores, join_indicadores_map(list_map_indicadores)
    ),
    tar_target(
        list_map_classificacao,
        map(
            .x = unique(classificacao$pilar), 
            .f = plot_classificacao_map, 
            sf_estados = sf_uf,
            df_classificacao = classificacao
        )
    ),
    tar_target(map_media_classificacao, plot_media_map(classificacao, sf_uf))
)