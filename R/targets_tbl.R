targets_tbl <- list(
    # Descrição
    tar_target(tbl_ind_desc, make_desc_ind_tbl()),
    tar_target(tbl_ind_class, make_tbl_ind_class()),
    tar_target(
        list_tbl_desc,
        map(
            c("I.", "II.", "III.", "IV.", "V.", "VI.", "0."),
            make_gt_desc,
            tbl_desc = tbl_ind_desc
        )
    ),
    tar_target(tbl_fontes, make_fontes_tbl()),
    # Fontes de dados
    tar_target(
        list_tbl_cnt,
        map(cnt$tabelas_2023, make_gt_cnt)
    ),
    # Indicadores
    tar_target(tbl_codinf, make_codinf_tbl()),
    tar_target(
        list_tbl_pilar2,
        map2(
            cnt$tabelas_2023,
            paste0("Indicador ", c("II.1", "II.2", "II.3", "II.4")),
            make_gt_pilar2
        )
    ),
    tar_target(
        pilarv_var, 
        c("n_profissionais", "n_leitos", "n_leitos_sus", "n_leitos_nao_sus")
    ),
    tar_target(
        list_tbl_pilar5,
        map(pilarv_var, make_pilar5_tbl, df_joined = joined_v)
    ),
    tar_target(
        list_gt_pilar5,
        pmap(
            list(
                list_tbl_pilar5,
                c("Profissionais", "Leitos", "Leitos SUS", "Leitos não-SUS"),
                paste0("Indicador ", c("V.1", "V.2", "V.3", "V.4"))
            ),
            make_pilar5_gt
        )
    ),
    tar_target(
        list_tbl_cameras,
        map(
            c("cameras_total", "cameras_Rodovia", "cameras_Via Urbana"),
            make_pilar6_cameras_tbl,
            df_joined = joined_cameras
        )
    ),
    tar_target(
        list_gt_cameras,
        pmap(
            list(
                list_tbl_cameras,
                c(
                    "Total de câmeras",
                    "Câmeras em rodovias",
                    "Câmeras em vias urbanas"
                ),
                paste0("Indicador ", c("VI.2", "VI.3", "VI.4"))
            ),
            make_pilar6_cameras_gt
        )
    ),
    # Resultados
    tar_target(
        tbl_indicadores,
        map(
            c("i.", "ii.", "iii.", "iv.", "v.", "vi.", "0."),
            make_tbl_indicadores,
            df_indicadores = indicadores
        )
    ),
    tar_target(gt_resultados, map(tbl_indicadores, make_gt_indicadores)),
    tar_target(eda, make_eda_df(indicadores)),
    tar_target(
        list_gt_eda,
        map(
            c("i.", "ii.", "iii.", "iv.", "v.", "vi.", "0."),
            make_eda_gt,
            eda_df = eda
        )
    ),
    # Resultados PCA
    tar_target(
        list_pca_stats,
        map(list_pca_results, arrange_pca_stats)
    ),
    tar_target(
        list_pca_numeric_results,
        map(
            list_pca_results,
            arrange_pca_results,
            df_indicadores_wide = indicadores_wide
        )
    ),
    tar_target(list_pca_stats_gt, map(list_pca_stats, make_pca_stats_gt)),
    tar_target(
        list_pca_results_gt,
        map(list_pca_numeric_results, make_pca_results_gt)
    ),
    # Resultados valores invertidos
    tar_target(
        tbl_invertidos, 
        make_tbl_invertidos(indicadores_scaled, indicadores_scaled_inverted)
    ),
    tar_target(
        gt_invertidos,
        map(
            c("i.", "ii.", "iii.", "iv.", "vi.", "0."),
            make_gt_invertidos,
            df_tbl_invertido = tbl_invertidos
        )
    ),
    # Resultados indicadores compostos
    tar_target(
        list_gt_compostos,
        map(
            unique(indicadores_compostos$pilar),
            make_gt_compostos,
            df_indicadores_compostos = indicadores_compostos,
            df_indicadores_notas = indicadores_notas
        )
    ),
    # Resultados classificacao
    tar_target(
        list_gt_classificacao,
        map(
            unique(classificacao$pilar),
            make_gt_classificacao,
            df_classificacao = classificacao
        )
    )
    # Dashboard
    # tar_target(
    #     list_gt_dashboard_ind,
    #     map(
    #         unique(indicadores$indicador),
    #         gt_dashboard_indicadores,
    #         df_indicadores = indicadores
    #     )
    # ),
    # tar_target(
    #     list_gt_dashboard_class,
    #     map(
    #         unique(classificacao$pilar),
    #         gt_dashboard_classificacao,
    #         df_classificacao = classificacao,
    #         df_uf = uf
    #     )
    # )
)