targets_indicadores <- list(
    # indicadores_pilar_1.R
    tar_target(indicador_i1, calc_i1(snt)),
    tar_target(raw_indicador_i2, calc_i2(renaest)),
    tar_target(indicador_i2, arrange_i2(raw_indicador_i2, uf)),
    tar_target(indicador_i3, calc_i3(ibge_pib, ibge_pop)),
    tar_target(indicador_i4, arrange_i4(detrans)),
    tar_target(
        indicadores_pilar1, 
        join_indicadores_pilar1(
            indicador_i1,
            indicador_i2,
            indicador_i3,
            indicador_i4,
            uf
        )
    ),
    # indicadores_pilar_2.R
    tar_target(
        list_indicadores_cnt,
        map2(
            cnt$tabelas_2024, 
            c("ii.1", "ii.3", "ii.4", "ii.2"), 
            calc_indicadores_cnt
        )
    ),
    tar_target(indicador_ii5, calc_ii5(dnit)),
    tar_target(
        indicadores_pilar2, 
        join_indicadores_pilar2(list_indicadores_cnt, indicador_ii5, uf)
    ),
    # indicadores_pilar_3.R
    tar_target(indicador_iii1, calc_iii1(renavam)),
    tar_target(indicador_iii2, calc_iii2(renavam_idade)),
    tar_target(indicador_iii3, calc_iii3(renavam)),
    tar_target(indicador_iii4, calc_iii4(renavam)),
    tar_target(
        indicadores_pilar3,
        join_indicadores_pilar3(
            indicador_iii1,
            indicador_iii2, 
            indicador_iii3,
            indicador_iii4, 
            uf
        )
    ),
    # indicadores_pilar_4.R
    tar_target(frota_motos, filter_frota_motos(renavam)),
    tar_target(joined_iv1, join_iv1(renach, frota_motos)),
    tar_target(indicador_iv1, calc_iv1(joined_iv1)),
    tar_target(frota_reduzida, calc_frota_reduzida(renavam)),
    tar_target(joined_iv2, join_iv2(uf, renainf, frota_reduzida)),
    tar_target(indicador_iv2, calc_iv2(joined_iv2)),
    tar_target(infracoes_iv3, filter_infracoes_iv3(renainf)),
    tar_target(joined_iv3, join_iv3(frota_reduzida, uf, infracoes_iv3)),
    tar_target(indicador_iv3, calc_iv3(joined_iv3)),
    tar_target(joined_iv4, join_iv4(renavam, renainf, uf)),
    tar_target(indicador_iv4, calc_iv4(joined_iv4)),
    tar_target(infracoes_iv5, filter_infracoes_iv5(renainf)),
    tar_target(joined_iv5, join_iv5(infracoes_iv5, frota_motos, uf)),
    tar_target(indicador_iv5, calc_iv5(joined_iv5)),
    tar_target(joined_iv6, join_iv6(frota_reduzida, renainf, uf)),
    tar_target(indicador_iv6, calc_iv6(joined_iv6)),
    tar_target(joined_iv7, join_iv7(renavam, renainf, uf)),
    tar_target(indicador_iv7, calc_iv7(joined_iv7)),
    tar_target(
        indicadores_pilar4,
        join_indicadores_pilar4(
            indicador_iv1,
            indicador_iv2,
            indicador_iv3,
            indicador_iv4,
            indicador_iv5,
            indicador_iv6,
            indicador_iv7,
            uf
        )
    ),
    # indicadores_pilar_5.R
    tar_target(joined_v, join_v(datasus_cnes, ibge_pop)),
    tar_target(indicadores_pilar5, calc_indicadores_pilar5(joined_v)),
    # indicadores_pilar_6.R
    tar_target(renavam_total_uf, calc_frota_total_uf(renavam)),
    tar_target(infracoes_total, calc_inf_total(renainf)),
    tar_target(joined_vi1, join_vi1(renavam_total_uf, infracoes_total, uf)),
    tar_target(indicador_vi1, calc_vi1(joined_vi1)),
    tar_target(arranged_cameras, arrange_cameras(cameras)),
    tar_target(
        joined_cameras, 
        join_cameras_frota(renavam_total_uf, arranged_cameras)
    ),
    tar_target(indicadores_cameras, calc_cameras_frota(joined_cameras)),
    tar_target(extensao_rodovia, calc_extensao(dnit)),
    tar_target(joined_vi5, join_vi5(extensao_rodovia, cameras)),
    tar_target(indicador_vi5, calc_vi5(joined_vi5)),
    tar_target(
        indicadores_cameras_fixed,
        arrange_indicadores_cameras(uf, indicadores_cameras)
    ),
    tar_target(total_cameras, calc_total_cameras(cameras)),
    tar_target(infracoes_vi6, calc_infracoes_vi6(renainf)),
    tar_target(joined_vi6, join_vi6(total_cameras, uf, infracoes_vi6)),
    tar_target(indicador_vi6, calc_vi6(joined_vi6)),
    tar_target(cameras_cidades, count_cameras_cidades(df_cameras)),
    tar_target(joined_vi7, join_vi7(osm, cameras_cidades)),
    tar_target(indicador_vi7, calc_vi7(joined_vi7, uf)),
    # tar_target(
    #     joined_pontos_fisc,
    #     join_pontos_fisc(pontos_fisc_uf, renavam_total_uf)
    # ),
    # tar_target(
    #     indicadores_pontos_fisc,
    #     calc_indicadores_pontos(joined_pontos_fisc)
    # ),
    # tar_target(joined_vi11, join_vi11(pontos_fisc_uf, dnit)),
    # tar_target(indicador_vi11, calc_vi11(joined_vi11)),
    # tar_target(joined_vi12, join_vi12(pontos_fisc_mun, osm)),
    # tar_target(indicador_vi12, calc_vi12(joined_vi12)),
    tar_target(
        indicadores_pilar6,
        join_indicadores_pilar6(
            indicador_vi1,
            indicador_vi5,
            indicador_vi6,
            indicador_vi7,
            uf,
            indicadores_cameras_fixed#,
            #indicadores_pontos_fisc,
            #indicador_vi11,
            #indicador_vi12
        )
    ),
    # indicadores_resultado.R
    tar_target(joined_01, join_01(renavam, datasus_sim, uf)),
    tar_target(indicador_01, calc_01(joined_01)),
    tar_target(joined_02, join_02(ibge_pop, datasus_sim)),
    tar_target(indicador_02, calc_02(joined_02)),
    tar_target(indicador_03, calc_03(uf, taxabilhao)),
    tar_target(
        indicadores_resultado,
        bind_rows(indicador_01, indicador_02, indicador_03)
    ),
    # indicadores_join.R
    tar_target(
        indicadores,
        join_all_indicadores(
            indicadores_pilar1,
            indicadores_pilar2,
            indicadores_pilar3,
            indicadores_pilar4,
            indicadores_pilar5,
            indicadores_pilar6,
            indicadores_resultado
        )
    )
)