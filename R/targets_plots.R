targets_plots <- list(
    tar_target(
        pca_plots, 
        map2(
            list_pca_stats,
            list_pca_numeric_results,
            plot_pca,
            df_uf = uf
        )
    ),
    tar_target(
        boxplot_notas,
        plot_boxplot_notas(classificacao, uf)
    ),
    tar_target(
        media_classificacao,
        plot_media_classificacao(classificacao, uf)
    ),
    tar_target(class_anim_plot, make_class_anim_plot(sf_uf, classificacao)),
    tar_target(animation_classificacao, render_classificacao(class_anim_plot)),
    tar_target(radar_anim_plot, make_radar_anim_plot(classificacao)),
    tar_target(animation_radar, render_radar(radar_anim_plot)),
    tar_target(
        exported_anims,
        export_animations(
            list(animation_classificacao, animation_radar),
            list("classificacao_mapas.gif", "radar_nota.gif"),
            path = "report/08"
        )
    )#,
    # tar_target(
    #     indicadores_maps,
    #     map(
    #         list(
    #             c(1, 2, 3),
    #             c(4, 5, 6, 7),
    #             c(8, 9, 10, 11, 12),
    #             c(13, 14, 15, 16),
    #             c(17, 18, 19, 20, 21, 22, 23),
    #             c(24, 25, 26, 27),
    #             c(28, 29, 30, 31, 32, 33, 34)
    #         ),
    #         create_indicadores_map_png,
    #         map_indicadores = list_map_indicadores
    #     ),
    # ),
    # tar_target(
    #     exported_maps,
    #     walk2(
    #         indicadores_maps,
    #         list(
    #             paste0(
    #                 "report/08/plots/",
    #                 c("0_1.png", "0_2.png", "0_3.png")
    #             ),
    #             paste0(
    #                 "report/08/plots/",
    #                 c("i_1.png", "i_2.png", "i_3.png", "i_4.png")
    #             ),
    #             paste0(
    #                 "report/08/plots/",
    #                 c(
    #                     "ii_1.png", "ii_2.png", "ii_3.png",
    #                     "ii_4.png", "ii_5.png"
    #                 )
    #             ),
    #             paste0(
    #                 "report/08/plots/",
    #                 c("iii_1.png", "iii_2.png", "iii_3.png", "iii_4.png")
    #             ),
    #             paste0(
    #                 "report/08/plots/",
    #                 c(
    #                     "iv_1.png", "iv_2.png", "iv_3.png", "iv_4.png",
    #                     "iv_5.png", "iv_6.png", "iv_7.png"
    #                 )
    #             ),
    #             paste0(
    #                 "report/08/plots/",
    #                 c("v_1.png", "v_2.png", "v_3.png", "v_4.png")
    #             ),
    #             paste0(
    #                 "report/08/plots/",
    #                 c(
    #                     "vi_1.png", "vi_2.png", "vi_3.png", "vi_4.png",
    #                     "vi_5.png", "vi_6.png", "vi_7.png"
    #                 )
    #             )
    #         ),
    #         ~ ggsave(
    #             filename = .y,
    #             plot = .x,
    #             path = "report/08/plots",
    #             device = "png",
    #             width = 6,
    #             height = 6,
    #             units = "in"
    #         )
    #     )
    # ),
    # tar_target(
    #     indicadores_animation_list,
    #     map(
    #         c("^0_", "^i_", "^ii_", "^iii_", "^iv_", "^v_", "^vi_"),
    #         animate_pngs
    #     )
    # ),
    # tar_target(
    #     exp_indicadores_anim,
    #     map2(
    #         indicadores_animation_list,
    #         paste0(
    #             "indicadores_",
    #             c(
    #                 "0.gif", "i.gif", "ii.gif",
    #                 "iii.gif", "iv.gif", "v.gif", "vi.gif"
    #             )
    #         ),
    #         image_write,
    #     ),
    #     format = "file"
    # )
)