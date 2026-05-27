targets_correlacao <- list(
    tar_target(df_cor_results, calc_correlacao_spear(indicadores)),
    tar_target(cor_matrix_full, plot_cor_matrix(df_cor_results)),
    tar_target(
        list_input_cor,
        list(
            pilar1 = c("0.1", "0.2", "0.3", "i.1", "i.2", "i.3", "i.4"),
            pilar2 = c(
                "0.1", "0.2", "0.3", "ii.1", "ii.2", "ii.3", "ii.4", "ii.5"
            ),
            pilar3 = c("0.1", "0.2", "0.3", "iii.1", "iii.2", "iii.3", "iii.4"),
            pilar4 = c(
                "0.1", "0.2", "0.3", "iv.1", "iv.2", "iv.3", "iv.4", "iv.5",
                "iv.6", "iv.7"
            ),
            pilar5 = c("0.1", "0.2", "0.3", "v.1", "v.2", "v.3", "v.4"),
            pilar6 = c(
                "0.1", "0.2", "0.3", "vi.1", "vi.2", "vi.3", "vi.4",
                "vi.5", "vi.6", "vi.7"
            )
        )
    ),
    tar_target(
        list_cor_matrix, 
        map(list_input_cor, plot_cor_matrix, tbl_cor = df_cor_results)
    )
)