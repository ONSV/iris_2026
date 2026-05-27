calc_correlacao_spear <- function(df_indicadores) {
    df_input <- df_indicadores |> 
        select(cod_uf, indicador, valor) |> 
        replace_na(list(valor = 1)) |> 
        pivot_wider(names_from = indicador, values_from = valor) |> 
        select(-cod_uf)
    
    cor_matrix <- cor(df_input, method = "spearman")
    
    rcorr_results <- df_input |> 
        as.matrix() |> 
        rcorr(type = "spearman")
    
    pvalue_matrix <- rcorr_results$P |> 
        as_tibble(rownames = "var_a") |> 
        pivot_longer(-var_a, names_to = "var_b", values_to = "p_value")
    
    cor_df <- cor_matrix |> 
        as_tibble(rownames = "var_a") |> 
        pivot_longer(-var_a, names_to = "var_b", values_to = "cor") |> 
        left_join(pvalue_matrix, by = c("var_a", "var_b"))
    
    # Organiza a ordem dos resultados para auxiliar no ggplot
    cor_df |> 
        mutate(
            var_a = factor(var_a, level = unique(cor_df$var_a)),
            var_b = factor(var_b, level = rev(unique(cor_df$var_b))),
            lvl_a = as.numeric(var_a),
            lvl_b = as.numeric(var_b |> fct_rev()),
            cor = if_else(lvl_a < lvl_b, cor, NA_real_)
        )
}

plot_cor_matrix <- function(tbl_cor, ind_vector = NULL) {
    
    if (!is.null(ind_vector)) {
        tbl_cor <- tbl_cor |> 
            filter(var_a %in% ind_vector, var_b %in% ind_vector)
    }
    
    plot_subtitle <- 
        "Nota: Células sem valores indicam correlações não significativas."
    
    axis_limits_x <- unique(tbl_cor$var_a) |> na.omit()
    axis_limits_y <- rev(unique(tbl_cor$var_b)) |> na.omit()
    
    cov_plot <- tbl_cor |>
        ggplot(aes(x = var_a, y = var_b, fill = cor)) +
        geom_tile(
            color = ifelse(
                is.na(tbl_cor$cor),
                NA,
                "grey10"
            )
        ) +
        geom_text(
            aes(
                label = scales::label_number(
                    big.mark = ".",
                    decimal.mark = ",",
                    accuracy = 0.01
                )(cor)
            ),
            color = ifelse(
                tbl_cor$p_value >= 0.05,
                NA,
                ifelse(
                    abs(tbl_cor$cor) > 0.6,
                    "grey90",
                    "grey10"
                )
            ),
            size = 2.8
        ) +
        scale_fill_gradient2(
            low = onsv_palette$red, 
            high = onsv_palette$blue,
            mid = "white",
            midpoint = 0,
            limits = c(-1, 1),
            na.value = NA
        ) +
        theme_bw(base_size = 10, base_family = "serif") +
        theme(
            panel.grid = element_blank(),
            legend.position = "inside",
            legend.position.inside = c(0.87, 0.75),
            plot.background = element_rect(fill = "white", color = "white"),
            plot.subtitle = element_markdown()
        ) +
        coord_fixed() +
        scale_x_discrete(
            limits = axis_limits_x[1:length(axis_limits_x) - 1]
        ) +
        scale_y_discrete(
            limits = axis_limits_y[1:length(axis_limits_y) - 1]
        ) +
        labs(
            x = element_blank(),
            y = element_blank(),
            fill = element_blank(),
            caption = plot_subtitle
        )
    
    return(cov_plot)
}