plot_pca <- function(pca_stats, pca_numeric_results, df_uf) {
    
    eigen_results <- pca_stats |> 
        filter(indicador == "eigenvalue") |> 
        select(starts_with("PC")) |> 
        pivot_longer(everything(), names_to = "pc", values_to = "eigenvalue")
    
    eigen_n <- sum(eigen_results$eigenvalue > 1)
    
    df_rotation <- pca_stats |> 
        filter(!indicador %in% c("eigenvalue", "variancia"))
    
    arrow_scale_factor <- 3
    arrow_color <- "grey30"
    
    line_type <- "dashed"
    line_color <- "grey40"
    
    pca_numeric_results <- pca_numeric_results |> 
        left_join(df_uf |> select(nome_uf, regiao_uf), by = "nome_uf")
    
    plot_minor_pca <- function(pc_x, pc_y) {
        
        var_pc_x <- pca_stats |> 
            filter(indicador == "variancia") |> 
            pull(pc_x)
        
        var_pc_y <- pca_stats |>
            filter(indicador == "variancia") |> 
            pull(pc_y)
        
        label_pc_x <- paste0(
            pc_x,
            " (",
            scales::percent(
                var_pc_x, 
                accuracy = 0.01, 
                decimal.mark = ",", 
                big.mark = "."
            ),
            ")"
        )
        
        label_pc_y <- paste0(
            pc_y,
            " (",
            scales::percent(
                var_pc_y, 
                accuracy = 0.01, 
                decimal.mark = ",", 
                big.mark = "."
            ),
            ")"
        )
        
        pc_x <- sym(pc_x)
        pc_y <- sym(pc_y)
        
        ggplot() +
            geom_point(
                data = pca_numeric_results,
                aes(
                    x = {{pc_x}},
                    y = {{pc_y}},
                    color = regiao_uf,
                    shape = regiao_uf
                )
                # pch = 21,
            ) +
            #coord_fixed() +
            theme_bw(base_size = 9, base_family = "serif") +
            geom_segment(
                data = df_rotation, 
                aes(
                    x = 0,
                    y = 0, 
                    xend = {{pc_x}} * arrow_scale_factor, 
                    yend = {{pc_y}} * arrow_scale_factor
                ),
                arrow = arrow(length = unit(0.2, "cm")), 
                color = arrow_color
            ) +
            geom_text_repel(
                data = df_rotation, 
                aes(
                    x = {{pc_x}} * arrow_scale_factor,
                    y = {{pc_y}} * arrow_scale_factor, 
                    label = toupper(indicador)
                ), 
                color = arrow_color,
                size = 2.5
            ) +
            geom_hline(yintercept = 0, color = line_color, lty = line_type) +
            geom_vline(xintercept = 0,  color = line_color, lty = line_type) +
            labs(
                x = label_pc_x,
                y = label_pc_y
            ) + 
            scale_discrete_onsv() +
            theme(
                legend.position = "top",
                legend.title = element_blank(),
                legend.direction = "horizontal"
            )
    }
    
    if (eigen_n < 3) {
        plot_pca <- plot_minor_pca("PC1", "PC2") & 
            coord_fixed()
    } else {
        plot_pca <- plot_minor_pca("PC1", "PC2") +
            plot_minor_pca("PC1", "PC3") +
            plot_minor_pca("PC3", "PC2") +
            plot_layout(ncol = 2, nrow = 2, guides = "collect") & 
            theme(legend.position = "top")
    }
    
    return(plot_pca)
}

plot_boxplot_notas <- function(df_classificacao, df_uf) {
    df_classificacao |> 
        # mutate(nome_uf = fct_reorder(nome_uf, nota, .fun = 'median')) |> 
        left_join(df_uf |> select(nome_uf, regiao_uf), by = "nome_uf") |> 
        ggplot(
            aes(
                x = nota,
                y = fct_reorder(nome_uf, nota, .fun = median, .desc = FALSE),
                fill = regiao_uf
            )
        ) +
        geom_boxplot(color = "grey40", lwd = 0.3) +
        theme_bw(base_size = 10, base_family = "serif") +
        scale_x_continuous(
            minor_breaks = NULL, 
            labels = scales::number_format(decimal.mark = ",")
        ) +
        labs(x = NULL, y = NULL, fill = NULL) +
        theme(
            legend.position = "top",
            legend.direction = "horizontal",
            legend.location = "plot",
            legend.justification = "left"
        ) +
        scale_fill_manual(
            values = c(
                onsv_palette$lightblue, 
                onsv_palette$lightyellow,
                onsv_palette$lightred,
                onsv_palette$lightgreen,
                onsv_palette$lightpink
            )
        )
}

plot_media_classificacao <- function(df_classificacao, df_uf) {
    df_classificacao |> 
        group_by(nome_uf) |> 
        summarise(class_media = mean(classificacao_numeric)) |> 
        mutate(
            nome_uf = fct_reorder(nome_uf, class_media),
            class_label = scales::number(
                class_media, 
                accuracy = 0.1, 
                decimal.mark = ","
            )
        ) |>
        left_join(df_uf |> select(nome_uf, regiao_uf), by = "nome_uf") |> 
        ggplot(aes(
            x = class_media, 
            y = reorder(nome_uf, class_media)#, 
            #color = regiao_uf
        )) +
        geom_segment(
            aes(
                x = 1, 
                xend = class_media, 
                y = nome_uf, 
                yend = nome_uf,
                color = regiao_uf
            )
        ) +
        geom_point(fill = "white", size = 2, aes(color = regiao_uf)) +
        geom_label(
            aes(
                x = class_media,
                y = fct_reorder(nome_uf, class_media),
                label = class_label
            ),
            nudge_x = 0.2,
            size = 2.5
        ) +
        theme_bw(base_size = 10, base_family = "serif") +
        scale_x_continuous(minor_breaks = NULL, limits = c(1, 5)) +
        labs(x = NULL, y = NULL, color = NULL) +
        theme(
            legend.position = "top",
            legend.direction = "horizontal",
            legend.location = "plot",
            legend.justification = "left"
        ) +
        scale_color_manual(
            values = c(
                onsv_palette$blue, 
                onsv_palette$yellow,
                onsv_palette$red,
                onsv_palette$green,
                onsv_palette$pink
            )
        )
}



