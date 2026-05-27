make_class_anim_plot <- function(sf_estados, df_classificacao) {
    sf_class <- sf_estados |>
        mutate(
            name_state = tolower(name_state),
            name_state = if_else(
                name_state == "espirito santo",
                "espírito santo",
                name_state
            )
        ) |> 
        left_join(
            df_classificacao |> mutate(nome_uf = tolower(nome_uf)),
            by = c("name_state" = "nome_uf")
        ) |> 
        mutate(pilar = if_else(pilar == "Resultado final", "Pilar VII", pilar))
    
    animation <- ggplot() +
        geom_sf(
            data = sf_class,
            aes(fill = classificacao),
            color = "grey20",
            lwd = 0.1
        ) +
        labs(fill = "Classificação") +
        scale_fill_brewer(palette = "RdYlGn") +
        theme_bw() +
        theme(
            legend.position = "top",
            legend.direction = "horizontal",
            legend.key.size = unit(0.5, "cm")
        ) +
        transition_filter(
            transition_length = 1,
            filter_length = 2,
            `Pilar I - Gestão da Segurança no Trânsito` = pilar == "Pilar I",
            `Pilar II - Vias Seguras` = pilar == "Pilar II",
            `Pilar III - Segurança Veicular` = pilar == "Pilar III",
            `Pilar IV - Educação para o Trânsito` = pilar == "Pilar IV",
            `Pilar V - Vigilância, Promoção da Saúde e Atendimento às Vítimas` = 
                pilar == "Pilar V",
            `Pilar VI - Normalização e Fiscalização` = pilar == "Pilar VI",
            `Pilar VII - Indicadores de mortalidade` = pilar == "Pilar VII"
        ) +
        ggtitle('{closest_filter}') +
        enter_fade()
    return(animation)
}

render_classificacao <- function(plot_anim) {
    animation_rendered <- animate(
        plot_anim,
        renderer = gifski_renderer(),
        res = 300,
        height = 6,
        width = 6,
        units = "in"
    )
    return(animation_rendered)
}

make_radar_anim_plot <- function(df_classificacao) {
    radar_anim <- df_classificacao |>
        mutate(
            label_pos = if_else(nota > 8, nota - 1.5, nota + 1.5),
            pilar = if_else(pilar == "Resultado final", "Pilar VII", pilar)
        ) |>
        ggplot(aes(x = pilar, y = nota)) +
        geom_col(fill = onsvplot::onsv_palette$blue) +
        geom_label(
            aes(
                label = as.character(round(nota, digits = 2)),
                y = label_pos
            ),
            size = 3.5
        ) +
        coord_polar() +
        theme_minimal() +
        scale_y_continuous(limits = c(0, 10), minor_breaks = NULL) +
        labs(x = NULL, y = NULL, title = "{closest_state}") +
        theme(
            axis.text.y = element_blank()
        ) +
        transition_states(
            states = nome_uf,
            transition_length = 1,
            state_length = 2
        )
    return(radar_anim)
}
 
render_radar <- function(radar_animation) {
    rendered <- animate(
        radar_animation,
        renderer = gifski_renderer(),
        res = 300,
        height = 6,
        width = 6,
        units = "in",
        nframes = 400
    )
    return(rendered)
}

export_animations <- function(list_anim, list_name, path) {
    map2(
        .x = list_anim,
        .y = list_name,
        ~ anim_save(
            filename = .y,
            animation = .x,
            path = path
        )
    )
}

create_indicadores_map_png <- function(map_indicadores, vector_select) {
    selected_list <- map_indicadores[vector_select]
    selected_list_themed <- map(
        selected_list,
        ~ .x + theme_bw()
    )
    return(selected_list_themed)
}

animate_pngs <- function(png_pattern) {
    list_images <- image_read(
        list.files("report/08/plots", full.names = TRUE, pattern = png_pattern)
    )
    images_animation <- image_animate(list_images, fps = 0.5)
    return(images_animation)
}

