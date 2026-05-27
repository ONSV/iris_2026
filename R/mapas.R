plot_indicadores_map <- function(sf_estados, df_indicadores, ind_input) {
    df_ind <- df_indicadores |> 
        filter(indicador == ind_input) |> 
        mutate(cod_uf = as.numeric(cod_uf))
    
    sf_ind <- sf_estados |> 
        left_join(df_ind, by = c("code_state" = "cod_uf"))
    
    plot <- ggplot() +
        geom_sf(
            data = sf_ind, 
            aes(fill = valor), 
            color = "grey50", 
            lwd = 0.05
        ) +
        theme_bw(base_size = 6, base_family = "serif") + 
        labs(fill = paste0("Indicador ", toupper(ind_input))) +
        theme(
            legend.position = "top",
            legend.direction = "horizontal",
            legend.key.height = unit(0.3, "cm"),
            legend.key.width = unit(0.5, "cm")
        )
    
    perc_ind_vector <- c(
        "i.1", "i.2", "ii.1", "ii.2", "ii.3", "ii.4", "ii.5", "iii.1", "iii.2",
        "iii.3", "iii.4"
    )
    
    if (ind_input %in% perc_ind_vector) {
        plot_final <- plot +
            scale_fill_distiller(
                palette = "Blues", 
                direction = 1, 
                labels = scales::label_percent(
                    decimal.mark = ",",  
                    big.mark = "."
                )
            )
    } else {
        plot_final <- plot +
            scale_fill_distiller(
                palette = "Blues", 
                direction = 1, 
                labels = scales::label_comma(
                    decimal.mark = ",",
                    big.mark = "."
                )
            )
    }
    
    return(plot_final)
}

join_indicadores_map <- function(list_map) {
    pilar_0 <- 
        list_map[[1]] + 
        list_map[[2]] + 
        list_map[[3]] + 
        plot_layout(ncol = 2)
    
    pilar_1 <- 
        list_map[[4]] + 
        list_map[[5]] + 
        list_map[[6]] + 
        list_map[[7]] + 
        plot_layout(ncol = 2)
    
    pilar_2 <- 
        list_map[[8]] + 
        list_map[[9]] + 
        list_map[[10]] + 
        list_map[[11]] + 
        list_map[[12]] + 
        plot_layout(ncol = 2)
    
    pilar_3 <- 
        list_map[[13]] + 
        list_map[[14]] + 
        list_map[[15]] + 
        list_map[[16]] + 
        plot_layout(ncol = 2)
    
    pilar_4 <- 
        list_map[[17]] + 
        list_map[[18]] + 
        list_map[[19]] + 
        list_map[[20]] + 
        list_map[[21]] + 
        list_map[[22]] + 
        list_map[[23]] + 
        plot_layout(ncol = 2)
    
    pilar_5 <- 
        list_map[[24]] + 
        list_map[[25]] + 
        list_map[[26]] + 
        list_map[[27]] + 
        plot_layout(ncol = 2)
    
    pilar_6 <- 
        list_map[[28]] + 
        list_map[[29]] + 
        list_map[[30]] + 
        list_map[[31]] + 
        list_map[[32]] + 
        list_map[[33]] + 
        list_map[[34]] + 
        # list_map[[35]] +
        # list_map[[36]] +
        # list_map[[37]] +
        # list_map[[38]] +
        # list_map[[39]] +
        plot_layout(ncol = 2)
    
    maps <- list(
        "pilar_i" = pilar_1,
        "pilar_ii" = pilar_2,
        "pilar_iii" = pilar_3,
        "pilar_iv" = pilar_4,
        "pilar_v" = pilar_5,
        "pilar_vi" = pilar_6,
        "resultado" = pilar_0
    )
    
    return(maps)
}

plot_classificacao_map <- function(df_classificacao, sf_estados, pilar_input) {
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
        filter(pilar == pilar_input)
    
    ggplot() +
        geom_sf(
            data = sf_class,
            aes(fill = classificacao),
            color = "grey20",
            lwd = 0.1
        ) +
        theme_bw(base_size = 8, base_family = "serif") +
        labs(fill = "Classificação") +
        scale_fill_brewer(palette = "RdYlGn") +
        theme(
            legend.position = "top",
            legend.direction = "horizontal",
            legend.key.size = unit(0.5, "cm")
        )
}

plot_media_map <- function(df_class, sf_estados) {
    media_data <- df_class |> 
        group_by(nome_uf) |> 
        summarise(media_class = mean(classificacao_numeric))

    media_sf <- sf_estados |>
        mutate(name_state = case_match(
            name_state,
            "Espirito Santo" ~ "Espírito Santo",
            "Rio De Janeiro" ~ "Rio de Janeiro",
            "Rio Grande Do Sul" ~ "Rio Grande do Sul",
            "Rio Grande Do Norte" ~ "Rio Grande do Norte",
            "Mato Grosso Do Sul" ~ "Mato Grosso do Sul",
            .default = name_state
        )) |> 
        left_join(media_data, by = c("name_state" = "nome_uf"))

    ggplot() +
        geom_sf(
            data = media_sf, 
            aes(fill = media_class),
            color = "grey20",
            lwd = 0.1
        ) +
        theme_bw(base_size = 8, base_family = "serif") +
        labs(fill = "Classificação média") +
        scale_fill_distiller(
            palette = "RdYlGn", 
            direction = 1, 
            limits = c(1, 5)
        ) +
        theme(
            legend.position = "top",
            legend.direction = "horizontal",
            legend.key.size = unit(0.5, "cm")
        )
}

# plot_indicadores_leaflet <- function(df_indicadores, df_uf, ind_input) {

#     df_ind <- df_indicadores |> 
#         filter(indicador == ind_input) |> 
#         mutate(cod_uf = as.numeric(cod_uf)) |> 
#         replace_na(list(valor = 0))
    
#     sf_ind <- df_uf |> 
#         left_join(df_ind, by = c("code_state" = "cod_uf")) |> 
#         st_transform(crs = 4326) |> 
#         mutate(
#             name_state = case_match(
#                 name_state,
#                 "Espirito Santo" ~ "Espírito Santo",
#                 "Rio De Janeiro" ~ "Rio de Janeiro",
#                 "Rio Grande Do Sul" ~ "Rio Grande do Sul",
#                 "Rio Grande Do Norte" ~ "Rio Grande do Norte",
#                 "Mato Grosso Do Sul" ~ "Mato Grosso do Sul",
#                 .default = name_state
#             )
#         )

#     perc_ind_vector <- c(
#         "i.1", "i.2", "ii.1", "ii.2", "ii.3", "ii.4", "ii.5", "iii.1", "iii.2",
#         "iii.3", "iii.4"
#     )

#     pal <- colorNumeric(
#         palette = "Blues",
#         domain = sf_ind$valor
#     )

#     if (ind_input %in% perc_ind_vector) { 
#         labels <- sprintf(
#             "<strong>%s</strong><br/>%s",
#             sf_ind$name_state,
#             scales::percent(sf_ind$valor, accuracy = 0.01, decimal.mark = ",")
#         ) |> lapply(htmltools::HTML)
#     } else {
#         labels <- sprintf(
#             "<strong>%s</strong><br/>%s",
#             sf_ind$name_state,
#             scales::number(
#                 sf_ind$valor, 
#                 accuracy = 0.01, 
#                 decimal.mark = ",", 
#                 big.mark = "."
#             )
#         ) |> lapply(htmltools::HTML)
#     }

#     my_labelFormat <- function(...) {
#         fun <- labelFormat(...)
#         evalq(formatNum <- function(x) {
#             format(
#                 round(transform(x), digits),
#                 trim = TRUE, 
#                 scientific = FALSE,
#                 big.mark = big.mark, 
#                 decimal.mark = ","
#             )
#         }, environment(fun))
#         return(fun)
#     }


#     map <- leaflet(data = sf_ind) |> 
#         addProviderTiles(providers$CartoDB.PositronNoLabels) |> 
#         addPolygons(
#             fillColor = ~pal(sf_ind$valor), 
#             smoothFactor = 0.2,
#             fillOpacity = 0.8,
#             color = "grey",
#             weight = 1,
#             highlightOptions = highlightOptions(
#                 color = "black",
#                 weight = 3,
#                 bringToFront = TRUE
#             ),
#             label = labels,
#             labelOptions = labelOptions(
#                 style = list("font-weight" = "normal"),
#                 textsize = "12px",
#                 direction = "auto"
#             )
#         )
    
#     if (ind_input %in% perc_ind_vector) {
#         final_map <- map %>%
#             addLegend(
#                 pal = pal,
#                 values = sf_ind$valor,
#                 position = "bottomright",
#                 opacity = 1,
#                 title = paste0("Indicador ", toupper(ind_input)),
#                 labFormat = my_labelFormat(
#                     transform = \(x) 100 * x,
#                     suffix = "%",
#                     big.mark = "."
#                 )
#             )
#     } else {
#         final_map <- map |> 
#             addLegend(
#                 pal = pal,
#                 values = sf_ind$valor,
#                 position = "bottomright",
#                 opacity = 1,
#                 title = paste0("Indicador ", toupper(ind_input)),
#                 labFormat = my_labelFormat(
#                     digits = 2,
#                     big.mark = "."
#                 )
#             )
#     }
    
#     return(final_map)
# }

# plot_classificacao_leaflet <- function(df_classificacao, df_uf, pilar_input) {
#     df_class <- df_classificacao |> 
#         mutate(
#             star = case_match(
#                 classificacao_numeric,
#                 1 ~ "★☆☆☆☆",
#                 2 ~ "★★☆☆☆",
#                 3 ~ "★★★☆☆",
#                 4 ~ "★★★★☆",
#                 5 ~ "★★★★★"
#             ),
#             star = factor(
#                 star, 
#                 levels = c("★☆☆☆☆", "★★☆☆☆", "★★★☆☆", "★★★★☆", "★★★★★")
#             ),
#             nome_uf_lower = tolower(nome_uf)
#         )
    
#     sf_class <- df_uf |> 
#         mutate(
#             name_state_lower = tolower(name_state),
#             name_state_lower = if_else(
#                 name_state_lower == "espirito santo",
#                 "espírito santo",
#                 name_state_lower
#             )
#         ) |> 
#         left_join(df_class, by = c("name_state_lower" = "nome_uf_lower")) |> 
#         select(nome_uf, pilar, star) |> 
#         filter(pilar == pilar_input) |> 
#         st_transform(crs = 4326)
    
#     pal <- colorFactor(
#         palette = "RdYlGn",
#         domain = sf_class$star
#     )
    
#     labels <- sprintf(
#         "<strong>%s</strong><br/>%s",
#         sf_class$nome_uf,
#         sf_class$star
#     ) |> lapply(htmltools::HTML)
    
#     leaflet(data = sf_class) |> 
#         addProviderTiles(providers$CartoDB.PositronNoLabels) |>
#         addPolygons(
#             fillColor = ~pal(sf_class$star), 
#             smoothFactor = 0.2,
#             fillOpacity = 0.8,
#             color = "grey",
#             weight = 1,
#             highlightOptions = highlightOptions(
#                 color = "black",
#                 weight = 3,
#                 bringToFront = TRUE
#             ),
#             label = labels,
#             labelOptions = labelOptions(
#                 style = list("font-weight" = "normal"),
#                 textsize = "12px",
#                 direction = "auto"
#             )
#         ) |> 
#         addLegend(
#             pal = pal,
#             values = sf_class$star,
#             position = "bottomright",
#             opacity = 1,
#             title = paste0("Classificação - ", pilar_input)
#         )
    
# }

# plot_leaflet_geral <- function(df_classificacao, df_uf) {
#     df_class <- df_classificacao |> 
#         group_by(nome_uf) |> 
#         summarise(classificacao_numeric = mean(classificacao_numeric)) |> 
#         mutate(nome_uf_lower = tolower(nome_uf))

#     sf_class <- df_uf |> 
#         mutate(
#             name_state_lower = tolower(name_state),
#             name_state_lower = if_else(
#                 name_state_lower == "espirito santo",
#                 "espírito santo",
#                 name_state_lower
#             )
#         ) |> 
#         left_join(df_class, by = c("name_state_lower" = "nome_uf_lower")) |> 
#         st_transform(crs = 4326) |> 
#         select(nome_uf, classificacao_numeric)

#     pal <- colorNumeric(
#         palette = "RdYlGn",
#         domain = c(1, 5)
#     )

#     labels <- sprintf(
#         "<strong>%s</strong><br/>Classificação média: %s",
#         sf_class$nome_uf,
#         scales::number(
#             sf_class$classificacao_numeric,
#             accuracy = 0.01,
#             decimal.mark = ",",
#             big.mark = "."
#         )
#     ) |> lapply(htmltools::HTML)

#     my_labelFormat <- function(...) {
#         fun <- labelFormat(...)
#         evalq(formatNum <- function(x) {
#             format(
#                 round(transform(x), digits),
#                 trim = TRUE, 
#                 scientific = FALSE,
#                 big.mark = big.mark, 
#                 decimal.mark = ","
#             )
#         }, environment(fun))
#         return(fun)
#     }


#     leaflet(data = sf_class) |> 
#         addProviderTiles(providers$CartoDB.PositronNoLabels) |>
#         addPolygons(
#             fillColor = ~pal(sf_class$classificacao_numeric), 
#             smoothFactor = 0.2,
#             fillOpacity = 0.8,
#             color = "grey",
#             weight = 1,
#             highlightOptions = highlightOptions(
#                 color = "black",
#                 weight = 3,
#                 bringToFront = TRUE
#             ),
#             label = labels,
#             labelOptions = labelOptions(
#                 style = list("font-weight" = "normal"),
#                 textsize = "12px",
#                 direction = "auto"
#             )
#         ) |> 
#         addLegend(
#             pal = pal,
#             values = seq(1, 5, 1),
#             position = "bottomright",
#             opacity = 1,
#             title = paste0("Classificação média"),
#             labFormat = my_labelFormat(big.mark = ".")
#         )
# }

