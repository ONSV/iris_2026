plot_classificacao_leaflet <- function(df_classificacao, pilar_input) {
    
    sf_class <- df_classificacao |> 
        filter(pilar == pilar_input)
    
    pal <- colorFactor(
        palette = "RdYlGn",
        domain = sf_class$star
    )
    
    labels <- sprintf(
        "<strong>%s</strong><br/>%s",
        sf_class$nome_uf,
        sf_class$star
    ) |> lapply(htmltools::HTML)
    
    leaflet(data = sf_class) |> 
        addProviderTiles(providers$Esri.WorldGrayCanvas) |>
        addPolygons(
            fillColor = ~pal(sf_class$star), 
            smoothFactor = 0.2,
            fillOpacity = 0.8,
            color = "grey",
            weight = 1,
            highlightOptions = highlightOptions(
                color = "black",
                weight = 3,
                bringToFront = TRUE
            ),
            label = labels,
            labelOptions = labelOptions(
                style = list("font-weight" = "normal"),
                textsize = "12px",
                direction = "auto"
            ),
            layerId = ~nome_uf
        ) |> 
        addLegend(
            pal = pal,
            values = sf_class$star,
            position = "bottomright",
            opacity = 1,
            title = paste0("Classificação - ", pilar_input)
        )
}

plot_leaflet_geral <- function(df_classificacao) {
    sf_class <- df_classificacao |> 
        group_by(nome_uf) |> 
        summarise(classificacao_numeric = mean(classificacao_numeric))

    pal <- colorNumeric(
        palette = "RdYlGn",
        domain = c(1, 5)
    )

    labels <- sprintf(
        "<strong>%s</strong><br/>Classificação média: %s",
        sf_class$nome_uf,
        scales::number(
            sf_class$classificacao_numeric,
            accuracy = 0.01,
            decimal.mark = ",",
            big.mark = "."
        )
    ) |> lapply(htmltools::HTML)

    my_labelFormat <- function(...) {
        fun <- labelFormat(...)
        evalq(formatNum <- function(x) {
            format(
                round(transform(x), digits),
                trim = TRUE, 
                scientific = FALSE,
                big.mark = big.mark, 
                decimal.mark = ","
            )
        }, environment(fun))
        return(fun)
    }


    leaflet(data = sf_class) |> 
        addProviderTiles(providers$Esri.WorldGrayCanvas) |>
        addPolygons(
            fillColor = ~pal(sf_class$classificacao_numeric), 
            smoothFactor = 0.2,
            fillOpacity = 0.8,
            color = "grey",
            weight = 1,
            highlightOptions = highlightOptions(
                color = "black",
                weight = 3,
                bringToFront = TRUE
            ),
            label = labels,
            labelOptions = labelOptions(
                style = list("font-weight" = "normal"),
                textsize = "12px",
                direction = "auto"
            )
        ) |> 
        addLegend(
            pal = pal,
            values = seq(1, 5, 1),
            position = "bottomright",
            opacity = 1,
            title = paste0("Classificação média"),
            labFormat = my_labelFormat(big.mark = ".")
        )
}

plot_indicadores_leaflet <- function(df_indicadores, ind_input) {

    sf_ind <- df_indicadores |> 
        filter(indicador == ind_input)

    perc_ind_vector <- c(
        "i.1", "i.2", "ii.1", "ii.2", "ii.3", "ii.4", "ii.5", "iii.1", "iii.2",
        "iii.3", "iii.4"
    )

    pal <- colorNumeric(
        palette = "Blues",
        domain = sf_ind$valor
    )

    if (ind_input %in% perc_ind_vector) { 
        labels <- sprintf(
            "<strong>%s</strong><br/>%s",
            sf_ind$nome_uf,
            scales::percent(sf_ind$valor, accuracy = 0.01, decimal.mark = ",")
        ) |> lapply(htmltools::HTML)
    } else {
        labels <- sprintf(
            "<strong>%s</strong><br/>%s",
            sf_ind$nome_uf,
            scales::number(
                sf_ind$valor, 
                accuracy = 0.01, 
                decimal.mark = ",", 
                big.mark = "."
            )
        ) |> lapply(htmltools::HTML)
    }

    my_labelFormat <- function(...) {
        fun <- labelFormat(...)
        evalq(formatNum <- function(x) {
            format(
                round(transform(x), digits),
                trim = TRUE, 
                scientific = FALSE,
                big.mark = big.mark, 
                decimal.mark = ","
            )
        }, environment(fun))
        return(fun)
    }


    map <- leaflet(data = sf_ind) |> 
        addProviderTiles(providers$Esri.WorldGrayCanvas) |> 
        addPolygons(
            fillColor = ~pal(sf_ind$valor), 
            smoothFactor = 0.2,
            fillOpacity = 0.8,
            color = "grey",
            weight = 1,
            highlightOptions = highlightOptions(
                color = "black",
                weight = 3,
                bringToFront = TRUE
            ),
            label = labels,
            labelOptions = labelOptions(
                style = list("font-weight" = "normal"),
                textsize = "12px",
                direction = "auto"
            )
        )
    
    if (ind_input %in% perc_ind_vector) {
        final_map <- map %>%
            addLegend(
                pal = pal,
                values = sf_ind$valor,
                position = "bottomright",
                opacity = 1,
                title = paste0("Indicador ", toupper(ind_input)),
                labFormat = my_labelFormat(
                    transform = \(x) 100 * x,
                    suffix = "%",
                    big.mark = "."
                )
            )
    } else {
        final_map <- map |> 
            addLegend(
                pal = pal,
                values = sf_ind$valor,
                position = "bottomright",
                opacity = 1,
                title = paste0("Indicador ", toupper(ind_input)),
                labFormat = my_labelFormat(
                    digits = 2,
                    big.mark = "."
                )
            )
    }
    
    return(final_map)
}

plot_variacao_geral_leaflet <- function(df_classificacao_24, df_classificacao_23){
    df_24 <- sf_classificacao |>  mutate(ano = 2024)
    df_23 <-  sf_classificacao_23 |> st_drop_geometry() |>  mutate(ano = 2023)
    
    base_variacao <- df_24 |> bind_rows(df_23) |>
        st_drop_geometry() |> 
        select(-c(name_region, star)) |> 
        group_by(nome_uf, ano) |>
        summarise(classificacao_media = mean(classificacao_numeric)) |>
        pivot_wider(
            names_from = ano,
            values_from = classificacao_media,
            id_cols = c(nome_uf)
        ) |>
        relocate(`2023`, .before = `2024`) |>
        mutate(variacao_cor = ((`2024` - `2023`) / `2023`) * 100) |> 
        arrange(desc(abs(variacao_cor))) |> 
        left_join(df_24 |> select(nome_uf, geometry) |> unique(), by = "nome_uf")
    
    sf_class <- base_variacao |>
        st_as_sf() |>
        st_collection_extract("POLYGON") |>
        st_cast("MULTIPOLYGON")
    
    pal <- colorNumeric(
        palette = "RdYlGn",
        domain = c(-20, 20),
        na.color = "#808080"
    )
    
    labels <- sprintf(
        "<strong>%s</strong><br/>Variação: %s%%",
        base_variacao$nome_uf,
        ifelse(is.na(sf_class$variacao_cor), "N/A", format(round(sf_class$variacao_cor, 2), nsmall = 2, decimal.mark = ","))
    ) |> lapply(htmltools::HTML)
    
    my_labelFormat <- function(...) {
        fun <- labelFormat(...)
        evalq(formatNum <- function(x) {
            format(
                round(transform(x), digits),
                trim = TRUE, 
                scientific = FALSE,
                big.mark = big.mark, 
                decimal.mark = ","
            )
        }, environment(fun))
        return(fun)
    }
    
    leaflet(data = sf_class) |> 
        addProviderTiles(providers$Esri.WorldGrayCanvas) |>
        addPolygons(
            fillColor = ~pal(variacao_cor), 
            smoothFactor = 0.2,
            fillOpacity = 0.8,
            color = "grey",
            weight = 1,
            highlightOptions = highlightOptions(
                color = "black",
                weight = 3,
                bringToFront = TRUE
            ),
            label = labels,
            labelOptions = labelOptions(
                style = list("font-weight" = "normal"),
                textsize = "12px",
                direction = "auto"
            ),
            layerId = ~nome_uf
        ) |> addLegendNumeric(
            pal = pal,
            values = c(-20, 20),
            position = "bottomright",
            title = "Classificação",
            width = 20,           
            height = 150,         
            orientation = "vertical",
            shape = "rect"
        )
}
