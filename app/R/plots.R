plot_radar_class <- function(class_data, uf) {
    class_data <- class_data |> 
        st_drop_geometry() |> 
        filter(nome_uf == uf)
    
    plot_ly(
        type = "scatterpolar",
        mode = "markers"
    ) |> 
        add_trace(
            r = class_data$classificacao_numeric,
            theta = class_data$pilar,
            fill = "toself",
            fillcolor = "rgba(0, 73, 109, 0.4)",
            marker = list(color = "rgb(0, 73, 109)", size = 7),
            # line = list(color = "rgb(0, 73, 109)"),
            hoverinfo = "text",
            text = paste0(
                class_data$pilar, "<br>",
                "Classificação: ", class_data$classificacao_numeric
            ),
            showlegend = FALSE
        ) |> 
        layout(
            polar = list(
                radialaxis = list(
                    visible = TRUE,
                    range = c(0, 5),
                    # Change values in breaks
                    tickvals = c(0, 1, 2, 3, 4, 5)
                )
            ),
            showlegend = FALSE
        )
}