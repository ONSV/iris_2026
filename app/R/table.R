make_class_dt <- function(sf_class, pilar_input) {
    sf_class |> 
        st_drop_geometry() |> 
        filter(pilar == pilar_input) |> 
        mutate(ranking = dense_rank(desc(nota))) |> 
        select(ranking, nome_uf, name_region, nota, star) |> 
        arrange(-nota) |> 
        datatable(
            rownames = FALSE,
            colnames = c("Ranking", "UF", "Região", "Nota", "Classificação"),
            options = list(dom = "ftp"),
            selection = "none",
        ) |> 
        formatStyle(
            "star",
            backgroundColor = styleEqual(
                levels = levels(sf_class$star),
                values = c(
                    "#d7191c",
                    "#fdae61",
                    "#ffffbf",
                    "#a6d96a",
                    "#1a9641"
                )
            ),
            color = styleEqual(
                levels = levels(sf_class$star),
                values = c(
                    "white",
                    "black",
                    "black",
                    "black",
                    "white"
                )
            )
        ) |> 
        formatStyle(
            columns = c("name_region", "nome_uf", "star", "nota", "ranking"),
            fontSize = "10pt"
        ) |> 
        formatRound(
            columns = "nota",
            digits = 3,
            dec.mark = ","
        )
}

make_ind_dt <- function(sf_ind, ind_input) {
    sf_indicadores_filtered <- sf_ind |> 
        st_drop_geometry() |> 
        filter(indicador == ind_input) |> 
        select(regiao_uf, nome_uf, valor)
    
    perc_ind_vector <- c(
        "i.1", "i.2", "ii.1", "ii.2", "ii.3", "ii.4", "ii.5", "iii.1", "iii.2",
        "iii.3", "iii.4"
    )

    pal <- colorNumeric(
        palette = "Blues", 
        domain = sf_indicadores_filtered$valor
    )
    
    dt <- datatable(
        sf_indicadores_filtered,
        #class = "compact",
        rownames = FALSE,
        colnames = c("Região", "UF", paste0("Indicador ", toupper(ind_input))),
        options = list(
            dom = "ftp"#,
            #paging = FALSE,
            #scrollY = "450px"
        ),
        selection = "none"
    ) |> 
    formatStyle(
        "valor",
        backgroundColor = styleInterval(
            cuts = quantile(
                sf_indicadores_filtered$valor,
                probs = seq(
                    0, 1, length.out = nrow(sf_indicadores_filtered) - 1
                )
            ),
            values = pal(quantile(
                sf_indicadores_filtered$valor,
                probs = seq(
                    0, 1, length.out = nrow(sf_indicadores_filtered)
                )
            ))
        ),
        color = styleInterval(
            cuts = quantile(
                sf_indicadores_filtered$valor,
                probs = 0.80
            ),
            values = c("black", "white")
        )
    ) |> 
    formatStyle(
        columns = c("regiao_uf", "nome_uf", "valor"),
        fontSize = "10pt"
    )
    
    if (ind_input %in% perc_ind_vector) {
        dt |> 
            formatPercentage(
                columns = "valor",
                digits = 2,
                dec.mark = ","
            )
    } else {
        dt |> 
            formatRound(
                columns = "valor",
                digits = 2,
                dec.mark = ",",
                mark = "."
            )
    }
}

make_results_gt <- function(df_desc_data, ind_input, uf) {
    df_desc_data |> 
        filter(
            nome_uf == uf, 
            str_extract(indicador, "^[^.]+\\.") == ind_input
        ) |> 
        select(indicador_upper, descricao, unidade, valor) |> 
        # merge descricao and unidade
        mutate(descricao = paste0(descricao, " [", unidade, "]")) |> 
        select(-unidade) |>
        gt() |> 
        cols_label(
            indicador_upper = "Indicador",
            descricao = "Descrição [unidade]",
            valor = "Resultado"
        ) |> 
        fmt_number(
            columns = "valor",
            decimals = 2,
            dec_mark = ",",
            sep_mark = "."
        ) |> 
        fmt_percent(
            columns = "valor",
            rows = indicador_upper %in% c(
                "I.1", "I.2", "II.1", "II.2", "II.3", "II.4", "II.5", "III.1",
                "III.2", "III.3", "III.4"
            ),
            decimals = 2,
            dec_mark = ",",
            sep_mark = "."
        ) |> 
        tab_style(
            style = list(
                cell_fill(color = "#00496d"),
                cell_text(color = "white")
            ),
            locations = cells_column_labels()
        )
    
}

make_gt_bench <- function(sf_class, uf) {
    df_melhores <- sf_class |> 
        st_drop_geometry() |> 
        filter(classificacao_numeric == 5) |> 
        group_by(pilar) |> 
        summarise(melhores_uf = paste(nome_uf, collapse = ", "))
    
    sf_class |> 
        st_drop_geometry() |> 
        filter(nome_uf == uf) |> 
        select(pilar, star) |> 
        left_join(df_melhores, by = "pilar") |>
        mutate(pilar = if_else(
            pilar == "Resultado final",
            "Indicadores de resultado", 
            pilar)
        ) |>
        gt() |> 
        cols_label(
            pilar = "Pilar",
            star = "Classificação",
            melhores_uf = "Melhores resultados (★★★★★)"
        ) |> 
        data_color(
            columns = star,
            palette = c(
                "#d7191c", "#fdae61", "#ffffbf", "#a6d96a", "#1a9641"
            )
        ) |> 
        tab_style(
            style = list(
                cell_fill(color = "#00496d"),
                cell_text(color = "white")
            ),
            locations = cells_column_labels()
        )
}

make_gt_class_inicio <- function(sf_class) {
    df_class <- sf_class |> 
        st_drop_geometry() |> 
        group_by(nome_uf) |> 
        summarise(class_media = mean(classificacao_numeric)) |> 
        mutate(ranking = dense_rank(desc(class_media))) |> 
        select(ranking, nome_uf, class_media) |> 
        arrange(ranking)
    
    df_class |> 
        gt() |> 
        cols_label(
            ranking = "Ranking",
            nome_uf = "UF",
            class_media = "Classificação média"
        ) |> 
        fmt_number(class_media, decimals = 2, dec_mark = ",") |> 
        data_color(class_media, palette = "RdYlGn", domain = c(1, 5)) |> 
        tab_options(table.width = pct(100))
}

