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


make_dt_variacao <- function(ind_data_24, ind_data_23, pilar_input, uf_input) {
    df_24 <- ind_data_24 |> mutate(ano = 2024)
    df_23 <-  ind_data_23 |> mutate(ano = 2023)
    
    
    base_variacao <- df_24 |> bind_rows(df_23) |>
        separate(col = indicador_upper, into = c("pilar", "ind"), sep = "\\.", remove = FALSE) |>
        mutate(pil = "Pilar") |>
        unite(col = pilar, pil, pilar, sep = " ") |>
        mutate(pilar = ifelse(pilar == "Pilar 0", "Pilar VII", pilar)) |>
        select(cod_uf, nome_uf, pilar, indicador_upper, descricao, ano, valor) |>
        pivot_wider(names_from = ano, values_from = valor) |>
        relocate(`2023`, .before = `2024`) |>
        mutate(
            variacao_cor = if_else(`2023` == 0, NA_real_, ((`2024` - `2023`) / `2023`) * 100),
            variacao = case_when(
                `2023` == 0 & `2024` == 0 ~ "0,00%",   
                `2023` == 0 & `2024` > 0  ~ "NA", 
                is.na(`2023`) & `2024` > 0  ~ "Novo",
                variacao_cor > 0.001      ~ paste0(format(round(variacao_cor, 2), nsmall = 2, decimal.mark = ","), "%", " ▲"),
                variacao_cor < -0.001     ~ paste0(format(round(variacao_cor, 2), nsmall = 2, decimal.mark = ","), "%", " ▼"),
                TRUE ~ paste0(format(round(variacao_cor, 2), nsmall = 2, decimal.mark = ","), "%")
            )
        )
    
    if (uf_input != "Todos"){
    base_variacao <- base_variacao |> filter(nome_uf == uf_input)
    }
    
    if (pilar_input != "Todos") {
        base_variacao <- base_variacao |> filter(pilar == pilar_input)
    }
    
    base_variacao |>
        select(-cod_uf) |> 
        datatable(
            colnames = c("Estado", "Pilar", "Indicador", "Descrição", "2023", "2024", "variacao_texto_velha", "Variação (%)"),
            class = 'cell-border compact',
            options = list(
                pageLength = 12,
                dom = 'ltip',
                # scrollY = "600px",
                columnDefs = list(
                    list(targets = 6, visible = FALSE),
                    list(className = 'dt-center', targets = c(0, 4, 5, 7))
                ),
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json')
            ),
            rownames = FALSE
        ) |> 
        formatRound(columns = c("2023", "2024"), digits = 2, mark = ",", dec.mark = ".") |>
        formatStyle(
            columns = "variacao",         
            valueColumns = "variacao_cor", 
            backgroundColor = styleInterval(
                cuts = c(-0.001, 0.001),    
                values = c("#ffe6cc", "#ffffff", "#D5EEFB")
            ),
            color = styleInterval(
                cuts = c(-0.001, 0.001),
                values = c("#EA6A24",  "#000000", "#00496d")
            )
        )
}

