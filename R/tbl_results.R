make_tbl_indicadores <- function(df_indicadores, ind_input) {
    df_indicadores |>  
        filter(str_extract(indicador, "^[^.]+\\.") == ind_input) |> 
        select(-cod_uf) |> 
        pivot_wider(names_from = indicador, values_from = valor)
}

make_gt_indicadores <- function(tbl_indicadores) {
    tbl_indicadores |> 
        group_by(regiao_uf) |> 
        gt() |> 
        cols_label(nome_uf = "UF") |> 
        cols_label_with(-nome_uf, \(x) toupper(x)) |> 
        fmt_number(decimals = 2, dec_mark = ",", sep_mark = ".") |> 
        tab_options(table.font.size = "10pt") |>
        data_color(
            columns = -nome_uf,
            palette = "Blues"
        )
}

make_eda_df <- function(df_indicadores) {
    df_indicadores |> 
        select(regiao_uf, indicador, valor) |> 
        arrange(indicador) |> 
        group_by(regiao_uf, indicador) |> 
        mutate(
            media = mean(valor, na.rm = TRUE),
            mediana = median(valor, na.rm = TRUE),
            desvio = sd(valor, na.rm = TRUE),
            min = min(valor, na.rm = TRUE),
            max = max(valor, na.rm = TRUE)
        ) |> 
        nest(valor = valor) |> 
        select(-valor)
}

make_eda_gt <- function(eda_df, ind_input) {
    
    eda_df <- eda_df |> 
        filter(str_extract(indicador, "^[^.]+\\.") == ind_input) |> 
        pivot_longer(media:max, values_to = "valor", names_to = "stat") |> 
        pivot_wider(names_from = indicador, values_from = valor) |> 
        mutate(
            stat = case_match(
                stat,
                "media" ~ "Média",
                "desvio" ~ "Desvio padrão",
                "min" ~ "Mínimo",
                "max" ~ "Máximo",
                "mediana" ~ "Mediana",
                .default = stat
            )
        )
    
    eda_df <- eda_df |> 
        ungroup() |>
        group_by(regiao_uf) |> 
        gt() |> 
        cols_label_with(fn = \(x) toupper(x)) |> 
        cols_label(stat = "Estatística") |> 
        fmt_number(decimals = 2, dec_mark = ",", sep_mark = ".") |> 
        tab_options(table.font.size = "10pt") |> 
        data_color(
            columns = -stat,
            palette = "Blues"
        )
    
    if (ind_input == "i.") {
        eda_df <- eda_df |> 
            fmt_percent(
                columns = c("i.1", "i.2"), 
                decimals = 2, 
                dec_mark = ",", 
                sep_mark = "."
            )
    }
    
    if (ind_input == "ii.") {
        eda_df <- eda_df |> 
            fmt_percent(
                columns = c("ii.1", "ii.2", "ii.3", "ii.4", "ii.5"), 
                decimals = 2, 
                dec_mark = ",", 
                sep_mark = "."
            )
    }
    
    if (ind_input == "iii.") {
        eda_df <- eda_df |> 
            fmt_percent(
                columns = c("iii.1", "iii.2", "iii.3", "iii.4"), 
                decimals = 2, 
                dec_mark = ",", 
                sep_mark = "."
            )
    }
    
    return(eda_df)
    
}

arrange_pca_stats <- function(pca_results) {
    df_pca_rotation <- 
        as_tibble(pca_results$rotation, rownames = "indicador")
    
    eigenvalue <- pca_results$sdev ^ 2
    prop_variance <- eigenvalue / sum(eigenvalue)
    
    rbind(
        df_pca_rotation, 
        c("eigenvalue", eigenvalue), 
        c("variancia", prop_variance)
    ) |> 
        as_tibble() |> 
        mutate(across(starts_with("PC"), ~ as.numeric(.)))
}

arrange_pca_results <- function(df_indicadores_wide, pca_results) {
    bind_cols(df_indicadores_wide, pca_results$x) |> 
        select(nome_uf, starts_with("PC"))
}

make_pca_stats_gt <- function(pca_stats) {
    pca_stats |> 
        mutate(
            indicador = toupper(indicador),
            indicador = case_match(
                indicador,
                "EIGENVALUE" ~ "Autovalor",
                "VARIANCIA" ~ "Variância",
                .default = indicador
            )
        ) |> 
        gt() |> 
        fmt_number(decimals = 2, dec_mark = ",", sep_mark = ".") |> 
        cols_label(indicador = "Indicador") |> 
        tab_options(table.font.size = "10pt") |> 
        data_color(
            columns = -indicador,
            palette = "RdBu",
            domain = c(-1, 1),
            rows = !indicador %in% c("Autovalor", "Variância")
        )
}

make_pca_results_gt <- function(pca_numeric_results) {
    pca_numeric_results |> 
        mutate(
            regiao_uf = case_match(
                nome_uf,
                "Rondônia" ~ "Norte",
                "Acre" ~ "Norte",
                "Amazonas" ~ "Norte",
                "Roraima" ~ "Norte",
                "Pará" ~ "Norte",
                "Amapá" ~ "Norte",
                "Tocantins" ~ "Norte",
                "Maranhão" ~ "Nordeste",
                "Piauí" ~ "Nordeste",
                "Ceará" ~ "Nordeste",
                "Rio Grande do Norte" ~ "Nordeste",
                "Paraíba" ~ "Nordeste",
                "Pernambuco" ~ "Nordeste",
                "Alagoas" ~ "Nordeste",
                "Sergipe" ~ "Nordeste",
                "Bahia" ~ "Nordeste",
                "Minas Gerais" ~ "Sudeste",
                "Espírito Santo" ~ "Sudeste",
                "Rio de Janeiro" ~ "Sudeste",
                "São Paulo" ~ "Sudeste",
                "Paraná" ~ "Sul",
                "Santa Catarina" ~ "Sul",
                "Rio Grande do Sul" ~ "Sul",
                "Mato Grosso do Sul" ~ "Centro-Oeste",
                "Mato Grosso" ~ "Centro-Oeste",
                "Goiás" ~ "Centro-Oeste",
                "Distrito Federal" ~ "Centro-Oeste"
            )
        ) |> 
        group_by(regiao_uf) |>
        gt() |> 
        fmt_number(decimals = 2, dec_mark = ",", sep_mark = ".") |>
        tab_options(table.font.size = "10pt") |> 
        cols_label(nome_uf = "UF")
}

make_tbl_invertidos <- function(df_scaled, df_invertido) {
    df_invertido |> 
        filter(indicador %in% c(
            "i.2", "ii.1", "ii.1", "ii.2", "ii.3", "ii.4", "iii.1", "iii.2",
            "iv.2", "iv.3", "iv.4", "iv.5", "iv.6", "iv.7", "vi.1", "vi.6",
            "0.1", "0.2", "0.3"
        )) |> 
        select(regiao_uf, nome_uf, indicador, invertido = valor_padronizado) |> 
        left_join(
            df_scaled |> 
                select(indicador, padronizado = valor_padronizado, nome_uf),
            by = c("nome_uf", "indicador")
        )
}

make_gt_invertidos <- function(df_tbl_invertido, ind_input) {
    gt_invertido <- df_tbl_invertido |> 
        filter(str_extract(indicador, "^[^.]+\\.") == ind_input) |> 
        pivot_wider(
            names_from = indicador,
            values_from = c(invertido, padronizado)
        ) |> 
        select(
            regiao_uf, 
            nome_uf, 
            starts_with("padronizado"), 
            starts_with("invertido")
        ) |> 
        group_by(regiao_uf) |> 
        gt() |> 
        tab_spanner(
            label = "Padronizado",
            columns = starts_with("padronizado")
        ) |>
        tab_spanner(
            label = "Invertido",
            columns = starts_with("invertido")
        ) |> 
        fmt_number(
            decimals = 2,
            dec_mark = ",",
            sep_mark = "."
        ) |> 
        tab_options(table.font.size = "10pt")
    
    if (ind_input == "i.") {
        gt_results <- gt_invertido |> 
            cols_label(
                padronizado_i.2 = "I.2",
                invertido_i.2 = "I.2",
                nome_uf = "UF"
            )
    }
    
    if (ind_input == "ii.") {
        gt_results <- gt_invertido |> 
            cols_label(
                padronizado_ii.1 = "II.1",
                invertido_ii.1 = "II.1",
                padronizado_ii.2 = "II.2",
                invertido_ii.2 = "II.2",
                padronizado_ii.3 = "II.3",
                invertido_ii.3 = "II.3",
                padronizado_ii.4 = "II.4",
                invertido_ii.4 = "II.4",
                nome_uf = "UF"
            )
    }
    
    if (ind_input == "iii.") {
        gt_results <- gt_invertido |> 
            cols_label(
                padronizado_iii.1 = "III.1",
                invertido_iii.1 = "III.1",
                padronizado_iii.2 = "III.2",
                invertido_iii.2 = "III.2",
                nome_uf = "UF"
            )
    }
    
    if (ind_input == "iv.") {
        gt_results <- gt_invertido |> 
            cols_label(
                padronizado_iv.2 = "IV.2",
                invertido_iv.2 = "IV.2",
                padronizado_iv.3 = "IV.3",
                invertido_iv.3 = "IV.3",
                padronizado_iv.4 = "IV.4",
                invertido_iv.4 = "IV.4",
                padronizado_iv.5 = "IV.5",
                invertido_iv.5 = "IV.5",
                padronizado_iv.6 = "IV.6",
                invertido_iv.6 = "IV.6",
                padronizado_iv.7 = "IV.7",
                invertido_iv.7 = "IV.7",
                nome_uf = "UF"
            ) |> 
            tab_options(table.font.size = "8pt")
    }
    
    if (ind_input == "vi.") {
        gt_results <- gt_invertido |> 
            cols_label(
                padronizado_vi.1 = "VI.1",
                invertido_vi.1 = "VI.1",
                padronizado_vi.6 = "VI.6",
                invertido_vi.6 = "VI.6",
                nome_uf = "UF"
            )
    }
    
    if (ind_input == "0.") {
        gt_results <- gt_invertido |> 
            cols_label(
                padronizado_0.1 = "0.1",
                invertido_0.1 = "0.1",
                padronizado_0.2 = "0.2",
                invertido_0.2 = "0.2",
                padronizado_0.3 = "0.3",
                invertido_0.3 = "0.3",
                nome_uf = "UF"
            )
    }
    
    return(gt_results)
}

make_gt_compostos <- function(
        df_indicadores_compostos, df_indicadores_notas, pilar_input
    ) {
    df_indicadores_compostos |> 
        left_join(df_indicadores_notas, by = c("nome_uf", "pilar")) |> 
        filter(pilar == pilar_input) |> 
        select(nome_uf, starts_with("PC"), indicador_composto, nota) |> 
        select(where(~ !all(is.na(.)))) |> 
        gt() |> 
        fmt_number(decimals = 2, dec_mark = ",", sep_mark = ".") |> 
        cols_label(
            nome_uf = "UF",
            indicador_composto = "Índice Composto",
            nota = "Nota"
        ) |> 
        tab_options(table.font.size = "10pt") |> 
        data_color(
            columns = nota,
            palette = "Blues"
        )
}

make_gt_classificacao <- function(df_classificacao, pilar_input) {
    df_classificacao |> 
        filter(pilar == pilar_input) |> 
        select(nome_uf, nota, classificacao) |> 
        gt() |> 
        fmt_number(decimals = 2, dec_mark = ",", sep_mark = ".") |> 
        cols_label(
            nome_uf = "UF",
            classificacao = "Classificação",
            nota = "Nota"
        ) |> 
        tab_options(table.font.size = "10pt") |> 
        data_color(
            columns = nota,
            palette = "Blues"
        ) |> 
        data_color(
            columns = classificacao,
            palette = c(
                "#d7191c",
                "#fdae61",
                "#ffffbf",
                "#a6d96a",
                "#1a9641"
            )
        )
}
