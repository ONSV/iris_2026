display_results <- function(df_indicadores, input_indicador, perc, max) {
    indicadores_results <- df_indicadores |> 
        filter(indicador == input_indicador)
    
    if (max == TRUE) {
        results <- indicadores_results |> 
            slice_max(valor, n = 3)
    } else {
        results <- indicadores_results |> 
            slice_min(valor, n = 3)
    }
    
    if (perc == TRUE) {
        results$valor <- scales::percent(
            results$valor, 
            accuracy = 0.01, 
            decimal.mark = ","
        )
    } else {
        results$valor <- scales::number(
            results$valor,
            accuracy = 0.01,
            decimal.mark = ",",
            big.mark = "."
        )
    }
    
    paste0(
        knitr::combine_words(
            results$nome_uf, 
            oxford_comma = FALSE,
            and = " e "
        ),
        ", com resultados de ",
        knitr::combine_words(results$valor, oxford_comma = FALSE, and = " e "),
        ", respectivamente"
    )
    
}

display_stats <- function(df_indicadores, indicador_input, perc) {
    values <- df_indicadores |> 
        filter(indicador == indicador_input) |> 
        pull(valor)
    
    value_mean <- mean(values, na.rm = TRUE)
    value_sd <- sd(values, na.rm = TRUE)
    
    if (perc == TRUE) {
        value_mean <- scales::percent(
            value_mean, 
            accuracy = 0.01, 
            decimal.mark = ","
        )
        value_sd <- scales::percent(
            value_sd,
            accuracy = 0.01,
            decimal.mark = ","
        )
    } else {
        value_mean <- scales::number(
            value_mean, 
            accuracy = 0.01, 
            decimal.mark = ",",
            big.mark = "."
        )
        value_sd <- scales::number(
            value_sd,
            accuracy = 0.01,
            decimal.mark = ",",
            big.mark = "."
        )
    }
    
    paste0("média de ", value_mean, " e desvio padrão de ", value_sd)
}

display_ind_names <- function(tbl_desc, ind_input) {
    tbl_desc |> 
        mutate(ind_name = glue::glue("{indicador} - {descricao}")) |> 
        filter(indicador == ind_input) |> 
        pull(ind_name)
}

display_cor_results <- function(df_correlacao, ind_a, ind_b) {
    df_correlacao |> 
        filter(var_a == ind_a, var_b == ind_b) |> 
        pull(cor) |> 
        scales::number(
            accuracy = 0.01,
            decimal.mark = ",",
            big.mark = "."
        )
}


display_cor_table <- function(df_correlacao, pilar_input) {
    df_correlacao |> 
        filter(
            str_extract(var_a, "^[^.]+\\.") %in% c(pilar_input, "0."),
            str_extract(var_b, "^[^.]+\\.") %in% c(pilar_input),
            p_value < 0.05, 
            !is.na(cor)
        ) |> 
        select(var_a, var_b, cor) |> 
        knitr::kable()
}

display_eda_table <- function(df_eda, ind_input) {
    df_eda |> 
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
            ),
            across(
                starts_with(ind_input),
                \(x) scales::number(
                    x, 
                    accuracy = 0.01, 
                    decimal.mark = ",",
                    big.mark = "."
                )
            )
        ) |> 
        knitr::kable()
}

display_pca_direction <- function(df_stats, pc_input) {
    df_stats |> 
        filter(!indicador %in% c("eigenvalue", "variancia")) |> 
        janitor::adorn_totals("row") |> 
        filter(indicador == "Total") |> 
        pull(pc_input)
}

display_pca_variance <- function(df_stats, pc_input) {
    df_stats |>
        filter(indicador == "variancia") |> 
        pull(pc_input)
}

display_notas <- function(df_notas, pilar_input, pos = c("max", "min")) {
    df_notas <- df_notas |> 
        filter(pilar == pilar_input)

    if (pos == "max") {
        df_notas <- df_notas |> 
            slice_head(n = 3)
    } else {
        df_notas <- df_notas |> 
            slice_tail(n = 3)
    }
    
    paste0(
        knitr::combine_words(
            df_notas$nome_uf, 
            oxford_comma = FALSE,
            and = " e "
        ),
        ", com resultados de ",
        knitr::combine_words(
            scales::number(
                df_notas$nota, 
                accuracy = 0.01, 
                decimal.mark = ",", 
                big.mark = "."
            ), 
            oxford_comma = FALSE, 
            and = " e "
        ),
        ", respectivamente"
    )
}

display_classificacao <- function(classificacao_df, pilar_input, class) {
    df_results <- classificacao_df |> 
        filter(pilar == pilar_input, classificacao_numeric == class)

    knitr::combine_words(df_results$nome_uf, oxford_comma = FALSE, and = " e ")
}

calc_qnt_ind <- function(df_indicadores, pilar) {
    df_indicadores |> 
        filter(str_extract(indicador, "^[^.]+\\.") == pilar) |> 
        pull(indicador) |> 
        unique() |> 
        length()
}

calc_media_regiao <- function(df_class, df_uf) {
    df_class |> 
        left_join(df_uf, by = "nome_uf") |> 
        group_by(regiao_uf) |> 
        summarise(media_regiao = mean(classificacao_numeric))
}

display_class_regiao <- function(df_media) {
    resultados <- df_media |> 
        arrange(-media_regiao) |> 
        mutate(
            media_regiao = scales::number(
                media_regiao,
                decimal.mark = ",",
                accuracy = 0.01
            ),
            results = glue::glue("{regiao_uf} ({media_regiao})")
        )

    knitr::combine_words(
        resultados$results, 
        oxford_comma = FALSE,
        and = " e "
    )
}
