library(sf)
library(tidyverse)
library(targets)

tar_load(classificacao, store = here::here("_targets"))
tar_load(indicadores, store = here::here("_targets"))
tar_load(sf_uf, store = here::here("_targets"))
tar_load(tbl_ind_desc, store = here::here("_targets"))

arrange_classificacao_data <- function(df_class, df_uf) {
    df_class <- df_class |> 
        mutate(
            star = case_match(
                classificacao_numeric,
                1 ~ "★☆☆☆☆",
                2 ~ "★★☆☆☆",
                3 ~ "★★★☆☆",
                4 ~ "★★★★☆",
                5 ~ "★★★★★"
            ),
            star = factor(
                star, 
                levels = c("★☆☆☆☆", "★★☆☆☆", "★★★☆☆", "★★★★☆", "★★★★★")
            ),
            nome_uf_lower = tolower(nome_uf)
        )
    
    sf_class <- df_uf |> 
        mutate(
            name_state_lower = tolower(name_state),
            name_state_lower = if_else(
                name_state_lower == "espirito santo",
                "espírito santo",
                name_state_lower
            )
        ) |> 
        left_join(df_class, by = c("name_state_lower" = "nome_uf_lower")) |> 
        select(
            name_region,
            nome_uf,
            pilar, 
            star,
            classificacao_numeric, 
            nota
        ) |> 
        st_transform(crs = 4326)

    return(sf_class)
}

arrange_indicadores_data <- function(df_ind, df_uf) {
    df_ind <- df_ind |> 
        mutate(cod_uf = as.numeric(cod_uf)) |> 
        replace_na(list(valor = 1))
    
    sf_ind <- df_uf |> 
        left_join(df_ind, by = c("code_state" = "cod_uf")) |> 
        st_transform(crs = 4326) |> 
        mutate(
            name_state = case_match(
                name_state,
                "Espirito Santo" ~ "Espírito Santo",
                "Rio De Janeiro" ~ "Rio de Janeiro",
                "Rio Grande Do Sul" ~ "Rio Grande do Sul",
                "Rio Grande Do Norte" ~ "Rio Grande do Norte",
                "Mato Grosso Do Sul" ~ "Mato Grosso do Sul",
                .default = name_state
            ),
            regiao_uf = if_else(
                regiao_uf == "Centro Oeste",
                "Centro-Oeste",
                regiao_uf
            )
        ) |> 
        select(nome_uf, regiao_uf, indicador, valor)

    return(sf_ind)
}

join_desc_data <- function(df_ind, df_desc) {
    df_ind |> 
        mutate(indicador_upper = toupper(indicador)) |>
        left_join(df_desc, by = c("indicador_upper" = "indicador"))
}

sf_classificacao <- arrange_classificacao_data(classificacao, sf_uf)
sf_indicadores <- arrange_indicadores_data(indicadores, sf_uf)
ind_desc_data <- join_desc_data(indicadores, tbl_ind_desc)

saveRDS(sf_classificacao, here::here("app/data", "sf_classificacao.rds"))
saveRDS(sf_indicadores, here::here("app/data", "sf_indicadores.rds"))
saveRDS(ind_desc_data, here::here("app/data", "ind_desc_data.rds"))
