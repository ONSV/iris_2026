library(shiny)
library(bslib)
library(bsicons)
library(leaflet)
library(markdown)
library(shinycssloaders)
library(DT)
library(sf)
library(dplyr)
library(gt)
library(plotly)
library(stringr)
library(sass)

### Só roda no servidor, não localmente #####################
source("R/table.R")
source("R/mapa.R") 
source("R/plots.R")

sf_classificacao <- readRDS("data/sf_classificacao.rds")
sf_indicadores <- readRDS("data/sf_indicadores.rds")
ind_desc_data <- readRDS("data/ind_desc_data.rds")

#############################################################

main_map_height <- 700
class_map_height <- 550
ind_map_height <- 550
card_info_height <- 700
value_box_height <- 150

sass(
    sass_file("style.scss"),
    output = "www/style.css"
)

home_panel <- nav_panel(
    tags$head(
        tags$link(href = "style.css", rel = "stylesheet", type = "text/css")
    ),
    value = "home",
    title = "Início",
    icon = bsicons::bs_icon("house"),
    value_box(
        title = NULL,
        value = "Início",
        height = 100
    ),
    layout_columns(
        col_widths = c(4, 4, 4),
        card(
            card_header("Informações"),
            full_screen = TRUE,
            height = card_info_height,
            card_body(
                includeMarkdown("home.md")
            )
        ),
        card(
            card_header("Classificação Geral"),
            full_screen = TRUE,
            card_body(
                class = "p-0",
                withSpinner(
                    leafletOutput("mapageral", height = main_map_height),
                    type = 8,
                    color = "#00496d"
                )
            )
        ),
        card(
            card_header("Tabela de classificação"),
            full_screen = TRUE,
            height = card_info_height,
            card_body(
                #class = "p-0",
                withSpinner(
                    gt_output("gtclasshome"),
                    type = 8,
                    color = "#00496d"
                )
            )
        )
    )
)

pilar01_panel <- nav_panel(
    value = "pilar01",
    title = "Pilar I - Gestão da Segurança no Trânsito",
    value_box(
        title = "Pilar I",
        value = "Gestão da Segurança no Trânsito",
        height = 100
    ),
    layout_columns(
        col_widths = c(6, 6, 12),
        card(
            card_header("Classificação - Mapa"),
            full_screen = TRUE,
            card_body(
                class = "p-0",
                withSpinner(
                    leafletOutput("mapapilar1", height = class_map_height),
                    type = 8,
                    color = "#00496d"
                )
            )
        ),
        card(
            card_header("Classificação - Tabela"),
            full_screen = TRUE,
            withSpinner(
                DTOutput("tblpilar1"),
                type = 8,
                color = "#00496d"
            )
        ),
        tabsetPanel(
            type = "pills",
            tabPanel(
                title = "Indicador I.1",
                card(
                    tags$h5("Percentual de municípios integrados ao SNT"),
                    max_height = "80px"
                ),
                layout_columns(
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                leafletOutput("mapai1", height = ind_map_height),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    ),
                    card(
                        full_screen = TRUE,
                        withSpinner(
                            DTOutput("tbli1"),
                            type = 8,
                            color = "#00496d"
                        )
                    )
                )
            ),
            tabPanel(
                title = "Indicador I.2",
                card(
                    tags$h5("Percentual de campos não informados nas bases do RENAEST"),
                    max_height = "80px"
                ),
                layout_columns(
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                leafletOutput("mapai2", height = ind_map_height),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    ),
                    card(
                        full_screen = TRUE,
                        withSpinner(
                            DTOutput("tbli2"),
                            type = 8,
                            color = "#00496d"
                        )
                    )
                )
            ),
            tabPanel(
                title = "Indicador I.3",
                card(
                    tags$h5("Produto interto bruto per capita [R$ 1.000,00 / Hab.]"),
                    max_height = "80px"
                ),
                layout_columns(
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                leafletOutput("mapai3", height = ind_map_height),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    ),
                    card(
                        full_screen = TRUE,
                        withSpinner(
                            DTOutput("tbli3"),
                            type = 8,
                            color = "#00496d"
                        )
                    )
                )
            ),
            tabPanel(
                title = "Indicador I.4",
                card(
                    tags$h5("Disponibilidade de informações no portal do Detran [Nota]"),
                    max_height = "80px"
                ),
                layout_columns(
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                leafletOutput("mapai4", height = ind_map_height),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    ),
                    card(
                        full_screen = TRUE,
                        withSpinner(
                            DTOutput("tbli4"),
                            type = 8,
                            color = "#00496d"
                        )
                    )
                )
            )
        )
    )
)

pilar02_panel <- nav_panel(
    value = "pilar02",
    title = "Pilar II - Vias Seguras",
    value_box(
        title = "Pilar II",
        value = "Vias Seguras",
        height = 100
    ),
    layout_columns(
        col_widths = c(6, 6, 12),
        card(
            card_header("Classificação - Mapa"),
            full_screen = TRUE,
            card_body(
                class = "p-0",
                withSpinner(
                    leafletOutput("mapapilar2", height = class_map_height),
                    type = 8,
                    color = "#00496d"
                )
            )
        ),
        card(
            card_header("Classificação - Tabela"),
            full_screen = TRUE,
            withSpinner(
                DTOutput("tblpilar2"),
                type = 8,
                color = "#00496d"
            )
        ),
        tabsetPanel(
            type = "pills",
            tabPanel(
                title = "Indicador II.1",
                card(
                    tags$h5("Percentual de extensão de rodovias em condições péssimas e ruins na categoria Estado Geral"),
                    max_height = "80px"
                ),
                layout_columns(
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                leafletOutput(
                                    "mapaii1", 
                                    height = ind_map_height
                                ),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    ),
                    card(
                        full_screen = TRUE,
                        withSpinner(
                            DTOutput("tblii1"),
                            type = 8,
                            color = "#00496d"
                        )
                    )
                )
            ),
            tabPanel(
                title = "Indicador II.2",
                card(
                    tags$h5("Percentual de extensão de rodovias em condições péssimas e ruins na categoria Geometria"),
                    max_height = "80px"
                ),
                layout_columns(
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                leafletOutput(
                                    "mapaii2", 
                                    height = ind_map_height
                                ),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    ),
                    card(
                        full_screen = TRUE,
                        withSpinner(
                            DTOutput("tblii2"),
                            type = 8,
                            color = "#00496d"
                        )
                    )
                )
            ),
            tabPanel(
                title = "Indicador II.3",
                card(
                    tags$h5("Percentual de extensão de rodovias em condições péssimas e ruins na categoria Pavimento"),
                    max_height = "80px"
                ),
                layout_columns(
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                leafletOutput(
                                    "mapaii3", 
                                    height = ind_map_height
                                ),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    ),
                    card(
                        full_screen = TRUE,
                        withSpinner(
                            DTOutput("tblii3"),
                            type = 8,
                            color = "#00496d"
                        )
                    )
                )
            ),
            tabPanel(
                title = "Indicador II.4",
                card(
                    tags$h5("Percentual de extensão de rodovias em condições péssimas e ruins na categoria Sinalização"),
                    max_height = "80px"
                ),
                layout_columns(
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                leafletOutput(
                                    "mapaii4", 
                                    height = ind_map_height
                                ),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    ),
                    card(
                        full_screen = TRUE,
                        withSpinner(
                            DTOutput("tblii4"),
                            type = 8,
                            color = "#00496d"
                        )
                    )
                )
            ),
            tabPanel(
                title = "Indicador II.5",
                card(
                    tags$h5("Percentual de extensão de rodovias federais com pistas duplas"),
                    max_height = "80px"
                ),
                layout_columns(
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                leafletOutput(
                                    "mapaii5", 
                                    height = ind_map_height
                                ),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    ),
                    card(
                        full_screen = TRUE,
                        withSpinner(
                            DTOutput("tblii5"),
                            type = 8,
                            color = "#00496d"
                        )
                    )
                )
            )
        )
    )
)

pilar03_panel <- nav_panel(
    value = "pilar03",
    title = "Pilar III - Segurança Veicular",
    value_box(
        title = "Pilar III",
        value = "Segurança Veicular",
        height = 100
    ),
    layout_columns(
        col_widths = c(6, 6, 12),
        card(
            card_header("Classificação - Mapa"),
            full_screen = TRUE,
            card_body(
                class = "p-0",
                withSpinner(
                    leafletOutput("mapapilar3", height = class_map_height),
                    type = 8,
                    color = "#00496d"
                )
            )
        ),
        card(
            card_header("Classificação - Tabela"),
            full_screen = TRUE,
            withSpinner(
                DTOutput("tblpilar3"),
                type = 8,
                color = "#00496d"
            )
        ),
        tabsetPanel(
            type = "pills",
            tabPanel(
                title = "Indicador III.1",
                card(
                    tags$h5("Percentual de frota de motocicletas"),
                    max_height = "80px"
                ),
                layout_columns(
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                leafletOutput(
                                    "mapaiii1",
                                    height = ind_map_height
                                ),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    ),
                    card(
                        full_screen = TRUE,
                        withSpinner(
                            DTOutput("tbliii1"),
                            type = 8,
                            color = "#00496d"
                        )
                    )
                )
            ),
            tabPanel(
                title = "Indicador III.2",
                card(
                    tags$h5("Percentual da frota com idade igual ou superior a 10 anos"),
                    max_height = "80px"
                ),
                layout_columns(
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                leafletOutput(
                                    "mapaiii2",
                                    height = ind_map_height
                                ),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    ),
                    card(
                        full_screen = TRUE,
                        withSpinner(
                            DTOutput("tbliii2"),
                            type = 8,
                            color = "#00496d"
                        )
                    )
                )
            ),
            tabPanel(
                title = "Indicador III.3",
                card(
                    tags$h5("Percentual mínimo de veículos com Airbag/ABS na frota"),
                    max_height = "80px"
                ),
                layout_columns(
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                leafletOutput(
                                    "mapaiii3",
                                    height = ind_map_height
                                ),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    ),
                    card(
                        full_screen = TRUE,
                        withSpinner(
                            DTOutput("tbliii3"),
                            type = 8,
                            color = "#00496d"
                        )
                    )
                )
            ),
            tabPanel(
                title = "Indicador III.4",
                card(
                    tags$h5("Percentual mínimo de veículos com ISOFIX na frota"),
                    max_height = "80px"
                ),
                layout_columns(
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                leafletOutput(
                                    "mapaiii4",
                                    height = ind_map_height
                                ),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    ),
                    card(
                        full_screen = TRUE,
                        withSpinner(
                            DTOutput("tbliii4"),
                            type = 8,
                            color = "#00496d"
                        )
                    )
                )
            )
        )
    )
)

pilar04_panel <- nav_panel(
    value = "pilar04",
    title = "Pilar IV - Educação para o Trânsito",
    value_box(
        title = "Pilar IV",
        value = "Educação para o Trânsito",
        height = 100
    ),
    layout_columns(
        col_widths = c(6, 6, 12),
        card(
            card_header("Classificação - Mapa"),
            full_screen = TRUE,
            card_body(
                class = "p-0",
                withSpinner(
                    leafletOutput("mapapilar4", height = class_map_height),
                    type = 8,
                    color = "#00496d"
                )
            )
        ),
        card(
            card_header("Classificação - Tabela"),
            full_screen = TRUE,
            withSpinner(
                DTOutput("tblpilar4"),
                type = 8,
                color = "#00496d"
            )
        ),
        tabsetPanel(
            type = "pills",
            tabPanel(
                title = "Indicador IV.1",
                card(
                    tags$h5("Relação entre condutores habilitados a conduzir motocicletas e frota de motocicletas [condutor/100 veic.]"),
                    max_height = "80px"
                ),
                layout_columns(
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                leafletOutput(
                                    "mapaiv1",
                                    height = ind_map_height
                                ),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    ),
                    card(
                        full_screen = TRUE,
                        withSpinner(
                            DTOutput("tbliv1"),
                            type = 8,
                            color = "#00496d"
                        )
                    )
                )
            ),
            tabPanel(
                title = "Indicador IV.2",
                card(
                    tags$h5("Taxa de infrações por consumo de bebidas alcoólicas a cada 10 mil veículos"),
                    max_height = "80px"
                ),
                layout_columns(
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                leafletOutput(
                                    "mapaiv2",
                                    height = ind_map_height
                                ),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    ),
                    card(
                        full_screen = TRUE,
                        withSpinner(
                            DTOutput("tbliv2"),
                            type = 8,
                            color = "#00496d"
                        )
                    )
                )
            ),
            tabPanel(
                title = "Indicador IV.3",
                card(
                    tags$h5("Taxa de infrações por excesso de velocidade a cada 10 mil veículos"),
                    max_height = "80px"
                ),
                layout_columns(
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                leafletOutput(
                                    "mapaiv3",
                                    height = ind_map_height
                                ),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    ),
                    card(
                        full_screen = TRUE,
                        withSpinner(
                            DTOutput("tbliv3"),
                            type = 8,
                            color = "#00496d"
                        )
                    )
                )
            ),
            tabPanel(
                title = "Indicador IV.4",
                card(
                    tags$h5("Taxa de infrações por não utilizar o cinto de segurança a cada 10 mil veículos"),
                    max_height = "80px"
                ),
                layout_columns(
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                leafletOutput(
                                    "mapaiv4",
                                    height = ind_map_height
                                ),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    ),
                    card(
                        full_screen = TRUE,
                        withSpinner(
                            DTOutput("tbliv4"),
                            type = 8,
                            color = "#00496d"
                        )
                    )
                )
            ),
            tabPanel(
                title = "Indicador IV.5",
                card(
                    tags$h5("Taxa de infrações cometidas por não utilizar o capacete a cada 10 mil veiculos"),
                    max_height = "80px"
                ),
                layout_columns(
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                leafletOutput(
                                    "mapaiv5",
                                    height = ind_map_height
                                ),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    ),
                    card(
                        full_screen = TRUE,
                        withSpinner(
                            DTOutput("tbliv5"),
                            type = 8,
                            color = "#00496d"
                        )
                    )
                )
            ),
            tabPanel(
                title = "Indicador IV.6",
                card(
                    tags$h5("Taxa de infrações por utilizar o celular na direção a cada 10 mil veiculos"),
                    max_height = "80px"
                ),
                layout_columns(
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                leafletOutput(
                                    "mapaiv6",
                                    height = ind_map_height
                                ),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    ),
                    card(
                        full_screen = TRUE,
                        withSpinner(
                            DTOutput("tbliv6"),
                            type = 8,
                            color = "#00496d"
                        )
                    )
                )
            ),
            tabPanel(
                title = "Indicador IV.7",
                card(
                    tags$h5("Taxa de infrações cometidas por não utilizar dispositivos de retenção a cada 10 mil veículos"),
                    max_height = "80px"
                ),
                layout_columns(
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                leafletOutput(
                                    "mapaiv7",
                                    height = ind_map_height
                                ),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    ),
                    card(
                        full_screen = TRUE,
                        withSpinner(
                            DTOutput("tbliv7"),
                            type = 8,
                            color = "#00496d"
                        )
                    )
                )
            )
        )
    )
)

pilar05_panel <- nav_panel(
    value = "pilar05",
    title = "Pilar V - Vigilência, Promoção da Saúde e Atendimento às Vítimas no Trânsito",
    value_box(
        title = "Pilar V",
        value = "Vigilência, Promoção da Saúde e Atendimento às Vítimas no Trânsito",
        height = 100
    ),
    layout_columns(
        col_widths = c(6, 6, 12),
        card(
            card_header("Classificação - Mapa"),
            full_screen = TRUE,
            card_body(
                class = "p-0",
                withSpinner(
                    leafletOutput("mapapilar5", height = class_map_height),
                    type = 8,
                    color = "#00496d"
                )
            )
        ),
        card(
            card_header("Classificação - Tabela"),
            full_screen = TRUE,
            withSpinner(
                DTOutput("tblpilar5"),
                type = 8,
                color = "#00496d"
            )
        ),
        tabsetPanel(
            type = "pills",
            tabPanel(
                title = "Indicador V.1",
                card(
                    tags$h5("Taxa de profissionais que atuam na área da saúde per capita [n / 1000 hab.]"),
                    max_height = "80px"
                ),
                layout_columns(
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                leafletOutput(
                                    "mapav1",
                                    height = ind_map_height
                                ),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    ),
                    card(
                        full_screen = TRUE,
                        withSpinner(
                            DTOutput("tblv1"),
                            type = 8,
                            color = "#00496d"
                        )
                    )
                )
            ),
            tabPanel(
                title = "Indicador V.2",
                card(
                    tags$h5("Taxa de leitos totais per capita [n / 1000 hab.]"),
                    max_height = "80px"
                ),
                layout_columns(
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                leafletOutput(
                                    "mapav2",
                                    height = ind_map_height
                                ),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    ),
                    card(
                        full_screen = TRUE,
                        withSpinner(
                            DTOutput("tblv2"),
                            type = 8,
                            color = "#00496d"
                        )
                    )
                )
            ),
            tabPanel(
                title = "Indicador V.3",
                card(
                    tags$h5("Taxa de leitos SUS per capita [n / 1000 hab.]"),
                    max_height = "80px"
                ),
                layout_columns(
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                leafletOutput(
                                    "mapav3",
                                    height = ind_map_height
                                ),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    ),
                    card(
                        full_screen = TRUE,
                        withSpinner(
                            DTOutput("tblv3"),
                            type = 8,
                            color = "#00496d"
                        )
                    )
                )
            ),
            tabPanel(
                title = "Indicador V.4",
                card(
                    tags$h5("Taxa de leitos não-SUS per capita [n / 1000 hab.]"),
                    max_height = "80px"
                ),
                layout_columns(
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                leafletOutput(
                                    "mapav4",
                                    height = ind_map_height
                                ),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    ),
                    card(
                        full_screen = TRUE,
                        withSpinner(
                            DTOutput("tblv4"),
                            type = 8,
                            color = "#00496d"
                        )
                    )
                )
            )
        )
    )
)

pilar06_panel <- nav_panel(
    value = "pilar06",
    title = "Pilar VI - Normatização e Fiscalização",
    value_box(
        title = "Pilar VI",
        value = "Normatização e Fiscalização",
        height = 100
    ),
    layout_columns(
        col_widths = c(6, 6, 12),
        card(
            card_header("Classificação - Mapa"),
            full_screen = TRUE,
            card_body(
                class = "p-0",
                withSpinner(
                    leafletOutput("mapapilar6", height = class_map_height),
                    type = 8,
                    color = "#00496d"
                )
            )
        ),
        card(
            card_header("Classificação - Tabela"),
            full_screen = TRUE,
            withSpinner(
                DTOutput("tblpilar6"),
                type = 8,
                color = "#00496d"
            )
        ),
        tabsetPanel(
            type = "pills",
            tabPanel(
                title = "Indicador VI.1",
                card(
                    tags$h5("Taxa de infrações por frota [n / 10.000 veic.]"),
                    max_height = "80px"
                ),
                layout_columns(
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                leafletOutput(
                                    "mapavi1",
                                    height = ind_map_height
                                ),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    ),
                    card(
                        full_screen = TRUE,
                        withSpinner(
                            DTOutput("tblvi1"),
                            type = 8,
                            color = "#00496d"
                        )
                    )
                )
            ),
            tabPanel(
                title = "Indicador VI.2",
                card(
                    tags$h5("Taxa de câmeras de segurança em geral em relacao à frota [n / 10.000 veic.]"),
                    max_height = "80px"
                ),
                layout_columns(
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                leafletOutput(
                                    "mapavi2",
                                    height = ind_map_height
                                ),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    ),
                    card(
                        full_screen = TRUE,
                        withSpinner(
                            DTOutput("tblvi2"),
                            type = 8,
                            color = "#00496d"
                        )
                    )
                )
            ),
            tabPanel(
                title = "Indicador VI.3",
                card(
                    tags$h5("Taxa de câmeras de segurança em rodovias em relação à frota [n / 10.000 veic.]"),
                    max_height = "80px"
                ),
                layout_columns(
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                leafletOutput(
                                    "mapavi3",
                                    height = ind_map_height
                                ),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    ),
                    card(
                        full_screen = TRUE,
                        withSpinner(
                            DTOutput("tblvi3"),
                            type = 8,
                            color = "#00496d"
                        )
                    )
                )
            ),
            tabPanel(
                title = "Indicador VI.4",
                card(
                    tags$h5("Taxa de câmeras de segurança em vias urbanas em relação à frota [n / 10.000 veic.]"),
                    max_height = "80px"
                ),
                layout_columns(
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                leafletOutput(
                                    "mapavi4",
                                    height = ind_map_height
                                ),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    ),
                    card(
                        full_screen = TRUE,
                        withSpinner(
                            DTOutput("tblvi4"),
                            type = 8,
                            color = "#00496d"
                        )
                    )
                )
            ),
            tabPanel(
                title = "Indicador VI.5",
                card(
                    tags$h5("Taxa de câmeras de segurança em relação à extensão de rodovias federais pavimentadas [n / 100 km]"),
                    max_height = "80px"
                ),
                layout_columns(
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                leafletOutput(
                                    "mapavi5",
                                    height = ind_map_height
                                ),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    ),
                    card(
                        full_screen = TRUE,
                        withSpinner(
                            DTOutput("tblvi5"),
                            type = 8,
                            color = "#00496d"
                        )
                    )
                )
            ),
            tabPanel(
                title = "Indicador VI.6",
                card(
                    tags$h5("Taxa de infrações de velocidade por câmera de segurança [n / n]"),
                    max_height = "80px"
                ),
                layout_columns(
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                leafletOutput(
                                    "mapavi6",
                                    height = ind_map_height
                                ),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    ),
                    card(
                        full_screen = TRUE,
                        withSpinner(
                            DTOutput("tblvi6"),
                            type = 8,
                            color = "#00496d"
                        )
                    )
                )
            ),
            tabPanel(
                title = "Indicador VI.7",
                card(
                    tags$h5("Taxa de câmeras de segurança em capitais em relação à extensão de vias [n / 100 km]"),
                    max_height = "80px"
                ),
                layout_columns(
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                leafletOutput(
                                    "mapavi7",
                                    height = ind_map_height
                                ),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    ),
                    card(
                        full_screen = TRUE,
                        withSpinner(
                            DTOutput("tblvi7"),
                            type = 8,
                            color = "#00496d"
                        )
                    )
                )
            )
        )
    )
)

obitos_panel <- nav_panel(
    value = "obitos",
    title = "Pilar VII - Indicadores de Mortalidade",
    value_box(
        title = "Pilar VII",
        value = "Indicadores de mortalidade",
        height = 100
    ),
    layout_columns(
        col_widths = c(6, 6, 12),
        card(
            card_header("Classificação - Mapa"),
            full_screen = TRUE,
            card_body(
                class = "p-0",
                withSpinner(
                    leafletOutput("mapaobitos", height = class_map_height),
                    type = 8,
                    color = "#00496d"
                )
            )
        ),
        card(
            card_header("Classificação - Tabela"),
            full_screen = TRUE,
            withSpinner(
                DTOutput("tblobitos"),
                type = 8,
                color = "#00496d"
            )
        ),
        tabsetPanel(
            type = "pills",
            tabPanel(
                title = "Indicador VII.1",
                card(
                    tags$h5("Taxa de óbitos por veículos [n / 10.000 veic.]"),
                    max_height = "80px"
                ),
                layout_columns(
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                leafletOutput(
                                    "mapa01",
                                    height = ind_map_height
                                ),
                                type = 8,
                                color = "#00496d"
                            )
                        ),
                    ),
                    card(
                        full_screen = TRUE,
                        withSpinner(
                            DTOutput("tbl01"),
                            type = 8,
                            color = "#00496d"
                        )
                    )
                )
            ),
            tabPanel(
                title = "Indicador VII.2",
                card(
                    tags$h5("Taxa de óbitos por habitantes [n / 100.000 hab.]"),
                    max_height = "80px"
                ),
                layout_columns(
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                leafletOutput(
                                    "mapa02",
                                    height = ind_map_height
                                ),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    ),
                    card(
                        full_screen = TRUE,
                        withSpinner(
                            DTOutput("tbl02"),
                            type = 8,
                            color = "#00496d"
                        )
                    )
                )
            ),
            tabPanel(
                title = "Indicador VII.3",
                card(
                    tags$h5("Taxa de óbitos por bilhão de quilômetros percorridos [n / 10^9 km]"),
                    max_height = "80px"
                ),
                layout_columns(
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                leafletOutput(
                                    "mapa03",
                                    height = ind_map_height
                                ),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    ),
                    card(
                        full_screen = TRUE,
                        withSpinner(
                            DTOutput("tbl03"),
                            type = 8,
                            color = "#00496d"
                        )
                    )
                )
            )
        )
    )
)

pilar_panel <- nav_menu(
    value = "pilar",
    title = "Pilares",
    icon = bsicons::bs_icon("bar-chart"),
    pilar01_panel,
    pilar02_panel,
    pilar03_panel,
    pilar04_panel,
    pilar05_panel,
    pilar06_panel,
    obitos_panel
)

uf_panel <- nav_panel(
    value = "uf",
    title = "Perfil das unidades da federação",
    icon = bsicons::bs_icon("map"),
    value_box(
        title = NULL,
        value = "Perfil das unidades da federação",
        height = 100
    ),
    layout_sidebar(
        sidebar = sidebar(
            selectizeInput(
                inputId = "filteruf",
                label = "Selecione a UF:",
                choices = sort(unique(sf_classificacao$nome_uf)),
                selected = sort(unique(sf_classificacao$nome_uf))[1],
                multiple = FALSE
            )
        ),
        layout_columns(
            value_box(
                title = "UF",
                value = textOutput("ufbox"),
                height = value_box_height
            ),
            value_box(
                title = "Região",
                value = textOutput("regiaobox"),
                height = value_box_height
            ),
            value_box(
                title = "Classificação média",
                value = textOutput("classificacaobox"),
                height = value_box_height
            ),
            value_box(
                title = "Ranking geral",
                value = textOutput("rankbox"),
                height = value_box_height
            )
        ),
        layout_columns(
            card(
                card_header("Benchmarking"),
                full_screen = TRUE,
                card_body(
                    class = "p-0",
                    withSpinner(
                        gt_output("tblbenchmark"),
                        type = 8,
                        color = "#00496d"
                    )
                )
            ),
            card(
                card_header("Classificação"),
                full_screen = TRUE,
                withSpinner(
                    plotlyOutput("radarplot"),
                    type = 8,
                    color = "#00496d"
                )
            )
        ),
        layout_columns(
            tabsetPanel(
                type = "pills",
                tabPanel(
                    "Pilar I",
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                gt_output("tblindpilar1"),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    )
                ),
                tabPanel(
                    "Pilar II",
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                gt_output("tblindpilar2"),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    )
                ),
                tabPanel(
                    "Pilar III",
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                gt_output("tblindpilar3"),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    )
                ),
                tabPanel(
                    "Pilar IV",
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                gt_output("tblindpilar4"),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    )
                ),
                tabPanel(
                    "Pilar V",
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                gt_output("tblindpilar5"),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    )
                ),
                tabPanel(
                    "Pilar VI",
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                gt_output("tblindpilar6"),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    )
                ),
                tabPanel(
                    "Indicadores de resultado",
                    card(
                        full_screen = TRUE,
                        card_body(
                            class = "p-0",
                            withSpinner(
                                gt_output("tblindresultado"),
                                type = 8,
                                color = "#00496d"
                            )
                        )
                    )
                )
            )
        )
    )
)

about_panel <- nav_panel(
    value = "about",
    title = "Sobre",
    icon = bsicons::bs_icon("info-circle"),
    card(
        full_screen = TRUE,
        card_header("Sobre o painel"),
        includeMarkdown("about.md")
    )
)

theme <- bs_theme(
    primary = "#00496d",
    warning = "#f7951d",
    danger = "#d51f29",
    success = "#1fa149"
)


ui <- bslib::page_navbar(
    title = "IRIS - Indicadores Rodoviários Integrados de Segurança",
    nav_spacer(),
    home_panel,
    pilar_panel,
    uf_panel,
    about_panel,
    nav_spacer(),
    nav_spacer(),
    nav_item(
        tags$a(
            href = "https://onsv.org.br",
            target = "_blank",
            tags$img(
                src = "onsv_logo.png",
                height = "25px",
                alt = "Logo"
            )
        )
    ),
    theme = theme,
    bg = "#00496d",
    fillable = FALSE
)

server <- function(input, output, session) {
    # Mapas 
    output$mapageral <- renderLeaflet({
        plot_leaflet_geral(sf_classificacao)
    })
    output$mapapilar1 <- renderLeaflet({
        plot_classificacao_leaflet(sf_classificacao, "Pilar I")
    })
    output$mapai1 <- renderLeaflet({
        plot_indicadores_leaflet(sf_indicadores, "i.1")
    })
    output$mapai2 <- renderLeaflet({
        plot_indicadores_leaflet(sf_indicadores, "i.2")
    })
    output$mapai3 <- renderLeaflet({
        plot_indicadores_leaflet(sf_indicadores, "i.3")
    })
    output$mapai4 <- renderLeaflet({
        plot_indicadores_leaflet(sf_indicadores, "i.4")
    })
    output$mapapilar2 <- renderLeaflet({
        plot_classificacao_leaflet(sf_classificacao, "Pilar II")
    })
    output$mapaii1 <- renderLeaflet({
        plot_indicadores_leaflet(sf_indicadores, "ii.1")
    })
    output$mapaii2 <- renderLeaflet({
        plot_indicadores_leaflet(sf_indicadores, "ii.2")
    })
    output$mapaii3 <- renderLeaflet({
        plot_indicadores_leaflet(sf_indicadores, "ii.3")
    })
    output$mapaii4 <- renderLeaflet({
        plot_indicadores_leaflet(sf_indicadores, "ii.4")
    })
    output$mapaii5 <- renderLeaflet({
        plot_indicadores_leaflet(sf_indicadores, "ii.5")
    })
    output$mapapilar3 <- renderLeaflet({
        plot_classificacao_leaflet(sf_classificacao, "Pilar III")
    })
    output$mapaiii1 <- renderLeaflet({
        plot_indicadores_leaflet(sf_indicadores, "iii.1")
    })
    output$mapaiii2 <- renderLeaflet({
        plot_indicadores_leaflet(sf_indicadores, "iii.2")
    })
    output$mapaiii3 <- renderLeaflet({
        plot_indicadores_leaflet(sf_indicadores, "iii.3")
    })
    output$mapaiii4 <- renderLeaflet({
        plot_indicadores_leaflet(sf_indicadores, "iii.4")
    })
    output$mapapilar4 <- renderLeaflet({
        plot_classificacao_leaflet(sf_classificacao, "Pilar IV")
    })
    output$mapaiv1 <- renderLeaflet({
        plot_indicadores_leaflet(sf_indicadores, "iv.1")
    })
    output$mapaiv2 <- renderLeaflet({
        plot_indicadores_leaflet(sf_indicadores, "iv.2")
    })
    output$mapaiv3 <- renderLeaflet({
        plot_indicadores_leaflet(sf_indicadores, "iv.3")
    })
    output$mapaiv4 <- renderLeaflet({
        plot_indicadores_leaflet(sf_indicadores, "iv.4")
    })
    output$mapaiv5 <- renderLeaflet({
        plot_indicadores_leaflet(sf_indicadores, "iv.5")
    })
    output$mapaiv6 <- renderLeaflet({
        plot_indicadores_leaflet(sf_indicadores, "iv.6")
    })
    output$mapaiv7 <- renderLeaflet({
        plot_indicadores_leaflet(sf_indicadores, "iv.7")
    })
    output$mapapilar5 <- renderLeaflet({
        plot_classificacao_leaflet(sf_classificacao, "Pilar V")
    })
    output$mapav1 <- renderLeaflet({
        plot_indicadores_leaflet(sf_indicadores, "v.1")
    })
    output$mapav2 <- renderLeaflet({
        plot_indicadores_leaflet(sf_indicadores, "v.2")
    })
    output$mapav3 <- renderLeaflet({
        plot_indicadores_leaflet(sf_indicadores, "v.3")
    })
    output$mapav4 <- renderLeaflet({
        plot_indicadores_leaflet(sf_indicadores, "v.4")
    })
    output$mapapilar6 <- renderLeaflet({
        plot_classificacao_leaflet(sf_classificacao, "Pilar VI")
    })
    output$mapavi1 <- renderLeaflet({
        plot_indicadores_leaflet(sf_indicadores, "vi.1")
    })
    output$mapavi2 <- renderLeaflet({
        plot_indicadores_leaflet(sf_indicadores, "vi.2")
    })
    output$mapavi3 <- renderLeaflet({
        plot_indicadores_leaflet(sf_indicadores, "vi.3")
    })
    output$mapavi4 <- renderLeaflet({
        plot_indicadores_leaflet(sf_indicadores, "vi.4")
    })
    output$mapavi5 <- renderLeaflet({
        plot_indicadores_leaflet(sf_indicadores, "vi.5")
    })
    output$mapavi6 <- renderLeaflet({
        plot_indicadores_leaflet(sf_indicadores, "vi.6")
    })
    output$mapavi7 <- renderLeaflet({
        plot_indicadores_leaflet(sf_indicadores, "vi.7")
    })
    output$mapaobitos <- renderLeaflet({
        plot_classificacao_leaflet(sf_classificacao, "Resultado final")
    })
    output$mapa01 <- renderLeaflet({
        plot_indicadores_leaflet(sf_indicadores, "0.1")
    })
    output$mapa02 <- renderLeaflet({
        plot_indicadores_leaflet(sf_indicadores, "0.2")
    })
    output$mapa03 <- renderLeaflet({
        plot_indicadores_leaflet(sf_indicadores, "0.3")
    })

    # Tabelas
    output$gtclasshome <- render_gt({
        make_gt_class_inicio(sf_classificacao)
    })

    output$tblpilar1 <- renderDT({
        make_class_dt(sf_classificacao, "Pilar I")
    })
    output$tbli1 <- renderDT({
        make_ind_dt(sf_indicadores, "i.1")
    })
    output$tbli2 <- renderDT({
        make_ind_dt(sf_indicadores, "i.2")
    })
    output$tbli3 <- renderDT({
        make_ind_dt(sf_indicadores, "i.3")
    })
    output$tbli4 <- renderDT({
        make_ind_dt(sf_indicadores, "i.4")
    })
    output$tblpilar2 <- renderDT({
        make_class_dt(sf_classificacao, "Pilar II")
    })
    output$tblii1 <- renderDT({
        make_ind_dt(sf_indicadores, "ii.1")
    })
    output$tblii2 <- renderDT({
        make_ind_dt(sf_indicadores, "ii.2")
    })
    output$tblii3 <- renderDT({
        make_ind_dt(sf_indicadores, "ii.3")
    })
    output$tblii4 <- renderDT({
        make_ind_dt(sf_indicadores, "ii.4")
    })
    output$tblii5 <- renderDT({
        make_ind_dt(sf_indicadores, "ii.5")
    })
    output$tblpilar3 <- renderDT({
        make_class_dt(sf_classificacao, "Pilar III")
    })
    output$tbliii1 <- renderDT({
        make_ind_dt(sf_indicadores, "iii.1")
    })
    output$tbliii2 <- renderDT({
        make_ind_dt(sf_indicadores, "iii.2")
    })
    output$tbliii3 <- renderDT({
        make_ind_dt(sf_indicadores, "iii.3")
    })
    output$tbliii4 <- renderDT({
        make_ind_dt(sf_indicadores, "iii.4")
    })
    output$tblpilar4 <- renderDT({
        make_class_dt(sf_classificacao, "Pilar IV")
    })
    output$tbliv1 <- renderDT({
        make_ind_dt(sf_indicadores, "iv.1")
    })
    output$tbliv2 <- renderDT({
        make_ind_dt(sf_indicadores, "iv.2")
    })
    output$tbliv3 <- renderDT({
        make_ind_dt(sf_indicadores, "iv.3")
    })
    output$tbliv4 <- renderDT({
        make_ind_dt(sf_indicadores, "iv.4")
    })
    output$tbliv5 <- renderDT({
        make_ind_dt(sf_indicadores, "iv.5")
    })
    output$tbliv6 <- renderDT({
        make_ind_dt(sf_indicadores, "iv.6")
    })
    output$tbliv7 <- renderDT({
        make_ind_dt(sf_indicadores, "iv.7")
    })
    output$tblpilar5 <- renderDT({
        make_class_dt(sf_classificacao, "Pilar V")
    })
    output$tblv1 <- renderDT({
        make_ind_dt(sf_indicadores, "v.1")
    })
    output$tblv2 <- renderDT({
        make_ind_dt(sf_indicadores, "v.2")
    })
    output$tblv3 <- renderDT({
        make_ind_dt(sf_indicadores, "v.3")
    })
    output$tblv4 <- renderDT({
        make_ind_dt(sf_indicadores, "v.4")
    })
    output$tblpilar6 <- renderDT({
        make_class_dt(sf_classificacao, "Pilar VI")
    })
    output$tblvi1 <- renderDT({
        make_ind_dt(sf_indicadores, "vi.1")
    })
    output$tblvi2 <- renderDT({
        make_ind_dt(sf_indicadores, "vi.2")
    })
    output$tblvi3 <- renderDT({
        make_ind_dt(sf_indicadores, "vi.3")
    })
    output$tblvi4 <- renderDT({
        make_ind_dt(sf_indicadores, "vi.4")
    })
    output$tblvi5 <- renderDT({
        make_ind_dt(sf_indicadores, "vi.5")
    })
    output$tblvi6 <- renderDT({
        make_ind_dt(sf_indicadores, "vi.6")
    })
    output$tblvi7 <- renderDT({
        make_ind_dt(sf_indicadores, "vi.7")
    })
    output$tblobitos <- renderDT({
        make_class_dt(sf_classificacao, "Resultado final")
    })
    output$tbl01 <- renderDT({
        make_ind_dt(sf_indicadores, "0.1")
    })
    output$tbl02 <- renderDT({
        make_ind_dt(sf_indicadores, "0.2")
    })
    output$tbl03 <- renderDT({
        make_ind_dt(sf_indicadores, "0.3")
    })

 
    render_ufbox <- reactive({
        req(input$filteruf)
        sf_classificacao |> 
            st_drop_geometry() |> 
            filter(nome_uf == input$filteruf) |> 
            pull(nome_uf) |> 
            unique()
    })

    render_regiaobox <- reactive({
        req(input$filteruf)
        sf_indicadores |> 
            st_drop_geometry() |> 
            filter(nome_uf == input$filteruf) |> 
            pull(regiao_uf) |> 
            unique()
    })

    render_classbox <- reactive({
        req(input$filteruf)
        sf_classificacao |> 
            st_drop_geometry() |> 
            filter(nome_uf == input$filteruf) |> 
            pull(classificacao_numeric) |> 
            mean() |> 
            scales::number(accuracy = 0.01, decimal.mark = ",")
    })

    render_rankbox <- reactive({
        req(input$filteruf)
        sf_classificacao |> 
            st_drop_geometry() |> 
            group_by(nome_uf) |> 
            summarise(media_class = mean(classificacao_numeric)) |> 
            arrange(-media_class) |> 
            mutate(
                ranking = dense_rank(desc(media_class)),
                ranking = glue::glue("{ranking}.°")
            ) |> 
            filter(nome_uf == input$filteruf) |> 
            pull(ranking)
    })

    render_tblindpilar1 <- reactive({
        req(input$filteruf)
        make_results_gt(ind_desc_data, "i.", input$filteruf)
    })

    render_tblindpilar2 <- reactive({
        req(input$filteruf)
        make_results_gt(ind_desc_data, "ii.", input$filteruf)
    })

    render_tblindpilar3 <- reactive({
        req(input$filteruf)
        make_results_gt(ind_desc_data, "iii.", input$filteruf)
    })

    render_tblindpilar4 <- reactive({
        req(input$filteruf)
        make_results_gt(ind_desc_data, "iv.", input$filteruf)
    })

    render_tblindpilar5 <- reactive({
        req(input$filteruf)
        make_results_gt(ind_desc_data, "v.", input$filteruf)
    })

    render_tblindpilar6 <- reactive({
        req(input$filteruf)
        make_results_gt(ind_desc_data, "vi.", input$filteruf)
    })

    render_tblindresultado <- reactive({
        req(input$filteruf)
        make_results_gt(ind_desc_data, "0.", input$filteruf)
    })

    render_radarplot <- reactive({
        req(input$filteruf)
        plot_radar_class(sf_classificacao, input$filteruf)
    })

    render_tblbenchmark <- reactive({
        req(input$filteruf)
        make_gt_bench(sf_classificacao, input$filteruf)
    })

    output$ufbox <- renderText(
        render_ufbox()
    )

    output$regiaobox <- renderText(
        render_regiaobox()
    )

    output$classificacaobox <- renderText(
        render_classbox()
    )

    output$rankbox <- renderText(
        render_rankbox()
    )

    output$tblindpilar1 <- render_gt(
        render_tblindpilar1()
    )

    output$tblindpilar2 <- render_gt(
        render_tblindpilar2()
    )

    output$tblindpilar3 <- render_gt(
        render_tblindpilar3()
    )

    output$tblindpilar4 <- render_gt(
        render_tblindpilar4()
    )

    output$tblindpilar5 <- render_gt(
        render_tblindpilar5()
    )

    output$tblindpilar6 <- render_gt(
        render_tblindpilar6()
    )

    output$tblindresultado <- render_gt(
        render_tblindresultado()
    )

    output$radarplot <- renderPlotly(
        render_radarplot()
    )

    output$tblbenchmark <- render_gt(
        render_tblbenchmark()
    )


}

shinyApp(ui, server)
