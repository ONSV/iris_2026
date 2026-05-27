library(targets)
library(tarchetypes)

tar_option_set(
    packages = c(
        "janitor", "tidyverse", "roadtrafficdeaths", "pdftools", "readxl",
        "driversbr", "arrow", "rvest", "fleetbr", "stringi", "gt", "sf", 
        "geobr", "patchwork", "Hmisc", "onsvplot", "glue", "ggtext", "ggrepel",
        "leaflet", "gganimate", "magick"
    )
)

tar_source(
    files = c(
        # import
        "R/import_datasus_sim.R",
        "R/import_datasus_cnes.R",
        "R/import_cnt.R",
        "R/import_cameras.R",
        "R/import_detrans.R",
        "R/import_dnit.R",
        "R/import_ibge_pib.R",
        "R/import_ibge_pop.R",
        "R/import_osm.R",
        "R/import_renach.R",
        "R/import_renaest.R",
        "R/import_renainf.R",
        "R/import_renavam.R",
        "R/import_snt.R",
        "R/import_taxabilhao.R",
        "R/import_uf.R",
        # Calculo dos indicadores
        "R/indicadores_pilar_1.R",
        "R/indicadores_pilar_2.R",
        "R/indicadores_pilar_3.R",
        "R/indicadores_pilar_4.R",
        "R/indicadores_pilar_5.R",
        "R/indicadores_pilar_6.R",
        "R/indicadores_resultado.R",
        "R/indicadores_join.R",
        # Tabelas
        "R/tbl_method.R",
        "R/tbl_results.R",
        # Indicadores compostos
        "R/compostos.R",
        # Mapas
        "R/mapas.R",
        # Correlação
        "R/correlacao.R",
        # Plots
        "R/plots.R",
        "R/plot_dinamicos.R"
    )
)

source("R/targets_import.R")
source("R/targets_indicadores.R")
source("R/targets_tbl.R")
source("R/targets_compostos.R")
source("R/targets_mapas.R")
source("R/targets_correlacao.R")
source("R/targets_plots.R")

list(
    targets_import,
    targets_indicadores,
    targets_tbl,
    targets_compostos,
    targets_mapas,
    targets_correlacao,
    targets_plots# ,
    # tar_quarto(report_01, "report/01"),
    # tar_quarto(report_02, "report/02")
    # tar_quarto(report_04, "report/04"),
    # tar_quarto(report_05, "report/05")
)
