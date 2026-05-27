###### data-raw/osm/raw_osm.R faz o download dos geojson ######
###### Eles não foram inseridos no repo devido ao tamanho dos arquivos #####

calc_osm_dist <- function(path) {
    sf_city <- st_read(path)
    road_length <- sum(st_length(sf_city))
    file_name <- str_split_i(path, "/", 3)
    city_state <- str_split_i(file_name, "\\.", 1)
    city_name <- str_split_i(city_state, ", ", 1)
    state_name <- str_split_i(city_state, ", ", 2)
    
    tibble(
        nome_mun = city_name,
        nome_uf = state_name,
        dist_vias = road_length
    )
}