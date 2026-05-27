library(sf)
library(osmdata)
library(tidyverse)

names <- c(
    "Rio Branco, Acre", "Maceió, Alagoas", "Macapá, Amapá", "Manaus, Amazonas",
    "Salvador, Bahia", "Fortaleza, Ceará", "Brasília, Distrito Federal",
    "Vitória, Espírito Santo", "Goiânia, Goiás", "São Luís, Maranhão", 
    "Cuiabá, Mato Grosso", "Campo Grande, Mato Grosso do Sul",
    "Belo Horizonte, Minas Gerais", "Belém, Pará", 
    "João Pessoa, Paraíba", "Curitiba, Paraná", "Recife, Pernambuco",
    "Teresina, Piauí", 
    "Rio de Janeiro, Rio de Janeiro", "Natal, Rio Grande do Norte",
    "Porto Alegre, Rio Grande do Sul", 
    "Porto Velho, Rondônia", "Boa Vista, Roraima",
    "Florianópolis, Santa Catarina", 
    "São Paulo, São Paulo", "Aracaju, Sergipe", "Palmas, Tocantins"
)

read_osm_data <- function(name_city) {
  bbox <- getbb(name_city, format_out = "polygon", featuretype = "city")
  message(paste0("Reading ", name_city, "..."))
  query_results <- opq(bbox = bbox, osm_types = "way", timeout = 1200) |> 
    add_osm_features(
      features = list(
        "highway" = "motorway",
        "highway" = "trunk",
        "highway" = "primary",
        "highway" = "secondary",
        "highway" = "tertiary",
        "highway" = "road",
        "highway" = "residential",
        "highway" = "motorway_link",
        "highway" = "trunk_link",
        "highway" = "primary_link",
        "highway" = "secondary_link",
        "highway" = "tertiary_link"
      )
    ) |> 
    osmdata_sf()

  axis <- query_results$osm_lines |> 
    select(osm_id) |> 
    st_transform(crs = 4674)

  file_name <- paste0("data-raw/osm/", name_city, ".geojson")
  
  st_write(axis, file_name, append = FALSE)
}

walk(names, read_osm_data)
