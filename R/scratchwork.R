library(dplyr)
library(sf)



df <- readr::read_csv("data/Food_Environment_Analyses_pointdata_Shelley2024.csv") |>
  dplyr::rename(lat=Y,lon=X) |>
  sf::st_as_sf(coords=c("lon", "lat"), crs="WGS84", remove = FALSE)

df |>
  sf::st_filter(ontario_shp)
supermarkets <- df |>
  dplyr::filter(type == "supermarket")

supermarkets[262:263,]
calculate_isochrones()





