library(dplyr)
library(sf)



df <- readr::read_csv("data/Food_Environment_Analyses_pointdata_Shelley2024.csv") |>
  dplyr::rename(lat = Y, lon = X) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84", remove = FALSE)

df |>
  sf::st_filter(ontario_shp)
supermarkets <- df |>
  dplyr::filter(type == "supermarket")

supermarkets[262:263, ]
calculate_isochrones()






## analysis 2 isos
targets::tar_load(foodspace)
grocers <- dplyr::filter(
  foodspace,
  type %in% c("grocery") |
    subtype %in% c("health_food", "fruit_vegetable_market", "cultural_grocer")
)


## analysis 2: coverage by at least 2?
targets::tar_load(grocer_isos)
grocer_isos <- sf::st_make_valid(grocer_isos)
targets::tar_load(phhs)

phhs |>
  dplyr::filter(ONS_Name == "ALTA VISTA") |>
  head(n = 1) |>
  dplyr::mutate(covered = purrr::map_int(
    geometry,
    function(x) {
      sf::st_covered_by(x, grocer_isos) |>
        unlist() |>
        sum()
    }
  )) |>
  dplyr::select(phh_id, covered)


phh <- phhs |>
  dplyr::filter(ONS_Name == "ALTA VISTA") |>
  head(n = 1)

phhs_test <- head(phhs, n = 10)

get_hood_grocer_coverage(phhs = phhs, grocer_isos = grocer_isos)
