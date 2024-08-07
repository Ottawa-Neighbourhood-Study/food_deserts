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


targets::tar_load(ons_shp)
library(ggplot2)

grocers <- dplyr::filter(
  foodspace,
  type %in% c("grocery", "supermarket") |
    subtype %in% c("health_food", "fruit_vegetable_market", "cultural_grocer")
) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84")



## THISP LOT TO SEND TO SHELLEY TOO
dplyr::left_join(ons_shp, hood_grocer_cov) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill = pct_coverage)) +
  ggplot2::geom_sf(data = grocers, colour = "red") +
  scale_fill_continuous(labels = scales::percent) +
  labs(
    title = "ONS Neighbourhoods: Grocer Coverage",
    subtitle = "% of Population covered by 2 or more grocers, plus grocer locations",
    fill = "% Pop. Cov."
  )



## This PLOT TO SEND TO SHELLEY AS CHECK ON LOW URBAN RESULTS
forplot <- dplyr::left_join(ons_shp, hood_grocer_cov)
grocer_isos |>
  dplyr::filter(lon > -75.84, lon < -75.58, lat > 45.3, lat < 45.5) |>
  dplyr::filter(costing == "pedestrian") |>
  ggplot() +
  geom_sf(
    data = dplyr::filter(forplot, rurality == "urban"),
    mapping = aes(fill = pct_coverage)
  ) +
  geom_sf() +
  scale_fill_continuous(labels = scales::percent) +
  labs(
    title = "Urban Neighbourhoods",
    subtitle = "% of Pop. Covered by 2 or more grocers,\nplus grocery 10-minute walking isochrones",
    fill = "% Pop. Cov."
  )

grocer_isos |>
  dplyr::filter(lon > -75.84, lon < -75.58, lat > 45.3, lat < 45.5) |>
  dplyr::filter(costing == "pedestrian") |>
  ggplot() +
  geom_sf(data = dplyr::filter(ons_shp, rurality == "urban"), mapping = ) +
  geom_sf()

grocer_isos |>
  dplyr::filter(lon > -75.84, lon < -75.58, lat > 45.3, lat < 45.5) |>
  dplyr::filter(costing == "pedestrian") |>
  leaflet::leaflet() |>
  leaflet::addTiles() |>
  leaflet::addCircleMarkers(data = dplyr::filter(phhs, rurality == "urban"), color = "red") |>
  leaflet::addPolygons()



### LOOKING AT ISOCHRONES FOR DBS IN BIG ONS REGION
library(sf)
targets::tar_load(foodspace)
targets::tar_load(db_centroids_snapped)


db_centroids_snapped

# ruralities <- neighbourhoodstudy::ons_gen3_shp |>
# sf::st_drop_geometry() |>
# dplyr::select(ONS_ID, rurality)


# inputs <- db_centroids_snapped |>
#   dplyr::mutate(DAUID = substr(DBUID, 1, 8)) |>
#   dplyr::left_join(neighbourhoodstudy::sli_das_gen3_mape, by = "DAUID") |>
#   dplyr::left_join(ruralities, by = "ONS_ID")




#####
inputs <- db_centroids_snapped |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84", remove = FALSE) |>
  sf::st_join(neighbourhoodstudy::ons_gen3_shp) |>
  dplyr::filter(dbpop2021 > 0)
