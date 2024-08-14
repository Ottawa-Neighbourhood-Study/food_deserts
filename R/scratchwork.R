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


## explore db errors

errors <- dplyr::filter(rfei_dbs, is.na(num_unhealthy) | is.na(num_healthy))

dbs <- db_centroids_snapped |>
  dplyr::filter(DBUID %in% errors$DBUID)

do_rfei_basic_calc(dbs, foodspace)


# explore moving to hood level measures
library(sf)
library(ggplot2)
targets::tar_load(rfei_dbs)
targets::tar_load(db_centroids_snapped)
targets::tar_load(foodspace)
ons_shp <- neighbourhoodstudy::ons_gen3_shp
ons_shp
rfei_dbs |>
  dplyr::filter(num_unhealthy == 0 & num_healthy == 0)

rfei_dbs |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84", remove = FALSE) |>
  ggplot() +
  geom_sf(aes(colour = ONS_Name)) +
  theme(legend.position = "none")

rfei_dbs |>
  dplyr::mutate(rfei_db = num_unhealthy / (num_unhealthy + num_healthy)) |>
  dplyr::filter(is.nan(rfei_db)) |>
  dplyr::slice_sample(n = 1) |>
  dplyr::select(-num_unhealthy, -num_healthy, -rfei_db, -rurality, -dplyr::starts_with("ONS")) |>
  do_rfei_basic_calc(foodspace)

foodspace |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84") |>
  ggplot() +
  geom_sf()
sf::st_join(ons_shp)


rfei_hoods <- rfei_dbs |>
  dplyr::mutate(rfei_db = num_unhealthy / (num_unhealthy + num_healthy)) |>
  dplyr::filter(!is.nan(rfei_db)) |>
  dplyr::group_by(ONS_ID, ONS_Name) |>
  dplyr::summarise(
    rfei_hood = sum(rfei_db * dbpop2021) / sum(dbpop2021),
    .groups = "drop"
  )



ons_shp |>
  dplyr::filter(!ONS_ID %in% rfei_hoods$ONS_ID) |>
  ggplot() +
  geom_sf(aes(fill = "ONS_ID")) +
  geom_sf(data = healthy_pts, colour = "red") +
  geom_sf(data = unhealthy_pts, colour = "blue")


ons_shp |>
  dplyr::filter(!ONS_ID %in% rfei_hoods$ONS_ID) |>
  leaflet::leaflet() |>
  leaflet::addTiles() |>
  leaflet::addPolygons() |>
  leaflet::addMarkers(data = healthy_pts, label = "healthy") |>
  leaflet::addMarkers(data = unhealthy_pts, label = "unhealthy")



rfei_dbs |>
  ggplot() +
  geom_point(aes(x = num_unhealthy, y = num_healthy, colour = num_unhealthy / (num_unhealthy + num_healthy))) +
  theme(legend.position = "bottom")

rfei_dbs |>
  ggplot() +
  geom_point(aes(x = (num_unhealthy + num_healthy), y = num_unhealthy / (num_unhealthy + num_healthy)))


rfei_dbs |>
  ggplot() +
  geom_point(aes(x = (num_unhealthy + num_healthy), y = num_unhealthy / num_healthy))


rfei_dbs |>
  dplyr::filter(num_unhealthy + num_healthy > 300)

ons_shp |>
  dplyr::filter(ONS_Name == "EDWARDS - CARLSBAD SPRINGS") |>
  ggplot() +
  geom_sf() +
  geom_sf(data = healthy_pts, colour = "red") +
  geom_sf(data = unhealthy_pts, colour = "blue")


rfei_dbs |>
  ggplot() +
  geom_point(aes(x = (num_unhealthy + num_healthy), y = num_unhealthy / num_healthy, colour = rurality))

# hood analysis
rfei_hoods <- rfei_dbs |>
  dplyr::mutate(rfei_db = num_unhealthy / (num_unhealthy + num_healthy)) |>
  dplyr::filter(!is.nan(rfei_db)) |>
  dplyr::group_by(ONS_ID, ONS_Name) |>
  dplyr::summarise(
    rfei_hood = sum(rfei_db * dbpop2021) / sum(dbpop2021),
    .groups = "drop"
  )

library(leaflet)

pal <- leaflet::colorNumeric(domain = rfei_hoods$rfei_hood, palette = "viridis")
dplyr::left_join(ons_shp, rfei_hoods) |>
  leaflet() |>
  addTiles() |>
  addPolygons(
    fillColor = ~ pal(rfei_hood), fillOpacity = 0.8, weight = 1,
    label = ~ paste0(ONS_Name, ": ", rfei_hood)
  )



####

targets::tar_load(c(rfei_dbs, rfei_sub_town_walk_dbs))
