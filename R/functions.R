# load foodspace filtered to only ontario, for travel analysis
load_ontario_foods <- function(foodspace_filename = "data/Food_Environment_Analyses_pointdata_Shelley2024.csv",
                               ontario_shp_filename = "~/datascience/data/spatial/lpr_000a21a_e/lpr_000a21a_e.shp") {
  ontario_shp <- sf::read_sf(ontario_shp_filename) |>
    dplyr::filter(PRUID == 35) |>
    sf::st_transform(crs = "WGS84")

  readr::read_csv(foodspace_filename) |>
    dplyr::rename(lat = Y, lon = X) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84", remove = FALSE) |>
    sf::st_filter(ontario_shp) |>
    sf::st_drop_geometry()
}


# calculate isochrones for given food locations, 10mins walking and 10 & 15 mins driving, return as flat sf tibble one row per isochrone
calculate_isochrones <- function(df) {
  df |>
    tidyr::nest(data = -new_UID) |>
    dplyr::mutate(isochrones = purrr::map(
      data,
      function(x) {
        drive <- valhallr::isochrone(from = x, costing = "auto", contours = c(10, 15), hostname = "192.168.0.150")
        walk <- valhallr::isochrone(from = x, costing = "pedestrian", contours = 10, hostname = "192.168.0.150")

        dplyr::bind_rows(drive, walk) |>
          dplyr::select(-fill.opacity, -fillColor, -opacity, -fill, -fillOpacity, -color)
      },
      .progress = TRUE
    )) |>
    tidyr::unnest(isochrones) |>
    dplyr::select(-data) |>
    sf::st_as_sf()
}




# wrapper function for convenience
# phhs: phhs for the analysis in sf tibble
# isochrones: isochrones for service locations
# service_locations: tibble with one row per service provider (each needs a corresponding isochrone)
# rurality - specific rurality to investigate
get_coverage <- function(facilities, phhs, isochrones, service_locations, rurality) {
  purrr::map_dfr(facilities, function(facility) facility_coverage_one(phhs, isochrones, service_locations, rurality))
}


# https://github.com/ottawa-Neighbourhood-Study/neighbourhoodstudy
# phhs <- sf::st_join(neighbourhoodstudy::ottawa_phhs, neighbourhoodstudy::ons_gen3_shp)
# isochrones <- supermkt_isos
# service_locations <- dplyr::tibble()
# rurality_filter <- "rural"
# rural_cov <- get_coverage_one(phhs, supermkt_isos, NA, "rural")
get_coverage_one <- function(phhs,
                             isochrones,
                             service_locations,
                             rurality_filter = c("urban", "rural", "suburban", "town")) {
  # extract the name of the rurality if it's a two-word filter parameter
  rurality_name <- stringr::str_extract(rurality_filter, ".*?(?=\\s|$)")

  phhs <- phhs |>
    dplyr::filter(tolower(rurality) == tolower(rurality_name))

  isos <- isochrones #|>
  # dplyr::left_join(service_locations) |>
  # dplyr::select(facility_type, costing, metric, contour)



  # differential filtering based on rurality
  if (rurality_filter == "urban") {
    costing_name <- "pedestrian"
    travel_limit <- 10
  } else if (rurality_filter == "rural") {
    costing_name <- "auto"
    travel_limit <- 15
  } else if (rurality_filter == "town") {
    costing_name <- "auto"
    travel_limit <- 10
  } else if (rurality_filter == "suburban") {
    costing_name <- "auto"
    travel_limit <- 10
  } else {
    stop("invalid rurality_filter parameter")
  }

  # filter isochrones based on conditions
  # (switch spherical geometry off/on so that the results are valid and fast)
  sf::sf_use_s2(FALSE)

  isos <- isos |>
    dplyr::filter(
      costing == costing_name,
      contour == travel_limit
    ) |>
    sf::st_union() |>
    sf::st_make_valid() |>
    suppressMessages()

  sf::sf_use_s2(TRUE)

  phhs$covered <- sf::st_covered_by(phhs, isos) |>
    purrr::map_lgl(length)

  # phhs

  # ggplot2::ggplot() + ggplot2::geom_sf(data = isos) + ggplot2::geom_sf(data = phhs, mapping = ggplot2::aes(colour = covered))

  phhs |>
    sf::st_drop_geometry() |>
    dplyr::group_by(ONS_ID, ONS_Name, rurality) |>
    dplyr::summarise(
      pct_covered = sum(dbpop * covered) / sum(dbpop),
      .groups = "drop"
    ) |>
    dplyr::mutate( # facility_type = facility,
      costing = costing_name,
      travel_limit_mins = travel_limit
    )
}


# load phhs for all CDs surrounding Ottawa
load_big_phhs <- function(cds = c("3501", "3502", "3506", "3507", "3509", "3547"), ons_shp) {
  cds |>
    purrr::map_dfr(function(cd) sf::read_sf(paste0("~/datascience/R/geospatial/phh_generator/output/Ontario-2023-07-17/", cd, "-phhs-2023-07-17.shp"))) |>
    dplyr::filter(dbpop > 0) |>
    sf::st_transform(crs = "WGS84") |>
    sf::st_join(ons_shp) |>
    dplyr::filter(!is.na(rurality))
}



# get neighbourhood-level coverage of population within travel distance
# of 2 grocers
# input set of phhs and tibble of isochrones
get_hood_grocer_coverage <- function(phhs, grocer_isos) {
  grocer_isos <- sf::st_make_valid(grocer_isos)
  results <- dplyr::tibble()

  for (rurality_foranalysis in c("rural", "town", "suburban", "urban")) {
    message(rurality_foranalysis)
    phhs_foranalysis <- dplyr::filter(phhs, rurality == rurality_foranalysis)

    # get the right isochrones for our analysis
    if (rurality_foranalysis == "urban") {
      grocer_isos_foranalysis <- dplyr::filter(
        grocer_isos,
        contour == 10,
        costing == "pedestrian"
      )
    } else if (rurality_foranalysis == "town") {
      grocer_isos_foranalysis <- dplyr::filter(
        grocer_isos,
        contour == 10,
        costing == "auto"
      )
    } else if (rurality_foranalysis == "suburban") {
      grocer_isos_foranalysis <- dplyr::filter(
        grocer_isos,
        contour == 10,
        costing == "auto"
      )
    } else if (rurality_foranalysis == "rural") {
      grocer_isos_foranalysis <- dplyr::filter(
        grocer_isos,
        contour == 15,
        costing == "auto"
      )
    } # end big if!


    phh_coverage_vec <- phhs_foranalysis |>
      sf::st_covered_by(grocer_isos_foranalysis) |>
      purrr::map_int(length)

    phhs_foranalysis$grocer_coverage <- phh_coverage_vec

    result <- phhs_foranalysis |>
      sf::st_drop_geometry() |>
      dplyr::mutate(grocer_coverage_two = grocer_coverage >= 2) |>
      dplyr::group_by(ONS_ID, ONS_Name) |>
      dplyr::summarise(
        pct_coverage = sum(grocer_coverage_two * dbpop) / sum(dbpop),
        .groups = "drop"
      )

    results <- dplyr::bind_rows(results, result)
  } # end rurality_foranalysis in ...

  results |>
    dplyr::mutate(pct_coverage = round(pct_coverage, digits = 3))
} # end function get_phh_grocer_coverage()




get_db_centroids_snapped_to_roads <- function() {
  dbs <- sf::read_sf("~/datascience/data/spatial/ldb_000a21a_e/ldb_000a21a_e.shp") |>
    dplyr::filter(PRUID == 35) |>
    dplyr::mutate(CDUID = substr(DBUID, 0, 4)) |>
    dplyr::filter(CDUID %in% c("3501", "3502", "3506", "3507", "3509", "3547")) |>
    sf::st_transform(crs = "WGS84") |>
    sf::st_make_valid() |>
    sf::st_filter(sf::st_make_valid(sf::st_union(sf::st_make_valid(neighbourhoodstudy::ons_gen3_shp))))

  dbpops <- readr::read_csv("~/datascience/data/spatial/geographic_attribute_file/2021_92-151_X.csv") |>
    dplyr::mutate(DBUID = as.character(DBUID_IDIDU), .before = 1) |>
    dplyr::filter(DBUID %in% dbs$DBUID) |>
    dplyr::select(DBUID, dbpop2021 = DBPOP2021_IDPOP2021)

  db_centroids <- dbs |>
    sf::st_make_valid() |>
    dplyr::left_join(dbpops, by = "DBUID") |>
    dplyr::select(DBUID, dbpop2021) |>
    sf::st_centroid() |>
    sf::st_transform(crs = 32189) |>
    suppressWarnings()

  roads <- sf::st_union(pseudohouseholds::ottawa_roads_shp)

  message("Loading Ontario roads...")
  ontario_roads <-
    sf::read_sf("~/datascience/data/spatial/lrnf000r21a_e/lrnf000r21a_e.shp",
      query = 'SELECT CSDUID_L,CSDUID_R,NGD_UID,NAME,RANK,CLASS FROM "lrnf000r21a_e" WHERE
                             ("PRNAME_L" = \'Ontario\' OR "PRNAME_R" = \'Ontario\') AND
                             (
								("CLASS" IN (\'20\',\'21\',\'22\',\'23\')) OR
								("RANK" IN (\'4\',\'5\') AND "CLASS" IN (\'12\', \'13\')) OR
								("RANK" = \'1\' AND NAME NOT LIKE \'4__%\')
							 ) AND NAME IS NOT NULL'
    )

  cds <- c("3501", "3502", "3506", "3507", "3509", "3547")

  roads <- ontario_roads |>
    dplyr::mutate(CDUID_L = substr(CSDUID_L, 1, 4), CDUID_R = substr(CSDUID_R, 1, 4), .before = 1) |>
    dplyr::filter(CDUID_L %in% cds | CDUID_R %in% cds) |>
    sf::st_union()

  roads <- sf::st_transform(roads, crs = 32189)

  message("Snapping to road segments...")
  step1 <- db_centroids %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      linestring = sf::st_nearest_points(geometry, roads),
      closest_point = sf::st_cast(linestring, "POINT")[seq(2, nrow(.) * 2, 2)]
    )

  step2 <- step1 |>
    sf::st_drop_geometry() |>
    dplyr::rename(geometry = closest_point) |>
    dplyr::select(-linestring) |>
    sf::st_as_sf() |>
    sf::st_transform(crs = "WGS84")

  step2 |>
    dplyr::bind_cols(sf::st_coordinates(step2)) |>
    dplyr::rename(lat = Y, lon = X) |>
    sf::st_drop_geometry()
} # end get_db_centroids_snapped_to_roads()





# experimental function to get rfei requirements--healthy and unhealthy
# food places within travel times of dbs
do_rfei_basic_calc <- function(db_centroids_snapped, foodspace) {
  inputs <- db_centroids_snapped |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84", remove = FALSE) |>
    sf::st_join(neighbourhoodstudy::ons_gen3_shp) |>
    dplyr::filter(dbpop2021 > 0) |>
    sf::st_drop_geometry() |>
    dplyr::filter(
      rurality != "excluded",
      !is.na(rurality)
    )

  foodspace$type |> unique()

  unhealthy_pts <- dplyr::filter(
    foodspace,
    type %in% c("fast_food", "convenience", "retail_with_convenience")
  ) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84") |>
    dplyr::select(name, address)

  healthy_pts <- dplyr::filter(
    foodspace,
    type %in% c("supermarket", "grocery", "health_food") |
      subtype %in% c("health_food", "fruit_vegetable_market", "cultural_grocer")
  ) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84") |>
    dplyr::select(name, address)

  # (all fast_food, convenience, and retailer_with_convenience marked as “unhealthy”) /
  #  (all supermarket, grocery, health_food, fruit_and_vegetable_market, and cultural_retailer marked as “healthy” + all fast_food, convenience, and retailer_with_convenience marked as “unhealthy”) within x distance of each household/DB within each neighbourhood


  future::plan(future::multisession, workers = 5)

  # 13.2s with purrr
  # 4.3s furrr with 5 workers
  # 7.3 furrr with 10 workers
  sf::sf_use_s2(FALSE)

  results <- inputs |>
    # dplyr::group_by(rurality) |>
    dplyr::slice_head(n = 50) |>
    dplyr::ungroup() |>
    tidyr::nest(data = -DBUID) |>
    # dplyr::slice(654) |>
    # dplyr::mutate(result = purrr::map(data, function(df) {
    dplyr::mutate(result = furrr::future_map(
      data,
      function(df, healthy_shp = healthy_pts, unhealthy_shp = unhealthy_pts) {
        # print(df)
        if (df$rurality == "rural") {
          costing <- "auto"
          distance <- 15
        } else if (df$rurality %in% c("town", "suburban")) {
          costing <- "auto"
          distance <- 10
        } else if (df$rurality == "urban") {
          costing <- "pedestrian"
          distance <- 10
        } else {
          stop("Unknown rurality type")
        }

        num_healthy <- NA
        num_unhealthy <- NA

        iso <- try(valhallr::isochrone(from = df, costing = costing, contours = distance, hostname = "192.168.0.150") |>
          sf::st_make_valid())

        # create an error for testing...
        # if (runif(n = 1) > 0.5) iso <- "something weird"
        num_unhealthy <- try(sf::st_filter(unhealthy_shp, iso) |> nrow())
        num_healthy <- try(sf::st_filter(healthy_shp, iso) |> nrow())

        if (!is.numeric(num_unhealthy)) num_unhealthy <- NA
        if (!is.numeric(num_healthy)) num_healthy <- NA

        dplyr::tibble(num_unhealthy = num_unhealthy, num_healthy = num_healthy)
      },
      .progress = TRUE
    )) |>
    tidyr::unnest(result) |>
    suppressWarnings() |>
    suppressMessages()

  results
  sf::sf_use_s2(TRUE)
  future::plan(future::sequential)

  # object.size(results)
  results |>
    tidyr::unnest(cols = c(data))
} # end function rfei_basic_calc()
