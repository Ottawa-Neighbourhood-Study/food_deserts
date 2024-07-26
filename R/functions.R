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
get_phh_grocer_coverage <- function(phhs, grocer_isos) {
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
