library(dplyr)
library(sf)

source("R/functions.R")

list(
  targets::tar_target(ons_shp, neighbourhoodstudy::ons_gen3_shp),
  targets::tar_target(phhs, load_big_phhs(cds = c("3501", "3502", "3506", "3507", "3509", "3547"), ons_shp = ons_shp)),
  targets::tar_target(foodspace, load_ontario_foods(
    foodspace_filename = "data/Food_Environment_Analyses_pointdata_Shelley2024-2024-08-06.csv",
    ontario_shp_filename = "~/datascience/data/spatial/lpr_000a21a_e/lpr_000a21a_e.shp"
  )),
  targets::tar_target(supermkt_isos, calculate_isochrones(dplyr::filter(foodspace, type == "supermarket"))),
  targets::tar_target(rural_cov, get_coverage_one(phhs, supermkt_isos, NA, "rural")),
  targets::tar_target(urban_cov, get_coverage_one(phhs, supermkt_isos, NA, "urban")),
  targets::tar_target(suburban_cov, get_coverage_one(phhs, supermkt_isos, NA, "suburban")),
  targets::tar_target(town_cov, get_coverage_one(phhs, supermkt_isos, NA, "town")),
  targets::tar_target(all_cov, dplyr::bind_rows(rural_cov, urban_cov, town_cov, suburban_cov)),
  targets::tar_target(save, {
    readr::write_csv(all_cov, paste0("output/supermarket-coverage-", Sys.Date(), ".csv"))
    TRUE
  }),

  # analysis 2: coverage of 2 or more grocers
  targets::tar_target(
    grocer_isos,
    calculate_isochrones(dplyr::filter(
      foodspace,
      type %in% c("grocery") |
        subtype %in% c("health_food", "fruit_vegetable_market", "cultural_grocer")
    )) |>
      dplyr::bind_rows(supermkt_isos) |>
      sf::st_as_sf() |>
      dplyr::left_join(foodspace, by = "new_UID")
  ),

  # save the coverage results to disk
  targets::tar_target(
    hood_grocer_cov,
    get_hood_grocer_coverage(phhs = phhs, grocer_isos = grocer_isos)
  ),
  targets::tar_target(
    save_analysis_2,
    {
      readr::write_csv(
        dplyr::arrange(hood_grocer_cov, ONS_ID),
        sprintf("output/analysis_2-hood-level-coverage-2-or-more-grocers-%s.csv", Sys.Date())
      )
      TRUE
    }
  ),

  # save the set of grocers used also for confirmation
  targets::tar_target(
    save_analysis_2_grocers,
    {
      readr::write_csv(dplyr::filter(
        foodspace,
        type %in% c("grocery", "supermarket") |
          subtype %in% c("health_food", "fruit_vegetable_market", "cultural_grocer")
      ), sprintf("output/analysis_2-grocers-%s.csv", Sys.Date()))
      TRUE
    }
  ),
  NULL
)
