# library(tidyr)
# library(dplyr)
# library(magrittr)
# library(ggplot2)
# library(rgcam)
# library(gcamdata)
# library(countrycode)
# library(tibble)

#------------------------
#' get_prices_gcam
#'
#' Extract and format prices from a GCAM-Europe database or project file for MEDUSA
#' @keywords GCAM, GCAM-Europe, prices
#' @return a dataframe with prices by sector and GCAM-Europe region
#' @param db_path Path to the GCAM database
#' @param query_path Path to the query file
#' @param db_name Name of the GCAM database
#' @param prj_name Name of the rgcam project. This can be an existing project, or, if not, this will be the name
#' @param prj rgcam loaded project
#' @param scenarios Vector names of the GCAM scenarios to be processed
#' @param queries Name of the GCAM query file. The file by default includes the queries required to run rfasst
#' @param final_db_year Final year in the GCAM database (this allows to process databases with user-defined "stop periods")
#' @param saveOutput Writes the files.By default=T
#' @param base_scen The base scenario that other scenarios will be compared against
#' @param selected_year The year of analysis, when the scenarios wil be compared
#' @importFrom magrittr %>%
#' @export
get_prices_gcam <- function(db_path = NULL, query_path = "inst/extdata", db_name = NULL, prj_name, prj = NULL,
                            scenarios, queries = "queries_GCAM_MEDUSA.xml", final_db_year = 2100,
                            saveOutput = T, base_scen, selected_year) {

  db_path <- 'C:\\Users\\claudia.rodes\\Documents\\GitHub\\gcam-core-eu\\output'
  db_name <- 'database_basexdb_heatpumps2'
  query_path = "inst/extdata"
  prj_name = 'medusa.dat'
  prj = NULL
  scenarios = c('Reference_HeatPumps17_2100','Reference_heatpumps')
  queries = "queries_GCAM_MEDUSA.xml"
  final_db_year = 2030
  saveOutput = T
  base_scen = 'Reference_heatpumps'
  selected_year = 2015


  # Set countries
  EU_COUNTRIES <- c("Austria", "Belgium", "Bulgaria", "Croatia",
                    "Cyprus", "Czech Republic", "Denmark",
                    "Estonia", "Finland", "France", "Germany",
                    "Greece", "Hungary", "Ireland", "Italy",
                    "Latvia", "Lithuania", "Luxembourg", "Malta",
                    "Netherlands", "Poland", "Portugal", "Romania",
                    "Slovakia", "Slovenia", "Spain", "Sweden")

  crop_prod_COUNTRY <- "Austria"

  # Set crop coicop codes (for adjustment)
  crop_coicop_codes <- c("CP01111",
                         "CP01112",
                         "CP01154",
                         "CP0116",
                         "CP01163",
                         "CP0117",
                         "CP01176",
                         "CP01181")

  # Set delivered biomass code (not working well)
  deliv_bio_coicop <- "CP04549"

  # Load price mapping files
  map_price <- medusa::mapping_gcam_medusa %>%
    dplyr::filter(query == "prices by sector") %>%
    dplyr::select(sector = gcam) %>%
    dplyr::distinct() %>%
    dplyr::pull()

  map_cost <- medusa::mapping_gcam_medusa %>%
    dplyr::filter(query == "costs by subsector") %>%
    dplyr::select(sector = gcam) %>%
    dplyr::distinct() %>%
    dplyr::pull()

  # Load the rgcam project if prj not passed as a parameter:
  if (is.null(prj)) {
    if (!is.null(db_path) & !is.null(db_name)) {
      rlang::inform('Creating project ...')
      conn <- rgcam::localDBConn(db_path,
                                 db_name,migabble = FALSE)

      prj <- rgcam::addScenario(conn = conn,
                                proj = prj_name,
                                scenario = scenarios,
                                queryFile = paste0(query_path,"/",queries),
                                clobber = F,
                                saveProj = F)

      if (!file.exists('output')) dir.create('output')
      rgcam::saveProject(prj, file = file.path('output',prj_name))

      QUERY_LIST <- c(rgcam::listQueries(prj, c(scenarios)))
    } else {
      rlang::inform('Loading project ...')
      prj <- rgcam::loadProject(prj_name)

      QUERY_LIST <- c(rgcam::listQueries(prj, c(scenarios)))
    }
  } else {
    QUERY_LIST <- c(rgcam::listQueries(prj, c(scenarios)))
  }


  rlang::inform('Extracting prices ...')


  prices <- rgcam::getQuery(prj, "prices by sector") %>%
    dplyr::filter(region %in% EU_COUNTRIES,
                  sector %in% map_price)

  costs <- rgcam::getQuery(prj, "costs by subsector") %>%
    dplyr::filter(region %in% EU_COUNTRIES,
                  subsector %in% map_cost) %>%
    dplyr::select(-sector) %>%
    dplyr::rename(sector = subsector)


  data <- dplyr::bind_rows(
    prices,
    costs
  )

  # -----
  # Aggregate to COICOP categories
  coicop_map <- medusa::mapping_gcam_medusa %>%
    dplyr::select(-query) %>%
    dplyr::rename(sector = gcam)

  data_coicop <- data %>%
    dplyr::filter(year == selected_year) %>%
    dplyr::left_join(coicop_map, by = "sector", relationship = "many-to-many") %>%
    dplyr::group_by(scenario, region, coicop, year) %>%
    dplyr::summarise(value = mean(value)) %>%
    dplyr::ungroup()

  # -----
  # Compute price changes in relation to base_scen
  data_coicop_diff <- data_coicop %>%
    dplyr::filter(scenario != base_scen) %>%
    gcamdata::left_join_error_no_match(
      data_coicop %>%
        dplyr::filter(scenario == base_scen) %>%
        dplyr::rename(value_base = value) %>%
        dplyr::select(-scenario),
      by = c("region", "coicop", "year")
    ) %>%
    # filter out negative prices from delivered biomass
    dplyr::filter(coicop != deliv_bio_coicop) %>%
    dplyr::mutate(price_diff = value / value_base)


  # With the new structure, we need to adjust price changes in crops: The price change in Austria
  # should be extended to all EU countries
  data_coicop_adjCrop <- data_coicop_diff %>%
    dplyr::filter(region == 'Austria') %>%
    dplyr::filter(coicop %in% crop_coicop_codes) %>%
    dplyr::select(-region) %>%
    tibble::as_tibble() %>%
    gcamdata::repeat_add_columns(tibble::tibble(region = EU_COUNTRIES))


  # Create the final dataset:
  data_coicop_fin <- data_coicop_diff %>%
    dplyr::filter(!coicop %in% crop_coicop_codes) %>%
    dplyr::bind_rows(
      data_coicop_adjCrop
    ) %>%
    dplyr::mutate(
      eurostat_code = countrycode::countrycode(region, origin = "country.name", destination = "eurostat")
    ) %>%
    dplyr::mutate(ctry_sce = paste0(eurostat_code, "_", scenario)) %>%
    dplyr::select(ctry_sce, coicop, price_diff) %>%
    dplyr::mutate(price_diff = dplyr::if_else(is.nan(price_diff), 1, price_diff)) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(
      names_from = ctry_sce,
      values_from = price_diff
    )

  write.csv(data_coicop_fin, file = file.path('output',paste0('data_coicop_fin_',stringr::str_remove(prj_name,'.dat'),'.csv')))

  # -------
  # Add all the remaining coicop categories (with no change, value = 1)
  data_coicop_fin_full <- data_coicop_diff %>%
    dplyr::filter(!coicop %in% crop_coicop_codes) %>%
    dplyr::bind_rows(
      data_coicop_adjCrop
    ) %>%
    dplyr::mutate(
      eurostat_code = countrycode::countrycode(region, origin = "country.name", destination = "eurostat")
    ) %>%
    dplyr::mutate(ctry_sce = paste0(eurostat_code, "_", scenario)) %>%
    dplyr::select(ctry_sce, coicop, price_diff) %>%
    dplyr::mutate(price_diff = dplyr::if_else(is.nan(price_diff), 1, price_diff)) %>%
    tidyr::complete(tidyr::nesting(ctry_sce), coicop = medusa::all_coicop) %>%
    tidyr::replace_na(list(price_diff = 1)) %>%
    tidyr::pivot_wider(
      names_from = ctry_sce,
      values_from = price_diff
    )
  # -------

  return(invisible(data_coicop_fin_full))

}

