library(usethis)
library(magrittr)
options(dplyr.summarise.inform = FALSE)


#' rawhbs_eu
#'
#' Function to process raw data from the Household Budget Survey (HBS) for each year and country.
#' @param year year/s of the HBS to process. Available options: 2010, 2015, 2020.
#' @param country code of the country or countries of the HBS to process. By default, it processes all available data in the working directory To see the available countries and codes, run country_code().
#' @param path Local path to the folder where the HBS's are stored. Not included in the package.
#' @importFrom dplyr %>%
#' @return RData file with the HBS microdata for each selected country and year.
#' @export
rawhbs_eu <- function(year, country = "all", path) {

  # Check year
  available_years <- c(2010, 2015, 2020)

  # Extract the years that are not available
  invalid_years <- year[!year %in% available_years]

  # If there is one invalid year, show a singular message
  if (length(invalid_years) == 1) {
    stop(sprintf("You introduced the year %s which is not available. Possible options are: %s.",
                 invalid_years, paste(available_years, collapse = ", ")))
  } else if (length(invalid_years) > 1) {
    # If there are multiple invalid years, show a plural message
    stop(sprintf("You introduced the years %s which are not available. Possible options are: %s.",
                 paste(invalid_years, collapse = ", "), paste(available_years, collapse = ", ")))
  }

  # Check country
  available_country <- country$code

  # Extract the years that are not available
  invalid_country <- country[!country %in% available_country]

  # Check countries
  if (country == "all") {
    country <- "all"
  } else {
    # If there is one invalid country, show a singular message
    if (length(invalid_country) == 1) {
      stop(sprintf("You introduced the country code %s which is not available. Possible options are: %s.",
                   invalid_years, paste(available_country, collapse = ", ")))
    } else if (length(invalid_country) > 1) {
      # If there are multiple invalid countries, show a plural message
      stop(sprintf("You introduced the countries %s which are not available. Possible options are: %s.",
                   paste(invalid_country, collapse = ", "), paste(available_country, collapse = ", ")))
  }

  # Loop to process raw data for each year
  for (y in year) {

    if (country == "all") {
      # Get the path of the year folder
      year_path <- file.path(path, y)

      # List xlsx files in the folder
      xlsx_files <- list.files(year_path, pattern = "\\.xlsx$", full.names = FALSE)

      # Extract country codes (first two characters of the file names)
      if (y == 2020) {
        countries <- unique(substr(xlsx_files, nchar(xlsx_files) - 6, nchar(xlsx_files) - 5))
      } else {
        countries <- unique(substr(xlsx_files, 1, 2))
      }
    } else {
      countries <- country
    }

    # Loop to process raw data for each country
    for (c in countries){

      if (y == 2010) {

        # Households file
        hbs_h <- read.xlsx(  paste0(path, "/calculation/rawdata/", y,"/", c, "_HBS_hh",".xlsx") ,
                             colNames = TRUE)

        # Members file
        hbs_m <- read.xlsx(  paste0(path, "/calculation/rawdata/", y,"/", c, "_HBS_hm",".xlsx") ,
                             colNames = TRUE)

      } else if (y == 2015) {

        # Households file
        hbs_h <- read.xlsx(  paste0(path, "/calculation/rawdata/", y,"/", c, "_MFR_hh",".xlsx") ,
                             colNames = TRUE)

        # Members file
        hbs_m <- read.xlsx(  paste0(path, "/calculation/rawdata/", y,"/", c, "_MFR_hm",".xlsx") ,
                             colNames = TRUE)

      } else {

        # Households file
        hbs_h <- read.xlsx(  paste0(path, "/calculation/rawdata/", y,"/HBS_HH_", c, ".xlsx") ,
                             colNames = TRUE)

        # Members file
        hbs_m <- read.xlsx(  paste0(path, "/calculation/rawdata/", y,"/HBS_HM_", c, ".xlsx") ,
                             colNames = TRUE)
      }

      # Set working directory
      setwd( paste0(path, "/calculation/inputs/", c,"/") )

      # Save data
      save( list = c( "hbs_h"  ,
                      "hbs_m"  ), file = paste0("hbs_", y, "_", c, ".RData") )

    }
  }
  }
}
