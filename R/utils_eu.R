library(usethis)
library(magrittr)
options(dplyr.summarise.inform = FALSE)


#' standardize_eu
#'
#' Function to standarize data names.
#' @param data dataset to be standardized.
#' @param mapping mapping file to be used (included in medusa)
#' @importFrom dplyr %>%
#' @return a dataset with the variables and labels renamed based in the mapping included in the package.
#' @export
standardize_eu <- function(data, mapping) {
  # rename columns
  old_names = colnames(data)                                                # creates a vector with the column names
  new_names = dplyr::left_join(data.frame(VAR_EPF = old_names),             # the vector of column names is converted to a df and the column name is VAR_EPF
                               mapping %>%
                                 dplyr::select(VAR_EPF, VAR_EN) %>%         # within the mapping selects only the columns we are interested in
                                 dplyr::distinct(.),                        # duplicate remove
                               by = 'VAR_EPF') %>%                          # left join in function to var_epf
    dplyr::mutate(VAR_EN = ifelse(is.na(VAR_EN), VAR_EPF, VAR_EN)) %>%      # when it is na you get var_epf and if it is not you get var
    dplyr::pull(VAR_EN)                                                     # only keep the var column (as a vector)
  colnames(data) = new_names                                                # assign to the column names the new vector

  # rename values' codes to values' names for all items whose name is in the
  # renamed mapping's column and have not NA values
  ccitems = mapping %>%
    dplyr::filter(VAR_EN %in% intersect(unique(mapping$VAR_EN), new_names),
                  !is.na(value)) %>%
    dplyr::pull(VAR_EN) %>%
    unique()
  for (cc in ccitems) {                                                     # for all columns where variables can be mapped we overwrite the standardised dataset (with rename_value function)
    data = rename_values(data, cc)
  }

  return(data)
}


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

  # Check if the folder for the selected year exists in the given path
  missing_folders <- year[!dir.exists(file.path(path, as.character(year)))]

  # If there are missing folders, show an error message
  if (length(missing_folders) > 0) {
    stop(sprintf("The folder(s) for the year(s) %s do not exist in the specified path: %s. Please check your data directory.",
                 paste(missing_folders, collapse = ", "), path))
  }

  # Check countries
  if (country == "all") {
    country <- "all"
  } else {
    # Check country
    available_country <- av_country$code

    # Extract the years that are not available
    invalid_country <- country[!country %in% available_country]

    # If there is one invalid country, show a singular message
    if (length(invalid_country) == 1) {
      stop(sprintf("You introduced the country code %s which is not available. Possible options are: %s.",
                   invalid_years, paste(available_country, collapse = ", ")))
    } else if (length(invalid_country) > 1) {
      # If there are multiple invalid countries, show a plural message
      stop(sprintf("You introduced the countries %s which are not available. Possible options are: %s.",
                   paste(invalid_country, collapse = ", "), paste(available_country, collapse = ", ")))
    }
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

      if (y == 2020) {
        countries <- unique(substr(xlsx_files, nchar(xlsx_files) - 6, nchar(xlsx_files) - 5))
      } else {
        countries <- unique(substr(xlsx_files, 1, 2))
      }
      # Verify that the selected country exists in the extracted country codes
      invalid_selected <- country[!country %in% countries]

      if (length(invalid_selected) > 0) {
        stop(sprintf("The selected country code(s) %s do not exist in your data for the year %s. Available countries: %s.",
                     paste(invalid_selected, collapse = ", "), y, paste(countries, collapse = ", ")))
      }
      countries <- country
    }

    # Loop to process raw data for each country
    for (c in countries){

      if (y == 2010) {

        # Households file
        hbs_h <- openxlsx::read.xlsx(  paste0(path, y, "/", c, "_HBS_hh",".xlsx") ,
                                       colNames = TRUE)

        # Members file
        hbs_m <- openxlsx::read.xlsx(  paste0(path, y, "/", c, "_HBS_hm",".xlsx") ,
                                       colNames = TRUE)

      } else if (y == 2015) {

        # Households file
        hbs_h <- openxlsx::read.xlsx(  paste0(path, y, "/", c, "_MFR_hh",".xlsx") ,
                                       colNames = TRUE)

        # Members file
        hbs_m <- openxlsx::read.xlsx(  paste0(path, y, "/", c, "_MFR_hm",".xlsx") ,
                                       colNames = TRUE)

      } else {

        # Households file
        hbs_h <- openxlsx::read.xlsx(  paste0(path, y, "/HBS_HH_", c, ".xlsx") ,
                                       colNames = TRUE)

        # Members file
        hbs_m <- openxlsx::read.xlsx(  paste0(path, y, "/HBS_HM_", c, ".xlsx") ,
                                       colNames = TRUE)
      }

      # Set working directory and create necessary folders
      inputs_path <- file.path(path, "inputs")

      # Create "inputs" folder if it does not exist
      if (!dir.exists(inputs_path)) {
        dir.create(inputs_path)
      }

      # Create a subfolder for each country inside "inputs"
      country_path <- file.path(inputs_path, c)

      if (!dir.exists(country_path)) {
        dir.create(country_path)
      }

      # Save data inside the correct folder
      save(list = c("hbs_h", "hbs_m"), file = file.path(country_path, paste0("hbs_", y, "_", c, ".RData")))

    }
  }
}


#' database_hbs
#'
#' Function to load each country HBS and create a joint database for household and member files
#' @param year year/s of the HBS to process. Available options: 2010, 2015, 2020.
#' @param country code of the country or countries of the HBS to process. By default, it processes all available data in the working directory To see the available countries and codes, run country_code().
#' @param path Local path to the folder where the HBS's are stored. Not included in the package.
#' @importFrom dplyr %>%
#' @return RData file with the HBS microdata for each selected country and year.
#' @export
database_hbs <- function(year, country = "all", inputs_path) {

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
  if (!("all" %in% country)) {

    available_country <- av_country$code

    # Extract the years that are not available
    invalid_country <- country[!country %in% available_country]

    # If there is one invalid country, show a singular message
    if (length(invalid_country) == 1) {
      stop(sprintf("You introduced the country code %s which is not available. Possible options are: %s.",
                   invalid_years, paste(available_country, collapse = ", ")))
    } else if (length(invalid_country) > 1) {
      # If there are multiple invalid countries, show a plural message
      stop(sprintf("You introduced the countries %s which are not available. Possible options are: %s.",
                   paste(invalid_country, collapse = ", "), paste(available_country, collapse = ", ")))
    }
  }

  # Check if the folder for the selected country exists in the given path
  if (!("all" %in% country)) {

    missing_folders <- country[!dir.exists(file.path(inputs_path, as.character(country)))]

    # If there are missing folders, show an error message
    if (length(missing_folders) > 0) {
      stop(sprintf("The folder(s) for the country(ies) %s do not exist in the specified path: %s. Please check your data directory.",
                   paste(missing_folders, collapse = ", "), inputs_path))
    }
  }

  # Define country if "all"
  if ("all" %in% country) {
    # List all folders in inputs_path
    available_folders <- list.dirs(inputs_path, recursive = FALSE, full.names = FALSE)

    # Keep only those that match the available country codes
    country <- available_folders[available_folders %in% av_country$code]

    # If no valid country folders are found, show an error
    if (length(country) == 0) {
      stop(sprintf("No valid country folders were found in the specified path: %s. Please check your data directory.", inputs_path))
    }
  }

  # Create an empty list to store the data frames by year
  hbs_yearly_data <- list()

  # Load the data for each country, rename  and create a joint database for h file
  for (y in year) {

    # List to store data for each country in that year
    country_data_list <- list()

    for (c in country) {

      # Define the file path
      file_path <- paste0(inputs_path, c, "/hbs_", y, "_", c, ".RData")

      # Check if the file exists before loading
      if (file.exists(file_path)) {

        load(file_path)

        # Add the data to the list
        country_data_list[[c]] <- hbs_h

      } else {
        warning(sprintf("No raw data found for year %s and country %s in the specified path.", y, c))
      }

      # Create income variable
      Nquantiles <- function(x, w = NULL, s, t = NULL) {
        if (is.null(t)) {
          if (is.null(w)) w <- rep(1,length(x))

          # In the case of weights with missing values: critical error

          if (sum(is.na(w)) > 0) stop("Error.")

          n <- length(x)
          nn <- 1:n
          nn <- nn[order(x)]

          ww <- ceiling((cumsum(w[order(x, na.last = NA)])/sum(w[!is.na(x)]))*s)

          ww[n] <- s

          y <-  c(ww,rep(NA,sum(is.na(x))))[order(nn)]

        } else {

          if (sum(t != t[order(t)]) > 0)

            stop("Error.")

          yy <- rep(1,length(x))

          for (i in 1:length(t)) {

            yy <- cbind(yy, as.numeric(x >= t[i]))

          }
          y <- apply(yy, 1, sum)
        }
        return(y)
      }

      hbs_h             <- dplyr::mutate(hbs_h, total_exp_eq = EUR_HE00/HB062)
      hbs_h$decile      <- Nquantiles(hbs_h$total_exp_eq, w = hbs_h$HA10 , 10)
      hbs_h$quintile    <- Nquantiles(hbs_h$total_exp_eq, w = hbs_h$HA10 , 5)
      hbs_h$ventile     <- Nquantiles(hbs_h$total_exp_eq, w = hbs_h$HA10 , 20)
      hbs_h$percentile  <- Nquantiles(hbs_h$total_exp_eq, w = hbs_h$HA10 , 100)

      # Create feminization degree
      fem_degree <- hbs_m %>%
        dplyr::group_by ( MA04, COUNTRY ) %>%
        dplyr::summarise( number_male   = sum( MB02 == 1 & MB03_Recoded_5Classes != "0_14"),
                          number_female = sum( MB02 == 2 & MB03_Recoded_5Classes != "0_14")) %>%
        dplyr::mutate   ( share_female  = number_female/(number_male + number_female) ) %>%
        dplyr::mutate   ( feminization_degree = ifelse(share_female <  0.2              , "FD1",
                                         ifelse(share_female >= 0.2 & share_female < 0.4, "FD2",
                                         ifelse(share_female >= 0.4 & share_female < 0.6, "FD3",
                                         ifelse(share_female >= 0.6 & share_female < 0.8, "FD4",
                                         ifelse(share_female >= 0.8                     , "FD5", "Not provided")))))) %>%
        dplyr::mutate( HA04 = MA04)

      # Keep just variables to merge
      fem_degree <- fem_degree[,c("HA04",
                                  "COUNTRY",
                                  "feminization_degree")]

      # Add fem_degree to hbs_h
      hbs_h <- dplyr::left_join( hbs_h , fem_degree , by = c("HA04", "COUNTRY") )

      # Standardize household dataset variables
      hbs_h <- standardize_eu(hbs_h, variables_h)

      # Rename datasets
      eval(parse(text = paste0(c, "_hbs_h <- hbs_h")))
      eval(parse(text = paste0(c, "_hbs_m <- hbs_m")))

    }

    # Combine all country data into a single dataframe by year
    if (length(country_data_list) > 0) {
      hbs_yearly_data[[paste0("hbs_h_", y)]] <- do.call(gtools::smartbind, country_data_list)
    }

  }

  # Remove duplicated data
  rm(hbs_h, hbs_m)

  # Define mapping for h file
  mapping <- variables_h

  # Standardize household dataset variables
  hbs_h <- standardize_eu(hbs_h, mapping)

  # Code to create a joint database for 2020 and m file
  if (year == 2020) {

    # Define mapping file
    mapping <- variables_m_2020

    # List to store the datasets for each country
    member_files <- list()

    # Standardize members files variables for 2020
    for (c in country){

      # Get data for each country
      data <- get(paste0(c,"_hbs_m"))

      # Rename values of education in SI to fix propblems with rename_values
      if (c == "SI") {

        SI_hbs_m <- dplyr::mutate(SI_hbs_m, MC02B_agg = base::ifelse(MC02B_Recoded_3Categ ==  2, "A2",
                                                        base::ifelse(MC02B_Recoded_3Categ ==  3, "A3",
                                                        base::ifelse(MC02B_Recoded_3Categ ==  5, "A5",
                                                        base::ifelse(MC02B_Recoded_3Categ == 88, "A88", NA)))))
      }

      # Standardize each country dataset
      hbs_m <- standardize_eu(data, mapping)

      # Create relationship reference person based in the income variable for BE
      if (c == "BE") {
        hbs_m <- hbs_m %>%
          dplyr::group_by(number) %>%
          dplyr::mutate(max_income   = max(EUR_MF099)) %>%
          dplyr::mutate(relationship = base::ifelse(EUR_MF099 == max_income, "Reference person", "Other")) %>%
          dplyr::ungroup()
      }

      # Keep just the selected variables
      new <- c("number","country", "year", "birth_country", "gender", "relationship", "education", "activity", "workday", "contract_type", "employment_sector", "age")
      hbs_m <- hbs_m[,new]

      # Number as character to avoid problems with bind_rows
      hbs_m$number <- as.character(hbs_m$number)

      # Save the dataset in the list
      member_files[[c]] <- hbs_m

      # Rename the dataset with the country
      eval(parse(text = paste0("hbs_m_", c, "<- hbs_m")))

      # For checking purpose: Check row number
      if (nrow(get(paste0(c,"_hbs_m"))) != nrow(get(paste0("hbs_m_",c)))) {
        warning(paste0(c, " nrows do not match"))
      }
    }

    # Combine all country data into a single dataframe
    EU_hbs_m_2020 <- dplyr::bind_rows(member_files)

  }

  # Code to create a joint database for 2015 and m file
  if (year == 2015) {

    # Define mapping file
    mapping <- variables_m_2015

    # List to store the datasets for each country
    member_files2 <- list()

    # Standardize members files variables for 2015
    for (c in country){

      # Get data for each country
      data <- get(paste0(c,"_hbs_m"))

      # Rename values of education in SI to fix propblems with rename_values
      if (c == "SI") {

        SI_hbs_m <- dplyr::mutate(SI_hbs_m, MC02B_agg = base::ifelse(MC02B_Recoded_3Categ == 2, "A2",
                                                        base::ifelse(MC02B_Recoded_3Categ == 3, "A3",
                                                        base::ifelse(MC02B_Recoded_3Categ == 5, "A5",
                                                        base::ifelse(MC02B_Recoded_3Categ == 9, "A9", NA)))))
      }

      # Standardize each country dataset
      hbs_m <- standardize(data)

      # Create relationship reference person based in the income variable for SE
      if (c == "SE") {
        hbs_m <- hbs_m %>%
          dplyr::group_by(number) %>%
          dplyr::mutate(max_income   = max(EUR_MF099)) %>%
          dplyr::mutate(relationship = base::ifelse(EUR_MF099 == max_income, "Reference person", "Other")) %>%
          dplyr::ungroup()
      }

      # Keep just the selected variables
      new <- c("number","country", "year", "birth_country", "gender", "relationship", "education", "activity", "workday", "contract_type", "employment_sector", "age")
      hbs_m <- hbs_m[,new]

      # Rename the dataset with the country
      eval(parse(text = paste0("hbs_m_", c, "<- hbs_m")))

      # Number as character to avoid problems with bind_rows
      hbs_m$number <- as.character(hbs_m$number)

      # Save the dataset in the list
      member_files2[[c]] <- hbs_m

      # For checking purpose: Check row number
      if (nrow(get(paste0(c,"_hbs_m"))) != nrow(get(paste0("hbs_m_",c)))) {
        warning(paste0(c, " nrows do not match"))
      }
    }

    # Combine all country data into a single dataframe
    EU_hbs_m_2015 <- dplyr::bind_rows(member_files2)

  }




}
