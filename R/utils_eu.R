library(usethis)
library(magrittr)
options(dplyr.summarise.inform = FALSE)


#' rename_values_eu
#'
#' Function to rename the codified values of the dataset to the meaningful
#' values detailed in the mapping included in the package.
#' @param data dataset to be standardized.
#' @param map mapping file to be used (included in medusa)
#' @param current_var column name to be standardized.
#' @importFrom dplyr %>%
#' @return a dataset with labels renamed based in the mapping included in the package.
#' @export
rename_values_eu = function(data, map = variables_h, current_var) {
  exchange_data = map %>%                                                # brings together the two df (mapping and data)
    dplyr::filter(VAR_EN == current_var) %>%                                 # but only takes the part we are interested in (only when the value of the variable is equal to current_var).
    dplyr::select(value, NAME) %>%
    dplyr::distinct()

  if (current_var != "NMIEMB" | (current_var == "NMIEMB" & sum(is.na(unique(exchange_data$value))) == 0) ) {                     # we look if there is any na in value and if so the below does not apply (to avoid errors with NMIEMB).
    data = data %>%
      dplyr::rename(value = {{ current_var }}) %>%                             # rename the column (variable name) to value
      dplyr::mutate(value = as.character(value)) %>%                           # convert to a character
      dplyr::left_join(exchange_data, by = "value") %>%                        # it keeps only the columns we are interested in (value and name).
      dplyr::mutate(NAME = dplyr::if_else(is.na(NAME), value, NAME)) %>%
      dplyr::select(-value) %>%                                                # removes the value column
      dplyr::rename_with(~current_var, 'NAME')                                 # rename to current_var
  }


  return(data)
}

#' standardize_eu
#'
#' Function to standarize data names.
#' @param data dataset to be standardized.
#' @param map mapping file to be used (included in medusa)
#' @importFrom dplyr %>%
#' @return a dataset with the variables and labels renamed based in the mapping included in the package.
#' @export
standardize_eu <- function(data, map = variables_h) {
  # rename columns
  old_names = colnames(data)                                                # creates a vector with the column names
  new_names = dplyr::left_join(data.frame(VAR_EPF = old_names),             # the vector of column names is converted to a df and the column name is VAR_EPF
                               map %>%
                                 dplyr::select(VAR_EPF, VAR_EN) %>%         # within the mapping selects only the columns we are interested in
                                 dplyr::distinct(.),                        # duplicate remove
                               by = 'VAR_EPF') %>%                          # left join in function to var_epf
    dplyr::mutate(VAR_EN = ifelse(is.na(VAR_EN), VAR_EPF, VAR_EN)) %>%      # when it is na you get var_epf and if it is not you get var
    dplyr::pull(VAR_EN)                                                     # only keep the var column (as a vector)
  colnames(data) = new_names                                                # assign to the column names the new vector

  # rename values' codes to values' names for all items whose name is in the
  # renamed mapping's column and have not NA values
  ccitems = map %>%
    dplyr::filter(VAR_EN %in% intersect(unique(map$VAR_EN), new_names),
                  !is.na(value)) %>%
    dplyr::pull(VAR_EN) %>%
    unique()
  for (cc in ccitems) {                                                     # for all columns where variables can be mapped we overwrite the standardised dataset (with rename_value function)
    data = rename_values_eu(data = data, map = map, current_var = cc)
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
#' @param year year of the HBS to process. Available options: 2015, 2020.
#' @param country code of the country or countries of the HBS to process. By default, it processes all available data in the working directory To see the available countries and codes, run country_code().
#' @param inputs_path Local path to the folder where the HBS's are stored. Not included in the package.
#' @importFrom dplyr %>%
#' @return RData file with the joint database with the HBS microdata.
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
    available_folders <- list.dirs(path = inputs_path, recursive = FALSE, full.names = FALSE)

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
      file_path <- paste0(inputs_path,"/", c, "/hbs_", y, "_", c, ".RData")

      # Check if the file exists before loading
      if (file.exists(file_path)) {

        load(file_path)

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
          dplyr::mutate   ( feminization_degree = base::ifelse(share_female <  0.2              , "FD1",
                                                  base::ifelse(share_female >= 0.2 & share_female < 0.4, "FD2",
                                                  base::ifelse(share_female >= 0.4 & share_female < 0.6, "FD3",
                                                  base::ifelse(share_female >= 0.6 & share_female < 0.8, "FD4",
                                                  base::ifelse(share_female >= 0.8                     , "FD5", "Not provided")))))) %>%
          dplyr::mutate( HA04 = MA04)

        # Keep just variables to merge
        fem_degree <- fem_degree[,c("HA04",
                                    "COUNTRY",
                                    "feminization_degree")]

        # Add fem_degree to hbs_h
        hbs_h <- dplyr::left_join( hbs_h , fem_degree , by = c("HA04", "COUNTRY") )

        # Add the data to the list
        country_data_list[[c]] <- hbs_h

        # Rename datasets
        eval(parse(text = paste0(c, "_hbs_h <- hbs_h")))
        eval(parse(text = paste0(c, "_hbs_m <- hbs_m")))

      } else {
        warning(sprintf("No raw data found for year %s and country %s in the specified path.", y, c))
      }
    }

    # Combine all country data into a single dataframe by year
    if (length(country_data_list) > 0) {
      hbs_yearly_data[[paste0("hbs_h_", y)]] <- do.call(gtools::smartbind, country_data_list)
    }

  }

  # Remove duplicated data
  rm(hbs_h, hbs_m)

  # Additional adjustments to all h files
  for (name in names(hbs_yearly_data)) {

    # Check if the name follows the pattern "hbs_h_YYYY"
    if (grepl("^hbs_h_\\d{4}$", name)) {

      # Extract the dataframe
      hbs_h <- hbs_yearly_data[[name]]

      # Add income variables at EU level
      hbs_h$decile_eu     <- Nquantiles(hbs_h$total_exp_eq, w = hbs_h$weight , 10 )
      hbs_h$quintile_eu   <- Nquantiles(hbs_h$total_exp_eq, w = hbs_h$weight , 5  )
      hbs_h$ventile_eu    <- Nquantiles(hbs_h$total_exp_eq, w = hbs_h$weight , 20 )
      hbs_h$percentile_eu <- Nquantiles(hbs_h$total_exp_eq, w = hbs_h$weight , 100)

      # Create country2
      hbs_h <- hbs_h %>%
        dplyr::mutate(country2 = dplyr::recode(COUNTRY, "AT" =  "Austria"     ,
                                                        "BE" =  "Belgium"     ,
                                                        "BG" =  "Bulgaria"    ,
                                                        "CY" =  "Cyprus"      ,
                                                        "CZ" =  "Czechia"     ,
                                                        "DE" =  "Germany"     ,
                                                        "DK" =  "Denmark"     ,
                                                        "EE" =  "Estonia"     ,
                                                        "EL" =  "Greece"      ,
                                                        "ES" =  "Spain"       ,
                                                        "FI" =  "Finland"     ,
                                                        "FR" =  "France"      ,
                                                        "HR" =  "Croatia"     ,
                                                        "HU" =  "Hungary"     ,
                                                        "IE" =  "Ireland"     ,
                                                        "IT" =  "Italy"       ,
                                                        "LT" =  "Lithuania"   ,
                                                        "LU" =  "Luxembourg"  ,
                                                        "LV" =  "Latvia"      ,
                                                        "MT" =  "Malta"       ,
                                                        "NL" =  "Netherlands" ,
                                                        "PL" =  "Poland"      ,
                                                        "PT" =  "Portugal"    ,
                                                        "RO" =  "Romania"     ,
                                                        "SE" =  "Sweden"      ,
                                                        "SI" =  "Slovenia"    ,
                                                        "SK" =  "Slovakia"    ))

      # Standardize household dataset variables
      hbs_h <- standardize_eu(hbs_h, map = variables_h)

      # Save the modified data frame back to the list
      hbs_yearly_data[[name]] <- hbs_h
    }
  }

  # Code to create a joint database for 2020 and m file
  if (year == 2020) {

    # List to store the datasets for each country
    member_files <- list()

    # Standardize members files variables for 2020
    for (c in country){

      # Define the file path
      file_path <- paste0(inputs_path, "/", c, "/hbs_", y, "_", c, ".RData")

      # Check if the file exists before loading
      if (file.exists(file_path)) {

        # Get data for each country
        data <- get(paste0(c,"_hbs_m"))

        # Rename values of education in SI to fix propblems with rename_values
        if (c == "SI") {

          data <- dplyr::mutate(data, MC02B_agg = base::ifelse(MC02B_Recoded_3Categ ==  2, "A2",
                                                  base::ifelse(MC02B_Recoded_3Categ ==  3, "A3",
                                                  base::ifelse(MC02B_Recoded_3Categ ==  5, "A5",
                                                  base::ifelse(MC02B_Recoded_3Categ == 88, "A88", NA)))))
        }

        # Standardize each country dataset
        hbs_m <- standardize_eu(data = data, map = variables_m_2020)

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
    }

    # Combine all country data into a single dataframe
    hbs_m_2020 <- dplyr::bind_rows(member_files)

    # Add the dataframe to hbs_yearly_data
    hbs_yearly_data[["hbs_m_2020"]] <- hbs_m_2020

  }

  # Include reference person data from m file to h file
  if (year == 2020) {

    # Extract hbs_h and hbs_m
    hbs_h <- hbs_yearly_data$hbs_h_2020
    hbs_m <- hbs_yearly_data$hbs_m_2020

    # Subset m dataset: keep just data for the household reference person
    hbs_m <- subset(hbs_m, relationship == "Reference person")

    # Check and fix duplicates in hbs_m for each country
    for (c in country) {
      h <- hbs_h %>% dplyr::filter(country == c)
      m <- hbs_m %>% dplyr::filter(country == c)

      # Identificar duplicados en miembros con relación Reference person
      dupp <- m %>%
        dplyr::filter(relationship == "Reference person") %>%
        dplyr::group_by(number) %>%
        dplyr::filter(dplyr::n() > 1) %>%
        dplyr::ungroup()

      if (nrow(dupp) > 0) {
        warning(paste0("Duplicate reference persons found in the members file (2020 wave) for ", c,
                       ". Rows with more missing values have been removed."))

        # Function to count "Not provided" and "Not applicable"
        count_missing_custom <- function(row) {
          row <- as.character(row)
          sum(row == "Not provided") * 10 + sum(row == "Not applicable")
        }

        # Count missing score
        dupp$missing_score <- apply(dupp, 1, count_missing_custom)

        # Remove duplicates with more missing score, keeping one row per 'number'
        dupp_cleaned <- dupp %>%
          dplyr::group_by(number) %>%
          dplyr::arrange(missing_score) %>%
          dplyr::slice(1) %>%
          dplyr::ungroup() %>%
          dplyr::select(-missing_score)

        # Replace the original duplicates in m
        m <- m %>%
          dplyr::filter(!(number %in% dupp$number & relationship == "Reference person")) %>%
          dplyr::bind_rows(dupp_cleaned)

        # Include the adjustment in hbs_m
        hbs_m <- hbs_m %>%
          dplyr::filter(country != c) %>%
          dplyr::bind_rows(m)
      }
    }

    # Merge reference person data with household data
    hbs_h$year <- as.character(hbs_h$year)
    hbs <- merge(hbs_h, hbs_m, by = c("number", "country"), all.x = TRUE)

    # Save joint data frame in the list
    hbs_yearly_data[["hbs_2020"]] <- hbs
  }

  # Code to create a joint database for 2015 and m file
  if (year == 2015) {

    # List to store the datasets for each country
    member_files2 <- list()

    # Standardize members files variables for 2015
    for (c in country){

      # Define the file path
      file_path <- paste0(inputs_path, "/", c, "/hbs_", y, "_", c, ".RData")

      # Check if the file exists before loading
      if (file.exists(file_path)) {

        # Get data for each country
        data <- get(paste0(c,"_hbs_m"))

        # Rename values of education in SI to fix propblems with rename_values
        if (c == "SI") {

          data <- dplyr::mutate(data, MC01= base::ifelse(MC01_Recoded_4Classes == 2, "A2",
                                            base::ifelse(MC01_Recoded_4Classes == 3, "A3",
                                            base::ifelse(MC01_Recoded_4Classes == 5, "A5",
                                            base::ifelse(MC01_Recoded_4Classes == 9, "A9", NA)))))
        }

        # Standardize each country dataset
        hbs_m <- standardize_eu(data = data, map = variables_m_2015)

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
    }

    # Combine all country data into a single dataframe
    hbs_m_2015 <- dplyr::bind_rows(member_files2)

    # Add the dataframe to hbs_yearly_data
    hbs_yearly_data[["hbs_m_2015"]] <- hbs_m_2015

  }

  # Include reference person data from m file to h file
  if (year == 2015) {

    # Extract hbs_h and hbs_m
    hbs_h <- hbs_yearly_data$hbs_h_2015
    hbs_m <- hbs_yearly_data$hbs_m_2015

    # Subset m dataset: keep just data for the household reference person
    hbs_m <- subset(hbs_m, relationship == "Reference person")

    # Check and fix duplicates in hbs_m for each country
    for (c in country) {
      h <- hbs_h %>% dplyr::filter(country == c)
      m <- hbs_m %>% dplyr::filter(country == c)

      # Identificar duplicados en miembros con relación Reference person
      dupp <- m %>%
        dplyr::filter(relationship == "Reference person") %>%
        dplyr::group_by(number) %>%
        dplyr::filter(dplyr::n() > 1) %>%
        dplyr::ungroup()

      if (nrow(dupp) > 0) {
        warning(paste0("Duplicate reference persons found in the members file (2015 wave) for ", c,
                       ". Rows with more missing values have been removed."))

        # Function to count "Not provided" and "Not applicable"
        count_missing_custom <- function(row) {
          row <- as.character(row)
          sum(row == "Not provided") * 10 + sum(row == "Not applicable")
        }

        # Count missing score
        dupp$missing_score <- apply(dupp, 1, count_missing_custom)

        # Remove duplicates with more missing score, keeping one row per 'number'
        dupp_cleaned <- dupp %>%
          dplyr::group_by(number) %>%
          dplyr::arrange(missing_score) %>%
          dplyr::slice(1) %>%
          dplyr::ungroup() %>%
          dplyr::select(-missing_score)

        # Replace the original duplicates in m
        m <- m %>%
          dplyr::filter(!(number %in% dupp$number & relationship == "Reference person")) %>%
          dplyr::bind_rows(dupp_cleaned)

        # Include the adjustment in hbs_m
        hbs_m <- hbs_m %>%
          dplyr::filter(country != c) %>%
          dplyr::bind_rows(m)
      }
    }

    # Merge reference person data with household data
    hbs_h$year <- as.character(hbs_h$year)
    hbs <- merge(hbs_h, hbs_m, by = c("number", "country"), all.x = TRUE)

    # Save joint data frame in the list
    hbs_yearly_data[["hbs_2015"]] <- hbs
  }

  return(hbs_yearly_data)
}


#' coicop_mapping
#'
#' Function to rename coicop expenditure categories
#' @param data dataframe with the hbs data to rename
#' @param mapping_file mapping file to be used (included in medusa)
#' @importFrom dplyr %>%
#' @return data file with the renamed expenditure categories
#' @export
coicop_mapping <- function(data, mapping_file = mapping_coicop) {

  # Assign mapping values to vectors
  var_old <- base::assign("var_old", mapping_file$var_hbs)
  var_new <- base::assign("var_new", mapping_file$var)

  # Create the selected variables with the new names
  for (v in 1:length(var_old)) {
    new = eval(parse(text = paste0("var_new[", v, "]")))
    var = eval(parse(text = paste0("var_old[", v, "]")))
    eval(parse( text = paste0("data <- data %>%
  dplyr::mutate(",var_new,"=", var_old,")")))
  }

  return(data)
}

