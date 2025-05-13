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


#' rename_coicop
#'
#' Function to rename all coicop expenditure categories
#' @param data dataframe with the hbs data to rename
#' @importFrom dplyr %>%
#' @return data file with the renamed expenditure categories
#' @export
rename_coicop <- function(data) {

  # Get original names
  original_names <- names(data)

  # Identificar columnas que empiezan por "EUR_HE" seguido de uno o más dígitos
  matching <- grep("^EUR_HE\\d+$", original_names)

  # Renombrarlas sustituyendo "EUR_HE" por "CP"
  names(data)[matching] <- gsub("^EUR_HE", "CP", original_names[matching])

  return(data)
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


#' update_year
#'
#' Function to update the expenditure data of the HBS
#' @param data dataframe with the hbs data to update
#' @param new_year year to which you want to update the microdata
#' @importFrom dplyr %>%
#' @return data file with the updated expenditure data
#' @export
update_year <- function(data, new_year){

  # Define countries
  country <- unique(data$country)

  # Define data year
  base_year <- unique(data$year.x)

  # Loop to upscale each country expenses
  for (c in country) {

    # Calculate adjustment coefficient if base year = 2015
    if (base_year == 2015) {

       # Get eurostat data: Harmonised index of consumer price (HICP)
      prc <- restatapi::get_eurostat_data("prc_hicp_aind",
                                          filters = c("INX_A_AVG", c),
                                          date_filter = new_year )

      # Calculate adjustment coefficient
      prc <- prc %>%
        dplyr::mutate(prc    = values/100)

    } else if (base_year == 2020 & new_year < 2020 & new_year > 2015 ) {

      # Define years
      years <- c(base_year, new_year)

      # Get eurostat data: Harmonised index of consumer price (HICP)
      prc <- restatapi::get_eurostat_data("prc_hicp_aind",
                                          filters = c("INX_A_AVG", c),
                                          date_filter = years )

      # Calculate adjustment coefficient if base year == 2020 & 2015 < new year > 2020
      prc <- prc %>%
        reshape2::dcast(coicop ~ time, value.var = "values") %>%
        dplyr::rename(exp_2019 = "2019",
               exp_2020 = "2020") %>%
        dplyr::mutate(prc = 1+((exp_2019-exp_2020)/100))

    }

    # Inicializar dataframe resultante
    data_updated <- data

    # Filtrar solo las filas del país actual
    idx <- which(data$country == c)

    # Obtener columnas que corresponden a códigos COICOP
    coicop_cols <- grep("^CP\\d+", names(data), value = TRUE)

    for (col in coicop_cols) {
      coicop_code <- col
      coef <- NA

      # Buscar el coeficiente más específico disponible
      while (nchar(coicop_code) >= 2 && is.na(coef)) {
        if (coicop_code %in% prc$coicop) {
          coef <- prc$prc[prc$coicop == coicop_code]
        } else {
          coicop_code <- substr(coicop_code, 1, nchar(coicop_code) - 1)
          if (coicop_code %in% prc$coicop) {
          coef <- prc$prc[prc$coicop == coicop_code]
          } else {
            coicop_code <- substr(coicop_code, 1, nchar(coicop_code) - 1)
            if (coicop_code %in% prc$coicop) {
              coef <- prc$prc[prc$coicop == coicop_code]
            } else {
              coicop_code <- substr(coicop_code, 1, nchar(coicop_code) - 1)
              if (coicop_code %in% prc$coicop) {
                coef <- prc$prc[prc$coicop == coicop_code]
              } else {
                coef <- prc$prc[prc$coicop == "CP00"]
              }
            }
          }
        }
      }

      # Aplicar el coeficiente si se ha encontrado uno válido
      if (!is.na(coef)) {
        data_updated[idx, col] <- data_updated[idx, col] * coef
      }
    }
  }

  return(data_updated)
}


#' price_shock_eu
#'
#' Function to apply a specific price shock to the different COICOP
#' categories of the Household Budget Survey (HBS).
#' @param data input data from the HBS to apply the price shocks.
#' @param shocks a dataset with the price shocks per coicop to be applied.
#' The format of the dataset has to correspond to the predefined one in the package.
#' To save a csv file with the right format to enter the price shocks run `ex_shocks_eu()`.
#' A price shock greater than 1 indicates a price increase (e.g. 1.1 indicates a
#' 10\% increase) and less than 1 indicates a price decrease (e.g. 0.9 indicates a
#' 10\% decrease).
#' @importFrom dplyr %>%
#' @return a dataset with the HBS data and the new expenses for COICOP categories
#' after the application of the price shock.
#' @export
price_shock_eu <- function(data, shocks) {

  # Extraer países presentes en los datos
  countries <- unique(data$country)

  # Extraer nombres de escenarios (columnas en shocks que contienen "_")
  scenario_cols <- setdiff(colnames(shocks), "coicop")
  scenarios <- unique(gsub(".*_", "", scenario_cols))  # s1, s2, etc.

  # Columnas COICOP en data
  coicop_cols <- grep("^CP\\d+", colnames(data), value = TRUE)
  coicop_cols <- setdiff(coicop_cols, "CP00")

  for (scenario in scenarios) {
    for (country in countries) {

      col_name <- paste0(country, "_", scenario)
      if (!(col_name %in% colnames(shocks))) next  # Saltar si ese país no tiene escenario

      idx <- which(data$country == country)

      for (col in coicop_cols) {
        # Buscar valor del shock
        shock_val <- shocks[shocks$coicop == col, col_name]

        # Si existe shock válido y distinto de 1
        if (length(shock_val) == 1 && !is.na(shock_val) && shock_val != 1) {
          new_col <- paste0(col, "_", scenario)

          if (!(new_col %in% colnames(data))) {
            data[[new_col]] <- data[[col]]
          }

          data[idx, new_col] <- data[idx, col] * shock_val
        }
      }

      # Calcular nueva CP00 (total)
      diff_cols <- grep(paste0("_", scenario, "$"), names(data), value = TRUE)
      orig_cols <- gsub(paste0("_", scenario, "$"), "", diff_cols)

      if (length(diff_cols) == 1) {
        diff_val <- data[idx, diff_cols] - data[idx, orig_cols]
      } else {
        diff_val <- data[idx, diff_cols] - data[idx, orig_cols]
      }

      if (is.null(dim(diff_val))) {
        diff_val <- matrix(diff_val, ncol = 1)
      }

      cp00_new <- paste0("CP00_", scenario)
      if (!(cp00_new %in% colnames(data))) {
        data[[cp00_new]] <- data$CP00
      }

      data[[cp00_new]][idx] <- data$CP00[idx] + rowSums(diff_val, na.rm = TRUE)
    }
  }

  return(data)
}


#' order_var_eu
#'
#' Function to order the labels of the socioeconomic and demographic variables
#' @param data dataset in which we want to order the labels of the socioeconomic and demographic variables
#' @param g variable for which we want to sort the labels
#' @importFrom dplyr %>%
#' @return a dataset in which the labels are ordered for the selected socioeconomic or demographic variable
#' @export
order_var_eu <- function(data, g){
  if (g == "children"){
    data <- data %>%
      dplyr::mutate(LABELS = factor(LABELS, levels = c("No children", "With children", "Large family")))
  } else if (g == "birth_country") {
    data <- data %>%
      dplyr::mutate(LABELS = factor(LABELS, levels = c("National", "EU", "Non-EU", "Non-national")))
  } else if (g == "education") {
    data <- data %>%
      dplyr::mutate(LABELS = factor(LABELS, levels = c("Early childhood education", "Primary education", "Secondary education", "Post-secondary education",  "Tertiary education", "Higher education")))
  } else if (g == "contract_type") {
    data <- data %>%
      dplyr::mutate(LABELS = factor(LABELS, levels = c("Permanent contract", "Fixed-term contract", "Not applicable")))
  } else if (g == "activity") {
    data <- data %>%
      dplyr::mutate(LABELS = factor(LABELS, levels = c("Employed", "Unemployed", "Retired", "Student", "Domestic tasks", "Disabled", "Military service")))
  } else if (g == "age") {
    data <- data %>%
      dplyr::mutate(LABELS = factor(LABELS, levels = c("Young", "Adult", "Elder")))
  } else if (g == "employment_sector") {
    data <- data %>%
      dplyr::mutate(LABELS = factor(LABELS, levels = c("Private sector", "Public sector")))
  } else if (g == "household_type") {
    data <- data %>%
      dplyr::mutate(LABELS = factor(LABELS, levels = c("Adult alone", "Couple", "Couple with children", "More than 2 adults", "More than 2 adults with children", "Single parent"  )))
  } else if (g == "income_source") {
    data <- data %>%
      dplyr::mutate(LABELS = factor(LABELS, levels = c( "Wages", "Self-employment", "Property", "Pensions", "Unemployment")))
  }
  return(data)
}


#' basic_graph_eu
#'
#' Function to create a basic graph to summarize the distributional impact in the EU
#' based in one or more socioeconomic or demographic variable (one plot per variable).
#' @param data a dataset with the input data needed to generate a basic graph.
#' @param var variable(s) according to which you want to generate the graph. If
#' graph_labels_eu$VARIABLE (by default) creates a graph with the distributional
#' impacts for each of the variables specified in the package. If not, you can
#' indicate a variable or a vector of variables to crate the graph.If you want to
#' see the variables for which the function is available run `available_var_impact_eu()`.
#' @importFrom dplyr %>%
#' @return a graph per selected variable/s summarizing distributional impacts.
#' @export
basic_graph_eu <- function(data, var = graph_labels_eu$VARIABLE) {
  if (!dir.exists("figures")) dir.create("figures")

  # Si es una lista de dataframes (como en el output original de impact_eu)
  if (is.list(data) && !is.data.frame(data)) {
    for (g in var) {
      df <- data[[paste0("di_", g)]]

      if (is.null(df)) next

      source_tag <- unique(df$SOURCE)
      folder <- ifelse(length(source_tag) == 1 && source_tag %in% c("EU"), "EU", source_tag)

      folder_path <- file.path("figures", folder)
      if (!dir.exists(folder_path)) dir.create(folder_path)

      datapl <- df %>%
        tidyr::pivot_longer(cols = dplyr::starts_with("DI_"), names_to = "Scenario", values_to = "Impact") %>%
        dplyr::mutate(
          Scenario = stringr::str_replace(Scenario, "^DI_", ""),
          LABELS = as.character(LABELS)
        ) %>%
        dplyr::filter(!LABELS %in% c("Not provided", "NA", "Others", "Other", "Not applicable")) %>%
        order_var_eu(., g) %>%
        dplyr::filter(!is.na(LABELS)) %>%
        droplevels()

      # Skip plot if there is no sufficient data after filtering
      if (nrow(datapl) <= 1) {
        country_name <- unique(df$SOURCE)
        message(sprintf(" Warning: Skipping plot for variable '%s' in '%s' (insufficient data after filtering).", g, country_name))
        next
      }

      # Ordenar LABELS para "country" por valor de impacto en cada escenario
      if (g == "country") {
        datapl <- datapl %>%
          dplyr::group_by(Scenario) %>%
          dplyr::mutate(LABELS = factor(LABELS, levels = LABELS[order(Impact)])) %>%
          dplyr::ungroup()
      }

      if (g %in% c("decile", "decile_eu", "ventile", "ventile_eu", "percentile", "percentile_eu")) {
        datapl <- datapl %>% dplyr::mutate(LABELS = as.numeric(LABELS))
      }

      clean_g <- graph_labels_eu %>%
        dplyr::filter(VARIABLE == g) %>%
        dplyr::pull(VAR_CLEAN)

      pl <- ggplot2::ggplot(datapl,
                            ggplot2::aes(x = LABELS, y = Impact, fill = Scenario)) +
        ggplot2::geom_col(position = ggplot2::position_dodge(width = 1)) +
        ggplot2::facet_grid(. ~ Scenario) +
        ggplot2::scale_fill_manual(values = c("#3ed8d8", "#7ee5b2", "#e5e57e", "#e5b27e", "#e57f7e", "#e78ae7", "#b98ae7")) +
        ggplot2::labs(y = "Change in welfare (%)", x = clean_g) +
        ggplot2::theme(legend.position = "none") +
        ggplot2::theme(text = ggplot2::element_text(size = 16))

      if (g %in% c("decile", "decile_eu")) {
        pl <- pl + ggplot2::scale_x_continuous(breaks = 1:10, labels = 1:10)
      }

      if (g %in% c("ventile", "ventile_eu")) {
        pl <- pl + ggplot2::scale_x_continuous(breaks = seq(0, 100, by = 2))
      }

      if (g %in% c("percentile", "percentile_eu")) {
        pl <- pl + ggplot2::scale_x_continuous(breaks = seq(0, 100, by = 10))
      }

      if (g %in% c("country", "zone", "household_type", "children", "income_source", "COUNTRYRP", "activity", "education")) {
        pl <- pl + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.25))
      }

      adj_wh <- adjust_wh(datapl, var_w = "Scenario", var_h = NULL)

      # Aumenta anchura si g == "country"
      if (g == "country") {
        adj_wh$width <- adj_wh$width + 50
      }

      ggplot2::ggsave(pl,
                      file = file.path(folder_path, paste0("DI_", g, ".png")),
                      width = adj_wh$width,
                      height = adj_wh$heigth,
                      units = "mm")
    }
  } else {
    warning("The `data` object must be a list of data frames as returned by `impact_eu().")
  }
}


#' impact_eu
#'
#' Function to calculate the distributional impacts based in one or more
#' socioeconomic or demographic variables (one impact per variable) for EU countries.
#' @param data a dataset with the input data needed to calculate distributional
#' impacts. The dataset should contain both the household expenditures collected
#' in the HBS and the expenditures after applying the price shock.
#' @param var variable(s) according to which you want to calculate distributional
#' impacts. If categories$categories (by default) calculates the distributional
#' impacts for each of the variables specified in the package. If not, you can
#' indicate a variable or a vector of variables to calculate distributional impacts.
#' If you want to see the variables for which the calculation is available run `available_var_impact()`.
#' @param by_country If TRUE (by default) in addition to calculating the general distributional impacts
#' for all the households (EU level), it also calculates the distributional impacts for
#' each  country (and for each specified variables).
#' @param save If TRUE (by default) saves a list of the generated datasets (.RData)
#' summarising the distributional impacts per selected variable. If FALSE do not save.
#' @param file_name name of the file to save the results, if save TRUE. By default "D_impacts".
#' @param fig generates and saves a graph that summarises the distributional impacts.
#' By default it is TRUE, for the graph/s not to be generated and saved indicate FALSE.
#' @param shocks_scenario_names vector of the names of the considered scenario shocks
#' @importFrom dplyr %>%
#' @return a list containing the generated datasets (.RData) summarising the
#' distributional impacts per selected variable.
#' @export
impact_eu <- function(data, var = graph_labels_eu$VARIABLE, by_country = TRUE, save = TRUE,
                      file_name = "D_impacts", fig = TRUE, shocks_scenario_names) {

  d_impacts <- list()
  missing_vars <- c()

  # 1. Calcular impacto distributivo por hogar
  for (s in shocks_scenario_names) {
    new_col <- paste0("CP00_", s)
    impact_col <- paste0("impact_", s)

    if (new_col %in% names(data)) {
      data[[impact_col]] <- 100 * (data$CP00 - data[[new_col]]) / data$CP00
    } else {
      warning(paste(new_col, "not found in the dataset."))
    }
  }

  # Asegurarse de que los pesos son numéricos
  data$weight <- as.numeric(data$weight)

  results_list <- list()

  # 2. Impactos generales (UE)
  for (g in var) {
    if (g %in% colnames(data)) {
      impact_cols <- grep("^impact_", names(data), value = TRUE)

      df <- data %>%
        dplyr::group_by(.data[[g]]) %>%
        dplyr::summarise(
          VARIABLE = g,
          WEIGHT = sum(weight, na.rm = TRUE),
          across(all_of(impact_cols), ~ weighted.mean(., w = weight, na.rm = TRUE), .names = "DI_{.col}")
        ) %>%
        dplyr::rename_with(~ gsub("^DI_impact_", "DI_", .), dplyr::starts_with("DI_impact_")) %>%
        dplyr::rename(LABELS = 1) %>%
        dplyr::mutate(LABELS = as.character(LABELS), SOURCE = "EU")

      d_impacts[[paste0("di_", g)]] <- df
      results_list[[paste0("EU_", g)]] <- df
    } else {
      missing_vars <- c(missing_vars, g)
      warning(paste0(g, " is not present in the dataset"))
    }
  }

  # 3. Guardar resultados generales
  if (save == TRUE) {
    if (!dir.exists("outputs_di")) dir.create("outputs_di")
    save(d_impacts, file = paste0("outputs_di/", file_name, ".RData"))
  }

  if (fig == TRUE) {
    var <- setdiff(var, missing_vars)
    basic_graph_eu(data = d_impacts, var = var)
  }

  # 4. Impactos por país
  if (by_country) {
    countries <- unique(data$country)

    for (c in countries) {
      data_c <- data[data$country == c, ]
      d_impacts_country <- list()

      for (g in var) {
        if (g %in% colnames(data_c)) {
          impact_cols <- grep("^impact_", names(data_c), value = TRUE)

          df <- data_c %>%
            dplyr::group_by(.data[[g]]) %>%
            dplyr::summarise(
              VARIABLE = g,
              WEIGHT = sum(weight, na.rm = TRUE),
              across(all_of(impact_cols), ~ weighted.mean(., w = weight, na.rm = TRUE), .names = "DI_{.col}")
            ) %>%
            dplyr::rename_with(~ gsub("^DI_impact_", "DI_", .), dplyr::starts_with("DI_impact_")) %>%
            dplyr::rename(LABELS = 1) %>%
            dplyr::mutate(LABELS = as.character(LABELS), SOURCE = c)

          d_impacts_country[[paste0("di_", g)]] <- df
          results_list[[paste0(c, "_", g)]] <- df
        }
      }

      if (save) {
        save(d_impacts_country, file = paste0("outputs_di/", file_name, "_", c, ".RData"))
      }

      if (fig) {
        var_country <- setdiff(var, missing_vars)
        basic_graph_eu(data = d_impacts_country, var = var_country)
      }
    }
  }

  # 5. Devolver resultados combinados en un solo data.frame
  combined_df <- dplyr::bind_rows(results_list, .id = "ID")
  return(combined_df)
}


#' order_vars_eu
#'
#' Function to order the labels of the socioeconomic and demographic variables in intersectional graphs.
#' @param data dataset in which we want to order the labels of the socioeconomic and demographic variables
#' @param g variable for which we want to sort the labels
#' @importFrom dplyr %>%
#' @return a dataset in which the labels are ordered for the selected socioeconomic or demographic variable
#' @export
order_vars_eu <- function(data, var_col, col_name = "LABELS") {
  levels_list <- list(
    children = c("No children", "With children", "Large family"),
    birth_country = c("National", "EU", "Non-EU", "Non-national"),
    education = c("Early childhood education", "Primary education", "Secondary education",
                  "Post-secondary education", "Tertiary education", "Higher education"),
    contract_type = c("Permanent contract", "Fixed-term contract", "Not applicable"),
    activity = c("Employed", "Unemployed", "Retired", "Student", "Domestic tasks", "Disabled", "Military service"),
    age = c("Young", "Adult", "Elder"),
    employment_sector = c("Private sector", "Public sector"),
    household_type = c("Adult alone", "Couple", "Couple with children", "More than 2 adults",
                       "More than 2 adults with children", "Single parent"),
    income_source = c("Wages", "Self-employment", "Property", "Pensions", "Unemployment")
  )

  if (var_col %in% names(levels_list)) {
    lvls <- levels_list[[var_col]]
    data[[col_name]] <- factor(data[[col_name]], levels = lvls)
  }

  return(data)
}


#' intersectional_graph_eu
#'
#' Function to create an intersectional graph to summarize the distributional
#' impact based in the intersection of two socioeconomic or demographic variables
#' (2 variables per plot) for EU countries.
#' @param data a dataset with the input data needed to generate the intersectional graph.
#' @param pairs set of variables (2) according to which you want to create the
#' intersectional graph. If is_categories (by default), it generates the intersectional
#' graph for each of the combinations of variables specified in the package. If not,
#' you can indicate the set of variables according to which you want to generste the
#' intersectional graph. If you wish to see the set of variables for which the
#' calculation is available, run `available_var_intersec_eu()`. To enter a set of
#' variables for the calculation, it must follow the same format as the output of
#' `available_var_intersec_eu()`, i.e. a table whose columns have category_a and
#' category_b as their titles.
#' @importFrom dplyr %>%
#' @return a graph per selected set of variables summarizing the distributional impacts.
#' @export
intersectional_graph_eu <- function(data, pairs = is_categories_eu) {
  if (!dir.exists("figures")) dir.create("figures")

  for (r in 1:nrow(pairs)) {
    var_a <- pairs$category_a[r]
    var_b <- pairs$category_b[r]

    # Buscar todos los objetos que coincidan con este par de variables
    matching_keys <- grep(paste0("^di_", var_a, "_", var_b), names(data), value = TRUE)

    for (key in matching_keys) {
      df <- data[[key]]
      if (is.null(df)) next

      source_tag <- unique(df$SOURCE)
      folder <- ifelse(length(source_tag) == 1 && source_tag %in% c("EU"), "EU", source_tag)
      folder_path <- file.path("figures", folder)
      if (!dir.exists(folder_path)) dir.create(folder_path)

      datapl <- df %>%
        tidyr::pivot_longer(cols = dplyr::starts_with("DI_"), names_to = "Scenario", values_to = "Impact") %>%
        dplyr::mutate(
          Scenario = stringr::str_replace(Scenario, "^DI_", ""),
          LABELS_A = as.character(LABELS_A),
          LABELS_B = as.character(LABELS_B)
        )

      # Ordenar
      datapl <- order_vars_eu(datapl, var_a, "LABELS_A")
      datapl <- order_vars_eu(datapl, var_b, "LABELS_B")

      datapl <- datapl %>%
        dplyr::filter(
          !LABELS_A %in% c("Not provided", "Others", "Other", "Not applicable"),
          !LABELS_B %in% c("Not provided", "Others", "Other", "Not applicable")
        ) %>%
        dplyr::filter(!is.na(LABELS_A) & LABELS_A != "NA") %>%
        dplyr::filter(!is.na(LABELS_B) & LABELS_B != "NA") %>%
        droplevels()

      clean_a <- graph_labels_eu %>% dplyr::filter(VARIABLE == var_a) %>% dplyr::pull(VAR_CLEAN)
      clean_b <- graph_labels_eu %>% dplyr::filter(VARIABLE == var_b) %>% dplyr::pull(VAR_CLEAN)

      # Determinar si se usa faceta por país
      if ("by_country" %in% names(datapl)) {
        facet_formula <- as.formula("LABELS_B ~ Scenario + by_country")
      } else {
        facet_formula <- as.formula("LABELS_B ~ Scenario")
      }

      if (var_a %in% c("decile", "decile_eu", "quintile", "quintile_eu", "ventile", "ventile_eu", "percentile", "percentile_eu")) {
        pl <- ggplot2::ggplot(datapl, ggplot2::aes(x = as.numeric(LABELS_A), y = Impact, colour = Scenario)) +
          ggplot2::geom_point() +
          ggplot2::geom_line() +
          ggplot2::facet_grid(facet_formula) +
          ggplot2::scale_colour_manual(values = c("#3ed8d8", "#7ee5b2", "#e5e57e", "#e5b27e", "#e57f7e", "#e78ae7", "#b98ae7")) +
          ggplot2::labs(y = "Change in welfare (%)", x = clean_a) +
          ggplot2::theme(text = ggplot2::element_text(size = 16),
                         legend.position = "none")
      } else {
        pl <- ggplot2::ggplot(datapl, ggplot2::aes(x = LABELS_A, y = Impact, fill = Scenario)) +
          ggplot2::geom_col(position = ggplot2::position_dodge(width = 1)) +
          ggplot2::facet_grid(facet_formula) +
          ggplot2::scale_fill_manual(values = c("#3ed8d8", "#7ee5b2", "#e5e57e", "#e5b27e", "#e57f7e", "#e78ae7", "#b98ae7")) +
          ggplot2::labs(y = "Change in welfare (%)", x = clean_a) +
          ggplot2::theme(text = ggplot2::element_text(size = 16),
                         legend.position = "none")
      }

      if (var_a %in% c("decile", "decile_eu")) {
        pl <- pl + ggplot2::scale_x_continuous(breaks = 1:10, labels = 1:10)
      }

      if (var_a %in% c("country", "zone", "household_type", "children", "income_source", "COUNTRYRP", "activity", "education", "contract_type")) {
        pl <- pl + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.25))
      }

      adj_wh <- adjust_wh_is(datapl, var_w = "Scenario", var_h = "LABELS_B")

      if ("by_country" %in% names(datapl)) {
        npaises <- length(unique(datapl$by_country))
        adj_wh$width <- adj_wh$width + 20 * npaises
      } else if (var_a == "country") {
        adj_wh$width <- adj_wh$width + 50
      }

      print(file.path(folder_path, paste0(key, ".png")))

      ggplot2::ggsave(pl,
                      file = file.path(folder_path, paste0(key, ".png")),
                      width = adj_wh$width,
                      height = adj_wh$heigth,
                      units = "mm",
                      limitsize = FALSE)
    }
  }

  return(invisible(NULL))
}



#' impact_intersectional_eu
#'
#' Function to calculate the distributional impacts based in the intersection
#' of two socioeconomic or demographic variables (2 variables per impact)
#' across EU and by EU country.
#' @param data a dataset with the input data needed to calculate the intersectional
#' distributional impacts. The dataset should contain both the household expenditures
#' collected in the HBS and the expenditures after applying the price shock.
#' @param pairs set of variables (2) according to which you want to calculate
#' distributional impacts. If is_categories (by default) calculates the intersectional
#' distributional impacts for each of the set of variables specified in the package.
#' If not, you can indicate the set of variables according to which you want to calculate
#' the intersectional distributional impacts.If you want to see the set of variables for
#' which the calculation is available run `available_var_intersec()`. To enter a
#' set of variables for the calculation, it must follow the same format as the
#' output of `available_var_intersec()`, i.e. a table whose columns have category_a
#' and category_b as their titles.
#' @param save If TRUE (by default) saves a list of the generated datasets (.RData)
#' summarising the intersectional distributional impacts per selected set of variable.
#' If FALSE do not save.
#' @param file_name name of the file to save the results, if save TRUE. By default "DII_impacts".
#' @param fig generates and saves a graph that summarises the intersectional distributional
#' impacts. By default it is TRUE, for the graph/s not to be generated and saved indicate FALSE.
#' @param shocks_scenario_names vector of the names of the considered scenario shocks
#' @importFrom dplyr %>%
#' @return a list containing the generated datasets (.RData) summarising the intersectional
#' distributional impacts per selected set of variables.
#' @export
impact_intersectional_eu <- function(data, pairs = is_categories_eu, by_country = TRUE, save = T,
                                     file_name = "DII_impact", fig = T, shocks_scenario_names) {

  is_d_impacts <- list()
  missing_vars <- c()

  # Internal function to compute impacts
  compute_impacts <- function(df, var_a, var_b, tag) {
    gastotS_cols <- intersect(paste0("CP00_", shocks_scenario_names), names(df))
    for (s in shocks_scenario_names) {
      new_col <- paste0("CP00_", s)
      impact_col <- paste0("impact_", s)
      if (new_col %in% names(df)) {
        df[[impact_col]] <- 100 * (df$CP00 - df[[new_col]]) / df$CP00
      }
    }
    impact_cols <- grep("^impact_", names(df), value = TRUE)

    df_summarised <- df %>%
      dplyr::group_by(.data[[var_a]], .data[[var_b]]) %>%
      dplyr::summarise(
        VARIABLE_A = var_a,
        VARIABLE_B = var_b,
        WEIGHT = sum(weight, na.rm = TRUE),
        dplyr::across(all_of(impact_cols), ~ weighted.mean(., w = weight, na.rm = TRUE), .names = "DI_{.col}")
      ) %>%
      dplyr::rename_with(~ gsub("^DI_impact_", "DI_", .), dplyr::starts_with("DI_impact_")) %>%
      dplyr::rename(LABELS_A = 1, LABELS_B = 2) %>%
      dplyr::mutate(LABELS_A = as.character(LABELS_A), LABELS_B = as.character(LABELS_B), SOURCE = tag)

    return(df_summarised)
  }

  data$weight <- as.numeric(data$weight)

  # EU-wide results
  for (r in 1:nrow(pairs)) {
    var_a <- pairs$category_a[r]
    var_b <- pairs$category_b[r]

    if (var_a %in% colnames(data) & var_b %in% colnames(data)) {
      df_out <- compute_impacts(data, var_a, var_b, tag = "EU")
      is_d_impacts[[paste0("di_", var_a, "_", var_b)]] <- df_out
    } else {
      missing_vars <- c(missing_vars, var_a, var_b)
      warning(paste0("Missing: ", var_a, " or ", var_b))
    }
  }

  # By-country results
  if (by_country == TRUE) {
    countries <- unique(data$country)
    for (c in countries) {
      data_c <- data[data$country == c, ]
      for (r in 1:nrow(pairs)) {
        var_a <- pairs$category_a[r]
        var_b <- pairs$category_b[r]

        if (var_a %in% colnames(data_c) & var_b %in% colnames(data_c)) {
          df_out <- compute_impacts(data_c, var_a, var_b, tag = c)
          is_d_impacts[[paste0("di_", var_a, "_", var_b, "_", c)]] <- df_out
        }
      }
    }
  }

  if (save == TRUE) {
    if (!dir.exists("outputs_dii")) dir.create("outputs_dii")
    save(is_d_impacts, file = paste0("outputs_dii/", file_name, ".RData"))
  }

  if (fig == TRUE) {
    intersectional_graph_eu(data = is_d_impacts, pairs = pairs)
  }

  # Return combined results in a single data.frame
  combined_df <- dplyr::bind_rows(is_d_impacts, .id = "ID")
  return(combined_df)

}

