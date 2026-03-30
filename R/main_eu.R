#' country_code
#'
#' Function that returns the country codes for EU27
#' @return available countries of the package and their codes
#' @export
country_code <- function(){
  a_country <- av_country
  print(a_country)
}


#' ex_shocks_eu
#'
#' Function to save a csv file in which the change in prices must be
#' introduced to apply a price shock for EU countries. Save the file,
#' introduce the price shocks and load in R.
#' @return a csv file in which the change in prices must be introduced to apply a price shock to EU countries
#' @export
ex_shocks_eu <- function(){
  exshock <- get("shocks_eu")
  write.csv(exshock, file = "Example_shocks_eu.csv", row.names = F)
  print(paste0("The example file has been saved in`", getwd(),"/Example_shocks_eu.csv´" ))
}


#' available_var_eu
#'
#' Function that returns the variables for which the calculation of
#' basic distributional impacts is available for EU countries.
#' @return variables for which the calculation of basic distributional impacts is available
#' @export
available_var_eu <- function(){
  av_var_impact <- graph_labels_eu$VARIABLE
  print(av_var_impact)
}


#' available_var_intersec_eu
#'
#' Function that returns the set of variables for which the calculation
#' of intersectional distributional impacts is available for EU countries.
#' @return set of variables for which the calculation of intersectional
#' distributional impacts is available for EU countries
#' @export
available_var_intersec_eu <- function(){
  av_var_intersec <- is_categories_eu
  print(av_var_intersec)
}


#' ex_var_intersec_eu
#'
#' Function to save a csv file in which the set of variables must be
#' introduced for the calculation of intersectional distributional impacts
#' for EU countries. Save the file, keep just the combinations you are
#' interested in and load in R.
#' @return a csv file with the set of variables for which the calculation of
#' intersectional distributional impacts is available for EU countries
#' @export
ex_var_intersec_eu <- function(){
  av_var_intersec <- get("is_categories_eu")
  write.csv(av_var_intersec, file = "Var_Intersec_eu.csv", row.names = F)
  print(paste0("The example file has been saved in`", getwd(),"/Var_Intersec_eu.csv´" ))
}


#' hbs_eu
#'
#' Main function to generate the database (from EUROSTAT microdata) to calculate
#' the distributional impacts with calc_di_eu function.
#' @param year year of the HBS to process. Available options: 2010, 2015, 2020.
#' @param country code of the country or countries of the HBS to process and calculate
#' distributional impacts. By default, it processes all available data in the working
#' directory To see the available countries and codes, run country_code().
#' @param path Local path to the folder where the HBS's are stored (microdata from
#' EUROSTAT, not included in the package). In the defined folder, the data provided
#' by EUROSTAT must be saved in a folder with the name of the year to which they belong.
#' @importFrom dplyr %>%
#' @return a list containing the generated datasets summarising the basic or/and the
#' intersectional distributional impacts per selected variable or set of variables.
#' @export
hbs_eu <- function(year, country = "all", path) {

  # Process raw data from EUROSTAT
  rawhbs_eu(year, country = country, path)

  # Create a joint database for household and member files
  if (length(year) != 1) {
    # Check that only one year is provided
    stop("Error: You have provided a vector of years to the 'year' argument. This function only works with a single year.")
  } else {
    inputs_path <- file.path(path, "inputs")
    hbs <- database_hbs(year, country, inputs_path)
    hbs <- hbs[[paste0("hbs_", year)]]
  }

  # Rename expenditure variables
  hbs <- rename_coicop(data = hbs)

  return(hbs)

}


#' calc_di_eu
#'
#' Main function to calculate the distributional impacts for different price shocks
#' and to save the results and the figures.
#' @param data Local
#' @param update_hbs year to which you want to update the microdata. Include only
#' if you want to update the HBS expenditure data to a year other than the base
#' year. By default, the parameter is F, so skip this step.
#' @param shocks a dataset with the price shocks per coicop to be applied.
#' The format of the dataset has to correspond to the predefined one in the package.
#' To save a csv file with the right format to enter the price shocks run `ex_shocks_eu()`.
#' You can enter more scenarios by including more columns to the right with all
#' the country codes + the scenario name (e.g. AT_s3, BE_s3...). A price shock
#' greater than 1 indicates a price increase (e.g. 1.1 indicates a 10\% increase)
#' and less than 1 indicates a price decrease (e.g. 0.9 indicates a 10\% decrease).
#' @param outputs_path Local path to the folder where the results will be stored
#' (not included in the package). By default, it creates and saves the results in
#' the outputs folder within your working directory.
#' @param var variable(s) according to which you want to calculate distributional
#' impacts. If "all" (by default) calculates the distributional impacts for each of the
#' variables specified in the package.If not, you can indicate a variable or a vector
#' of variables to calculate distributional impacts. If you want to see the variables
#' for which the calculation is available run `available_var_eu()`.If you do not want
#' to calculate distributional impacts per variable indicate `NULL`.
#' @param var_intersec set of variables (2) according to which you want to calculate
#' the intersectional distributional impacts. If "all", it calculates the distributional
#' impacts for each of the combinations of variables specified in the package. If not,
#' you can indicate the set of variables according to which you want to calculate the
#' intersectional distributional impacts. If you wish to see the set of variables for
#' which the calculation is available, run `available_var_intersec()`. To enter a set
#' of variables for the calculation, it must follow the same format as the output of
#' `available_var_intersec()`, i.e. a table whose columns have category_a and category_b
#' as their titles.  If you do not want to calculate distributional impacts, indicate NULL (by default).
#' @param by_country If TRUE (by default) in addition to calculating the general distributional impacts
#' for all the households (EU level), it also calculates the distributional impacts for
#' each  country (and for each specified variables).
#' @param save if TRUE saves the dataframes with the results for the distributional
#' impacts calculated by the function. If FALSE do not save.
#' @param file_name name of the file to save the results of basic distributional
#' impacts, if save TRUE. By default "D_impacts".
#' @param file_name_intersec name of the file to save the results of the intersectional
#' distributional impacts, if save TRUE. By default "DI_impacts".
#' @param fig if TRUE (by default) create and save the figures of the distributional
#' impacts calculated by the function. If FALSE do not create neither save.
#' @importFrom dplyr %>%
#' @return a list containing the generated datasets summarising the basic or/and the
#' intersectional distributional impacts per selected variable or set of variables.
#' @export
calc_di_eu <-function(data, update_hbs=F, shocks, outputs_path=F, var = "all",
                      var_intersec = NULL, by_country = TRUE, save=T, fig=TRUE,
                      file_name = "D_impact", file_name_intersec = "DI_impact") {

  # Check var parameter
  if (!is.null(var) && !any(var == "all")) {
    check_var_eu(var)
  }

  # Update hbs
  if(update_hbs != F) {
    data <- update_year(data, new_year = update_hbs )
  }

  # Apply price shocks
  hbs <- price_shock_eu(data, shocks)

  # Set working directory to save results
  if (outputs_path == F) {
    outputs_path <- file.path(getwd(), "outputs")
    if (!dir.exists(outputs_path)) dir.create(outputs_path, recursive = TRUE)
    setwd(outputs_path)
  } else {
    setwd(outputs_path)
  }

  # Define names of the scenario shocks
  colnames_df <- colnames(shocks)
  colnames_df <- setdiff(colnames_df, c("names", "coicop"))
  shocks_scenario_names <- unique(sub(".*_", "", colnames_df))

  # Calculate and save distributional impacts + figures
  if(!is.null(var)){
    if(any(var == "all")){
      var = graph_labels_eu$VARIABLE
    }

    di <- impact_eu(data = hbs,
                    var = var,
                    by_country = by_country,
                    save = save,
                    file_name = file_name,
                    fig = fig,
                    shocks_scenario_names = shocks_scenario_names)
  }

  if(!is.null(var_intersec)){
    if(length(var_intersec) == 1 & is.character(var_intersec)){
      if(var_intersec == "all"){
        var_intersec = is_categories_eu
      }
    }

    dii <- impact_intersectional_eu(data = hbs,
                                    pairs = var_intersec,
                                    by_country = by_country,
                                    save = save,
                                    file_name = file_name_intersec,
                                    fig = fig,
                                    shocks_scenario_names = shocks_scenario_names)
  }

  # create a list to return
  return_list <- list()

  if(!is.null(var)){
    return_list[["di"]] <- di
  }

  if(!is.null(var_intersec)){
    return_list[["dii"]] <- dii
  }

  return(return_list)

}


#' calc_ep_eu
#'
#' Function to calculate energy poverty indices for EU countries.
#' Thresholds are calculated per member state.
#' @param data dataset with the EU HBS data (output from hbs_eu, with COICOP columns
#' renamed via rename_coicop). Must contain columns: CP045, CP00, CP0411, CP0421,
#' eq_size, weight, country.
#' @param index energy poverty index or indices to be calculated. Possible
#' options: "10%", "2M", "LIHC", "HEP", "HEP_LI". If "all" (by default) calculates
#' all the indices for each country.
#' @importFrom dplyr %>%
#' @importFrom tidyr pivot_longer pivot_wider
#' @return a dataframe with the selected energy poverty indices per country.
#' @export
calc_ep_eu <- function(data, index = "all") {

  accepted <- c("all", "10%", "2M", "LIHC", "HEP", "HEP_LI")
  missmatch <- setdiff(index, accepted)

  if (length(missmatch) == 1) {
    warning(sprintf('ATTENTION: The indicated index %s is not available. Possible options are %s.',
                    paste(missmatch, collapse = ", "), paste(accepted, collapse = ", ")))
  }
  if (length(missmatch) > 1) {
    warning(sprintf('ATTENTION: The indicated indices %s are not available. Possible options are %s.',
                    paste(missmatch, collapse = ", "), paste(accepted, collapse = ", ")))
  }

  # Identify energy poor households
  data <- id_ep_eu(data)

  # Calculate total households per country
  df <- data %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(
      TOT_FACTOR  = sum(weight, na.rm = TRUE),
      EP10PC      = sum(IEP10PC,   na.rm = TRUE) / TOT_FACTOR,
      EP2M        = sum(IEP2M,     na.rm = TRUE) / TOT_FACTOR,
      EPHEP       = sum(IEPHEP,    na.rm = TRUE) / TOT_FACTOR,
      EPHEP_LI    = sum(IEPHEP_LI, na.rm = TRUE) / TOT_FACTOR,
      EPLIHC      = sum(IEPLIHC,   na.rm = TRUE) / TOT_FACTOR,
      .groups     = "drop"
    ) %>%
    dplyr::select(-TOT_FACTOR) %>%
    tidyr::pivot_longer(
      cols      = c(EP10PC, EP2M, EPHEP, EPHEP_LI, EPLIHC),
      names_to  = "EP_index",
      values_to = "value"
    ) %>%
    dplyr::mutate(EP_index = dplyr::recode(EP_index,
                                           "EP10PC"   = "10%",
                                           "EP2M"     = "2M",
                                           "EPHEP"    = "HEP",
                                           "EPHEP_LI" = "HEP_LI",
                                           "EPLIHC"   = "LIHC")) %>%
    tidyr::pivot_wider(names_from = country, values_from = value)

  if (!any(index == "all")) {
    df <- df %>% dplyr::filter(EP_index %in% index)
  }

  return(df)
}


#' calc_tp_eu
#'
#' Function to calculate transport poverty indices for EU countries.
#' Thresholds are calculated per member state. Long-distance transport
#' (air and sea) is excluded from the calculation.
#' @param data dataset with the EU HBS data (output from hbs_eu, with COICOP columns
#' renamed via rename_coicop). Must contain columns: CP072, CP073, CP0731, CP0732,
#' CP0733, CP0734, CP0735, CP0411, CP0421, CP00, eq_size, weight, country.
#' @param index transport poverty index or indices to be calculated. Possible
#' options: "10%", "2M", "LIHC", "VTU". If "all" (by default) calculates all the
#' indices for each country.
#' @importFrom dplyr %>%
#' @importFrom tidyr pivot_longer pivot_wider
#' @return a dataframe with the selected transport poverty indices per country.
#' @export
calc_tp_eu <- function(data, index = "all") {

  accepted <- c("all", "10%", "2M", "LIHC", "VTU")
  missmatch <- setdiff(index, accepted)

  if (length(missmatch) == 1) {
    warning(sprintf('ATTENTION: The indicated index %s is not available. Possible options are %s.',
                    paste(missmatch, collapse = ", "), paste(accepted, collapse = ", ")))
  }
  if (length(missmatch) > 1) {
    warning(sprintf('ATTENTION: The indicated indices %s are not available. Possible options are %s.',
                    paste(missmatch, collapse = ", "), paste(accepted, collapse = ", ")))
  }

  # Identify transport poor households
  data <- id_tp_eu(data)

  # Calculate total households per country
  df <- data %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(
      TOT_FACTOR = sum(weight,    na.rm = TRUE),
      TP10PC     = sum(ITP10PC,   na.rm = TRUE) / TOT_FACTOR,
      TP2M       = sum(ITP2M,     na.rm = TRUE) / TOT_FACTOR,
      TPLIHC     = sum(ITPLIHC,   na.rm = TRUE) / TOT_FACTOR,
      TPVTU      = sum(ITPVTU,    na.rm = TRUE) / TOT_FACTOR,
      .groups    = "drop"
    ) %>%
    dplyr::select(-TOT_FACTOR) %>%
    tidyr::pivot_longer(
      cols      = c(TP10PC, TP2M, TPLIHC, TPVTU),
      names_to  = "TP_index",
      values_to = "value"
    ) %>%
    dplyr::mutate(TP_index = dplyr::recode(TP_index,
                                           "TP10PC"  = "10%",
                                           "TP2M"    = "2M",
                                           "TPLIHC"  = "LIHC",
                                           "TPVTU"   = "VTU")) %>%
    tidyr::pivot_wider(names_from = country, values_from = value)

  if (!any(index == "all")) {
    df <- df %>% dplyr::filter(TP_index %in% index)
  }

  return(df)
}
