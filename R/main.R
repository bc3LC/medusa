#' ex_shocks
#'
#' Function to save a csv file in which the change in prices must be
#' introduced to apply a price shock. Save the file, introduce the price shocks
#' and load in R.The COICOP variables of the file correspond to the aggregate
#' variables of the package, if you are not going to aggregate the COICOP
#' variables you have to replace the column labels by the COICOP variables that
#' appear in your dataset.
#' @return a csv file in which the change in prices must be introduced to apply a price shock
#' @export
ex_shocks <- function(){
  exshock <- get("shocks")
  write.csv(exshock, file = "Example_shocks.csv", row.names = F)
}

#' calc_di
#'
#' Main function to calculate the distributional impacts for different price shocks
#' and to save the results and the figures.
#' @param year year for the simulation. Available time series: 2006-2021.
#' @param elevate if TRUE elevate the HBS to national accounting. If FALSE (by default) do not elevate.
#' @param shocks a dataset with the price shocks per coicop to be applied.
#' The format of the dataset has to correspond to the predefined one in the package.
#' To save a csv file with the right format to enter the price shocks run `ex_shocks()`.
#' You can enter more scenarios by including more columns to the right (e.g. s3).
#' A price shock greater than 1 indicates a price increase
#' (e.g. 1.1 indicates a 10\% increase) and less than 1 indicates a price decrease
#' (e.g. 0.9 indicates a 10\% decrease).
#' @param var_impact variable(s) according to which you want to calculate distributional
#' impacts. If "all" (by default) calculates the distributional impacts for each of the
#' variables specified in the package. If not, you can indicate the variable on the basis
#' of which you want to calculate the distributional impacts. If not, you can indicate a
#' variable or a vector of variables to calculate distributional impacts.If you want to
#' see the variables for which the calculation is available run `available_var_impact()`.
#' If you do not want to calculate distributional impacts per variable indicate `NULL`.
#' @param var_intersec set of variables (2) according to which you want to calculate
#' the intersectional distributional impacts. If "all", it calculates the distributional
#' impacts for each of the combinations of variables specified in the package. If not,
#' you can indicate the set of variables according to which you want to calculate the
#' intersectional distributional impacts. If you wish to see the set of variables for
#' which the calculation is available, run `available_var_intersec()`. To enter a set
#' of variables for the calculation, it must follow the same format as the output of
#' `available_var_intersec()`, i.e. a table whose columns have category_a and category_b
#' as their titles.  If you do not want to calculate distributional impacts, indicate NULL (by default).
#' @param save if TRUE saves the dataframes with the results for the distributional
#' impacts calculated by the function. If FALSE do not save.
#' @param file_name_impact name of the file to save the results of basic distributional
#' impacts, if save TRUE. By default "D_impacts".
#' @param file_name_intersec name of the file to save the results of the intersectional
#' distributional impacts, if save TRUE. By default "DI_impacts".
#' @param fig if TRUE (by default) create and save the figures of the distributional
#' impacts calculated by the function. If FALSE do not create neither save.
#' @importFrom dplyr %>%
#' @return a list containing the generated datasets summarising the basic or/and the
#' intersectional distributional impacts per selected variable or set of variables.
#' @export
calc_di <-function(year, elevate=F, shocks, var_impact = "all", var_intersec = NULL, save=T,
                   file_name_impact = "D_impact", file_name_intersec = "DI_impact", fig=T) {

  # Check year parameter
  check_year(year)

  # get hbs files
  hbs <- get(paste0("epf_list_", year))

  epf_hg  <- hbs$epf_hg

  # add coicop categories
  epf_hg <- add_coicop(epf_hg, year)

  # elevate the hbs
  if(elevate == T){
    epf <- elevate_hbs(epf_hg, year)
  }

  # apply the price shocks
  if(elevate == F){
    epf <- epf_hg

    # Get the mapping list
    lists <- get(paste0("coicop_", year))

    # Convert lists df to vectors
    for (r in colnames(lists)) {
      assign(r, lists %>% dplyr::filter(nchar(get(r))>0) %>% dplyr::pull(r))      # Extrae una columna y se le asigna al nombre de la columna en un vector
    }

    for (c in coicop) {
      new = paste0( c, "_CNR")
      var = c
      epf <- epf %>%
        dplyr::mutate({{new}} := get(var))
    }

    epf <- epf %>%
      dplyr::mutate(GASTOT_CNR = rowSums(dplyr::select(epf, contains('_CNR'))))
  }

  epf <- price_shock(epf, shocks = shocks, year = year)

  # names of the scenario shocks
  shocks_scenario_names = names(shocks)[3:length(names(shocks))]

  # calculate distributional impacts
  if(!is.null(var_impact)){

    if(var_impact == "all"){
      var_impact = categories$categories
      }
    di <- impact(epf, var = var_impact, save = save,
                 file_name = file_name_impact, fig = fig,
                 shocks_scenario_names = shocks_scenario_names)

  }

   if(!is.null(var_intersec)){
     if(length(var_intersec) == 1 & is.character(var_intersec)){
       if(var_intersec == "all"){

         var_intersec = is_categories

       }
     }
     dii <- impact_intersectional(epf, pairs = var_intersec, save = save,
                                  file_name = file_name_intersec, fig = fig,
                                  shocks_scenario_names = shocks_scenario_names)

   }

   # create a list to return
   return_list <- list()

   if(!is.null(var_impact)){
     return_list[["di"]] <- di
   }

   if(!is.null(var_intersec)){
     return_list[["dii"]] <- dii
   }

   return(return_list)
}

#' available_var_impact
#'
#' Function that returns the variables for which the calculation of
#' basic distributional impacts is available.
#' @return variables for which the calculation of basic distributional impacts is available
#' @export
available_var_impact <- function(){
  av_var_impact <- categories$categories
  print(av_var_impact)
}

#' available_var_intersec
#'
#' Function that returns the set of variables for which the calculation
#' of intersectional distributional impacts is available.
#' @return set of variables for which the calculation of intersectional
#' distributional impacts is available
#' @export
available_var_intersec <- function(){
  av_var_intersec <- is_categories
  print(av_var_intersec)
}

#' ex_var_intersec
#'
#' Function to save a csv file in which the set of variables must be
#' introduced for the calculation of intersectional distributional impacts.
#' Save the file, keep just the conbinations you are interested in and load in R.
#' @return a csv file with the set of variables for which the calculation of
#' intersectional distributional impacts is available
#' @export
ex_var_intersec <- function(){
  av_var_intersec <- get("is_categories")
  write.csv(av_var_intersec, file = "Var_Intersec.csv", row.names = F)
}

#' calc_ep
#'
#' Function to calculate energy poverty indices
#' @param year year/s for energy poverty indices calculation
#' @param index energy poverty index or indices to be calculated. Possible
#' options: 10%, 2M, LIHC, HEP, HEP_LI. If "all" (by default) calculates
#' all the indices for the selected year/s.
#' @importFrom dplyr %>%
#' @return a dataframe with the selected energy poverty indices
#' @export
calc_ep <- function(year, index = "all"){

  if(is.character(year)) {
    year <- as.numeric(year)
  }

  if (!year %in% seq(2006,2021,1)) {
    stop(sprintf('You introduced year %s which is not available. Possible options are %s.',
                 year, paste(seq(2006,2021,1), collapse = ", ")))
  }

  accepted <- c("all",
                "10%",
                "2M",
                "LIHC",
                "HEP",
                "HEP_LI")

  missmatch <- setdiff(index, accepted)

  if (length(missmatch) == 1) {
    warning(sprintf('ATTENTION: The indicated index %s is not available. Possible options are %s.',
                    paste(missmatch, collapse = ", "),  paste(accepted, collapse = ", ")))
  }

  if (length(missmatch) > 1) {
    warning(sprintf('ATTENTION: The indicated indices %s are not available. Possible options are %s.',
                    paste(missmatch, collapse = ", "),  paste(accepted, collapse = ", ")))
  }

  df <- data.frame("EP_index" = c("10%",
                                  "2M",
                                  "LIHC",
                                  "HEP",
                                  "HEP_LI"))

  # Loop to calculate the indices for diferent years
  for (y in year) {

    # get hbs files
    hbs <- get(paste0("epf_list_", y))
    epf_hg  <- hbs$epf_hg

    # Calculate total households
    TOT_FACTOR <- sum(epf_hg$FACTOR)

    # 10%
    TOT_IEP10PC <- sum(epf_hg$IEP10PC)
    EP10PC      <- TOT_IEP10PC/TOT_FACTOR

    # 2M
    TOT_IEP2M  <- sum(epf_hg$IEP2M)
    EP2M       <- TOT_IEP2M /TOT_FACTOR

    # Hidden Energy Poverty (HEP)
    TOT_IEPHEP <- sum(epf_hg$IEPHEP)
    EPHEP      <- TOT_IEPHEP/TOT_FACTOR

    # Hidden Energy Poverty Low Income (HEP_LI)
    TOT_IEPHEP_LI <- sum(epf_hg$IEPHEP_LI)
    EPHEP_LI      <- TOT_IEPHEP_LI/TOT_FACTOR

    # Low Income High Cost (LIHC)
    TOT_IEPLIHC <- sum(epf_hg$IEPLIHC)
    EPLIHC      <- TOT_IEPLIHC/TOT_FACTOR

    y <- as.character(y)

    # Create a dataframe with the indices
    df[[y]] <- c(EP10PC, EP2M, EPLIHC, EPHEP, EPHEP_LI)
  }

  if (index != "all") {
    df <- df %>%
      dplyr::filter(EP_index %in% index)
  }

  return(df)
}

#' calc_tp
#'
#' Function to calculate transport poverty indices
#' @param year year/s for transport poverty indices calculation
#' @param index transport poverty index or indices to be calculated. Possible
#' options: 10%, 2M, LIHC, VTU. If "all" (by default) calculates all the indices
#' for the selected year/s.
#' @importFrom dplyr %>%
#' @return a dataframe with the selected transport poverty indices
#' @export
calc_tp <- function(year, index = "all"){

  if(is.character(year)) {
    year <- as.numeric(year)
  }

  if (!year %in% seq(2006,2021,1)) {
    stop(sprintf('You introduced year %s which is not available. Possible options are %s.',
                 year, paste(seq(2006,2021,1), collapse = ", ")))
  }

  accepted <- c("all",
                "10%",
                "2M",
                "LIHC",
                "VTU")

  missmatch <- setdiff(index, accepted)

  if (length(missmatch) == 1) {
    warning(sprintf('ATTENTION: The indicated index %s is not available. Possible options are %s.',
                    paste(missmatch, collapse = ", "),  paste(accepted, collapse = ", ")))
  }

  if (length(missmatch) > 1) {
    warning(sprintf('ATTENTION: The indicated indices %s are not available. Possible options are %s.',
                    paste(missmatch, collapse = ", "),  paste(accepted, collapse = ", ")))
  }

  df <- data.frame("TP_index" = c("10%",
                                  "2M",
                                  "LIHC",
                                  "VTU"))

  # Loop to calculate the indices for different years
  for (y in year) {

    # get hbs files
    hbs <- get(paste0("epf_list_", y))
    epf_hg  <- hbs$epf_hg

    # Calculate total households
    TOT_FACTOR <- sum(epf_hg$FACTOR)

    # 10%
    TOT_ITP10PC <- sum(epf_hg$ITP10PC)
    TP10PC      <- TOT_ITP10PC/TOT_FACTOR

    # 2M
    TOT_ITP2M  <- sum(epf_hg$ITP2M)
    TP2M       <- TOT_ITP2M /TOT_FACTOR

    # Low Income High Cost (LIHC)
    TOT_ITPLIHC <- sum(epf_hg$ITPLIHC)
    TPLIHC      <- TOT_ITPLIHC/TOT_FACTOR

    # Vulnerable Transport Users (VTU)
    TOT_ITPVTU <- sum(epf_hg$ITPVTU)
    TPVTU      <- TOT_ITPVTU/TOT_FACTOR

    y <- as.character(y)

    # Create a dataframe with the indices
    df[[y]] <- c(TP10PC, TP2M, TPLIHC, TPVTU)
  }

  if (index != "all") {
    df <- df %>%
      dplyr::filter(TP_index %in% index)
  }

  return(df)
}
