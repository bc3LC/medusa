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


#' available_var_impact_eu
#'
#' Function that returns the variables for which the calculation of
#' basic distributional impacts is available for EU countries.
#' @return variables for which the calculation of basic distributional impacts is available
#' @export
available_var_impact_eu <- function(){
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
available_var_intersec <- function(){
  av_var_intersec <- is_categories_eu
  print(av_var_intersec)
}
