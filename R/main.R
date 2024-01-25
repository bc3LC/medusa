# calc_di
#'
#' Details: main function to calculate the distributional impacts for different price shocks and to save the results and the figures
#'
#' @param year year for the simulation. Available time series: 2006-2021
#' @param elevate if TRUE elevate the HBS to national accounting. If FALSE (by default) do not elevate.
#' @param var_impact variable(s) according to which you want to calculate distributional impacts. If "all" (by default) calculates the distributional impacts for each of the variables specified in the package. If not, you can indicate the variable on the basis of which you want to calculate the distributional impacts. If not, you can indicate a variable or a vector of variables to calculate distributional impacts.If you want to see the variables for which the calculation is available run `available_var_impact()`. If you do not want to calculate distributional impacts per variable indicate NULL.
#' @param var_intersec set of variables (2) according to which you want to calculate the intersectional distributional impacts. If "all", it calculates the distributional impacts for each of the combinations of variables specified in the package. If not, you can indicate the set of variables according to which you want to calculate the intersectional distributional impacts. If you wish to see the set of variables for which the calculation is available, run `available_var_intersec()`. To enter a set of variables for the calculation, it must follow the same format as the output of `available_var_intersec()`, i.e. a table whose columns have category_a and category_b as their titles.  If you do not want to calculate distributional impacts, indicate NULL (by default).
#' @param fig if TRUE (by default) create and save the figures of the distributional impacts calculated by the function. If FALSE do not create neither save.
#' @param save if TRUE saves the dataframes with the results for the distributional impacts calculated by the function. If FALSE do not save.
#' @export
calc_di <-function(year, elevate=F, var_impact = "all", var_intersec = NULL,
                   fig=T, save=T, file_name_impact = "D_impact", file_name_intersec = "DI_impact") {

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
    for (c in coicop) {
      new = paste0( c, "_CNR")
      var = c
      epf <- epf %>%
        dplyr::mutate({{new}} := get(var))
    }
  }

   epf <- price_shock(epf)

  # calculate distributional impacts
  if(!is.null(var_impact)){

    if(var_impact == "all"){
      var_impact = categories$categories
      }
    di <- impact(epf, var = var_impact, fig = fig, save = save, file_name = file_name_impact)

  }

   if(!is.null(var_intersec)){
     if(var_intersec == "all"){
       var_intersec = is_categories
     }
     dii <- impact_intersectional(epf, pairs = var_intersec, fig, save = save, file_name = file_name_intersec)

   }

}

# available_var_impact
#'
#' Details: function that returns the variables for which the calculation of basic distributional impacts is available
#' @export
available_var_impact <- function(){
  av_var_impact <- categories$categories
  print(av_var_impact)
}

# available_var_intersec
#'
#' Details: function that returns the variables for which the calculation of intersectional distributional impacts is available
#' @export
available_var_intersec <- function(){
  av_var_intersec <- is_categories
  print(av_var_intersec)
}
