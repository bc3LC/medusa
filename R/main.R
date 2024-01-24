# calc_di
#'
#' Details: main function to calculate distributional impacts for different price shocks
#'
#' @param year     year for simulation, options = c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)
#' @param elevate  elevation to national accounting, options = c (T, F)
#' @param var_impact  variable/s de acuerdo a la/s que quieres calcular el impacto distributivo, por defecto van a ser todos, pero se puede seleccionar solo una variable o un vector de variables, si no quieres indicar NULL, si quieres ver todas las opciones corre `available_var_impact()`
#' @param var_intersec  variables de acuerdo a las que quieres calcular el impacto distributivo interseccional (es decir, se pueden cruzar dos variables), por defecto va a ser NULL, si quieres insertar una tabla con columnas llamadas category_a y category_b
#' @param fig  if TRUE (by default) create and save the figures of the distributional impacts. If FALSE do not create neither save.
#' @param save     save the results, options = c (T, F)

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
#' Details: main function to calculate distributional impacts for different price shocks
#'
#' @export
available_var_impact <- function(){
  av_var_impact <- categories$categories
  print(av_var_impact)
}


