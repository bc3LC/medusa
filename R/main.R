# calc_di
#'
#' Details: main function to calculate distributional impacts for different price shocks
#'
#' @param input    from where to load input data, options = c(manual, gcam)
#' @param year     year for simulation, options = c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)
#' @param country  country for simulation, in this case the package works only for Spain (ES)
#' @param elevate  elevation to national accounting, options = c (T, F)
#' @param save     save the results, options = c (T, F)

calc_di <-function(input, year, elevate=F, save=T) {

  # get hbs files
  hbs <- get(paste0("epf_list_", year))

  epf_hg  <- hbs$epf_hg
  epf_hgm <- hbs$epf_hgm
  epf_hc  <- hbs$epf_hc

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





}



