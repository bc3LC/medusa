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

  # load hbs files
  hbs <- load_hbs(year)

  epf_hg  <- hbs$epf_hg
  epf_hgm <- hbs$epf_hgm
  epf_hc  <- hbs$epf_hc

  # add coicop categories
  epf_hg <- add_coicop(year)

  # elevate the hbs
  if(elevate=T){
    epf <- elevate_hbs(year)
    }

  # apply the shocks



  # calculate distributional impacts





}



