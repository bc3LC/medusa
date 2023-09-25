# calc_di
#'
#' Details: main function to calculate distributional impacts for different price shocks
#'
#' @param input from where to load input data, options = c(manual, gcam)
#' @param year  year for simulation, options = c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)
#' @param elevate  elevation to national accounting, options = c (T, F)
#' @param save save the results, options = c (T, F)

calc_di <-function(input, year,  mapping, elevate=F, save=T) {

  # load hbs files
  raw_hbs <- load_hbs(year)

  epf_hg <- raw_hbs$epf_hg
  epf_hgm <- raw_hbs$epf_hgm
  epf_hc <- raw_hbs$epf_hc





}



