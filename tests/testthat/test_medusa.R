library(medusa)
library(testthat)
library(magrittr)

# test_that("Test1_Available basic variables", {
#   test_result <- available_var_impact()
#   test_expect <- c("QUINTILE", "DECILE", "VENTILE", "PERCENTILE", "REGION",
#                    "MUNISIZE", "ZONE", "HHTYPE", "CHILDREN", "POVERTY",
#                    "GENDERRP", "FEMDEGREE", "AGERP", "COUNTRYRP", "STUDIESRP",
#                    "PROFESSIONALSRP", "CONTTYPERP", "WORKDAYRP", "REGMR")
#   testthat::expect_equal(test_result, test_expect)
# })
#
#
# test_that("Test2_Available intersectional variables", {
#   test_result <- available_var_intersec()
#   path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs")
#   test_expect <- read.csv(file = paste0(path, "/is_categories.csv"),
#                           header = TRUE,
#                           sep = ",",
#                           dec = ".")
#   testthat::expect_equal(test_result, test_expect)
# })
#
#
# test_that("Test3_Example shocks", {
#   setwd(file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs"))
#   ex_shocks()
#   path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs")
#   test_result <- read.csv(file = paste0(path, "/Example_shocks.csv"),
#                           header = TRUE,
#                           sep = ",",
#                           dec = ".")
#   path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs")
#   test_expect <- read.csv(file = paste0(path, "/shocks.csv"),
#                           header = TRUE,
#                           sep = ",",
#                           dec = ".")
#   testthat::expect_equal(test_result, test_expect)
# })
#
#
# test_that("Test4_Example intersectional variables csv", {
#   setwd(file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs"))
#   ex_var_intersec()
#   path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs")
#   test_result <- read.csv(file = paste0(path, "/Var_Intersec.csv"),
#                           header = TRUE,
#                           sep = ",",
#                           dec = ".")
#
#    path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs")
#   test_expect <- read.csv(file = paste0(path, "/is_categories.csv"),
#                           header = TRUE,
#                           sep = ",",
#                           dec = ".")
#
#    testthat::expect_equal(test_result, test_expect)
# })
#
#
# test_that("Test5_Load raw hbs data", {
#   y <- seq(2006,2021,1)
#   for (year in y){
#
#   path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs/csv")
#   epf_hh <- read.csv(file.path(path, paste0("epf_", year, "_h.csv")))
#   test_expect <- round(sum(epf_hh$GASTOT/epf_hh$FACTOR, na.rm = TRUE))
#
#   load_rawhbs(year = year, path = file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs/csv"),
#               path_outputs = file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs"))
#   load(file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs",paste0("/epf_list_", year, ".RData")))
#   epf_hg <- epf_list$epf_hg
#   test_result <- round(sum(epf_hg$GASTOT/epf_hg$FACTOR, na.rm = TRUE))
#
#   testthat::expect_equal(test_result, test_expect)
#   }
# })
#
#
# test_that("Test6_Standardize & rename values", {
#   path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs")
#   test_expect <- read.csv(file.path(path, "ex_dataset_renamed.csv"))
#
#   epf_hh <- read.csv(file.path(path, "ex_dataset.csv"))
#   test_result <- standardize(epf_hh)
#
#   testthat::expect_equal(test_result, test_expect)
# })
#

test_that("Test7_Add coicop", {
  y <- seq(2006,2021,1)
  for (year in y){
  path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs/coicop")
  lists <- read.csv(file.path(path, paste0("coicop_", year, ".csv")))
  for (r in colnames(lists)) {
    assign(r, lists %>% dplyr::filter(nchar(get(r))>0) %>% dplyr::pull(r))      # Extrae una columna y se le asigna al nombre de la columna en un vector
  }
  test_expect <- coicop

  hbs <- get(paste0("epf_list_", year))
  epf_hg  <- hbs$epf_hg
  epf_hg <- add_coicop(epf_hg, year)
  test_result <- dplyr::filter(epf_hg, "EUR_A_")
  test_result <- epf_hg %>% dplyr:: select(starts_with("EUR_A_")) %>% colnames()
  testthat::expect_equal(test_result, test_expect)
  }
})


# test_that("Test8_Elevate_hbs", {
#
# })
#
# test_that("Test9_Price_shock", {
#
# })
