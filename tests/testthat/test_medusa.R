library(medusa)
library(testthat)
library(magrittr)
library(png)

test_that("Test1_Available basic variables", {
  test_result <- available_var_impact()
  test_expect <- c("QUINTILE", "DECILE", "VENTILE", "PERCENTILE", "REGION",
                   "MUNISIZE", "ZONE", "HHTYPE", "CHILDREN", "POVERTY",
                   "GENDERRP", "FEMDEGREE", "AGERP", "COUNTRYRP", "STUDIESRP",
                   "PROFESSIONALSRP", "CONTTYPERP", "WORKDAYRP", "REGMR")
  testthat::expect_equal(test_result, test_expect)
})


test_that("Test2_Available intersectional variables", {
  test_result <- available_var_intersec()
  path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs")
  test_expect <- read.csv(file = paste0(path, "/is_categories.csv"),
                          header = TRUE,
                          sep = ",",
                          dec = ".")
  testthat::expect_equal(test_result, test_expect)
})


test_that("Test3_Example shocks", {
  setwd(file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs"))
  ex_shocks()
  path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs")
  test_result <- read.csv(file = paste0(path, "/Example_shocks.csv"),
                          header = TRUE,
                          sep = ",",
                          dec = ".")
  path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs")
  test_expect <- read.csv(file = paste0(path, "/shocks.csv"),
                          header = TRUE,
                          sep = ",",
                          dec = ".")
  testthat::expect_equal(test_result, test_expect)
})


test_that("Test4_Example intersectional variables csv", {
  setwd(file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs"))
  ex_var_intersec()
  path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs")
  test_result <- read.csv(file = paste0(path, "/Var_Intersec.csv"),
                          header = TRUE,
                          sep = ",",
                          dec = ".")

   path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs")
  test_expect <- read.csv(file = paste0(path, "/is_categories.csv"),
                          header = TRUE,
                          sep = ",",
                          dec = ".")

   testthat::expect_equal(test_result, test_expect)
})


test_that("Test5_Load raw hbs data", {
  y <- seq(2006,2021,1)
  for (year in y){

  path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs/csv")
  epf_hh <- read.csv(file.path(path, paste0("epf_", year, "_h.csv")))
  test_expect <- round(sum(epf_hh$GASTOT/epf_hh$FACTOR, na.rm = TRUE))

  load_rawhbs(year = year, path = file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs/csv"),
              path_outputs = file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs"))
  load(file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs",paste0("/epf_list_", year, ".RData")))
  epf_hg <- epf_list$epf_hg
  test_result <- round(sum(epf_hg$GASTOT/epf_hg$FACTOR, na.rm = TRUE))

  testthat::expect_equal(test_result, test_expect)
  }
})


test_that("Test6_Standardize & rename values", {
  path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs")
  test_expect <- read.csv(file.path(path, "ex_dataset_renamed.csv"))

  epf_hh <- read.csv(file.path(path, "ex_dataset.csv"))
  test_result <- standardize(epf_hh)

  testthat::expect_equal(test_result, test_expect)
})


test_that("Test7_Add coicop", {
  y <- seq(2006,2021,1)
  for (year in y){
  path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs/coicop")
  lists <- read.csv(file.path(path, paste0("coicop_", year, ".csv")))
  for (r in colnames(lists)) {
    assign(r, lists %>% dplyr::filter(nchar(get(r))>0) %>% dplyr::pull(r))
  }
  test_expect <- coicop

  hbs <- get(paste0("epf_list_", year))
  epf_hg  <- hbs$epf_hg
  epf_hg <- add_coicop(epf_hg, year)
  test_result <- epf_hg %>% dplyr:: select(starts_with("EUR_A_")) %>% colnames()
  testthat::expect_equal(test_result, test_expect)
  }
})


test_that("Test8_Elevate_hbs population", {
  y <- seq(2006,2021,1)
  country <-"ES"
  for (year in y){
  pop_NA <- restatapi::get_eurostat_data("demo_gind",
                                         filters = c("AVG", country),
                                         date_filter = year)
  pop_NA <- pop_NA$values
  test_expect <- round( pop_NA, digits = 0)

  hbs <- get(paste0("epf_list_", year))
  epf_hg  <- hbs$epf_hg
  epf_hg <- add_coicop(epf_hg, year)
  epf <- elevate_hbs(epf_hg, year)
  test_result <- round(sum(epf$POBLACIONCN), digits = 0)

  testthat::expect_equal(test_result, test_expect)
  }
})


test_that("Test8_Elevate_hbs expenses", {
  y <- seq(2006,2021,1)
  country <-"ES"
  for (year in y){

    macro <- gcfhogares95_22 %>%
      dplyr::select(COICOP, as.character(year[1])) %>%
      dplyr::rename('macro_ref' = as.character({{year}}))
    macro$macro_ref <- as.numeric(gsub(",","",macro$macro_ref))
    gf_na <- macro %>%
      dplyr::filter(COICOP == "TOTAL") %>%
      dplyr::select(macro_ref)
    gf_na <- as.numeric(gf_na$macro_ref)
    test_expect <- gf_na * 1000000

    hbs <- get(paste0("epf_list_", year))
    epf_hg  <- hbs$epf_hg
    epf_hg <- add_coicop(epf_hg, year)
    epf <- elevate_hbs(epf_hg, year)
    test_result <- round(sum(epf$GASTOT_CNR), digits = 0)

    testthat::expect_equal(test_result, test_expect)
  }
})


test_that("Test9_Price_shock", {
  path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs")
  epf <- read.csv(file.path(path, "ex_dataset_expenses.csv"))
  shocks <- read.csv(file.path(path, "shocks_ps.csv"))
  test_result <- price_shock(epf, shocks, 2006)

  test_expect <- read.csv(file.path(path, "ex_dataset_expenses_ps.csv"))

  testthat::expect_equal(test_result, test_expect)
})


test_that("Test10_Impact", {
  path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs")
  epf <- read.csv(file.path(path, "ex_dataset_expenses_ps.csv"))
  shocks <- read.csv(file.path(path, "shocks_ps.csv"))
  shocks_scenario_names <- names(shocks)[3:length(names(shocks))]
  path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs")
  setwd(path)
  test_result <- impact(epf, shocks_scenario_names = shocks_scenario_names)

  test_expect <- load(path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs/D_impacts.RData"))
  test_expect <- d_impacts

  testthat::expect_equal(test_result, test_expect)
})


test_that("Test11_Basic graph", {

  test_result <- readPNG(file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs/figures/DI_AGERP.png"))
  test_expect <- readPNG(file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs/figures/DI_AGERP.png"))

  test_result2 <- readPNG(file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs/figures/DI_COUNTRYRP.png"))
  test_expect2 <- readPNG(file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs/figures/DI_COUNTRYRP.png"))

  test_result3 <- readPNG(file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs/figures/DI_GENDERRP.png"))
  test_expect3 <- readPNG(file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs/figures/DI_GENDERRP.png"))

  test_result4 <- readPNG(file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs/figures/DI_HHTYPE.png"))
  test_expect4 <- readPNG(file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs/figures/DI_HHTYPE.png"))

  test_result5 <- readPNG(file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs/figures/DI_MUNISIZE.png"))
  test_expect5 <- readPNG(file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs/figures/DI_MUNISIZE.png"))

  test_result6 <- readPNG(file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs/figures/DI_REGION.png"))
  test_expect6 <- readPNG(file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs/figures/DI_REGION.png"))

  test_result7 <- readPNG(file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs/figures/DI_REGMR.png"))
  test_expect7 <- readPNG(file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs/figures/DI_REGMR.png"))

  test_result8 <- readPNG(file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs/figures/DI_STUDIESRP.png"))
  test_expect8 <- readPNG(file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs/figures/DI_STUDIESRP.png"))

  test_result9 <- readPNG(file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs/figures/DI_ZONE.png"))
  test_expect9 <- readPNG(file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs/figures/DI_ZONE.png"))

  testthat::expect_equal(test_result, test_expect)
  testthat::expect_equal(test_result2, test_expect2)
  testthat::expect_equal(test_result3, test_expect3)
  testthat::expect_equal(test_result4, test_expect4)
  testthat::expect_equal(test_result5, test_expect5)
  testthat::expect_equal(test_result6, test_expect6)
  testthat::expect_equal(test_result7, test_expect7)
  testthat::expect_equal(test_result8, test_expect8)
  testthat::expect_equal(test_result9, test_expect9)
})
#
#
# test_that("Test12_Impact Intersectional", {
#   path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs")
#   epf <- read.csv(file.path(path, "ex_dataset_expenses_ps.csv"))
#   shocks <- read.csv(file.path(path, "shocks_ps.csv"))
#   shocks_scenario_names <- names(shocks)[3:length(names(shocks))]
#   category_a <- c("AGERP", "GENDERRP", "COUNTRYRP", "ZONE", "REGION")
#   category_b <- c("GENDERRP", "COUNTRYRP", "REGMR", "HHTYPE", "MUNISIZE")
#   pairs <- data.frame(category_a, category_b)
#   path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs")
#   setwd(path)
#   test_result <- impact_intersectional(epf, pairs = pairs , shocks_scenario_names = shocks_scenario_names)
#
#   test_expect <- load(path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs/DI_impacts.RData"))
#   test_expect <- di_impacts
#
#   testthat::expect_equal(test_result, test_expect)
# })
