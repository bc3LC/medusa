library(medusa)
library(testthat)
library(magrittr)

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
                          fileEncoding = "UTF-8-BOM",
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
                          fileEncoding = "UTF-8-BOM",
                          header = TRUE,
                          sep = ",",
                          dec = ".")
  path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs")
  test_expect <- read.csv(file = paste0(path, "/shocks.csv"),
                          fileEncoding = "UTF-8-BOM",
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
                          fileEncoding = "UTF-8-BOM",
                          header = TRUE,
                          sep = ",",
                          dec = ".")

  path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs")
  test_expect <- read.csv(file = paste0(path, "/is_categories.csv"),
                          fileEncoding = "UTF-8-BOM",
                          header = TRUE,
                          sep = ",",
                          dec = ".")

  testthat::expect_equal(test_result, test_expect)
})


test_that("Test5_Load raw hbs data", {
  #y <- seq(2006,2021,1)
  y <- c(2006,2012,2019)

  db_path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs")
  options(timeout = max(300, getOption("timeout"))) #increase downloading time
  rpackageutils::download_unpack_zip(
    data_directory = db_path,
    url = "https://zenodo.org/record/10654144/files/csv.zip?download=1")

  for (year in y){
    path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs/csv")
    epf_hh <- read.csv(file.path(path, paste0("/epf_", year, "_h.csv")), header=T, fileEncoding = "UTF-8-BOM")
    test_expect <- round(sum(epf_hh$GASTOT/epf_hh$FACTOR, na.rm = TRUE))

    load_rawhbs(year = year, path = file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs/csv/"),
                path_outputs = file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs"))
    load(file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs",paste0("/epf_list_", year, ".RData")))
    epf_hg <- epf_list$epf_hg
    test_result <- round(sum(epf_hg$GASTOT/epf_hg$FACTOR, na.rm = TRUE))

    testthat::expect_equal(test_result, test_expect)
  }
})


test_that("Test6_Standardize & rename values", {
  path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs")
  test_expect <- read.csv(file.path(path, "ex_dataset_renamed.csv"), header=T, fileEncoding = "UTF-8-BOM")

  epf_hh <- read.csv(file.path(path, "ex_dataset.csv"), header=T, fileEncoding = "UTF-8-BOM")
  test_result <- standardize(epf_hh)

  testthat::expect_equal(test_result, test_expect)
})


test_that("Test7_Add coicop", {
  #y <- seq(2006,2021,1)
  y <- c(2006,2012,2019)

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
  #y <- seq(2006,2021,1)
  y <- c(2006,2012,2019)

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
  #y <- seq(2006,2021,1)
  y <- c(2006,2012,2019)

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
  epf <- read.csv(file.path(path, "ex_dataset_expenses.csv"), header=T, fileEncoding = "UTF-8-BOM")
  shocks <- read.csv(file.path(path, "shocks_ps.csv"), header=T, fileEncoding = "UTF-8-BOM")
  test_result <- price_shock(epf, shocks, 2006)

  test_expect <- read.csv(file.path(path, "ex_dataset_expenses_ps.csv"), header=T, fileEncoding = "UTF-8-BOM")

  testthat::expect_equal(test_result, test_expect)
})


test_that("Test10_Impact & basic graph", {
  path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs")
  epf <- read.csv(file.path(path, "ex_dataset_expenses_ps.csv"), header=T, fileEncoding = "UTF-8-BOM")
  decode_html <- function(text) {
    xml2::xml_text(xml2::read_xml(paste0("<x>", text, "</x>")))
  }
  epf$REGION <- sapply(epf$REGION, decode_html)

  shocks <- read.csv(file.path(path, "shocks_ps.csv"), header=T, fileEncoding = "UTF-8-BOM")
  shocks_scenario_names <- names(shocks)[3:length(names(shocks))]
  path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs")
  setwd(path)
  test_result <- impact(epf, shocks_scenario_names = shocks_scenario_names)

  assign("test_expect", get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs/D_impacts.RData"))))

  testthat::expect_equal(test_result, test_expect)

  #Check figures
  vars <- c("AGERP", "COUNTRYRP", "GENDERRP", "HHTYPE", "MUNISIZE", "REGION", "REGMR", "STUDIESRP", "ZONE" )
  for (g in vars) {
    test_result <- png::readPNG(file.path(rprojroot::find_root(rprojroot::is_testthat), paste0("test_inputs/figures/DI_",g,".png")))
    test_expect <- png::readPNG(file.path(rprojroot::find_root(rprojroot::is_testthat), paste0("test_outputs/figures/DI_",g,".png")))

    testthat::expect_true(identical(attributes(test_result), attributes(test_expect)))
  }
})


test_that("Test11_Impact Intersectional", {
  path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs")
  epf <- read.csv(file.path(path, "ex_dataset_expenses_ps.csv"), header=T, fileEncoding = "UTF-8-BOM")
  shocks <- read.csv(file.path(path, "shocks_ps.csv"), header=T, fileEncoding = "UTF-8-BOM")
  shocks_scenario_names <- names(shocks)[3:length(names(shocks))]
  category_a <- c("AGERP", "GENDERRP", "COUNTRYRP", "ZONE", "REGION")
  category_b <- c("GENDERRP", "COUNTRYRP", "REGMR", "HHTYPE", "MUNISIZE")
  pairs <- data.frame(category_a, category_b)
  path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs")
  setwd(path)
  test_result <- impact_intersectional(epf, pairs = pairs , shocks_scenario_names = shocks_scenario_names)

  test_expect <- load(path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs/DI_impact.RData"))
  test_expect <- is_d_impacts

  testthat::expect_equal(test_result, test_expect)
})


test_that("Test12_Calculate distributional impacts for all years (basic), no elevated", {
  #y <- seq(2006,2021,1)
  y <- c(2006,2012,2019)

  path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs")
  shocks <- read.csv(file.path(path, "shocks_cdi.csv"), header=T, fileEncoding = "UTF-8-BOM")

  for (year in y){
    path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs")
    setwd(path)
    calc_di(year = year, shocks = shocks, file_name_impact = paste0("D_impact_",year))
    test_result <- load(path <- file.path(rprojroot::find_root(rprojroot::is_testthat), paste0("test_outputs/outputs_di/D_impact_", year,".RData")))
    test_result <- d_impacts

    test_expect <- load(path <- file.path(rprojroot::find_root(rprojroot::is_testthat), paste0("test_inputs/D_impacts/D_impact_", year,".RData")))
    test_expect <- d_impacts

    testthat::expect_equal(test_result, test_expect)
  }
})


test_that("Test13_Calculate distributional impacts for all years (basic), elevated", {
  #y <- seq(2006,2021,1)
  y <- c(2006,2012,2019)

  path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs")
  shocks <- read.csv(file.path(path, "shocks_cdi.csv"), header=T, fileEncoding = "UTF-8-BOM")

  for (year in y){
    path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs")
    setwd(path)
    calc_di(year = year, elevate = T, shocks = shocks, file_name_impact = paste0("D_impact_e_",year))
    test_result <- load(path <- file.path(rprojroot::find_root(rprojroot::is_testthat), paste0("test_outputs/outputs_di/D_impact_e_", year,".RData")))
    test_result <- d_impacts

    test_expect <- load(path <- file.path(rprojroot::find_root(rprojroot::is_testthat), paste0("test_inputs/D_impacts/D_impact_e_", year,".RData")))
    test_expect <- d_impacts

    testthat::expect_equal(test_result, test_expect)
  }
})


test_that("Test14_Calculate distributional impacts for all years (intersectional), no elevated", {
  #y <- seq(2006,2021,1)
  y <- c(2006,2012,2019)

  path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs")
  shocks <- read.csv(file.path(path, "shocks_cdii.csv"), header=T, fileEncoding = "UTF-8-BOM")

  for (year in y){
    path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs")
    setwd(path)
    calc_di(year = year, shocks = shocks, var_impact = NULL, var_intersec = "all", file_name_intersec = paste0("DI_impact_",year), fig = F)
    test_result <- load(path <- file.path(rprojroot::find_root(rprojroot::is_testthat), paste0("test_outputs/outputs_dii/DI_impact_", year,".RData")))
    test_result <- is_d_impacts

    test_expect <- load(path <- file.path(rprojroot::find_root(rprojroot::is_testthat), paste0("test_inputs/DI_impacts/DI_impact_", year,".RData")))
    test_expect <- is_d_impacts

    testthat::expect_equal(test_result, test_expect)
  }
})


test_that("Test15_Calculate distributional impacts for all years (intersectional), no elevated", {
  #y <- seq(2006,2021,1)
  y <- c(2006,2012,2019)

  path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs")
  shocks <- read.csv(file.path(path, "shocks_cdii.csv"), header=T, fileEncoding = "UTF-8-BOM")

  for (year in y){
    path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs")
    setwd(path)
    calc_di(year = year, elevate = T, shocks = shocks, var_impact = NULL, var_intersec = "all", file_name_intersec = paste0("DI_impact_e_",year), fig = F)
    test_result <- load(path <- file.path(rprojroot::find_root(rprojroot::is_testthat), paste0("test_outputs/outputs_dii/DI_impact_e_", year,".RData")))
    test_result <- is_d_impacts

    test_expect <- load(path <- file.path(rprojroot::find_root(rprojroot::is_testthat), paste0("test_inputs/DI_impacts/DI_impact_e_", year,".RData")))
    test_expect <- is_d_impacts

    testthat::expect_equal(test_result, test_expect)
  }
})


test_that("Test16_Calculate energy poverty indices", {
  y <- seq(2006,2021,1)

  test_result <- calc_ep(year = y, index = "all")

  path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs")
  test_expect <- read.csv(file = paste0(path, "/EP.csv"),
                          # fileEncoding = "UTF-8-BOM",
                          header = TRUE,
                          sep = ",",
                          dec = ".") %>%
    dplyr::rename_with(~ sub("^X", "", .))

  testthat::expect_equal(test_result, test_expect)

})


test_that("Test17_Calculate transport poverty indices", {
  y <- seq(2006,2021,1)

  test_result <- calc_tp(year = y, index = "all")

  path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs")
  test_expect <- read.csv(file = paste0(path, "/TP.csv"),
                          fileEncoding = "UTF-8-BOM",
                          header = TRUE,
                          sep = ",",
                          dec = ".") %>%
    dplyr::rename_with(~ sub("^X", "", .))

  testthat::expect_equal(test_result, test_expect)

})
