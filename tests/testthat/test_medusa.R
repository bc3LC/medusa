library(medusa)
library(testthat)
library(magrittr)

test_that("Test1_Available basic variables", {
  test_result <- available_var_impact()
  test_expect <- c("QUINTIL", "DECIL", "VENTIL", "PERCENTIL","CCAA",
                   "TAMAMU",   "ZONA", "TIPHOGAR", "HIJOS", "POBREZA",
                   "GENEROPR", "GRADOFEM", "EDADPR", "PAISPR", "ESTUDIOSPR",
                   "SPROFESIONALPR", "TIPOCONTPR", "JORNADAPR", "REGIMENVP")
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
