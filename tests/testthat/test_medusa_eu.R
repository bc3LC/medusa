library(medusa)
library(testthat)
library(magrittr)

test_that("Test1_Country codes", {
  test_result <- country_code()
  test_expect <- data.frame(
    code = c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES",
             "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT",
             "NL", "PL", "PT", "RO", "SE", "SI", "SK"),
    country = c("Austria", "Belgium", "Bulgaria", "Cyprus", "Czech Republic",
                "Germany", "Denmark", "Estonia", "Greece", "Spain",
                "Finland", "France", "Croatia", "Hungary", "Ireland",
                "Italy", "Lithuania", "Luxembourg", "Latvia", "Malta",
                "Netherlands", "Poland", "Portugal", "Romania", "Sweden",
                "Slovenia", "Slovakia")
  )
  testthat::expect_equal(test_result, test_expect)
})


test_that("Test2_Example shocks EU", {
  setwd(file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs"))
  ex_shocks_eu()
  path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs")
  test_result <- read.csv(file = paste0(path, "/Example_shocks_eu.csv"),
                          fileEncoding = "UTF-8-BOM",
                          header = TRUE,
                          sep = ",",
                          dec = ".")
  path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs")
  test_expect <- read.csv(file = paste0(path, "/Example_shocks_eu.csv"),
                          fileEncoding = "UTF-8-BOM",
                          header = TRUE,
                          sep = ",",
                          dec = ".")
  testthat::expect_equal(test_result, test_expect)
})

test_that("Test3_Available basic variables EU", {
  test_result <- available_var_eu()
  test_expect <- c("quintile", "quintile_eu", "decile", "decile_eu", "ventile", "ventile_eu",
                   "percentile", "percentile_eu", "country", "zone", "household_type", "income_source",
                   "POVERTY", "gender", "feminization_degree", "age", "birth_country", "education",
                   "activity", "contract_type", "workday", "employment_sector", "REGMR")

  testthat::expect_equal(test_result, test_expect)
})


test_that("Test4_Available intersectional variables EU", {
  test_result <- available_var_intersec_eu()
  path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs")
  test_expect <- read.csv(file = paste0(path, "/is_categories_eu.csv"),
                          fileEncoding = "UTF-8-BOM",
                          header = TRUE,
                          sep = ",",
                          dec = ".")
  testthat::expect_equal(test_result, test_expect)
})


test_that("Test5_Standardize & rename values EU", {
  path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs")
  test_expect <- read.csv(file.path(path, "ex_dataset_renamed_eu.csv"), header=T, fileEncoding = "UTF-8-BOM")
  test_expect$number <- as.character(test_expect$number)
  test_expect$year <- as.character(test_expect$year)
  test_expect$weight <- as.character(test_expect$weight)
  test_expect$household_size <- as.character(test_expect$household_size)

  epf_hh <- read.csv(file.path(path, "ex_dataset_eu.csv"), header=T, fileEncoding = "UTF-8-BOM")
  test_result <- standardize_eu(epf_hh)

  testthat::expect_equal(test_result, test_expect)
})


test_that("Test6_Raw data processing", {
  paths <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs/raw_data")
  years <- c(2010, 2015, 2020)

  rawhbs_eu(year = years, path = paths)

  for (y in years) {

    # Dynamically read the countries available for each year according to the xlsx files
    year_path <- file.path(paths, as.character(y))
    xlsx_files <- list.files(year_path, pattern = "\\.xlsx$", full.names = FALSE)

    # Extract country codes according to each year's format (same as in rawhbs_eu)
    if (y == 2020) {
      test_countries <- unique(substr(xlsx_files, nchar(xlsx_files) - 6, nchar(xlsx_files) - 5))
    } else {
      test_countries <- unique(substr(xlsx_files, 1, 2))
    }

    testthat::expect_gt(length(test_countries), 0,
                        label = paste0("No countries found in raw_data/", y))

    for (cc in test_countries) {

      # 1. Verificar que el archivo RData se ha creado
      expected_file <- file.path(paths, "inputs", cc, paste0("hbs_", y, "_", cc, ".RData"))
      testthat::expect_true(file.exists(expected_file),
                            label = paste0("File hbs_", y, "_", cc, ".RData was not created"))

      # 2. Verify that the RData file has been created
      env <- new.env()
      load(expected_file, envir = env)
      testthat::expect_true("hbs_h" %in% ls(env),
                            label = paste0("hbs_h missing in hbs_", y, "_", cc, ".RData"))
      testthat::expect_true("hbs_m" %in% ls(env),
                            label = paste0("hbs_m missing in hbs_", y, "_", cc, ".RData"))

      # 3. Verify that the dataframes are not empty
      testthat::expect_gt(nrow(env$hbs_h), 0,
                          label = paste0("hbs_h is empty for ", cc, " ", y))
      testthat::expect_gt(nrow(env$hbs_m), 0,
                          label = paste0("hbs_m is empty for ", cc, " ", y))
    }
  }
})


test_that("Test7_Create joint database", {
  path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs/raw_data/inputs")
  years <- c(2010, 2015, 2020)

  for (y in years) {

    result <- suppressWarnings(database_hbs(year = y, inputs_path = path))

    # 1. Verificar que devuelve una lista
    testthat::expect_type(result, "list")

    # 2. Verificar que contiene el dataframe de hogares para ese año
    expected_hbs_h <- paste0("hbs_h_", y)
    testthat::expect_true(expected_hbs_h %in% names(result),
                          label = paste0(expected_hbs_h, " not found in result"))

    # 3. Verificar que el dataframe de hogares no está vacío
    testthat::expect_gt(nrow(result[[expected_hbs_h]]), 0,
                        label = paste0(expected_hbs_h, " is empty"))

    # 4. Para 2015 y 2020, verificar que existe el dataframe conjunto (h + m merged)
    if (y %in% c(2015, 2020)) {
      expected_hbs <- paste0("hbs_", y)
      testthat::expect_true(expected_hbs %in% names(result),
                            label = paste0(expected_hbs, " (merged h+m) not found in result"))
      testthat::expect_gt(nrow(result[[expected_hbs]]), 0,
                          label = paste0(expected_hbs, " is empty"))
    }

    # 5. Verificar que contiene las columnas clave generadas por la función
    hbs_h <- result[[paste0("hbs_h_", y)]]
    expected_cols <- c("decile", "quintile", "ventile", "percentile",
                       "decile_eu", "quintile_eu", "ventile_eu", "percentile_eu",
                       "feminization_degree", "country2")
    for (col in expected_cols) {
      testthat::expect_true(col %in% colnames(hbs_h),
                            label = paste0("Column '", col, "' missing in hbs_h_", y))
    }
  }
})


test_that("Test8_Rename COICOP columns EU", {
  path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs")
  hbs <- read.csv(file.path(path, "ex_coicop_eu.csv"), header = T, fileEncoding = "UTF-8-BOM")
  test_result <- rename_coicop(hbs)

  path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs")
  test_expect <- read.csv(file.path(path, "ex_coicop_eu.csv"), header = T, fileEncoding = "UTF-8-BOM")

  # Check that EUR_HE columns have been renamed to CP columns
  testthat::expect_equal(test_result, test_expect)
})


test_that("Test9_Map COICOP columns EU", {
  path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs")
  hbs <- read.csv(file.path(path, "ex_coicop2_eu.csv"), header = T, fileEncoding = "UTF-8-BOM")
  test_result <- suppressWarnings(coicop_mapping(hbs))

  path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs")
  test_expect <- read.csv(file.path(path, "ex_coicop2_eu.csv"), header = T, fileEncoding = "UTF-8-BOM")

  # Check that EUR_HE columns have been renamed to CP columns
  testthat::expect_equal(test_result, test_expect)
})


test_that("Test10_Price shock EU", {
  path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs")
  hbs   <- read.csv(file.path(path, "ex_dataset_expenses_eu.csv"), header = T, fileEncoding = "UTF-8-BOM")
  shocks <- read.csv(file.path(path, "shocks_ps_eu.csv"), header = T, fileEncoding = "UTF-8-BOM")

  test_result <- price_shock_eu(hbs, shocks)

  test_expect <- read.csv(file.path(path, "ex_dataset_expenses_ps_eu.csv"), header = T, fileEncoding = "UTF-8-BOM")

  testthat::expect_equal(test_result, test_expect)
})


