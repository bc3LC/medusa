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
  path2 <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs")
  years <- c(2010, 2015, 2020)

  for (y in years) {

    result <- suppressWarnings(database_hbs(year = y, inputs_path = path))
    saveRDS(result[[paste0("hbs_", y)]], file.path(path2, paste0("hbs_", y, ".rds")))
    test_result <- result[[paste0("hbs_", y)]]

    path3 <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs")
    test_expect <- readRDS(file.path(path3, paste0("hbs_", y, ".rds")))

    testthat::expect_equal(test_result, test_expect)

    # 1. Verify that it returns a list
    testthat::expect_type(result, "list")

    # 2. Verify that it contains the dataframe of households for that year
    expected_hbs_h <- paste0("hbs_h_", y)
    testthat::expect_true(expected_hbs_h %in% names(result),
                          label = paste0(expected_hbs_h, " not found in result"))

    # 3. Verify that the household dataframe is not empty
    testthat::expect_gt(nrow(result[[expected_hbs_h]]), 0,
                        label = paste0(expected_hbs_h, " is empty"))

    # 4. Verify that the joint dataframe (h + m merged) exists for all years
    expected_hbs <- paste0("hbs_", y)
    testthat::expect_true(expected_hbs %in% names(result),
                          label = paste0(expected_hbs, " (merged h+m) not found in result"))
    testthat::expect_gt(nrow(result[[expected_hbs]]), 0,
                        label = paste0(expected_hbs, " is empty"))

    # 5. Verify that it contains the key columns generated by the function
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


test_that("Test8b_Rename COICOP columns EU on full database", {
  path_inputs  <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs")
  path_outputs <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs")
  years <- c(2010, 2015, 2020)

  for (y in years) {

    # Load the RDS created in Test7
    hbs <- readRDS(file.path(path_inputs, paste0("hbs_", y, ".rds")))

    # Apply rename_coicop
    test_result <- rename_coicop(hbs)

    # Load or create the reference RDS
    path2 <- file.path(path_outputs, paste0("hbs_coicop_", y, ".rds"))

    # Subsequent runs: compare against reference
    test_expect <- readRDS(path2)
    testthat::expect_equal(test_result, test_expect,
                           label = paste0("rename_coicop output mismatch for year ", y))
  }
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


test_that("Test10_Update HBS year", {
  path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs")
  hbs   <- read.csv(file.path(path, "ex_dataset_expenses_eu.csv"), header = T, fileEncoding = "UTF-8-BOM")
  shocks <- read.csv(file.path(path, "shocks_ps_eu.csv"), header = T, fileEncoding = "UTF-8-BOM")

  test_result <- price_shock_eu(hbs, shocks)

  test_expect <- read.csv(file.path(path, "ex_dataset_expenses_ps_eu.csv"), header = T, fileEncoding = "UTF-8-BOM")

  testthat::expect_equal(test_result, test_expect)
})


test_that("Test10b_Update year EU with full dataset", {
  path_inputs  <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs")
  path_outputs <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs")
  new_year <- 2022

  # Load input RDS generated in Test8b (rename_coicop output for 2015)
  hbs <- readRDS(file.path(path_outputs, "hbs_coicop_2015.rds"))

  # Apply update_year
  test_result <- update_year(data = hbs, new_year = new_year)

  path2 <- file.path(path_outputs, paste0("hbs_updated_2015_to_", new_year, ".rds"))
  test_expect <- readRDS(path2)
  testthat::expect_equal(test_result, test_expect,
                         label = paste0("update_year output mismatch for 2015 -> ", new_year))

  # 1. Verify output is a dataframe
  testthat::expect_s3_class(test_result, "data.frame")

  # 2. Verify same number of rows as input
  testthat::expect_equal(nrow(test_result), nrow(hbs),
                         label = "update_year changed the number of rows")

  # 3. Verify same columns as input
  testthat::expect_equal(colnames(test_result), colnames(hbs),
                         label = "update_year changed the column names")

  # 4. Verify that CP columns have been updated (values should differ from input)
  coicop_cols <- grep("^CP\\d+", names(hbs), value = TRUE)
  testthat::expect_gt(length(coicop_cols), 0,
                      label = "No CP columns found in input data")

  cols_updated <- sapply(coicop_cols, function(col) {
    !isTRUE(all.equal(test_result[[col]], hbs[[col]]))
  })
  testthat::expect_true(any(cols_updated),
                        label = "No CP columns were updated by update_year")

  # 5. Verify that non-COICOP columns remain unchanged
  other_cols <- setdiff(names(hbs), coicop_cols)
  for (col in other_cols) {
    testthat::expect_equal(test_result[[col]], hbs[[col]],
                           label = paste0("Non-COICOP column '", col, "' was modified"))
  }
})


test_that("Test11_Price shock EU", {
  path_inputs <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs")
  path_outputs <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs")
  hbs   <- readRDS(file.path(path_outputs, "hbs_coicop_2015.rds"))
  shocks <- read.csv(file.path(path_inputs, "shocks_ps_eu2.csv"), header = T, fileEncoding = "UTF-8-BOM")

  test_result <- price_shock_eu(hbs, shocks)

  # --- Reference RDS check ---
  path2 <- file.path(path_outputs, "hbs_price_shock_2015.rds")

  if (!file.exists(path2)) {
    saveRDS(test_result, path2)
    message("Reference RDS created: ", path2)
  } else {
    test_expect <- readRDS(path2)
    testthat::expect_equal(test_result, test_expect,
                           label = "price_shock_eu output mismatch")
  }

  # --- Structural checks ---

  # 1. Verify output is a dataframe
  testthat::expect_s3_class(test_result, "data.frame")

  # 2. Verify same number of rows as input
  testthat::expect_equal(nrow(test_result), nrow(hbs),
                         label = "price_shock_eu changed the number of rows")

  # 3. Verify that scenario columns have been created
  scenario_cols <- setdiff(colnames(shocks), "coicop")
  scenarios <- unique(gsub(".*_", "", scenario_cols))
  for (s in scenarios) {
    new_cols <- grep(paste0("_", s, "$"), colnames(test_result), value = TRUE)
    testthat::expect_gt(length(new_cols), 0,
                        label = paste0("No columns created for scenario ", s))
  }

  # 4. Verify that CP00 scenario columns have been created
  for (s in scenarios) {
    cp00_col <- paste0("CP00_", s)
    testthat::expect_true(cp00_col %in% colnames(test_result),
                          label = paste0(cp00_col, " not found in result"))
  }

  # --- Shock value checks ---
  idx_BE <- which(test_result$country == "BE")
  idx_ES <- which(test_result$country == "ES")

  # 5. Verify shock columns have been created
  expected_new_cols <- c("CP0111_Scenario1", "CP0112_Scenario2", "CP0113_Scenario1", "CP0114_Scenario2")
  for (col in expected_new_cols) {
    testthat::expect_true(col %in% colnames(test_result),
                          label = paste0("Column '", col, "' was not created"))
  }

  # 6. Verify shocks applied correctly per country and scenario
  # BE Scenario1: CP0111 * 2
  testthat::expect_equal(test_result$CP0111_Scenario1[idx_BE],
                         as.numeric(hbs$CP0111[idx_BE]) * 2,
                         label = "BE CP0111_Scenario1: shock x2 not applied correctly")

  # BE Scenario2: CP0112 * 3
  testthat::expect_equal(test_result$CP0112_Scenario2[idx_BE],
                         as.numeric(hbs$CP0112[idx_BE]) * 3,
                         label = "BE CP0112_Scenario2: shock x3 not applied correctly")

  # ES Scenario1: CP0113 * 4
  testthat::expect_equal(test_result$CP0113_Scenario1[idx_ES],
                         as.numeric(hbs$CP0113[idx_ES]) * 4,
                         label = "ES CP0113_Scenario1: shock x4 not applied correctly")

  # ES Scenario2: CP0114 * 2
  testthat::expect_equal(test_result$CP0114_Scenario2[idx_ES],
                         as.numeric(hbs$CP0114[idx_ES]) * 2,
                         label = "ES CP0114_Scenario2: shock x2 not applied correctly")

  # 7. Verify shock = 1 leaves values unchanged (ES CP0111_Scenario1 should equal original)
  if ("CP0111_Scenario1" %in% colnames(test_result)) {
    testthat::expect_equal(test_result$CP0111_Scenario1[idx_ES],
                           as.numeric(hbs$CP0111[idx_ES]),
                           label = "ES CP0111_Scenario1: should be unchanged (shock = 1)")
  }

  # 8. Verify CP00 per scenario has changed for countries with active shocks
  testthat::expect_false(isTRUE(all.equal(test_result$CP00_Scenario1[idx_BE],
                                          as.numeric(hbs$CP00[idx_BE]))),
                         label = "CP00_Scenario1 for BE was not updated")
  testthat::expect_false(isTRUE(all.equal(test_result$CP00_Scenario2[idx_ES],
                                          as.numeric(hbs$CP00[idx_ES]))),
                         label = "CP00_Scenario2 for ES was not updated")
})


test_that("Test12_Impact EU & basic graph", {
  path_inputs  <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_inputs")
  path_outputs <- file.path(rprojroot::find_root(rprojroot::is_testthat), "test_outputs")

  # Load input from Test11 (price_shock output)
  hbs <- readRDS(file.path(path_outputs, "hbs_price_shock_2015.rds"))

  # Define scenario names from the shock columns
  shocks_scenario_names <- unique(gsub(".*_", "", grep("^CP00_", colnames(hbs), value = TRUE)))

  # Run impact_eu
  setwd(path_outputs)
  test_result <- impact_eu(data = hbs,
                           shocks_scenario_names = shocks_scenario_names,
                           save = TRUE,
                           fig  = TRUE)

  # --- Reference RDS check ---
  path2 <- file.path(path_inputs, "D_impacts_eu.rds")

  test_expect <- readRDS(path2)
  testthat::expect_equal(test_result, test_expect,
                         label = "impact_eu output mismatch")

  # --- Structural checks ---

  # 1. Verify output is a dataframe
  testthat::expect_s3_class(test_result, "data.frame")

  # 2. Verify expected columns exist
  expected_cols <- c("ID", "LABELS", "VARIABLE", "WEIGHT", "SOURCE")
  for (col in expected_cols) {
    testthat::expect_true(col %in% colnames(test_result),
                          label = paste0("Column '", col, "' missing in impact_eu output"))
  }

  # 3. Verify DI columns have been created for each scenario
  for (s in shocks_scenario_names) {
    di_col <- paste0("DI_", s)
    testthat::expect_true(di_col %in% colnames(test_result),
                          label = paste0("DI column '", di_col, "' not found in result"))
  }

  # 4. Verify output is not empty
  testthat::expect_gt(nrow(test_result), 0,
                      label = "impact_eu returned an empty dataframe")

  # 5. Verify SOURCE contains EU and country codes
  sources <- unique(test_result$SOURCE)
  testthat::expect_true("EU" %in% sources,
                        label = "EU-level results not found in SOURCE column")
  countries <- unique(hbs$country)
  for (c in countries) {
    testthat::expect_true(c %in% sources,
                          label = paste0("Country '", c, "' not found in SOURCE column"))
  }

  # --- Figure checks ---
  vars <- intersect(unique(test_result$VARIABLE),
                    list.files(file.path(path_outputs, "figures"),
                               pattern = "^DI_.*\\.png$") |>
                      gsub(pattern = "^DI_|\\.png$", replacement = ""))

  for (g in vars) {
    fig_output <- file.path(path_outputs, "figures", paste0("DI_", g, ".png"))
    fig_input  <- file.path(path_inputs,  "figures", paste0("DI_", g, ".png"))

    # Verify figure was generated
    testthat::expect_true(file.exists(fig_output),
                          label = paste0("Figure DI_", g, ".png was not generated"))

    # Compare against reference if it exists
    if (file.exists(fig_input)) {
      img_result <- png::readPNG(fig_output)
      img_expect <- png::readPNG(fig_input)
      testthat::expect_true(identical(attributes(img_result), attributes(img_expect)),
                            label = paste0("Figure DI_", g, ".png dimensions do not match"))
    } else {
      # First run: copy figures to test_inputs as reference
      if (!dir.exists(file.path(path_inputs, "figures"))) dir.create(file.path(path_inputs, "figures"))
      file.copy(fig_output, fig_input)
      message("Reference figure created: ", fig_input)
    }
  }
})
