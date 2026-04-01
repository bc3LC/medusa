# Calculate distributional impacts (EU)

## Step by step tutorials

Before running any of the examples below, make sure you have processed
the Eurostat HBS microdata with [`hbs_eu()`](../reference/hbs_eu.md).
See [Preparing the
data](https://bc3lc.github.io/medusa/articles/TutorialsEU_data.html) for
details.

``` r
hbs <- hbs_eu(year = 2015, country = "all", path = "raw_data")
```

### Example 1. How to enter price shocks in `calc_di_eu`

1.  Download the example file into your working directory by running the
    following function in the R terminal:

``` r
ex_shocks_eu()
```

2.  Go to your working directory and open the csv
    **“Example_shocks_eu.csv”**.

3.  Define the scenarios: each column named after a country code and a
    scenario suffix (e.g. `AT_s1`, `BE_s1`, …, `AT_s2`, `BE_s2`, …)
    represents a scenario for each country. To run `calc_di_eu` for a
    single scenario, delete all columns for Scenario 2. To add another
    scenario, copy the Scenario 2 columns to the right and change the
    suffix to `_s3`. You can rename scenarios by changing the suffix
    (e.g. `_shock1`), but keep the country code prefix intact.

4.  Enter the price shocks: each row corresponds to a COICOP code. Enter
    the price change to be applied to each COICOP category for each
    country and each scenario. A value greater than 1 indicates a price
    increase (e.g. `1.1` for a 10% increase) and less than 1 indicates a
    price decrease (e.g. `0.9` for a 10% decrease). If there is no shock
    in a category, keep `1`.

5.  Save the edited csv file.

6.  Upload the edited file to R:

``` r
shocks <- read.csv("Example_shocks_eu.csv",
                   header = TRUE,
                   sep = ",",
                   dec = ".")
```

7.  Run `calc_di_eu` with the processed HBS data and the price shocks:

``` r
results <- calc_di_eu(data = hbs,           # Output from hbs_eu()
                      shocks = shocks)       # Edited shocks file
```

By default, `calc_di_eu` calculates distributional impacts for all
available socioeconomic variables and saves the results and figures in
an `outputs/` folder within your working directory.

------------------------------------------------------------------------

### Example 2. How to select variables for distributional impacts

#### Individual variables (`var`)

1.  To see the variables available for distributional impact analysis,
    run:

``` r
available_var_eu()
```

2.  Select a variable (e.g. `"decile"`) and pass it to the `var`
    argument of `calc_di_eu`:

``` r
results <- calc_di_eu(data = hbs,
                      shocks = shocks,
                      var = "decile")     # Single variable
```

3.  To calculate impacts for several variables, pass a vector:

``` r
vars <- c("decile", "zone", "gender")

results <- calc_di_eu(data = hbs,
                      shocks = shocks,
                      var = vars)
```

For more information on available variables, see [Available
Variables](https://bc3lc.github.io/medusa/articles/AvailableVariables.html).

#### Intersectional variables (`var_intersec`)

1.  To see the combinations of variables available for intersectional
    distributional impacts, run:

``` r
available_var_intersec_eu()
```

2.  Download the intersectional variables file into your working
    directory:

``` r
ex_var_intersec_eu()
```

3.  Open **“Var_Intersec_eu.csv”** and delete the rows for combinations
    you do not want to analyse. Save the edited file.

4.  Upload the file to R:

``` r
example_vars <- read.csv("Var_Intersec_eu.csv",
                         header = TRUE,
                         sep = ",",
                         dec = ".")
```

5.  Pass the file to the `var_intersec` argument of `calc_di_eu`:

``` r
results <- calc_di_eu(data = hbs,
                      shocks = shocks,
                      var_impact = NULL,           # Skip individual variables
                      var_intersec = example_vars) # Intersectional combinations
```

------------------------------------------------------------------------

### Example 3. Country-level vs. EU-level results

By default, `calc_di_eu` calculates distributional impacts both at the
**EU level** (across all households jointly) and at the **country
level** (separately for each member state). To disable country-level
results and obtain only EU-level results, set `by_country = FALSE`:

``` r
results <- calc_di_eu(data = hbs,
                      shocks = shocks,
                      by_country = FALSE)
```

------------------------------------------------------------------------

### Example 4. Update the microdata to a different year

If your price shocks come from a macroeconomic model calibrated to a
year different from the HBS wave, you can update the HBS expenditure
data before running the simulation using the `update_hbs` argument:

``` r
results <- calc_di_eu(data = hbs,           # HBS 2015 microdata
                      update_hbs = 2018,    # Update expenditure to 2018 prices
                      shocks = shocks)
```

------------------------------------------------------------------------

### Outputs

`calc_di_eu` returns a list with two elements:

- **`di`**: a data frame with the basic distributional impacts per
  selected variable and scenario.
- **`dii`** (if `var_intersec` is specified): a data frame with the
  intersectional distributional impacts.

By default, results are saved to the `outputs/` folder and figures are
generated automatically. To suppress saving, set `save = FALSE` and
`fig = FALSE`.
