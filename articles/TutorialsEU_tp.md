# Calculate transport poverty indices (EU)

## Step by step tutorials

Before running any of the examples below, make sure you have processed
the Eurostat HBS microdata with [`hbs_eu()`](../reference/hbs_eu.md).
See [Preparing the
data](https://bc3lc.github.io/medusa/articles/TutorialsEU_data.html) for
details.

``` r
hbs <- hbs_eu(year = 2015, country = "all", path = "raw_data")
```

For a description of the transport poverty indices calculated by
`medusa`, see [Transport poverty
indices](https://bc3lc.github.io/medusa/articles/TransportPoverty.html).

------------------------------------------------------------------------

### Example 1. How to calculate a specific transport poverty index

1.  Define the index to calculate (e.g. `"VTU"`) in `calc_tp_eu`:

``` r
file <- calc_tp_eu(data = hbs,      # Output from hbs_eu()
                   index = "VTU")   # Select the index
```

2.  Save the output data frame to Excel:

``` r
library(openxlsx)

write.xlsx(file,
           "TP_VTU_2015.xlsx",
           sheetName = "VTU")
```

The result is a data frame with one row per index and one column per
country. Values represent the **proportion of households** classified as
transport poor in each member state according to the selected index.

------------------------------------------------------------------------

### Example 2. How to calculate all transport poverty indices

1.  Run `calc_tp_eu` with `index = "all"` (the default):

``` r
file <- calc_tp_eu(data = hbs,
                   index = "all")   # All indices (default)
```

2.  Save the output:

``` r
library(openxlsx)

write.xlsx(file,
           "TP_all_2015.xlsx",
           sheetName = "Transport poverty")
```

The returned data frame contains four rows, one per index (`10%`, `2M`,
`LIHC`, `VTU`), and one column per country.

------------------------------------------------------------------------

### Example 3. How to calculate a vector of indices

To calculate a subset of indices, pass a vector to the `index` argument:

``` r
file <- calc_tp_eu(data = hbs,
                   index = c("2M", "LIHC"))
```

------------------------------------------------------------------------

### Example 4. How to calculate indices for a subset of countries

If you processed only a subset of countries in
[`hbs_eu()`](../reference/hbs_eu.md), `calc_tp_eu` will automatically
calculate indices only for those countries:

``` r
hbs_subset <- hbs_eu(year = 2015,
                     country = c("BE", "ES"),
                     path = "raw_data")

file <- calc_tp_eu(data = hbs_subset,
                   index = "all")
```

------------------------------------------------------------------------

### Available indices

| Index  | Description                                                                                                                                                                                 |
|--------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `10%`  | Household spends more than 10% of its total expenditure on transport (private + short/medium distance public).                                                                              |
| `2M`   | Household transport expenditure share is more than twice the national median (among transport users).                                                                                       |
| `LIHC` | Low income (below poverty line after housing and transport costs) AND high transport expenditure (above national median).                                                                   |
| `VTU`  | Vulnerable transport user: high transport expenditure (\>2× national median), low income (\<median), AND low public transport expenditure (\<national median among public transport users). |

Thresholds (national medians and poverty lines) are calculated
separately for each **member state**. Long-distance transport (air:
COICOP CP0733, and sea: CP0734) is excluded from all calculations.
