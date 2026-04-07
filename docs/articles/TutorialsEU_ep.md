# Calculate energy poverty indices (EU)

## Step by step tutorials

Before running any of the examples below, make sure you have processed
the Eurostat HBS microdata with [`hbs_eu()`](../reference/hbs_eu.md).
See [Preparing the
data](https://bc3lc.github.io/medusa/articles/TutorialsEU_data.html) for
details.

``` r
hbs <- hbs_eu(year = 2015, country = "all", path = "raw_data")
```

For a description of the energy poverty indices calculated by `medusa`,
see [Energy poverty
indices](https://bc3lc.github.io/medusa/articles/EnergyPoverty.html).

------------------------------------------------------------------------

### Example 1. How to calculate a specific energy poverty index

1.  Define the index to calculate (e.g. `"LIHC"`) in `calc_ep_eu`:

``` r
file <- calc_ep_eu(data = hbs,       # Output from hbs_eu()
                   index = "LIHC")   # Select the index
```

2.  Save the output data frame to Excel:

``` r
library(openxlsx)

write.xlsx(file,
           "EP_LIHC_2015.xlsx",
           sheetName = "LIHC")
```

The result is a data frame with one row per index and one column per
country. Values represent the **proportion of households** classified as
energy poor in each member state according to the selected index.

------------------------------------------------------------------------

### Example 2. How to calculate all energy poverty indices

1.  Run `calc_ep_eu` with `index = "all"` (the default):

``` r
file <- calc_ep_eu(data = hbs,
                   index = "all")   # All indices (default)
```

2.  Save the output:

``` r
library(openxlsx)

write.xlsx(file,
           "EP_all_2015.xlsx",
           sheetName = "Energy poverty")
```

The returned data frame contains five rows, one per index (`10%`, `2M`,
`LIHC`, `HEP`, `HEP_LI`), and one column per country.

------------------------------------------------------------------------

### Example 3. How to calculate indices for a subset of countries

If you processed only a subset of countries in
[`hbs_eu()`](../reference/hbs_eu.md), `calc_ep_eu` will automatically
calculate indices only for those countries:

``` r
hbs_subset <- hbs_eu(year = 2015,
                     country = c("BE", "ES"),
                     path = "raw_data")

file <- calc_ep_eu(data = hbs_subset,
                   index = "all")
```

------------------------------------------------------------------------

### Available indices

| Index | Description |
|----|----|
| `10%` | Household spends more than 10% of its total expenditure on domestic energy. |
| `2M` | Household energy expenditure share is more than twice the national median. |
| `LIHC` | Low income (below poverty line after energy costs) AND high energy expenditure (above national median). |
| `HEP` | Hidden energy poverty: energy expenditure is less than half the national median. |
| `HEP_LI` | Hidden energy poverty AND low income (below poverty line after energy costs). |

Thresholds (national medians and poverty lines) are calculated
separately for each **member state**.
