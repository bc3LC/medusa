# add_coicop

Function to add COICOP categories in the Spanish Household Budget Survey
(HBS) according to the aggregation (coicop_year) specified in the
package.

## Usage

``` r
add_coicop(data, year)
```

## Arguments

- data:

  dataset with the data from the HBS.

- year:

  year of the HBS to be modified according to the aggregation specified
  in the package.

## Value

a dataset with HBS data where COICOP categories are aggregated according
to the classification specified in the package.
