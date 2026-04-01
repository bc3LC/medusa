# rename_values_eu

Function to rename the codified values of the dataset to the meaningful
values detailed in the mapping included in the package.

## Usage

``` r
rename_values_eu(data, map = variables_h, current_var)
```

## Arguments

- data:

  dataset to be standardized.

- map:

  mapping file to be used (included in medusa)

- current_var:

  column name to be standardized.

## Value

a dataset with labels renamed based in the mapping included in the
package.
