# calc_ep_eu

Function to calculate energy poverty indices for EU countries.
Thresholds are calculated per member state.

## Usage

``` r
calc_ep_eu(data, index = "all")
```

## Arguments

- data:

  dataset with the EU HBS data (output from hbs_eu, with COICOP columns
  renamed via rename_coicop). Must contain columns: CP045, CP00, CP0411,
  CP0421, eq_size, weight, country.

- index:

  energy poverty index or indices to be calculated. Possible options:
  "10 all the indices for each country.

## Value

a dataframe with the selected energy poverty indices per country.
