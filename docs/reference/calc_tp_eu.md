# calc_tp_eu

Function to calculate transport poverty indices for EU countries.
Thresholds are calculated per member state. Long-distance transport (air
and sea) is excluded from the calculation.

## Usage

``` r
calc_tp_eu(data, index = "all")
```

## Arguments

- data:

  dataset with the EU HBS data (output from hbs_eu, with COICOP columns
  renamed via rename_coicop). Must contain columns: CP072, CP073,
  CP0731, CP0732, CP0733, CP0734, CP0735, CP0411, CP0421, CP00, eq_size,
  weight, country.

- index:

  transport poverty index or indices to be calculated. Possible options:
  "10 indices for each country.

## Value

a dataframe with the selected transport poverty indices per country.
