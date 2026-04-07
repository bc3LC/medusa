# id_tp_eu

Function to identify transport poor households in EU countries.
Thresholds are calculated per member state. Long-distance transport (air
and sea) is excluded from the calculation.

## Usage

``` r
id_tp_eu(data)
```

## Arguments

- data:

  dataset with the EU HBS data (output from hbs_eu, with COICOP columns
  renamed via rename_coicop).

## Value

a dataset with EU HBS data where transport poor households are
identified per country.
