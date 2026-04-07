# id_ep_eu

Function to identify energy poor households in EU countries. Thresholds
are calculated per member state.

## Usage

``` r
id_ep_eu(data)
```

## Arguments

- data:

  dataset with the EU HBS data (output from hbs_eu, with COICOP columns
  renamed via rename_coicop).

## Value

a dataset with EU HBS data where energy poor households are identified
per country.
