# weighted.median

Function to calculate weighted median. Extracted from package
spatstat.geom v3.2-5

## Usage

``` r
weighted.median(x, w, na.rm = TRUE, type = 2, collapse = TRUE)
```

## Arguments

- x:

  Data values. A vector pf numeric values, for which the median or
  quantiles are required.

- w:

  Weights. A vector of nonnegative numbers, of the same lenght as x.

- na.rm:

  Logical. Whether to ignore NA values.

- type:

  Integer specifying the rule for calculating the median or quantile,
  corresponding to the rules available for quantile. The only valid
  choices are type=1, 2 or 4.

- collapse:

  Research use only.
