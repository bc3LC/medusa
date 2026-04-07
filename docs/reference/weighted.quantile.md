# weighted.quantile

Function to calculate weighted quantile. Extracted from package
spatstat.geom v3.2-5

## Usage

``` r
weighted.quantile(
  x,
  w,
  probs = seq(0, 1, 0.25),
  na.rm = TRUE,
  type = 4,
  collapse = TRUE
)
```

## Arguments

- x:

  Data values. A vector of numeric values, for which the median or
  quantiles are required.

- w:

  Weights. A vector of non negative numbers, of the same lenght as x.

- probs:

  Probabilities for which the quantiles should be computed. A numeric
  vector of values between 0 and 1.

- na.rm:

  Logical. Whether to ignore NA values.

- type:

  Integer specifying the rule for calculating the median or quantile,
  corresponding to the rules available for quantile. The only valid
  choices are type=1, 2 or 4.

- collapse:

  Research use only.
