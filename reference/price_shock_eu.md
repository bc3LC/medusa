# price_shock_eu

Function to apply a specific price shock to the different COICOP
categories of the Household Budget Survey (HBS).

## Usage

``` r
price_shock_eu(data, shocks)
```

## Arguments

- data:

  input data from the HBS to apply the price shocks.

- shocks:

  a dataset with the price shocks per coicop to be applied. The format
  of the dataset has to correspond to the predefined one in the package.
  To save a csv file with the right format to enter the price shocks run
  \`ex_shocks_eu()\`. A price shock greater than 1 indicates a price
  increase (e.g. 1.1 indicates a 10% increase) and less than 1 indicates
  a price decrease (e.g. 0.9 indicates a 10% decrease).

## Value

a dataset with the HBS data and the new expenses for COICOP categories
after the application of the price shock.
