# elevate_hbs

Function to elevate the Spanish Household Budget Survey (HBS) to
national accounting.

## Usage

``` r
elevate_hbs(data, year, country = "ES")
```

## Arguments

- data:

  dataset with the data from the HBS.

- year:

  year of the HBS you want to elevate to national accounting.

- country:

  country of the HBS you want to elevate. By default "ES" (for the
  moment it only works for Spain, so DO NOT TOUCH).

## Value

a dataset with the HBS data where expenses are elevated to national
accounting.
