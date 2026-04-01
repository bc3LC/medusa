# rawhbs_eu

Function to process raw data from the Household Budget Survey (HBS) for
each year and country.

## Usage

``` r
rawhbs_eu(year, country = "all", path)
```

## Arguments

- year:

  year/s of the HBS to process. Available options: 2010, 2015, 2020.

- country:

  code of the country or countries of the HBS to process. By default, it
  processes all available data in the working directory To see the
  available countries and codes, run country_code().

- path:

  Local path to the folder where the HBS's are stored. Not included in
  the package.

## Value

RData file with the HBS microdata for each selected country and year.
