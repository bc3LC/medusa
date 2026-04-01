# database_hbs

Function to load each country HBS and create a joint database for
household and member files

## Usage

``` r
database_hbs(year, country = "all", inputs_path)
```

## Arguments

- year:

  year of the HBS to process. Available options: 2015, 2020.

- country:

  code of the country or countries of the HBS to process. By default, it
  processes all available data in the working directory To see the
  available countries and codes, run country_code().

- inputs_path:

  Local path to the folder where the HBS's are stored. Not included in
  the package.

## Value

RData file with the joint database with the HBS microdata.
