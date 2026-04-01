# hbs_eu

Main function to generate the database (from EUROSTAT microdata) to
calculate the distributional impacts with calc_di_eu function.

## Usage

``` r
hbs_eu(year, country = "all", path)
```

## Arguments

- year:

  year of the HBS to process. Available options: 2010, 2015, 2020.

- country:

  code of the country or countries of the HBS to process and calculate
  distributional impacts. By default, it processes all available data in
  the working directory To see the available countries and codes, run
  country_code().

- path:

  Local path to the folder where the HBS's are stored (microdata from
  EUROSTAT, not included in the package). In the defined folder, the
  data provided by EUROSTAT must be saved in a folder with the name of
  the year to which they belong.

## Value

a list containing the generated datasets summarising the basic or/and
the intersectional distributional impacts per selected variable or set
of variables.
