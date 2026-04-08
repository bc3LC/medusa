# get_prices_gcameurope

Extract and format prices from a GCAM-Europe database or project file
for MEDUSA.

## Usage

``` r
get_prices_gcameurope(
  db_path = NULL,
  query_path = "inst/extdata",
  db_name = NULL,
  prj_name,
  scenarios,
  queries = "queries_GCAM_MEDUSA.xml",
  final_db_year = 2100,
  saveOutput = T,
  base_scen,
  selected_year
)
```

## Arguments

- db_path:

  Path to the GCAM-Europe database.

- query_path:

  Path to the query file.

- db_name:

  Name of the GCAM-Europe database.

- prj_name:

  Name of the rgcam project. This can be an existing project, or, if
  not, this will be the newly created project name.

- scenarios:

  Vector names of the GCAM-Europe scenarios to be processed.

- queries:

  Name of the GCAM-Europe query file. The file by default includes the
  queries required to run rfasst.

- final_db_year:

  Final year in the GCAM-Europe database (this allows to process
  databases with user-defined "stop periods").

- saveOutput:

  Writes the files.By default=T.

- base_scen:

  The base scenario that other scenarios will be compared against.

- selected_year:

  The year of analysis, when the scenarios will be compared.

## Value

a dataframe with prices by sector and GCAM-Europe region.
