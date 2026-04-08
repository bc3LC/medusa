# impact

Function to calculate the distributional impacts based in one or more
socioeconomic or demographic variables (one impact per variable).

## Usage

``` r
impact(
  data,
  var = categories$categories,
  save = T,
  file_name = "D_impacts",
  fig = T,
  shocks_scenario_names
)
```

## Arguments

- data:

  a dataset with the input data needed to calculate distributional
  impacts. The dataset should contain both the household expenditures
  collected in the HBS and the expenditures after applying the price
  shock.

- var:

  variable(s) according to which you want to calculate distributional
  impacts. If categories\$categories (by default) calculates the
  distributional impacts for each of the variables specified in the
  package. If not, you can indicate a variable or a vector of variables
  to calculate distributional impacts. If you want to see the variables
  for which the calculation is available run \`available_var_impact()\`.

- save:

  If TRUE (by default) saves a list of the generated datasets (.RData)
  summarising the distributional impacts per selected variable. If FALSE
  do not save.

- file_name:

  name of the file to save the results, if save TRUE. By default
  "D_impacts".

- fig:

  generates and saves a graph that summarises the distributional
  impacts. By default it is TRUE, for the graph/s not to be generated
  and saved indicate FALSE.

- shocks_scenario_names:

  vector of the names of the considered scenario shocks

## Value

a dataframe summarising the distributional impacts per selected
variable.
