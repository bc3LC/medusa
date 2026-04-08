# basic_graph

Function to create a basic graph to summarize the distributional impact
based in one or more socioeconomic or demographic variable (one plot per
variable).

## Usage

``` r
basic_graph(data, var = categories$categories)
```

## Arguments

- data:

  a dataset with the input data needed to generate a basic graph.

- var:

  variable(s) according to which you want to generate the graph. If
  categories\$categories (by default) creates a graph with the
  distributional impacts for each of the variables specified in the
  package. If not, you can indicate a variable or a vector of variables
  to crate the graph.If you want to see the variables for which the
  function is available run \`available_var_impact()\`.

## Value

a graph per selected variable/s summarizing distributional impacts.
