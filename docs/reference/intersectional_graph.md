# intersectional_graph

Function to create an intersectional graph to summarize the
distributional impact based in the intersection of two socioeconomic or
demographic variables (2 variables per plot).

## Usage

``` r
intersectional_graph(data, pairs = is_categories)
```

## Arguments

- data:

  a dataset with the input data needed to generate the intersectional
  graph.

- pairs:

  set of variables (2) according to which you want to create the
  intersectional graph. If is_categories (by default), it generates the
  intersectional graph for each of the combinations of variables
  specified in the package. If not, you can indicate the set of
  variables according to which you want to generste the intersectional
  graph. If you wish to see the set of variables for which the
  calculation is available, run \`available_var_intersec()\`. To enter a
  set of variables for the calculation, it must follow the same format
  as the output of \`available_var_intersec()\`, i.e. a table whose
  columns have category_a and category_b as their titles.

## Value

a graph per selected set of variables summarizing the distributional
impacts.
