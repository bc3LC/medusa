% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{impact_intersectional}
\alias{impact_intersectional}
\title{impact_intersectional}
\usage{
impact_intersectional(
  data,
  pairs = is_categories,
  save = T,
  file_name = "DI_impact",
  fig = T,
  shocks_scenario_names
)
}
\arguments{
\item{data}{a dataset with the input data needed to calculate the intersectional
distributional impacts. The dataset should contain both the household expenditures
collected in the HBS and the expenditures after applying the price shock.}

\item{pairs}{set of variables (2) according to which you want to calculate
distributional impacts. If is_categories (by default) calculates the intersectional
distributional impacts for each of the set of variables specified in the package.
If not, you can indicate the set of variables according to which you want to calculate
the intersectional distributional impacts.If you want to see the set of variables for
which the calculation is available run `available_var_intersec()`. To enter a
set of variables for the calculation, it must follow the same format as the
output of `available_var_intersec()`, i.e. a table whose columns have category_a
and category_b as their titles.}

\item{save}{If TRUE (by default) saves a list of the generated datasets (.RData)
summarising the intersectional distributional impacts per selected set of variable.
If FALSE do not save.}

\item{file_name}{name of the file to save the results, if save TRUE. By default "DI_impacts".}

\item{fig}{generates and saves a graph that summarises the intersectional distributional
impacts. By default it is TRUE, for the graph/s not to be generated and saved indicate FALSE.}

\item{shocks_scenario_names}{vector of the names of the considered scenario shocks}
}
\value{
a list containing the generated datasets (.RData) summarising the intersectional
distributional impacts per selected set of variables.
}
\description{
Function to calculate the distributional impacts based in the intersection
of two socioeconomic or demographic variables (2 variables per impact).
}
