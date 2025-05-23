% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{load_rawhbs}
\alias{load_rawhbs}
\title{load_rawhbs}
\usage{
load_rawhbs(year, path, path_outputs)
}
\arguments{
\item{year}{year of the HBS you want to load.}

\item{path}{Local path to the folder where the HBS's are stored. Not included in the package.}

\item{path_outputs}{path to save the results (RData)}
}
\value{
a list with the 3 files of the HBS.
}
\description{
Function to load the Spanish Household Budget Survey (HBS).
}
