# MEDUSA - Modelling Equity and DistribUtional impacts for Socioeconomic Analysis

[![docs](https://github.com/bc3LC/medusa/actions/workflows/docs.yaml/badge.svg)](https://github.com/bc3LC/medusa/actions/workflows/docs.yaml)
[![pages-build-deployment](https://github.com/bc3LC/medusa/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/bc3LC/medusa/actions/workflows/pages/pages-build-deployment)
[![test_coverage](https://github.com/bc3LC/medusa/actions/workflows/test_coverage.yml/badge.svg)](https://github.com/bc3LC/medusa/actions/workflows/test_coverage.yml)
[![codecov](https://codecov.io/gh/bc3LC/medusa/graph/badge.svg?token=VSmmxRUGO2)](https://codecov.io/gh/bc3LC/medusa)
[![build](https://github.com/bc3LC/medusa/actions/workflows/build.yaml/badge.svg)](https://github.com/bc3LC/medusa/actions/workflows/build.yaml)
[![draft-pdf](https://github.com/bc3LC/medusa/actions/workflows/draft-pdf.yml/badge.svg)](https://github.com/bc3LC/medusa/actions/workflows/draft-pdf.yml)

## <a name="contents"></a>Contents

<!-- ------------------------>

<!-- ------------------------>

-   [Introduction](#introduction)

	-   [The package](#pkg)

    -   [The microsimulation model](#ms-model)

-   [Installation Guide](#installation-guide)

-   [Getting Started](#get-started)

<!-- ------------------------>

<!-- ------------------------>

## <a name="introduction"></a>Introduction

<!-- ------------------------>

<!-- ------------------------>

[Back to Contents](#contents)

### <a name="pkg"></a>The package

MEDUSA is an R package that allows the development of distributional analyses in isolation or in connection with other models (soft links). The extensive database in which the microsimulation model is based allows for highly disaggregated results, taking into account numerous socioeconomic and demographic characteristics of households, such as income level, place of residence, type of family or the  feminization degree of the household. Additionally, the package combines these with the calculation of energy and transport poverty indices.

:exclamation::exclamation: **Note**: At the moment, `medusa` package works for Spain, but in the short term the idea is to extend it to all EU countries. Moreover, the package could be extended to all countries that are able to provide the raw data of the model.

### <a name="ms-model"></a>The microsimulation model

MEDUSA allows to develop distributional analyses based in an overnight effect microsimulation model. The model is built up with the microdata from the Household Budget Survey. For more details on the model in which `medusa` is based click [here](https://bc3lc.github.io/medusa/articles/TheModel.html).


## <a name="installation-guide"></a>Installation Guide

<!-- ------------------------>

<!-- ------------------------>

[Back to Contents](#contents)

To use `medusa` package first you need to follow this steps:

1.  Install R and Rstudio (only if not already installed)

    -   To download R click [here](https://www.r-project.org/)

    -  To download Rstudio click [here](https://www.rstudio.com/)

2.  Load `medusa` package. For this, open Rstudio and load the library:
    
    ``` r
    install.packages('remotes')
    remotes::install_github("bc3LC/medusa")
    library(medusa)
    ```
<br>
Now  the package is fully loaded and you can start using its functions.
<br>

## <a name="get-started"></a>Getting Started

<!-- ------------------------>

<!-- ------------------------>

[Back to Contents](#contents)

The `medusa` package  contains 3 main blocks of functions:

1. Module 1. Functions to calculate distributional impacts
	- Main function: `calc_di`, `ex_shocks`, `available_var_impact` , `available_var_intersec` and `ex_var_intersec`.
	- Auxiliary functions: `load_rawhbs`, `rename_values`, `standardize`, `add_coicop`, `elevate_hbs`, `price_shock`, `impact`, `impact_intersectional`, `basic_graph`, `intersectional_graph`, `order_var`, `adjust_wh` and `adjust_wh_is`.


2. Module 2. Functions to calculate energy poverty indices
	- Main function: `calc_ep`
	- Auxiliary functions: `id_ep1`, `id_ep2`, `weighted.median` and `weighted.quantile`.


3. Module 3. Functions to calculate transport poverty indices
	- Main function: `calc_tp`
	- Auxiliary functions: `id_tp`, `weighted.median` and `weighted.quantile`.


In addition, the package includes some default input files (.Rda), that are read by the different functions. 
