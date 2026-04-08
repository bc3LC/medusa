# MEDUSA - Modelling Equity and DistribUtional impacts for Socioeconomic Analysis

[![docs](https://github.com/bc3LC/medusa/actions/workflows/docs.yaml/badge.svg)](https://github.com/bc3LC/medusa/actions/workflows/docs.yaml)
[![pages-build-deployment](https://github.com/bc3LC/medusa/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/bc3LC/medusa/actions/workflows/pages/pages-build-deployment)
[![test_coverage](https://github.com/bc3LC/medusa/actions/workflows/test_coverage.yml/badge.svg)](https://github.com/bc3LC/medusa/actions/workflows/test_coverage.yml)
[![codecov](https://codecov.io/gh/bc3LC/medusa/graph/badge.svg?token=VSmmxRUGO2)](https://codecov.io/gh/bc3LC/medusa)
[![build](https://github.com/bc3LC/medusa/actions/workflows/build.yaml/badge.svg)](https://github.com/bc3LC/medusa/actions/workflows/build.yaml)
[![draft-pdf](https://github.com/bc3LC/medusa/actions/workflows/draft-pdf.yml/badge.svg)](https://github.com/bc3LC/medusa/actions/workflows/draft-pdf.yml)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15729051.svg)](https://doi.org/10.5281/zenodo.15729051)
[![DOI](https://joss.theoj.org/papers/10.21105/joss.07697/status.svg)](https://doi.org/10.21105/joss.07697)

## Contents

- [Introduction](#introduction)

  - [State of the field](#state)

  - [The package](#pkg)

  - [The microsimulation model](#ms-model)

- [Installation Guide](#installation-guide)

- [Getting Started](#get-started)

- [Contributing](#contributing)

## Introduction

[Back to Contents](#contents)

### State of the field

Addressing critical challenges, such as climate change, requires
ambitious policies that promote social justice while avoiding the
deepening of existing inequalities, such as income or gender
disparities. To achieve this, policy impact assessments must not only
take a holistic view of the economy, energy, land, and water systems but
also analyze the distributional effects on different population groups.

While Integrated Assessment Models (IAMs) play a key role in policy
evaluation, they often lack the granularity needed to capture
socio-economic disparities. Micro-simulation models help bridge this gap
by providing detailed, heterogeneous results that allow policymakers to
identify vulnerable populations and design targeted compensatory
measures. However, despite the growing emphasis on distributional
analysis in the academic literature, these approaches have not yet been
widely integrated into policymaking. A key barrier is the lack of
accessible tools that facilitate this type of analysis for practitioners
and policymakers.

MEDUSA is the first R package specifically designed to fill this gap.
MEDUSA provides a user-friendly framework for conducting distributional
analysis. By simplifying complex modeling processes, it enables a
broader audience —including researchers, policymakers, and analysts— to
incorporate equity considerations into policy design, ensuring that
policies are both effective and socially just.

### The package

MEDUSA is an R package that allows the development of distributional
analyses in isolation or in connection with other models (soft links).
The extensive database in which the microsimulation model is based
allows for highly disaggregated results, taking into account numerous
socioeconomic and demographic characteristics of households, such as
income level, place of residence, type of family or the feminization
degree of the household. Additionally, the package combines these with
the calculation of energy and transport poverty indices.

❗❗ **Note**: At the moment, `medusa` package works for Spain (INE
microdata) and all EU countries (EUROSTAT microdata). But the package
could be extended to all countries that are able to provide the raw data
of the model.

### The microsimulation model

MEDUSA allows to develop distributional analyses based in an overnight
effect microsimulation model. The model is built up with the microdata
from the Household Budget Survey. For more details on the model in which
`medusa` is based click
[here](https://bc3lc.github.io/medusa/articles/TheModel.html).

## Installation Guide

[Back to Contents](#contents)

To use `medusa` package first you need to follow this steps:

1.  Install R (required) and optionally an interface to facilitate
    usage, such as RStudio (only if not already installed):

    - To download R click [here](https://www.r-project.org/)

    - To download Rstudio click [here](https://www.rstudio.com/)

2.  Load `medusa` package. For this, open Rstudio and load the library:

    ``` r
    install.packages('remotes')
    options(timeout=400)                             # to prevent errors if the connection is slow
    remotes::install_github("bc3LC/medusa")
    library(medusa)
    ```

❗❗ This may take a few minutes because the package contains a lot of
data. If the installation fails, the timeout parameter may need to be
further increased. Additionally, sometimes users downloading `medusa`
with remotes may need to set up a Personal Access Token (PAT) and store
it in their credentials file. For more information about how to proceed
click [here](https://usethis.r-lib.org/articles/git-credentials.html).

  
Now the package is fully loaded and you can start using its functions.  

## Getting Started

[Back to Contents](#contents)

The `medusa` package contains 3 main blocks of functions:

1.  Module 1. Functions to calculate distributional impacts
    - Main functions (Spain): `calc_di`, `ex_shocks`,
      `available_var_impact`, `available_var_intersec` and
      `ex_var_intersec`.
    - Main functions (EU): `hbs_eu`, `calc_di_eu`, `ex_shocks_eu`,
      `available_var_eu`, `ex_var_intersec_eu` and `check_var_eu`.
    - Auxiliary functions: `load_rawhbs`, `rename_values`,
      `standardize`, `add_coicop`, `elevate_hbs`, `price_shock`,
      `impact`, `impact_intersectional`, `basic_graph`,
      `intersectional_graph`, `order_var`, `adjust_wh` and
      `adjust_wh_is`.
2.  Module 2. Functions to calculate energy poverty indices
    - Main functions: `calc_ep` (Spain), `calc_ep_eu` (EU)
    - Auxiliary functions: `id_ep1`, `id_ep2`, `weighted.median` and
      `weighted.quantile`.
3.  Module 3. Functions to calculate transport poverty indices
    - Main functions: `calc_tp` (Spain), `calc_tp_eu` (EU)
    - Auxiliary functions: `id_tp`, `weighted.median` and
      `weighted.quantile`.
4.  Module 4. Functions to connect with other models
    - Main functions: `get_prices_gcameurope` (EU).

In addition, the package includes some default input files (.Rda), that
are read by the different functions.

### Quick example — Spain

``` r
library(medusa)

# 1. Download the example shocks file and edit it
ex_shocks()
# [open Example_shocks.csv, fill in price shocks, save]
shocks <- read.csv("Example_shocks.csv", header = TRUE, sep = ",", dec = ".")

# 2. Distributional impacts by income decile
results <- calc_di(shocks = shocks, var = "decile")

# 3. Energy and transport poverty indices
ep <- calc_ep(index = "all")
tp <- calc_tp(index = "all")
```

### Quick example — EU

``` r
library(medusa)

# 0. Process HBS microdata (requires Eurostat raw files)
hbs <- hbs_eu(year = 2015, country = c("BE", "ES"), path = "raw_data")

# 1. Download the example shocks file and edit it
ex_shocks_eu()
# [open Example_shocks_eu.csv, fill in price shocks for each country-scenario, save]
shocks <- read.csv("Example_shocks_eu.csv", header = TRUE, sep = ",", dec = ".")

# 2. Distributional impacts by income decile
results <- calc_di_eu(data = hbs, shocks = shocks, var = "decile", by_country = TRUE)

# 3. Energy and transport poverty indices
ep <- calc_ep_eu(data = hbs, index = "all")
tp <- calc_tp_eu(data = hbs, index = "all")
```

For detailed step-by-step tutorials, see: - [Spain
tutorial](https://bc3lc.github.io/medusa/articles/TutorialsExample.html) -
[EU
tutorial](https://bc3lc.github.io/medusa/articles/TutorialsEU_example.html) -
[GCAM-Europe connection
tutorial](https://bc3lc.github.io/medusa/articles/TutorialsEU_di.html#example-5--connect-with-gcam-europe.html)

## Contributing

[Back to Contents](#contributing)

We welcome contributions! You can provide feedback, report issues, or
suggest improvements by opening an issue on GitHub
[here](https://github.com/bc3LC/medusa/issues). If you would like to
make changes to the code, we recommend first opening an issue to outline
your proposed modifications before submitting a pull request.

If you have any questions about the package, feel free to post them as
issues on GitHub as well.
