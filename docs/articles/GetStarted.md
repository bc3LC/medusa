# Get started

## Get started with `medusa`

`medusa` is an R package for microsimulation-based distributional
analysis of price shocks and poverty assessment. It is built on
microdata from the **Household Budget Survey (HBS)**, which collects
detailed information on household consumption expenditure and
socioeconomic characteristics across EU countries.

The package provides three main modules:

| Module | Spain | EU countries |
|----|----|----|
| **Distributional impacts** | [`calc_di()`](../reference/calc_di.md) | [`calc_di_eu()`](../reference/calc_di_eu.md) |
| **Energy poverty indices** | [`calc_ep()`](../reference/calc_ep.md) | [`calc_ep_eu()`](../reference/calc_ep_eu.md) |
| **Transport poverty indices** | [`calc_tp()`](../reference/calc_tp.md) | [`calc_tp_eu()`](../reference/calc_tp_eu.md) |

For a full description of the microsimulation model, see the
[Distributional
impacts](https://bc3lc.github.io/medusa/articles/TheModel.html) article.

### Installation

Install the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("bc3LC/medusa")
```

Load the package:

``` r
library(medusa)
```

### Spain

`medusa` includes pre-processed Spanish HBS microdata for **2006–2021**.
No prior data preparation is needed.

#### Quick example

``` r
library(medusa)

# Download the example price shocks file
ex_shocks()

# Load the file (after editing the shocks)
shocks <- read.csv("Example_shocks.csv", header = TRUE, sep = ",", dec = ".")

# Calculate distributional impacts for 2019 by income decile
calc_di(2019, shocks = shocks, var_impact = "DECILE")

# Calculate energy poverty indices for 2019
calc_ep(year = 2019, index = "all")

# Calculate transport poverty indices for 2019
calc_tp(year = 2019, index = "all")
```

For step-by-step guidance, see the [Spain
tutorials](https://bc3lc.github.io/medusa/articles/Tutorials.html).

### EU countries

`medusa` supports **27 EU member states** using Eurostat HBS microdata
for the waves **2010, 2015 and 2020**. The microdata are
**confidential** and must be requested directly from Eurostat — they are
not included in the package.

#### Workflow

``` r
library(medusa)

# Step 1: Process the raw Eurostat microdata (once per wave)
hbs <- hbs_eu(year = 2015, country = "all", path = "path/to/your/data")

# Step 2: Download the example price shocks file for EU
ex_shocks_eu()

# Load the file (after editing the shocks)
shocks <- read.csv("Example_shocks_eu.csv", header = TRUE, sep = ",", dec = ".")

# Step 3: Calculate distributional impacts
calc_di_eu(data = hbs, shocks = shocks, var = "all")

# Step 4 (optional): Calculate energy poverty indices
calc_ep_eu(data = hbs, index = "all")

# Step 5 (optional): Calculate transport poverty indices
calc_tp_eu(data = hbs, index = "all")
```

For step-by-step guidance:

- [Preparing the
  data](https://bc3lc.github.io/medusa/articles/TutorialsEU_data.html) —
  how to organise the Eurostat files and run
  [`hbs_eu()`](../reference/hbs_eu.md)
- [Calculate distributional
  impacts](https://bc3lc.github.io/medusa/articles/TutorialsEU_di.html)
- [Calculate energy poverty
  indices](https://bc3lc.github.io/medusa/articles/TutorialsEU_ep.html)
- [Calculate transport poverty
  indices](https://bc3lc.github.io/medusa/articles/TutorialsEU_tp.html)
- [Step-by-step
  example](https://bc3lc.github.io/medusa/articles/TutorialsEU_example.html)

### Available variables

The socioeconomic and demographic variables available for distributional
analysis differ between Spain and EU data. For a full list, see
[Available
Variables](https://bc3lc.github.io/medusa/articles/AvailableVariables.html).

``` r
# Spain
available_var_impact()

# EU
available_var_eu()
```

### Package data

For details on the data sources and processing methodology, see [Package
Data](https://bc3lc.github.io/medusa/articles/PackageData.html).
