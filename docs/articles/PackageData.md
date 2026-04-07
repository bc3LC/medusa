# Package Data

## Package data

### Household Budget Survey

The microsimulation model in which `medusa` is based is built up with
the microdata from the Household Budget Survey (HBS), a common statistic
in all EU countries which is increasingly standardized and which has
relevant potential due the large amount of socioeconomic information
that it collects. The HBS provides information about household final
consumption expenditure on goods and services and information on some
socioeconomic and demographic characteristics of each household. The HBS
provides information at two levels: one for households and their
expenditures and the other for household members. For more information
about the HBS click
[here](https://ec.europa.eu/eurostat/web/household-budget-surveys).

#### Data included in medusa

`medusa` includes pre-processed HBS microdata with some adjustments to
enhance usability. These modifications are common to both Spain and EU
data and include:

1.  Renaming some variables to make them more intuitive for users.
2.  Creating new socioeconomic indicators, such as income quintiles,
    deciles, ventiles, and percentiles — calculated at the **national
    level** for both Spain and EU data, and additionally at the **EU
    level** for EU data.
3.  Adding gender-sensitive variables:
    - Gender of the household reference person (already included in the
      HBS).
    - Degree of household feminization, calculated based on the
      proportion of women over 14 years old within the household:
      - FD1: 0–20% (lowest feminization)
      - FD2: 20–40%
      - FD3: 40–60%
      - FD4: 60–80%
      - FD5: 80–100% (highest feminization)

For a full list of variables available for distributional analysis,
click
[here](https://bc3lc.github.io/medusa/articles/AvailableVariables.html).

**Spain:** `medusa` includes Spanish HBS microdata for the period
**2006–2021**. The raw microdata can be downloaded from
[INE](https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176806&menu=resultados&idp=1254735976608#_tabs-1254736195147)
and processed using [`load_rawhbs()`](../reference/load_rawhbs.md).

**EU:** `medusa` does **not** include Eurostat HBS microdata, as these
are confidential and access is restricted. The available waves are
**2010, 2015 and 2020**. To request access to the microdata, visit the
[Eurostat microdata access
page](https://ec.europa.eu/eurostat/web/microdata/household-budget-survey).
Once access is granted, the data can be processed using
[`hbs_eu()`](../reference/hbs_eu.md). See the [EU
tutorials](https://bc3lc.github.io/medusa/articles/TutorialsEU_data.html)
for details.

#### Expenditure variables

HBS expenditure data follows the Classification of Individual
Consumption by Purpose (COICOP), an internationally recognised
classification developed by the United Nations Statistics Division. This
system categorises household expenditures into groups such as food,
clothing, housing, water, electricity, gas, and other fuels. For more
details on the COICOP classification click
[here](https://unstats.un.org/unsd/classifications/unsdclassifications/COICOP_2018_-_pre-edited_white_cover_version_-_2018-12-26.pdf).

### Population census

Although the HBS covers a representative sample of the population, the
total population does not coincide with the data collected in the
National Accounts. Therefore, in order to make the HBS data consistent
with the National Accounts data, the population data from the survey
would first have to be adjusted. To do this, `medusa` takes the Eurostat
census data as of 1 January and calculates adjustment coefficients. The
population data is available
[here](https://ec.europa.eu/eurostat/databrowser/view/tps00001/default/table?lang=en&category=t_demo.t_demo_pop).

### National accounting

Despite the fact that the HBS provides a very detailed image of the
annual consumption of households, the aggregate costs of the survey are
not aligned with the principles and data of the National Accounts.
Therefore, sometimes (e.g. when price shocks come from a macro model),
before the simulation the HBS data should be adjusted to make them
consistent with the macroeconomic dimension. `medusa` takes the National
Accounts consumption data and calculates an adjustment factor at the
highest possible level of disaggregation.

- **Spain:** The national accounting data used in `medusa` can be
  downloaded
  [here](https://github.com/bc3LC/medusa/blob/main/inst/extdata/gcfhogares95_22.csv).
- **EU:** Household final consumption expenditure data by COICOP
  category is retrieved from Eurostat. The relevant dataset is
  [`nama_10_co3_p3`](https://ec.europa.eu/eurostat/databrowser/view/nama_10_co3_p3/default/table),
  accessible via the Eurostat data browser or directly through the
  `restatapi` package.

### Data processing in `medusa`

#### Spain

`medusa` includes **pre-processed versions of the Spanish HBS data**
(2006–2021), so no prior data preparation is required to use the Spain
functions. Users who wish to work directly with the raw data can do so
using [`load_rawhbs()`](../reference/load_rawhbs.md). The main
modifications applied are:

- Merging household and expenditure datasets.
- Adding the socioeconomic and gender-sensitive variables described
  above.

Aligning expenditures with National Accounts is **optional** and only
performed when users set `elevate = TRUE` in
[`calc_di()`](../reference/calc_di.md).

#### EU

`medusa` does **not** include pre-processed EU data. To use the EU
functions, users must:

1.  Obtain access to Eurostat HBS microdata (see [Preparing the
    data](https://bc3lc.github.io/medusa/articles/TutorialsEU_data.html)).
2.  Process the raw microdata using
    [`hbs_eu()`](../reference/hbs_eu.md), which handles merging,
    variable creation, and standardisation.
3.  Use the resulting dataset as input to
    [`calc_di_eu()`](../reference/calc_di_eu.md),
    [`calc_ep_eu()`](../reference/calc_ep_eu.md), and
    [`calc_tp_eu()`](../reference/calc_tp_eu.md).
