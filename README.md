# MEDUSA - Modelling Equity and DistribUtional impacts for Socioeconomic Analysis

## <a name="contents"></a>Contents

<!-- ------------------------>

<!-- ------------------------>

-   [Introduction](#introduction)

	-   [The package](#pkg)

    -   [The microsimulation model](#ms-model)

-   [Installation Guide](#installation-guide)

-   [Getting Started](#get-started)

-   [References](#references)

<!-- ------------------------>

<!-- ------------------------>

## <a name="introduction"></a>Introduction

<!-- ------------------------>

<!-- ------------------------>

[Back to Contents](#contents)

### <a name="pkg"></a>The package

MEDUSA is an R package that allows the development of distributional analyses in isolation or in connection with other models (soft links). The extensive database in which the microsimulation model is based allows for highly disaggregated results, taking into account numerous socioeconomic and demographic characteristics of households, such as income level, place of residence, type of family or the  feminization degree of the household. 

:exclamation::exclamation: **Note**: At the moment, `medusa` package works for Spain, but in the short term the idea is to extend it to all EU countries. Moreover, the package could be extended to all countries that are able to provide the raw data of the model.

### <a name="ms-model"></a>The microsimulation model

MEDUSA allows to develop distributional analyses based in the
following overnight effect microsimulation model. The model is built up with the microdata from the Household Budget Survey, a common statistic in all EU countries which is increasingly standardized and which has relevant potential due the large amount of socioeconomic information that it collects. The HBS provides information about household final consumption expenditure on goods and services and information on some socioeconomic and demographic characteristics of each household. The HBS provides information at two levels: one for households and their expenditures and the other for household members. 

In order to have a suitable database for the model, some adjustments are necessary. Firstly, some socio-economic variables have been renamed and new ones have been created based on the information collected in the survey (e.g. income variables: decile and quintile). In this sense, and since medusa aims to facilitate gender-sensitive analyses, the model includes two variables that allow capturing gender implications: the gender of the reference person of the household and the degree of feminization of the household. Although the HBS already includes the gender of the reference person among its variables, the second indicator is calculated using the information on the gender of household members. Thus, we calculate the percentage of household members who are women over 14 years (this age is used because it is considered the age at which individuals begin to have decision-making capacity) and then we divide the households into 5 groups based on the share of women: 0-20% (lowest feminization degree), 20%-40%, 40%-60%, 60%-80% and 80%-100% (highest feminization degree). We adopt this approach proposed by Osorio et al. (2022) within the framework of household carbon footprints for capturing all the dimensions of the intra-household consumption behaviour. Secondly, it is usual that expenditure variables by COICOP categories have to be aggregated because it is sometimes difficult to have price impacts at such a disaggregated level. To adjust the consumption expenditure data to the inputs provided by the price model, we include in the package a function that allows the aggregation of variables (the aggregation is determined by default but can also be customized according to the user's needs). Moreover, if the inputs of the price shock are determined by a macro model, an additional adjustment is necessary. In fact, despite the fact that the HBS covers a representative sample of the population and provides a very detailed image of the annual consumption of households, the aggregate costs of the survey are not aligned with the principles and data of the National Accounting, which builds its macroeconomic aggregates based on more complete sources of information. Therefore, before the simulation, the HBS data should be adjusted to make them consistent with the macroeconomic dimension. For this purpose, the package has a function that allows the survey to be elevated to make it consistent with the National Accounts. The following adjustments are made within the function: i) Scaling the HBS population to be consistent with the reference population of the National Accounts and ii) Scaling the HBS consumption data according to the final consumption per energy good of the National Accounts.

The model simulates the changes in spending in the COICOP categories, multiplying the changes in prices by the current levels of spending on the different products consumed by households that are part of the HBS dataset. Thus, the model reflects the direct impacts of the selected price shock before assuming any changes in behaviour related to the new prices. In other words, the microsimulation model does not reflect the reaction of the different types of households to the expected changes in prices. In order to carry out a "behavioural" impact study, it would be necessary to collect the direct reactions of consumers (through the price elasticities of demand for goods) and the induced reactions (through cross-elasticities and income elasticities). At the distributional level the lack of elasticities would not have a large effect because if the elasticity introduced is homogeneous for all households it will not affect the differences between different types of households. However, in future updates of the model it would be interesting to introduce heterogeneous household elasticities. Also, in the case of energy goods and transportation services, these effects are known to be small in the short and medium term, since households do not easily change their behaviour as far as energy consumption is concerned (Labandeira et al., 2017).

The results derived from the model are presented as the relative impact (%) on total equivalent consumption expenditure [[1]](#1). The relative impact, INSERTAR, shows the additional cost that household  would assume in a proposed scenario  in relative terms (%), compared to the initial household expenditure, and it is calculated as:

   ```r
 INSERT EQUATION

```
 
INSERT here  refers to the total spending on each consumption category, c,  consumed by each of the household, in the baseline scenario and  is the increase in prices by consumption category and scenario obtained with the price model.

The great granularity of the data contained in the HBS allows the integration of the intersectional approach [[2]](#2) in the analysis. The results can therefore be calculated according to numerous socioeconomic characteristics of the households (such as, class, gender, race…). Furthermore, in order to facilitate the analysis of the results, the package allows the generation of summary graphs of the distributional impacts either for one or several socio-economic variables.

<small><i> <a name="1">[1]</a>Equivalent consumption expenditure is used instead of income as it is considered a better proxy for permanent household income since it fluctuates less in the long run (Goodman & Oldfield, 2004). The equivalent spending is calculated based on household spending relativized by the modified OECD equivalence scale, thus considering the economies of scale generated in households based on their size. The modified OECD scale values 1 for the reference person in the household, 0.5 for other people aged 14 or over, and 0.3 for other people under 14 years of age.</i></small>

<small><i> <a name="2">[2]</a> Intersectionality refers to the fact that the privileges or oppression of each individual depend on the multiple social categories to which he or she belongs, which are social constructs and can change over time  (Cho et al., 2013; Crenshaw, 1994; Davis, 2008; Djoudi et al., 2016; Kaijser & Kronsell, 2014). Intersectionality is therefore also a tool for analysing the articulation of different socio-economic categories (e.g. class, gender, race, etc.) rather than considering them as independent forms of power relations (Colombo & Rebughini, 2016).</i></small>


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
    remotes::install_github("bc3LC/medusa") #you can skip all updates in case you are asked
    library(medusa)
    ```
<br>
Now  the package is fully loaded and you can start using its functions.
<br>

