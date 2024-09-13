---
title: 'MEDUSA: An R tool to model equity and distributional impacts'
tags:
- R
- distributional impacts
- equality
- socioeconomic analysis
date: "28 August 2024"
affiliations:
- name: "Basque Centre for Climate Change (BC3), Edificio Sede 1-1, Parque Científico
    de UPV/EHU, Barrio Sarriena s/n, 48940 Leioa, Spain"
  index: 1
authors:
- name: "Eva Alonso-Epelde"
  orcid: "0000-0002-4052-1703"
  affiliation: 1
- name: "Clàudia Rodés-Bachs"
  orcid: "0000-0001-6696-7685"
  affiliation: 1
bibliography: paper.bib
---

# Summary

Addressing 21st-century challenges like climate change requires policies that promote social justice and avoid deepening inequalities. While Integrated Assessment Models (IAMs) have been a fundamental tool to carry out impact analyses of policies from a holistic perspective, micro-simulation models are crucial for identifying heterogeneous socioeconomic impacts and ensuring fairer and more targeted policies.

`medusa` is an R package that allows the development of distributional analyses in isolation or in connection with other models (including IAMs). The extensive database in which the microsimulation model is based allows for highly disaggregated results, taking into account numerous socioeconomic and demographic characteristics of households, such as income level, place of residence, type of family or the feminization degree of the household. The package combines these with the calculation of energy and transport poverty indices. The structure of the `medusa` package is summarized in Figure 1.

![Structure of the `medusa` package](figure1.png)

The `medusa` package is available online through the public domain at <https://github.com/bc3LC/medusa>. Below is a simplified code example demonstrating how to execute the package. For a comprehensive introduction to `medusa`, a detailed step-by-step tutorial is provided in the form of an R vignette, accessible [here](https://bc3lc.github.io/medusa/).

``` r
install.packages("remotes")
library(remotes)
remotes::install_github("bc3LC/medusa")
library(medusa)

# Download the example file to enter price shocks
ex_shocks() 

# After introducing the price shocks in the csv file, upload the edited file
file_name <- read.csv(file,        # File name or full file path
             header = TRUE,        # Read the header (TRUE)
             sep = ",",            # Value separator
             dec = ".",            # Decimal point
             ...)                  # Additional arguments

# Calculate distributional impacts
calc_di( year,                     # Base year for the simulation
         elevate = F,              # Do not elevate the raw data
         shocks = file_name,       # Indicate the name of the uploaded file
         var_impact = "DECILE",    # Indicate the socioeconomic variable
         ...)
```

# Statement of need

Addressing critical challenges like climate change requires ambitious policies that promote social justice without worsening existing inequalities, such as income or gender disparities [@alonso-epelde2024]. To ensure this, it is essential to conduct policy impact assessments that not only consider the economy, energy, land, and water systems holistically but also analyze the distributional impacts across different population groups [@bazoli2022, @walker2010]. While Integrated Assessment Models (IAMs) have been invaluable in policy evaluation [@van2020], they often lack the granularity needed to assess socio-economic disparities. Micro-simulation models for distributional analysis fill this gap by providing detailed, heterogeneous results, enabling policymakers to identify vulnerable populations and implement targeted compensatory measures [@tomas2023]. This ensures that policies are equitable and socially just.

`medusa` facilitates distributional impact analyses through an overnight-effect microsimulation model, leveraging microdata from the Household Budget Survey (HBS), a standardized and comprehensive dataset available across EU countries. The HBS offers detailed insights into household consumption patterns and socioeconomic characteristics at both household and individual levels, allowing for highly granular analysis. This enables the integration of an intersectional approach [^1], considering factors such as class, gender, and race, and provides more robust and nuanced results for assessing policy impacts on diverse population groups.

[^1]: Intersectionality refers to the fact that the privileges or oppression of each individual depend on the multiple social categories to which he or she belongs, which are social constructs and can change over time ([@cho2013]; [@crenshaw1994, @davis1983, @djoudi2016, @kaijser2014]). Intersectionality is therefore also a tool for analysing the articulation of different socio-economic categories (e.g. class, gender, race, etc.) rather than considering them as independent forms of power relations (Colombo & Rebughini, 2016).

The results derived from the model are presented as the relative impact (%) on total equivalent consumption expenditure [^2]. The relative impact, $\Delta e_h^s$, shows the additional cost that household $h$ would assume in a proposed scenario in relative terms (%), compared to the initial household expenditure, and it is calculated as:

[^2]: Equivalent consumption expenditure is used instead of income as it is considered a better proxy for permanent household income since it fluctuates less in the long run [@goodman2004]. The equivalent spending is calculated based on household spending relativized by the modified OECD equivalence scale, thus considering the economies of scale generated in households based on their size. The modified OECD scale values 1 for the reference person in the household, 0.5 for other people aged 14 or over, and 0.3 for other people under 14 years of age.

$$\Delta e_h^s = \frac{ \sum_c e_{c,h} (1+\Delta p_c^s) - \sum_c e_{c,h} }{ \sum_c e_{c,h} } \times 100$$

here $e_{c,h}$ refers to the total spending on each consumption category, $c$, consumed by each of the household, in the baseline scenario and $\Delta p_c^s$ is the increase in prices by consumption category and scenario obtained with the price model.

# Functionality

The `medusa` package includes several functions that have been classified in 3 main modules. Note that this functions are listed in an [R vignette](https://bc3lc.github.io/medusa/), which includes a [step-by-step tutorial](https://bc3lc.github.io/medusa/articles/Tutorials.html).

-   Module 1: Functions to calculate the distributional impacts. The main function for users, calc_di(), calculates the distributional impacts for different households according to a wide range of socioeconomic and demographic characteristics.The distributional impacts could be calculated for one or for several intersecting variables. When introducing the outputs of a macro model in `medusa` the microdata in which the microsimulation model is based should be elevated to the National Accounts, this can be easily done indicating TRUE in the elevate parameter. Furthermore, in order to facilitate the analysis of the results, the package allows the generation of summary dataframes and figures of the distributional impacts either for one or several socioeconomic variables.

-   Module 2: Functions to calculate energy poverty indices.The main function for users, calc_ep(), calculates the energy poverty index for the selected year/s and the selected indicator. The indicators included in the package are the 10%, 2M, LIHC, HEP and HEP_LI.

-   Module 3: Functions to calculate transport poverty indices.The main function for users, calc_tp(), calculates the transport poverty index for the selected year/s and the selected indicator. The indicators included in the package are the 10%, 2M, LIHC and VTU.

The package includes default input files (.Rda), which are required for running the various functions, simplifying the process for users.

Output files are generated in both comma-separated values (CSV) and Portable Network Graphics (PNG) formats, with user control over file creation. When the save parameter is set to TRUE, the function saves a CSV file containing the selected results in the defined directory. Additionally, if the fig parameter is set to TRUE, the function produces and saves a barplot to visualize the corresponding output.

The package is actively evolving to meet research and policy needs, with several new features planned for future releases. For instance, an upcoming update will extend simulation capabilities to all EU countries, as the initial release currently covers only Spain. Additionally, we are developing a user interface designed to enable individuals without R programming expertise to perform socioeconomic analyses effectively.

# Acknowledgements

E.A. and C.R. acknowledges financial support from the European Union's Horizon research program under grant agreement 101056306 (IAM COMPACT project).

# References
