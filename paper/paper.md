---
title: 'MEDUSA: An R tool to model equity and distributional impacts'
tags:
  - R
  - distributional impacts
  - equality
  - socioeconomic analysis
authors:
  - name: Eva Alonso-Epelde
    orcid: 0000-0002-4052-1703
    affiliation: 1
  - name: Clàudia Rodés-Bachs
    orcid: 0000-0001-6696-7685
    affiliation: 1
  - name: María Moyano-Reina
    orcid: 0009-0000-4290-0697
    affiliation: 1  
affiliations:
 - name: Basque Centre for Climate Change (BC3), Edificio Sede 1-1, Parque Científico de UPV/EHU, Barrio Sarriena s/n, 48940 Leioa, Spain
   index: 1
date: 28 August 2024
bibliography: paper.bib
---
# Summary
Addressing the major challenges of the 21st century, such as climate change, will require complex and ambitious policies that promote social justice. To do so, it is necessary to design efficient policies that do not exacerbate existing inequalities, such as income or gender inequality. In this sense, it is essential to carry out impact analyses of policies from a holistic perspective that evaluates the economy, energy, land, and water systems in an integrated manner before implementing them. While IAMs have been a fundamental tool in the past, micro-simulation models for distributional analysis have the advantage of providing more heterogeneous results that help to more robustly identify the socio-economic impacts of the policies to be implemented. These analyses make it possible to identify the people who will be most affected by policies and to implement compensatory measures to make the policy fairer. Thus, the combination of both models (IAMs and microsimulation models) can provide valuable results for decision making.

`medusa` is an R package that allows the development of distributional analyses in isolation or in connection with other models. The extensive database in which the microsimulation model is based allows for highly disaggregated results, taking into account numerous socioeconomic and demographic characteristics of households, such as income level, place of residence, type of family or the feminization degree of the household. The package combines these with the calculation of energy and transport poverty indices. 
The structure of the `medusa` package is summarized in Figure 1.

![Structure of the `medusa` package](figure1.png)

The `medusa` package is available online through the public domain at https://github.com/bc3LC/medusa. Below is a simplified code example demonstrating how to execute the package. For a comprehensive introduction to `medusa`, a detailed step-by-step tutorial is provided in the form of an R vignette, accessible [here](https://bc3lc.github.io/medusa/).

```r
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
Add Statement of need


# Functionality
Add functionality


# Acknowledgements
E.A. and C.R. acknowledges financial support from the European Union's Horizon research program under grant agreement 101056306 (IAM COMPACT project).


# References
