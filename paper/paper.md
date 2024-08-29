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
Add summary

`medusa` is an R package that allows the development of distributional analyses in isolation or in connection with other models. The extensive database in which the microsimulation model is based allows for highly disaggregated results, taking into account numerous socioeconomic and demographic characteristics of households, such as income level, place of residence, type of family or the feminization degree of the household.The package combines these with the calculations of energy and transport poverty indexes. 
The structure of the `medusa` package is summarized in Figure 1.

![Structure of the `medusa` package](figure1.png)

`medusa` can be accessed via the web at the public domain https://github.com/bc3LC/medusa. The following code is a simplified example that shows how to run the package. In addition, to get started with `medusa` we provide an R vignette step-by-step tutorial which is accessible [here](https://bc3lc.github.io/medusa/).

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
