#' Cleaned Household Budget Survey (HBS) microdata for 2006
#'
#' This object contains cleaned microdata from the 2006 wave of the Spanish Household Budget Survey (EPF, Encuesta de Presupuestos Familiares), harmonized and prepared for use with the `medusa` package.
#'
#' The data are structured as a named list with the following main components:
#'
#' - `epf_hg`: Household-level data including consumption expenditure (COICOP categories prefixed with `EUR_`) and key socioeconomic variables such as income groupings, household type, region, and gender of the household reference person.
#' - `epf_hgm`: Member-level data with individual characteristics of household members (e.g., age, gender, education).
#' - `epf_hc`: Household-level data containing consumption quantities (physical amounts) by COICOP category.
#'
#' @section Main socioeconomic variables in `epf_hg`:
#' - `QUINTILE`, `DECILE`, `VENTILE`, `PERCENTILE`: Income distribution indicators.
#' - `REGION`: Autonomous Community of residence (NUTS2).
#' - `MUNISIZE`: Municipality size (`>10.000`, `<10.000`).
#' - `ZONE`: Area type (`Urban`, `Semi-urban`, `Rural`).
#' - `HHTYPE`: Household type (e.g., `Couples with children`, `Single parent`, `Elderly alone`).
#' - `CHILDREN`: Children in the household (`No children`, `With children`, `Large family`).
#' - `POVERTY`: Poverty risk status (`At risk`, `No risk`).
#' - `GENDERRP`: Gender of the reference person (`Man`, `Woman`).
#' - `FEMDEGREE`: Feminization degree of the household (FD1 to FD5).
#' - `AGERP`: Age group of the reference person (`Young`, `Adult`, `Elder`).
#' - `COUNTRYRP`: Country of birth of the reference person (`Spain`, `EU27`, `Other Europe`, `Rest of world`).
#' - `STUDIESRP`: Education level (`Without studies` to `Higher education`).
#' - `PROFESSIONALSRP`: Professional status (`Employee`, `Self-employed`, `Employer`, `Other`, `Not apply`).
#' - `CONTTYPERP`: Employment contract type (`Indefinite`, `Temporary`, `Not apply`).
#' - `WORKDAYRP`: Working hours (`Full time`, `Part time`, `Not apply`).
#' - `REGMR`: Tenure status of main residence (`Ownership`, `Rented`, `Relinquish`).
#'
#' @source medusa based on Instituto Nacional de Estadística (INE). Full survey documentation available at: \url{https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176806&menu=resultados&idp=1254735976608#_tabs-1254736195147}
#'
#' @format .RData
"epf_list_2006"

#' Cleaned Household Budget Survey (HBS) microdata for 2007
#'
#' This object contains cleaned microdata from the 2007 wave of the Spanish Household Budget Survey (EPF, Encuesta de Presupuestos Familiares), harmonized and prepared for use with the `medusa` package.
#'
#' The data are structured as a named list with the following main components:
#'
#' - `epf_hg`: Household-level data including consumption expenditure (COICOP categories prefixed with `EUR_`) and key socioeconomic variables such as income groupings, household type, region, and gender of the household reference person.
#' - `epf_hgm`: Member-level data with individual characteristics of household members (e.g., age, gender, education).
#' - `epf_hc`: Household-level data containing consumption quantities (physical amounts) by COICOP category.
#'
#' @section Main socioeconomic variables in `epf_hg`:
#' - `QUINTILE`, `DECILE`, `VENTILE`, `PERCENTILE`: Income distribution indicators.
#' - `REGION`: Autonomous Community of residence (NUTS2).
#' - `MUNISIZE`: Municipality size (`>10.000`, `<10.000`).
#' - `ZONE`: Area type (`Urban`, `Semi-urban`, `Rural`).
#' - `HHTYPE`: Household type (e.g., `Couples with children`, `Single parent`, `Elderly alone`).
#' - `CHILDREN`: Children in the household (`No children`, `With children`, `Large family`).
#' - `POVERTY`: Poverty risk status (`At risk`, `No risk`).
#' - `GENDERRP`: Gender of the reference person (`Man`, `Woman`).
#' - `FEMDEGREE`: Feminization degree of the household (FD1 to FD5).
#' - `AGERP`: Age group of the reference person (`Young`, `Adult`, `Elder`).
#' - `COUNTRYRP`: Country of birth of the reference person (`Spain`, `EU27`, `Other Europe`, `Rest of world`).
#' - `STUDIESRP`: Education level (`Without studies` to `Higher education`).
#' - `PROFESSIONALSRP`: Professional status (`Employee`, `Self-employed`, `Employer`, `Other`, `Not apply`).
#' - `CONTTYPERP`: Employment contract type (`Indefinite`, `Temporary`, `Not apply`).
#' - `WORKDAYRP`: Working hours (`Full time`, `Part time`, `Not apply`).
#' - `REGMR`: Tenure status of main residence (`Ownership`, `Rented`, `Relinquish`).
#'
#' @source medusa based on Instituto Nacional de Estadística (INE). Full survey documentation available at: \url{https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176806&menu=resultados&idp=1254735976608#_tabs-1254736195147}
#' @format .RData
"epf_list_2007"

#' Cleaned Household Budget Survey (HBS) microdata for 2008
#'
#' This object contains cleaned microdata from the 2008 wave of the Spanish Household Budget Survey (EPF, Encuesta de Presupuestos Familiares), harmonized and prepared for use with the `medusa` package.
#'
#' The data are structured as a named list with the following main components:
#'
#' - `epf_hg`: Household-level data including consumption expenditure (COICOP categories prefixed with `EUR_`) and key socioeconomic variables such as income groupings, household type, region, and gender of the household reference person.
#' - `epf_hgm`: Member-level data with individual characteristics of household members (e.g., age, gender, education).
#' - `epf_hc`: Household-level data containing consumption quantities (physical amounts) by COICOP category.
#'
#' @section Main socioeconomic variables in `epf_hg`:
#' - `QUINTILE`, `DECILE`, `VENTILE`, `PERCENTILE`: Income distribution indicators.
#' - `REGION`: Autonomous Community of residence (NUTS2).
#' - `MUNISIZE`: Municipality size (`>10.000`, `<10.000`).
#' - `ZONE`: Area type (`Urban`, `Semi-urban`, `Rural`).
#' - `HHTYPE`: Household type (e.g., `Couples with children`, `Single parent`, `Elderly alone`).
#' - `CHILDREN`: Children in the household (`No children`, `With children`, `Large family`).
#' - `POVERTY`: Poverty risk status (`At risk`, `No risk`).
#' - `GENDERRP`: Gender of the reference person (`Man`, `Woman`).
#' - `FEMDEGREE`: Feminization degree of the household (FD1 to FD5).
#' - `AGERP`: Age group of the reference person (`Young`, `Adult`, `Elder`).
#' - `COUNTRYRP`: Country of birth of the reference person (`Spain`, `EU27`, `Other Europe`, `Rest of world`).
#' - `STUDIESRP`: Education level (`Without studies` to `Higher education`).
#' - `PROFESSIONALSRP`: Professional status (`Employee`, `Self-employed`, `Employer`, `Other`, `Not apply`).
#' - `CONTTYPERP`: Employment contract type (`Indefinite`, `Temporary`, `Not apply`).
#' - `WORKDAYRP`: Working hours (`Full time`, `Part time`, `Not apply`).
#' - `REGMR`: Tenure status of main residence (`Ownership`, `Rented`, `Relinquish`).
#'
#' @source medusa based on Instituto Nacional de Estadística (INE). Full survey documentation available at: \url{https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176806&menu=resultados&idp=1254735976608#_tabs-1254736195147}
#' @format .RData
"epf_list_2008"

#' Cleaned Household Budget Survey (HBS) microdata for 2009
#'
#' This object contains cleaned microdata from the 2009 wave of the Spanish Household Budget Survey (EPF, Encuesta de Presupuestos Familiares), harmonized and prepared for use with the `medusa` package.
#'
#' The data are structured as a named list with the following main components:
#'
#' - `epf_hg`: Household-level data including consumption expenditure (COICOP categories prefixed with `EUR_`) and key socioeconomic variables such as income groupings, household type, region, and gender of the household reference person.
#' - `epf_hgm`: Member-level data with individual characteristics of household members (e.g., age, gender, education).
#' - `epf_hc`: Household-level data containing consumption quantities (physical amounts) by COICOP category.
#'
#' @section Main socioeconomic variables in `epf_hg`:
#' - `QUINTILE`, `DECILE`, `VENTILE`, `PERCENTILE`: Income distribution indicators.
#' - `REGION`: Autonomous Community of residence (NUTS2).
#' - `MUNISIZE`: Municipality size (`>10.000`, `<10.000`).
#' - `ZONE`: Area type (`Urban`, `Semi-urban`, `Rural`).
#' - `HHTYPE`: Household type (e.g., `Couples with children`, `Single parent`, `Elderly alone`).
#' - `CHILDREN`: Children in the household (`No children`, `With children`, `Large family`).
#' - `POVERTY`: Poverty risk status (`At risk`, `No risk`).
#' - `GENDERRP`: Gender of the reference person (`Man`, `Woman`).
#' - `FEMDEGREE`: Feminization degree of the household (FD1 to FD5).
#' - `AGERP`: Age group of the reference person (`Young`, `Adult`, `Elder`).
#' - `COUNTRYRP`: Country of birth of the reference person (`Spain`, `EU27`, `Other Europe`, `Rest of world`).
#' - `STUDIESRP`: Education level (`Without studies` to `Higher education`).
#' - `PROFESSIONALSRP`: Professional status (`Employee`, `Self-employed`, `Employer`, `Other`, `Not apply`).
#' - `CONTTYPERP`: Employment contract type (`Indefinite`, `Temporary`, `Not apply`).
#' - `WORKDAYRP`: Working hours (`Full time`, `Part time`, `Not apply`).
#' - `REGMR`: Tenure status of main residence (`Ownership`, `Rented`, `Relinquish`).
#'
#' @source medusa based on Instituto Nacional de Estadística (INE). Full survey documentation available at: \url{https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176806&menu=resultados&idp=1254735976608#_tabs-1254736195147}
#'
#' @format .RData
"epf_list_2009"

#' Cleaned Household Budget Survey (HBS) microdata for 2010
#'
#' This object contains cleaned microdata from the 2010 wave of the Spanish Household Budget Survey (EPF, Encuesta de Presupuestos Familiares), harmonized and prepared for use with the `medusa` package.
#'
#' The data are structured as a named list with the following main components:
#'
#' - `epf_hg`: Household-level data including consumption expenditure (COICOP categories prefixed with `EUR_`) and key socioeconomic variables such as income groupings, household type, region, and gender of the household reference person.
#' - `epf_hgm`: Member-level data with individual characteristics of household members (e.g., age, gender, education).
#' - `epf_hc`: Household-level data containing consumption quantities (physical amounts) by COICOP category.
#'
#' @section Main socioeconomic variables in `epf_hg`:
#' - `QUINTILE`, `DECILE`, `VENTILE`, `PERCENTILE`: Income distribution indicators.
#' - `REGION`: Autonomous Community of residence (NUTS2).
#' - `MUNISIZE`: Municipality size (`>10.000`, `<10.000`).
#' - `ZONE`: Area type (`Urban`, `Semi-urban`, `Rural`).
#' - `HHTYPE`: Household type (e.g., `Couples with children`, `Single parent`, `Elderly alone`).
#' - `CHILDREN`: Children in the household (`No children`, `With children`, `Large family`).
#' - `POVERTY`: Poverty risk status (`At risk`, `No risk`).
#' - `GENDERRP`: Gender of the reference person (`Man`, `Woman`).
#' - `FEMDEGREE`: Feminization degree of the household (FD1 to FD5).
#' - `AGERP`: Age group of the reference person (`Young`, `Adult`, `Elder`).
#' - `COUNTRYRP`: Country of birth of the reference person (`Spain`, `EU27`, `Other Europe`, `Rest of world`).
#' - `STUDIESRP`: Education level (`Without studies` to `Higher education`).
#' - `PROFESSIONALSRP`: Professional status (`Employee`, `Self-employed`, `Employer`, `Other`, `Not apply`).
#' - `CONTTYPERP`: Employment contract type (`Indefinite`, `Temporary`, `Not apply`).
#' - `WORKDAYRP`: Working hours (`Full time`, `Part time`, `Not apply`).
#' - `REGMR`: Tenure status of main residence (`Ownership`, `Rented`, `Relinquish`).
#'
#' @source medusa based on Instituto Nacional de Estadística (INE). Full survey documentation available at: \url{https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176806&menu=resultados&idp=1254735976608#_tabs-1254736195147}
#'
#' @format .RData
"epf_list_2010"

#' Cleaned Household Budget Survey (HBS) microdata for 2011
#'
#' This object contains cleaned microdata from the 2011 wave of the Spanish Household Budget Survey (EPF, Encuesta de Presupuestos Familiares), harmonized and prepared for use with the `medusa` package.
#'
#' The data are structured as a named list with the following main components:
#'
#' - `epf_hg`: Household-level data including consumption expenditure (COICOP categories prefixed with `EUR_`) and key socioeconomic variables such as income groupings, household type, region, and gender of the household reference person.
#' - `epf_hgm`: Member-level data with individual characteristics of household members (e.g., age, gender, education).
#' - `epf_hc`: Household-level data containing consumption quantities (physical amounts) by COICOP category.
#'
#' @section Main socioeconomic variables in `epf_hg`:
#' - `QUINTILE`, `DECILE`, `VENTILE`, `PERCENTILE`: Income distribution indicators.
#' - `REGION`: Autonomous Community of residence (NUTS2).
#' - `MUNISIZE`: Municipality size (`>10.000`, `<10.000`).
#' - `ZONE`: Area type (`Urban`, `Semi-urban`, `Rural`).
#' - `HHTYPE`: Household type (e.g., `Couples with children`, `Single parent`, `Elderly alone`).
#' - `CHILDREN`: Children in the household (`No children`, `With children`, `Large family`).
#' - `POVERTY`: Poverty risk status (`At risk`, `No risk`).
#' - `GENDERRP`: Gender of the reference person (`Man`, `Woman`).
#' - `FEMDEGREE`: Feminization degree of the household (FD1 to FD5).
#' - `AGERP`: Age group of the reference person (`Young`, `Adult`, `Elder`).
#' - `COUNTRYRP`: Country of birth of the reference person (`Spain`, `EU27`, `Other Europe`, `Rest of world`).
#' - `STUDIESRP`: Education level (`Without studies` to `Higher education`).
#' - `PROFESSIONALSRP`: Professional status (`Employee`, `Self-employed`, `Employer`, `Other`, `Not apply`).
#' - `CONTTYPERP`: Employment contract type (`Indefinite`, `Temporary`, `Not apply`).
#' - `WORKDAYRP`: Working hours (`Full time`, `Part time`, `Not apply`).
#' - `REGMR`: Tenure status of main residence (`Ownership`, `Rented`, `Relinquish`).
#'
#' @source medusa based on Instituto Nacional de Estadística (INE). Full survey documentation available at: \url{https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176806&menu=resultados&idp=1254735976608#_tabs-1254736195147}
#'
#' @format .RData
"epf_list_2011"

#' Cleaned Household Budget Survey (HBS) microdata for 2012
#'
#' This object contains cleaned microdata from the 2012 wave of the Spanish Household Budget Survey (EPF, Encuesta de Presupuestos Familiares), harmonized and prepared for use with the `medusa` package.
#'
#' The data are structured as a named list with the following main components:
#'
#' - `epf_hg`: Household-level data including consumption expenditure (COICOP categories prefixed with `EUR_`) and key socioeconomic variables such as income groupings, household type, region, and gender of the household reference person.
#' - `epf_hgm`: Member-level data with individual characteristics of household members (e.g., age, gender, education).
#' - `epf_hc`: Household-level data containing consumption quantities (physical amounts) by COICOP category.
#'
#' @section Main socioeconomic variables in `epf_hg`:
#' - `QUINTILE`, `DECILE`, `VENTILE`, `PERCENTILE`: Income distribution indicators.
#' - `REGION`: Autonomous Community of residence (NUTS2).
#' - `MUNISIZE`: Municipality size (`>10.000`, `<10.000`).
#' - `ZONE`: Area type (`Urban`, `Semi-urban`, `Rural`).
#' - `HHTYPE`: Household type (e.g., `Couples with children`, `Single parent`, `Elderly alone`).
#' - `CHILDREN`: Children in the household (`No children`, `With children`, `Large family`).
#' - `POVERTY`: Poverty risk status (`At risk`, `No risk`).
#' - `GENDERRP`: Gender of the reference person (`Man`, `Woman`).
#' - `FEMDEGREE`: Feminization degree of the household (FD1 to FD5).
#' - `AGERP`: Age group of the reference person (`Young`, `Adult`, `Elder`).
#' - `COUNTRYRP`: Country of birth of the reference person (`Spain`, `EU27`, `Other Europe`, `Rest of world`).
#' - `STUDIESRP`: Education level (`Without studies` to `Higher education`).
#' - `PROFESSIONALSRP`: Professional status (`Employee`, `Self-employed`, `Employer`, `Other`, `Not apply`).
#' - `CONTTYPERP`: Employment contract type (`Indefinite`, `Temporary`, `Not apply`).
#' - `WORKDAYRP`: Working hours (`Full time`, `Part time`, `Not apply`).
#' - `REGMR`: Tenure status of main residence (`Ownership`, `Rented`, `Relinquish`).
#'
#' @source medusa based on Instituto Nacional de Estadística (INE). Full survey documentation available at: \url{https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176806&menu=resultados&idp=1254735976608#_tabs-1254736195147}
#'
#' @format .RData
"epf_list_2012"

#' Cleaned Household Budget Survey (HBS) microdata for 2013
#'
#' This object contains cleaned microdata from the 2013 wave of the Spanish Household Budget Survey (EPF, Encuesta de Presupuestos Familiares), harmonized and prepared for use with the `medusa` package.
#'
#' The data are structured as a named list with the following main components:
#'
#' - `epf_hg`: Household-level data including consumption expenditure (COICOP categories prefixed with `EUR_`) and key socioeconomic variables such as income groupings, household type, region, and gender of the household reference person.
#' - `epf_hgm`: Member-level data with individual characteristics of household members (e.g., age, gender, education).
#' - `epf_hc`: Household-level data containing consumption quantities (physical amounts) by COICOP category.
#'
#' @section Main socioeconomic variables in `epf_hg`:
#' - `QUINTILE`, `DECILE`, `VENTILE`, `PERCENTILE`: Income distribution indicators.
#' - `REGION`: Autonomous Community of residence (NUTS2).
#' - `MUNISIZE`: Municipality size (`>10.000`, `<10.000`).
#' - `ZONE`: Area type (`Urban`, `Semi-urban`, `Rural`).
#' - `HHTYPE`: Household type (e.g., `Couples with children`, `Single parent`, `Elderly alone`).
#' - `CHILDREN`: Children in the household (`No children`, `With children`, `Large family`).
#' - `POVERTY`: Poverty risk status (`At risk`, `No risk`).
#' - `GENDERRP`: Gender of the reference person (`Man`, `Woman`).
#' - `FEMDEGREE`: Feminization degree of the household (FD1 to FD5).
#' - `AGERP`: Age group of the reference person (`Young`, `Adult`, `Elder`).
#' - `COUNTRYRP`: Country of birth of the reference person (`Spain`, `EU27`, `Other Europe`, `Rest of world`).
#' - `STUDIESRP`: Education level (`Without studies` to `Higher education`).
#' - `PROFESSIONALSRP`: Professional status (`Employee`, `Self-employed`, `Employer`, `Other`, `Not apply`).
#' - `CONTTYPERP`: Employment contract type (`Indefinite`, `Temporary`, `Not apply`).
#' - `WORKDAYRP`: Working hours (`Full time`, `Part time`, `Not apply`).
#' - `REGMR`: Tenure status of main residence (`Ownership`, `Rented`, `Relinquish`).
#'
#' @source medusa based on Instituto Nacional de Estadística (INE). Full survey documentation available at: \url{https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176806&menu=resultados&idp=1254735976608#_tabs-1254736195147}
#'
#' @format .RData
"epf_list_2013"

#' Cleaned Household Budget Survey (HBS) microdata for 2014
#'
#' This object contains cleaned microdata from the 2014 wave of the Spanish Household Budget Survey (EPF, Encuesta de Presupuestos Familiares), harmonized and prepared for use with the `medusa` package.
#'
#' The data are structured as a named list with the following main components:
#'
#' - `epf_hg`: Household-level data including consumption expenditure (COICOP categories prefixed with `EUR_`) and key socioeconomic variables such as income groupings, household type, region, and gender of the household reference person.
#' - `epf_hgm`: Member-level data with individual characteristics of household members (e.g., age, gender, education).
#' - `epf_hc`: Household-level data containing consumption quantities (physical amounts) by COICOP category.
#'
#' @section Main socioeconomic variables in `epf_hg`:
#' - `QUINTILE`, `DECILE`, `VENTILE`, `PERCENTILE`: Income distribution indicators.
#' - `REGION`: Autonomous Community of residence (NUTS2).
#' - `MUNISIZE`: Municipality size (`>10.000`, `<10.000`).
#' - `ZONE`: Area type (`Urban`, `Semi-urban`, `Rural`).
#' - `HHTYPE`: Household type (e.g., `Couples with children`, `Single parent`, `Elderly alone`).
#' - `CHILDREN`: Children in the household (`No children`, `With children`, `Large family`).
#' - `POVERTY`: Poverty risk status (`At risk`, `No risk`).
#' - `GENDERRP`: Gender of the reference person (`Man`, `Woman`).
#' - `FEMDEGREE`: Feminization degree of the household (FD1 to FD5).
#' - `AGERP`: Age group of the reference person (`Young`, `Adult`, `Elder`).
#' - `COUNTRYRP`: Country of birth of the reference person (`Spain`, `EU27`, `Other Europe`, `Rest of world`).
#' - `STUDIESRP`: Education level (`Without studies` to `Higher education`).
#' - `PROFESSIONALSRP`: Professional status (`Employee`, `Self-employed`, `Employer`, `Other`, `Not apply`).
#' - `CONTTYPERP`: Employment contract type (`Indefinite`, `Temporary`, `Not apply`).
#' - `WORKDAYRP`: Working hours (`Full time`, `Part time`, `Not apply`).
#' - `REGMR`: Tenure status of main residence (`Ownership`, `Rented`, `Relinquish`).
#'
#' @source medusa based on Instituto Nacional de Estadística (INE). Full survey documentation available at: \url{https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176806&menu=resultados&idp=1254735976608#_tabs-1254736195147}
#'
#' @format .RData
"epf_list_2014"

#' Cleaned Household Budget Survey (HBS) microdata for 2015
#'
#' This object contains cleaned microdata from the 2015 wave of the Spanish Household Budget Survey (EPF, Encuesta de Presupuestos Familiares), harmonized and prepared for use with the `medusa` package.
#'
#' The data are structured as a named list with the following main components:
#'
#' - `epf_hg`: Household-level data including consumption expenditure (COICOP categories prefixed with `EUR_`) and key socioeconomic variables such as income groupings, household type, region, and gender of the household reference person.
#' - `epf_hgm`: Member-level data with individual characteristics of household members (e.g., age, gender, education).
#' - `epf_hc`: Household-level data containing consumption quantities (physical amounts) by COICOP category.
#'
#' @section Main socioeconomic variables in `epf_hg`:
#' - `QUINTILE`, `DECILE`, `VENTILE`, `PERCENTILE`: Income distribution indicators.
#' - `REGION`: Autonomous Community of residence (NUTS2).
#' - `MUNISIZE`: Municipality size (`>10.000`, `<10.000`).
#' - `ZONE`: Area type (`Urban`, `Semi-urban`, `Rural`).
#' - `HHTYPE`: Household type (e.g., `Couples with children`, `Single parent`, `Elderly alone`).
#' - `CHILDREN`: Children in the household (`No children`, `With children`, `Large family`).
#' - `POVERTY`: Poverty risk status (`At risk`, `No risk`).
#' - `GENDERRP`: Gender of the reference person (`Man`, `Woman`).
#' - `FEMDEGREE`: Feminization degree of the household (FD1 to FD5).
#' - `AGERP`: Age group of the reference person (`Young`, `Adult`, `Elder`).
#' - `COUNTRYRP`: Country of birth of the reference person (`Spain`, `EU27`, `Other Europe`, `Rest of world`).
#' - `STUDIESRP`: Education level (`Without studies` to `Higher education`).
#' - `PROFESSIONALSRP`: Professional status (`Employee`, `Self-employed`, `Employer`, `Other`, `Not apply`).
#' - `CONTTYPERP`: Employment contract type (`Indefinite`, `Temporary`, `Not apply`).
#' - `WORKDAYRP`: Working hours (`Full time`, `Part time`, `Not apply`).
#' - `REGMR`: Tenure status of main residence (`Ownership`, `Rented`, `Relinquish`).
#'
#' @source medusa based on Instituto Nacional de Estadística (INE). Full survey documentation available at: \url{https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176806&menu=resultados&idp=1254735976608#_tabs-1254736195147}
#'
#' @format .RData
"epf_list_2015"

#' Cleaned Household Budget Survey (HBS) microdata for 2016
#'
#' This object contains cleaned microdata from the 2016 wave of the Spanish Household Budget Survey (EPF, Encuesta de Presupuestos Familiares), harmonized and prepared for use with the `medusa` package.
#'
#' The data are structured as a named list with the following main components:
#'
#' - `epf_hg`: Household-level data including consumption expenditure (COICOP categories prefixed with `EUR_`) and key socioeconomic variables such as income groupings, household type, region, and gender of the household reference person.
#' - `epf_hgm`: Member-level data with individual characteristics of household members (e.g., age, gender, education).
#' - `epf_hc`: Household-level data containing consumption quantities (physical amounts) by COICOP category.
#'
#' @section Main socioeconomic variables in `epf_hg`:
#' - `QUINTILE`, `DECILE`, `VENTILE`, `PERCENTILE`: Income distribution indicators.
#' - `REGION`: Autonomous Community of residence (NUTS2).
#' - `MUNISIZE`: Municipality size (`>10.000`, `<10.000`).
#' - `ZONE`: Area type (`Urban`, `Semi-urban`, `Rural`).
#' - `HHTYPE`: Household type (e.g., `Couples with children`, `Single parent`, `Elderly alone`).
#' - `CHILDREN`: Children in the household (`No children`, `With children`, `Large family`).
#' - `POVERTY`: Poverty risk status (`At risk`, `No risk`).
#' - `GENDERRP`: Gender of the reference person (`Man`, `Woman`).
#' - `FEMDEGREE`: Feminization degree of the household (FD1 to FD5).
#' - `AGERP`: Age group of the reference person (`Young`, `Adult`, `Elder`).
#' - `COUNTRYRP`: Country of birth of the reference person (`Spain`, `EU27`, `Other Europe`, `Rest of world`).
#' - `STUDIESRP`: Education level (`Without studies` to `Higher education`).
#' - `PROFESSIONALSRP`: Professional status (`Employee`, `Self-employed`, `Employer`, `Other`, `Not apply`).
#' - `CONTTYPERP`: Employment contract type (`Indefinite`, `Temporary`, `Not apply`).
#' - `WORKDAYRP`: Working hours (`Full time`, `Part time`, `Not apply`).
#' - `REGMR`: Tenure status of main residence (`Ownership`, `Rented`, `Relinquish`).
#'
#' @source medusa based on Instituto Nacional de Estadística (INE). Full survey documentation available at: \url{https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176806&menu=resultados&idp=1254735976608#_tabs-1254736195147}
#'
#' @format .RData
"epf_list_2016"

#' Cleaned Household Budget Survey (HBS) microdata for 2017
#'
#' This object contains cleaned microdata from the 2017 wave of the Spanish Household Budget Survey (EPF, Encuesta de Presupuestos Familiares), harmonized and prepared for use with the `medusa` package.
#'
#' The data are structured as a named list with the following main components:
#'
#' - `epf_hg`: Household-level data including consumption expenditure (COICOP categories prefixed with `EUR_`) and key socioeconomic variables such as income groupings, household type, region, and gender of the household reference person.
#' - `epf_hgm`: Member-level data with individual characteristics of household members (e.g., age, gender, education).
#' - `epf_hc`: Household-level data containing consumption quantities (physical amounts) by COICOP category.
#'
#' @section Main socioeconomic variables in `epf_hg`:
#' - `QUINTILE`, `DECILE`, `VENTILE`, `PERCENTILE`: Income distribution indicators.
#' - `REGION`: Autonomous Community of residence (NUTS2).
#' - `MUNISIZE`: Municipality size (`>10.000`, `<10.000`).
#' - `ZONE`: Area type (`Urban`, `Semi-urban`, `Rural`).
#' - `HHTYPE`: Household type (e.g., `Couples with children`, `Single parent`, `Elderly alone`).
#' - `CHILDREN`: Children in the household (`No children`, `With children`, `Large family`).
#' - `POVERTY`: Poverty risk status (`At risk`, `No risk`).
#' - `GENDERRP`: Gender of the reference person (`Man`, `Woman`).
#' - `FEMDEGREE`: Feminization degree of the household (FD1 to FD5).
#' - `AGERP`: Age group of the reference person (`Young`, `Adult`, `Elder`).
#' - `COUNTRYRP`: Country of birth of the reference person (`Spain`, `EU27`, `Other Europe`, `Rest of world`).
#' - `STUDIESRP`: Education level (`Without studies` to `Higher education`).
#' - `PROFESSIONALSRP`: Professional status (`Employee`, `Self-employed`, `Employer`, `Other`, `Not apply`).
#' - `CONTTYPERP`: Employment contract type (`Indefinite`, `Temporary`, `Not apply`).
#' - `WORKDAYRP`: Working hours (`Full time`, `Part time`, `Not apply`).
#' - `REGMR`: Tenure status of main residence (`Ownership`, `Rented`, `Relinquish`).
#'
#' @source medusa based on Instituto Nacional de Estadística (INE). Full survey documentation available at: \url{https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176806&menu=resultados&idp=1254735976608#_tabs-1254736195147}
#'
#' @format .RData
"epf_list_2017"

#' Cleaned Household Budget Survey (HBS) microdata for 2018
#'
#' This object contains cleaned microdata from the 2018 wave of the Spanish Household Budget Survey (EPF, Encuesta de Presupuestos Familiares), harmonized and prepared for use with the `medusa` package.
#'
#' The data are structured as a named list with the following main components:
#'
#' - `epf_hg`: Household-level data including consumption expenditure (COICOP categories prefixed with `EUR_`) and key socioeconomic variables such as income groupings, household type, region, and gender of the household reference person.
#' - `epf_hgm`: Member-level data with individual characteristics of household members (e.g., age, gender, education).
#' - `epf_hc`: Household-level data containing consumption quantities (physical amounts) by COICOP category.
#'
#' @section Main socioeconomic variables in `epf_hg`:
#' - `QUINTILE`, `DECILE`, `VENTILE`, `PERCENTILE`: Income distribution indicators.
#' - `REGION`: Autonomous Community of residence (NUTS2).
#' - `MUNISIZE`: Municipality size (`>10.000`, `<10.000`).
#' - `ZONE`: Area type (`Urban`, `Semi-urban`, `Rural`).
#' - `HHTYPE`: Household type (e.g., `Couples with children`, `Single parent`, `Elderly alone`).
#' - `CHILDREN`: Children in the household (`No children`, `With children`, `Large family`).
#' - `POVERTY`: Poverty risk status (`At risk`, `No risk`).
#' - `GENDERRP`: Gender of the reference person (`Man`, `Woman`).
#' - `FEMDEGREE`: Feminization degree of the household (FD1 to FD5).
#' - `AGERP`: Age group of the reference person (`Young`, `Adult`, `Elder`).
#' - `COUNTRYRP`: Country of birth of the reference person (`Spain`, `EU27`, `Other Europe`, `Rest of world`).
#' - `STUDIESRP`: Education level (`Without studies` to `Higher education`).
#' - `PROFESSIONALSRP`: Professional status (`Employee`, `Self-employed`, `Employer`, `Other`, `Not apply`).
#' - `CONTTYPERP`: Employment contract type (`Indefinite`, `Temporary`, `Not apply`).
#' - `WORKDAYRP`: Working hours (`Full time`, `Part time`, `Not apply`).
#' - `REGMR`: Tenure status of main residence (`Ownership`, `Rented`, `Relinquish`).
#'
#' @source medusa based on Instituto Nacional de Estadística (INE). Full survey documentation available at: \url{https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176806&menu=resultados&idp=1254735976608#_tabs-1254736195147}
#'
#' @format .RData
"epf_list_2018"

#' Cleaned Household Budget Survey (HBS) microdata for 2019
#'
#' This object contains cleaned microdata from the 2019 wave of the Spanish Household Budget Survey (EPF, Encuesta de Presupuestos Familiares), harmonized and prepared for use with the `medusa` package.
#'
#' The data are structured as a named list with the following main components:
#'
#' - `epf_hg`: Household-level data including consumption expenditure (COICOP categories prefixed with `EUR_`) and key socioeconomic variables such as income groupings, household type, region, and gender of the household reference person.
#' - `epf_hgm`: Member-level data with individual characteristics of household members (e.g., age, gender, education).
#' - `epf_hc`: Household-level data containing consumption quantities (physical amounts) by COICOP category.
#'
#' @section Main socioeconomic variables in `epf_hg`:
#' - `QUINTILE`, `DECILE`, `VENTILE`, `PERCENTILE`: Income distribution indicators.
#' - `REGION`: Autonomous Community of residence (NUTS2).
#' - `MUNISIZE`: Municipality size (`>10.000`, `<10.000`).
#' - `ZONE`: Area type (`Urban`, `Semi-urban`, `Rural`).
#' - `HHTYPE`: Household type (e.g., `Couples with children`, `Single parent`, `Elderly alone`).
#' - `CHILDREN`: Children in the household (`No children`, `With children`, `Large family`).
#' - `POVERTY`: Poverty risk status (`At risk`, `No risk`).
#' - `GENDERRP`: Gender of the reference person (`Man`, `Woman`).
#' - `FEMDEGREE`: Feminization degree of the household (FD1 to FD5).
#' - `AGERP`: Age group of the reference person (`Young`, `Adult`, `Elder`).
#' - `COUNTRYRP`: Country of birth of the reference person (`Spain`, `EU27`, `Other Europe`, `Rest of world`).
#' - `STUDIESRP`: Education level (`Without studies` to `Higher education`).
#' - `PROFESSIONALSRP`: Professional status (`Employee`, `Self-employed`, `Employer`, `Other`, `Not apply`).
#' - `CONTTYPERP`: Employment contract type (`Indefinite`, `Temporary`, `Not apply`).
#' - `WORKDAYRP`: Working hours (`Full time`, `Part time`, `Not apply`).
#' - `REGMR`: Tenure status of main residence (`Ownership`, `Rented`, `Relinquish`).
#'
#' @source medusa based on Instituto Nacional de Estadística (INE). Full survey documentation available at: \url{https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176806&menu=resultados&idp=1254735976608#_tabs-1254736195147}
#'
#' @format .RData
"epf_list_2019"

#' Cleaned Household Budget Survey (HBS) microdata for 2020
#'
#' This object contains cleaned microdata from the 2020 wave of the Spanish Household Budget Survey (EPF, Encuesta de Presupuestos Familiares), harmonized and prepared for use with the `medusa` package.
#'
#' The data are structured as a named list with the following main components:
#'
#' - `epf_hg`: Household-level data including consumption expenditure (COICOP categories prefixed with `EUR_`) and key socioeconomic variables such as income groupings, household type, region, and gender of the household reference person.
#' - `epf_hgm`: Member-level data with individual characteristics of household members (e.g., age, gender, education).
#' - `epf_hc`: Household-level data containing consumption quantities (physical amounts) by COICOP category.
#'
#' @section Main socioeconomic variables in `epf_hg`:
#' - `QUINTILE`, `DECILE`, `VENTILE`, `PERCENTILE`: Income distribution indicators.
#' - `REGION`: Autonomous Community of residence (NUTS2).
#' - `MUNISIZE`: Municipality size (`>10.000`, `<10.000`).
#' - `ZONE`: Area type (`Urban`, `Semi-urban`, `Rural`).
#' - `HHTYPE`: Household type (e.g., `Couples with children`, `Single parent`, `Elderly alone`).
#' - `CHILDREN`: Children in the household (`No children`, `With children`, `Large family`).
#' - `POVERTY`: Poverty risk status (`At risk`, `No risk`).
#' - `GENDERRP`: Gender of the reference person (`Man`, `Woman`).
#' - `FEMDEGREE`: Feminization degree of the household (FD1 to FD5).
#' - `AGERP`: Age group of the reference person (`Young`, `Adult`, `Elder`).
#' - `COUNTRYRP`: Country of birth of the reference person (`Spain`, `EU27`, `Other Europe`, `Rest of world`).
#' - `STUDIESRP`: Education level (`Without studies` to `Higher education`).
#' - `PROFESSIONALSRP`: Professional status (`Employee`, `Self-employed`, `Employer`, `Other`, `Not apply`).
#' - `CONTTYPERP`: Employment contract type (`Indefinite`, `Temporary`, `Not apply`).
#' - `WORKDAYRP`: Working hours (`Full time`, `Part time`, `Not apply`).
#' - `REGMR`: Tenure status of main residence (`Ownership`, `Rented`, `Relinquish`).
#'
#' @source medusa based on Instituto Nacional de Estadística (INE). Full survey documentation available at: \url{https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176806&menu=resultados&idp=1254735976608#_tabs-1254736195147}
#'
#' @format .RData
"epf_list_2020"

#' Cleaned Household Budget Survey (HBS) microdata for 2021
#'
#' This object contains cleaned microdata from the 2021 wave of the Spanish Household Budget Survey (EPF, Encuesta de Presupuestos Familiares), harmonized and prepared for use with the `medusa` package.
#'
#' The data are structured as a named list with the following main components:
#'
#' - `epf_hg`: Household-level data including consumption expenditure (COICOP categories prefixed with `EUR_`) and key socioeconomic variables such as income groupings, household type, region, and gender of the household reference person.
#' - `epf_hgm`: Member-level data with individual characteristics of household members (e.g., age, gender, education).
#' - `epf_hc`: Household-level data containing consumption quantities (physical amounts) by COICOP category.
#'
#' @section Main socioeconomic variables in `epf_hg`:
#' - `QUINTILE`, `DECILE`, `VENTILE`, `PERCENTILE`: Income distribution indicators.
#' - `REGION`: Autonomous Community of residence (NUTS2).
#' - `MUNISIZE`: Municipality size (`>10.000`, `<10.000`).
#' - `ZONE`: Area type (`Urban`, `Semi-urban`, `Rural`).
#' - `HHTYPE`: Household type (e.g., `Couples with children`, `Single parent`, `Elderly alone`).
#' - `CHILDREN`: Children in the household (`No children`, `With children`, `Large family`).
#' - `POVERTY`: Poverty risk status (`At risk`, `No risk`).
#' - `GENDERRP`: Gender of the reference person (`Man`, `Woman`).
#' - `FEMDEGREE`: Feminization degree of the household (FD1 to FD5).
#' - `AGERP`: Age group of the reference person (`Young`, `Adult`, `Elder`).
#' - `COUNTRYRP`: Country of birth of the reference person (`Spain`, `EU27`, `Other Europe`, `Rest of world`).
#' - `STUDIESRP`: Education level (`Without studies` to `Higher education`).
#' - `PROFESSIONALSRP`: Professional status (`Employee`, `Self-employed`, `Employer`, `Other`, `Not apply`).
#' - `CONTTYPERP`: Employment contract type (`Indefinite`, `Temporary`, `Not apply`).
#' - `WORKDAYRP`: Working hours (`Full time`, `Part time`, `Not apply`).
#' - `REGMR`: Tenure status of main residence (`Ownership`, `Rented`, `Relinquish`).
#'
#' @source medusa based on Instituto Nacional de Estadística (INE). Full survey documentation available at: \url{https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176806&menu=resultados&idp=1254735976608#_tabs-1254736195147}
#'
#' @format .RData
"epf_list_2021"

#' Mapping to rename socioeconomic variables
#'
#' @source medusa
#' @format .csv
"mapping"

#' List to add coicop categories
#'
#' @source medusa
#' @format .csv
"lists"

#' List to add coicop categories for 2006
#'
#' @source medusa
#' @format .csv
"coicop_2006"

#' List to add coicop categories for 2007
#'
#' @source medusa
#' @format .csv
"coicop_2007"

#' List to add coicop categories for 2008
#'
#' @source medusa
#' @format .csv
"coicop_2008"

#' List to add coicop categories for 2009
#'
#' @source medusa
#' @format .csv
"coicop_2009"

#' List to add coicop categories for 2010
#'
#' @source medusa
#' @format .csv
"coicop_2010"

#' List to add coicop categories for 2011
#'
#' @source medusa
#' @format .csv
"coicop_2011"

#' List to add coicop categories for 2012
#'
#' @source medusa
#' @format .csv
"coicop_2012"

#' List to add coicop categories for 2013
#'
#' @source medusa
#' @format .csv
"coicop_2013"

#' List to add coicop categories for 2014
#'
#' @source medusa
#' @format .csv
"coicop_2014"

#' List to add coicop categories for 2015
#'
#' @source medusa
#' @format .csv
"coicop_2015"

#' List to add coicop categories for 2016
#'
#' @source medusa
#' @format .csv
"coicop_2016"

#' List to add coicop categories for 2017
#'
#' @source medusa
#' @format .csv
"coicop_2017"

#' List to add coicop categories for 2018
#'
#' @source medusa
#' @format .csv
"coicop_2018"

#' List to add coicop categories for 2019
#'
#' @source medusa
#' @format .csv
"coicop_2019"

#' List to add coicop categories for 2020
#'
#' @source medusa
#' @format .csv
"coicop_2020"

#' List to add coicop categories for 2021
#'
#' @source medusa
#' @format .csv
"coicop_2021"


#' File containing household expenditure by COICOP category from the national accounts
#'
#' @source medusa
#' @format .csv
"gcfhogares95_22"

#' Example csv file in which the change in prices must be introduced to apply a price shock
#'
#' @source medusa
#' @format .csv
"shocks"

#' List containing the socioeconomic and demographic categories for the calculation of distributional impacts
#'
#' @source medusa
#' @format .csv
"categories"

#' List containing the socioeconomic and demographic categories for the calculation of intersectional distributional impacts
#'
#' @source medusa
#' @format .csv
"is_categories"

#' List containing long names for the socioeconomic and demographic categories to introduce them on the axes of the graphs
#'
#' @source medusa
#' @format .csv
"graph_labels"
