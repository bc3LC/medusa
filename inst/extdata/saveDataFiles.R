# Converting raw data into package data
library(usethis)
library(magrittr)

rawDataFolder = paste0(here::here(),"/inst/extdata/")

# epf_list_2006
epf_list_2006 = get(load(paste0(rawDataFolder,"epf_lists/epf_list_2006.RData")))
use_data(epf_list_2006, overwrite=T)

# epf_list_2007
epf_list_2007 = get(load(paste0(rawDataFolder,"epf_lists/epf_list_2007.RData")))
use_data(epf_list_2007, overwrite=T)

# epf_list_2008
epf_list_2008 = get(load(paste0(rawDataFolder,"epf_lists/epf_list_2008.RData")))
use_data(epf_list_2008, overwrite=T)

# epf_list_2009
epf_list_2009 = get(load(paste0(rawDataFolder,"epf_lists/epf_list_2009.RData")))
use_data(epf_list_2009, overwrite=T)

# epf_list_2010
epf_list_2010 = get(load(paste0(rawDataFolder,"epf_lists/epf_list_2010.RData")))
use_data(epf_list_2010, overwrite=T)

# epf_list_2011
epf_list_2011 = get(load(paste0(rawDataFolder,"epf_lists/epf_list_2011.RData")))
use_data(epf_list_2011, overwrite=T)

# epf_list_2012
epf_list_2012 = get(load(paste0(rawDataFolder,"epf_lists/epf_list_2012.RData")))
use_data(epf_list_2012, overwrite=T)

# epf_list_2013
epf_list_2013 = get(load(paste0(rawDataFolder,"epf_lists/epf_list_2013.RData")))
use_data(epf_list_2013, overwrite=T)

# epf_list_2014
epf_list_2014 = get(load(paste0(rawDataFolder,"epf_lists/epf_list_2014.RData")))
use_data(epf_list_2014, overwrite=T)

# epf_list_2015
epf_list_2015 = get(load(paste0(rawDataFolder,"/epf_lists/epf_list_2015.RData")))
use_data(epf_list_2015, overwrite=T)

# epf_list_2016
epf_list_2016 = get(load(paste0(rawDataFolder,"/epf_lists/epf_list_2016.RData")))
use_data(epf_list_2016, overwrite=T)

# epf_list_2017
epf_list_2017 = get(load(paste0(rawDataFolder,"/epf_lists/epf_list_2017.RData")))
use_data(epf_list_2017, overwrite=T)

# epf_list_2018
epf_list_2018 = get(load(paste0(rawDataFolder,"/epf_lists/epf_list_2018.RData")))
use_data(epf_list_2018, overwrite=T)

# epf_list_2019
epf_list_2019 = get(load(paste0(rawDataFolder,"/epf_lists/epf_list_2019.RData")))
use_data(epf_list_2019, overwrite=T)

# epf_list_2020
epf_list_2020 = get(load(paste0(rawDataFolder,"/epf_lists/epf_list_2020.RData")))
use_data(epf_list_2020, overwrite=T)

# epf_list_2021
epf_list_2021 = get(load(paste0(rawDataFolder,"/epf_lists/epf_list_2021.RData")))
use_data(epf_list_2021, overwrite=T)

# mapping
mapping = read.csv(paste0(rawDataFolder,"mapping.csv"),header=T, fileEncoding = "UTF-8-BOM") %>%
  # substitue all empty items for NA
  dplyr::mutate_all(~dplyr::na_if(., ""))
use_data(mapping, overwrite=T)

# lists
lists = read.csv(paste0(rawDataFolder,"lists.csv"),header=T)
use_data(lists, overwrite=T)

# coicop_2006
coicop_2006 = read.csv(paste0(rawDataFolder,"coicop_2006.csv"),header=T, fileEncoding = "UTF-8-BOM")
use_data(coicop_2006, overwrite=T)

# coicop_2007
coicop_2007 = read.csv(paste0(rawDataFolder,"coicop_2007.csv"),header=T, fileEncoding = "UTF-8-BOM")
use_data(coicop_2007, overwrite=T)

# coicop_2008
coicop_2008 = read.csv(paste0(rawDataFolder,"coicop_2008.csv"),header=T, fileEncoding = "UTF-8-BOM")
use_data(coicop_2008, overwrite=T)

# coicop_2009
coicop_2009 = read.csv(paste0(rawDataFolder,"coicop_2009.csv"),header=T, fileEncoding = "UTF-8-BOM")
use_data(coicop_2009, overwrite=T)

# coicop_2010
coicop_2010 = read.csv(paste0(rawDataFolder,"coicop_2010.csv"),header=T, fileEncoding = "UTF-8-BOM")
use_data(coicop_2010, overwrite=T)

# coicop_2011
coicop_2011 = read.csv(paste0(rawDataFolder,"coicop_2011.csv"),header=T, fileEncoding = "UTF-8-BOM")
use_data(coicop_2011, overwrite=T)

# coicop_2012
coicop_2012 = read.csv(paste0(rawDataFolder,"coicop_2012.csv"),header=T, fileEncoding = "UTF-8-BOM")
use_data(coicop_2012, overwrite=T)

# coicop_2013
coicop_2013 = read.csv(paste0(rawDataFolder,"coicop_2013.csv"),header=T, fileEncoding = "UTF-8-BOM")
use_data(coicop_2013, overwrite=T)

# coicop_2014
coicop_2014 = read.csv(paste0(rawDataFolder,"coicop_2014.csv"),header=T, fileEncoding = "UTF-8-BOM")
use_data(coicop_2014, overwrite=T)

# coicop_2015
coicop_2015 = read.csv(paste0(rawDataFolder,"coicop_2015.csv"),header=T, fileEncoding = "UTF-8-BOM")
use_data(coicop_2015, overwrite=T)

# coicop_2016
coicop_2016 = read.csv(paste0(rawDataFolder,"coicop_2016.csv"),header=T, fileEncoding = "UTF-8-BOM")
use_data(coicop_2016, overwrite=T)

# coicop_2017
coicop_2017 = read.csv(paste0(rawDataFolder,"coicop_2017.csv"),header=T, fileEncoding = "UTF-8-BOM")
use_data(coicop_2017, overwrite=T)

# coicop_2018
coicop_2018 = read.csv(paste0(rawDataFolder,"coicop_2018.csv"),header=T, fileEncoding = "UTF-8-BOM")
use_data(coicop_2018, overwrite=T)

# coicop_2019
coicop_2019 = read.csv(paste0(rawDataFolder,"coicop_2019.csv"),header=T, fileEncoding = "UTF-8-BOM")
use_data(coicop_2019, overwrite=T)

# coicop_2020
coicop_2020 = read.csv(paste0(rawDataFolder,"coicop_2020.csv"),header=T, fileEncoding = "UTF-8-BOM")
use_data(coicop_2020, overwrite=T)

# coicop_2021
coicop_2021 = read.csv(paste0(rawDataFolder,"coicop_2021.csv"),header=T, fileEncoding = "UTF-8-BOM")
use_data(coicop_2021, overwrite=T)

# gcfhogares95_22
gcfhogares95_22 = read.csv(paste0(rawDataFolder,"gcfhogares95_22.csv"),header=T, fileEncoding = "UTF-8-BOM") %>%
  dplyr::rename_with(~gsub("^X", "", .x), starts_with("X"))
use_data(gcfhogares95_22, overwrite=T)

# shocks
shocks = read.csv(paste0(rawDataFolder,"shocks.csv"),header=T)
use_data(shocks, overwrite=T)

# categories
categories = read.csv(paste0(rawDataFolder,"categories.csv"),header=T)
use_data(categories, overwrite=T)

# is_categories
is_categories = read.csv(paste0(rawDataFolder,"is_categories.csv"),header=T)
use_data(is_categories, overwrite=T)

# graph_labels
graph_labels = read.csv(paste0(rawDataFolder,"graph_labels.csv"),header=T)
use_data(graph_labels, overwrite=T)



#################################################################################
# Data medusa EU

rawDataFolder = paste0(here::here(),"/inst/extdata/eu/")

# av_country
av_country = read.csv(paste0(rawDataFolder,"av_country.csv"),header=T)
use_data(av_country, overwrite=T)

# variables_h
variables_h = read.csv(paste0(rawDataFolder,"variables_h.csv"),header=T)
use_data(variables_h, overwrite=T)

# variables_m_2015
variables_m_2015 = read.csv(paste0(rawDataFolder,"variables_m_2015.csv"),header=T)
use_data(variables_m_2015, overwrite=T)

# variables_m_2020
variables_m_2020 = read.csv(paste0(rawDataFolder,"variables_m_2020.csv"),header=T)
use_data(variables_m_2020, overwrite=T)
