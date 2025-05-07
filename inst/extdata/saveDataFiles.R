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

# mapping GCAM - MEDUSA COICOP categories
mapping_gcam_medusa <- read.csv(paste0(rawDataFolder, "mapping_gcam_medusa.csv"), header = T)
use_data(mapping_gcam_medusa, overwrite=T)

# coicop codes list
all_coicop <- c(
  "CP01", "CP011", "CP0111", "CP01111", "CP01112", "CP01113", "CP01114", "CP01115", "CP01116", "CP01117", "CP01118",
  "CP0112", "CP01121", "CP01122", "CP01123", "CP01124", "CP01125", "CP01126", "CP01127", "CP01128",
  "CP0113", "CP01131", "CP01132", "CP01133", "CP01134", "CP01135", "CP01136",
  "CP0114", "CP01141", "CP01142", "CP01143", "CP01144", "CP01145", "CP01146", "CP01147",
  "CP0115", "CP01151", "CP01152", "CP01153", "CP01154", "CP01155",
  "CP0116", "CP01161", "CP01162", "CP01163", "CP01164",
  "CP0117", "CP01171", "CP01172", "CP01173", "CP01174", "CP01175", "CP01176",
  "CP0118", "CP01181", "CP01182", "CP01183", "CP01184", "CP01185", "CP01186",
  "CP0119", "CP01191", "CP01192", "CP01193", "CP01194", "CP01199",
  "CP012", "CP0121", "CP01211", "CP01212", "CP01213",
  "CP0122", "CP01221", "CP01222", "CP01223",
  "CP02", "CP021", "CP0211", "CP02111", "CP02112",
  "CP0212", "CP02121", "CP02122", "CP02123", "CP02124",
  "CP0213", "CP02131", "CP02132", "CP02133", "CP02134",
  "CP022", "CP0220", "CP02201", "CP02202", "CP02203",
  "CP023",
  "CP03", "CP031", "CP0311", "CP03110", "CP0312", "CP03121", "CP03122", "CP03123",
  "CP0313", "CP03131", "CP03132",
  "CP0314", "CP03141", "CP03142",
  "CP032", "CP0321", "CP03211", "CP03212", "CP03213",
  "CP0322", "CP03220",
  "CP04", "CP041", "CP0411", "CP04110", "CP0412", "CP04121", "CP04122",
  "CP042", "CP0421", "CP04210", "CP0422", "CP04220",
  "CP043", "CP0431", "CP04310", "CP0432", "CP04321", "CP04322", "CP04323", "CP04324", "CP04325", "CP04329",
  "CP044", "CP0441", "CP04410", "CP0442", "CP04420", "CP0443", "CP04430", "CP0444", "CP04441", "CP04442", "CP04449",
  "CP045", "CP0451", "CP04510", "CP0452", "CP04521", "CP04522", "CP0453", "CP04530", "CP0454", "CP04541", "CP04549",
  "CP0455", "CP04550",
  "CP05", "CP051", "CP0511", "CP05111", "CP05112", "CP05113", "CP05119",
  "CP0512", "CP05121", "CP05122", "CP05123", "CP0513", "CP05130",
  "CP052", "CP0520", "CP05201", "CP05202", "CP05203", "CP05204", "CP05209",
  "CP053", "CP0531", "CP05311", "CP05312", "CP05313", "CP05314", "CP05315", "CP05319",
  "CP0532", "CP05321", "CP05322", "CP05323", "CP05324", "CP05329",
  "CP0533", "CP05330",
  "CP054", "CP0540", "CP05401", "CP05402", "CP05403", "CP05404",
  "CP055", "CP0551", "CP05511", "CP05512", "CP0552", "CP05521", "CP05522", "CP05523",
  "CP056", "CP0561", "CP05611", "CP05612", "CP0562", "CP05621", "CP05622", "CP05623", "CP05629",
  "CP06", "CP061", "CP0611", "CP06110", "CP0612", "CP06121", "CP06129",
  "CP0613", "CP06131", "CP06132", "CP06133", "CP06139",
  "CP062", "CP0621", "CP06211", "CP06212", "CP0622", "CP06220",
  "CP0623", "CP06231", "CP06232", "CP06239",
  "CP063", "CP0630", "CP06300",
  "CP07", "CP071", "CP0711", "CP07111", "CP07112", "CP0712", "CP07120", "CP0713", "CP07130", "CP0714", "CP07140",
  "CP072", "CP0721", "CP07211", "CP07212", "CP07213", "CP0722", "CP07221", "CP07222", "CP07223", "CP07224",
  "CP0723", "CP07230", "CP0724", "CP07241", "CP07242", "CP07243",
  "CP073", "CP0731", "CP07311", "CP07312", "CP0732", "CP07321", "CP07322",
  "CP0733", "CP07331", "CP07332", "CP0734", "CP07341", "CP07342",
  "CP0735", "CP07350", "CP0736", "CP07361", "CP07362", "CP07369",
  "CP08", "CP081", "CP0810", "CP08101", "CP08109",
  "CP082", "CP0820", "CP08201", "CP08202", "CP08203", "CP08204",
  "CP083", "CP0830", "CP08301", "CP08302", "CP08303", "CP08304", "CP08305",
  "CP09", "CP091", "CP0911", "CP09111", "CP09112", "CP09113", "CP09119",
  "CP0912", "CP09121", "CP09122", "CP09123",
  "CP0913", "CP09131", "CP09132", "CP09133", "CP09134",
  "CP0914", "CP09141", "CP09142", "CP09149", "CP0915", "CP09150",
  "CP092", "CP0921", "CP09211", "CP09212", "CP09213", "CP09214", "CP09215",
  "CP0922", "CP09221", "CP09222",
  "CP0923", "CP09230",
  "CP093", "CP0931", "CP09311", "CP09312", "CP0932", "CP09321", "CP09322", "CP09323",
  "CP0933", "CP09331", "CP09332",
  "CP0934", "CP09341", "CP09342",
  "CP0935", "CP09350",
  "CP094", "CP0941", "CP09411", "CP09412", "CP0942", "CP09421", "CP09422", "CP09423", "CP09424", "CP09425", "CP09429",
  "CP0943",
  "CP095", "CP0951", "CP09511", "CP09512", "CP09513", "CP09514",
  "CP0952", "CP09521", "CP09522",
  "CP0953", "CP09530",
  "CP0954", "CP09541", "CP09549",
  "CP096", "CP0960", "CP09601", "CP09602",
  "CP10", "CP101", "CP1010", "CP10101", "CP10102",
  "CP102", "CP1020", "CP10200",
  "CP103", "CP1030", "CP10300",
  "CP104", "CP1040", "CP10400",
  "CP105", "CP1050", "CP10500",
  "CP11", "CP111", "CP1111", "CP11111", "CP11112",
  "CP1112", "CP11120",
  "CP112", "CP1120", "CP11201", "CP11202", "CP11203",
  "CP12", "CP121", "CP1211", "CP12111", "CP12112", "CP12113",
  "CP1212", "CP12121", "CP12122",
  "CP1213", "CP12131", "CP12132",
  "CP122",
  "CP123", "CP1231", "CP12311", "CP12312", "CP12313",
  "CP1232", "CP12321", "CP12322", "CP12323", "CP12329",
  "CP124", "CP1240", "CP12401", "CP12402", "CP12403", "CP12404",
  "CP125", "CP1252", "CP12520", "CP1253", "CP12531", "CP12532",
  "CP1254", "CP12541", "CP12542",
  "CP1255", "CP12550",
  "CP126", "CP1262", "CP12621", "CP12622",
  "CP127", "CP1270", "CP12701", "CP12702", "CP12703", "CP12704"
)
use_data(all_coicop, overwrite=T)

