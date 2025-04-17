library(usethis)
library(magrittr)
options(dplyr.summarise.inform = FALSE)


#' rename_values
#'
#' Function to rename the codified values of the dataset to the meaningful
#' values detailed in the mapping included in the package.
#' @param data dataset to be standardized.
#' @param current_var column name to be standardized.
#' @importFrom dplyr %>%
#' @return a dataset with labels renamed based in the mapping included in the package.
#' @export
rename_values = function(data, current_var) {
  exchange_data = mapping %>%                                                # brings together the two df (mapping and data)
    dplyr::filter(VAR_EN == current_var) %>%                                 # but only takes the part we are interested in (only when the value of the variable is equal to current_var).
    dplyr::select(value, NAME) %>%
    dplyr::distinct()

  if (current_var != "NMIEMB" | (current_var == "NMIEMB" & sum(is.na(unique(exchange_data$value))) == 0) ) {                     # we look if there is any na in value and if so the below does not apply (to avoid errors with NMIEMB).
    data = data %>%
      dplyr::rename(value = {{ current_var }}) %>%                             # rename the column (variable name) to value
      dplyr::mutate(value = as.character(value)) %>%                           # convert to a character
      dplyr::left_join(exchange_data, by = "value") %>%                        # it keeps only the columns we are interested in (value and name).
      dplyr::select(-value) %>%                                                # removes the value column
      dplyr::rename_with(~current_var, 'NAME')                                 # rename to current_var
  }


  return(data)
}


#' standardize
#'
#' Function to standarize data names.
#' @param data dataset to be standardized.
#' @importFrom dplyr %>%
#' @return a dataset with the variables and labels renamed based in the mapping included in the package.
#' @export
standardize <- function(data) {
  # rename columns
  old_names = colnames(data)                                                # creates a vector with the column names
  new_names = dplyr::left_join(data.frame(VAR_EPF = old_names),             # the vector of column names is converted to a df and the column name is VAR_EPF
                               mapping %>%
                                 dplyr::select(VAR_EPF, VAR_EN) %>%         # within the mapping selects only the columns we are interested in
                                 dplyr::distinct(.),                        # duplicate remove
                               by = 'VAR_EPF') %>%                          # left join in function to var_epf
    dplyr::mutate(VAR_EN = ifelse(is.na(VAR_EN), VAR_EPF, VAR_EN)) %>%      # when it is na you get var_epf and if it is not you get var
    dplyr::pull(VAR_EN)                                                     # only keep the var column (as a vector)
  colnames(data) = new_names                                                # assign to the column names the new vector

  # rename values' codes to values' names for all items whose name is in the
  # renamed mapping's column and have not NA values
  ccitems = mapping %>%
    dplyr::filter(VAR_EN %in% intersect(unique(mapping$VAR_EN), new_names),
                  !is.na(value)) %>%
    dplyr::pull(VAR_EN) %>%
    unique()
  for (cc in ccitems) {                                                     # for all columns where variables can be mapped we overwrite the standardised dataset (with rename_value function)
    data = rename_values(data, cc)
  }

  return(data)
}


#' weighted.quantile
#'
#' Function to calculate weighted quantile. Extracted from package spatstat.geom v3.2-5
#' @param x Data values. A vector pf numeric values, for which the median or quantiles are required.
#' @param w Weights. A vector of nonnegative numbers, of the same lenght as x.
#' @param probs Probabilities for which the quantiles should be computed. A numeric vector of values between 0 and 1.
#' @param na.rm Logical. Whether to ignore NA values.
#' @param type Integer specifying the rule for calculating the median or quantile, corresponding to the rules available for quantile. The only valid choices are type=1, 2 or 4.
#' @param collapse Research use only.
#' @export
weighted.quantile <- function(x, w, probs=seq(0,1,0.25), na.rm=TRUE, type=4, collapse=TRUE) {
  x <- as.numeric(as.vector(x))
  w <- as.numeric(as.vector(w))
  if(length(x) == 0)
    stop("No data given")
  stopifnot(length(x) == length(w))
  if(is.na(m <- match(type, c(1,2,4))))
    stop("Argument 'type' must equal 1, 2 or 4", call.=FALSE)
  type <- c(1,2,4)[m]
  if(anyNA(x) || anyNA(w)) {
    ok <- !(is.na(x) | is.na(w))
    x <- x[ok]
    w <- w[ok]
  }
  if(length(x) == 0)
    stop("At least one non-NA value is required")
  stopifnot(all(w >= 0))
  if(all(w == 0)) stop("All weights are zero", call.=FALSE)
  #'
  oo <- order(x)
  x <- x[oo]
  w <- w[oo]
  Fx <- cumsum(w)/sum(w)
  #'
  if(collapse && anyDuplicated(x)) {
    dup <- rev(duplicated(rev(x)))
    x <- x[!dup]
    Fx <- Fx[!dup]
  }
  #'
  if(length(x) > 1) {
    out <- switch(as.character(type),
                  "1" = approx(Fx, x, xout=probs, ties="ordered", rule=2,
                               method="constant", f=1),
                  "2" = approx(Fx, x, xout=probs, ties="ordered", rule=2,
                               method="constant", f=1/2),
                  "4" = approx(Fx, x, xout=probs, ties="ordered", rule=2,
                               method="linear"))
    result <- out$y
  } else {
    result <- rep.int(x, length(probs))
  }
  names(result) <- paste0(format(100 * probs, trim = TRUE), "%")
  return(result)
}


#' weighted.median
#'
#' Function to calculate weighted median. Extracted from package spatstat.geom v3.2-5
#' @param x Data values. A vector pf numeric values, for which the median or quantiles are required.
#' @param w Weights. A vector of nonnegative numbers, of the same lenght as x.
#' @param na.rm Logical. Whether to ignore NA values.
#' @param type Integer specifying the rule for calculating the median or quantile, corresponding to the rules available for quantile. The only valid choices are type=1, 2 or 4.
#' @param collapse Research use only.
#' @export
weighted.median <- function(x, w, na.rm=TRUE, type=2, collapse=TRUE) {
  unname(weighted.quantile(x, probs=0.5, w=w, na.rm=na.rm, type=type, collapse=collapse))
}

#' id_ep1
#'
#' Function to identify energy poor households from 2016
#' @param data dataset with the data from the HBS.
#' @importFrom dplyr %>%
#' @return a dataset with HBS data where energy poor households are identified.
#' @export
id_ep1 <- function(data){

  # Calculate the variables needed for EP indices calculation
  data <- data %>%
    dplyr::mutate(endom = EUR_04511 + EUR_04521 + EUR_04523 + EUR_04531 + EUR_04541 + EUR_04548 + EUR_04549,  # domestic energy expenditure
                  endom_eq = endom/UC2,                         # equivalent domestic energy
                  total_eq = GASTOT/(FACTOR*UC2) ,              # equivalent total expenditure
                  share_endom = endom_eq/total_eq,              # share of domestic energy
                  exp_aec = total_eq - endom_eq,                # total expenditure after energy costs
                  exp_aehc = exp_aec - ((EUR_04110 + EUR_04210)/UC2))   # total expenditure after energy and housing costs

  # Calculate medians and thresholds
  data <- data %>%
    dplyr::mutate(med_sendom = weighted.median(share_endom, w= FACTOR, na.rm = TRUE),   # median of the share of domestic energy
                  med_endom  = weighted.median(endom_eq, w= FACTOR,  na.rm = TRUE),     # median of domestic energy expenditure
                  med_exp    = weighted.median(total_eq, w= FACTOR, na.rm = TRUE),      # income median (using expenditure as a better proxy of permanent income)
                  poverty_t  = med_exp*0.6 )                                            # poverty threshold

  # Calculate energy poverty indices
  data <- data %>%
    dplyr::mutate(IEP10PC     = base::ifelse(share_endom >= 0.10 , FACTOR, 0),                                                   # 10% index
                  ID_EP10PC   = base::ifelse(share_endom >= 0.10 , "Vulnerable", "No vulnerable"),                               # 10% ID
                  IEP2M       = base::ifelse(share_endom >= 2*med_sendom , FACTOR, 0 ),                                          # 2M index
                  ID_EP2M     = base::ifelse(share_endom >= 2*med_sendom , "Vulnerable", "No vulnerable"),                       # 2M ID
                  IEPHEP      = base::ifelse(endom_eq <= med_endom/2 , FACTOR, 0),                                               # HEP index
                  ID_EPHEP    = base::ifelse(endom_eq <= med_endom/2 , "Vulnerable", "No vulnerable"),                           # HEP ID
                  IEPHEP_LI   = base::ifelse(endom_eq <= med_endom/2 & exp_aec <= poverty_t, FACTOR, 0),                         # HEP_LI index
                  ID_EPHEP_LI = base::ifelse(endom_eq <= med_endom/2 & exp_aec <= poverty_t, "Vulnerable", "No vulnerable"),     # HEP_LI ID
                  IEPLIHC     = base::ifelse(endom_eq >= med_endom & exp_aec <= poverty_t, FACTOR, 0),                           # LIHC index
                  ID_EPLIHC   = base::ifelse(endom_eq >= med_endom & exp_aec <= poverty_t, "Vulnerable", "No vulnerable"))       # LIHC ID

  return(data)
}

#' id_ep2
#'
#' Function to identify energy poor households before 2016 (included)
#' @param data dataset with the data from the HBS.
#' @importFrom dplyr %>%
#' @return a dataset with HBS data where energy poor households are identified.
#' @export
id_ep2 <- function(data){

  # Calculate the variables needed for EP indices calculation
  data <- data %>%
    dplyr::mutate(endom = rowSums(dplyr::select(., any_of(c("EUR_04511", "EUR_04521", "EUR_04523", "EUR_04531", "EUR_04541", "EUR_04551"))), na.rm = TRUE),  # domestic energy expenditure
                  endom_eq = endom/UC2,                         # equivalent domestic energy
                  total_eq = GASTOT/(FACTOR*UC2) ,              # equivalent total expenditure
                  share_endom = endom_eq/total_eq,              # share of domestic energy
                  exp_aec = total_eq - endom_eq)                # total expenditure after energy costs

  # Calculate medians and thresholds
  data <- data %>%
    dplyr::mutate(med_sendom = weighted.median(share_endom, w= FACTOR, na.rm = TRUE),   # median of the share of domestic energy
                  med_endom  = weighted.median(endom_eq, w= FACTOR,  na.rm = TRUE),     # median of domestic energy expenditure
                  med_exp    = weighted.median(total_eq, w= FACTOR, na.rm = TRUE),      # income median (using expenditure as a better proxy of permanent income)
                  poverty_t  = med_exp*0.6 )                                            # poverty threshold

  # Calculate energy poverty indices
  data <- data %>%
    dplyr::mutate(IEP10PC     = base::ifelse(share_endom >= 0.10 , FACTOR, 0),                                                   # 10% index
                  ID_EP10PC   = base::ifelse(share_endom >= 0.10 , "Vulnerable", "No vulnerable"),                               # 10% ID
                  IEP2M       = base::ifelse(share_endom >= 2*med_sendom , FACTOR, 0 ),                                          # 2M index
                  ID_EP2M     = base::ifelse(share_endom >= 2*med_sendom , "Vulnerable", "No vulnerable"),                       # 2M ID
                  IEPHEP      = base::ifelse(endom_eq <= med_endom/2 , FACTOR, 0),                                               # HEP index
                  ID_EPHEP    = base::ifelse(endom_eq <= med_endom/2 , "Vulnerable", "No vulnerable"),                           # HEP ID
                  IEPHEP_LI   = base::ifelse(endom_eq <= med_endom/2 & exp_aec <= poverty_t, FACTOR, 0),                         # HEP_LI index
                  ID_EPHEP_LI = base::ifelse(endom_eq <= med_endom/2 & exp_aec <= poverty_t, "Vulnerable", "No vulnerable"),     # HEP_LI ID
                  IEPLIHC     = base::ifelse(endom_eq >= med_endom & exp_aec <= poverty_t, FACTOR, 0),                           # LIHC index
                  ID_EPLIHC   = base::ifelse(endom_eq >= med_endom & exp_aec <= poverty_t, "Vulnerable", "No vulnerable"))       # LIHC ID

  return(data)
}

#' id_tp
#'
#' Function to identify transport poor households before 2015 (included)
#' @param data dataset with the data from the HBS.
#' @param year year of the HBS data.
#' @importFrom dplyr %>%
#' @return a dataset with HBS data where transport poor households are identified.
#' @export
id_tp <- function(data, year){

  if (year == 2014) {
    data <- data[-19710,]
    data <- data[-20375,]
  }

  if (year %in% seq(2006,2015,1)) {
    # Calculate the variables needed for TP indices calculation
    data <- data %>%
      dplyr::mutate(transport = EUR_07221 + EUR_07311 + EUR_07313 + EUR_07321 + EUR_07322 + EUR_07323 + EUR_07351,  # transport expenditure
                    transport_eq = transport/UC2,                                                                   # equivalent transport expenditure
                    total_eq = GASTOT/(FACTOR*UC2),                                                                 # equivalent total expenditure
                    share_transport = transport_eq/total_eq,                                                        # share of transport expenditure
                    transpub = EUR_07311 + EUR_07313 + EUR_07321 + EUR_07322 + EUR_07323 + EUR_07351,               # public transport expenditure
                    transpub_eq = transpub/(FACTOR*UC2),                                                            # equivalent public transport expenditure
                    exp_atc = total_eq - transport_eq,                                                              # total expenditure after transport costs
                    exp_athc = exp_atc - ((EUR_04111 + EUR_04211)/UC2),                                             # total expenditure after energy and housing costs
                    exp_ahc = total_eq - ((EUR_04111 + EUR_04211)/UC2))
  } else {
    # Calculate the variables needed for TP indices calculation
    data <- data %>%
      dplyr::mutate(transport = EUR_07221 + EUR_07222 + EUR_07223 + EUR_07311 + EUR_07313 + EUR_07321 + EUR_07322 + EUR_07323 + EUR_07350,  # transport expenditure
                    transport_eq = transport/UC2,                                                                                           # equivalent transport expenditure
                    total_eq = GASTOT/(FACTOR*UC2),                                                                                         # equivalent total expenditure
                    share_transport = transport_eq/total_eq,                                                                                # share of transport expenditure
                    transpub = EUR_07311 + EUR_07313 + EUR_07321 + EUR_07322 + EUR_07323 + EUR_07350,                                       # public transport expenditure
                    transpub_eq = transpub/(FACTOR*UC2),                                                                                    # equivalent public transport expenditure
                    exp_atc = total_eq - transport_eq,                                                                                      # total expenditure after transport costs
                    exp_athc = exp_atc - ((EUR_04110 + EUR_04210)/UC2),                                                                     # total expenditure after energy and housing costs
                    exp_ahc = total_eq - ((EUR_04110 + EUR_04210)/UC2))
  }

  # Remove household without transport or public transport expenses
  data2 <- data[data$transport>0, ]
  data3 <- data[data$transpub_eq>0, ]

  # Calculate medians and thresholds
  med_stransp   <- weighted.median(data2$share_transport, w= data2$FACTOR, na.rm = TRUE)
  med_transp    <- weighted.median(data2$transport_eq, w= data2$FACTOR,  na.rm = TRUE)
  med_transpub  <- weighted.median(data3$transpub_eq, w= data3$FACTOR,  na.rm = TRUE)
  data <- data %>%
    dplyr::mutate(med_exp    = weighted.median(exp_ahc, w= FACTOR, na.rm = TRUE),        # income median (using expenditure as a better proxy of permanent income)
                  poverty_t  = med_exp*0.6)                                              # poverty threshold

  # Calculate energy poverty indices
  data <- data %>%
    dplyr::mutate(ITP10PC     = base::ifelse(share_transport >= 0.10 , FACTOR, 0),                                                                            # 10% index
                  ID_TP10PC   = base::ifelse(share_transport >= 0.10 , "Vulnerable", "No vulnerable"),                                                        # 10% ID
                  ITP2M       = base::ifelse(share_transport >= 2*med_stransp , FACTOR, 0 ),                                                                  # 2M index
                  ID_TP2M     = base::ifelse(share_transport >= 2*med_stransp , "Vulnerable", "No vulnerable"),                                               # 2M ID
                  ITPLIHC     = base::ifelse(transport_eq >= med_transp & exp_athc <= poverty_t, FACTOR, 0),                                                  # LIHC index
                  ID_TPLIHC   = base::ifelse(transport_eq >= med_transp & exp_athc <= poverty_t, "Vulnerable", "No vulnerable"),                              # LIHC ID
                  ITPVTU      = base::ifelse(share_transport >= 2*med_stransp & transpub < med_transpub & total_eq < med_exp, FACTOR , 0),                    # VTU index
                  ID_TPVTU    = base::ifelse(share_transport >= 2*med_stransp & transpub < med_transpub & total_eq < med_exp, "Vulnerable", "No vulnerable")) # VTU ID

  return(data)
}


#' load_rawhbs
#'
#' Function to load the Spanish Household Budget Survey (HBS).
#' @param year year of the HBS you want to load.
#' @param path Local path to the folder where the HBS's are stored. Not included in the package.
#' @param path_outputs path to save the results (RData)
#' @importFrom dplyr %>%
#' @return a list with the 3 files of the HBS.
#' @export
load_rawhbs <- function(year, path, path_outputs) {


  # ************************************************************
  # 1. Load EPF data
  # ************************************************************


  epf_hh <- read.csv(file.path(path,paste0("epf_", year, "_h.csv")))

  epf_hg <- read.csv(file.path(path,paste0("epf_", year, "_g.csv")))

  epf_hm <- read.csv(file.path(path,paste0("epf_", year, "_m.csv")))

  if (typeof(epf_hg$CODIGO) == "integer"){
    epf_hg$CODIGO <- as.character(epf_hg$CODIGO)
    epf_hg <- epf_hg %>% dplyr::filter(!is.na(CODIGO))
    epf_hg[nchar(epf_hg$CODIGO) == 4,]$CODIGO <- paste0("0",  epf_hg[nchar(epf_hg$CODIGO) == 4,]$CODIGO)
  }

  # Rename socioeconomic variables
  epf_hh <- standardize(epf_hh)


  # ************************************************************
  # 2. Join the household and expenditure datasets
  # ************************************************************

  # Crear variables de gasto total hogares
  g <-  epf_hg %>%
    dplyr::mutate(CODIGO = stringr::str_sub(CODIGO) <- paste0("EUR_", CODIGO),
           gasto  = GASTO/FACTOR)

  # Renombrar gastmon porque en 2021 aparece como GASTOMON
  if (year == 2021) {
    epf_hg = epf_hg %>%
      dplyr::rename(GASTMON = GASTOMON)
  }

  gm <- epf_hg  %>%
    dplyr::mutate(CODIGO = stringr::str_sub(CODIGO) <- paste0("EURMON_", CODIGO),
           gastmon  = GASTMON/FACTOR)

  c <-  epf_hg  %>%
    dplyr::mutate(CODIGO = stringr::str_sub(CODIGO) <- paste0("CAN_", CODIGO),
           cantidad  = CANTIDAD/FACTOR)

  # Change the tables from long format to wide format to make it compatible with the household file
  hh_g  <- reshape2::dcast(g, NUMERO ~ CODIGO, value.var= "gasto", fun.aggregate = sum)
  hh_gm <- reshape2::dcast(gm, NUMERO ~ CODIGO, value.var= "gastmon", fun.aggregate = sum)
  hh_c  <- reshape2::dcast(c, NUMERO ~ CODIGO, value.var= "cantidad", fun.aggregate = sum)

  # Join the household data and the expenditure data
  if (year == 2019) {
    epf_hh$NUMERO <- as.character(epf_hh$NUMERO)
    hh_g$NUMERO <- as.character(hh_g$NUMERO)
    hh_gm$NUMERO <- as.character(hh_gm$NUMERO)
    hh_c$NUMERO <- as.character(hh_c$NUMERO)
  }
  epf_hg  <- dplyr::left_join( epf_hh , hh_g  , by = "NUMERO" )
  epf_hgm <- dplyr::left_join( epf_hh , hh_gm , by = "NUMERO" )
  epf_hc  <- dplyr::left_join( epf_hh , hh_c  , by = "NUMERO" )


  # Ensure that the sum of expenditures in the household file and in the hh_g file is the same
  if (round(sum(hh_g[2:length(hh_g)], na.rm = TRUE)) != round(sum(epf_hh$GASTOT/epf_hh$FACTOR, na.rm = TRUE))) {
    stop("UNION is wrong")
  }

  # **********************************************************************
  # 3. Create new socioeconomic variables
  # **********************************************************************

  # Create the variable DECILE (on total expenditure in equivalent consumption units and by weights)
  epf_hg <- dplyr::mutate(epf_hg, GASTOT_UC2 = GASTOT/(FACTOR*UC2))
  Nquantiles <- function(x, w = NULL, s, t = NULL) {
    if (is.null(t)) {
      if (is.null(w)) w <- rep(1,length(x))
      # In the case of weights with missing values: critical error
      if (sum(is.na(w)) > 0) stop("Error.")
      n <- length(x)
      nn <- 1:n
      nn <- nn[order(x)]
      ww <- ceiling((cumsum(w[order(x, na.last = NA)])/sum(w[!is.na(x)]))*s)
      ww[n] <- s
      y <-  c(ww,rep(NA,sum(is.na(x))))[order(nn)]
    } else {
      if (sum(t != t[order(t)]) > 0)
        stop("Error.")
      yy <- rep(1,length(x))
      for (i in 1:length(t)) {
        yy <- cbind(yy, as.numeric(x >= t[i]))
      }
      y <- apply(yy, 1, sum)
    }
    return(y)
  }

  epf_hg$DECILE <- Nquantiles(epf_hg$GASTOT_UC2, w = epf_hg$FACTOR , 10)

  # Create the variable: QUINTILE
  epf_hg$QUINTILE <- Nquantiles(epf_hg$GASTOT_UC2, w = epf_hg$FACTOR , 5)

  # Create the variable: VENTILE
  epf_hg$VENTILE <- Nquantiles(epf_hg$GASTOT_UC2, w = epf_hg$FACTOR , 20)

  # Create the variable: PERCENTILE
  epf_hg$PERCENTILE <- Nquantiles(epf_hg$GASTOT_UC2, w = epf_hg$FACTOR , 100)

  # Create COUNTRYRP for 2006-2010
  if(year %in% c(2006, 2007, 2008,2009,2010)){
  epf_hg <- epf_hg %>%
    dplyr::mutate(COUNTRYRP = ifelse(NACIONA_SP == 1 , "Spain",
                              ifelse(NACIONA_SP != 1 & PAISSP == 1, "EU27",
                              ifelse(NACIONA_SP != 1 & PAISSP == 2, "Other Europe",
                              ifelse(NACIONA_SP != 1 & PAISSP == 3, "Rest of world", "Not provided")))))
  }

  # Create the variables in gender data from the HBS's individuals file: GRADOFEM
  gender <- epf_hm %>%
    dplyr::group_by ( NUMERO                                                      ) %>%    # MA04          : identification number of the household
    dplyr::summarise( number_male   = sum( SEXO == 1 & EDAD >= 14)                ,        # number_male   : number of male members in the household
                      number_female = sum( SEXO == 6 & EDAD >= 14)                ) %>%    # number_female : number of female members in the household
    dplyr::mutate   ( share_female  = number_female/(number_male + number_female) ) %>%    # perce_female  : share of female members in the household

    dplyr::mutate   ( FEMDEGREE =  ifelse(share_female <  0.2                     , "FD1",
                                   ifelse(share_female >= 0.2 & share_female < 0.4, "FD2",
                                   ifelse(share_female >= 0.4 & share_female < 0.6, "FD3",
                                   ifelse(share_female >= 0.6 & share_female < 0.8, "FD4",
                                   ifelse(share_female >= 0.8                      ,"FD5", "Not provided"))))))

  # Create the variable: POVERTY
  med_gastot <- weighted.median(epf_hg$GASTOT_UC2, epf_hg$FACTOR, na.rm = TRUE)
  u_pobreza <- 0.6*med_gastot
  epf_hg <- dplyr::mutate(epf_hg, POVERTY =ifelse(GASTOT_UC2 < u_pobreza, "At risk", "No risk"))

  # Merge the data generated at the household level by HA04 (household ID) in hg dataset
  if (year == 2019) {
    gender$NUMERO <- as.character(gender$NUMERO)
  }
  epf_hg <- dplyr::left_join( epf_hg , gender , by = "NUMERO" )

  # Create the variables for energy poor households
  if (year %in% seq(2006,2015,1)) {
    epf_hg <- id_ep2(epf_hg)
  } else {
    epf_hg <- id_ep1(epf_hg)
  }

  # Create the variables for transport poor households
  epf_hg <- id_tp(epf_hg, year = year)

  # **********************************************************************
  # 4. Remove GASTOT NA
  # **********************************************************************

  epf_hg <- epf_hg %>% dplyr::filter(!is.na(GASTOT))

  # **********************************************************************
  # 5. Outputs of the function
  # **********************************************************************

  epf_list <- list(epf_hg = epf_hg, epf_hgm = epf_hgm, epf_hc = epf_hc)
  save(epf_list, file = paste0(path_outputs,"/epf_list_", year,".RData"))

}


#' add_coicop
#'
#' Function to add COICOP categories in the Spanish Household Budget Survey
#' (HBS) according to the aggregation (coicop_year) specified in the package.
#' @param data dataset with the data from the HBS.
#' @param year year of the HBS to be modified according to the aggregation specified in the package.
#' @importFrom dplyr %>%
#' @return a dataset with HBS data where COICOP categories are aggregated according to the classification specified in the package.
#' @export
add_coicop <- function(data, year) {

  # **********************************************************************
  # 1. Load lists with the coicop aggregation we want to use
  # **********************************************************************

  # Get the mapping list
  lists <- get(paste0("coicop_", year))

  # Convert lists df to vectors

  for (r in colnames(lists)) {
    assign(r, lists %>% dplyr::filter(nchar(get(r))>0) %>% dplyr::pull(r))      # Extracts a column and maps it to the column name in a vector
  }


  # **********************************************************************
  # 2. Add coicop categories according to the lists aggregation
  # **********************************************************************

  # Add consumption variables
  epf_hg <- data
  for (c in coicop) {
      eval(parse(text = paste0("epf_hg <- epf_hg %>%
                                dplyr::mutate(", c, " = rowSums(dplyr::select(epf_hg, contains(c(", c, ")))))")))
  }

  # Ensure that the sum of expenditure of the categories created matches the original total expenditure of the survey
  if (round(sum(epf_hg$GASTOT/epf_hg$FACTOR, na.rm = TRUE)) != round(sum(rowSums(dplyr::select(epf_hg, contains(coicop))), na.rm = TRUE))){
    stop("Add coicop procedure is wrong")
  }


  return(epf_hg)
}


#' elevate_hbs
#'
#' Function to elevate the Spanish Household Budget Survey (HBS) to national accounting.
#' @param data dataset with the data from the HBS.
#' @param year year of the HBS you want to elevate to national accounting.
#' @param country country of the HBS you want to elevate. By default "ES"
#' (for the moment it only works for Spain, so DO NOT TOUCH).
#' @importFrom dplyr %>%
#' @return a dataset with the HBS data where expenses are elevated to national accounting.
#' @export
elevate_hbs <- function(data, year, country = "ES") {

  epf_hg <- data

  # ************************************************************
  # 1. Adjust HBS and national accounts population
  # ************************************************************

  # Enter population value of the NA: INE census 1 January
  pop_NA <- restatapi::get_eurostat_data("demo_gind",
                                         filters = c("AVG", country),
                                         date_filter = year)
  pop_NA <- pop_NA$values

  # Calculate survey population: sum of households x members
  pop_hbs <- sum(epf_hg$FACTOR*epf_hg$NMEMBERS)

  # Calculate the population adjustment factor (NA/HBS)
  pop_adf <- pop_NA/pop_hbs

  # Apply the population adjustment factor (factor hbs x pop_adf).
  epf_hg <- epf_hg %>%
    dplyr::mutate (HOGARESCN    = FACTOR * pop_adf  ,
                   POBLACION    = FACTOR * NMEMBERS   ,
                   POBLACIONCN  = HOGARESCN *NMEMBERS )

  # Make sure that the population adjusted to the NA matches the NA population to know if the adjustment is well done.
  if (round(sum(epf_hg$POBLACIONCN), digits = 0) != round( pop_NA, digits = 0) ){
    stop("Population adjustment is wrong")
  }

  # ************************************************************
  # 2. Adjust HBS and NA coicop categories
  # ************************************************************

  # Get the mapping list
  lists <- get(paste0("coicop_", year))

  # Convert lists df to vectors

  for (r in colnames(lists)) {
    assign(r, lists %>% dplyr::filter(nchar(get(r))>0) %>% dplyr::pull(r))      # Extracts a column and maps it to the column name in a vector
  }

  # Calculate the expenditure of each category at the level of NA population
  for (c in coicop) {
    new = paste0( c, "_PCN")
    var = c

    epf_hg <- epf_hg %>%
      dplyr::mutate({{new}} := get(var) * HOGARESCN)

    micro_ref <- sum(epf_hg[[new]])

    if (!exists("stat")){
      stat <- data.frame(COICOP = var,
                         micro_ref = micro_ref)
    }else{
      stat <- rbind(stat, data.frame(COICOP = var,
                                     micro_ref = micro_ref))
    }

  }


  # Aggregate EUR_A_073
  stat2 <- stat %>%
           dplyr::filter(COICOP %in% c("EUR_A_073_T", "EUR_A_073_A", "EUR_A_073_M")) %>%
           tibble::add_row( COICOP = "EUR_A_073", micro_ref = sum(.$micro_ref)) %>%
           dplyr::filter(COICOP == "EUR_A_073")
  stat <- rbind(stat, stat2)

  # Select macro_ref, add to stat and calculate micro-macro adjustment factor
  macro <- gcfhogares95_22 %>%
    dplyr::select(COICOP, as.character(year[1])) %>%
    dplyr::rename('macro_ref' = as.character({{year}}))
  macro$macro_ref <- as.numeric(gsub(",","",macro$macro_ref))                   # Convert to numerical in order to calculate the coefficient, for that you have to remove the commas.

  stat <- dplyr::left_join( stat , macro , by = "COICOP" )

  stat <- dplyr::mutate(stat, coicop_adf = macro_ref*1000000/micro_ref) %>%
    dplyr::mutate(coicop_adf = ifelse(COICOP %in% c("EUR_A_073_T", "EUR_A_073_A", "EUR_A_073_M"), coicop_adf[which(COICOP == "EUR_A_073")], coicop_adf))  # To do: generalising this process to any disaggregation

  # Apply EUR_A_073 adf to EUR_A_073_T, EUR_A_073_A and EUR_A_073_M


  # Apply adjustment coefficient
  for (c in coicop) {
    new = paste0( c, "_CN")
    var = paste0( c, "_PCN")
    adf = stat %>% dplyr::filter(COICOP == c) %>% dplyr::pull(coicop_adf)

    epf_hg <- epf_hg %>%
    dplyr::mutate({{new}} := get(var) * adf)
  }


  # ************************************************************
  # 3. Residents VS non-residents adjustment
  # ************************************************************

  # Calculate total hbs expenditure with the prevous adjustments

  epf_hg <- epf_hg %>% dplyr::mutate(GASTOT_CN = rowSums(dplyr::select(epf_hg, contains('_CN'))))

  # Calculate adjustment coefficient
  gf_na <- macro %>%
    dplyr::filter(COICOP == "TOTAL") %>%
    dplyr::select(macro_ref)
  gf_na <- as.numeric(gf_na$macro_ref)
  gf_na <- gf_na * 1000000

  res_adf <- gf_na/sum(epf_hg$GASTOT_CN)

  # Apply de adjustment coefficient
  for (c in coicop) {
    new = paste0( c, "_CNR")
    var = paste0( c, "_CN")
    adf = res_adf

    epf_hg <- epf_hg %>%
      dplyr::mutate({{new}} := get(var) * adf)
  }

  epf_hg <- epf_hg %>% dplyr::mutate(GASTOT_CNR = rowSums(dplyr::select(epf_hg, contains('_CNR')))) #check

  # Asegurarse de que la suma de gastos del fichero de hogares y el de hh_g coinciden
  if (round(sum(epf_hg$GASTOT_CNR)) != gf_na) {
    stop("Elevation to National Accounts is wrong")
  }

  return(epf_hg)

}


#' price_shock
#'
#' Function to apply a specific price shock to the different COICOP
#' categories of the Household Budget Survey (HBS).
#' @param data input data from the HBS to apply the price shocks.
#' @param shocks a dataset with the price shocks per coicop to be applied.
#' The format of the dataset has to correspond to the predefined one in the package.
#' To save a csv file with the right format to enter the price shocks run `ex_shocks()`.
#' You can enter more scenarios by including more columns to the right (e.g. s3).
#' A price shock greater than 1 indicates a price increase (e.g. 1.1 indicates a
#' 10\% increase) and less than 1 indicates a price decrease (e.g. 0.9 indicates a
#' 10\% decrease). The COICOP variables correspond to the aggregate variables of
#' the package, if you are not going to aggregate the COICOP variables you have to
#' replace the column labels by the COICOP variables that appear in your dataset.
#' @param year base year for the simulation. It must be the same as the year of the HBS.
#' @importFrom dplyr %>%
#' @return a dataset with the HBS data and the new expenses for COICOP categories
#' after the application of the price shock.
#' @export
price_shock <- function(data, shocks, year) {

  # Get the shock list
  # shocks <- get("shocks")

  if(year >= 2016){
    shocks <- shocks[!(shocks$coicop %in% "EUR_A_023"),]
    shocks <- shocks[!(shocks$coicop %in% "EUR_A_122"),]
  }

  # Convert lists df to vectors

  for (r in colnames(shocks)) {
    assign(r, shocks %>% dplyr::filter(nchar(get(r))>0) %>% dplyr::pull(r))
  }

  scenarios <- colnames(shocks)[3:length(colnames(shocks))]

  # Apply the price shocks
  for (s in scenarios) {
    for (c in coicop) {
      new = paste0( c, "_",s)
      var = paste0( c, "_CNR")
      shock = shocks[[s]][which(shocks$coicop == c)]

      data <- data %>%
        dplyr::mutate({{new}} := get(var) * shock)
    }
  }

  # Calculate new total consumption

  for (s in scenarios) {
    new = paste0("GASTOT_", s)

    data <- data %>%
      dplyr::mutate({{new}} := rowSums(dplyr::select(data, dplyr::contains(s))))

  }

  epf <- data

  return(epf)

}


#' adjust_wh
#'
#' Function to adjust the width and height of a basic graph depending
#' on the number of scenarios and labels.
#' @param data a dataset used to create a basic graph.
#' @param var_w variable on which the width of the basic graph depends.
#' @param var_h variable on which the height of the basic graph depends.
#' @return a list containing the width and the height of the basic graph to be created.
#' @export
adjust_wh <- function(data, var_w, var_h) {
  base_w <- 110
  a_w <- 90
  base_h <- 150
  a_h <- 160
  if(!is.null(var_w)) {
    n_elem <- length(unique(data[[var_w]]))
    final_w <- base_w + a_w * (n_elem -1)
  } else {
    final_w <- base_w
  }
  if(!is.null(var_h)) {
    n_elem <- length(unique(data[[var_h]]))
    final_h <- base_h + a_h * (n_elem -1)
  } else {
    final_h <- base_h
  }

  return(list(width = final_w, heigth = final_h))

}

#' adjust_wh_is
#'
#' Function to adjust the width and height of a intersectional graph
#' depending on the number of scenarios and labels.
#' @param data a dataset used to create an intersectional graph.
#' @param var_w variable on which the width of the intersectional graph depends.
#' @param var_h variable on which the height of the intersectional graph depends.
#' @return a list containing the width and the height of the intersectional graph to be created.
#' @export
adjust_wh_is <- function(data, var_w, var_h) {
  base_w <- 90
  a_w <- 90
  base_h <- 150
  a_h <- 50
  if(!is.null(var_w)) {
    n_elem <- length(unique(data[[var_w]]))
    final_w <- base_w + a_w * (n_elem -1)
  } else {
    final_w <- base_w
  }
  if(!is.null(var_h)) {
    n_elem <- length(unique(data[[var_h]]))
    final_h <- base_h + a_h * (n_elem -1)
  } else {
    final_h <- base_h
  }

  return(list(width = final_w, heigth = final_h))

}

#' order_var
#'
#' Function to order the labels of the socioeconomic and demographic variablesin basic graph
#' @param data dataset in which we want to order the labels of the socioeconomic and demographic variables
#' @param g variable for which we want to sort the labels
#' @importFrom dplyr %>%
#' @return a dataset in which the labels are ordered for the selected socioeconomic or demographic variable
#' @export
order_var <- function(data, g){
  if (g == "CHILDREN"){
    data <- data %>%
    dplyr::mutate(LABELS = factor(LABELS, levels = c("No children", "With children", "Large family")))
  } else if (g == "COUNTRYRP") {
    data <- data %>%
    dplyr::mutate(LABELS = factor(LABELS, levels = c("Spain", "EU27", "Other Europe", "Rest of world")))
  } else if (g == "STUDIESRP") {
    data <- data %>%
    dplyr::mutate(LABELS = factor(LABELS, levels = c("Without studies", "Primary education", "Secondary education", "Post-secondary education", "Higher education")))
  } else if (g == "REGMR") {
    data <- data %>%
    dplyr::mutate(LABELS = factor(LABELS, levels = c("Rented","Ownership", "Relinquish")))
  } else if (g == "PROFESSIONALSRP") {
    data <- data %>%
    dplyr::mutate(LABELS = factor(LABELS, levels = c("Employee", "Self-employed", "Employer")))
  } else if (g == "AGERP") {
    data <- data %>%
      dplyr::mutate(LABELS = factor(LABELS, levels = c("Young", "Adult", "Elder")))
  }
  return(data)
}

#' basic_graph
#'
#' Function to create a basic graph to summarize the distributional impact
#' based in one or more socioeconomic or demographic variable (one plot per variable).
#' @param data a dataset with the input data needed to generate a basic graph.
#' @param var variable(s) according to which you want to generate the graph. If
#' categories$categories (by default) creates a graph with the distributional
#' impacts for each of the variables specified in the package. If not, you can
#' indicate a variable or a vector of variables to crate the graph.If you want to
#' see the variables for which the function is available run `available_var_impact()`.
#' @importFrom dplyr %>%
#' @return a graph per selected variable/s summarizing distributional impacts.
#' @export
basic_graph <- function(data, var = categories$categories){
  if (!dir.exists("figures")) {dir.create("figures")}
  for (g in var) {
    datapl <- data[[paste0("di_",g)]] %>%
      tidyr::pivot_longer(cols = dplyr::starts_with("DI_"), names_to = "Scenario", values_to = "Impact") %>%
      dplyr::mutate(Scenario = stringr::str_replace(Scenario, "^DI_", "")) %>%
      dplyr::filter(! LABELS %in% c("Not provided", "NA", "Others")) %>%
      order_var(., g)

    # Convert LABELS to character if not DECILE
    if (g == "DECILE") {
      datapl <- datapl %>% dplyr::mutate(LABELS = as.numeric(LABELS))
    } else {
      datapl <- datapl %>% dplyr::mutate(LABELS = as.character(LABELS))
    }

    # Para definir el nombre largo de la variable que va a ir en el título del eje
    clean_g <- graph_labels %>%
      dplyr::filter(VARIABLE == g) %>%
      dplyr::pull(VAR_CLEAN)

    pl <- ggplot2::ggplot(datapl,
                          ggplot2::aes(x = LABELS, y = Impact, fill = Scenario)) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 1)) +
      ggplot2::facet_grid(.~Scenario) +
      ggplot2::scale_fill_manual(values=c("#3ed8d8", "#7ee5b2", "#e5e57e", "#e5b27e", "#e57f7e", "#e78ae7", "#b98ae7" )) +
      ggplot2::labs(y = "Change in welfare (%)", x = clean_g) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::theme(text = ggplot2::element_text(size = 16))


    if (g %in% c("DECILE")) {
      pl = pl + ggplot2::scale_x_continuous(breaks = 1:10, labels = 1:10)
    }
    if (g %in% c("REGION", "HHTYPE", "CHILDREN", "AGERP", "COUNTRYRP", "PROFESSIONALSRP", "STUDIESRP")) {
      pl <- pl +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.25))
    }

    adj_wh <- adjust_wh(datapl, var_w = "Scenario", var_h = NULL)
    ggplot2::ggsave(pl, file = paste0("figures/DI_",g,".png"), width = adj_wh$width  , height = adj_wh$heigth , units = "mm")
  }

  return(pl)
}


#' impact
#'
#' Function to calculate the distributional impacts based in one or more
#' socioeconomic or demographic variables (one impact per variable).
#' @param data a dataset with the input data needed to calculate distributional
#' impacts. The dataset should contain both the household expenditures collected
#' in the HBS and the expenditures after applying the price shock.
#' @param var variable(s) according to which you want to calculate distributional
#' impacts. If categories$categories (by default) calculates the distributional
#' impacts for each of the variables specified in the package. If not, you can
#' indicate a variable or a vector of variables to calculate distributional impacts.
#' If you want to see the variables for which the calculation is available run `available_var_impact()`.
#' @param save If TRUE (by default) saves a list of the generated datasets (.RData)
#' summarising the distributional impacts per selected variable. If FALSE do not save.
#' @param file_name name of the file to save the results, if save TRUE. By default "D_impacts".
#' @param fig generates and saves a graph that summarises the distributional impacts.
#' By default it is TRUE, for the graph/s not to be generated and saved indicate FALSE.
#' @param shocks_scenario_names vector of the names of the considered scenario shocks
#' @importFrom dplyr %>%
#' @return a dataframe summarising the distributional impacts per selected variable.
#' @export
impact <- function(data, var = categories$categories, save = T, file_name = "D_impacts", fig = T,
                   shocks_scenario_names) {

  d_impacts = list()                                                                                         # empty list
  missing_vars = c()

  for (g in var) {
    if (g %in% colnames(data)) {
      gastotS_cols <- intersect(paste('GASTOT',shocks_scenario_names,sep='_'), names(data))                  # generate a vector with all the scenario names of the shocks that are in the dataset
      assign(paste0('di_',g),                                                                                # assign everything calculated below to di_ g (from the loop)
             data %>%
               dplyr::group_by(!!dplyr::sym(g)) %>%                                                          # group by g, sym is to understand the value of g (override the get)
               dplyr::summarise(VARIABLE = g,                                                                # create the following columns
                         WEIGHT = sum(FACTOR),
                         dplyr::across(dplyr::all_of(gastotS_cols),                                          # for all columns that are in gastotS_cols
                                       list(DI_s = ~ 100*(sum(GASTOT_CNR) - sum(.))/sum(GASTOT_CNR)),        # generate a new column with the distributional impacts, where sum(.) is the value of the column we are using
                                       .names = "DI_{.col}")) %>%                                            # change the column name by adding DI to the column name you are using
               dplyr::rename_with(~ gsub("^DI_GASTOT", "DI", .), dplyr::starts_with("DI_GASTOT"))  %>%       # change the names of the columns starting with DI_GASTOT to DI_ only (gsub is to replace and the latter to only look at the columns starting with DI_GASTOT).
               dplyr::rename(LABELS = 1) %>%
               dplyr::mutate(LABELS = as.character(LABELS))
      )
      d_impacts[[paste0('di_',g)]] = get(paste0('di_',g))                                                    # add the result to the list with the name di_g

    } else {
        missing_vars <- c(missing_vars, g)
        warning(paste0(g, " is not present in the dataset"))
      }
    }

  if (save == T){
    if (!dir.exists("outputs_di")) {dir.create("outputs_di")}
    save(d_impacts, file = paste0("outputs_di/", file_name, ".RData"))
  }


  if (fig == T) {
    var <- setdiff(var, missing_vars)
    basic_graph(data = d_impacts, var)

  }

  if (length(d_impacts) == 1) {
    return(d_impacts[[1]])
  } else {
    combined_df <- dplyr::bind_rows(d_impacts)
    return(combined_df)
  }

}

#' order_vars
#'
#' Function to order the labels of the socioeconomic and demographic variables in intersectional graph
#' @param data dataset in which we want to order the labels of the socioeconomic and demographic variables
#' @param g variable for which we want to sort the labels
#' @importFrom dplyr %>%
#' @return a dataset in which the labels are ordered for the selected socioeconomic or demographic variable
#' @export
order_var <- function(data, g){
  if (g == "CHILDREN"){
    data <- data %>%
      dplyr::mutate(CHILDREN = factor(CHILDREN, levels = c("No children", "With children", "Large family")))
  } else if (g == "COUNTRYRP") {
    data <- data %>%
      dplyr::mutate(LABELS = factor(LABELS, levels = c("Spain", "EU27", "Other Europe", "Rest of world")))
  } else if (g == "STUDIESRP") {
    data <- data %>%
      dplyr::mutate(COUNTRYRP = factor(COUNTRYRP, levels = c("Without studies", "Primary education", "Secondary education", "Post-secondary education", "Higher education")))
  } else if (g == "REGMR") {
    data <- data %>%
      dplyr::mutate(REGMR = factor(REGMR, levels = c("Rented","Ownership", "Relinquish")))
  } else if (g == "PROFESSIONALSRP") {
    data <- data %>%
      dplyr::mutate(PROFESSIONALSRP = factor(PROFESSIONALSRP, levels = c("Employee", "Self-employed", "Employer")))
  } else if (g == "AGERP") {
    data <- data %>%
      dplyr::mutate(AGERP = factor(AGERP, levels = c("Young", "Adult", "Elder")))
  }
  return(data)
}


#' intersectional_graph
#'
#' Function to create an intersectional graph to summarize the distributional
#' impact based in the intersection of two socioeconomic or demographic variables
#' (2 variables per plot).
#' @param data a dataset with the input data needed to generate the intersectional graph.
#' @param pairs set of variables (2) according to which you want to create the
#' intersectional graph. If is_categories (by default), it generates the intersectional
#' graph for each of the combinations of variables specified in the package. If not,
#' you can indicate the set of variables according to which you want to generste the
#' intersectional graph. If you wish to see the set of variables for which the
#' calculation is available, run `available_var_intersec()`. To enter a set of
#' variables for the calculation, it must follow the same format as the output of
#' `available_var_intersec()`, i.e. a table whose columns have category_a and
#' category_b as their titles.
#' @importFrom dplyr %>%
#' @return a graph per selected set of variables summarizing the distributional impacts.
#' @export
intersectional_graph <- function(data, pairs = is_categories){
  if (!dir.exists("figures")) {dir.create("figures")}
  for (r in 1:nrow(pairs)) {      # para el numero de filas de pairs
    var_a = pairs$category_a[r]   # te coge el valor de pair en la columna a en la row r
    var_b = pairs$category_b[r]
    datapl <- data[[paste0("di_", var_a, "_", var_b)]] %>%
      tidyr::pivot_longer(cols = dplyr::starts_with("DI_"), names_to = "Scenario", values_to = "Impact") %>%
      dplyr::mutate(Scenario = stringr::str_replace(Scenario, "^DI_", "")) %>%
      dplyr::filter(! get(var_a) %in% c("Not provided", "NA", "Others")) %>%
      dplyr::filter(! get(var_b) %in% c("Not provided", "NA", "Others")) %>%
      order_vars(., var_a) %>%
      order_vars(., var_b)

    # Para definir el nombre largo de la variable que va a ir en el título del eje
    clean_a <- graph_labels %>%
      dplyr::filter(VARIABLE == var_a) %>%
      dplyr::pull(VAR_CLEAN)
    clean_b <- graph_labels %>%
      dplyr::filter(VARIABLE == var_b) %>%
      dplyr::pull(VAR_CLEAN)

    if (var_a %in% c("QUINTILE", "DECILE", "VENTILE", "PERCENTILE")) {

      pl <- ggplot2::ggplot(datapl, ggplot2::aes(x = !!dplyr::sym(var_a), y = Impact, colour = Scenario, fill = Scenario)) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::facet_grid(as.formula(paste0("~",var_b,"~Scenario"))) +
      ggplot2::scale_colour_manual(values=c("#3ed8d8", "#7ee5b2", "#e5e57e", "#e5b27e", "#e57f7e", "#e78ae7", "#b98ae7" )) +
      ggplot2::labs(y = "Change in welfare (%)", x = clean_a) +
      ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~., name = clean_b)) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::theme(text = ggplot2::element_text(size = 16))

    } else {

      pl <- ggplot2::ggplot(datapl, ggplot2::aes(x = !!dplyr::sym(var_a), y = Impact, fill = Scenario)) +
        ggplot2::geom_col(position = ggplot2::position_dodge(width = 1)) +
        ggplot2::facet_grid(as.formula(paste0("~",var_b,"~Scenario"))) +
        ggplot2::scale_fill_manual(values=c("#3ed8d8", "#7ee5b2", "#e5e57e", "#e5b27e", "#e57f7e", "#e78ae7", "#b98ae7" )) +
        ggplot2::labs(y = "Change in welfare (%)", x = clean_a) +
        ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~., name = clean_b))+
        ggplot2::theme(legend.position = "none") +
        ggplot2::theme(text = ggplot2::element_text(size = 16))

    }

    if (var_a %in% c("DECILE")) {
      pl = pl + ggplot2::scale_x_continuous(breaks = 1:10, labels = 1:10, name = clean_a)
    }

    if (var_a %in% c("REGION", "HHTYPE", "CHILDREN", "AGERP", "COUNTRYRP", "PROFESSIONALSRP", "STUDIESRP")) {
      pl <- pl +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.25))
    }

    adj_wh <- adjust_wh_is(datapl, var_w = "Scenario", var_h = var_b)
    ggplot2::ggsave(pl, file = paste0("figures/DI_",var_a,"_", var_b,".png"), width = adj_wh$width  , height = adj_wh$heigth , units = "mm")
  }

  return(pl)

}


#' impact_intersectional
#'
#' Function to calculate the distributional impacts based in the intersection
#' of two socioeconomic or demographic variables (2 variables per impact).
#' @param data a dataset with the input data needed to calculate the intersectional
#' distributional impacts. The dataset should contain both the household expenditures
#' collected in the HBS and the expenditures after applying the price shock.
#' @param pairs set of variables (2) according to which you want to calculate
#' distributional impacts. If is_categories (by default) calculates the intersectional
#' distributional impacts for each of the set of variables specified in the package.
#' If not, you can indicate the set of variables according to which you want to calculate
#' the intersectional distributional impacts.If you want to see the set of variables for
#' which the calculation is available run `available_var_intersec()`. To enter a
#' set of variables for the calculation, it must follow the same format as the
#' output of `available_var_intersec()`, i.e. a table whose columns have category_a
#' and category_b as their titles.
#' @param save If TRUE (by default) saves a list of the generated datasets (.RData)
#' summarising the intersectional distributional impacts per selected set of variable.
#' If FALSE do not save.
#' @param file_name name of the file to save the results, if save TRUE. By default "DI_impacts".
#' @param fig generates and saves a graph that summarises the intersectional distributional
#' impacts. By default it is TRUE, for the graph/s not to be generated and saved indicate FALSE.
#' @param shocks_scenario_names vector of the names of the considered scenario shocks
#' @importFrom dplyr %>%
#' @return a list containing the generated datasets (.RData) summarising the intersectional
#' distributional impacts per selected set of variables.
#' @export
impact_intersectional <- function(data, pairs = is_categories, save = T, file_name = "DI_impact", fig = T,
                                  shocks_scenario_names) {

  is_d_impacts = list()            # empty list
  missing_vars = c()
   for (r in 1:nrow(pairs)) {      # for the number of rows of pairs
    var_a = pairs$category_a[r]    # takes the value of pair in column a in row r
    var_b = pairs$category_b[r]    # takes the value of pair in column b in row r
    # ensure that var_a and var_b are in the dataset (as column names)
    if (var_a %in% colnames(data) & var_b %in% colnames(data)) {
      gastotS_cols <- intersect(paste('GASTOT',shocks_scenario_names,sep='_'), names(data))                       # generate a vector with all the scenario names of the shocks that are in the dataset
      assign(paste0('di_',var_a,"_",var_b),                                                                       # assign everything calculated below to di_ g (from the loop)
             data %>%
               dplyr::group_by(!!dplyr::sym(var_a),!!dplyr::sym(var_b)) %>%                                       # group by g, sym is so that it understands the value of g (it replaces get)
               dplyr::summarise(VARIABLE_A = var_a,                                                               # create the following columns
                                VARIABLE_B = var_b,                                                               # create the following columns
                                WEIGHT = sum(FACTOR),
                                dplyr::across(dplyr::all_of(gastotS_cols),                                        # for all columns that are in gastotS_cols
                                              list(DI_s = ~ 100*(sum(GASTOT_CNR) - sum(.))/sum(GASTOT_CNR)),          # generate a new column with the distributional impacts, where sum(.) is the value of the column we are using
                                              .names = "DI_{.col}")) %>%                                          # change the name of the column by adding DI to the column name that is being used
               dplyr::rename_with(~ gsub("^DI_GASTOT", "DI", .), dplyr::starts_with("DI_GASTOT"))                 # change the names of the columns that start with DI GASTOR to DI_ only (gsub is for replacing and the second so that it only looks at those that start with DI_GASTOT)
      )
      is_d_impacts[[paste0('di_',var_a,'_',var_b)]] = get(paste0('di_',var_a,'_',var_b))                          # add the result to the list with the name di_g
    } else {
      if (var_a %in% colnames(data) & !var_b %in% colnames(data)) {
        missing_vars <- c(missing_vars, var_b)
        warning(paste0(var_b," is not present in the dataset"))}
      else if (!var_a %in% colnames(data) & var_b %in% colnames(data)) {
        missing_vars <- c(missing_vars, var_a)
        warning(paste0(var_a," is not present in the dataset"))}
      else {
        missing_vars <- c(missing_vars, var_a, var_b)
        warning(paste0(var_a, "and ", var_b," are not present in the dataset"))}
    }
  }

  if (save == T){
    if (!dir.exists("outputs_dii")) {dir.create("outputs_dii")}
    save(is_d_impacts, file = paste0("outputs_dii/", file_name, ".RData"))
  }

  if (fig == T) {
    pairs <- pairs %>%
      dplyr::filter(!category_a %in% missing_vars,
                    !category_b %in% missing_vars)
    intersectional_graph(data = is_d_impacts, pairs)

  }

  return(is_d_impacts)
}

#' check_year
#'
#' Check if the parameter year is valid
#' @param year year introduce by the user
#' @return stop if year is wrongly introduced
#' @export

check_year <- function(year) {

  if (length(year) != 1) {
    stop(sprintf('You introduced a vector of years, only a single year is allowed. Possible options are %s.',
                 paste(seq(2006,2021,1), collapse = ", ")))
  }

  if(is.character(year)) {
    year <- as.numeric(year)
  }

  if (!year %in% seq(2006,2021,1)) {
    stop(sprintf('You introduced year %s which is not available. Possible options are %s.',
                 year, paste(seq(2006,2021,1), collapse = ", ")))
  }

}


#' check_var_impact
#'
#' Check if the parameter var_impact is valid
#' @param var_impact var_impact introduce by the user
#' @return stop if year is wrongly introduced
#' @export

check_var_impact <- function(var_impact) {

  wrong_vars <- var_impact[!var_impact %in% categories$categories]

  if (length(wrong_vars) == 1) {
    stop(sprintf('You introduced the variable %s which is not available. Possible options are: %s.',
                   wrong_vars, paste(categories$categories, collapse = ", ")))
  } else if (length(wrong_vars) > 1) {
    stop(sprintf('You introduced the variables %s which are not available. Possible options are: %s.',
                 paste(wrong_vars, collapse = ", "), paste(categories$categories, collapse = ", ")))
    }
  }


