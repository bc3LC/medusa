library(usethis)
library(magrittr)

# rename_values
#'
#' Details: function to rename the codified values of the dataset to the meaningful values detailed in the mapping included in the package
#' @param data dataset to be standardized
#' @param current_var column name to be standardized
#' @return a dataset with labels renamed based in the mapping included in the package.
#' @export
rename_values = function(data, current_var) {
  exchange_data = mapping %>%                                             # te junta los dos df (mapping y data)
    dplyr::filter(VAR == current_var) %>%                                 # pero solo coge la parte que nos interesa (solo cuando el valor de la variable es igual a current_var)
    dplyr::select(value, NOMBRE) %>%
    dplyr::distinct()

  if (current_var != "NMIEMB" | (current_var == "NMIEMB" & sum(is.na(unique(exchange_data$value))) == 0) ) {                     # miramos si hay algun na en value y si es así no se aplica lo de abajo (para evitar errores con NMIEMB)
    data = data %>%
      dplyr::rename(value = {{ current_var }}) %>%                             # renombra la columna (nombre de la variable) a value
      dplyr::mutate(value = as.character(value)) %>%                           # te convierte en caracter
      dplyr::left_join(exchange_data, by = "value") %>%                        # se queda solo con las columnas que nos interesan (value y nombre)
      dplyr::select(-value) %>%                                                # te elimina la columna value
      dplyr::rename_with(~current_var, 'NOMBRE')                               # te renombra de nombre a current_var
  }


  return(data)
}


# standardize
#'
#' Details: function to standarize data names
#' @param data dataset to be standardized
#' @return a dataset with the variables and labels renamed based in the mapping included in the package.
#' @export
standardize <- function(data) {
  # rename columns
  old_names = colnames(data)                                                # te crea un vector con los nombres de las columnas
  new_names = dplyr::left_join(data.frame(VAR_EPF = old_names),             # el vector de los nombres de las columnas se convierte en un df y el nombre de la columna es VAR_EPF
                               mapping %>%
                                 dplyr::select(VAR_EPF, VAR) %>%            # dentro del mapping selecciona solo las columnas que nos interesan
                                 dplyr::distinct(.),                        # para eliminar duplicados
                               by = 'VAR_EPF') %>%                          # Haces el left join en funcion a var_epf
    dplyr::mutate(VAR = ifelse(is.na(VAR), VAR_EPF, VAR)) %>%               # cuando es na te pone var_epf y si no te pone var
    dplyr::pull(VAR)                                                        # para solo quedarte con la columna var (como vector)
  colnames(data) = new_names                                                # asignamos a los nombres de las columnas el nuevo vector

  # rename values' codes to values' names for all items whose name is in the
  # renamed mapping's column and have not NA values
  ccitems = mapping %>%
    dplyr::filter(VAR %in% intersect(unique(mapping$VAR), new_names),
                  !is.na(value)) %>%
    dplyr::pull(VAR) %>%
    unique()
  for (cc in ccitems) {                                                     # para todas las columnas donde se puede hacer mapping de la variables sobreescribimos el dataset standarizado (con función rename_value)
    data = rename_values(data, cc)
  }

  return(data)
}


# load_rawhbs
#'
#' Details: function to load the Spanish Household Budget Survey (HBS)
#' @param year year of the HBS you want to load
#' @param path Local path to the folder where the HBS's are stored. Not included in the package.
#' @return a list with the 3 files of the HBS
#' @export
load_rawhbs <- function(year, path) {


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
  epf_hh0 <- standardize(epf_hh)


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

  # Pasar las tablas de formato largo a formato ancho para hacerla compatible con el fichero de hogares
  hh_g  <- reshape2::dcast(g, NUMERO ~ CODIGO, value.var= "gasto", fun.aggregate = sum)
  hh_gm <- reshape2::dcast(gm, NUMERO ~ CODIGO, value.var= "gastmon", fun.aggregate = sum)
  hh_c  <- reshape2::dcast(c, NUMERO ~ CODIGO, value.var= "cantidad", fun.aggregate = sum)

  # Unir la tabla de hogares y con los datos de gasto
  if (year == 2019) {
    epf_hh$NUMERO <- as.character(epf_hh$NUMERO)
    hh_g$NUMERO <- as.character(hh_g$NUMERO)
    hh_gm$NUMERO <- as.character(hh_gm$NUMERO)
    hh_c$NUMERO <- as.character(hh_c$NUMERO)
  }
  epf_hg  <- dplyr::left_join( epf_hh , hh_g  , by = "NUMERO" )
  epf_hgm <- dplyr::left_join( epf_hh , hh_gm , by = "NUMERO" )
  epf_hc  <- dplyr::left_join( epf_hh , hh_c  , by = "NUMERO" )


  # Asegurarse de que la suma de gastos del fichero de hogares y el de hh_g coinciden
  if (round(sum(hh_g[2:length(hh_g)], na.rm = TRUE)) != round(sum(epf_hh$GASTOT/epf_hh$FACTOR, na.rm = TRUE))) {
    stop("UNION is wrong")
  }

  # **********************************************************************
  # 3. Create new socioeconomic variables
  # **********************************************************************

  # Create the variable DECIL (on total expenditure in equivalent consumption units and by weights)
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

  epf_hg$DECIL <- Nquantiles(epf_hg$GASTOT_UC2, w = epf_hg$FACTOR , 10)

  # Create the variable: QUINTIL
  epf_hg$QUINTIL <- Nquantiles(epf_hg$GASTOT_UC2, w = epf_hg$FACTOR , 5)

  # Create the variable: VENTIL
  epf_hg$VENTIL <- Nquantiles(epf_hg$GASTOT_UC2, w = epf_hg$FACTOR , 20)

  # Create the variable: PERCENTIL
  epf_hg$PERCENTIL <- Nquantiles(epf_hg$GASTOT_UC2, w = epf_hg$FACTOR , 100)

  # Create PAISPR for 2006-2010
  if(year %in% c(2006, 2007, 2008,2009,2010)){
  epf_hg <- epf_hg %>%
    dplyr::mutate(PAISPR = ifelse(NACIONA_SP == 1 , "España",
                           ifelse(NACIONA_SP != 1 & PAISSP == 1, "UE27",
                           ifelse(NACIONA_SP != 1 & PAISSP == 2, "Otros Europa",
                           ifelse(NACIONA_SP != 1 & PAISSP == 3, "Resto mundo", "No consta")))))
  }

  # Create the variables in gender data from the HBS's individuals file: GRADOFEM
  gender <- epf_hm %>%
    dplyr::group_by ( NUMERO                                                      ) %>%    # MA04          : identification number of the household
    dplyr::summarise( number_male   = sum( SEXO == 1 & EDAD >= 14)                ,        # number_male   : number of male members in the household
                      number_female = sum( SEXO == 6 & EDAD >= 14)                ) %>%    # number_female : number of female members in the household
    dplyr::mutate   ( share_female  = number_female/(number_male + number_female) ) %>%    # perce_female  : share of female members in the household

    dplyr::mutate   ( GRADOFEM =  ifelse(share_female <  0.2                     , "GF1",
                                  ifelse(share_female >= 0.2 & share_female < 0.4, "GF2",
                                  ifelse(share_female >= 0.4 & share_female < 0.6, "GF3",
                                  ifelse(share_female >= 0.6 & share_female < 0.8, "GF4",
                                  ifelse(share_female >= 0.8                      ,"GF5", "Not provided"))))))

  # Create the variable: POBREZA
  med_gastot <- spatstat.geom::weighted.median(epf_hg$GASTOT_UC2, epf_hg$FACTOR, na.rm = TRUE)
  u_pobreza <- 0.6*med_gastot
  epf_hg <- dplyr::mutate(epf_hg, POBREZA =ifelse(GASTOT_UC2 < u_pobreza, "En riesgo", "Sin riesgo"))

  # Merge the data generated at the household level by HA04 (household ID) in hg dataset
  if (year == 2019) {
    gender$NUMERO <- as.character(gender$NUMERO)
  }
  epf_hg <- dplyr::left_join( epf_hg , gender , by = "NUMERO" )


  # **********************************************************************
  # 4. Remove GASTOT NA
  # **********************************************************************

  epf_hg <- epf_hg %>% dplyr::filter(!is.na(GASTOT))

  # **********************************************************************
  # 5. Outputs of the function
  # **********************************************************************

  epf_list <- list(epf_hg = epf_hg, epf_hgm = epf_hgm, epf_hc = epf_hc)
  save(epf_list, file = paste0("inst/extdata/epf_lists/epf_list_", year,".RData"))

}


# add_coicop
#'
#' Details: function to add COICOP categories in the Spanish Household Budget Survey (HBS) according to the aggregation (coicop_year) specified in the package.
#' @param data dataset with the data from the HBS
#' @param year year of the HBS to be modified according to the aggregation specified in the package
#' @return a dataset with HBS data where COICOP categories are aggregated according to the classification specified in the package
#' @export
add_coicop <- function(data, year) {

  # **********************************************************************
  # 1. Load lists with the coicop aggregation we want to use
  # **********************************************************************

  # Get the mapping list
  lists <- get(paste0("coicop_", year))

  # Convert lists df to vectors

  for (r in colnames(lists)) {
    assign(r, lists %>% dplyr::filter(nchar(get(r))>0) %>% dplyr::pull(r))      # Extrae una columna y se le asigna al nombre de la columna en un vector
  }


  # **********************************************************************
  # 2. Add coicop categories according to de lists aggregation
  # **********************************************************************

  # Add consumption variables
  epf_hg <- data
  for (c in coicop) {
      eval(parse(text = paste0("epf_hg <- epf_hg %>%
                                dplyr::mutate(", c, " = rowSums(dplyr::select(epf_hg, contains(c(", c, ")))))")))
  }

  # Ensure that the sum of expenditure of the categories created matches the original total expenditure of the survey
  if (sum(epf_hg$GASTOT/epf_hg$FACTOR, na.rm = TRUE) == sum(rowSums(dplyr::select(epf_hg, contains(coicop))), na.rm = TRUE)){
    print(paste0("Add coicop procedure is correct"))
  }


  return(epf_hg)
}


# elevate_hbs
#'
#' Details: function to elevate the Spanish Household Budget Survey (HBS) to national accounting.
#' @param data dataset with the data from the HBS
#' @param year year of the HBS you want to elevate to national accounting
#' @param country country of the HBS you want to elevate. By default "ES" (for the moment it only works for Spain, so DO NOT TOUCH)
#' @return a dataset with the HBS data where expenses are elevated to national accounting
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
  pop_hbs <- sum(epf_hg$FACTOR*epf_hg$NMIEMB)

  # Calculate the population adjustment factor (NA/HBS)
  pop_adf <- pop_NA/pop_hbs

  # Apply the population adjustment factor (factor hbs x pop_adf).
  epf_hg <- epf_hg %>%
    dplyr::mutate (HOGARESCN    = FACTOR * pop_adf  ,
                   POBLACION    = FACTOR * NMIEMB   ,
                   POBLACIONCN  = HOGARESCN *NMIEMB )

  # Make sure that the population adjusted to the NA matches the NA population to know if the adjustment is well done.
  if (round(sum(epf_hg$POBLACIONCN), digits = 0) == round( pop_NA, digits = 0) ){
    print(paste0("AJUSTE POBLACION is correct"))
  }

  # ************************************************************
  # 2. Adjust HBS and NA coicop categories
  # ************************************************************

  # Get the mapping list
  lists <- get(paste0("coicop_", year))

  # Convert lists df to vectors

  for (r in colnames(lists)) {
    assign(r, lists %>% dplyr::filter(nchar(get(r))>0) %>% dplyr::pull(r))      # Extrae una columna y se le asigna al nombre de la columna en un vector
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
  macro$macro_ref <- as.numeric(gsub(",","",macro$macro_ref))                   # Convertir en numerico para poder calcular el coeficiente, para eso hay que quitarle las comas

  stat <- dplyr::left_join( stat , macro , by = "COICOP" )

  stat <- dplyr::mutate(stat, coicop_adf = macro_ref*1000000/micro_ref) %>%
    dplyr::mutate(coicop_adf = ifelse(COICOP %in% c("EUR_A_073_T", "EUR_A_073_A", "EUR_A_073_M"), coicop_adf[which(COICOP == "EUR_A_073")], coicop_adf))                         # To do: generalizar este proceso a cualquier desagregacion

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
  if (round(sum(epf_hg$GASTOT_CNR)) == gf_na) {
    print(paste0("AJUSTE RESIDENTES/NO RESIDENTES is correct"))
  }else{
    print(paste0("AJUSTE RESIDENTES/NO RESIDENTES is wrong"))
  }

  return(epf_hg)

}


# price_shock
#'
#' Details: function to apply a specific price shock to the different COICOP categories of the Household Budget Survey (HBS)
#' @param data input data from the HBS to apply the price shocks
#' @return a dataset with the HBS data and the new expenses for COICOP categories after the application of the price shock
#' @export
price_shock <- function(data) {

  # Get the shock list
  shocks <- get("shocks")

  if(year >= 2016){
    shocks <- shocks[!(shocks$coicop %in% "EUR_A_023"),]
    shocks <- shocks[!(shocks$coicop %in% "EUR_A_122"),]
  }

  # Convert lists df to vectors

  for (r in colnames(shocks)) {
    assign(r, shocks %>% dplyr::filter(nchar(get(r))>0) %>% dplyr::pull(r))      # Extrae una columna y se le asigna al nombre de la columna en un vector
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


# adjust_wh
#'
#' Details: function to adjust the width and height of a basic graph depending on the number of scenarios and labels
#' @param data a dataset used to create a basic graph
#' @param var_w variable on which the width of the basic graph depends
#' @param var_h variable on which the height of the basic graph depends
#' @return a list containing the width and the height of the basic graph to be created
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

# adjust_wh_is
#'
#' Details: function to adjust the width and height of a intersectional graph depending on the number of scenarios and labels
#' @param data a dataset used to create an intersectional graph
#' @param var_w variable on which the width of the intersectional graph depends
#' @param var_h variable on which the height of the intersectional graph depends
#' @return a list containing the width and the height of the intersectional graph to be created
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

# basic_graph
#'
#' Details: function to create a basic graph to summarize the distributional impact based in one or more socioeconomic or demographic variable (one plot per variable) (INTERNAL FUNCTION)
#' @param data a dataset with the input data needed to generate a basic graph
#' @param var variable(s) according to which you want to generate the graph. If categories$categories (by default) creates a graph with the distributional impacts for each of the variables specified in the package. If not, you can indicate a variable or a vector of variables to crate the graph.If you want to see the variables for which the function is available run `available_var_impact()`.
#' @return a graph per selected variable/s summarizing distributional impacts
#' @export
basic_graph <- function(data, var = categories$categories){
  if (!dir.exists("figures")) {dir.create("figures")}
  for (g in var) {
    print(g)
    datapl <- data[[paste0("di_",g)]] %>%
      tidyr::pivot_longer(cols = dplyr::starts_with("DI_"), names_to = "Scenario", values_to = "Impact") %>%
      dplyr::mutate(Scenario = stringr::str_replace(Scenario, "^DI_", ""))

    pl <- ggplot2::ggplot(datapl, ggplot2::aes(x = !!dplyr::sym(g), y = Impact)) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 1)) +
      ggplot2::facet_grid(.~Scenario) +
      ggplot2::labs(y = "Change in welfare (%)", x = g) +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::theme(text = ggplot2::element_text(size = 16))
    if (g %in% c("CCAA")) {
      pl <- pl +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.25))
    }

    adj_wh <- adjust_wh(datapl, var_w = "Scenario", var_h = NULL)
    ggplot2::ggsave(pl, file = paste0("figures/DI_",g,".png"), width = adj_wh$width  , height = adj_wh$heigth , units = "mm")
  }

  return(pl)
}


# impact
#'
#' Details: function to calculate the distributional impacts based in one or more socioeconomic or demographic variables (one impact per variable)
#' @param data a dataset with the input data needed to calculate distributional impacts. The dataset should contain both the household expenditures collected in the HBS and the expenditures after applying the price shock.
#' @param var variable(s) according to which you want to calculate distributional impacts. If categories$categories (by default) calculates the distributional impacts for each of the variables specified in the package. If not, you can indicate a variable or a vector of variables to calculate distributional impacts.If you want to see the variables for which the calculation is available run `available_var_impact()`.
#' @param save If TRUE (by default) saves a list of the generated datasets (.RData) summarising the distributional impacts per selected variable. If FALSE do not save.
#' @param file_name name of the file to save the results, if save TRUE. By default "D_impacts".
#' @param fig generates and saves a graph that summarises the distributional impacts. By default it is TRUE, for the graph/s not to be generated and saved indicate FALSE.
#' @return a list containing the generated datasets (.RData) summarising the distributional impacts per selected variable.
#' @export
impact <- function(data, var = categories$categories, save = T, file_name = "D_impacts", fig = T) {

  d_impacts = list()                                                                                        # Generamos una lista vacia
  for (g in var) {
    if (g %in% colnames(data)) {
      gastotS_cols <- grep("^GASTOT_s", names(data), value = TRUE)                                           # generamos un vector con todos los nombres que empiecen por GASTOT_s
      assign(paste0('di_',g),                                                                                # asignamos todo lo que se calcula debajo a di_ g (del loop)
             data %>%
               dplyr::group_by(!!dplyr::sym(g)) %>%                                                          # agrupamos por g, sym es para que entienda el valor de g (sustituye el get)
               dplyr::summarise(VARIABLE = g,                                                                # crear las columnas siguientes
                         WEIGHT = sum(FACTOR),
                         dplyr::across(dplyr::all_of(gastotS_cols),                                          # para todas las columnas que estan en gastotS_cols
                                       list(DI_s = ~ 100*(sum(GASTOT_CNR) - sum(.))/sum(GASTOT_CNR)),            # generamos una nueva columna con los impactos distributivos, donde sum(.) es el valor de la columna que estamos usando
                                       .names = "DI_{.col}")) %>%                                            # cambiamos el nombre de la columna añadiendo DI al nombre de columna que esta usando
               dplyr::rename_with(~ gsub("^DI_GASTOT", "DI", .), dplyr::starts_with("DI_GASTOT"))            # cambiamos los nombres de las columnas que empiezen por DI_GASTOT a DI_ solo (gsub es para sustituir y lo segundo para que solo se fije en las que empiezan por DI_GASTOT)
      )
      d_impacts[[paste0('di_',g)]] = get(paste0('di_',g))                                                     # añadir el resultado a la lista con el nombre di_g
    } else {
        warning(paste0(var, " is not present in the dataset"))
      }
    }

  if (save == T){
    if (!dir.exists("outputs_di")) {dir.create("outputs_di")}
    save(d_impacts, file = paste0("outputs_di/", file_name, ".RData"))
  }


  if (fig == T) {

    basic_graph(data = d_impacts, var)

  }

  return(d_impacts)

}


# intersectional_graph
#'
#' Details: function to create an intersectional graph to summarize the distributional impact based in the intersection of two socioeconomic or demographic variables (2 variables per plot)
#' @param data a dataset with the input data needed to generate the intersectional graph
#' @param pairs set of variables (2) according to which you want to create the intersectional graph. If is_categories (by default), it generates the intersectional graph for each of the combinations of variables specified in the package. If not, you can indicate the set of variables according to which you want to generste the intersectional graph. If you wish to see the set of variables for which the calculation is available, run `available_var_intersec()`. To enter a set of variables for the calculation, it must follow the same format as the output of `available_var_intersec()`, i.e. a table whose columns have category_a and category_b as their titles.
#' @return a graph per selected set of variables summarizing the distributional impacts
#' @export
intersectional_graph <- function(data, pairs = is_categories){
  if (!dir.exists("figures")) {dir.create("figures")}
  for (r in 1:nrow(pairs)) {      # para el numero de filas de pairs
    var_a = pairs$category_a[r]   # te coge el valor de pair en la columna a en la row r
    var_b = pairs$category_b[r]
    datapl <- data[[paste0("di_", var_a, "_", var_b)]] %>%
      tidyr::pivot_longer(cols = dplyr::starts_with("DI_"), names_to = "Scenario", values_to = "Impact") %>%
      dplyr::mutate(Scenario = stringr::str_replace(Scenario, "^DI_", ""))

    pl <- ggplot2::ggplot(datapl, ggplot2::aes(x = !!dplyr::sym(var_a), y = Impact)) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::facet_grid(as.formula(paste0("~",var_b,"~Scenario"))) +
      ggplot2::labs(y = "Change in welfare (%)", x = var_a) +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::theme(text = ggplot2::element_text(size = 16))

    adj_wh <- adjust_wh_is(datapl, var_w = "Scenario", var_h = var_b)
    ggplot2::ggsave(pl, file = paste0("figures/DI_",var_a,"_", var_b,".png"), width = adj_wh$width  , height = adj_wh$heigth , units = "mm")
  }

  return(pl)

}


# impact_intersectional
#'
#' #' Details: function to calculate the distributional impacts based in the intersection of two socioeconomic or demographic variables (2 variables per impact)
#' @param data a dataset with the input data needed to calculate the intersectional distributional impacts. The dataset should contain both the household expenditures collected in the HBS and the expenditures after applying the price shock.
#' @param pairs set of variables (2) according to which you want to calculate distributional impacts. If is_categories (by default) calculates the intersectional distributional impacts for each of the set of variables specified in the package. If not, you can indicate the set of variables according to which you want to calculate the intersectional distributional impacts.If you want to see the set of variables for which the calculation is available run `available_var_intersec()`. To enter a set of variables for the calculation, it must follow the same format as the output of `available_var_intersec()`, i.e. a table whose columns have category_a and category_b as their titles.
#' @param save If TRUE (by default) saves a list of the generated datasets (.RData) summarising the intersectional distributional impacts per selected set of variable. If FALSE do not save.
#' @param file_name name of the file to save the results, if save TRUE. By default "DI_impacts".
#' @param fig generates and saves a graph that summarises the intersectional distributional impacts. By default it is TRUE, for the graph/s not to be generated and saved indicate FALSE.
#' @return a list containing the generated datasets (.RData) summarising the intersectional distributional impacts per selected set of variables.
#' @export
impact_intersectional <- function(data, pairs = is_categories, save = T, file_name = "DI_impact", fig = T) {

  is_d_impacts = list()                                                                                        # Generamos una lista vacia
  for (r in 1:nrow(pairs)) {      # para el numero de filas de pairs
    var_a = pairs$category_a[r]   # te coge el valor de pair en la columna a en la row r
    var_b = pairs$category_b[r]   # te coge el valor de pair en la columna a en la row r
    # ensure that var_a and var_b are in the dataset (as column names)
    if (var_a %in% colnames(data) & var_b %in% colnames(data)) {
      gastotS_cols <- grep("^GASTOT_s", names(data), value = TRUE)                                           # generamos un vector con todos los nombres que empiecen por GASTOT_s
      assign(paste0('di_',var_a,"_",var_b),                                                                                # asignamos todo lo que se calcula debajo a di_ g (del loop)
             data %>%
               dplyr::group_by(!!dplyr::sym(var_a),!!dplyr::sym(var_b)) %>%                                                          # agrupamos por g, sym es para que entienda el valor de g (sustituye el get)
               dplyr::summarise(VARIABLE_A = var_a,                                                                # crear las columnas siguientes
                                VARIABLE_B = var_b,                                                                # crear las columnas siguientes
                                WEIGHT = sum(FACTOR),
                                dplyr::across(dplyr::all_of(gastotS_cols),                                          # para todas las columnas que estan en gastotS_cols
                                              list(DI_s = ~ (sum(GASTOT_CNR) - sum(.))/sum(GASTOT_CNR)),            # generamos una nueva columna con los impactos distributivos, donde sum(.) es el valor de la columna que estamos usando
                                              .names = "DI_{.col}")) %>%                                            # cambiamos el nombre de la columna añadiendo DI al nombre de columna que esta usando
               dplyr::rename_with(~ gsub("^DI_GASTOT", "DI", .), dplyr::starts_with("DI_GASTOT"))                   # cambiamos los nombres de las columnas que empiezen por DI_GASTOT a DI_ solo (gsub es para sustituir y lo segundo para que solo se fije en las que empiezan por DI_GASTOT)
      )
      is_d_impacts[[paste0('di_',var_a,'_',var_b)]] = get(paste0('di_',var_a,'_',var_b))     # añadir el resultado a la lista con el nombre di_g
    } else {
      if (var_a %in% colnames(data) & !var_b %in% colnames(data)) {warning(paste0(var_b," is not present in the dataset"))}
      else if (!var_a %in% colnames(data) & var_b %in% colnames(data)) {warning(paste0(var_a," is not present in the dataset"))}
      else {warning(paste0(var_a, "and ", var_b," are not present in the dataset"))}
    }
  }

  if (save == T){
    if (!dir.exists("outputs_dii")) {dir.create("outputs_dii")}
    save(is_d_impacts, file = paste0("outputs_dii/", file_name, ".RData"))
  }

  if (fig == T) {

    intersectional_graph(data = is_d_impacts, pairs)

  }

  return(is_d_impacts)
}
