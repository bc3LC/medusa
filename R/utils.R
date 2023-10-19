library(usethis)
library(magrittr)
library(medusa)

# rename_values
#'
#' Details: function to rename the codified values of the dataset to the meaningful values detailed in the mapping
#' @param data dataset to be standardized
#' @param current_var column name to be standardized
#' @export
rename_values = function(data, current_var) {
  exchange_data = mapping %>%                                             # te junta los dos df (mapping y data)
    dplyr::filter(VAR == current_var) %>%                                 # pero solo coge la parte que nos interesa (solo cuando el valor de la variable es igual a current_var)
    dplyr::select(value, NOMBRE)

  if (sum(is.na(unique(exchange_data$value))) == 0) {                     # miramos si hay algun na en value y si es así no se aplica lo de abajo (para evitar errores con NMIEMB)
    data = data %>%
      dplyr::rename(value = {{ current_var }}) %>%                             # renombra la columna (nombre de la variable) a value
      dplyr::mutate(value = as.character(value)) %>%                           # te convierte en caracter
      dplyr::left_join(exchange_data, by = "value") %>%       # se queda solo con las columnas que nos interesan (value y nombre)
      dplyr::select(-value) %>%                                                # te elimina la columna value
      dplyr::rename_with(~current_var, 'NOMBRE')                               # te renombra de nombre a current_var
  }


  return(data)
}


# standardize
#'
#' Details: function to standarize data names
#' @param data dataset to be standardized
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


# load_hbs
#'
#' Details: main function to load the Spanish Household Budget Survey (HBS)
#' @param year year of the HBS you want to load
#' @return a list with the 3 files of the HBS
#' @export
load_hbs <- function(year) {


  # ************************************************************
  # 1. Load EPF data
  # ************************************************************

  epf_hh <- get(paste0("epf_", year, "_h"))

  epf_hg <- get(paste0("epf_", year, "_g"))

  epf_hm <- get(paste0("epf_", year, "_m"))

  if (typeof(epf_hg$CODIGO) == "integer"){
    epf_hg$CODIGO <- as.character(epf_hg$CODIGO)
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

  # Pasar las tablas de formato largo a formato ancho para hacerla compatible con el fichero de hogares
  hh_g  <- reshape2::dcast(g, NUMERO ~ CODIGO, value.var= "gasto", fun.aggregate = sum)
  hh_gm <- reshape2::dcast(gm, NUMERO ~ CODIGO, value.var= "gastmon", fun.aggregate = sum)
  hh_c  <- reshape2::dcast(c, NUMERO ~ CODIGO, value.var= "cantidad", fun.aggregate = sum)

  # Unir la tabla de hogares y con los datos de gasto
  epf_hg  <- dplyr::left_join( epf_hh , hh_g  , by = "NUMERO" )
  epf_hgm <- dplyr::left_join( epf_hh , hh_gm , by = "NUMERO" )
  epf_hc  <- dplyr::left_join( epf_hh , hh_c  , by = "NUMERO" )


  # Asegurarse de que la suma de gastos del fichero de hogares y el de hh_g coinciden
  if (sum(hh_g[2:length(hh_g)]) == sum(epf_hh$GASTOT/epf_hh$FACTOR)) {
    print(paste0("UNION is correct"))
  }else{
    print(paste0("UNION is wrong"))
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
  epf_hg$QUINTILE <- Nquantiles(epf_hg$GASTOT_UC2, w = epf_hg$FACTOR , 5)  # Como ya esta ejecutada la funcion, se puede aplicar directamente a los quintiles

  # Create the variables in gender data from the HBS's individuals file
  gender <- epf_hm %>%
    dplyr::group_by ( NUMERO                                                      ) %>%    # MA04          : identification number of the household
    dplyr::summarise( number_male   = sum( SEXO == 1 & EDAD >= 14)                ,        # number_male   : number of male members in the household
                      number_female = sum( SEXO == 6 & EDAD >= 14)                ) %>%    # number_female : number of female members in the household
    dplyr::mutate   ( share_female  = number_female/(number_male + number_female) ) %>%    # perce_female  : share of female members in the household

    dplyr::mutate   ( FEMDEGREE = ifelse(share_female <  0.2                     , "FD1",
                                  ifelse(share_female >= 0.2 & share_female < 0.4, "FD2",
                                  ifelse(share_female >= 0.4 & share_female < 0.6, "FD3",
                                  ifelse(share_female >= 0.6 & share_female < 0.8, "FD4",
                                  ifelse(share_female >= 0.8                      ,"FD5", "Not provided"))))))

  # Merge the data generated at the household level by HA04 (household ID) in hg dataset
  epf_hg <- dplyr::left_join( epf_hg , gender , by = "NUMERO" )


  # **********************************************************************
  # 4. Outputs of the function
  # **********************************************************************

  return(list(epf_hg = epf_hg, epf_hgm = epf_hgm, epf_hc = epf_hc))

}


# add_coicop
#'
#' Details: main function to add coicop categories in the Spanish Household Budget Survey (HBS)
#' @param year year of the HBS you want to use
#' @export
add_coicop <- function(year) {

  # **********************************************************************
  # 1. Load lists with the coicop aggregation we want to use
  # **********************************************************************

  # Convert lists df to vectors

  for (r in colnames(lists)) {
    assign(r, lists %>% dplyr::filter(nchar(get(r))>0) %>% dplyr::pull(r))      # Extrae una columna y se le asigna al nombre de la columna en un vector
  }


  # **********************************************************************
  # 2. Add coicop categories according to de lists aggregation
  # **********************************************************************

  # Add consumption variables
  for (c in coicop) {
    if(c %in% issue_empty){
      eval(parse(text = paste0("epf_hg <- epf_hg %>%
                                dplyr::mutate(", c, " = 0)")))
    } else {
      eval(parse(text = paste0("epf_hg <- epf_hg %>%
                                dplyr::mutate(", c, " = rowSums(dplyr::select(epf_hg, contains(c(", c, ")))))")))
    }
  }

  # Ensure that the sum of expenditure of the categories created matches the original total expenditure of the survey
  if (sum(epf_hg$GASTOT/epf_hg$FACTOR) == sum(rowSums(dplyr::select(epf_hg, contains(coicop))))){
    print(paste0("Add coicop procedure is correct"))
  }


  return(epf_hg)
}


# elevate_hbs
#'
#' Details: main function to elevate the Spanish Household Budget Survey (HBS)
#' @param year year of the HBS you want to elevate
#' @export
elevate_hbs <- function(year, country = "ES") {

  # ************************************************************
  # 1. Adjust HBS and national accounts population
  # ************************************************************

  # Enter population value of the NA: INE census 1 January
  pop_NA <- eurostat::get_eurostat("demo_gind", time_format = "num")
  pop_NA <- dplyr::filter(pop_NA, time == year & geo == country, indic_de == "JAN")
  pop_NA <- pop_NA$values

  # Calculate survey population: sum of households x members
  pop_hbs <- sum(epf_hh$FACTOR*epf_hh$NMIEMB)

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
    dplyr::mutate(coicop_adf = ifelse(COICOP %in% c("EUR_A_073_T", "EUR_A_073_A", "EUR_A_073_M"), coicop_adf[which(COICOP == "EUR_A_073")], coicop_adf))                         # To do: generalozar este proceso a cualquier desagregacion

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
#' Details: main function to apply a specific price shock to the different COICOP categories
#' @param data input data to apply the price shocks
#' @export
price_shock <- function(data) {

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


# impact
#'
#' Details: main function to calculate the distributional impacts based in one or more socioeconomic or demographic variables
#' @param data input data calculate
#' @param var socioeconomic or demographic variable/s for which distributional impacts are calculated. By default: all variables in categories.
#' @param fig generates and saves a figure that summarises the distributional impacts. By default it is F, for the figure to be saved indicate T
#' @export
impact <- function(data, var = categories$categories, fig = F) {

  d_impacts = list()                                                                                        # Generamos una lista vacia
  for (g in var) {
    gastotS_cols <- grep("^GASTOT_s", names(data), value = TRUE)                                           # generamos un vector con todos los nombres que empiecen por GASTOT_s
    assign(paste0('di_',g),                                                                                # asignamos todo lo que se calcula debajo a di_ g (del loop)
           data %>%
             dplyr::group_by(!!dplyr::sym(g)) %>%                                                          # agrupamos por g, sym es para que entienda el valor de g (sustituye el get)
             dplyr::summarise(VARIABLE = g,                                                                # crear las columnas siguientes
                       WEIGHT = sum(FACTOR),
                       dplyr::across(dplyr::all_of(gastotS_cols),                                          # para todas las columnas que estan en gastotS_cols
                                     list(DI_s = ~ (sum(GASTOT_CNR) - sum(.))/sum(GASTOT_CNR)),            # generamos una nueva columna con los impactos distributivos, donde sum(.) es el valor de la columna que estamos usando
                                     .names = "DI_{.col}")) %>%                                            # cambiamos el nombre de la columna añadiendo DI al nombre de columna que esta usando
             dplyr::rename_with(~ gsub("^DI_GASTOT", "DI", .), dplyr::starts_with("DI_GASTOT"))            # cambiamos los nombres de las columnas que empiezen por DI_GASTOT a DI_ solo (gsub es para sustituir y lo segundo para que solo se fije en las que empiezan por DI_GASTOT)
    )
    d_impacts[[paste0('di_',g)]] = get(paste0('di_',g))                                                     # añadir el resultado a la lista con el nombre di_g
  }

  if (fig == T) {

    if (!dir.exists("figures")) {dir.create("figures")}
    for (g in var) {
      datapl <- data[[paste0("di_",g)]] %>%
        tidyr::pivot_longer(cols = dplyr::starts_with("DI_"), names_to = "Scenario", values_to = "Impact") %>%
        dplyr::mutate(Scenario = stringr::str_replace(Scenario, "^DI_", ""))

      pl <- ggplot2::ggplot(datapl, ggplot2::aes(x = !!dplyr::sym(g), y = Impact)) +
        ggplot2::geom_col(position = ggplot2::position_dodge(width = 1)) +
        ggplot2::facet_grid(.~Scenario) +
        ggplot2::labs(y = "Change in welfare (%)", x = g) +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::theme(text = ggplot2::element_text(size = 16))
      save(pl, file = paste0("figures/DI_",g,".png"))
    }
  }

  return(d_impacts)

}

# basic_graph
#'
#' Details: main function to create a graph to summarize the distributional impact based in one or more socioeconomic or demographic variable (one plot per variable)
#' @param data input data calculate
#' @param var socioeconomic or demographic variable/s for which figures are generated. By default: all variables in categories.
#' @export
basic_graph <- function(data, var = categories$categories){
  if (!dir.exists("figures")) {dir.create("figures")}
  for (g in var) {
    datapl <- data[[paste0("di_",g)]] %>%
      tidyr::pivot_longer(cols = dplyr::starts_with("DI_"), names_to = "Scenario", values_to = "Impact") %>%
      dplyr::mutate(Scenario = stringr::str_replace(Scenario, "^DI_", ""))

    pl <- ggplot2::ggplot(datapl, ggplot2::aes(x = !!dplyr::sym(g), y = Impact)) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 1)) +
      ggplot2::facet_grid(.~Scenario) +
      ggplot2::labs(y = "Change in welfare (%)", x = g) +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::theme(text = ggplot2::element_text(size = 16))
    save(pl, file = paste0("figures/DI_",g,".png"))
  }

}

# impact_intersectional
#'
#' Details: main function to calculate the distributional impacts based in one or more socioeconomic or demographic variables
#' @param data input data calculate
#' @param var1 first socioeconomic or demographic variable for which intersectional distributional impact is calculated. By default: all variables of category_a in is_categories.
#' @param var2 second socioeconomic or demographic variable for which intersectional distributional impact is calculated. By default: all variables of category_b in is_categories.
#' @export
impact_intersectional <- function(data, var1 = is_categories$category_a, var2 = is_categories$category_b) {

  is_d_impacts = list()                                                                                        # Generamos una lista vacia
  for (g in var) {
    gastotS_cols <- grep("^GASTOT_s", names(data), value = TRUE)                                           # generamos un vector con todos los nombres que empiecen por GASTOT_s
    assign(paste0('di_',g),                                                                                # asignamos todo lo que se calcula debajo a di_ g (del loop)
           data %>%
             dplyr::group_by(!!dplyr::sym(g)) %>%                                                          # agrupamos por g, sym es para que entienda el valor de g (sustituye el get)
             dplyr::summarise(VARIABLE = g,                                                                # crear las columnas siguientes
                              WEIGHT = sum(FACTOR),
                              dplyr::across(dplyr::all_of(gastotS_cols),                                          # para todas las columnas que estan en gastotS_cols
                                            list(DI_s = ~ (sum(GASTOT_CNR) - sum(.))/sum(GASTOT_CNR)),            # generamos una nueva columna con los impactos distributivos, donde sum(.) es el valor de la columna que estamos usando
                                            .names = "DI_{.col}")) %>%                                            # cambiamos el nombre de la columna añadiendo DI al nombre de columna que esta usando
             dplyr::rename_with(~ gsub("^DI_GASTOT", "DI", .), dplyr::starts_with("DI_GASTOT"))            # cambiamos los nombres de las columnas que empiezen por DI_GASTOT a DI_ solo (gsub es para sustituir y lo segundo para que solo se fije en las que empiezan por DI_GASTOT)
    )
    is_d_impacts[[paste0('di_',g)]] = get(paste0('di_',g))                                                     # añadir el resultado a la lista con el nombre di_g
  }

  return(is_d_impacts)
}

# intersectional_graph
#'
#' Details: main function to create a graph to summarize the distributional impact based in the intersection of two socioeconomic or demographic variables (2 variables per plot)
#' @param data input data calculate
#' @param var1 socioeconomic or demographic variable/s................ By default: all variables in categories
#' @param var2 socioeconomic or demographic variable/s................ By default: all variables in categories
#' @export
intersectional_graph <- function(data, var1 = categories$categories, var2=){
  if (!dir.exists("figures")) {dir.create("figures")}
  for (g in var) {
    datapl <- data[[paste0("di_",g)]] %>%
      tidyr::pivot_longer(cols = dplyr::starts_with("DI_"), names_to = "Scenario", values_to = "Impact") %>%
      dplyr::mutate(Scenario = stringr::str_replace(Scenario, "^DI_", ""))

    pl <- ggplot2::ggplot(datapl, aes(x = !!dplyr::sym(g), y = Impact)) +
      ggplot2::geom_col(position = position_dodge(width = 1)) +
      ggplot2::facet_grid(.~Scenario) +
      ggplot2::labs(y = "Change in welfare (%)", x = element_blank()) +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::theme(legend.title = "g") + #esto no me funciona
      ggplot2::theme(text = element_text(size = 16))
    save(pl, file = paste0("figures/DI_",g,".png"))
  }

}
