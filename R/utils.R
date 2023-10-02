library(usethis)
library(magrittr)


# rename_values
#'
#' Details: function to rename the codified values of the dataset to the meaningful values detailed in the mapping
#' @param data dataset to be standardized
#' @param current_var column name to be standardized
rename_values = function(data, current_var) {
  data = data %>%
    dplyr::rename(value = {{ current_var }}) %>%                             # renombra la columna (nombre de la variable) a value
    dplyr::mutate(value = as.character(value)) %>%                           # te convierte en caracter
    dplyr::left_join(mapping %>%                                             # te junta los dos df (mapping y data)
                       dplyr::filter(VAR == current_var) %>%                 # pero solo coge la parte que nos interesa (solo cuando el valor de la variable es igual a current_var)
                       dplyr::select(value, NOMBRE), by = "value") %>%       # se queda solo con las columnas que nos interesan (value y nombre)
    dplyr::select(-value) %>%                                                # te elimina la columna value
    dplyr::rename_with(~current_var, 'NOMBRE')                               # te renombra de nombre a current_var

  return(data)
}


# standardize
#'
#' Details: function to standarize data names
#' @param data dataset to be standardized
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

  # rename values' codes to values' names
  for (cc in intersect(unique(mapping$VAR), new_names)) {                   # para todas las columnas donde se puede hacer mapping de la variables sobreescribimos el dataset standarizado (con lunción rename_value)
    data = rename_values(data, cc)
  }

  return(data)
}


# load_epf
#'
#' Details: main function to load the Spanish Household Budget Survey (HBS)
#' @param year year of the HBS you want to load
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

  return(list(epf_hg = epf_hg, epf_hgm = epf_hgm, epf_hc = epf_hc))

}

