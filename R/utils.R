library(usethis)
library(magrittr)

# standardize
#'
#' Details: function to standarize data names
#' @param data dataset to be standardized
# TO BE REPAIRED BY CLAUDIA ;)
standardize <- function(data) {
  data2 = data %>%
    dplyr::mutate_at(3:ncol(data), as.character) %>%
    tidyr::pivot_longer(cols = 3:ncol(data), names_to = "VAR_EPF") %>%
    dplyr::left_join(mapping, by = c("VAR_EPF","value")) %>%
    dplyr::mutate(VAR = ifelse(is.na(VAR), VAR_EPF, VAR)) %>%
    dplyr::mutate(NOMBRE = ifelse(is.na(NOMBRE), value, NOMBRE)) %>%
    dplyr::select(ANOENC, NUMERO, VAR, NOMBRE) %>%
    tidyr::pivot_wider(names_from = NOMBRE, values_from = VAR)
  return(data2)
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

