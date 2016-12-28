
library(raster)
library(rgdal)
library(sp)
library(rgeos)
library(maptools)
library(maps)
library(dplyr)
library(gtools)


# --------------------------------------------------------------------------------------------------------
# Funcion de conversión de temperaturas 
# --------------------------------------------------------------------------------------------------------

conv_temp         <- function(tmean, tmin, tmax, ...){
  
  tmean_layers_ok <- list()
  tmin_layers_ok  <- list()
  tmax_layers_ok  <- list()
  
  for(i in 1:12){
    
    tmean_layers_ok[[i]] <- tmean[[i]]/10
    tmax_layers_ok[[i]]  <- tmax[[i]]/10
    tmin_layers_ok[[i]]  <- tmin[[i]]/10
    
  }
  
  out <- list(tmean_layers_ok, tmin_layers_ok, tmax_layers_ok)
  return(out)
  #return(c(tmean_layers_ok, tmax_layers_ok, tmin_layers_ok))
}


# ---------------------------------------------------------------------------------------------------------
# Function de calculo de ETP Global
# ---------------------------------------------------------------------------------------------------------


calc_ETP <- function(tmean_layers, tmax_layers, tmin_layers, rad_layers, ...){
  
  parte_1    <- list()
  parte_2    <- list()
  etp_layers <- list()
  
  for(i in 1:length(rad_layers)){
    
    parte_1[[i]]    <- 0.0023 * (tmean_layers[[i]] + 17.78)
    parte_2[[i]]    <- (tmax_layers[[i]] - tmin_layers[[i]]) ^ 0.5
    etp_layers[[i]] <- parte_1[[i]] * rad_layers[[i]] * parte_2[[i]]
    
  }
  
  # 31 dias
  etp_31days    <- etp_layers[c(1, 3, 5, 7, 8, 10, 12)] 
  etp_31days_ok <- list() 
  
  for (i in 1:length(etp_31days)){
    etp_31days_ok[[i]]   <- etp_31days[[i]] * 31 
  }
  
  # Meses con 30 días # Abril, junio, septiembre, noviembre [4, 6, 9, 11]
  etp_30days    <- etp_layers[c(4, 6, 9, 11)] 
  etp_30days_ok <- list() 
  
  for (i in 1:length(etp_30days)){
    etp_30days_ok[[i]]   <- etp_30days[[i]] *30 
  }
  
  # Mes con 29 días
  etp_29days    <- etp_layers[2]
  etp_29days_ok <- etp_29days[[1]] * 29
  
  etp_monthly <- c(etp_31days_ok[[1]], etp_29days_ok[[1]], etp_31days_ok[[2]], etp_30days_ok[[1]], etp_31days_ok[[3]], etp_30days_ok[[2]], 
                   etp_31days_ok[[4]], etp_31days_ok[[5]], etp_30days_ok[[3]], etp_31days_ok[[6]], etp_30days_ok[[4]], etp_31days_ok[[7]])
  
  return(etp_monthly)
  
}



# --------------------------------------------------------------------------------------------------------
# Funcion de calculo de ETP
# --------------------------------------------------------------------------------------------------------

# calc_ETP <- function(tmean_layers, tmax_layers, tmin_layers, rad_layers, ...){
#   
#   parte_1    <- list()
#   parte_2    <- list()
#   etp_layers <- list()
#   
#   for(i in 1:length(rad_layers)){
#     
#     parte_1[[i]]    <- 0.0023 * (tmean_layers[[i]] + 17.78)
#     parte_2[[i]]    <- (tmax_layers[[i]] - tmin_layers[[i]]) ^ 0.5
#     etp_layers[[i]] <- parte_1[[i]] * rad_layers[[i]] * parte_2[[i]]
#     
#   }
#   
#   return(etp_layers)
#   
# }



