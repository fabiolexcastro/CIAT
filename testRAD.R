
# Conversion de datos de Radiación Solar

# Librerias 

library(raster)
library(rgdal)
library(sp)
library(rgeos)
library(maptools)
library(maps)
library(dplyr)
library(gtools)

# Paths

#output_folder <- "D:/CC/_bd/_colombia/_evapotranspiration/_newBD/_current_etp"
output_folder <- "D:/CC/_bd/_colombia/_evapotranspiration/_newBD/_rad_mmDia"

rad_folder    <- "D:/CC/_bd/_colombia/_raster/_worldclim_v2/_30s/_radiacion/_asc"
rad_list      <- list.files(rad_folder, full.names = T, pattern = ".asc$")
rad_list      <- mixedsort(rad_list)
rad_layers    <- lapply(rad_list, FUN = raster) #Units: kJ m-2 day-1 

min(rad_layers[[1]][], na.rm = T)# valor minimo del raster de Radiación para el primer mes del año

cmip5_folder  <- "Y:/_bd/_colombia/_raster/_cmip5/rcp60_extracts/Global_30s/bcc_csm1_1/2020_2049"
temp_list     <- list.files(cmip5_folder, full.names = T, pattern = ".asc$")

tmean         <- grep("tmean", temp_list, value = T)
tmean         <- mixedsort(tmean)
tmean_layers  <- lapply(tmean, FUN = raster)

tmin          <- grep("tmin", temp_list, value = T)
tmin          <- mixedsort(tmin)
tmin_layers   <- lapply(tmin, FUN = raster)

tmax          <- grep("tmax", temp_list, value = T)
tmax          <- mixedsort(tmax)
tmax_layers   <- lapply(tmax, FUN = raster)

min(tmax_layers[[1]][], na.rm = T)


radiacion_mm <- "D:/CC/_bd/_colombia/_evapotranspiration/_newBD/_rad_mmDia"
list_radmm   <- list.files(radiacion_mm, full.names = T, pattern = ".asc$")


# Los valores de temperatura estan multiplicados por 10, por tanto se deben dividir entre 10 para poder operar la formula

summary(tmean_layers[[1]][], na.rm = T)

tmean_layers_ok <- list()
tmin_layers_ok  <- list()
tmax_layers_ok  <- list()

for (i in 1:length(tmean_layers)){
  
  tmean_layers_ok[[i]] <- tmean_layers[[i]]/10
  tmin_layers_ok[[i]]  <- tmin_layers[[i]]/10
  tmax_layers_ok[[i]]  <- tmax_layers[[i]]/10
  
}

summary(tmean_layers_ok[[1]][], na.rm = T)

# -----------------------------------------------------------------------------
# Conversion de valores de radiación solar (descargado de WC Version 2.0)
# -----------------------------------------------------------------------------

# Conversion de Kj a Mj y de Mj a mm/dia

rad_layers_Mj    <- list()
rad_layers_mmdia <- list()

for (i in 1:length(rad_layers)){ 
  
  rad_layers_Mj[[i]]      <- rad_layers[[i]] / 1000
  #rad_layers_mmdia[[i]]  <- (rad_layers_Mj[[i]] * 0.0036) * 0.408
  rad_layers_mmdia[[i]]   <- rad_layers_Mj[[i]] * 0.408 #otro camino según "Esta tabla en Mjulio/m2/dia, para pasar a mm/dia multiplicar por 0.408
} 

min(rad_layers_Mj[[1]][], na.rm = T)
min(rad_layers_mmdia[[1]][], na.rm = T)

# ------------------------------------------------------------------------------------
# Guardado de archivos de Radiación Solar en mm/dia
# ------------------------------------------------------------------------------------


rad_31days    <- rad_layers_mmdia[c(1, 3, 5, 7, 8, 10, 12)] 
rad_31days_ok <- list() 

for (i in 1:length(rad_31days)){
  
  rad_31days_ok[[i]]   <- rad_31days[[i]] * 31 
  
}

# Meses con 30 días
# Abril, junio, septiembre, noviembre [4, 6, 9, 11]

rad_30days    <- rad_layers_mmdia[c(4, 6, 9, 11)] 
rad_30days_ok <- list() 

for (i in 1:length(rad_30days)){
  
  rad_30days_ok[[i]]   <- rad_30days[[i]] *30 
  
}

# Mes con 29 días

rad_29days    <- rad_layers_mmdia[2]
rad_29days_ok <- rad_29days[[1]] * 29

#

rad_monthly <- c(rad_31days_ok[[1]], rad_29days_ok[[1]], rad_31days_ok[[2]], rad_30days_ok[[1]], rad_31days_ok[[3]], rad_30days_ok[[2]], 
                 rad_31days_ok[[4]], rad_31days_ok[[5]], rad_30days_ok[[3]], rad_31days_ok[[6]], rad_30days_ok[[4]], rad_31days_ok[[7]])

names_rad <- paste0("rad_mm_", 1:12, ".asc")



for (i in 1:length(rad_monthly)){ 
  
  writeRaster(rad_monthly[[i]], paste0(output_folder, "/", names_rad[i]))
  
} 

# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
# Calculo de la ETP - ET0 = 0.0023 (tmed + 17.78) R0 * (tmax - tmin) ^ 0.5
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------

# Parte 1 = 0.0023(tmed + 17.78)
# Parte 2 = (tmax - tmin) ^ 0.5
# Parte 3 = Parte 1 * R0 * Parte 2

parte_1    <- list()
parte_2    <- list()
etp_layers <- list()

for (i in 1:length(rad_monthly)){
  
  parte_1[[i]]    <- 0.0023 * (tmean_layers_ok[[i]] + 17.78)
  parte_2[[i]]    <- (tmax_layers[[i]] - tmin_layers[[i]]) ^ 0.5
  etp_layers[[i]] <- parte_1[[i]] * rad_monthly[[i]] * parte_2[[i]]
  
}

stackk <- stack(etp_layers)
annual <- sum(stackk)
plot(annual)

# Meses con 31 días
# Enero, marzo, mayo, julio, agosto, octubre, diciembre [1, 3, 5, 7, 8, 10, 12]

etp_31days    <- etp_layers[c(1, 3, 5, 7, 8, 10, 12)]
etp_31days_ok <- list()

for (i in 1:length(etp_31days)){
  
  etp_31days_ok[[i]]   <- etp_31days[[i]] * 31 
  
}

# Meses con 30 días
# Abril, junio, septiembre, noviembre [4, 6, 9, 11]

etp_30days    <- etp_layers[c(4, 6, 9, 11)]
etp_30days_ok <- list()

for (i in 1:length(etp_30days)){
  
  etp_30days_ok[[i]]   <- etp_30days[[i]] *30 
  
}

# Mes con 29 días

etp_29days    <- etp_layers[2]
etp_29days_ok <- etp_29days[[1]] * 29

# Union de los layers

etp_monthly <- c(etp_31days_ok[[1]], etp_29days_ok[[1]], etp_31days_ok[[2]], etp_30days_ok[[1]], etp_31days_ok[[3]], etp_30days_ok[[2]], 
                 etp_31days_ok[[4]], etp_31days_ok[[5]], etp_30days_ok[[3]], etp_31days_ok[[6]], etp_30days_ok[[4]], etp_31days_ok[[7]])


summary(etp_monthly[[1]][])
min(etp_monthly[[1]][], na.rm = T)

names_etp <- paste0("etp_", 1:12, ".asc")

# Guardado de archivos resultantes

for (i in 1:length(etp_monthly)){ 
  
  writeRaster(etp_monthly[[i]], paste0(output_folder, "/", names_etp[i]))
  
} 



rm(list = ls())
#
