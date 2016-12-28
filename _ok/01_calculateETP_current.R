# Target: calculate the Potential ETP
# Author: Fabio Castro
# CIAT, 2016

# Load libraries

library(raster)
library(rgdal)
library(dplyr)
library(gtools)

# Load spatial files

path       <- "Y:/_bd/_civ"
files      <- list.files(paste0(path,"/_raster/_current/_asc"), full.names = T, pattern = ".asc$") %>%
  mixedsort()

# Files temperature

files_tmean <- grep("tmean_", files, value = T)
files_tmin  <- grep("tmin_", files, value = T)
files_tmax  <- grep("tmax_", files, value = T)

layers_tmean <- lapply(files_tmean, FUN = raster); rm(files_tmean)
layers_tmin  <- lapply(files_tmin, FUN = raster); rm(files_tmin)
layers_tmax  <- lapply(files_tmax, FUN = raster); rm(files_tmax)

# Files rad_s

files_rad_mmDia  <- grep("rad_mmDia",files, value = T)
layers_rad_mmDia <- lapply(files_rad_mmDia, FUN = raster); rm(files_rad_mmDia); 

# Divide the temperature between 10

layers_tmean_ok <- list()
layers_tmax_ok  <- list()
layers_tmin_ok  <- list()

for(i in 1:length(layers_tmean)){
  
  layers_tmean_ok[[i]] <- layers_tmean[[i]]/10
  layers_tmax_ok[[i]]  <- layers_tmax[[i]]/10
  layers_tmin_ok[[i]]  <- layers_tmin[[i]]/10
  
}

rm(layers_tmean); rm(layers_tmin); rm(layers_tmax)

# Calculate of Potential ETP

parte_1    <- list()
parte_2    <- list()
etp_layers <- list()

for (i in 1:length(layers_rad_mmDia)){
  
  parte_1[[i]]    <- 0.0023 * (layers_tmean_ok[[i]] + 17.78)
  parte_2[[i]]    <- (layers_tmax_ok[[i]] - layers_tmin_ok[[i]]) ^ 0.5
  etp_layers[[i]] <- parte_1[[i]] * layers_rad_mmDia[[i]] * parte_2[[i]]
  
}

# Meses con 31 días # Enero, marzo, mayo, julio, agosto, octubre, diciembre [1, 3, 5, 7, 8, 10, 12]

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

# Union de los layers

etp_monthly <- c(etp_31days_ok[[1]], etp_29days_ok[[1]], etp_31days_ok[[2]], etp_30days_ok[[1]], etp_31days_ok[[3]], etp_30days_ok[[2]], 
                 etp_31days_ok[[4]], etp_31days_ok[[5]], etp_30days_ok[[3]], etp_31days_ok[[6]], etp_30days_ok[[4]], etp_31days_ok[[7]])

summary(etp_monthly[[1]][])
min(etp_monthly[[1]][], na.rm = T)

names_etp <- paste0("etp_", 1:12, ".asc")

# Guardado de archivos resultantes

for (i in 1:length(etp_monthly)){ 
  
  writeRaster(etp_monthly[[i]], paste0(path, "/_raster/_current/_asc/", names_etp[i]))
  
} 

