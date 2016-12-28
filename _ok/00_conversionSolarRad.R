# Target: calculate tha incident solar radiation
# Author: Fabio Castro
# CIAT, 2016

# Load libraries

library(raster)
library(rgdal)
library(dplyr)
library(gtools)

# Load spatial files

path       <- "Y:/_bd/_malawi"
files      <- list.files(paste0(path,"/_raster/_climate/_current/_asc"), full.names = T, pattern = ".asc$") %>%
  mixedsort()

files_rad  <- grep("rad_", files, value = T)
layers_rad <- lapply(files_rad, FUN = raster); rm(files_rad)

# Layers radiation solar  (kJ m-2 day-1)

rad_Mj    <- list()
rad_mmDia <- list()

for(i in 1:length(layers_rad)){
 
  rad_Mj[[i]]    <- layers_rad[[i]] / 1000
  rad_mmDia[[i]] <- rad_Mj[[i]] * 0.408

}

# Write raster

names_rad_mmDia <- paste0("rad_mmDia_", 1:12)

for(i in 1:length(rad_mmDia)){
  
 writeRaster(rad_mmDia[[i]], paste0(path, "/_raster/_climate/_current/_asc/", names_rad_mmDia[i], ".asc"))
  
}






