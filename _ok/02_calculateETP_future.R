# Target: calculate the Potential ETP
# Author: Fabio Castro
# CIAT, 2016

# Load libraries

library(raster)
library(rgdal)
library(dplyr)
library(gtools)


source("//mnt/workspace_cluster_8/Coffee_Cocoa/CIAT2016/_bd/_ghana/_codes/_r/_etp/_gha/functionsETP.R")

# Load spatial files

path       <- "//mnt/workspace_cluster_8/Coffee_Cocoa/CIAT2016/_bd/_malawi"
models     <- list.files(paste0(path, "/_raster/_climate/_future_order/_2030"))

files      <- list.files(paste0(path,"/_raster/_climate/_current/_asc"), full.names = T, pattern = ".asc$") %>%
  mixedsort()

years      <- c("_2030", "_2050")

# Files solar radiation

files_rad_mmDia  <- list.files(paste0(path,"/_raster/_climate/_current/_asc"), full.names = T, pattern = ".asc$") %>%
  mixedsort()
files_rad_mmDia  <- grep("rad_mmDia",files, value = T)
layers_rad_mmDia <- lapply(files_rad_mmDia, FUN = raster); rm(files_rad_mmDia)
rad_layers       <- layers_rad_mmDia

# Files temperature

i = 1 

for(i in 1:length(models)){
  
  print(models[i])
  
  files     <- list.files(paste0(path, "/_raster/_climate/_future_order/", years[2], "/", models[i]), full.names = T, pattern = ".asc$") %>%
    mixedsort()
  
  tmean_                  <- grep("tmean", files, value = T)
  tmean_layers            <- lapply(tmean_, FUN = raster)
  
  tmin_                   <- grep("tmin", files, value = T)
  tmin_layers             <- lapply(tmin_, FUN = raster)
  
  tmax_                   <- grep("tmax", files, value = T)
  tmax_layers             <- lapply(tmax_, FUN = raster)
  
  
  out_temp  <- conv_temp(tmean = tmean_layers, tmin = tmin_layers, tmax = tmax_layers)
  
  tmean_ok  <- out_temp[[1]]
  tmin_ok   <- out_temp[[2]]
  tmax_ok   <- out_temp[[3]]
  
  names_etp <- paste0("etp_", 1:12, ".asc")
  
  out_etp   <- calc_ETP(tmean_layers = tmean_ok, tmax_layers = tmax_ok, tmin_layers = tmin_ok, rad_layers = rad_layers)
  
  print("WriteRaster...")
  
  for (j in 1:length(out_etp)){ 
    
    writeRaster(out_etp[[j]], paste0(path, "/_raster/_climate/_future_order/", years[2], "/", models[i], "/", names_etp[j]))
  }
} 




