### Agosto 2016 - Castro & Mesa


## Librerias para trabajar en paralelo
library(foreach)
library(doMC) 
library(raster)
library(dplyr)
library(gtools)
library(rgdal)
library(sp)


### Lineas a Modificar

path <- "/mnt/workspace_cluster_8/Coffee_Cocoa/CIAT2016/_bd/_colombia/_raster/_etpVariables/_etp_rcp60/_asc/"  ## path raster evapatransporation

output <- "/mnt/workspace_cluster_8/Coffee_Cocoa/CIAT2016/_bd/_colombia/_raster/_etpVariables/_bios_etp_rcp60/_asc"  ## output bio ETP (Evapo...)

path_models <- list.dirs(path, recursive = F)
models <- basename(path_models)
year <- c('2020_2049', '2040_2069')

models_by_year <- lapply(path_models, list.dirs, recursive = F)
names(models_by_year) <- models

ETP_5 <- function(etp, ...){
  
  etp_max <- max(etp)
  etp_min <- min(etp)
  range <- etp_max - etp_min
  
  writeRaster(range, ...)
  return(range)
  
}

## Librerias para trabajar en paralelo
library(foreach)
library(doMC) 
registerDoMC(19)  ## For Linux


length_run <- length(models)

pb <- txtProgressBar(max = length_run, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)

opts <- list(progress=progress)

foreach(i = 1:length(models), .packages = c('raster', 'dplyr', 'gtools', 'foreach'), .options.snow=opts, .export = 'ETP_5') %dopar% {
  
  foreach(j = 1:length(year)) %do% {
    
    etp_months <- list.files(models_by_year[[models[i]]][j], full.names = T, pattern = '.asc$') %>% ### models[[1]] esta en la posicion del objeto str models y luego [1] es para el escenario 2049; [2] escenario 2069
      mixedsort()
    
    etp_months_raster <- lapply(etp_months, raster)  %>%
      stack()
    
    ETP_5(etp_months_raster, paste0(output, '/',  models[i],  '/', year[j], '/etp_5.asc'), overwrite=TRUE)
    
  }
  
}
close(pb)