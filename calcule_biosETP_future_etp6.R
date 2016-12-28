### Agosto 2016 - Castro & Mesa

library(gtools)
library(rgdal)
library(sp)
library(raster)
library(dplyr)
library(usdm)
library(maps)
library(SDMTools)
library(maptools)
library(stringr)

## Librerias para trabajar en paralelo
library(foreach)
library(doSNOW) 

### Lineas a Modificar

#path <- "//dapadfs/workspace_cluster_8/Coffee_Cocoa/CIAT2016/_bd/_colombia/_raster/_etpVariables/_etp_rcp60/_asc/"  ## path raster evapatransporation
path           <- "/mnt/workspace_cluster_8/Coffee_Cocoa/CIAT2016/_bd/_colombia/_raster/_etpVariables/_new/_etp/_future/_monthly"

#output <- "//dapadfs/workspace_cluster_8/Coffee_Cocoa/CIAT2016/_bd/_colombia/_raster/_etpVariables/_bios_etp_rcp60/_asc"  ## output bio ETP (Evapo...)
output         <- "/mnt/workspace_cluster_8/Coffee_Cocoa/CIAT2016/_bd/_colombia/_raster/_etpVariables/_new/_etp/_future/_bios"

#path_prec <- "//dapadfs/workspace_cluster_8/Coffee_Cocoa/CIAT2016/_bd/_colombia/_raster/_cmip5/_prec"
path_prec <- "/mnt/workspace_cluster_8/Coffee_Cocoa/CIAT2016/_bd/_colombia/_raster/_cmip5/_prec"

path_models <- list.dirs(path, recursive = F)

path_models_prec <- list.dirs(path_prec, recursive = F)

models <- basename(path_models)
year <- c('2020_2049', '2040_2069')

models_by_year <- lapply(path_models, list.dirs, recursive = F)
names(models_by_year) <- models

models_by_year_prec <- lapply(path_models_prec, list.dirs, recursive = F)
names(models_by_year_prec) <- models

## Librerias para trabajar en paralelo
library(foreach)
library(doSNOW) 

library(foreach)
library(doMC)

registerDoMC(19)  ## For Linux


cl <- makeCluster(3)
registerDoSNOW(cl)  ## For Windows


length_run <- length(models)

pb <- txtProgressBar(max = length_run, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)

opts <- list(progress=progress)

foreach(i = 1:length(models), .packages = c('raster', 'dplyr', 'gtools', 'foreach'), .options.snow=opts, .export = 'ETP_6') %dopar% {
  
  foreach(j = 1:length(year)) %do% {
    
    etp_months <- list.files(models_by_year[[models[i]]][j], full.names = T, pattern = '.asc$') %>% ### models[[1]] esta en la posicion del objeto str models y luego [1] es para el escenario 2049; [2] escenario 2069
      mixedsort()
    
    prec_months <- list.files(models_by_year_prec[[models[i]]][j], full.names = T, pattern = '.asc$') %>% ### models[[1]] esta en la posicion del objeto str models y luego [1] es para el escenario 2049; [2] escenario 2069
      mixedsort()  ### hacer subset
    
        #etp_months_raster <- lapply(etp_months, raster)  %>%
    #stack()
    
    rlist2 <- lapply(etp_months, raster)  #%>%
      #stack()
    
    #prec_months_raster <- lapply(prec_months, raster)  %>%
    #stack()
    
    rlist <- lapply(prec_months, raster) # %>%
      #stack()
    
    
    #ETP_3(etp_months_raster, paste0(output, '/',  models[i],  '/', year[j], '/etp_3.asc'))
    
    outfile = paste0(output, '/',  models[i],  '/', year[j], '/etp_6.asc')
    format = 'ascii'
    
    ETP_6(rlist, rlist2, outfile, format)
  }
  
}

