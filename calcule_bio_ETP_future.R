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

path <- "//dapadfs/workspace_cluster_8/Coffee_Cocoa/CIAT2016/_bd/_colombia/_raster/_etpVariables/_etp_rcp60/_asc/"  ## path raster evapatransporation

#"D:/CC/_bd/_colombia/_evapotranspiration/_newBD/_rad_mmDia"
 
output <- "//dapadfs/workspace_cluster_8/Coffee_Cocoa/CIAT2016/_bd/_colombia/_raster/_etpVariables/_bios_etp_rcp60/_asc"  ## output bio ETP (Evapo...)

#output <- "//dapadfs/workspace_cluster_8/Coffee_Cocoa/CIAT2016/_bd/_colombia/_raster/_etpVariables/_bios_etp_rcp60/_asc"

path_models <- list.dirs(path, recursive = F)
models <- basename(path_models)
year <- c('2020_2049', '2040_2069')

models_by_year <- lapply(path_models, list.dirs, recursive = F)
names(models_by_year) <- models


## Librerias para trabajar en paralelo
library(foreach)
library(doSNOW) 

cl <- makeCluster(3)
registerDoSNOW(cl)  ## For Windows


length_run <- length(models)

pb <- txtProgressBar(max = length_run, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)

opts <- list(progress=progress)

foreach(i = 1:length(models), .packages = c('raster', 'dplyr', 'gtools', 'foreach'), .options.snow=opts, .export = 'ETP_3') %dopar% {
  
  foreach(j = 1:length(year)) %do% {
    
    etp_months <- list.files(models_by_year[[models[i]]][j], full.names = T, pattern = '.asc$') %>% ### models[[1]] esta en la posicion del objeto str models y luego [1] es para el escenario 2049; [2] escenario 2069
      mixedsort()
    
    etp_months_raster <- lapply(etp_months, raster)  %>%
      stack()
    
    ETP_3(etp_months_raster, paste0(output, '/',  models[i],  '/', year[j], '/etp_3.asc'))
    
  }
  
}


close(pb)
stopCluster(cl)

ETP_1 <- function(x, filename, return_, ...){
  
  out <- raster(x)
  big <- ! canProcessInMemory(out, 3)
  filename <- trim(filename)
  
  if (big & filename == '') {
    
    filename <- rasterTmpFile()
    
  }
  
  if (filename != '') {
    
    out <- writeStart(out, filename, ...)
    todisk <- TRUE
    
  } else {
    vv <- matrix(ncol=nrow(out), nrow=ncol(out))
    todisk <- FALSE
  }
  
  bs <- blockSize(x)
  
  pb <- pbCreate(bs$n, ...)
  
  if (todisk) {
    
    for (i in 1:bs$n) {
      v <- getValues(x, row=bs$row[i], nrows=bs$nrows[i] )
      
      v <- apply(v, 1, sum)  ## Linea a Cambiar
      
      out <- writeValues(out, v, bs$row[i])
      pbStep(pb, i)
      
    }
    
    out <- writeStop(out)
  } else {
    
    for (i in 1:bs$n) {
      
      v <- getValues(x, row=bs$row[i], nrows=bs$nrows[i] )
      v <- apply(v, 1, sum) ## Linea a Cambiar
      cols <- bs$row[i]:(bs$row[i]+bs$nrows[i]-1)
      vv[,cols] <- matrix(v, nrow=out@ncols)
      pbStep(pb, i)
      
    }
    out <- setValues(out, as.vector(vv))
    
  }
  
  pbClose(pb)
  return(out)
}


##############################
### Calculo Seasonality CV 
##############################

ETP_2 <- function(etp, ...){
  
  etp_mean <- mean(etp)
  etp_sd   <- calc(etp, sd, na.rm=TRUE)
  etp_cv   <- (etp_sd/etp_mean)*100
  
  writeRaster(etp_cv, ...)
  return(etp_cv)
  
}

##############################
### Calculo ETP Max
##############################

ETP_3 <- function(etp, ...){
  
  etp <- max(etp)
  writeRaster(etp, ...)
  return(etp)
  
}

##############################
### Calculo ETP Min
##############################

ETP_4 <- function(etp, ...){
  
  etp <- min(etp)
  writeRaster(etp, ...)
  return(etp)
  
}

##############################
### Calculo ETP Range(max-min)
##############################

ETP_5 <- function(etp, ...){
  
  etp_max <- max(etp)
  etp_min <- min(etp)
  range <- etp_max - etp_min
  
  writeRaster(range, ...)
  return(range)
  
}