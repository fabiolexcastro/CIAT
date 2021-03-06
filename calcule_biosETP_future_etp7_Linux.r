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
library(doMC) ##For Linux

### Lineas a Modificar

#path <- "/mnt/workspace_cluster_8/Coffee_Cocoa/CIAT2016/_bd/_colombia/_raster/_etpVariables/_etp_rcp60/_asc/"  ## path raster evapatransporation
path           <- "/mnt/workspace_cluster_8/Coffee_Cocoa/CIAT2016/_bd/_colombia/_raster/_etpVariables/_new/_etp/_future/_monthly"

#output <- "/mnt/workspace_cluster_8/Coffee_Cocoa/CIAT2016/_bd/_colombia/_raster/_etpVariables/_bios_etp_rcp60/_asc"  ## output bio ETP (Evapo...)
output         <- "/mnt/workspace_cluster_8/Coffee_Cocoa/CIAT2016/_bd/_colombia/_raster/_etpVariables/_new/_etp/_future/_bios"

path_prec <- "/mnt/workspace_cluster_8/Coffee_Cocoa/CIAT2016/_bd/_colombia/_raster/_cmip5/_prec"

path_models <- list.dirs(path, recursive = F)

path_models_prec <- list.dirs(path_prec, recursive = F)

models <- basename(path_models)
year <- c('2020_2049', '2040_2069')

models_by_year <- lapply(path_models, list.dirs, recursive = F)
names(models_by_year) <- models

models_by_year_prec <- lapply(path_models_prec, list.dirs, recursive = F)
names(models_by_year_prec) <- models


##################################################
######### Funcion ################################
##################################################

ETP_7 <- function(rlist, rlist2, outfile, format='') {
  
  if (!is.list(rlist)) {
    stop('First argument should be a list or rasters (prec)')
  } else if (!is.list(rlist2)) {
    stop('Second argument should be a list or rasters (ETP)')
  }
  
  if (!file.exists(outfile)) {
    cat("", "\n", "ETP Temperature of Driest Quarter (P7)", "\n")
    
    PpTaStack <- stack(c(rlist, rlist2))
    
    p7fun <- function(DataPixel) {
      if(is.na(DataPixel[1])) {
        return(NA)
      } else {
        
        q1 <- -9999
        mnt12 <- -9999
        
        for (wm in 1:12) {
          i <- wm
          j <- wm + 1
          k <- wm + 2
          
          PptDataPixel <- DataPixel[1:12]
          TavDataPixel <- DataPixel[13:24]
          
          if (j > 12) {j <- j-12}
          if (k > 12) {k <- k-12}
          
          assign(paste("q", wm, sep=""), PptDataPixel[i] + PptDataPixel[j] + PptDataPixel[k])
          assign(paste("t", wm, sep=""), TavDataPixel[i] + TavDataPixel[j] + TavDataPixel[k])
        }
        
        mnt1 <- 1
        dry1 <- q1
        
        for (wm in 1:11) {
          j <- wm + 1
          assign(paste("mnt", j, sep=""), if (get(paste("q", j, sep="")) < get(paste("dry", wm, sep=""))) {j} else {get(paste("mnt", wm, sep=""))})
          assign(paste("dry", j, sep=""), if (get(paste("q", j, sep="")) < get(paste("dry", wm, sep=""))) {get(paste("q", j, sep=""))} else {get(paste("dry", wm, sep=""))})
        }
        drym <- mnt12
        
        for (wm in 1:12) {
          assign(paste("yy", wm, sep=""), if (drym == wm) {get(paste("t", wm, sep=""))} else {-9999})
        }
        res <- round(max(yy1,yy2,yy3,yy4,yy5,yy6,yy7,yy8,yy9,yy10,yy11,yy12) / 3)
        return(res)
      }
    }
    
    coordenadas <- rasterToPoints(PpTaStack[[1]])[, 1:2]
    
    value_point <- as.matrix(rbind.data.frame(coordenadas))
    colnames(value_point) <- c('x', 'y')
    
    rowVals <- raster::extract(PpTaStack, value_point)
    
    RasVals <- apply(rowVals, 1, p7fun) 
    
    p7 <- PpTaStack[[1]]
    pos_with_values <-  which(!is.na(p7[]))
    
    p7[pos_with_values] <- RasVals
    names(p7) <- 'ETP_7'
    
    p7 <- writeRaster(p7, outfile, format = format, overwrite = TRUE)
    rm(PpTaStack)
    #} else {
    #	cat("", "\n", "File bio_9 already exists, skipping calculation, but loading", "\n")
    #	p9 <- raster(outfile)
    #}
    return(p7)
  }
}

##################################################
## Librerias para trabajar en paralelo
##################################################

library(foreach)

registerDoMC(19)  ## For Linux

length_run <- length(models)

pb <- txtProgressBar(max = length_run, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)

opts <- list(progress=progress)

foreach(i = 1:length(models), .packages = c('raster', 'dplyr', 'gtools', 'foreach'), .options.snow=opts, .export = 'ETP_7') %dopar% {
  
  foreach(j = 1:length(year)) %do% {
    
    etp_months <- list.files(models_by_year[[models[i]]][j], full.names = T, pattern = '.asc$') %>% ### models[[1]] esta en la posicion del objeto str models y luego [1] es para el escenario 2049; [2] escenario 2069
      mixedsort()
    
    prec_months <- list.files(models_by_year_prec[[models[i]]][j], full.names = T, pattern = '.asc$') %>% ### models[[1]] esta en la posicion del objeto str models y luego [1] es para el escenario 2049; [2] escenario 2069
      mixedsort()  ### hacer subset
    
    rlist2 <- lapply(etp_months, raster)  
            
    rlist <- lapply(prec_months, raster) 
    
    #outfile = paste0(output, '/',  models[i],  '/', year[j], '/etp_7.asc')
	outfile = paste0(output, '/',  models[i],  '/', year[j], '/bio_27.asc')
    format = 'ascii'
    
    ETP_7(rlist, rlist2, outfile, format)
  }
  
}