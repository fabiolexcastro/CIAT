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
library(doMC) ## For Linux
library(doSNOW) ## For Windows

### Lineas a Modificar

path           <- "/mnt/workspace_cluster_8/Coffee_Cocoa/CIAT2016/_bd/_colombia/_raster/_etpVariables/_new/_etp/_future/_monthly" 
output         <- "/mnt/workspace_cluster_8/Coffee_Cocoa/CIAT2016/_bd/_colombia/_raster/_etpVariables/_new/_etp/_future/_bios" #new

#path_prec <- "//dapadfs/workspace_cluster_8/Coffee_Cocoa/CIAT2016/_bd/_colombia/_raster/_cmip5/_prec"

path_tmean <- "/mnt/workspace_cluster_8/Coffee_Cocoa/CIAT2016/_bd/_colombia/_raster/_cmip5/rcp60_extracts/Global_30s"
#path_tmean <- "//dapadfs/workspace_cluster_8/Coffee_Cocoa/CIAT2016/_bd/_colombia/_raster/_cmip5/rcp60_extracts/Global_30s" #Windows
#path_tmean <- "/mnt/workspace_cluster_8/Coffee_Cocoa/CIAT2016/_bd/_colombia/_raster/_cmip5/rcp60_extracts/Global_30s" #Linux

path_models <- list.dirs(path, recursive = F)

#path_models_prec <- list.dirs(path_prec, recursive = F)
path_models_tmean <- list.dirs(path_tmean, recursive = F)

models <- basename(path_models)
year <- c('2020_2049', '2040_2069')

models_by_year <- lapply(path_models, list.dirs, recursive = F)
names(models_by_year) <- models

#models_by_year_prec <- lapply(path_models_prec, list.dirs, recursive = F)
#names(models_by_year_prec) <- models

models_by_year_tmean <- lapply(path_models_tmean, list.dirs, recursive = F)
names(models_by_year_tmean) <- models

ETP_9 <- function(rlist, rlist2, outfile, format='') {
  
  if (!is.list(rlist)) {
    stop('First argument should be a list or rasters (ETP)')
  } else if (!is.list(rlist2)) {
    stop('Second argument should be a list or rasters (tmean)')
  }
  
  if (!file.exists(outfile)) {
    cat("", "\n", "ETP of Coldest Quarter (ETP9)", "\n")
    
    PpTaStack <- stack(c(rlist, rlist2))
    
    p9fun <- function(DataPixel) {
      if(is.na(DataPixel[1])) {
        return(NA)
      } else {
        
        t1 <- -9999
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
        cld1 <- t1
        
        for (wm in 1:11) {
          j <- wm + 1
          assign(paste("mnt", j, sep=""), if (get(paste("t", j, sep="")) < get(paste("cld", wm, sep=""))) {j} else {get(paste("mnt", wm, sep=""))})
          assign(paste("cld", j, sep=""), if (get(paste("t", j, sep="")) < get(paste("cld", wm, sep=""))) {get(paste("t", j, sep=""))} else {get(paste("cld", wm, sep=""))})
        }
        cldm <- mnt12
        
        for (wm in 1:12) {
          assign(paste("yy", wm, sep=""), if (cldm == wm) {get(paste("q", wm, sep=""))} else {-9999})
        }
        res <- round(max(yy1,yy2,yy3,yy4,yy5,yy6,yy7,yy8,yy9,yy10,yy11,yy12)/3)
        return(res)
      }
    }
    
    coordenadas <- rasterToPoints(PpTaStack[[1]])[, 1:2]
    
    value_point <- as.matrix(rbind.data.frame(coordenadas))
    colnames(value_point) <- c('x', 'y')
    
    rowVals <- cbind(value_point, raster::extract(PpTaStack, value_point))
    rowVals <- rowVals[complete.cases(rowVals),]
    colnames(rowVals)[1:2] <- c('x', 'y')
    
    ######## Lineas para revisar
    # for(i in 1:nrow(rowVals)){
    #   p9fun(DataPixel = rowVals[i,])
    #   print(i)
    # }
    # sum(complete.cases(rowVals)==FALSE)/nrow(rowVals)
    
    RasVals <- apply(rowVals[,3:ncol(rowVals)], 1, p9fun) ## esto se puede paralelizar
    
    p9 <- PpTaStack[[1]] #raster guia
    p9[] <- NA
    pos_with_values <- cellFromXY(object=p9, xy=rowVals[,1:2])
    p9[pos_with_values] <- RasVals
    names(p9) <- 'ETP_9'
    
    p9 <- writeRaster(p9, outfile, format = format, overwrite = TRUE)
    
  }
  
  #} else {
  #cat("", "\n", "File bio_19 already exists, skipping calculation, but loading", "\n")
  #p19 <- raster(outfile)
  return(p9)
  
}

##################################################
## Librerias para trabajar en paralelo
##################################################

library(foreach)
library(doSNOW) ## For Windows

cl <- makeCluster(3) ## For Windows
registerDoSNOW(cl)  ## For Windows

registerDoMC(19)  ## For Linux

length_run <- length(models)

pb <- txtProgressBar(max = length_run, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)

opts <- list(progress=progress)

#i = 1; j = 1 #prueba de escritorio
#.options.snow=opts #iba en la siguiente linea

foreach(i = 1:length(models), .packages = c('raster', 'dplyr', 'gtools', 'foreach', 'stringr'), .export = 'ETP_9') %dopar% {
  
  foreach(j = 1:length(year)) %do% {
    
    etp_months <- list.files(models_by_year[[models[i]]][j], full.names = T, pattern = '.asc$') %>% ### models[[1]] esta en la posicion del objeto str models y luego [1] es para el escenario 2049; [2] escenario 2069
      mixedsort()
    
    tmean_months <- list.files(models_by_year_tmean[[models[i]]][j], full.names = T, pattern = '.asc$') %>% ### models[[1]] esta en la posicion del objeto str models y luego [1] es para el escenario 2049; [2] escenario 2069
      mixedsort() ### hacer subset
    
    tmean_months <- subset(tmean_months, str_detect(tmean_months,"tmean")) #escoge solamente las variables tmena
    
    rlist2 <- lapply(etp_months, raster)  
    
    rlist <- lapply(tmean_months, raster) 
    
    outfile = paste0(output, '/',  models[i],  '/', year[j], '/bio_29.asc')
    
    format = 'ascii'
    
    ETP_9(rlist, rlist2, outfile, format)
    
  }
}    




