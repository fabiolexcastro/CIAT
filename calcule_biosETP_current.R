##########################################################
###########################################################
##### Calculate BioVariables ETPP
###########################################################
###########################################################

### Agosto 2016 -  

require(rgdal); require(sp); require(raster); require(dplyr); require(usdm); require(maps); library(SDMTools); library(maptools)

#path <- "D:/CC/_bd/_colombia/_evapotranspiration/_etp_worldclim_v1/_asc/_monthly/" raw
path <- "D:/CC/_bd/_colombia/_model/_cormacarena/_input/_variables/_etp/_asc/_rcp85/"

#path_output <- "D:/CC/_bd/_colombia/_evapotranspiration/_etp_worldclim_v1/_asc/_bios_etp" #raw
path_output <- "D:/CC/_bd/_colombia/_model/_cormacarena/_input/_variables/_etp/_bios_etp/_rcp85"
  
etp <- paste0(path, "/etp_m_", 1:12, ".asc")
etp_layers <- lapply(etp, FUN = raster)

etp_layer_st <- stack(etp_layers)
plot(etp_layer_st)

#path_prec <- "D:/CC/_bd/_colombia/_raster/_worldclim_v1/_30s/_asc" #raw
path_prec <- "D:/CC/_bd/_colombia/_model/_cormacarena/_input/_variables/_variables/_tif/_rcp85/_asc"
prec <- paste0(path_prec, "/prec_", 1:12, ".asc")
prec <- lapply(prec, FUN = raster) 

#path_tmean <- "D:/CC/_bd/_colombia/_raster/_worldclim_v1/_30s/_asc" #raw
path_tmean <- "D:/CC/_bd/_colombia/_model/_cormacarena/_input/_variables/_variables/_tif/_rcp85/_asc"
tmean <- paste0(path_tmean, "/tmean_", 1:12, ".asc")
tmean <- lapply(tmean, FUN = raster) 


###################################
### Calculo Annual ETP
##############################

annual_pet <- sum(etp_layer_st)
plot(annual_pet)
writeRaster(annual_pet, paste0(path_output, "/annual_etp.asc"))

##############################
### Calculo Seasonality CV 
##############################

etp_mean <- mean(etp_layer_st)
etp_sd   <- calc(etp_layer_st, sd, na.rm=TRUE)
etp_cv   <- (etp_sd/etp_mean)*100

plot(etp_sd)
#min(etp_sd[], na.rm = TRUE)

writeRaster(etp_cv, paste0(path_output, "/etp_cv.asc"))

##############################
### Calculo ETP Max
##############################

etp_max <- max(etp_layer_st)
plot(etp_max)

writeRaster(etp_max, paste0(path_output, "/etp_max.asc"))

##############################
### Calculo ETP Min
##############################

etp_min <- min(etp_layer_st)
plot(etp_min)

writeRaster(etp_min, paste0(path_output, "/etp_min.asc"))

##############################
### Calculo ETP Range(max-min)
##############################

range <- etp_max-etp_min

writeRaster(range, paste0(path_output, "/etp_range.asc"))

###################################
### Calculo ETP of wettest quarter (trimestre más húmedo (con mayor ppt)) #package Bioclim
###################################

rlist <- prec
rlist2 <- etp_layers
outfile <- 'D:/CC/_bd/_colombia/_model/_cormacarena/_input/_variables/_etp/_bios_etp/_rcp85/ETP8.asc'
format <- 'ascii'

# fileoutput <- "D:/CC/_bd/_colombia/_evapotranspiration/_etp_worldclim_v1/_asc/_bios_etp/ETP8.asc"
# rasterETP <- raster(fileoutput)
# plot(rasterETP)

ETP8Calc <- function(rlist, rlist2, outfile, format='') {
  
  if (!is.list(rlist)) {
    stop('First argument should be a list or rasters (prec)')
  } else if (!is.list(rlist2)) {
    stop('Second argument should be a list or rasters (ETP)')
  }
  
  if (!file.exists(outfile)) {
    cat("", "\n", "ETP of wettest quarter (ETP8)", "\n")
    
    
    
    PpTaStack <- stack(c(rlist, rlist2))
    
    p8fun <- function(DataPixel) {
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
        wet1 <- q1
        
        for (wm in 1:11) {
          j <- wm + 1
          assign(paste("mnt", j, sep=""), if (get(paste("q", j, sep="")) > get(paste("wet", wm, sep=""))) {j} else {get(paste("mnt", wm, sep=""))})
          assign(paste("wet", j, sep=""), if (get(paste("q", j, sep="")) > get(paste("wet", wm, sep=""))) {get(paste("q", j, sep=""))} else {get(paste("wet", wm, sep=""))})
        }
        wetm <- mnt12
        
        for (wm in 1:12) {
          assign(paste("xx", wm, sep=""), if (wetm == wm) {get(paste("t", wm, sep=""))} else {-9999})
        }
        res <- round(max(xx1,xx2,xx3,xx4,xx5,xx6,xx7,xx8,xx9,xx10,xx11,xx12) / 3)
        return(res)
      }
    }
    
    
      coordenadas <- rasterToPoints(PpTaStack[[1]])[, 1:2]
    
      value_point <- as.matrix(rbind.data.frame(coordenadas))
      colnames(value_point) <- c('x', 'y')
      
      rowVals <- raster::extract(PpTaStack, value_point)
      
      RasVals <- apply(rowVals, 1, p8fun) ## esto se puede paralelizar 
      
      ## Como tarea Fabio investigar sobre la mierda de pbCreate
      
      p8 <- PpTaStack[[1]]
      pos_with_values <-  which(!is.na(p8[]))
      
      p8[pos_with_values] <- RasVals
      names(p8) <- 'ETP_8'
    
    p8 <- writeRaster(p8, outfile, format = format, overwrite = TRUE)
    
    rm(PpTaStack)
  # } else {
  #   cat("", "\n", "File ETP_8 already exists, skipping calculation, but loading", "\n")
  #   p8 <- raster(outfile)
  # }
  return(p8)
  }
  
}

#outfile <- 'D:/CC/_bd/_colombia/_evapotranspiration/_etp_worldclim_v1/_asc/_bios_etp/ETP8.asc'

ETP8Calc(prec, etp_layers, outfile, format = 'ascii')

#############################################
### Calculo ETP of driest quarter ETP 9 
#############################################

rlist <- prec
rlist2 <- etp_layers
outfile <- 'D:/CC/_bd/_colombia/_model/_cormacarena/_input/_variables/_etp/_bios_etp/_rcp85/ETP9.asc'
format <- 'ascii'

ETP9Calc <- function(rlist, rlist2, outfile, format='') {
  
  if (!is.list(rlist)) {
    stop('First argument should be a list or rasters (prec)')
  } else if (!is.list(rlist2)) {
    stop('Second argument should be a list or rasters (ETP)')
  }
  
  if (!file.exists(outfile)) {
    cat("", "\n", "ETP Temperature of Driest Quarter (P9)", "\n")
    
    PpTaStack <- stack(c(rlist, rlist2))
    
    p9fun <- function(DataPixel) {
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
    
    RasVals <- apply(rowVals, 1, p9fun) ## esto se puede paralelizar
    
    p9 <- PpTaStack[[1]]
    pos_with_values <-  which(!is.na(p9[]))
    
    p9[pos_with_values] <- RasVals
    names(p9) <- 'ETP_9'
    
    p9 <- writeRaster(p9, outfile, format = format, overwrite = TRUE)
    rm(PpTaStack)
    #} else {
    #	cat("", "\n", "File bio_9 already exists, skipping calculation, but loading", "\n")
    #	p9 <- raster(outfile)
    #}
    return(p9)
  }
}

#outfile <- 'D:/CC/_bd/_colombia/_evapotranspiration/_etp_worldclim_v1/_asc/_bios_etp/ETP9.asc'

ETP9Calc(prec, etp_layers, outfile, format = 'ascii')


#################################################
### Calculo ETP of warmest quarter ETP 10
#################################################

rlist <- etp_layers
rlist2 <- tmean
outfile <- 'D:/CC/_bd/_colombia/_model/_cormacarena/_input/_variables/_etp/_bios_etp/_rcp85/ETP10.asc'
format <- 'ascii'

ETP10Calc <- function(rlist, rlist2, outfile, format='') {
  
  if (!is.list(rlist)) {
    stop('First argument should be a list or rasters (ETP)')
  } else if (!is.list(rlist2)) {
    stop('Second argument should be a list or rasters (tmean)')
  }
  
  if (!file.exists(outfile)) {
    cat("", "\n", "ETP of Warmest Quarter (ETP)", "\n")
    
    PpTaStack <- stack(c(rlist, rlist2))
    
    p10fun <- function(DataPixel) {
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
        hot1 <- t1
        
        for (wm in 1:11) {
          j <- wm + 1
          assign(paste("mnt", j, sep=""), if (get(paste("t", j, sep="")) > get(paste("hot", wm, sep=""))) {j} else {get(paste("mnt", wm, sep=""))})
          assign(paste("hot", j, sep=""), if (get(paste("t", j, sep="")) > get(paste("hot", wm, sep=""))) {get(paste("t", j, sep=""))} else {get(paste("hot", wm, sep=""))})
        }
        hotm <- mnt12
        
        for (wm in 1:12) {
          assign(paste("xx", wm, sep=""), if (hotm == wm) {get(paste("q", wm, sep=""))} else {-9999})
        }
        res <- round(max(xx1,xx2,xx3,xx4,xx5,xx6,xx7,xx8,xx9,xx10,xx11,xx12)/3)
        return(res)
      }
    }
    
    coordenadas <- rasterToPoints(PpTaStack[[1]])[, 1:2]
    
    value_point <- as.matrix(rbind.data.frame(coordenadas))
    colnames(value_point) <- c('x', 'y')
    
    rowVals <- raster::extract(PpTaStack, value_point)
    
    RasVals <- apply(rowVals, 1, p10fun) ## esto se puede paralelizar 
    
    p10 <- PpTaStack[[1]]
    pos_with_values <-  which(!is.na(p10[]))
    
    p10[pos_with_values] <- RasVals
    names(p10) <- 'ETP_10'
    
    p10 <- writeRaster(p10, outfile, format = format, overwrite = TRUE)
    
    
    rm(PpTaStack)
    #} else {
    #cat("", "\n", "File bio_18 already exists, skipping calculation, but loading", "\n")
    #p18 <- raster(outfile)
    return(p10)
  }
}

#outfile <- 'D:/CC/_bd/_colombia/_evapotranspiration/_etp_worldclim_v1/_asc/_bios_etp/ETP10.asc'

#ETP10Calc(tmean, etp_layers, outfile, format = 'ascii')

ETP10Calc(etp_layers, tmean, outfile, format = 'ascii')

#############################################
### Calculo ETP of coldest quarter ETP 11
#############################################

rlist <- etp_layers
rlist2 <- tmean
outfile <- 'D:/CC/_bd/_colombia/_model/_cormacarena/_input/_variables/_etp/_bios_etp/_rcp85/ETP11.asc'
format <- 'ascii'

ETP11Calc <- function(rlist, rlist2, outfile, format='') {
  
  if (!is.list(rlist)) {
    stop('First argument should be a list or rasters (ETP)')
  } else if (!is.list(rlist2)) {
    stop('Second argument should be a list or rasters (tmean)')
  }
  
  if (!file.exists(outfile)) {
    cat("", "\n", "ETP of Coldest Quarter (ETP)", "\n")
    
    PpTaStack <- stack(c(rlist, rlist2))
    
    p11fun <- function(DataPixel) {
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
    
    rowVals <- raster::extract(PpTaStack, value_point)
    
    RasVals <- apply(rowVals, 1, p11fun) ## esto se puede paralelizar 
    
    p11 <- PpTaStack[[1]]
    pos_with_values <-  which(!is.na(p11[]))
    
    p11[pos_with_values] <- RasVals
    names(p11) <- 'ETP_11'
    
    p11 <- writeRaster(p11, outfile, format = format, overwrite = TRUE)
    
  }
  
  #} else {
  #cat("", "\n", "File bio_19 already exists, skipping calculation, but loading", "\n")
  #p19 <- raster(outfile)
  return(p11)
  
}

#outfile <- 'D:/CC/_bd/_colombia/_evapotranspiration/_etp_worldclim_v1/_asc/_bios_etp/ETP11.asc'

ETP11Calc(etp_layers, tmean, outfile, format = 'ascii')

