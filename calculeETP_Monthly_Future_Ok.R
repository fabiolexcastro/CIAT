
# CALCULO DE RADIACIÓN PARA LOS GCM - CMIP5

# Librerias 

library(raster)
library(rgdal)
library(sp)
library(rgeos)
library(maptools)
library(maps)
library(dplyr)
library(gtools)


# Cargar funciones de ETP

source("//mnt/workspace_cluster_8/Coffee_Cocoa/CIAT2016/_bd/_malawi/_codes/_r/_etp/functionsETP.R")

# ------------------------------------------------------------------------------------------------------------------------------
# -------------- Paths Tmean - Tmax - Tmin -------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------

#rad_folder              <- 'D:/CC/_bd/_colombia/_evapotranspiration/_newBD/_rad_mmDia/_float/'
rad_folder              <- '//mnt/workspace_cluster_8/Coffee_Cocoa/CIAT2016/_bd/_malawi/_raster/_climate/_current/_rad/_mmDia/'
rad_list                <- list.files(rad_folder, full.names = T, pattern = ".asc$")
rad_list                <- mixedsort(rad_list)
rad_layers              <- lapply(rad_list, FUN = raster) #Units: kJ m-2 day-1 

min(rad_layers[[1]][], na.rm = T)# valor minimo del raster de Radiación para el primer mes del año

# ------------------------------------------------------------------------------------------------------------------------------
# -------------- Paths Tmean - Tmax - Tmin -------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------

#cmip5_folder            <- "Y:/_bd/_colombia/_raster/_cmip5/rcp60_extracts/Global_30s"
cmip5_folder            <- "//mnt/workspace_cluster_8/Coffee_Cocoa/CIAT2016/_bd/_malawi/_raster/_climate/_future/_rcp60"
models                  <- list.files(paste0(cmip5_folder, "/_2030"), full.names = F)
output_folder           <- "//mnt/workspace_cluster_8/Coffee_Cocoa/CIAT2016/_bd/_malawi/_raster/_climate/_future/_rcp60"


#years                   <- c("2020_2049", "2040_2069") 
years                    <- c("_2030", "_2050") 

#files                   <- list.files(paste0(cmip5_folder, "/", models[1], "/", years[1]), full.names = T, pattern = ".asc$")
files                    <- list.files(paste0(cmip5_folder, "/", years[1], "/", models[1]), full.names = T, pattern = ".asc$")

tmean_                  <- grep("tmean", files, value = T)
tmean_                  <- mixedsort(tmean_)
tmean_layers            <- lapply(tmean_, FUN = raster)

tmin_                   <- grep("tmin", files, value = T)
tmin_                   <- mixedsort(tmin_)
tmin_layers             <- lapply(tmin_, FUN = raster)

tmax_                   <- grep("tmax", files, value = T)
tmax_                   <- mixedsort(tmax_)
tmax_layers            <- lapply(tmax_, FUN = raster)

min(tmax_layers[[1]][], na.rm = T)

# -------------------------------------------------------------------------------------------------------------------------------------
# Los valores de temperatura estan multiplicados por 10, por tanto se deben dividir entre 10 para poder operar la formula
# Funcion para dividir por 10
# -------------------------------------------------------------------------------------------------------------------------------------

i = 1

for(i in 1:length(models)){

	print(models[i])

	files     <- list.files(paste0(cmip5_folder, "/", years[2], "/", models[i]), full.names = T, pattern = ".asc$") %>%
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

	etp_31days    <- out_etp[c(1, 3, 5, 7, 8, 10, 12)]
	etp_31days_ok <- list()

	for (m in 1:length(etp_31days)){
  		etp_31days_ok[[m]]   <- etp_31days[[m]] * 31 
  	}

	etp_30days    <- out_etp[c(4, 6, 9, 11)]
	etp_30days_ok <- list()

	for (n in 1:length(etp_30days)){
	  etp_30days_ok[[n]]   <- etp_30days[[n]] *30 
	}

	etp_29days    <- out_etp[2]
	etp_29days_ok <- etp_29days[[1]] * 29

	# Union de los layers
	etp_monthly <- c(etp_31days_ok[[1]], etp_29days_ok[[1]], etp_31days_ok[[2]], etp_30days_ok[[1]], etp_31days_ok[[3]], etp_30days_ok[[2]], 
	                 etp_31days_ok[[4]], etp_31days_ok[[5]], etp_30days_ok[[3]], etp_31days_ok[[6]], etp_30days_ok[[4]], etp_31days_ok[[7]])


	print("WriteRaster...")

	for (j in 1:length(etp_monthly)){ 
  
 		writeRaster(etp_monthly[[j]], paste0(output_folder, "/", years[2], "/", models[i], "/", names_etp[j]))
  	}
} 

}

# Meses con 30 días # Abril, junio, septiembre, noviembre [4, 6, 9, 11]

etp_30days    <- out_etp[c(4, 6, 9, 11)]
etp_30days_ok <- list()

for (i in 1:length(etp_30days)){
  etp_30days_ok[[i]]   <- etp_30days[[i]] *30 
}

# Mes con 29 días
etp_29days    <- out_etp[2]
etp_29days_ok <- etp_29days[[1]] * 29

# Union de los layers
etp_monthly <- c(etp_31days_ok[[1]], etp_29days_ok[[1]], etp_31days_ok[[2]], etp_30days_ok[[1]], etp_31days_ok[[3]], etp_30days_ok[[2]], 
                 etp_31days_ok[[4]], etp_31days_ok[[5]], etp_30days_ok[[3]], etp_31days_ok[[6]], etp_30days_ok[[4]], etp_31days_ok[[7]])



}


conv_temp         <- function(tmean, tmin, tmax, ...){
  
  tmean_layers_ok <- list()
  tmin_layers_ok  <- list()
  tmax_layers_ok  <- list()
  
  for(i in 1:12){
    
    tmean_layers_ok[[i]] <- tmean[[i]]/10
    tmax_layers_ok[[i]]  <- tmax[[i]]/10
    tmin_layers_ok[[i]]  <- tmin[[i]]/10
    
  }
  
  out <- list(tmean_layers_ok, tmin_layers_ok, tmax_layers_ok)
  return(out)
  #return(c(tmean_layers_ok, tmax_layers_ok, tmin_layers_ok))
}

out_temp <- conv_temp(tmean = tmean_layers, tmin = tmin_layers, tmax = tmax_layers)

tmean_ok <- out_temp[[1]]
tmin_ok  <- out_temp[[2]]
tmax_ok  <- out_temp[[3]]

# ----------------------------------------------------------------------------------------
# Calculo de la ETP - ET0 = 0.0023 (tmed + 17.78) R0 * (tmax - tmin) ^ 0.5
# ----------------------------------------------------------------------------------------

# Parte 1 = 0.0023(tmed + 17.78)
# Parte 2 = (tmax - tmin) ^ 0.5
# Parte 3 = Parte 1 * R0 * Parte 2

calc_ETP <- function(tmean_layers, tmax_layers, tmin_layers, rad_layers, ...){
  
  parte_1    <- list()
  parte_2    <- list()
  etp_layers <- list()
  
  for(i in 1:length(rad_layers)){
    
    parte_1[[i]]    <- 0.0023 * (tmean_layers[[i]] + 17.78)
    parte_2[[i]]    <- (tmax_layers[[i]] - tmin_layers[[i]]) ^ 0.5
    etp_layers[[i]] <- parte_1[[i]] * rad_layers[[i]] * parte_2[[i]]
    
  }
  
  # 31 dias
  rad_31days    <- rad_layers_mmdia[c(1, 3, 5, 7, 8, 10, 12)] 
  rad_31days_ok <- list() 
  
  for (i in 1:length(rad_31days)){
    rad_31days_ok[[i]]   <- rad_31days[[i]] * 31 
  }
  
  # Meses con 30 días # Abril, junio, septiembre, noviembre [4, 6, 9, 11]
  rad_30days    <- rad_layers_mmdia[c(4, 6, 9, 11)] 
  rad_30days_ok <- list() 
  
  for (i in 1:length(rad_30days)){
    rad_30days_ok[[i]]   <- rad_30days[[i]] *30 
  }
  
  # Mes con 29 días
  rad_29days    <- rad_layers_mmdia[2]
  rad_29days_ok <- rad_29days[[1]] * 29
  
  etp_monthly <- c(etp_31days_ok[[1]], etp_29days_ok[[1]], etp_31days_ok[[2]], etp_30days_ok[[1]], etp_31days_ok[[3]], etp_30days_ok[[2]], 
                   etp_31days_ok[[4]], etp_31days_ok[[5]], etp_30days_ok[[3]], etp_31days_ok[[6]], etp_30days_ok[[4]], etp_31days_ok[[7]])
  
  return(etp_layers)
  
}

# ETP Names

names_etp <- paste0("etp_", 1:12, ".asc")

out_etp <- calc_ETP(tmean_layers = tmean_ok, tmax_layers = tmax_ok, tmin_layers = tmin_ok, rad_layers = rad_layers)


# Guardado de archivos resultantes

# Meses con 31 días # Enero, marzo, mayo, julio, agosto, octubre, diciembre [1, 3, 5, 7, 8, 10, 12]

etp_31days    <- out_etp[c(1, 3, 5, 7, 8, 10, 12)]
etp_31days_ok <- list()

for (i in 1:length(etp_31days)){
  etp_31days_ok[[i]]   <- etp_31days[[i]] * 31 
}

# Meses con 30 días # Abril, junio, septiembre, noviembre [4, 6, 9, 11]

etp_30days    <- out_etp[c(4, 6, 9, 11)]
etp_30days_ok <- list()

for (i in 1:length(etp_30days)){
  etp_30days_ok[[i]]   <- etp_30days[[i]] *30 
}

# Mes con 29 días
etp_29days    <- out_etp[2]
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


etp_stack <- stack(out_etp)

annual_etp <- sum(etp_stack)


for (i in 1:length(etp_monthly)){ 
  
  writeRaster(etp_monthly[[i]], paste0(output_folder, "/", names_etp[i]))
  
} 


