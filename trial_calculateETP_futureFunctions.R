# CALCULO DE RADIACIÓN PARA LOS GCM - CMIP5

# Librerias 

library(raster); library(maptools)
library(rgdal); library(maps)
library(sp); library(dplyr)
library(rgeos); library(gtools)

# Cargar funciones de ETP

source("D:/CC/_bd/_colombia/_evapotranspiration/_codes/_r/functionsETP.R")

# -----------------------------------------------------------------------------------------------------------------
# -------------- Paths Radiación ----------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------

rad_folder              <- 'D:/CC/_bd/_colombia/_evapotranspiration/_newBD/_rad_mmDia/_float/'
rad_list                <- list.files(rad_folder, full.names = T, pattern = ".asc$")
rad_list                <- mixedsort(rad_list)
rad_layers              <- lapply(rad_list, FUN = raster) #Units: kJ m-2 day-1 

min(rad_layers[[1]][], na.rm = T)# valor minimo del raster de Radiación para el primer mes del año

cmip5_folder            <- "D:/CC/_bd/_colombia/_raster/_cmip5_ok"
models                  <- list.files(paste0(cmip5_folder, "/2020_2049"), full.names = F)

years                   <- c("2020_2049", "2040_2069") 

# -----------------------------------------------------------------------------------------------------------------
# -------------- Paths Tmean - Tmax - Tmin ------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------------

files_bcc_csm1_1        <- list.files(paste0(cmip5_folder, "/", years[1], "/", models[1]),  full.names = T, pattern = ".asc$")

tmean_bcc_csm1_1        <- grep("tmean", files_bcc_csm1_1, value = T)
tmean_bcc_csm1_1        <- mixedsort(tmean_bcc_csm1_1)
tmean_bcc_csm1_1_layers <- lapply(tmean_bcc_csm1_1, FUN = raster)

plot(tmean_bcc_csm1_1_layers[[1]])

tmin_bcc_csm1_1         <- grep("tmin", files_bcc_csm1_1, value = T)
tmin_bcc_csm1_1         <- mixedsort(tmin_bcc_csm1_1)
tmin_bcc_csm1_1_layers  <- lapply(tmin_bcc_csm1_1, FUN = raster)

tmax_bcc_csm1_1         <- grep("tmax", files_bcc_csm1_1, value = T)
tmax_bcc_csm1_1         <- mixedsort(tmax_bcc_csm1_1)
tmax_bcc_csm1_1_layers  <- lapply(tmax_bcc_csm1_1, FUN = raster)

min(tmax_bcc_csm1_1_layers[[1]], na.rm = T)

# -------------------------------------------------------------------------------------------------------------------------------------
# Los valores de temperatura estan multiplicados por 10, por tanto se deben dividir entre 10 para poder operar la formula
# Funcion para dividir por 10
# -------------------------------------------------------------------------------------------------------------------------------------

out_temp <- conv_temp(tmean = tmean_bcc_csm1_1_layers, tmin = tmin_bcc_csm1_1_layers, tmax = tmax_bcc_csm1_1_layers)

tmean_ok <- out_temp[[1]]
tmin_ok  <- out_temp[[2]]
tmax_ok  <- out_temp[[3]]

summary(tmean_ok[[1]], na.rm = T); summary(tmin_ok[[1]], na.rm = T); summary(tmax_ok[[1]], na.rm = T)

# ----------------------------------------------------------------------------------------
# Calculo de la ETP - ET0 = 0.0023 (tmed + 17.78) R0 * (tmax - tmin) ^ 0.5
# ----------------------------------------------------------------------------------------

# ETP Names

names_etp <- paste0("etp_", 1:12, ".asc")

out_etp <- calc_ETP(tmean_layers = tmean_ok, tmax_layers = tmax_ok, tmin_layers = tmin_ok, rad_layers = rad_layers)

etp_monthly <- out_etp

summary(etp_monthly[[1]][])
min(etp_monthly[[1]][], na.rm = T)

names_etp <- paste0("etp_", 1:12, ".asc")

# Guardado de archivos resultantes

for (i in 1:length(etp_monthly)){ 
  
  writeRaster(etp_monthly[[i]], paste0(output_folder, "/", names_etp[i]))
  
} 

etp_stack <- stack(out_etp)
annual_etp <- sum(etp_stack)



