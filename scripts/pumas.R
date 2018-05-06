#---------------------------------------------------
# Simulación ruta Chasquipampa Puma Katari
# Simulation of the Chasquipampa Puma Katari route 
#---------------------------------------------------

# Fijar locale/set locale
Sys.setlocale(locale = "es_ES.UTF-8")

# Fijar directorio de trabajo/set working directory
setwd("/Users/rafalopezv/Dropbox/PUMA KATARI/")

# Cargar librerías/load libraries
pkgs <- c("tidyverse", "lubridate")
lapply(pkgs, function(x) require(x, character.only = TRUE))
rm(pkgs)

# jalar funciones/source functions
source("scripts/functions.R")

# Crear vectores con los datos para el loop/preparing the loop
puma <- NULL
inicio <- "2017-04-03 07:00:00" %>% as.POSIXct()
parada <- 1:21 # paradas/stops
vuelta <- 1:4676 # vueltas por mes/monthly rounds

for(x in vuelta) {
  for(i in parada) {
    if (parada[i] == 1) {
      ita <- as.POSIXct(NA) # tiempo ideal de llegada/ideal time of arrival 
      rta <- as.POSIXct(NA) # tiemporeal de arrivo/real time of arrival 
      its <- inicio # tiempo ideal de salida/ideal time of departure 
      rts <- inicio + runif(1, 0, 120) # tiempo real de salida/real time of departure 
      dist <- 0 # distancia en metros/distance in meters
    } else {
      ita <- its + tiempos.paradas(i)[[1]]
      rta <- rts + tiempos.paradas(i)[[1]]  + variacion.tiempos(ita, i)[[1]] + conflicto(ita, i)
      its <- ita + tiempos.paradas(i)[[2]] 
      rts <- rta + tiempos.paradas(i)[[2]]  + variacion.tiempos(its, i)[[2]] 
      dist <- distancia(i)
    }
    temp <- data.frame(its, rts, ita, rta, parada[i], vuelta[x], distancia(i)) 
    puma <- rbind(puma, temp) %>% as.data.frame()
  }
  inicio <- inicio + intervalos.partida(inicio)
  cat("Vuelta no; ", x, "\n")
}

rio::export(puma, "data/puma.csv")

