#--------------------------
# Estadísticas Puma Katari/Stats 
#--------------------------
rm(list = ls())

# Fijar locale/set locale
Sys.setlocale(locale = "es_ES.UTF-8")

# Fijar directorio de trabajo/set working directory
setwd("/Users/rafalopezv/Dropbox/PUMA KATARI/")

# Cargar librerías/load libraries
pkgs <- c("magrittr", "tidyverse", "lubridate", "rio", "highcharter")
lapply(pkgs, function(x) require(x, character.only = TRUE))
rm(pkgs)

# Importar base generada/import data
puma <- import("data/puma.csv")
lapply(puma, class)
for(i in 1:4) {
  puma[, i] <- as.POSIXct(puma[, i])
}
rm(i)


# Renombrar variables y corregir rts y rta para parada 21
# Rename variables and fix rts and rta for stop 21
names(puma)
puma %<>% rename(parada = parada.i., vuelta = vuelta.x., distancia = distancia.i.)
temp <- puma$parada == 21
puma[temp, c("rts", "its")] <- NA
rm(temp)

# Crear nuevas variables/new variables
puma %<>% mutate(atraso.salidas.minutos = round((rts - its)/60, 2) %>% as.numeric(),
                 atraso.llegada.minutos = round((rta - ita)/60, 2) %>% as.numeric(),
                 lag.ideal.salida = lag(its, 1),
                 tiempo.ideal.tramos.min = round(ita - lag.ideal.salida, 2) %>% as.numeric(),
                 lag.real.salida = lag(rts, 1),
                 tiempo.real.tramos.min = round(rta - lag.real.salida, 2) %>% as.numeric(),
                 atraso.real.de.ideal.min = round(tiempo.real.tramos.min - tiempo.ideal.tramos.min, 2) %>% as.numeric(),
                 tiempo.espera.ideal.segundos = as.numeric(its - ita),
                 tiempo.espera.real.segundos = as.numeric(rts - rta),
                 atraso.esperas.segundos = tiempo.espera.real.segundos - tiempo.espera.ideal.segundos,
                 `KM/H.ideal` =  round((distancia/1000) / (as.numeric(tiempo.ideal.tramos.min/60))),
                 `KM/H.real` =  round((distancia/1000) / (as.numeric(tiempo.real.tramos.min/60))),
                 dia = wday(rts),
                 hora = hour(rts)) %>%
  select(-lag.ideal.salida, -lag.real.salida)

# rellenar NA en horas y dias/complete vector of hours and days
puma$dia[is.na(puma$dia)] <- wday(puma$rta[is.na(puma$dia)]) 
puma$dia[is.na(puma$hora)] <- hour(puma$rta[is.na(puma$hora)]) 

# cambiar días de la semana de forma literal/change weekdays as names
puma$dia.literal[puma$dia == 1] <- "domingo"
puma$dia.literal[puma$dia == 2] <- "lunes"
puma$dia.literal[puma$dia == 3] <- "martes"
puma$dia.literal[puma$dia == 4] <- "miercoles"
puma$dia.literal[puma$dia == 5] <- "jueves"
puma$dia.literal[puma$dia == 6] <- "viernes"
puma$dia.literal[puma$dia == 7] <- "sabado"

#-------------------
# graficos/graphs
summary(puma)

# atrasos en salidas
a <- hchist(puma$atraso.llegada.minutos, breaks = 200, showInLegend = FALSE) %>%
  hc_title(text = "Minutos de atraso en las salidas y llegadas a cada parada") %>%
  hc_subtitle(text = "93520 salidas y llegadas en un mes") %>%
  hc_add_theme(hc_theme_db()) %>% #hc_theme_db
  hc_xAxis(title = list(text = "Minutos"), max = 10) %>%
  hc_yAxis(title = list(text = "Salidas y llegadas"))


b <- hchist(puma$atraso.salidas.minutos, showInLegend = FALSE) %>%
  hc_title(text = "Minutos de atraso en las salidas de cada parada") %>%
  hc_subtitle(text = "93520 salidas en un mes") %>%
  hc_add_theme(hc_theme_db()) %>% #hc_theme_db
  hc_xAxis(title = list(text = "Minutos"), max = 130) %>%
  hc_yAxis(title = list(text = "Salidas"))

# tiempo para subir pasajeros
hchist(puma$tiempo.espera.real.segundos, showInLegend = FALSE, breaks = 20) %>%
  hc_subtitle(text = "98196 paradas en un mes") %>%
  hc_title(text = "Tiempo para subir pasajeros") %>%
  hc_add_theme(hc_theme_economist()) %>% #hc_theme_db
  hc_xAxis(title = list(text = "Segundos")) %>%
  hc_yAxis(title = list(text = "Paradas")) 

# tiempo de partida a llegada final
a <- puma %>% select(rts, rta, parada, vuelta) %>%
  filter(parada == 1 | parada == 21) %>%
  mutate(rts = lag(rts, 1), tiempo = as.numeric(rta - rts)) %>% 
  filter(parada == 21)  

hchist(a$tiempo, showInLegend = FALSE, breaks = 20) %>%
  hc_subtitle(text = "4676 viajes en un mes") %>%
  hc_title(text = "Tiempo de toda la ruta") %>%
  hc_add_theme(hc_theme_db()) %>% #hc_theme_db
  hc_xAxis(title = list(text = "Minutos")) %>%
  hc_yAxis(title = list(text = "Viajes")) 

hchist(a$tiempo, showInLegend = FALSE, breaks = 400) %>%
  hc_subtitle(text = "4676 viajes en un mes") %>%
  hc_title(text = "Tiempo de toda la ruta") %>%
  hc_add_theme(hc_theme_db()) %>% #hc_theme_d
  hc_xAxis(title = list(text = "Minutos"), max = 63) %>%
  hc_yAxis(title = list(text = "Viajes")) 

# kilometros por hora
hchist(puma$`KM/H.real`, showInLegend = FALSE, breaks = 40) %>%
  hc_subtitle(text = "98195 tramos en un mes") %>%
  hc_title(text = "Velocidad entre tramos") %>%
  hc_add_theme(hc_theme_db()) %>% #hc_theme_d
  hc_xAxis(title = list(text = "Kilómetros por hora")) %>%
  hc_yAxis(title = list(text = "Tramos")) 


