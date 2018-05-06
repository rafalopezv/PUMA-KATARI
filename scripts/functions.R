
# ------------------
# Funciones: tiempos, bloqueos, velocidad, intervalos de partida

# Funcion para definir intervalos de partida
# Function to define departure intervals
intervalos.partida <- function(x) {
  if (hour(x) >= 7 & hour(x) < 11) incremento <- 10 * 60
  if (hour(x) >= 11 & hour(x) <= 13) incremento <- 5 * 60
  if (hour(x) >= 13 & hour(x) <= 17) incremento <- 10 * 60
  if (hour(x) >= 17 & hour(x) <= 23) incremento <- 5 * 60
  if (hour(x) >= 23) incremento <- 30 * 60
  if (hour(x) >= 0 & hour(x) <= 4) incremento <- 30 * 60
  if (hour(x) >= 4 & hour(x) < 6) incremento <- 10 * 60
  if (hour(x) ==  6) incremento <- 60 * 60
  
  incremento
}

# tiempos entre parada y parada y tiempos de espera 
#  stop to stop times and waitinig time at stop

temp <- list()
tiempos.paradas <- function(x) {
  if (x == 1) {
    temp[[1]] <- 0
    temp[[2]] <- 0
  }  
  if (x == 2) {
    temp[[1]] <- hms("00:02:56") # from stop 1 to 2
    temp[[2]] <- hms("00:00:24") # time in stop 2, getting commuters
  }
  if (x == 3) {
    temp[[1]] <- hms("00:02:29")
    temp[[2]] <- hms("00:00:19")
  }
  if (x == 4) {
    temp[[1]] <- hms("00:04:02")
    temp[[2]] <- hms("00:00:11")
  }
  if (x == 5) {
    temp[[1]] <- hms("00:02:48")
    temp[[2]] <- hms("00:00:21")
  }
  if (x == 6) {
    temp[[1]] <- hms("00:01:30")
    temp[[2]] <- hms("00:00:10")
  }
  if (x == 7) {
    temp[[1]] <- hms("00:01:11")
    temp[[2]] <- hms("00:00:10")
  }
  if (x == 8) {
    temp[[1]] <- hms("00:02:53")
    temp[[2]] <- hms("00:00:12")
  }
  if (x == 9) {
    temp[[1]] <- hms("00:03:08")
    temp[[2]] <- hms("00:00:12")
  }
  if (x == 10) {
    temp[[1]] <- hms("00:02:38")
    temp[[2]] <- hms("00:00:23")
  }
  if (x == 11) {
    temp[[1]] <- hms("00:02:49")
    temp[[2]] <- hms("00:00:32")
  }
  if (x == 12) {
    temp[[1]] <- hms("00:02:14")
    temp[[2]] <- hms("00:00:13")
  }
  if (x == 13) {
    temp[[1]] <- hms("00:02:16")
    temp[[2]] <- hms("00:00:14")
  }
  if (x == 14) {
    temp[[1]] <- hms("00:01:16")
    temp[[2]] <- hms("00:00:11")
  }
  if (x == 15) {
    temp[[1]] <- hms("00:02:15")
    temp[[2]] <- hms("00:00:12")
  }
  if (x == 16) {
    temp[[1]] <- hms("00:01:52")
    temp[[2]] <- hms("00:00:15")
  }
  if (x == 17) {
    temp[[1]] <- hms("00:02:10")
    temp[[2]] <- hms("00:00:17")
  }
  if (x == 18) {
    temp[[1]] <- hms("00:01:18")
    temp[[2]] <- hms("00:00:11")
  }
  if (x == 19) {
    temp[[1]] <- hms("00:01:19")
    temp[[2]] <- hms("00:00:15")
  }
  if (x == 20) {
    temp[[1]] <- hms("00:01:12")
    temp[[2]] <- hms("00:00:07")
  }
  if (x == 21) {
    temp[[1]] <- hms("00:00:88")
    temp[[2]] <- 0
  }
  
  temp
}

# distancia entre paradas en metros
# distance between stops in meters
distancia <- function(x) {
  if(x == 1) distancia <- 0
  if(x == 2) distancia <- 500
  if(x == 3) distancia <- 600
  if(x == 4) distancia <- 2200
  if(x == 5) distancia <- 1400
  if(x == 6) distancia <- 450
  if(x == 7) distancia <- 350
  if(x == 8) distancia <- 1400
  if(x == 9) distancia <- 1100
  if(x == 10) distancia <- 1100
  if(x == 11) distancia <- 1000
  if(x == 12) distancia <- 650
  if(x == 13) distancia <- 800
  if(x == 14) distancia <- 500
  if(x == 15) distancia <- 550
  if(x == 16) distancia <- 850
  if(x == 17) distancia <- 900
  if(x == 18) distancia <- 550
  if(x == 19) distancia <- 500
  if(x == 20) distancia <- 700
  if(x == 21) distancia <- 450
  
  distancia
}

# modificando tiempos de acuerdo a tramos y horas
# modifying time according to stops and time
temp1 <- list()

variacion.tiempos <-  function(x, y) {
  if(hour(x) >= 23 & hour(x) < 24 & y == 2 | 
     y == 3 | y == 4 | y == 5 | y == 6 |
     y == 7 | y == 8 | y == 9 | y == 10 | 
     y == 11 | y == 12 | y == 13 | y == 14 |
     y == 15 | y == 16 | y == 17 | y == 18 |
     y == 19 | y == 20 | y == 21) {
    temp1[[1]] <- runif(1, 0,  10) # tiempo entre paradas/time between stops
    temp1[[2]] <- runif(1, 0,  5) # tiempo en parada/time at stop
  }
  if(hour(x) >= 0 & hour(x) < 7 & y == 2 |
     y == 3 | y == 4 | y == 5 | y == 6 |
     y == 7 | y == 8 | y == 9 | y == 10 | 
     y == 11 | y == 12 | y == 13 | y == 14 |
     y == 15 | y == 16 | y == 17 | y == 18 |
     y == 19 | y == 20 | y == 21) {
    temp1[[1]] <- runif(1, -30,  5) 
    temp1[[2]] <- runif(1, -7,  2) 
  } 
  if(hour(x) >= 22 & hour(x) < 23 & y == 5 |
     y == 6 | y == 7 | y == 8 | y == 9 | y == 10 | 
     y == 11 | y == 12 | y == 13 | y == 14 |
     y == 15 | y == 16 | y == 17 | y == 18 |
     y == 19 | y == 20 | y == 21) {
    temp1[[1]] <- runif(1, 0,  5) 
    temp1[[2]] <- runif(1, 0,  5) 
  }
  if(hour(x) >= 21 & hour(x) < 22 & y == 5 |
     y == 6 | y == 7 | y == 8 | y == 9 | y == 10 | 
     y == 11 | y == 12 | y == 13 | y == 14 |
     y == 15 | y == 16 | y == 17 | y == 18 |
     y == 19 | y == 20 | y == 21) {
    temp1[[1]] <- runif(1, 0,  30)  
    temp1[[2]] <- runif(1, 0,  5) 
  }
  if(hour(x) >= 20 & hour(x) < 21 & y == 10 | 
     y == 11 | y == 12 | y == 13 | y == 14 |
     y == 15 | y == 16 | y == 17 | y == 18 |
     y == 19 | y == 20 | y == 21) {
    temp1[[1]] <- runif(1, 0,  30)  
    temp1[[2]] <- runif(1, 0,  5) 
  }
  temp1
  if(hour(x) >= 19 & hour(x) < 20 & y == 5 |
     y == 6 | y == 7 | y == 8 | y == 9 | y == 10 | 
     y == 11 | y == 12 | y == 13 | y == 14 |
     y == 15 | y == 16 | y == 17 | y == 18 |
     y == 19 | y == 20 | y == 21) {
    temp1[[1]] <- runif(1, 30,  60) 
    temp1[[2]] <- runif(1, 5,  10) 
  }
  if(hour(x) >= 18 & hour(x) < 19 & y == 5 |
     y == 6 | y == 7 | y == 8 | y == 9 | y == 10 | 
     y == 11 | y == 12 | y == 13 | y == 14 |
     y == 15 | y == 16 | y == 17 | y == 18 |
     y == 19 | y == 20 | y == 21) {
    temp1[[1]] <- runif(1, 0,  30)  
    temp1[[2]] <- runif(1, 0,  5) 
  }
  if(hour(x) >= 17 & hour(x) < 18 & y == 2 |
     y == 3 | y == 4 | y == 5 |
     y == 6 | y == 7 | y == 8 | y == 9 | y == 10 | 
     y == 11 | y == 12 | y == 13 | y == 14 |
     y == 15 | y == 16 | y == 17 | y == 18 |
     y == 19 | y == 20 | y == 21) {
    temp1[[1]] <- runif(1, 0,  30) 
    temp1[[2]] <- runif(1, 0,  5) 
  }
  if(hour(x) >= 16 & hour(x) < 17 & y == 5 |
     y == 6 | y == 7 | y == 8 | y == 9 | y == 10 | 
     y == 11 | y == 12 | y == 13 | y == 14 |
     y == 15 | y == 16 | y == 17 | y == 18 |
     y == 19 | y == 20 | y == 21) {
    temp1[[1]] <- runif(1, 0,  10)  
    temp1[[2]] <- runif(1, 0,  5) 
  }
  if(hour(x) >= 15 & hour(x) < 16 & y == 5 |
     y == 6 | y == 7 | y == 8 | y == 9 | y == 10 | 
     y == 11 | y == 12 | y == 13 | y == 14 |
     y == 15 | y == 16 | y == 17 | y == 18 |
     y == 19 | y == 20 | y == 21) {
    temp1[[1]] <- runif(1, 0,  30)  
    temp1[[2]] <- runif(1, 0,  5) 
  }
  if(hour(x) >= 14 & hour(x) < 15 & y == 5 |
     y == 6 | y == 7 | y == 8 | y == 9 | y == 10 | 
     y == 11 | y == 12 | y == 13 | y == 14 |
     y == 15 | y == 16 | y == 17 | y == 18 |
     y == 19 | y == 20 | y == 21) {
    temp1[[1]] <- runif(1, 0,  30)  
    temp1[[2]] <- runif(1, 0,  5) 
  }
  if(hour(x) >= 13 & hour(x) < 14 & y == 12 | y == 13 | 
     y == 14 | y == 15 | y == 16 | y == 17 | y == 18 |
     y == 19 | y == 20 | y == 21) {
    temp1[[1]] <- runif(1, 30,  60)  
    temp1[[2]] <- runif(1, 5,  10) 
  }
  if(hour(x) >= 12 & hour(x) < 13 & y == 5 |
     y == 6 | y == 7 | y == 8 | y == 9 | y == 10 | 
     y == 11 | y == 12 | y == 13 | y == 14 |
     y == 15 | y == 16 | y == 17 | y == 18 |
     y == 19 | y == 20 | y == 21) {
    temp1[[1]] <- runif(1, 0,  30)  
    temp1[[2]] <- runif(1, 0,  5) 
  }
  if(hour(x) >= 10 & hour(x) < 12 & y == 5 |
     y == 6 | y == 7 | y == 8 | y == 9 | y == 10 | 
     y == 11 | y == 12 | y == 13 | y == 14 |
     y == 15 | y == 16 | y == 17 | y == 18 |
     y == 19 | y == 20 | y == 21) {
    temp1[[1]] <- runif(1, 0,  10)  
    temp1[[2]] <- runif(1, 0,  5) 
  }
  if(hour(x) >= 7 & hour(x) < 10 & y == 5 |
     y == 6 | y == 7 | y == 8 | y == 9 | y == 10 | 
     y == 11 | y == 12 | y == 13 | y == 14 |
     y == 15 | y == 16 | y == 17 | y == 18 |
     y == 19 | y == 20 | y == 21) {
    temp1[[1]] <- runif(1, 0,  30)  
    temp1[[2]] <- runif(1, 0,  5) 
  }
  if(hour(x) >= 22 & hour(x) < 23 & y == 2 |
     y == 3 | y == 4) {
    temp1[[1]] <- runif(1, 0,  30)  
    temp1[[2]] <- runif(1, 0,  5) 
  }
  if(hour(x) >= 21 & hour(x) < 22 & y == 2) {
    temp1[[1]] <- runif(1, 0,  30)  
    temp1[[2]] <- runif(1, 0,  5) 
  }
  if(hour(x) >= 21 & hour(x) < 22 & y == 3 | y == 4) {
    temp1[[1]] <- runif(1, 30,  60)  
    temp1[[2]] <- runif(1, 5,  10) 
  }
  if(hour(x) >= 20 & hour(x) < 21 & y == 9) {
    temp1[[1]] <- runif(1, 30,  60) 
    temp1[[2]] <- runif(1, 5,  10) 
  }
  if(hour(x) >= 20 & hour(x) < 21 & y == 2 |
     y == 5 | y == 6 | y == 7 | y == 8) {
    temp1[[1]] <- runif(1, 0,  30)  
    temp1[[2]] <- runif(1, 0,  5) 
  }
  if(hour(x) >= 20 & hour(x) < 21 & y == 3 | y == 4) {
    temp1[[1]] <- runif(1, 60, 120)  
    temp1[[2]] <- runif(1, 5, 10) 
  }
  if(hour(x) >= 19 & hour(x) < 20 & y == 4) {
    temp1[[1]] <- runif(1, 60,  600) 
    temp1[[2]] <- runif(1, 0, 10) 
  }
  if(hour(x) >= 19 & hour(x) < 20 & y == 3) {
    temp1[[1]] <- runif(1, 60,  480) 
    temp1[[2]] <- runif(1, 5, 20) 
  }
  if(hour(x) >= 19 & hour(x) < 20 & y == 2) {
    temp1[[1]] <- runif(1, 30,  60)  
    temp1[[2]] <- runif(1, 5, 10) 
  }
  if(hour(x) >= 18 & hour(x) < 19 & y == 4) {
    temp1[[1]] <- runif(1, 60,  120) 
    temp1[[2]] <- runif(1, 5, 10) 
  }
  if(hour(x) >= 18 & hour(x) < 19 & y == 2 | y == 3) {
    temp1[[1]] <- runif(1, 0,  30) 
    temp1[[2]] <- runif(1, 0, 5) 
  }
  if(hour(x) >= 16 & hour(x) < 17 & y == 3 | y == 4) {
    temp1[[1]] <- runif(1, 0,  30) 
    temp1[[2]] <- runif(1, 0, 5) 
  }
  if(hour(x) >= 16 & hour(x) < 17 & y == 2) {
    temp1[[1]] <- runif(1, 0,  10) 
    temp1[[2]] <- runif(1, 0, 5) 
  }
  if(hour(x) >= 15 & hour(x) < 16 & y == 4) {
    temp1[[1]] <- runif(1, 60,  120) 
    temp1[[2]] <- runif(1, 5, 10) 
  }
  if(hour(x) >= 15 & hour(x) < 16 & y == 2 | y == 3) {
    temp1[[1]] <- runif(1, 0, 30) 
    temp1[[2]] <- runif(1, 0, 5) 
  }
  if(hour(x) >= 14 & hour(x) < 15 & y == 4) {
    temp1[[1]] <- runif(1, 60, 600) 
    temp1[[2]] <- runif(1, 0, 10) 
  }
  if(hour(x) >= 14 & hour(x) < 15 & y == 2 | y == 3) {
    temp1[[1]] <- runif(1, 60, 120)
    temp1[[2]] <- runif(1, 5, 10) 
  }
  if(hour(x) >= 13 & hour(x) < 14 & y == 5 | y == 6 |
     y == 7 | y == 8 | y == 9 | y == 10 | y == 11) {
    temp1[[1]] <- runif(1, 0, 30)  
    temp1[[2]] <- runif(1, 0, 5) 
  }
  if(hour(x) >= 13 & hour(x) < 14 & y == 2 | y == 3 |
     y == 4 ) {
    temp1[[1]] <- runif(1, 60, 120)  
    temp1[[2]] <- runif(1, 5, 10) 
  }
  if(hour(x) >= 12 & hour(x) < 13 & y == 4) {
    temp1[[1]] <- runif(1, 60, 600)  
    temp1[[2]] <- runif(1, 5, 10) 
  }
  if(hour(x) >= 12 & hour(x) < 13 & y == 2 | y == 3) {
    temp1[[1]] <- runif(1, 30, 60)  
    temp1[[2]] <- runif(1, 5, 10) 
  }
  if(hour(x) >= 11 & hour(x) < 12 & y == 3 | y == 4) {
    temp1[[1]] <- runif(1, 30, 60)  
    temp1[[2]] <- runif(1, 5, 10) 
  }
  if(hour(x) >= 11 & hour(x) < 12 & y == 3 | y == 4) {
    temp1[[1]] <- runif(1, 30, 60)  
    temp1[[2]] <- runif(1, 5, 10) 
  }
  if(hour(x) >= 11 & hour(x) < 12 & y == 2) {
    temp1[[1]] <- runif(1, 0, 10)  
    temp1[[2]] <- runif(1, 0, 5) 
  }
  if(hour(x) >= 10 & hour(x) < 11 & y == 3 | y == 4) {
    temp1[[1]] <- runif(1, 0, 30)  
    temp1[[2]] <- runif(1, 0, 5) 
  }
  if(hour(x) >= 10 & hour(x) < 11 & y == 2) {
    temp1[[1]] <- runif(1, 0, 10)  
    temp1[[2]] <- runif(1, 0, 5) 
  }
  if(hour(x) >= 9 & hour(x) < 10 & y == 2 | y == 3 | y == 4) {
    temp1[[1]] <- runif(1, 30, 60)  
    temp1[[2]] <- runif(1, 5, 10) 
  }
  if(hour(x) >= 8 & hour(x) < 9 & y == 4) {
    temp1[[1]] <- runif(1, 60, 120)  
    temp1[[2]] <- runif(1, 5, 10) 
  }
  if(hour(x) >= 8 & hour(x) < 9 & y == 2 | y == 3) {
    temp1[[1]] <- runif(1, 30, 60)  
    temp1[[2]] <- runif(1, 5, 10) 
  }
  if(hour(x) >= 7 & hour(x) < 8 & y == 4) {
    temp1[[1]] <- runif(1, 60, 600)  
    temp1[[2]] <- runif(1, 0, 10) 
  }
  if(hour(x) >= 7 & hour(x) < 8 & y == 2 | y == 3) {
    temp1[[1]] <- runif(1, 0, 30)  
    temp1[[2]] <- runif(1, 0, 5) 
  }
  
  temp1
}

# variacion de tiempo por causas extraordinarias (marchas, bloqueos, etc)
# variation of time for extraordinary events (demostrations, etc)
conflicto <- function(x, y) {
  marcha <- 0
  
  if((day(x) == 3|day(x) == 4|day(x) == 13|day(x) == 14) &
     hour(x) >= 11 & hour(x) <= 12 & 
     y == 3) {
    marcha <- runif(1, 45*60, 120*60) # atraso de 45 minutos a 2 horas/delay:  45 minutes to 2 hours
  }
  
  marcha
}






