temp1 <- list()

variacion.tiempos <-  function(x, y) {
  if(hour(x) >= 23 & hour(x) < 0 & y == 2 | 
     y == 3 | y == 4 | y == 5 | y == 6 |
     y == 7 | y == 8 | y == 9 | y == 10 | 
     y == 11 | y == 12 | y == 13 | y == 14 |
     y == 15 | y == 16 | y == 17 | y == 18 |
     y == 19 | y == 20 | y == 21) {
    temp1[[1]] <- runif(1, 0,  10) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 0,  5) # tiempo en la parada
  }
  if(hour(x) >= 0 & hour(x) < 7 & y == 2 |
     y == 3 | y == 4 | y == 5 | y == 6 |
     y == 7 | y == 8 | y == 9 | y == 10 | 
     y == 11 | y == 12 | y == 13 | y == 14 |
     y == 15 | y == 16 | y == 17 | y == 18 |
     y == 19 | y == 20 | y == 21) {
    temp1[[1]] <- runif(1, -30,  5) # tiempo en el tramo 
    temp1[[2]] <- runif(1, -7,  2) # tiempo en la parada
  } 
  if(hour(x) >= 22 & hour(x) < 23 & y == 5 |
     y == 6 | y == 7 | y == 8 | y == 9 | y == 10 | 
     y == 11 | y == 12 | y == 13 | y == 14 |
     y == 15 | y == 16 | y == 17 | y == 18 |
     y == 19 | y == 20 | y == 21) {
    temp1[[1]] <- runif(1, 0,  5) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 0,  5) # tiempo en la parada
  }
  if(hour(x) >= 21 & hour(x) < 22 & y == 5 |
     y == 6 | y == 7 | y == 8 | y == 9 | y == 10 | 
     y == 11 | y == 12 | y == 13 | y == 14 |
     y == 15 | y == 16 | y == 17 | y == 18 |
     y == 19 | y == 20 | y == 21) {
    temp1[[1]] <- runif(1, 0,  30) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 0,  5) # tiempo en la parada
  }
  if(hour(x) >= 20 & hour(x) < 21 & y == 10 | 
     y == 11 | y == 12 | y == 13 | y == 14 |
     y == 15 | y == 16 | y == 17 | y == 18 |
     y == 19 | y == 20 | y == 21) {
    temp1[[1]] <- runif(1, 0,  30) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 0,  5) # tiempo en la parada
  }
  temp1
  if(hour(x) >= 19 & hour(x) < 20 & y == 5 |
     y == 6 | y == 7 | y == 8 | y == 9 | y == 10 | 
     y == 11 | y == 12 | y == 13 | y == 14 |
     y == 15 | y == 16 | y == 17 | y == 18 |
     y == 19 | y == 20 | y == 21) {
    temp1[[1]] <- runif(1, 30,  60) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 5,  10) # tiempo en la parada
  }
  if(hour(x) >= 18 & hour(x) < 19 & y == 5 |
     y == 6 | y == 7 | y == 8 | y == 9 | y == 10 | 
     y == 11 | y == 12 | y == 13 | y == 14 |
     y == 15 | y == 16 | y == 17 | y == 18 |
     y == 19 | y == 20 | y == 21) {
    temp1[[1]] <- runif(1, 0,  30) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 0,  5) # tiempo en la parada
  }
  if(hour(x) >= 17 & hour(x) < 18 & y == 2 |
     y == 3 | y == 4 | y == 5 |
     y == 6 | y == 7 | y == 8 | y == 9 | y == 10 | 
     y == 11 | y == 12 | y == 13 | y == 14 |
     y == 15 | y == 16 | y == 17 | y == 18 |
     y == 19 | y == 20 | y == 21) {
    temp1[[1]] <- runif(1, 0,  30) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 0,  5) # tiempo en la parada
  }
  if(hour(x) >= 16 & hour(x) < 17 & y == 5 |
     y == 6 | y == 7 | y == 8 | y == 9 | y == 10 | 
     y == 11 | y == 12 | y == 13 | y == 14 |
     y == 15 | y == 16 | y == 17 | y == 18 |
     y == 19 | y == 20 | y == 21) {
    temp1[[1]] <- runif(1, 0,  10) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 0,  5) # tiempo en la parada
  }
  if(hour(x) >= 15 & hour(x) < 16 & y == 5 |
     y == 6 | y == 7 | y == 8 | y == 9 | y == 10 | 
     y == 11 | y == 12 | y == 13 | y == 14 |
     y == 15 | y == 16 | y == 17 | y == 18 |
     y == 19 | y == 20 | y == 21) {
    temp1[[1]] <- runif(1, 0,  30) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 0,  5) # tiempo en la parada
  }
  if(hour(x) >= 14 & hour(x) < 15 & y == 5 |
     y == 6 | y == 7 | y == 8 | y == 9 | y == 10 | 
     y == 11 | y == 12 | y == 13 | y == 14 |
     y == 15 | y == 16 | y == 17 | y == 18 |
     y == 19 | y == 20 | y == 21) {
    temp1[[1]] <- runif(1, 0,  30) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 0,  5) # tiempo en la parada
  }
  if(hour(x) >= 13 & hour(x) < 14 & y == 12 | y == 13 | 
     y == 14 | y == 15 | y == 16 | y == 17 | y == 18 |
     y == 19 | y == 20 | y == 21) {
    temp1[[1]] <- runif(1, 30,  60) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 5,  10) # tiempo en la parada
  }
  if(hour(x) >= 12 & hour(x) < 13 & y == 5 |
     y == 6 | y == 7 | y == 8 | y == 9 | y == 10 | 
     y == 11 | y == 12 | y == 13 | y == 14 |
     y == 15 | y == 16 | y == 17 | y == 18 |
     y == 19 | y == 20 | y == 21) {
    temp1[[1]] <- runif(1, 0,  30) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 0,  5) # tiempo en la parada
  }
  if(hour(x) >= 10 & hour(x) < 12 & y == 5 |
     y == 6 | y == 7 | y == 8 | y == 9 | y == 10 | 
     y == 11 | y == 12 | y == 13 | y == 14 |
     y == 15 | y == 16 | y == 17 | y == 18 |
     y == 19 | y == 20 | y == 21) {
    temp1[[1]] <- runif(1, 0,  10) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 0,  5) # tiempo en la parada
  }
  if(hour(x) >= 7 & hour(x) < 10 & y == 5 |
     y == 6 | y == 7 | y == 8 | y == 9 | y == 10 | 
     y == 11 | y == 12 | y == 13 | y == 14 |
     y == 15 | y == 16 | y == 17 | y == 18 |
     y == 19 | y == 20 | y == 21) {
    temp1[[1]] <- runif(1, 0,  30) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 0,  5) # tiempo en la parada
  }
  if(hour(x) >= 22 & hour(x) < 23 & y == 2 |
     y == 3 | y == 4) {
    temp1[[1]] <- runif(1, 0,  30) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 0,  5) # tiempo en la parada
  }
  if(hour(x) >= 21 & hour(x) < 22 & y == 2) {
    temp1[[1]] <- runif(1, 0,  30) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 0,  5) # tiempo en la parada
  }
  if(hour(x) >= 21 & hour(x) < 22 & y == 3 | y == 4) {
    temp1[[1]] <- runif(1, 30,  60) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 5,  10) # tiempo en la parada
  }
  if(hour(x) >= 20 & hour(x) < 21 & y == 9) {
    temp1[[1]] <- runif(1, 30,  60) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 5,  10) # tiempo en la parada
  }
  if(hour(x) >= 20 & hour(x) < 21 & y == 2 |
     y == 5 | y == 6 | y == 7 | y == 8) {
    temp1[[1]] <- runif(1, 0,  30) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 0,  5) # tiempo en la parada
  }
  if(hour(x) >= 20 & hour(x) < 21 & y == 3 | y == 4) {
    temp1[[1]] <- runif(1, 60, 120) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 5, 10) # tiempo en la parada
  }
  if(hour(x) >= 19 & hour(x) < 20 & y == 4) {
    temp1[[1]] <- runif(1, 60,  600) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 0, 10) # tiempo en la parada
  }
  if(hour(x) >= 19 & hour(x) < 20 & y == 3) {
    temp1[[1]] <- runif(1, 60,  480) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 5, 20) # tiempo en la parada
  }
  if(hour(x) >= 19 & hour(x) < 20 & y == 2) {
    temp1[[1]] <- runif(1, 30,  60) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 5, 10) # tiempo en la parada
  }
  if(hour(x) >= 18 & hour(x) < 19 & y == 4) {
    temp1[[1]] <- runif(1, 60,  120) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 5, 10) # tiempo en la parada
  }
  if(hour(x) >= 18 & hour(x) < 19 & y == 2 | y == 3) {
    temp1[[1]] <- runif(1, 0,  30) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 0, 5) # tiempo en la parada
  }
  if(hour(x) >= 16 & hour(x) < 17 & y == 3 | y == 4) {
    temp1[[1]] <- runif(1, 0,  30) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 0, 5) # tiempo en la parada
  }
  if(hour(x) >= 16 & hour(x) < 17 & y == 2) {
    temp1[[1]] <- runif(1, 0,  10) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 0, 5) # tiempo en la parada
  }
  if(hour(x) >= 15 & hour(x) < 16 & y == 4) {
    temp1[[1]] <- runif(1, 60,  120) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 5, 10) # tiempo en la parada
  }
  if(hour(x) >= 15 & hour(x) < 16 & y == 2 | y == 3) {
    temp1[[1]] <- runif(1, 0, 30) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 0, 5) # tiempo en la parada
  }
  if(hour(x) >= 14 & hour(x) < 15 & y == 4) {
    temp1[[1]] <- runif(1, 60, 600) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 0, 10) # tiempo en la parada
  }
  if(hour(x) >= 14 & hour(x) < 15 & y == 2 | y == 3) {
    temp1[[1]] <- runif(1, 60, 120) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 5, 10) # tiempo en la parada
  }
  if(hour(x) >= 13 & hour(x) < 14 & y == 5 | y == 6 |
     y == 7 | y == 8 | y == 9 | y == 10 | y == 11) {
    temp1[[1]] <- runif(1, 0, 30) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 0, 5) # tiempo en la parada
  }
  if(hour(x) >= 13 & hour(x) < 14 & y == 2 | y == 3 |
     y == 4 ) {
    temp1[[1]] <- runif(1, 60, 120) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 5, 10) # tiempo en la parada
  }
  if(hour(x) >= 12 & hour(x) < 13 & y == 4) {
    temp1[[1]] <- runif(1, 60, 600) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 5, 10) # tiempo en la parada
  }
  if(hour(x) >= 12 & hour(x) < 13 & y == 2 | y == 3) {
    temp1[[1]] <- runif(1, 30, 60) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 5, 10) # tiempo en la parada
  }
  if(hour(x) >= 11 & hour(x) < 12 & y == 3 | y == 4) {
    temp1[[1]] <- runif(1, 30, 60) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 5, 10) # tiempo en la parada
  }
  if(hour(x) >= 11 & hour(x) < 12 & y == 3 | y == 4) {
    temp1[[1]] <- runif(1, 30, 60) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 5, 10) # tiempo en la parada
  }
  if(hour(x) >= 11 & hour(x) < 12 & y == 2) {
    temp1[[1]] <- runif(1, 0, 10) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 0, 5) # tiempo en la parada
  }
  if(hour(x) >= 10 & hour(x) < 11 & y == 3 | y == 4) {
    temp1[[1]] <- runif(1, 0, 30) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 0, 5) # tiempo en la parada
  }
  if(hour(x) >= 10 & hour(x) < 11 & y == 2) {
    temp1[[1]] <- runif(1, 0, 10) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 0, 5) # tiempo en la parada
  }
  if(hour(x) >= 9 & hour(x) < 10 & y == 2 | y == 3 | y == 4) {
    temp1[[1]] <- runif(1, 30, 60) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 5, 10) # tiempo en la parada
  }
  if(hour(x) >= 8 & hour(x) < 9 & y == 4) {
    temp1[[1]] <- runif(1, 60, 120) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 5, 10) # tiempo en la parada
  }
  if(hour(x) >= 8 & hour(x) < 9 & y == 2 | y == 3) {
    temp1[[1]] <- runif(1, 30, 60) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 5, 10) # tiempo en la parada
  }
  if(hour(x) >= 7 & hour(x) < 8 & y == 4) {
    temp1[[1]] <- runif(1, 60, 600) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 0, 10) # tiempo en la parada
  }
  if(hour(x) >= 7 & hour(x) < 8 & y == 2 | y == 3) {
    temp1[[1]] <- runif(1, 0, 30) # tiempo en el tramo 
    temp1[[2]] <- runif(1, 0, 5) # tiempo en la parada
  }
  
  temp1
} 



