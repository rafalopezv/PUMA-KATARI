
library(lubridate)

a <- "2017-04-03 10:00:00" %>% as.POSIXct() # this is just to test the function
                                            # check that it only fulfills two conditions
                                            # therefore, the resut should be 0

# function
conflicto <- function(x, y) {
  marcha <- 0
  
  if((day(x) == 3 | day(x) == 4) &
     hour(x) >= 10 & hour(x) <= 11 & 
     y == 3) {
    marcha <- 80
  }
  
  marcha
}


# test
conflicto(a, 1) # should be 0 because it does not fulfill the hour condition.

