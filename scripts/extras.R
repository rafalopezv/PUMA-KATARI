#---------------------
data(diamonds, economics_long, mpg, package = "ggplot2")
library(dplyr)
library(ggplot2)

hchart(mpg, "scatter", hcaes(x = displ, y = hwy, group = class))
hchart(b, "scatter", hcaes(x = a, y = atraso.salidas.minutos, group = parada))

b <- puma[1:10000, ]
b <- b[!is.na(b$dia.literal), ]
b %<>% filter(dia.literal == "lunes" | dia.literal == "martes")

b <- puma %>% select(a, atraso.salidas.minutos, parada)
b <- b[!is.na(b$a), ]


#---------------------
hc <- highchart() %>% 
  hc_chart(type = "area") %>% 
  hc_title(text = "Historic and Estimated Worldwide Population Distribution by Region") %>% 
  hc_subtitle(text = "Source: Wikipedia.org") %>% 
  hc_xAxis(categories = c("1750", "1800", "1850", "1900", "1950", "1999", "2050"),
           tickmarkPlacement = "on",
           title = list(enabled = FALSE)) %>% 
  hc_yAxis(title = list(text = "Percent")) %>% 
  hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
             <b>{point.percentage:.1f}%</b> ({point.y:,.0f} millions)<br/>",
             shared = TRUE) %>% 
  hc_plotOptions(area = list(
    stacking = "percent",
    lineColor = "blue",
    lineWidth = 1,
    marker = list(
      lineWidth = 1,
      lineColor = "red"
    ))
  ) %>% 
  hc_add_series(name = "Asia", data = c(502, 635, 809, 947, 1402, 3634, 5268)) %>% 
  hc_add_series(name = "Africa", data = c(106, 107, 111, 133, 221, 767, 1766)) %>%
  hc_add_series(name = "Europe", data = c(163, 203, 276, 408, 547, 729, 628)) %>% 
  hc_add_series(name = "America", data = c(18, 31, 54, 156, 339, 818, 1201)) %>% 
  hc_add_series(name = "Oceania", data = c(2, 2, 2, 6, 13, 30, 46)) 

hc


#-----------

data(citytemp)

data(citytemp)

hc <- highchart() %>% 
  hc_plotOptions(line = list(color = "red",
                             marker = list(
                               fillColor = "white",
                               lineWidth = 2,
                               lineColor = NULL
                             )
  )) %>%  
  hc_add_series(name = "Tokyo", data = citytemp$tokyo) %>% 
  hc_add_series(name = "London", data = citytemp$london,
                marker = list(fillColor = "black"))


hc

# Cálculo de dimensiones bases para 7 rutas y peso en (giga)megabytes
((dim(puma)[1] * dim(puma)[2])*2) * 7 # mensual casi 10 millones de datos
(((dim(puma)[1] * dim(puma)[2])*2) * 7) * 12 # anual 115 millones de datos
(((object.size(puma) * 2) * 7) * 12) / 1073741824 # 4.8 gigabytes

