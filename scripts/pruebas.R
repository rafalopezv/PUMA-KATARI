data("globaltemp")
attach(globaltemp)



hchart(puma, type = "columnrange",
       hcaes(x = hora, puma$atraso.salidas.minutos))



x <- c("Min", "Median", "Max")
y <- sprintf("{point.%s}", c("lower", "median", "upper"))
tltip <- tooltip_table(x, y)

library(highcharter)
hchart(globaltemp, type = "columnrange",
       hcaes(x = date, low = lower, high = upper, color = median)) %>% 
  hc_yAxis(tickPositions = c(-2, 0, 1.5, 2),
           gridLineColor = "#B71C1C",
           labels = list(format = "{value} C", useHTML = TRUE)) %>% 
  hc_tooltip(
    useHTML = TRUE,
    headerFormat = as.character(tags$small("{point.x: %Y %b}")),
    pointFormat = tltip
  ) %>% 
  hc_add_theme(hc_theme_db())


hcboxplot(x = diamonds$x, var = diamonds$color, var2 = diamonds$cut,
          outliers = FALSE) %>% 
  hc_chart(type = "column") %>%
  hc_add_theme(hc_theme_google())

b <- puma[!is.na(puma$atraso.salidas.minutos),]
hcboxplot(x = b$tiempo.real.tramos.min, var = b$parada, 
          outliers = T) %>% 
  hc_add_theme(hc_theme_db())

c <- b %>% filter(parada != 2 & parada != 3 & parada != 4)

d <- b %>% filter(parada == 2)

hchart(d, "scatter", hcaes(x = hora , y = tiempo.real.tramos.min))

hcboxplot(x = c$tiempo.real.tramos.min, var = c$parada, 
          outliers = T) %>% 
  hc_add_theme(hc_theme_economist())


hcboxplot(x = d$tiempo.real.tramos.min, var = d$parada, 
          outliers = T) %>% 
  hc_add_theme(hc_theme_economist())


hcboxplot(x = d$tiempo.real.tramos.min, var = d$hora, 
          outliers = T) %>% 
  hc_add_theme(hc_theme_economist())


hcboxplot(x = a$tiempo,
          outliers = T) %>% 
  hc_add_theme(hc_theme_economist())



hchart(b, "scatter", hcaes(x = parada , y = tiempo.real.tramos.min))
hchart(b, "scatter", hcaes(x = tiempo.real.tramos.min  , y = parada))

runif(1, 1, 10)




