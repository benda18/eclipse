library(png)

rm(list=ls());cat('\f');gc()

getwd()
tot_png <- png::readPNG("shiny_eclipse_timer/www/totality - Copy.png")


str(tot_png)

tot_png %>%
  matrix() %>% 
  str()
tot_png[[14550]]

length(tot_png)/3
