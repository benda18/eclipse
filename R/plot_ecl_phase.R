
library(renv)
library(ggplot2)

rm(list=ls());cat('\f');gc()

renv::status()
renv::snapshot()

# https://mathworld.wolfram.com/Circle-CircleIntersection.html
# https://www.youtube.com/watch?v=rm-JfTRUxzA
# https://stackoverflow.com/questions/32370485/convert-radians-to-degree-degree-to-radians


radius.moon <- 2
radius.sun  <- 2
distbw      <- 2

area.moon <- pi * radius.moon^2
area.sun  <- pi * radius.sun^2

# get area of segment
theta.moon <- (acos((0.5 * radius.moon)/ 
                     radius.moon)*180)/pi

twotheta.moon <- theta.moon * 2

area.sector <- (twotheta.moon / 360) *
  pi * 
  radius.moon^2

area.triangle <- 0.5 * 
  radius.moon^2 * 
  sin(twotheta.moon)

area.segment <- (area.sector - area.triangle) * 2

ggplot() + 
  geom_point(aes(x = 0, y = 0), 
             color = "yellow", 
             size = 100)+
  geom_point(aes(x = -4, y = 0), 
             color = "lightblue", 
             size = 100)+
  scale_size_ordinal()+
  scale_y_continuous(limits = c(-4,4))+
  scale_x_continuous(limits = c(-4,4))
