
library(renv)
#library(swephR)
#library(lubridate)
#library(dplyr)
#library(tigris)
#library(shiny)
#library(censusxy)
#library(scales)
#library(ggplot2)
#library(sf)
#library(glue)
#library(rsconnect)
#library(ggmap)


# https://www.r-bloggers.com/2022/06/creating-flowcharts-with-ggplot2/

library(graphics)
library(tidyr)
library(ggplot2)
library(igraph)
library(showtext)
library(rcartocolor)
rm(list=ls());cat('\f')

# renv::status(
#   
# )
# 
# renv::snapshot()

# goldilocks <- tibble(from = c("Goldilocks",
#                               "Porridge", "Porridge", "Porridge",
#                               "Just right",
#                               "Chairs", "Chairs", "Chairs",
#                               "Just right2",
#                               "Beds", "Beds", "Beds",
#                               "Just right3"),
#                      to   = c("Porridge",
#                               "Too cold", "Too hot", "Just right",
#                               "Chairs",
#                               "Still too big", "Too big", "Just right2",
#                               "Beds",
#                               "Too soft", "Too hard", "Just right3",
#                               "Bears!"))

goldilocks <- as_tibble(data.frame(from = c("Mailing Address", "Census Geocoder",
                                            "Lon-Lat", "Swiss Ephemeris", 
                                            "NASA JPL", 
                                            "Eclipse Calculations",
                                            "Eclipse Calculations", 
                                            "Timeline", "Obscuration"), 
                                   to   = c("Census Geocoder","Lon-Lat", 
                                            "Swiss Ephemeris", 
                                            "Eclipse Calculations",
                                            "Swiss Ephemeris", 
                                            "Obscuration", 
                                            "Timeline", 
                                            "Chart", "Plot")))



g = graph_from_data_frame(goldilocks, directed = TRUE)
coords = layout_as_tree(g)
colnames(coords) = c("x", "y")

output_df = as_tibble(coords) %>%
  mutate(step = vertex_attr(g, "name"),
         label = gsub("\\d+$", "", step),
         x = x*-1,
         type = factor(c("input", "agcy", "calc", "agcy", 
                         "agcy", "calc", "calc", 
                         "calc", rep("output",2)), 
                       levels = c("input", "agcy", "calc", "output")))

plot_nodes = output_df %>%
  mutate(xmin = x - 0.35,
         xmax = x + 0.35,
         ymin = y - 0.25,
         ymax = y + 0.25)

plot_edges = goldilocks %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c("from", "to"),
               names_to = "s_e",
               values_to = "step") %>%
  left_join(plot_nodes, by = "step") %>%
  select(-c(label, type, y, xmin, xmax)) %>%
  mutate(y = ifelse(s_e == "from", ymin, ymax)) %>%
  select(-c(ymin, ymax))

p = ggplot() +
  geom_rect(data = plot_nodes,
            mapping = aes(xmin = xmin, ymin = ymin, 
                          xmax = xmax, ymax = ymax, 
                          fill = type, colour = type),
            alpha = 0.5) 

p

p = p + 
  geom_text(data = plot_nodes,
            mapping = aes(x = x, y = y, label = label),
            family = "henny",
            color = "#585c45") 

p

p = p + 
  geom_path(data = plot_edges,
            mapping = aes(x = x, y = y, group = id),
            colour = "#585c45",
            arrow = arrow(length = unit(0.3, "cm"), type = "closed"))

p

p = p + 
  scale_fill_carto_d(palette = "Antique") +
  scale_colour_carto_d(palette = "Antique")

p

# p = p + 
#   labs(title = "The Goldilocks Decision Tree",
#        caption = "N. Rennie\n\nData: Robert Southey. Goldilocks and the Three Bears. 
#        1837.\n\nImage: New York Public Library\n\n#30DayChartChallenge") 
# p


