library(dplyr)
library(ggmap)
library(ggplot2)
library(ggtext)
library(viridis)

austin_measles <- readr::read_csv("https://raw.githubusercontent.com/WSJ/measles-data/master/individual-states/texas.csv") %>% 
  filter(county == "Travis")

get_stamenmap(bbox = c(-97.9, 30.16, -97.6, 30.42), zoom = 12, maptype = "toner-lines") %>% 
  ggmap() +
  geom_point(aes(lng, lat, color = mmr), data = austin_measles, size = 4) +
  scale_color_viridis_c(option = "plasma", direction = -1) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        text = element_text(family = 'Verdana'),
        legend.position = c(0.85, 0.15)) +
  labs(x = "", y = "", color = "Vaccination rate [%]")

ggsave("test.png", g, width = 1.75, height = 1.75, units = "in", scale = 1, dpi = "retina")
