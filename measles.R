library(dplyr)
library(forcats)
library(ggmap)
library(ggplot2)
library(ggtext)
library(viridis)

texas_measles <- readr::read_csv("https://raw.githubusercontent.com/WSJ/measles-data/master/individual-states/texas.csv") %>% 
  mutate(mmr_cat = cut(mmr, breaks = c(0, 75, 85, 95, 100)) %>% fct_rev())

pal <- c("#b72a77", "#f1b6da", "#aed4ee", "#41a9b5")

get_stamenmap(bbox = c(-98.1, 30.16, -97.6, 30.42), zoom = 12, maptype = "toner-lines") %>% 
  ggmap() +
  geom_point(aes(lng, lat), data = filter(texas_measles, county == "Travis"), color = "black", size = 5) +
  geom_point(aes(lng, lat, color = mmr_cat), data = filter(texas_measles, county == "Travis"), size = 4) +
  # scale_color_viridis_c(option = "plasma", direction = -1) +
  scale_color_manual(values = rev(pal),
                     labels = c(
                       `(0,75]` = "Less than 75%",
                       `(75,85]` = "75%-85%",
                       `(85,95]` = "85%-95%",
                       `(95,100]` = "Greater than 95%"
                     )) +
  labs(x = "", y = "", title = "Private School Immunization Rates", subtitle = "Austin, Tx") +
  theme_void() +
  theme(text = element_text(family = 'Helvetica'),
        plot.title.position = element_text(vjust = 0),
        legend.position = c(-0.05, 1),
        legend.justification = c(0, 1),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white", linetype = 0))

ggsave("test.png", g, width = 1.75, height = 1.75, units = "in", scale = 1, dpi = "retina")
