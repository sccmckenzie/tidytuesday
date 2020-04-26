library(dplyr)
library(forcats)
library(showtext)
library(ggmap)
library(glue)
library(purrr)
library(ggtext)
library(ggplot2)

texas_measles <- readr::read_csv("https://raw.githubusercontent.com/WSJ/measles-data/master/individual-states/texas.csv") %>% 
  mutate(mmr_cat = cut(mmr, breaks = c(0, 75, 85, 95, 100)) %>% fct_rev())

pal <- c("#b72a77", "#f1b6da", "#aed4ee", "#41a9b5")
names(pal) <- c("Less than 75%", "75%-85%", "85%-95%", "Greater than 95%")

font_add_google("Roboto")
showtext_auto()
x11()

get_stamenmap(bbox = c(-98, 30.16, -97.6, 30.42), zoom = 12, maptype = "toner-lines") %>% 
  ggmap() +
  geom_point(aes(lng, lat), data = filter(texas_measles, county == "Travis"), color = "black", size = 3.1) +
  geom_point(aes(lng, lat, color = mmr_cat), data = filter(texas_measles, county == "Travis"), size = 3) +
  scale_color_manual(values = rev(unname(pal))) +
  labs(x = "",
       y = "",
       title = "Austin, Tx - Private School Immunization Rates",
       subtitle = glue(imap(pal[c(1, length(pal))], ~ glue("<b style = 'color: {.x}'>{.y}</b>")) %>% reduce(paste)),
       caption = "@sccmckenzie (Data: WSJ, ggmap (stamen))") +
  theme_void() +
  theme(text = element_text(family = 'Roboto'),
        plot.title = element_markdown(size = 60),
        plot.subtitle = element_markdown(size = 40),
        plot.caption = element_text(size = 35),
        legend.position = "none")

ggsave(here::here("measles", "measles.png"), type = "cairo")
