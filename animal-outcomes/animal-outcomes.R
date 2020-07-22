library(tidyverse)
library(lubridate)
library(glue)
library(scales)
library(showtext)
library(patchwork)
library(grid)
library(ggtext)


# Get Data

animal_outcomes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_outcomes.csv')

url_population <- "https://www.qgso.qld.gov.au/issues/3261/components-population-change-persons-qld-198106-201912.csv"

population <- read_csv(url_population, skip = 2, na = c("", "n.a."), n_max = 157) %>% 
  janitor::clean_names() %>% 
  slice(-(1:2)) %>% 
  select(year, quarter, estimated_resident_population) %>%
  fill(year) %>% 
  transmute(date = mdy(glue("{quarter}/1/{year}")),
            population = estimated_resident_population)


# Generate Plots

font_add_google("Noto Sans TC")
showtext_auto()
x11()

p1 <- animal_outcomes %>% 
  filter(outcome == "Euthanized",
         animal_type %in% c("Dogs", "Cats", "Wildlife")) %>% 
  select(year, animal_type, value = QLD) %>% 
  ggplot() +
  geom_area(aes(year, value, group = animal_type, fill = animal_type)) +
  scale_fill_manual(values = c("Dogs" = "#60BEA0", "Cats" = "#C2E5D4", "Wildlife" = "#937E6A"), name = "") +
  theme_minimal() +
  scale_y_continuous(expand = c(0, 0), breaks = c(10000, 20000), labels = comma_format()) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1999, 2017, by = 6)) +
  labs(title = "", x = "", y = "") +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(hjust = 0.2),
        legend.spacing.x = unit(1, 'mm'))

p2 <- population %>% 
  filter(year(date) >= 1999,
         year(date) <= 2018) %>% 
  ggplot(aes(date, population)) +
  geom_line(color = "#787878", size = 3) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = c(3e6, 4e6, 5e6),
                     limits = c(2.75e6, 5.25e6),
                     labels = label_number(scale = 1/1e6, accuracy = 1, suffix = "M")) +
  scale_x_date(breaks = breaks_width(width = "6 year", offset = 400), labels = date_format(format = "%Y")) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#bdbdbd"),
        plot.title = element_text(size = 40)) +
  labs(title = "Human Population",
       x = "",
       y = "")


p1 +  p2 +
  plot_layout(ncol = 2, widths = c(3, 2)) +
  plot_annotation(title = "Animal Euthanizations in Queensland, Australia",
                  caption = "@sccmckenzie [Data: RSPCA | QGSO]") &
  theme(plot.background = element_rect(fill = "#FAF8F2", color = "#FAF8F2"),
        axis.ticks = element_line(color = "#bdbdbd"),
        text = element_text(family = "Noto Sans TC", size = 50))

ggsave(here::here("animal-outcomes/animal-outcomes.png"), type = "cairo")        


