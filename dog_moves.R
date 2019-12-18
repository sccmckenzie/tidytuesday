library(tidyverse)
library(sp)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)

dog_travel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_travel.csv')

usa <- ne_countries(scale = "medium", returnclass = 'sf', country = "United States of America")

cities <- us.cities %>% 
  as_tibble() %>% 
  select(city = name, state = country.etc, pop, lat, long) %>% 
  mutate(city = str_remove(city, " [:upper:]+$"))

dog_travel %>% 
  count(contact_city, contact_state) %>% 
  left_join(cities)

theme_set(theme_bw())

ggplot(usa) +
  geom_sf(fill = "cornsilk") +
  coord_sf(crs = st_crs(2163), xlim = c(-2000000, 2500000), ylim = c(-2300000, 730000))
