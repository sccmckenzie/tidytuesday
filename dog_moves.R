library(tidyverse)
library(sp)
library(sf)
library(rnaturalearth)

dog_descriptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_descriptions.csv')

usa <- ne_countries(scale = "medium", returnclass = 'sf', country = "United States of America")

cities <- as_tibble(maps::us.cities) %>% 
  select(city = name, state = country.etc, pop, lat, long) %>% 
  mutate(city = str_remove(city, " [:upper:]+$"))

dog_pop <- dog_descriptions %>% 
  filter(str_detect(contact_state, "[:upper:]+")) %>% 
  transmute(city = str_to_title(contact_city),
            state = contact_state) %>% 
  count(city, state) %>% 
  left_join(cities) %>% 
  drop_na() %>% 
  st_as_sf(coords = c("long", "lat"), remove = FALSE, scrs = 4326, agr = "constant")

ggplot(usa) +
  geom_sf(fill = "azure") +
  geom_sf(aes(size = n), data = dog_pop, show.legend = 'point', color = "#1c69ab") +
  coord_sf(crs = st_crs(2163), xlim = c(-2000000, 2500000), ylim = c(-2000000, 730000)) +
  theme(panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom") +
  labs(title = "Adoptable Dog Listings",
       caption = "Source: The Pudding\nPetfinder.com",
       size = "# of listings")
