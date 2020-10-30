library(tidyverse)
library(glue)
library(showtext)
library(ggtext)

# crazy gymnastics we have to do since JSON request url cannot exceed 8102 characters
# uncomment below and provide your own api key if you want to run
#
# library(googleway)
# 
# api_key <- "YOUR KEY HERE"
#
# df <- wind_turbine %>%
#   select(latitude, longitude) %>%
#   mutate(grp = row_number() %/% 100) %>%
#   nest_by(grp) %>%
#   transmute(elevation = list(
#     google_elevation(data[,c("longitude", "latitude")], key = api_key)$results
#   )) %>%
#   unnest(cols = elevation) %>%
#   ungroup() %>%
#   transmute(latitude = location$lat, longitude = location$lng, elevation)
#
# df %>% write_rds("elevation.rds")

wind_turbine <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-27/wind-turbine.csv') %>% 
  left_join(read_rds("wind-turbines/elevation.rds")) %>% 
  drop_na(elevation)

by_project <- wind_turbine %>% 
  group_by(province_territory, project_name) %>% 
  summarize(n_turbines = n(),
            elevation = median(elevation),
            capacity = first(total_project_capacity_mw),
            .groups = "drop") %>% 
  arrange(elevation) %>% 
  mutate(id = row_number(),
         province_territory = fct_lump_n(province_territory, 5)) %>% 
  filter(province_territory != "Other")

centroids <- by_project %>% 
  with_groups(province_territory,
              summarize,
              id = median(id),
              elevation = median(elevation),
              project_name = project_name[n_turbines == max(n_turbines)][[1]]) %>% 
  left_join(by_project %>% select(province_territory, project_name, capacity)) %>% 
  bind_rows(by_project %>% arrange(desc(elevation)) %>% slice(1) %>% mutate(id = id + 3)) %>% 
  select(-n_turbines) %>% 
  arrange(desc(elevation)) %>% 
  mutate(size = 30 * exp(capacity/max(capacity)),
         label = glue("<img src='wind-turbines/turbine-icon.png' width='{size}'/>"))
  

showtext_auto()

font_add_google("Quicksand")

x11()

by_project %>% 
  mutate(elevation = elevation + 50,
         ocean = FALSE) %>% 
  bind_rows(tibble(id = 1:-25, elevation = 40, ocean = TRUE)) %>% 
  bind_rows(tibble(id = 274:280, elevation = max(by_project$elevation) + 55, ocean = FALSE)) %>% 
  ggplot() +
  geom_area(aes(id, elevation, fill = ocean)) +
  geom_richtext(aes(id, elevation, label = label), fill = NA, label.color = NA, data = centroids, vjust = 0) +
  geom_text(label = "Canadian Wind Farm Elevations", x = -120, y = 1500, size = 55, color = "#056676", vjust = -0.2, family = "Quicksand") +
  geom_text(label = "Graphic: @sccmckenzie | Data: Government of Canada", x = -100, y = 1500, size = 20, color = "#056676", vjust = 1.2, family = "Quicksand") +
  # geom_text(aes(id, elevation, label = glue("{province_territory} - {project_name}")), hjust = 0, nudge_y = 200, nudge_x = 10, data = centroids, size = 20) +
  annotate("text", x = 265, y = 2000, label = "At 1911m, the Shinish project is the highest wind farm in Canada,\npowering 4,500 homes in British Columbia.", size = 15, lineheight = 0.35, hjust = 0, color = "#197163") +
  annotate("text", x = 200, y = 750, label = "With a total capacity of 350MW,\nthe Quebec Riviere-du-Moulin is the largest wind farm in Canada.", size = 15, lineheight = 0.35, hjust = 0, color = "#197163") +
  annotate("text", x = 5, y = 200, label = "West Cape Wind Farm\nPrince Edward Island", size = 10, lineheight = 0.35, hjust = 0, color = "#197163") +
  scale_fill_manual(values = c("#42B883", "#056676")) +
  scale_x_reverse(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 2200), position = "right", breaks = c(500, 1000, 1500), labels = c( "500m", "1000m", "1500m")) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#A3D2CA", color = "#A3D2CA"),
        legend.position = "none",
        plot.background = element_rect(fill = "#A3D2CA"),
        plot.margin = margin(0,0,0,0,"cm"),
        text = element_text(family = "Quicksand"),
        axis.text.y = element_text(size = 50)) +
  labs(x = NULL, y = NULL)

ggsave("wind-turbines/wind-turbines.png", type = "cairo")
