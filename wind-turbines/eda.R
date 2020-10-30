library(tidyverse)
# library(googleway)

wind_turbine <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-27/wind-turbine.csv')

# crazy gymnastics we have to do since JSON request url cannot exceed 8102 characters
# uncomment below and provide your own api key if you want to run
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

wind_turbine_df <- wind_turbine %>% 
  left_join(read_rds("elevation.rds")) %>% 
  drop_na(elevation)

wind_turbine_df %>% 
  ggplot(aes(elevation)) +
  geom_density()

wind_turbine_df %>% 
  ggplot(aes(rotor_diameter_m, elevation)) +
  geom_point() # nothing interesting

wind_turbine_df %>% 
  ggplot(aes(latitude, elevation)) +
  geom_line() # a bit more interesting

wind_turbine_df %>% 
  arrange(elevation) %>% 
  transmute(id = row_number(), elevation, province_territory) %>% 
  ggplot(aes(id, elevation)) +
  geom_line()


# next: roll_most_freq (show if there is a common province for each elevation group)
# adding graphics

# it might be better to consolidate by project_name, what is variation between projects?

wind_turbine_df %>% 
  with_groups(project_name, summarize, elevation_sd = sd(elevation)) %>% 
  ggplot(aes(elevation_sd)) +
  geom_density()

# consolidating would be ok

by_project <- wind_turbine_df %>% 
  group_by(province_territory, project_name) %>% 
  summarize(n_turbines = n(),
            elevation = median(elevation),
            capacity = first(total_project_capacity_mw),
            .groups = "drop") %>% 
  arrange(elevation) %>% 
  mutate(id = row_number(),
         province_territory = fct_lump_n(province_territory, 5)) %>% 
  filter(province_territory != "Other")

by_project %>% 
  ggplot(aes(id, elevation)) +
  geom_line()

by_project %>% 
  ggplot(aes(id, elevation)) +
  geom_point(aes(color = province_territory), alpha = 0.7)

by_project %>% 
  ggplot(aes(id, elevation)) +
  geom_point(aes(color = province_territory), alpha = 0.7) +
  facet_wrap(~ province_territory, ncol = 1)

by_project %>% 
  ggplot(aes(elevation)) +
  geom_density(aes(fill = province_territory), alpha = 0.7) +
  facet_wrap(~ province_territory, ncol = 1, scales = "free_y")

centroids <- by_project %>% 
  with_groups(province_territory,
              summarize,
              id = median(id),
              elevation = median(elevation),
              project_name = project_name[n_turbines == max(n_turbines)][[1]]) %>% 
  left_join(by_project %>% select(province_territory, project_name, capacity)) %>% 
  bind_rows(by_project %>% arrange(desc(elevation)) %>% slice(1) %>% mutate(id = id + 3)) %>% 
  select(-n_turbines) %>% 
  mutate(elevation = elevation + 50)


by_project %>% 
  ggplot(aes(id, elevation)) +
  geom_point(aes(color = province_territory), alpha = 0.7) +
  geom_point(aes(id, elevation, color = province_territory), size = 4, data = centroids) +
  facet_wrap(~ province_territory, ncol = 1)


#### The real plot

# library(showtext)
library(glue)
library(ggtext)

# showtext_auto()
# 
# x11()

by_project %>% 
  mutate(elevation = elevation + 50,
         ocean = FALSE) %>% 
  bind_rows(tibble(id = 1:-25, elevation = 40, ocean = TRUE)) %>% 
  bind_rows(tibble(id = 274:280, elevation = max(by_project$elevation) + 55, ocean = FALSE)) %>% 
  ggplot(aes(id, elevation)) +
  geom_area(aes(fill = ocean)) +
  # geom_point(aes(id, elevation, size = capacity), data = centroids, color = "white") +
  geom_richtext(aes(id, elevation + 100), label = "<img src = 'wind_turbines/turbine-icon.png' width = '50'/>", fill = NA, label.color = NA, data = centroids) +
  scale_fill_manual(values = c("#42B883", "#056676")) +
  scale_x_reverse(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 50), position = "right", breaks = c(500, 1000, 1500), labels = c("500m", "1000m", "1500m")) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#A3D2CA", color = "#A3D2CA"),
        legend.position = "none",
        plot.background = element_rect(fill = "#A3D2CA")) +
  labs(x = NULL, y = NULL)
