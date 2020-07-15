library(tidyverse)

astronauts <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

# How does astronaut age at first mission change with calendar year?

astronauts %>% 
  filter(nationality == "U.S.") %>% 
  with_groups(name, 
              summarise,
              year_first_mission = min(year_of_mission),
              age_first_mission = year_first_mission - first(year_of_birth),
              gender = first(sex)) %>% 
  ggplot(aes(year_first_mission, age_first_mission)) +
  geom_point() +
  geom_smooth(method = "lm")

data <- astronauts %>% 
  filter(str_detect(selection, "NASA")) %>% 
  with_groups(name, 
              summarise,
              year_first_mission = min(year_of_mission),
              age_first_mission = year_first_mission - first(year_of_birth),
              gender = first(sex))

broom::tidy(lm(age_first_mission ~ year_first_mission, data = data))

astronauts %>% 
  with_groups(name, 
              summarise,
              year_first_mission = min(year_of_mission),
              age_first_mission = year_first_mission - first(year_of_birth),
              gender = first(sex)) %>% 
  ggplot(aes(year_first_mission)) +
  geom_density()

astronauts %>% 
  with_groups(name, 
              summarise,
              year_first_mission = min(year_of_mission),
              age_first_mission = year_first_mission - first(year_of_birth),
              nationality = first(nationality))

astronauts %>% 
  filter(nationality == "U.S.") %>% 
  mutate(age = year_of_mission - year_of_birth) %>% 
  with_groups(c(mission_title, year_of_mission),
              summarise,
              age_range = max(age) - min(age)) %>% 
  ggplot(aes(year_of_mission, age_range)) +
  geom_point()

astronauts %>% 
  with_groups(nationality,
              summarise,
              year = min(year_of_mission)) %>% 
  rows_insert(tibble(nationality = .$nationality, year = 2020), by = c("nationality", "year")) %>% 
  ggplot(aes(year, nationality)) +
    geom_point() +
  geom_line(aes(group = nationality))

astronauts %>% 
  filter(str_detect(mission_title, "STS")) %>% 
  mutate(mission_title = str_remove_all(mission_title, "[^[:alnum:]]")) %>% 
  with_groups(c(mission_title, year_of_mission),
              summarise,
              name = list(name),
              n = n()) %>%
  with_groups(year_of_mission,
              filter,
              any(n == 1)) %>% 
  arrange(year_of_mission)
