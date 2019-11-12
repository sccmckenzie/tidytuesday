library(tidyverse)
library(lubridate)

horror_movies_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")

horror_movies <- horror_movies_raw %>% 
  mutate(title_length = str_length(title),
         release_date = dmy(release_date),
         movie_run_time = str_extract(movie_run_time, "\\d+") %>% as.integer())

horror_movies %>% 
  group_by(month(release_date)) %>% 
  summarise(n = n())

scale_fill_brewer("Letters", palette = "Spectral")

horror_movies %>% 
  drop_na(movie)
