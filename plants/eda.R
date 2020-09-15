library(tidyverse)

plants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/plants.csv')
actions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/actions.csv')
threats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/threats.csv')

View(plants)
View(threats)

# library(tidymodels)
# 
# kclust <- tibble(k = 1:9)
# 
# kclust

threats %>% 
  distinct(threat_type)

points <- plants %>% 
  select(starts_with("threat_")) %>% 
  select(-last_col()) 

kclusts <- tibble(k = 1:25) %>% 
  mutate(kclust = map(k, ~ kmeans(points, centers = ..1)),
         glanced = map(kclust, glance),
         augmented = map(kclust, augment, points))

clusterings <- 
  kclusts %>%
  unnest(cols = c(glanced))

ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line() +
  geom_point()

kclusts %>% 
  filter(k == 4) %>% 
  unnest(cols = augmented) %>% 
  arrange(.cluster) %>% 
  View()

by_country <- threats %>% 
  group_by(threat_type, country) %>% 
  summarize(threat_rate = sum(threatened) / n(),
            n = n(),
            .groups = "drop") %>% 
  arrange(desc(threat_rate))

by_country %>% 
  filter(threat_rate != 0,
         n > 5) %>% 
  ggplot() +
  geom_point(aes(n, threat_rate, color = threat_type))

threats %>% 
  filter(country == "United States") %>% 
  distinct(binomial_name) %>% 
  ggplot() +
  geom_point(aes("", binomial_name))