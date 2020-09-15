library(tidyverse)
library(tokenizers)
library(widyr)

chopped <- read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv')

chopped %>% 
  filter(!is.na(episode_rating)) %>% 
  select(episode_rating, appetizer, entree) %>% 
  pivot_longer(appetizer:entree, names_to = "course", values_to = "ingredient") %>% 
  mutate(ingredient = tokenize_regex(ingredient, patter = ", ")) %>% 
  unnest_longer(ingredient) %>% 
  group_by(ingredient) %>% 
  summarise(ratings = list(episode_rating),
            n = n(),
            .groups = "drop") %>% 
  arrange(desc(n))


chopped %>% 
  filter(!is.na(episode_rating)) %>% 
  ggplot(aes(factor(season), episode_rating)) + 
  geom_boxplot()

chopped %>% 
  filter(!is.na(episode_rating)) %>% 
  transmute(episode_rating,
            entree = tokenize_regex(entree, ",")) %>% 
  rowwise() %>% 
  mutate(n_ingredients = length(entree)) %>% 
  ggplot(aes(n_ingredients, episode_rating)) +
  geom_point()

# What is the most versatile ingredient?

chopped %>% 
  filter(!is.na(episode_rating)) %>% 
  select(episode_rating, series_episode, appetizer, entree) %>% 
  pivot_longer(appetizer:entree, names_to = "course", values_to = "ingredient") %>% 
  mutate(ingredient = tokenize_regex(ingredient, patter = ", "),
         recipe_id = group_indices(., series_episode, course)) %>% 
  unnest_longer(ingredient) %>%
  pairwise_count(ingredient, recipe_id) %>% 
  with_groups(item1, summarise, n = n()) %>% 
  arrange(desc(n))

# What are common ingredient combinations across courses?

chopped %>% 
  select(appetizer:entree) %>% 
  rowid_to_column(var = "pairing") %>% 
  mutate(across(appetizer:entree, tokenize_regex, pattern = ", ")) %>% 
  rowwise() %>% 
  transmute(combinations = list(expand.grid(pairing, appetizer, entree, stringsAsFactors = FALSE))) %>% 
  unnest(combinations) %>% 
  count(Var2, Var3, sort = T) %>% 
  View()

# nothing interesting here...