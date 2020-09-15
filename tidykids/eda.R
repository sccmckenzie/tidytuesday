library(tidyverse)
library(tidykids)
library(widyr)

# states decreasing funding in one area to increase in another - is there a consistent pattern?
delta <- tidykids %>% 
  mutate(year = as.integer(year)) %>% 
  group_by(state, variable) %>% 
  arrange(state, variable, year) %>% 
  mutate(across(raw:inf_adj_perchild, ~ .x - lag(.x))) %>% 
  filter(!is.na(raw)) %>% 
  ungroup()

delta %>% 
  pairwise_cor(variable, year, inf_adj, upper = FALSE) %>% 
  filter(!is.nan(correlation)) %>% 
  arrange(correlation)

# possible effect of interest between highered and unemp
delta %>% 
  filter(variable %in% c("highered", "unemp")) %>% 
  select(-raw, -inf_adj_perchild) %>% 
  pivot_wider(names_from = variable, values_from = inf_adj) %>% 
  ggplot(aes(highered, unemp)) +
  geom_point()

# nevermind, lol
# maybe by state?

delta %>% 
  nest_by(state) %>% 
  mutate(data = list(pairwise_cor(data, variable, year, inf_adj, upper = FALSE))) %>% 
  ungroup() %>% 
  unnest(cols = data) %>% 
  slice_min(order_by = correlation, n = 10)

delta %>% 
  filter(state == "New Jersey",
         variable %in% c("CTC", "TANFbasic")) %>% 
  select(-raw, -inf_adj_perchild) %>% 
  pivot_wider(names_from = variable, values_from = inf_adj) %>% 
  ggplot(aes(CTC, TANFbasic)) +
  geom_point()

tidykids %>% 
  filter(state == "New Jersey",
         variable %in% c("CTC", "TANFbasic")) %>% 
  ggplot(aes(year, inf_adj)) +
  geom_line(aes(group = variable))

# losing interset....

# new idea:
# percent change in spending per child

library(broom)

regression <- tidykids %>% 
  select(state:year, inf_adj_perchild) %>% 
  mutate(year = as.integer(year)) %>% 
  group_by(state, variable) %>% 
  arrange(year) %>% 
  mutate(percent_change = (inf_adj_perchild - inf_adj_perchild[1])/inf_adj_perchild[1]) %>% 
  filter(!any(is.nan(percent_change)),
         !is.na(percent_change)) %>% 
  ungroup() %>% 
  nest_by(state, variable) %>% 
  mutate(mod = list(tidy(lm(percent_change ~ year, data = data))),
         total_change = with(data, percent_change[year == max(year)] - percent_change[year == min(year)])) %>% 
  select(-data) %>% 
  unnest(cols = mod) %>% 
  filter(term == "year") %>% 
  ungroup()

regression %>% 
  filter(p.value < 0.05) %>% 
  arrange(desc(estimate)) %>% 
  slice(1:10, (n() - 9):n())
