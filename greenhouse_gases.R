library(tidyverse)
library()

greenhouse_gases <- as_tibble(dslabs::greenhouse_gases)
carbon_emissions <- as_tibble(dslabs::temp_carbon)
greenhouse_gases %>% 
  group_by(year) %>% 
  mutate(concentration_wt = concentration / sum(concentration)) %>% 
  ggplot(aes(year, concentration_wt)) +
  geom_line(aes(color = gas, group = gas)) +
  theme_minimal()
