library(tidyverse)
library(readxl)
library(lubridate)
library(countrycode)
library(ggsci)

population <- read_xlsx(here::here("food_consumption", "population.xlsx")) %>% # obtained from gapminder - http://gapm.io/dl_pop
  filter(time == year(now())) %>% 
  select(geo, population)

food_consumption <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')

food_consumption$continent <- countrycode(sourcevar = food_consumption[, "country"][[1]],
                                          origin = "country.name",
                                          destination = "continent")

food_consumption$geo <- countrycode(sourcevar = food_consumption[, "country"][[1]],
                                    origin = "country.name",
                                    destination = "genc3c") %>% 
  str_to_lower()

food_consumption %>% 
  left_join(population) %>% 
  drop_na() %>% 
  mutate(total_emmission = co2_emmission * population / 1e9,
         food_category = str_replace(food_category, " and ", " & "),
         food_category = fct_recode(food_category,
                                    `Beef *` = "Beef",
                                    `Milk - inc. cheese*` = "Milk - inc. cheese",
                                    `Pork *` = "Pork",
                                    `Lamb & Goat *` = "Lamb & Goat",
                                    `Poultry *` = "Poultry",
                                    `Fish *` = "Fish",
                                    `Eggs *` = "Eggs")) %>% 
  group_by(food_category, continent) %>% 
  summarise(total_emmission = sum(total_emmission)) %>% 
  ggplot(aes(fct_reorder(food_category, total_emmission), total_emmission)) +
  geom_col(aes(fill = continent)) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "black"),
        plot.title.position = "plot",
        plot.caption.position = "plot") +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_locuszoom() +
  labs(x = "",
       y = "",
       fill = "Continent",
       title = bquote(CO^2~Emissions~Driven~By~Food~Consumption),
       subtitle = paste0("Billions of kg/year - ", year(now()), " projection"),
       caption = "* denotes animal-based food product")




            