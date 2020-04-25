library(tidyverse)
library(readxl)
library(lubridate)
library(countrycode)
library(showtext)
library(glue)
library(ggtext)
library(rcartocolor)

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

####### Generate plots
font_add_google("Roboto")
showtext_auto()
x11()

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
  scale_y_continuous(expand = c(0, 0), position = "right") +
  coord_flip() +
  scale_fill_carto_d() +
  labs(x = "",
       y = "Billions of kg/year",
       fill = "Continent",
       title = glue("Food Consumption - CO<sub>2</sub> Emissions"),
       subtitle = "Animal-based food production accounts for 87% of 2020 projections",
       caption = "@sccmckenzie") +
  theme(text = element_text("Roboto", size = 35, color = "#242424"),
        plot.title = element_markdown(size = 60),
        plot.subtitle = element_text(size = 35, margin = margin(0, 0, 10, 0)),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 30, color = "#242424"),
        plot.title.position = "plot",
        plot.caption.position = "plot")

ggsave(here::here("food_consumption", "food_consumption.png"), type = "cairo")


            