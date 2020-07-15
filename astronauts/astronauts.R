library(tidyverse)
library(modelr)
library(showtext)
library(ggtext)

astronauts <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

nasa <- astronauts %>% 
  filter(str_detect(selection, "NASA")) %>% 
  with_groups(name, 
              summarise,
              year_first_mission = min(year_of_mission),
              age_first_mission = year_first_mission - first(year_of_birth),
              gender = first(sex))

mod <- lm(age_first_mission ~ year_first_mission, data = nasa)

font_add_google("Noto Sans TC")
showtext_auto()
x11()

nasa %>% 
  add_residuals(mod) %>% 
  add_predictions(mod) %>% 
  mutate(resid_scaled = exp(abs(resid)) ^ -0.6) %>% 
  ggplot(aes(year_first_mission, age_first_mission)) +
  geom_jitter(aes(alpha = resid_scaled), color = "#00BACA", size = 2) +
  geom_line(aes(year_first_mission, pred), color = "#00BACA", size = 2) +
  theme_minimal() +
  scale_y_continuous(limits = c(30, 50)) +
  labs(title = "Average Age of NASA Astronauts @ First Mission",
       subtitle = "It's never too late!",
       x = "Year",
       y = "",
       caption = '<b style = "color: #C05858">@sccmckenzie</b>') +
  theme(plot.background = element_rect(fill = "#000817"),
        panel.grid = element_blank(),
        axis.line = element_line(color = "#FBF2D7"),
        text = element_text(color = "#FBF2D7", family = "Noto Sans TC", size = 60),
        axis.text = element_text(color = "#FBF2D7"),
        plot.caption = element_markdown(size = 50, color = "#F7BCB5"),
        legend.position = "none")

ggsave(here::here("astronauts.png"), type = "cairo")
