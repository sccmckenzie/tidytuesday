# Tidy Tuesday - wk 13
# 2020-03-26
# Traumatic Brain Injuries
# https://github.com/sccmckenzie/tidytuesday

library(tidyverse)
library(glue) # easily tunnel environment variables into strings
library(httr) # directly download files from url
library(readxl)
library(ggsci) # palettes
library(ggtext) # advanced ggplot text rendering
library(showtext)
library(patchwork) # seamlessly combine plots into same graphic

# Get population data from US Census Bereau
paths <- bind_rows(tibble(year = c(2006, 2007, 2008, 2009, 2010, 2013), ext = c("-1.xls", rep(".xls", 5))),
                   tibble(year = c(2011, 2012, 2014), ext = ".xlsx")) %>% 
  mutate(url = glue("https://www2.census.gov/programs-surveys/demo/tables/age-and-sex/{year}/age-sex-composition/{year}gender_table1{ext}")) %>% 
  arrange(year)

census <- paths %>% 
  mutate(data = map2(url, ext, ~ {
    GET(.x, write_disk(tf <- tempfile(fileext = .y)))
    df <- read_excel(tf, skip = 6, col_names = FALSE) %>% 
      select(1:3) %>% 
      setNames(., c("age_group", "number", "percent")) %>% 
      filter(age_group %in% c(".Under 15 years", ".15 to 17 years", ".18 to 20 years", ".21 to 44 years", ".45 to 64 years", ".65 years and over")) %>% 
      mutate_at(vars(number, percent), as.double) %>%
      mutate(age_group = str_remove(age_group, "^\\."))
    file.remove(tf)
    return(df)
  })) %>% 
  select(year, data) %>% 
  unnest(cols = data)
  
# Read TidyTuesday data
tbi_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_year.csv')


####### Generate plots
font_add_google("Heebo")
showtext_auto()
x11()
pal <- rev(pal_npg("nrc")(3))
census_color <- pal_npg("nrc")(4)[4]

# Unintentional Falls

p1 <- tbi_year %>%
  filter(injury_mechanism == "Unintentional falls") %>%
  mutate(type = ifelse(str_detect(type, "Emer"), "ER Visits", type),
         type = factor(type, levels = c("ER Visits", "Hospitalizations" , "Deaths"))) %>%
  group_by(type) %>%
  mutate(percent_change = (number_est - first(number_est)) / first(number_est)) %>%
  ggplot(aes(year, percent_change)) +
  geom_line(aes(color = type), size = 1.5) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(values = pal) +
  theme_minimal() +
  labs(title = "Unintentional falls",
       subtitle = glue(" % change from '08 - <b style='color: {pal[1]}'>ER Visits</b> , <b style='color: {pal[2]}'>Hospitalizations</b> , and <b style='color: {pal[3]}'>Deaths</b> "),
       x = "",
       y = "") +
  theme(text = element_text("Heebo", size = 50, color = "#242424"),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(size = 40, color = "darkslategray"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.5),
        axis.ticks.x = element_line(color = "darkgray", size = 0.5),
        legend.position = "none")

p2 <- census %>% 
  filter(age_group == "65 years and over") %>% 
  ggplot(aes(year, percent)) +
  geom_line(color = census_color, size = 1.5) +
  geom_hline(yintercept = 12, color = "black", size = 1) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  labs(title = " % of US population",
       subtitle = glue("Ages <b style='color: {census_color}'>65 years and older</b>"),
       x = "",
       y = "") +
  theme(text = element_text("Heebo", size = 50, color = "#242424"),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(size = 40, color = "darkslategray"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.5),
        axis.ticks.x = element_line(color = "darkgray", size = 0.5),
        legend.position = "none")

(p1 | p2) + 
  plot_annotation(title = "Traumatic Brain Injuries",
                  caption = "@sccmckenzie (Data: cdc.gov, census.gov)",
                  theme = theme(plot.title = element_text("Heebo", size = 100, color = "#242424"),
                                plot.caption = element_markdown("Heebo", size = 50, color = "#242424")))

ggsave("test.png", type = "cairo")
