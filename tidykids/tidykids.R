library(tidyverse)
library(broom)
library(tidykids)
library(glue)
library(showtext)
library(ggtext)

tidykids_summary <- tidykids %>% 
  select(state:year, inf_adj_perchild) %>% 
  mutate(year = as.integer(year)) %>% 
  group_by(state, variable) %>% 
  arrange(year) %>% 
  mutate(percent_change = (inf_adj_perchild - lag(inf_adj_perchild))/lag(inf_adj_perchild)) %>% 
  filter(!is.na(percent_change),
         is.finite(percent_change)) %>% 
  summarize(mean_percent_change = mean(percent_change), .groups = "drop") %>% 
  arrange(desc(mean_percent_change))


s <- 5
offset <- 3

green <- "#7FB185"
red <- "#ED5C4D"

plot_data <- tidykids_summary %>%
  mutate(mean_percent_change = if_else(mean_percent_change > 1, 1, mean_percent_change)) %>% 
  mutate(x = c(seq(s + offset, 1 + offset), seq(0.5 + offset, -0.5 - offset, length = nrow(.) - s * 2), seq(-1 - offset, -s - offset)),
         width = c(rep(0.9, s), rep(0.01, nrow(.) - s * 2), rep(0.9, s)),
         increase = mean_percent_change > 0,
         label = ifelse(row_number() %in% c(1:s, (nrow(.) - s + 1):nrow(.)), glue("{state} {variable}"), NA_character_),
         hjust = case_when(
           row_number() %in% 1:s ~ "outward",
           row_number() %in% (nrow(.) - s + 1):nrow(.) ~ "inward",
           TRUE ~ NA_character_
         ))

font_add_google("Quicksand")
ggplot(plot_data) +
  geom_col(aes(x, mean_percent_change, fill = increase), width = plot_data$width, position = "dodge") +
  geom_hline(yintercept = 0, size = 0.2) +
  geom_text(aes(x = x, y = 0, label = label), hjust = plot_data$hjust, family = "Quicksand") +
  scale_y_continuous(limits = c(-1, 1.2), position = "right", labels = scales::percent_format()) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line.x = element_line(size = 0.2),
        axis.text.y = element_blank(),
        legend.position = "none",
        plot.title.position = "plot",
        panel.grid = element_blank(),
        text = element_text(family = "Quicksand")) +
  scale_fill_manual(values = c(red, green)) +
  labs(title = "US Public Spending - Top Categories", subtitle = "Mean Annual Percentage Change", x = "", y = "")
