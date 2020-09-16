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
offset <- 2

green <- "#00AE91"
red <- "#ED5C4D"

plot_data <- tidykids_summary %>%
  mutate(mean_percent_change = if_else(mean_percent_change > 1, 1.01, mean_percent_change)) %>% 
  mutate(x = c(seq(s + offset, 1 + offset), seq(0.475 + offset, -0.475 - offset, length = nrow(.) - s * 2), seq(-1 - offset, -s - offset)),
         width = c(rep(0.9, s), rep(0.01, nrow(.) - s * 2), rep(0.9, s)),
         increase = mean_percent_change > 0,
         label = case_when(
           row_number() %in% 1:s ~ glue("<b style = 'color:{green}'>{state}: {variable}</span>"),
           row_number() %in% (nrow(.) - s + 1):nrow(.) ~ glue("<b style = 'color:{red}'>{state}: {variable}</span>"),
           TRUE ~ NA_character_
         ),
         label_y = case_when(
           row_number() %in% 1:s ~ -0.025,
           row_number() %in% (nrow(.) - s + 1):nrow(.) ~ 0.025,
           TRUE ~ NA_real_
         ),
         hjust = case_when(
           row_number() %in% 1:s ~ "outward",
           row_number() %in% (nrow(.) - s + 1):nrow(.) ~ "inward",
           TRUE ~ NA_character_
         ))

# font_add_google("Quicksand")
# showtext_auto()
x11()
ggplot(plot_data) +
  geom_col(aes(x, mean_percent_change, fill = increase), width = plot_data$width, position = "dodge") +
  geom_hline(yintercept = 0, size = 0.2) +
  geom_richtext(aes(x = x, y = label_y, label = label), hjust = plot_data$hjust, family = "Quicksand", label.color = NA, fill = NA, size = 10) +
  geom_hline(yintercept = 1, linetype = 2, alpha = 0.4) +
  geom_segment(x = offset + s, xend = offset + s, y = 1, yend = 1.1, arrow = arrow(length = unit(0.03, "npc")), size = 0.2) +
  annotate("label", offset + s, 1.2, label = "811%", size = 10) +
  geom_segment(x = offset + s - 1, xend = offset + s - 1, y = 1, yend = 1.1, arrow = arrow(length = unit(0.03, "npc")), size = 0.2) +
  annotate("label", offset + s - 1, 1.2, label = "331%", size = 10) +
  scale_y_continuous(limits = c(-1, 1.2), position = "right", labels = scales::percent_format()) +
  coord_flip() +
  scale_fill_manual(values = c(red, green)) +
  labs(title = "US Government Spending - Top Categories",
       subtitle = "Mean Annual Percentage Change",
       x = "",
       y = "",
       caption = glue('<b>@sccmckenzie</b> | Source: Urban Institute')) +
  theme_minimal() +
  theme(axis.line.x = element_line(size = 0.2),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 40),
        legend.position = "none",
        plot.title.position = "plot",
        panel.grid = element_blank(),
        text = element_text(family = "Quicksand", size = 50),
        plot.caption = element_markdown(size = 30))

ggsave(here::here("tidykids", "tidykids.png"), type = "cairo")
