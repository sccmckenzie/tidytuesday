library(tidyverse)
library(rvest)
library(showtext)

raw <- read_html("https://en.wikipedia.org/wiki/List_of_United_States_presidential_elections_by_popular_vote_margin") %>% 
  html_nodes("table") %>% 
  .[[4]] %>% 
  html_table() %>% 
  janitor::clean_names()

names(raw) <- str_c(names(raw), raw[1,], sep = "_") %>% str_remove("_$")
clean <- raw[-c(1, 2),] %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  transmute(year = as.integer(election_2),
            winner = str_remove(winner_party, "^[:alpha:]+,"),
            party = winner_party_2,
            electoral_votes = str_extract(electoral_college_votes, "\\d+") %>% as.integer(),
            electoral_margin = electoral_votes - 270,
            winner_votes = str_remove_all(popular_vote_3_votes, ",") %>% as.integer(),
            popular_margin = popular_vote_4_margin %>% str_remove_all(., ","),
            popular_margin = case_when(
              str_detect(popular_margin, "\\D") ~ -1 * as.double(str_extract(popular_margin, "\\d+")),
              TRUE ~ as.double(popular_margin)
            ),
            loser_votes = winner_votes - popular_margin) %>% 
  filter(year >= 1964)

clean[clean$year == 2020, "winner_votes"] <- 75550480
clean[clean$year == 2020, "popular_margin"] <- 4288057
clean[clean$year == 2020, "loser_votes"] <- 71188487

plot_df <- clean %>% 
  transmute(year,
            winner = party,
            `Dem.` = ifelse(party == "Dem.", winner_votes, loser_votes),
            `Rep.` = ifelse(party == "Dem.", loser_votes, winner_votes)) %>% 
  pivot_longer(3:4) %>% 
  mutate(winner = winner == name)


showtext_auto()

font_add_google("Lato")

x11()

plot_df %>% 
  ggplot(aes(year, value)) +
  geom_line(aes(group = name, color = name), size = 1) +
  geom_point(aes(color = name), size = 2) +
  geom_rug(aes(year, color = name), data = filter(plot_df, winner), size = 2, inherit.aes = FALSE) +
  geom_label(label = "Axis color denotes\nelectoral result", hjust = 0, x = 2008, y = 35e6, family = "Lato", size = 15, lineheight = 0.35) +
  geom_curve(x = 2008, y = 35e6, xend = 2000, yend = min(plot_df$value), arrow = arrow(length = unit(0.02, "npc")), curvature = 0.4) +
  scale_color_manual(values = c("dodgerblue", "red"), name = NULL) +
  scale_x_continuous(breaks = scales::breaks_width(8, offset = 4)) +
  scale_y_continuous(labels = scales::label_number()) +
  theme_minimal() +
  theme(panel.grid.minor.y = element_blank(),
        text = element_text(family = "Lato", size = 60),
        plot.title.position = "plot") +
  labs(title = "US Presidential Election Popular Votes", x = "", y = "", caption = "@sccmckenzie")

ggsave("election/election.png", type = "cairo")
 