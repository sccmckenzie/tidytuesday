library(tidyverse)

# Read data
cran_code <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-12/loc_cran_packages.csv")


# Are there any n-letter combinations that frequently appear in pkg names?
pkg_names <- cran_code %>% 
  filter(language == "R") %>% 
  pull(pkg_name)

# Define string subsetting function
cut.letters <- function(x, n) {
  l <- str_length(x)
  
  if (l < n) {
    NA
  } else {
    i <- c(1:(l - n + 1))
    j <- i + n - 1
    
    map2(.x = i, .y = j, .f = str_sub, string = x)
  } 
}

cut.pkg_names <- function(n) {
  map(.x = pkg_names, .f = cut.letters, n = n) %>% 
    unlist()
}

# Generate results
results <- tibble(n = 5:10) %>% 
  mutate(combinations = map(.x = n, .f = cut.pkg_names)) %>% 
  unnest(combinations) %>% 
  count(combinations, sort = TRUE)

# There are lots of letter combinations that aren't quite words... 
# Using lexicon package to subset results down to actual words
results_top10 <- results %>% 
  filter(combinations %in% lexicon::grady_augmented,
         !combinations %in% c("luster", "ample", "elect", "lysis")) %>% 
  slice(1:10)

results_top10 %>% 
  ggplot(aes(fct_reorder(combinations, n), n)) +
  geom_col(fill = "#3643c3") +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "CRAN keywords",
       subtitle = "Most common words/affixes found \nin CRAN package names", 
       x = "",
       y = "Frequency",
       caption = "Data: TidyTuesday/CRAN\nPackages: tidyverse, lexicon") +
  theme_classic() +
  theme(plot.background = element_rect(fill = "#baf4ff"),
        panel.background = element_rect(fill = "#ccf7ff"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"))

