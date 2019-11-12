library(tidyverse)
library(tidytext)

cran_code <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-12/loc_cran_packages.csv")

cran_code %>% 
  filter(pkg_name == "dplyr")

cran_code %>% 
  group_by(pkg_name) %>% 
  arrange(language) %>% 
  summarise(languages = str_c(language, collapse = ", ")) %>% 
  count(languages, sort = T)

cran_code %>% 
  sample_n(20)

# Are there any n-letter combinations that frequently appear in pkg names?

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

cran_code %>% 
  filter(language == "R") %>% 
  pull(pkg_name)
