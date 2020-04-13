pacman::p_load(rtweet,tidyverse)


## search for 5000 tweets 
rt_se_va <- search_tweets(
  "#amloseva", n = 5000, include_rts = FALSE
)

## search for 5000 tweets 
rt_se_queda <- search_tweets(
  "#amlosequeda", n = 5000, include_rts = FALSE
)


rt_se_queda %>% 
  select(screen_name) %>% 
  group_by(screen_name) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  
