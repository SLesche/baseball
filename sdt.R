library(baseballr)
library(tidyverse)

data <- data.table::fread("data/batter_leaderboards.csv") %>%  # done via fg_bat_leaders(2018, 2022)
  janitor::clean_names()

data22 <- fg_bat_leaders(2022, 2022) %>% 
  janitor::clean_names()

# Get the hit rate and fa rate
data22 <- data22 %>% 
  mutate(fa = o_swing_pct, 
         hit = z_swing_pct) %>% 
  mutate(z_fa = scale(fa)[,1],
         z_hit = scale(hit)[,1]) %>% 
  mutate(d = z_hit - z_fa,
         c = - ((z_hit + z_fa)/2))

cor(data22$o_swing_pct, data22$bb_pct)
