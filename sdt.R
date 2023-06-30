library(baseballr)
library(tidyverse)
library(ggcorrplot)
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
         c = - ((z_hit + z_fa)/2)) %>% 
  mutate(d_plus = 100 + 10*d,
         c_plus = 100 + 10*c)

relevant_stats <- c(
  "hr", "r", "rbi", "bb", "so", "avg", "bb_pct", "k_pct", "bb_k",
  "obp", "slg", "ops", "iso", "babip", "w_oba", "war", 
  "w_rc_plus", "wpa", "wpa_plus",
  "o_swing_pct", "z_swing_pct", "swing_pct",
  "o_contact_pct", "z_contact_pct",
  "contact_pct", "zone_pct", "f_strike_pct", "sw_str_pct"
  )


valid <- data.frame(
  stat = 1:length(relevant_stats),
  d = 1:length(relevant_stats),
  c = 1:length(relevant_stats),
  o_swing = 1:length(relevant_stats),
  z_swing = 1:length(relevant_stats)
)

for (i in seq_along(relevant_stats)){
  stat = relevant_stats[i]
  valid$stat[i] = stat
  valid$d[i] = cor(data22$d_plus, data22[, stat])
  valid$c[i] = cor(data22$c_plus, data22[, stat])
  valid$o_swing[i] = cor(data22$o_swing_pct, data22[, stat])
  valid$z_swing[i] = cor(data22$z_swing_pct, data22[, stat])
}

valid <- valid %>%
  pivot_longer(
    cols = c("d", "c", "o_swing", "z_swing"),
    names_to = "method"
  )

valid %>% 
  ggplot(
    aes(
      x = stat,
      y = value,
      color = method
    )
  ) +
  geom_point()+
  coord_flip()+
  theme_classic()
