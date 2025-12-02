library(tidyverse)

steam.games <- read_csv("~/Library/CloudStorage/GoogleDrive-andrewwm@gmail.com/My Drive/Teaching/99. Statistics Materials/Datasets/video games/steam.games.updated.csv")

steam.games <- steam.games %>% 
  select(1:27)

steam.games <- steam.games %>%
  rename_with(~ gsub(" ", ".", .x))


#steam.games <- steam.games %>% 
#  filter(Median.playtime.forever > 0)


library(lubridate)

steam.games <- steam.games %>%
  mutate(Release.date = ymd(Release.date)) %>%
  filter(Release.date >= as.Date("2022-01-01"))

steam.games <- steam.games %>%
  filter(!(Positive == 0 & Negative == 0))

write.csv(steam.games, "steam.games.csv")

