epl <- read.csv("E:/Github/dkustats101fall2025s2/homeworks/Unit 1 homework/englishpl.stats.csv")

epl <- epl %>% 
  select(-class)

library(readr)

epl <- epl %>% 
  mutate(attendance = parse_number(attendance))

epl <- epl  %>% 
  rename(home_team_league_rank = Home.Team,
         away_team_league_rank = Away.Team)

epl <- epl  %>% 
  rename(home_goals = Goals.Home,
         away_goals = Away.Goals) %>%  
  rename(match_start_time = clock)

library(lubridate)

epl <- epl %>% 
  mutate(date_cleaned = dmy(date))  %>% 
  mutate(year = year(date_cleaned)) %>% 
  mutate(month = month(date_cleaned)) %>% 
  mutate(day = day(date_cleaned)) %>% 
  mutate(day_of_week = wday(date_cleaned, label = TRUE)) %>% 
  relocate(date_cleaned, year, month, day, day_of_week, .after=date)

epl <- epl %>% 
  mutate(match_start_time_u = 
    if_else(
      match_start_time == "12:30p" | match_start_time == "12:00p", 
      paste0(match_start_time, "m"),        
      match_start_time                      
    )
  )

epl <- epl %>% 
  select(-match_start_time) %>% 
  rename(match_start_time = match_start_time_u)

epl <- epl %>% 
  mutate(match_start_time_cleaned = parse_date_time(match_start_time, "%I:%M%p")) %>% 
  mutate(match_start_hour = hour(match_start_time_cleaned)) %>% 
  mutate(match_start_minute = minute(match_start_time_cleaned)) %>% 
  relocate(match_start_time, match_start_time_cleaned, match_start_hour, match_start_minute, .after=day_of_week)

epl <- epl %>% 
  mutate(home_team = str_extract(links, "(?<=/football/)[^/]+(?=-vs-)"),
         away_team = str_extract(links, "(?<=-vs-)[^/]+(?=/)")) %>% 
  relocate(home_team, away_team, .after=match_start_minute)

write.csv(epl, "E:/Github/dkustats101fall2025s2/homeworks/Unit 1 homework/epl.stats.cleaned.csv")