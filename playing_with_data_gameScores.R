# Percentage of wins on thursday as opposed to other days. 

game key 
isThurs = 1
new variable = season (year)
type of season = playoffs or not 
total number of games 
total number of thursday games 

fulldata_with_thurs
lapply(fulldata_with_thurs, class)

library(janitor)
library(dplyr)
games_clean_data <- fulldata_with_thurs %>%
  clean_names() %>%
  mutate(season = factor(season),
         is_thurs = as.logical(is_thurs))
  
thurs_performance <- games_clean_data %>% 
  select(season, game_key, week, game_day, home_team, visit_team, location, is_thurs) %>% 
  group_by(season) %>%
  filter(is_thurs == 1)

pbp_clean_data <- pbp_Smith %>% 
  clean_names()
  
results <- pbp_clean_data %>% 
  select(game_key,
         play_id,
         home_club_code, 
         visitor_club_code, 
         home_score_before_play, 
         home_score_after_play,
         visitor_score_before_play, 
         visitor_score_after_play) %>% 
  group_by(game_key)

num_of_plays_per_game <- results %>% 
  select(game_key, home_club_code, visitor_club_code) %>% 
  tally()

lapply(pbp_clean_data, class)
  


