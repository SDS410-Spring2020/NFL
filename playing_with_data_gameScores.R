# Percentage of wins on thursday as opposed to other days. 

#game key 
#isThurs = 1
#new variable = season (year)
#type of season = playoffs or not 
#total number of games 
#total number of thursday games 

#fulldata_with_thurs
#lapply(fulldata_with_thurs, class)

library(janitor)
library(dplyr)

#Full data with Thursday binary variable (created by Anna), cleaned up a little bit 
View(fulldata_with_thurs)
games_clean_data <- fulldata_with_thurs %>%
  clean_names() %>%
  mutate(season = factor(season),
         is_thurs = as.logical(is_thurs))

#New data frame with selected variables
thurs_performance <- games_clean_data %>% 
  select(season, game_key, week, game_day, home_team, visit_team, location, is_thurs) %>% 
  group_by(season) %>%
  filter(is_thurs == 1)

#Cleaned the names of origianl play-by-play data frame 
pbp_clean_data <- pbp %>% 
  clean_names()
  
#Attempting to determine final score of each game 
results_df <- pbp_clean_data %>% 
  select(game_key,
         play_id,
         home_club_code, 
         visitor_club_code, 
         home_score_before_play, 
         home_score_after_play,
         visitor_score_before_play, 
         visitor_score_after_play, 
         is_scoring_play)

scores <- results_df %>%
  select(game_key, home_club_code, visitor_club_code, is_scoring_play) %>% 
  group_by(game_key, home_club_code, visitor_club_code) %>% 
  #total number of scoring plays in a game 
  summarise(num_scoring_plays = sum(is_scoring_play == 1),
            num_non_scoring_plays = sum(is_scoring_play == 0))

#Tells you score of both teams after each scoring play 
#Doesn't tell you which team made the scoring play 

lapply(score_after_play, class)

score_after_play <- results_df %>% 
  select(game_key, 
         home_club_code, 
         visitor_club_code, 
         is_scoring_play, 
         home_score_after_play, 
         visitor_score_after_play) %>%
  group_by(game_key, home_club_code, visitor_club_code) %>% 
  mutate(
    is_scoring_play = is.logical(is_scoring_play),
    home_score_after_play = 
      case_when(
        is_scoring_play == 0 ~ 0,
        TRUE ~ home_score_after_play),
    visitor_score_after_play = 
      case_when(
        is_scoring_play == 0 ~ 0, 
        TRUE ~ visitor_score_after_play))


#goal: if scoring play is 1, add up all the home score after play
#example: 

#Number of plays per game 
num_of_plays_per_game <- results %>% 
  select(game_key, home_club_code, visitor_club_code) %>% 
  tally()

final_score <- pbp_clean_data %>%
  select(game_key, 
         play_id,
         is_scoring_play)

#Finding out scores 

#add count for each type of play by play in each games 
