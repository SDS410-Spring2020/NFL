# Goal: Find statistics about varaibles impact on performance  
# - percentage of wins on thursday as opposed to non-thursday games 
# - 

# Data Exploration: 
# - game key, isThurs
---------------------------------------------------------------------

#loading libraries 
library(janitor)
library(lubridate)
library(dplyr)

#reading in the two nfl data frames 
games<- read.csv("data/games_Smith.csv")
pbp<- read.csv("data/pbp_Smith.csv")

#cleaning data 
games <- games %>% 
  clean_names() %>% 
  mutate(season = factor(season)) 

pbp <- pbp %>% 
  clean_names()

#joining games and pbp (from Elaona)
full_data <- games %>% 
  left_join(pbp, by="game_key")

#adding is_thurs variable (from Anna)
full_data <- full_data %>% 
  mutate(is_thurs = ifelse(game_day == "Thursday", 1, 0),
         is_thurs = is.logical(is_thurs)) 

#attempting to determine final score of each game 
#using pbp data frame
final_score_vars <- pbp %>% 
  select(game_key,
         play_id,
         home_club_code, 
         visitor_club_code, 
         home_score_before_play, 
         home_score_after_play,
         visitor_score_before_play, 
         visitor_score_after_play, 
         is_scoring_play) 

#checking for na values 
is.na(final_score)

#finds total number of scoring plays in each game (does not differentiate between teams)  
scoring_plays <- final_score_vars %>%
  select(game_key,
         is_scoring_play) %>% 
  group_by(game_key) %>%
  #total number of scoring plays in a game - does not differentiate between teams 
  summarise(num_scoring_plays = sum(is_scoring_play == 1),
            num_non_scoring_plays = sum(is_scoring_play == 0)) 

#mutates home/visitor_score_after_play. The observations of these variables will only show 
#score if the observation (or play) is a scoring play. Otherwise, observation will be a 0.  
mutate_score_after_play <- pbp %>% 
  select(game_key, 
         home_club_code, 
         visitor_club_code, 
         is_scoring_play, 
         home_score_after_play, 
         visitor_score_after_play) %>%
  group_by(game_key, 
           home_club_code, 
           visitor_club_code) %>% 
  mutate( 
    home_score_after_play = 
      case_when(
        is_scoring_play == 0 ~ 0,
        TRUE ~ as.double(home_score_after_play)),
    visitor_score_after_play = 
      case_when(
        is_scoring_play == 0 ~ 0, 
        TRUE ~ as.double(visitor_score_after_play)))

#filters out home/visitor_score_after_play with values that has 0.
score_after_play <- mutate_score_after_play %>% 
  filter(!home_score_after_play == 0,
         !visitor_score_after_play == 0) %>%
  group_by(game_key)

#shows final score the home and visitor team of each game 
final_scores <- score_after_play %>% 
  mutate(home_final = max(home_score_after_play),
            visitor_final = max(visitor_score_after_play))

#tells you winning team 
winner <- final_scores %>% 
  mutate(
    winning_team = 
      case_when(
        home_final > visitor_final ~ as.character(home_club_code), 
        home_final < visitor_final ~ as.character(visitor_club_code),
        home_final == visitor_final ~ "tie"), 
      is_tie = ifelse(winning_team == "tie", 1, 0))

# nfl game attendance 
# rivarlries 
# winning percentages 
# offense production - expected points 
# delta wp for each play 
# rankings of the games 


