# Goal: Find statistics about varaibles impact on performance  
# - percentage of wins on thursday as opposed to non-thursday games 
# - 

# Data Exploration: 
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

important variables
is_scoring_play - 
home_score_after play - 
visitor_score_after_play - 
home_final
visitor_final 
winning_team
is_tie

#attempting to determine final score of each game 
#using pbp data frame

#finds total number of scoring plays in each game (does not differentiate between teams)  


#mutates home/visitor_score_after_play. The observations of these variables will only show 
#score if the observation (or play) is a scoring play. Otherwise, observation will be a 0.  

#filters out home/visitor_score_after_play with values that has 0.

#shows final score the home and visitor team of each game 

#tells you winning team 

#joining both data frames (includes seasons btw 2010 and 2019)
joined <- inner_join(pbp, games, by = NULL, copy = FALSE) 

#full data with final scores 
data_with_scores <- joined %>% 
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
        TRUE ~ as.double(visitor_score_after_play))) %>% 
  filter(!home_score_after_play == 0,
         !visitor_score_after_play == 0) %>%
  group_by(game_key) %>%
  mutate(home_final = max(home_score_after_play),
         visitor_final = max(visitor_score_after_play),
         winning_team = 
           case_when(
             home_final > visitor_final ~ as.character(home_club_code), 
             home_final < visitor_final ~ as.character(visitor_club_code),
             home_final == visitor_final ~ "tie"), 
         is_tie = ifelse(winning_team == "tie", 1, 0))
