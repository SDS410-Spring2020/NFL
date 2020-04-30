# importing libraries
library(datasets)
library(maps)
library(tidyr)
library(tidyverse)
library(readr)
# install.packages("geosphere")
library(geosphere)
library(dplyr)
library(sp)
library(leaflet)
library(magrittr)
library(janitor)
library(lubridate)
#  install.packages("infer")
library(infer)
# install.packages("ggridges")
library(ggridges)
library(scales)

set.seed(1234)


# win and lose vs travel distance 
#determining whether the visiting team won or loss

#game data
games <-read.csv("data/games_Smith.csv") 

# pbp data with just win or lose data 
win_pbp <- read.csv("data/pbp_Smith.csv") %>% group_by(GameKey) %>%
  
  #sorting by PlayID
  
  arrange(PlayID) %>%
  
  #summarizing results
  
  summarize(#final score of home team
    
    finalHomeScore = last(HomeScoreAfterPlay),
    
    #final score of visiting team
    
    finalVisitorScore = last(VisitorScoreAfterPlay),
    
    #whether the visiting team won
    
    Visit_Win = finalVisitorScore > finalHomeScore)

win_data <- games %>%
  inner_join(win_pbp, by="GameKey")

#cleaning df column names to have a common variable with games (home_team)
stadium_data_coordinates <- read.csv("stadium_data_coordinates.csv") %>% 
  clean_names() %>%
  rename(Stadium = stadium) %>%
  rename(Home_Team = home_team) %>%
  rename(Season = season)

visit_team_data <- read.csv("visit_team_data.csv")

win_data_w_coordinates <- win_data %>%
  inner_join(stadium_data_coordinates)

win_data_w_coordinates <- win_data_w_coordinates %>%
  inner_join(visit_team_data)

win_data_w_coordinates$num <-NULL
win_data_w_coordinates$additional_info_location <- NULL
colnames(win_data_w_coordinates)[colnames(win_data_w_coordinates) == 'longitude'] <- 'Home_lat'
colnames(win_data_w_coordinates)[colnames(win_data_w_coordinates) == 'latitude'] <- 'Home_long'

win_data_w_coordinates <- win_data_w_coordinates[!duplicated(win_data_w_coordinates$GameKey), ]

remove.list <- paste(c('Wembley Stadium',"Rogers Centre","Wembley","Twickenham","Estadio Azteca","Twickenham Stadium","Azteca Stadium","Tottenham Hotspur Stadium","Tottenham Hotspur"), collapse = '|')

# extracting the domestic games
win_domestic_games <- win_data_w_coordinates %>%
  filter(!grepl(remove.list, Stadium)) 

win_domestic_games <- win_domestic_games[!duplicated(win_domestic_games$GameKey), ]


# calculate distance

win_calc_domestic <- win_domestic_games %>% 
  rowwise() %>%
  mutate(distance_m = distCosine(c(Home_long, Home_lat),c(Visit_long, Visit_lat))) %>%
  mutate(distance_mile = distance_m*0.6214/1000)


# create dataset

win_distance_all <- win_calc_domestic %>%
  group_by(Visit_Team,Visit_Win) %>%
  summarise(travel_dis =sum(distance_mile)) %>%
  rename(Team = Visit_Team)


win_num_games_visit <- win_calc_domestic %>%
  group_by(Visit_Team,Visit_Win) %>%
  summarise(num_game_visit =n()) %>%
  rename(Team = Visit_Team)

win_num_games_home <- win_calc_domestic %>%
  group_by(Home_Team,Visit_Win) %>%
  summarise(num_game_home =n()) %>%
  rename(Team = Home_Team) 

win_distance_all <- win_distance_all %>%
  inner_join(win_num_games_visit) %>%
  left_join(win_num_games_home)

win_distance_all[is.na(win_distance_all)] <- 0

win_distance_all<- win_distance_all %>%
  mutate(num_game_visit = as.numeric(num_game_visit))%>%
  mutate(total_game = num_game_visit + num_game_home) %>%
  mutate(avg_travel = travel_dis/total_game) %>%
  mutate(Visit_Win = as.character(Visit_Win))


win_distance_all <- win_distance_all %>%
  mutate(Visit_Win = recode(Visit_Win,
                           "TRUE" = "visitor team win",
                           "FALSE" = "home team win")) %>%
  mutate(Visit_Win = as.character(Visit_Win))


# run a permutation test 

# mean difference thurs - not thurs
diff_in_win_distance <- win_distance_all %>% 
  specify(avg_travel ~ Visit_Win) %>% 
  calculate("diff in means", order = c("visitor team win", "home team win"))
diff_in_win_distance


# generate samples 
null_win_distance <- win_distance_all %>%
  specify(avg_travel ~ Visit_Win) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 5000, type = "permute") %>%
  calculate("diff in means", order = c("visitor team win", "home team win"))



null_win_distance %>% 
  get_pvalue(obs_stat = diff_in_win_distance, direction = "both")


# the p-value is 0.901, which means that there’s a 90.1% chance of seeing a difference at least as large as -5.03 in a world where there’s no difference

# --> not statistically significant difference between the two 

ggplot(win_distance_all, aes(x = avg_travel, y = Visit_Win, fill = Visit_Win)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2, scale = 3, color = "white") + 
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), guide = FALSE) +
  labs(x = "Average travel distance from 2007 to 2018", y = NULL) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())


null_win_distance %>% 
  visualise() +
  shade_p_value(obs_stat = diff_in_win_distance,direction = "both") + 
  labs(x = "Difference in mean travel distance\n(visitor team win − home team win)",
       y = "Count",
       subtitle = "Red line shows observed difference in mean travel distances") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) 

