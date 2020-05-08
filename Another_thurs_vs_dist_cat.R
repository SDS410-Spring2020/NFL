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


#game data
games <-read.csv("data/games_Smith.csv") 

#pbp data
pbp<-read.csv("data/pbp_Smith.csv") %>% 
  group_by(GameKey) %>%
  slice(n()) %>%
  ungroup 

short_pbp <- pbp %>%
  select(1,18,19,30)%>%
  mutate(margin_victory = HomeScoreAfterPlay - VisitorScoreAfterPlay)


win_mergin_data <- games %>%
  inner_join(short_pbp, by="GameKey") %>%
  mutate(is_thurs = ifelse(Game_Day == "Thursday","Thursday games","Not Thursday games"))


#cleaning df column names to have a common variable with games (home_team)
# stadium coordinates data
stadium_data_coordinates <- read.csv("stadium_data_coordinates.csv") %>% 
  clean_names() %>%
  rename(Stadium = stadium) %>%
  rename(Home_Team = home_team) %>%
  rename(Season = season)

# visit team stadium coordinates data
visit_team_data <- read.csv("visit_team_data.csv")


# merging the datasets
mv_w_coordinates <- win_mergin_data %>%
  inner_join(stadium_data_coordinates )

mv_w_coordinates <- mv_w_coordinates %>%
  inner_join(visit_team_data)

mv_w_coordinates$num <-NULL
mv_w_coordinates$additional_info_location <- NULL
colnames(mv_w_coordinates)[colnames(mv_w_coordinates) == 'longitude'] <- 'Home_lat'
colnames(mv_w_coordinates)[colnames(mv_w_coordinates) == 'latitude'] <- 'Home_long'

mv_w_coordinates <- mv_w_coordinates[!duplicated(mv_w_coordinates$GameKey), ]

remove.list <- paste(c('Wembley Stadium',"Rogers Centre","Wembley","Twickenham","Estadio Azteca","Twickenham Stadium","Azteca Stadium","Tottenham Hotspur Stadium","Tottenham Hotspur"), collapse = '|')

mv_domestic_games <- mv_w_coordinates %>%
  filter(!grepl(remove.list, Stadium)) %>%
  mutate(is_thurs = ifelse(Game_Day == "Thursday", 1, 0)) 

mv_domestic_games <- mv_domestic_games[!duplicated(mv_domestic_games$GameKey), ]

mv_international_games <- mv_domestic_games %>%
  filter(grepl(remove.list, Stadium)) %>%
  mutate(is_thurs = ifelse(Game_Day == "Thursday", 1, 0)) 

mv_international_games <- mv_international_games[!duplicated(mv_international_games$GameKey), ]


# calculate distance

mv_calc_domestic <- mv_domestic_games %>% 
  rowwise() %>%
  mutate(distance_m = distCosine(c(Home_long, Home_lat),c(Visit_long, Visit_lat))) %>%
  mutate(distance_mile = distance_m*0.6214/1000)

mv_calc_domestic <- mv_calc_domestic[!duplicated(mv_calc_domestic$GameKey), ]

mv_calc_domestic <- mv_calc_domestic %>%
  mutate(distance = ifelse(distance_mile > 2000, "very far",
                           ifelse(distance_mile > 1000 && distance_mile <= 2000, "far", "close"))) 



# Categorizing the travel distance 
# filtered the data to just visit teams

mv_cat_distance_all <- mv_calc_domestic %>%
  group_by(Visit_Team, distance,is_thurs) %>%
  summarise(travel_dis =sum(distance_mile)) %>%
  rename(Team = Visit_Team)


mv_cat_num_games_visit <- mv_calc_domestic %>%
  group_by(Visit_Team, distance,is_thurs) %>%
  summarise(num_game_visit =n()) %>%
  rename(Team = Visit_Team)


mv_cat_average_visit <- mv_calc_domestic %>%
  group_by(Visit_Team, distance,is_thurs)%>%
  summarise(avg_visit_mv = mean(margin_victory))%>%
  rename(Team = Visit_Team)


mv_cat_distance_all <- mv_cat_distance_all %>%
  inner_join(mv_cat_num_games_visit ) %>%
  left_join(mv_cat_average_visit) %>%
  mutate(avg_travel = travel_dis/num_game_visit) 

Thurs_data <-mv_cat_distance_all %>%
  filter(is_thurs == 1)

Thurs_data %>%
  group_by(distance) %>%
  summarise(num_games = n())

# close = 32
# far = 21
# very far = 2

mean(Thurs_data$avg_visit_mv)
# 1.49151515

# run a permutation test 

# obtaining the test stats
diff_cat_distance_thurs <- Thurs_data %>%
  specify(avg_visit_mv ~ distance) %>% 
  calculate(stat = "F")

diff_cat_distance_thurs
# 1.82

# generate samples 
null_mv_travel_thurs <- Thurs_data %>%
  specify(avg_visit_mv ~ distance) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 5000, type = "permute") %>%
  calculate("F")

null_mv_travel_thurs

null_mv_travel_thurs %>% 
  get_pvalue(obs_stat = diff_cat_distance_thurs, direction = "right")

# the p-value is 0.169


  
ggplot(Thurs_data, aes(x = avg_visit_mv, y = distance, fill = distance)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2, scale = 3, color = "white") + 
  scale_fill_manual(values = c("#E69F00", "#56B4E9","#999999"), guide = FALSE) +
  labs(x = "Average Margin of Victory from 2010 to 2019", y = NULL,
       title="Comparison of Average Margin of Victory for Visiting Teams for Thursday Games \n (close < 1000 miles, 1000 <= far <= 2000, 2000 < very far)") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) 


null_mv_travel_thurs %>% 
  visualise() +
  shade_p_value(obs_stat = diff_cat_distance_thurs, direction = "right") + 
  labs(x = "The F-value from comparing three different groups for Thursday games \n (close < 1000 miles, 1000 <= far <= 2000, 2000 < very far)",
       y = "Count",
       subtitle = "Red line shows observed variation of margin of victory") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) 


# run a bootstrap test 
# However, I am not sure if I am running the test correctly.

# generate samples 
boot_mv_travel_thurs <- Thurs_data %>%
  specify(response = avg_visit_mv, explanatory = distance ) %>%
  hypothesize(null = "point", mu = 1.49) %>%
  generate(reps = 5000, type = "bootstrap") %>%
  calculate("F")




boot_mv_travel_thurs

boot_mv_travel_thurs %>% 
  get_pvalue(obs_stat = diff_cat_distance_thurs, direction = "right")

# the p-value is 0.599


boot_mv_travel_thurs %>% 
  visualise() +
  shade_p_value(obs_stat = diff_cat_distance_thurs, direction = "right") + 
  labs(x = "The F-value from comparing three different groups for Thursday games \n (close < 1000 miles, 1000 <= far <= 2000, 2000 < very far)",
       y = "Count",
       subtitle = "Red line shows observed variation of margin of victory") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) 




