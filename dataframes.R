#games
games<-read.csv("data/games_Smith.csv")

#pbp
pbp<-read.csv("data/pbp_Smith.csv")

#data (combined)
data<-games %>% 
  left_join(pbp, by="GameKey")
#note: NAs for 2007-2010 in pbp

#Fulldata_with_thurs 
fulldata_with_thurs <- data %>% 
  mutate(is_thurs = ifelse(Game_Day == "Thursday", 1, 0))

#Nfl_elo_2019 (cleaned)

#Nfl_elo 

#NFL_team_city

#Stadium_data

#Stadium_data_coordinates

#Visit_team_data 
