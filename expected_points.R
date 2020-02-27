#load tidyverse
library(tidyverse)

#reading in the two nfl data frames 
games<- read.csv("data/games_Smith.csv")
pbp<- read.csv("data/pbp_Smith.csv")

#create merged df 
combined_df <- games %>% 
  inner_join(pbp, by="GameKey")

#add a thursday column
fulldata_with_thurs <- data %>% 
  mutate(is_thurs = ifelse(Game_Day == "Thursday", 1, 0))

#compare ep on thurs vs non thurs
#note: higher expected points values on non_thurs
ep_df <- fulldata_with_thurs %>% 
  select(GameKey, is_thurs, Home_Team, Visit_Team, ep, PlayResult) %>% 
  group_by(is_thurs) %>% 
  summarise(sum(ep))

#plot AFC's ep on thurs vs non thurs 
team_ep <- fulldata_with_thurs %>% 
  select(is_thurs, Home_Team, ep) %>% 
  filter(Home_Team == "Buffalo Bills" | Home_Team == "Miami Dolphins" |
           Home_Team == "New England Patriots" | Home_Team == "New York Jets" |
           Home_Team == "Baltimore Ravens" | Home_Team == "Cincinnati Bengals"
         | Home_Team == "Cleveland Browns" | Home_Team == "Pittsburgh Steelers" 
         | Home_Team == "Houston Texans" | Home_Team == "Indianapolis Colts" | 
           Home_Team == "Jacksonville Jaguars" | Home_Team == "Tennessee Titans"
         | Home_Team == "Denver Broncos" | Home_Team == "Kansas City Chiefs" |
           Home_Team == "Oakland Raiders") %>% 
  group_by(Home_Team, is_thurs) %>% 
  summarise(sum_ep = sum(ep))

team_plot <- ggplot(data = team_ep, aes(x = Home_Team, y = sum_ep, fill= as.factor(is_thurs))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle=90))
team_plot
