#load tidyverse
library(tidyverse)

#reading in the two nfl data frames 
games <- read.csv("data/games_Smith.csv")
pbp<- read.csv("data/pbp_Smith.csv")

#create merged df 
combined_df <- games %>% 
  inner_join(pbp, by="GameKey")

#add a thursday column
fulldata_with_thurs <- combined_df %>% 
  mutate(is_thurs = ifelse(Game_Day == "Thursday", 1, 0))

#wrangle 
ep_df_2 <- fulldata_with_thurs %>% 
  select(GameKey, is_thurs, Home_Team, ep, PlayResult, Game_Day) %>% 
  filter(Game_Day == "Thursday" | Game_Day == "Sunday") %>% 
  filter(ep > 0) %>%  
  filter(Home_Team == "Buffalo Bills" | Home_Team == "Miami Dolphins" |
           Home_Team == "New England Patriots" | Home_Team == "New York Jets" |
           Home_Team == "Baltimore Ravens" | Home_Team == "Cincinnati Bengals"
         | Home_Team == "Cleveland Browns" | Home_Team == "Pittsburgh Steelers" 
         | Home_Team == "Houston Texans" | Home_Team == "Indianapolis Colts" | 
           Home_Team == "Jacksonville Jaguars" | Home_Team == "Tennessee Titans"
         | Home_Team == "Denver Broncos" | Home_Team == "Kansas City Chiefs" |
           Home_Team == "Oakland Raiders") %>% 
  group_by(GameKey, is_thurs, Home_Team) %>% 
  summarise(sum_ep = sum(ep)) %>% 
  group_by(Home_Team, is_thurs) %>% 
  summarise(mean_ep = mean(sum_ep))

#plot 
team_allseason_plot <- ggplot(data = ep_df_2, aes(x = Home_Team, y = mean_ep, fill = as.factor(is_thurs))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle=90)) +
  ggtitle(label = "Comparing mean expected points", subtitle = "Offensive Production on Thursday  vs. Sunday") +
  labs(fill = "Is_Thurs")
team_allseason_plot

#facet wrap by Season 
ep_df_3 <- fulldata_with_thurs %>% 
  select(GameKey, is_thurs, Home_Team, ep, PlayResult, Game_Day, Season) %>% 
  filter(Game_Day == "Thursday" | Game_Day == "Sunday") %>% 
  filter(ep > 0) %>%  
  filter(Home_Team == "Buffalo Bills" | Home_Team == "Miami Dolphins" |
           Home_Team == "New England Patriots" | Home_Team == "New York Jets" |
           Home_Team == "Baltimore Ravens" | Home_Team == "Cincinnati Bengals"
         | Home_Team == "Cleveland Browns" | Home_Team == "Pittsburgh Steelers" 
         | Home_Team == "Houston Texans" | Home_Team == "Indianapolis Colts" | 
           Home_Team == "Jacksonville Jaguars" | Home_Team == "Tennessee Titans"
         | Home_Team == "Denver Broncos" | Home_Team == "Kansas City Chiefs" |
           Home_Team == "Oakland Raiders") %>% 
  group_by(GameKey, is_thurs, Home_Team, Season) %>% 
  summarise(sum_ep = sum(ep)) %>% 
  group_by(Home_Team, is_thurs, Season) %>% 
  summarise(mean_ep = mean(sum_ep))

#plot
team_facet_plot <- ggplot(data = ep_df_3, aes(x = Home_Team, y = mean_ep, fill = as.factor(is_thurs))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(facets = "Season", nrow = 3) +
  theme(axis.text.x = element_text(angle=90)) +
  ggtitle(label = "Comparing mean expected points", subtitle = "Offensive Production on Thursday  vs. Sunday") +
  labs(fill = "Is_Thurs")
team_facet_plot

