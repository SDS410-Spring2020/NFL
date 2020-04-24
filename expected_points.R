#load tidyverse
library(tidyverse)

pbp <- read.csv("data/pbp_Smith.csv") %>% 
  clean_names()

data <- games %>% 
  inner_join(pbp, by="game_key") %>% 
  filter(game_day == "Thursday"| game_day == "Sunday")

#Using Tom's code
epa_df <- data %>% 
  mutate(quarter = as.numeric(quarter)) %>% 
  mutate(half = ifelse(quarter < 3, 1, ifelse(quarter == 5, 3, 2))) %>%
  #grouping by game and half because EPA does not carry over halves or games
  group_by(game_key, half) %>%
  #make sure plays are in correct order
  arrange(game_key, play_id) %>%
  #calculating EPA - ADDED: if possession changes EPA must be calculated differently
  mutate(epa = 
           ifelse(possession_team == lead(possession_team),
                  lead(ep) - ep,
                  -lead(ep) - ep)
  ) %>%
  ungroup()

#view epa distribution
ggplot(data = epa_df, aes(x = epa)) +
  geom_density(kernel = "rectangular") +
  ggtitle("Distribution of EPA")

#plot epa on thurs vs sunday - density
ggplot(data = epa_df, aes(x = epa)) +
  geom_density(kernel = "rectangular", aes(color = game_day)) +
  ggtitle("Distribution of EPA by gameday") 

#plot epa on thurs vs sunday - boxplots
ggplot(data = epa_df, aes(y = epa)) +
  geom_boxplot(aes(color = game_day)) +
  ggtitle("comparing thursday and sunday games")

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

