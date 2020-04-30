#load tidyverse and read in correctly formatted versions of data
library(tidyverse)
library(janitor)
library(ggridges)

pbp <- read.csv("data/pbp_Smith.csv") %>% 
  clean_names()

data <- games %>% 
  inner_join(pbp, by="game_key")

data_thurs <- games %>% 
  inner_join(pbp, by="game_key") %>% 
  filter(game_day == "Thursday")


#### Using Tom's code 

epa_df <- data %>% 
  mutate(quarter = as.numeric(quarter)) %>% 
  mutate(half = ifelse(quarter < 3, 1, ifelse(quarter == 5, 3, 2))) %>%
  mutate(is_thurs = ifelse(game_day == "Thursday", 1, 0)) %>% 
  mutate(is_thurs = as.factor(is_thurs)) %>% 
  #grouping by game and half because EPA does not carry over halves or games
  group_by(game_key, half) %>%
  #make sure plays are in correct order
  arrange(game_key, play_id) %>%
  #calculating EPA - ADDED: if possession changes EPA must be calculated differently
  mutate(
    #actual points added
    points_added = ifelse(home_club_code == possession_team,
                         #if home team has the ball its the home points gained - visitor points gained
                         home_score_after_play - home_score_before_play - (visitor_score_after_play - visitor_score_before_play),
                         #otherwise its the visitor points gained - home points gained
                         visitor_score_after_play - visitor_score_before_play - (home_score_after_play - home_score_before_play)),
    #adding extra point to TDs - leaving rest of points added alone
    points_added = ifelse(points_added == 6, 7, points_added),
    points_added = ifelse(points_added == -6, -7, points_added),
    #calculating EPA - ADDED: if possession changes EPA must be calculated differently, removing Extra Point / kickoff plays
    epa = 
      case_when(#when play is a scrimmage play, no scoring and no change of possession
        down != 0 & points_added == 0 & possession_team == lead(possession_team) ~ lead(ep) - ep,
        #when a play is a scrimmage play, no scoring and a change of possession
        down != 0 & points_added == 0 & possession_team != lead(possession_team) ~ -lead(ep) - ep,
        #when a play is a scrimmage play with scoring
        down != 0 & points_added != 0 ~ points_added - ep)
  ) %>%
  #ungrouping dataframe
  ungroup() %>% 
  select(epa, is_thurs)

#view epa distribution
ggplot(data = epa_df, aes(x = epa)) +
  geom_density(kernel = "rectangular") +
  ggtitle("Distribution of EPA")

#plot epa on thurs vs sunday - density
### USE THIS ONE
ggplot(data = epa_df, aes(x = epa)) +
  geom_density(kernal = "rectangular", aes(color = is_thurs), size = 1.5) +
  ggtitle("Distribution of EPA", subtitle = "Comparing Thursday to Non Thursday Games") +
  coord_cartesian(xlim = c(-8, 8), ylim = c(0, 0.55)) +
  theme_minimal()

#plot epa on thurs vs sunday - histogram
ggplot(data = epa_df, aes(x = epa)) +
  geom_histogram(aes(color = is_thurs)) +
  ggtitle("Distribution of EPA by gameday", subtitle = "Histogram")

#plot epa on thurs vs sunday - boxplots
ggplot(data = epa_df, aes(y = epa, x = is_thurs)) +
  geom_boxplot(aes(color = is_thurs)) +
  ggtitle("comparing thursday and sunday games") +
  theme(legend.position = "none")

###attempt permutation test

thurs <- epa_df %>% 
  filter(is_thurs == 1) %>% 
  select(is_thurs, epa)
dim(thurs)
sun <- epa_df %>% 
  filter(is_thurs == 0) %>% 
  select(is_thurs, epa)
dim(sun)

#cacluate original test stat - difference in mean EPA
mean_epa_thurs <- mean(thurs$epa, na.rm = T)
mean_epa_sun <- mean(sun$epa, na.rm = T)
(original_ts <- mean_epa_sun - mean_epa_thurs)

#regroup data
thurs_epa <- thurs %>% select(epa)
sun_epa <- sun %>% select(epa)
all_days <- rbind(thurs_epa, sun_epa)
all_days <- as.data.frame(all_days)

#permutations
distribution = NULL
set.seed(76)
for (i in 1:1000){
  temp = sample_frac(all_days, size = 1, replace = F)
  tempR = temp[1:26055,]
  tempS = temp[26056:389014,]
  
  mean_R <- mean(tempR, na.rm = TRUE)
  mean_S <- mean(tempS, na.rm = TRUE)
  
  distribution[i] = mean_S - mean_R
  
}

sum(distribution <= original_ts)/length(distribution)
#note: p = 0.935, which is not less than alpha = 0.1
#or 0.065 depending on how you look at it

### Plots of permutation test results
###USE THIS ONE
mean_differences <- as.data.frame(distribution) 
mean_val <- mean(distribution)
original_ts_df = data.frame(value = original_ts, Line = "Observed Difference in EPA")
ggplot(data = mean_differences, aes(x = distribution)) + 
  geom_histogram(color = "blue", fill = "lightblue", bins = 15) +
  geom_vline(xintercept = original_ts, linetype = 2, color = "red") +
  ggtitle("Results of Permutation test", subtitle = "Mean EPA on Non Thursday Games — Mean EPA on Thursday Games") +
  xlab("Non Thursday ELA — Thursday EPA") +
  theme_minimal()
  

### BootStrap test
bootstrap_epa <- epa_df %>% 
  select(is_thurs, epa) %>% 
  group_by(is_thurs) %>% 
  summarize(epa = mean(epa, na.rm = TRUE))
#initialize vector
results = c()
#compute actual observed difference
observed_difference <- bootstrap_epa$epa[1] - bootstrap_epa$epa[2]

#set number of iterations and seed
num_iter = 500
set.seed(23)

#bootstrap
for(i in seq(1, num_iter)){
  
  #making a new df_total that will be randomly sample for the bootstrap
  df_total_temp <- epa_df %>% mutate(is_thurs = sample(is_thurs, size = nrow(epa_df), replace = T) )
  
  #splitting into thursday and not thursday and them summarizing results
  df_bootstrap <- df_total_temp %>% 
    group_by(is_thurs) %>% summarize(epa = mean(epa, na.rm = T))
  
  #appending data
  results = append(results, df_bootstrap$epa[1] - df_bootstrap$epa[2])
  
}

observed = data.frame(value = observed_difference, Line = "Observed Difference")
observed_difference

#plot
###USE THIS ONE
bootstrap_plot <- ggplot(as.data.frame(results), aes(results)) +
  #making histogram
  geom_histogram(colour = "blue", fill = "lightblue", bins = 10) +
  #adding line to represent observed data
  geom_vline(xintercept = observed_difference, linetype = 2, color = "red") +
  #setting xlabel
  xlab("Non-Thursday EPA  — Thursday EPA") + 
  #setting title
  ggtitle("Histogram: Bootstrap Test of EPA Difference") +
  theme_minimal()
bootstrap_plot

#plot by team
epa_team_df <-  data %>% 
  filter(home_team == "Buffalo Bills" | home_team == "Miami Dolphins" |
           home_team == "New England Patriots" | home_team == "New York Jets" | home_team == "Kansas City Cheifs"
         | home_team == "Denver Broncos" | home_team == "Oakland Raiders") %>% 
  mutate(quarter = as.numeric(quarter)) %>% 
  mutate(half = ifelse(quarter < 3, 1, ifelse(quarter == 5, 3, 2))) %>%
  mutate(is_thurs = ifelse(game_day == "Thursday", 1, 0)) %>% 
  mutate(is_thurs = as.factor(is_thurs)) %>% 
  #grouping by game and half because EPA does not carry over halves or games
  group_by(game_key, half) %>%
  #make sure plays are in correct order
  arrange(game_key, play_id) %>%
  #calculating EPA - ADDED: if possession changes EPA must be calculated differently
  mutate(
    #actual points added
    points_added = ifelse(home_club_code == possession_team,
                          #if home team has the ball its the home points gained - visitor points gained
                          home_score_after_play - home_score_before_play - (visitor_score_after_play - visitor_score_before_play),
                          #otherwise its the visitor points gained - home points gained
                          visitor_score_after_play - visitor_score_before_play - (home_score_after_play - home_score_before_play)),
    #adding extra point to TDs - leaving rest of points added alone
    points_added = ifelse(points_added == 6, 7, points_added),
    points_added = ifelse(points_added == -6, -7, points_added),
    #calculating EPA - ADDED: if possession changes EPA must be calculated differently, removing Extra Point / kickoff plays
    epa = 
      case_when(#when play is a scrimmage play, no scoring and no change of possession
        down != 0 & points_added == 0 & possession_team == lead(possession_team) ~ lead(ep) - ep,
        #when a play is a scrimmage play, no scoring and a change of possession
        down != 0 & points_added == 0 & possession_team != lead(possession_team) ~ -lead(ep) - ep,
        #when a play is a scrimmage play with scoring
        down != 0 & points_added != 0 ~ points_added - ep)
  ) %>%
  #ungrouping dataframe
  ungroup() %>% 
  select(epa, is_thurs, home_team) %>% 
  group_by(home_team, is_thurs) %>% 
  summarise(mean_epa = mean(epa, na.rm = T))

#plot 
###USE THIS ONE
team_plot <- ggplot(data = epa_team_df, aes(x = home_team, y = mean_epa, fill = is_thurs)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle=90)) +
  ggtitle(label = "Mean EPA in the AFC East & West", subtitle = " Comparing  Thursday  vs. Non Thursday games") +
  labs(fill = "Thursday Status", y = "Mean EPA", x = "Home Team")
team_plot

#### Old Code - use this for visualization ideas

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
  group_by(home_team, is_thurs) %>% 
  summarise(mean_ep = mean(sum_ep, rm.na = T))

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

