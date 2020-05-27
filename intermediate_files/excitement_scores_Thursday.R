### THIS IS TOMS CODE

#importing libararies
library(tidyverse)


### READING IN AND MERGING DATA
#reading in data
df_games <- read_csv('data/games_Smith.csv')
df_plays <- read_csv('data/pbp_Smith.csv')

#calculating excitement
df_excitement <- df_plays %>% 
  #grouping by game
  group_by(GameKey) %>%
  #summarizing results
  summarize(excite_score = sum(abs(delta_wp_home)))

#merging excitement to games
df_total <- inner_join(df_games, df_excitement, by = c("GameKey" = "GameKey"))








### EVERY DAY EXCITMENT
df_plot <- df_total %>% select(excite_score, Game_Day) %>%
  
  #removing games that had to be pushed to a non-typical gameday due to weather (i.e. Tuesday game Minnesota @ Philadelphia 2010)
  filter(Game_Day %in%  c("Saturday", "Monday", "Thursday", "Sunday")) %>%
  
  #grouping by game
  group_by(Game_Day) %>%
  
  #summarizing results
  summarize(Excitement = mean(excite_score))

#plotting
plot1 <- ggplot(df_plot, aes(reorder(Game_Day, -Excitement), Excitement)) +
  
  #making bar graph
  geom_bar(stat = "identity",position = "dodge", color = 'blue', fill = 'lightblue') +
  
  #black white theme to look better
  theme_bw() +
 
  #adding title
  ggtitle("Excitement Scores by Game Day") +
  
  #setting xlabel
  xlab("Game Day")

#saving the plot to a png file
ggsave(plot1, file =  "ExcitementByDay.png", width = 7, height = 7)







### THURSDAY VS OTHER DAY PLOT
df_plot2 <- df_total %>% select(excite_score, Game_Day) %>%
  
  #removing games that had to be pushed to a non-typical gameday due to unusual circumstances (i.e. Tuesday game Minnesota @ Philadelphia 2010)
  filter(Game_Day %in%  c("Saturday", "Monday", "Thursday", "Sunday")) %>%
  
  #Splitting Game_Day into two categories
  mutate(Game_Day = ifelse(Game_Day == "Thursday", "Thursday", "Not Thursday")) %>%
  
  #creating variable to determine whether
  group_by(Game_Day) %>%
  summarize(Excitement = mean(excite_score))

#plotting
plot2 <- ggplot(df_plot2, aes(reorder(Game_Day, -Excitement), Excitement)) +
  
  #making bar graph
  geom_bar(stat = "identity", position = "dodge", color = 'blue', fill = 'lightblue') +
  
  #adding title
  ggtitle("Excitement Scores - Thursday vs. Other Gamedays") + 
  
  #setting x label
  xlab("Game Day") +
  
  #black and white theme to look better
  theme_bw()

#saving the plot to a png file
ggsave(plot2, file =  "Excitement_ThursdayVsOtherDays.png", width = 7, height = 7)






### BOOTSTRAP TEST
df_bootstrap <- df_plot2

#will be used to store results of bootstrap
results = c()

#determining actual excitement difference (Not Thursday Excitement - Thursday Excitement)
observed_difference <- df_bootstrap$Excitement[1] - df_bootstrap$Excitement[2]

#number of bootstrap iterations and setting seed
num_iter = 1000
set.seed(15)

#iterating through boot straps
for(i in seq(1, num_iter)){
  
  #making a new df_total that will be randomly sample for the bootstrap
  df_total_temp <- df_total %>% mutate(Game_Day = sample(Game_Day, size = nrow(df_total)) )
  
  #splitting into thursday and not thursday and them summarizing results
  df_bootstrap <- df_total_temp %>% mutate(Game_Day = ifelse(Game_Day == "Thursday", "Thursday", "Not Thursday")) %>%
    group_by(Game_Day) %>% summarize(Excitement = mean(excite_score))
  
  #appending data
  results = append(results, df_bootstrap$Excitement[1] - df_bootstrap$Excitement[2])
  
}

#used to plot vertical line of the observed difference on the histogram
observed = data.frame(value = observed_difference, Line = "Observed Difference of Excitement Scores")

#plotting
plot3 <- ggplot(as.data.frame(results), aes(results)) +
  
  #making histogram
  geom_histogram(colour = "blue", fill = "lightblue", bins = 10) +
  
  #adding line to represent observed data
  geom_vline(data = observed, aes(xintercept = value, color = Line)) +
  
  #setting xlabel
  xlab("Non-Thursday Excitement Score - Thursday Excitement Score") + 
  
  #setting title
  ggtitle("Histogram: Bootstrap Test of Excitment Score Difference") +
  
  #black and white theme to look better
  theme_bw()

#saving the plot to a png file
ggsave(plot3, file = "Excitment_ThursdayBootStrap.png", width = 10, height = 7)
