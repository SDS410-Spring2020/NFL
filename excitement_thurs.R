#Use the fulldata_with_thurs ds (thursday = binary)
head(fulldata_with_thurs)

#reading in the two nfl data frames 
games<- read.csv("data/games_Smith.csv")
pbp<- read.csv("data/pbp_Smith.csv")

#might want to look at this in terms of game excitement
fulldata_with_thurs <- data %>% 
  mutate(is_thurs = ifelse(Game_Day == "Thursday", 1, 0))

#create an excitement variable
#using thurs_games data
excitement <- fulldata_with_thurs %>% 
  group_by(GameKey) %>% 
  summarise(excite_score = sum(abs(delta_wp_home)))
  
#join excitement score with thurs_games ds 
excitement_joined <- excitement %>% 
  full_join(fulldata_with_thurs, by = "GameKey")

#look at exctiment score on Thurs vs Not Thurs using ggplot
thurs_excite_box <- ggplot(data = excitement_joined, aes(y = excite_score, group = as.factor(is_thurs), color = as.factor(is_thurs))) +
  geom_boxplot() +
  ggtitle("Comparing excitement on Thurs. vs. not Thurs games")
thurs_excite_box

#look at excitement score on Thurs vs Not Thurs
boxplot(excitement_joined[["excite_score"]] ~ excitement_joined[["is_thurs"]], data = excitement_joined,
        xlab="Thursday/Not Thursday", ylab="excitement score",
        main = "Score Distributions on Thursdays and Non-Thursdays")
