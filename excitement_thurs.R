#Use the games ds (thursday = binary)
head(games)


#creating binary thursday in games df
thurs_games <- games %>% 
  mutate(is_thurs = ifelse(Game_Day == "Thursday", 1, 0))

#create an excitement variable
#using thurs_games data
excitement <- thurs_games %>% 
  group_by(GameKey) %>% 
  summarise(excite_score = sum(abs(delta_wp_home)))
  
#join excitement score with thurs_games ds 
excitement_joined <- excitement %>% 
  full_join(thurs_games, by = "GameKey")

#look at excitement score on Thurs vs Not Thurs
boxplot(excitement_joined[["excite_score"]] ~ excitement_joined[["is_thurs"]], data = excitement_joined,
        xlab="Thursday/Not Thursday", ylab="excitement score",
        main = "Score Distributions on Thursdays and Non-Thursdays")
