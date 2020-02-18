#Use the games ds (thursday = binary)
head(games)

#reading in the two nfl data frames 
games<- read.csv("data/games_Smith.csv")
pbp<- read.csv("data/pbp_Smith.csv")

#creating binary thursday in games df
thurs_games <- fulldata_with_thurs %>% 
  mutate(is_thurs = ifelse(Game_Day == "Thursday", 1, 0))

#create an excitement variable
#using thurs_games data
excitement <- fulldata_with_thurs %>% 
  group_by(GameKey) %>% 
  summarise(excite_score = sum(abs(delta_wp_home)))
  
#join excitement score with thurs_games ds 
excitement_joined <- excitement %>% 
  full_join(thurs_games, by = "GameKey")

thurs_excite_box <- ggplot(data = excitement_joined, aes(y = rowSums, group = as.factor(have_arthritis), color = as.factor(have_arthritis))) +
  geom_boxplot() +
  ggtitle("Side by Side Box plots using rowSums ~ Gender")

#look at excitement score on Thurs vs Not Thurs
boxplot(excitement_joined[["excite_score"]] ~ excitement_joined[["is_thurs"]], data = excitement_joined,
        xlab="Thursday/Not Thursday", ylab="excitement score",
        main = "Score Distributions on Thursdays and Non-Thursdays")
