#Use the fulldata_with_thurs ds (thursday = binary)
head(fulldata_with_thurs)

#create an excitement variable
#using fulldata_with_thurs data

excitement <- fulldata_with_thurs %>% 
  group_by(GameKey) %>% 
  summarise(excite_score = sum(abs(delta_wp_home)))
  
#look at excitement score on Thurs vs Not Thurs
boxplot(excitement[["excite_score"]] ~ excitement[[i]], data = excitement,
        xlab="Thursday/Not Thursday", ylab="excitement score",
        main = "Score Distributions on Thursdays and Non-Thursdays")
