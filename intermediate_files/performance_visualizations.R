library(dplyr)
library(ggplot2)

data_2013 <- filter(data_with_scores, 
                    season == 2013)

ggplot(data_2013, 
       aes(y = winning_team, x = season, color = game_day)) + 
  geom_boxplot() 
