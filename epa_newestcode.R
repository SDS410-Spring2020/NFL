library(tidyverse)

#reading data
df_plays <- read_csv('pbp_Smith.csv')

#calculating EPA
df_plays <- df_plays %>%
  #creating half variable as EPA does not carry over a half of football
  mutate(Half = ifelse(Quarter < 3, 1, ifelse(Quarter == 5, 3, 2))) %>%
  
  #grouping by game and half because EPA does not carry over halves or games
  group_by(GameKey, Half) %>%
  
  #make sure plays are in correct order
  arrange(GameKey, PlayID) %>%
  
  
  mutate(
          #actual points added
          pointsAdded = ifelse(HomeClubCode == PossessionTeam,
                               
                               #if home team has the ball its the home points gained - visitor points gained
                               HomeScoreAfterPlay - HomeScoreBeforePlay - (VisitorScoreAfterPlay - VisitorScoreBeforePlay),
                               
                               #otherwise its the visitor points gained - home points gained
                               VisitorScoreAfterPlay - VisitorScoreBeforePlay - (HomeScoreAfterPlay - HomeScoreBeforePlay)),
          
          #adding extra point to TDs - leaving rest of points added alone
          pointsAdded = ifelse(pointsAdded == 6, 7, pointsAdded),
          pointsAdded = ifelse(pointsAdded == -6, -7, pointsAdded),
          
          
          #calculating EPA - ADDED: if possession changes EPA must be calculated differently, removing Extra Point / kickoff plays
          epa = 
           case_when(#when play is a scrimmage play, no scoring and no change of possession
                     Down != 0 & pointsAdded == 0 & PossessionTeam == lead(PossessionTeam) ~ lead(ep) - ep,
                     
                     #when a play is a scrimmage play, no scoring and a change of possession
                     Down != 0 & pointsAdded == 0 & PossessionTeam != lead(PossessionTeam) ~ -lead(ep) - ep,
                     
                     #when a play is a scrimmage play with scoring
                     Down != 0 & pointsAdded != 0 ~ pointsAdded - ep)
  ) %>%
  
  #ungrouping dataframe
  ungroup()
