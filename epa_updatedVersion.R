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
  
  #calculating EPA - ADDED: if possession changes EPA must be calculated differently
  mutate(epa = 
           ifelse(PossessionTeam == lead(PossessionTeam),
                  lead(ep) - ep,
                  -lead(ep) - ep)
  ) %>%
  
  #ungrouping dataframe
  ungroup()
