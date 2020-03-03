#ep_rankings.R 
#compares game performance on thursday vs sunday games by expected point

#load tidyverse
library(tidyverse)
library(lubridate)

#reading in the two nfl data frames 
games <- read.csv("data/games_Smith.csv")
pbp<- read.csv("data/pbp_Smith.csv")

#reading in nfl elo data from FiveThirtyEight
nfl_elo_2019 <- read.csv("nfl_elo_latest.csv")
nfl_elo <- read.csv("nfl_elo.csv")

#create merged df 
combined_df <- games %>% 
  inner_join(pbp, by="GameKey")

#add a thursday column
fulldata_with_thurs <- combined_df %>% 
  mutate(is_thurs = ifelse(Game_Day == "Thursday", 1, 0))

#clean game dates 
combined_dates <- fulldata_with_thurs %>% 
  select(Game_Date, GameKey, is_thurs, HomeClubCode, VisitorClubCode, Home_Team, Visit_Team, PossessionTeam, ep, PlayResult, Game_Day) %>%
  mutate(Game_Date = str_replace(Game_Date, "T00:00:00Z", ""),
         Game_Date = as.Date(Game_Date, "%Y-%m-%d")) %>%
  rename(date = Game_Date)

ep1 <- combined_dates %>% 
  select(GameKey, date, HomeClubCode, VisitorClubCode, Home_Team, PossessionTeam, is_thurs, Game_Day, ep, PlayResult) %>%
  mutate(poss1 = ifelse(as.character(HomeClubCode) == as.character(PossessionTeam), 1, 0),
         ep = 
           case_when(
             poss1 == 1 ~ ep, 
             poss1 == 0 ~ 0)) %>% 
  rename(ep1 = ep)

ep2 <- combined_dates %>% 
  select(GameKey, date, HomeClubCode, VisitorClubCode, Home_Team, PossessionTeam, is_thurs, Game_Day, ep, PlayResult) %>%
  mutate(poss2 = ifelse(as.character(HomeClubCode) == as.character(PossessionTeam), 0, 1),
         ep = 
           case_when(
             poss2 == 1 ~ ep, 
             poss2 == 0 ~ 0)) %>% 
  rename(ep2 = ep)

combined_dates_ep <- ep1 %>% 
  inner_join(ep2, by="GameKey")

#change date in nfl_elo into date from factor
nfl_elo <- nfl_elo %>% 
  mutate(date = as.Date(date, "%Y-%m-%d")) %>% 
  rename(HomeClubCode = team1, 
         VisitorClubCode = team2)

#read in nfl club codes
club_codes <- read.csv("nfl_club_codes.csv")
club_codes <- club_codes %>% 
  rename(code = NFL,
         team = X)

#merge nfl_elo and combined_dates 
combined_elo_1 = merge(nfl_elo, ep1, 
            by.x=c("HomeClubCode", "VisitorClubCode", "date"), by.y=c("HomeClubCode", "VisitorClubCode", "date"))

#compare nfl_club_codes and fivethirtyeight club codes 
combined_elo1_afc <- combined_elo_1 %>% 
  filter(HomeClubCode %in% c("BUF", "MIA", "NEP", "NYJ",
                             "BAL", "CIN", "CLE", "PIT",
                             "HOU", "IND", "JAC", "TEN",
                             "DEN", "KC", "OAK"),
         VisitorClubCode %in% c("BUF", "MIA", "NEP", "NYJ",
                                "BAL", "CIN", "CLE", "PIT",
                                "HOU", "IND", "JAC", "TEN",
                                "DEN", "KC", "OAK"))

                       
ep1_elo1_afc <- combined_elo1_afc %>% 
  select(date, HomeClubCode, VisitorClubCode, Home_Team, PossessionTeam, ep1, is_thurs, Game_Day, PlayResult, elo1_pre, elo2_pre) %>% 
  filter(Game_Day == "Thursday" | Game_Day == "Sunday",
         ep1 > 0) %>%  
  group_by(HomeClubCode, is_thurs, ep1, elo1_pre, elo2_pre) %>% 
  summarise(diff_ratings = first(abs(elo1_pre - elo2_pre)),
            sum_ep_1 = sum(ep1)) %>%
  group_by(HomeClubCode, is_thurs, diff_ratings, sum_ep_1)


