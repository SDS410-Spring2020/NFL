#games
games<-read.csv("data/games_Smith.csv")

#pbp
pbp<-read.csv("data/pbp_Smith.csv")

#data (combined)
data<-games %>% 
  left_join(pbp, by="GameKey")
#note: NAs for 2007-2010 in pbp

#fulldata_with_thurs 
fulldata_with_thurs <- data %>% 
  mutate(is_thurs = ifelse(Game_Day == "Thursday", 1, 0))

#nfl_elo_2019 (cleaned)
nfl_elo_2019 <- read.csv("nfl_elo_latest.csv")
nfl_elo_2019 <- nfl_elo_2019 %>% 
  rename(HomeClubCode = team1, 
         VisitorClubCode = team2)
  mutate(
    date = as.Date(date, "%Y-%m-%d"), 
    HomeClubCode = 
      case_when(
        as.character(HomeClubCode) == "HOU" ~ "HST",
        as.character(HomeClubCode) == "LAR" ~ "SL",
        as.character(HomeClubCode) == "WSH" ~ "WAS", 
        as.character(HomeClubCode) == "CLE" ~ "CLV",
        as.character(HomeClubCode) == "BAL" ~ "BLT",
        as.character(HomeClubCode) == "ARI" ~ "ARZ",
        as.character(HomeClubCode) == "LAC" & as.character(season) >= "2017" ~ "SD",
        as.character(HomeClubCode) == "LAC" & as.character(season) <= "2016" ~ "LA",
        TRUE ~ as.character(HomeClubCode)), 
    VisitorClubCode = 
      case_when(
        as.character(VisitorClubCode) == "HOU" ~ "HST",
        as.character(VisitorClubCode) == "LAR" ~ "SL",
        as.character(VisitorClubCode) == "WSH" ~ "WAS", 
        as.character(VisitorClubCode) == "CLE" ~ "CLV",
        as.character(VisitorClubCode) == "BAL" ~ "BLT",
        as.character(VisitorClubCode) == "ARI" ~ "ARZ",
        as.character(VisitorClubCode) == "LAC" & as.character(season) >= "2017" ~ "SD",
        as.character(VisitorClubCode) == "LAC" & as.character(season) <= "2016" ~ "LA",
        TRUE ~ as.character(VisitorClubCode)))

#nfl_elo (cleaned)
nfl_elo <- read.csv("nfl_elo.csv")
nfl_elo_ <- nfl_elo %>% 
  rename(HomeClubCode = team1, 
         VisitorClubCode = team2) %>% 
  mutate(
    date = as.Date(date, "%Y-%m-%d"), 
    HomeClubCode = 
      case_when(
        as.character(HomeClubCode) == "HOU" ~ "HST",
        as.character(HomeClubCode) == "LAR" ~ "SL",
        as.character(HomeClubCode) == "WSH" ~ "WAS", 
        as.character(HomeClubCode) == "CLE" ~ "CLV",
        as.character(HomeClubCode) == "BAL" ~ "BLT",
        as.character(HomeClubCode) == "ARI" ~ "ARZ",
        as.character(HomeClubCode) == "LAC" & as.character(season) >= "2017" ~ "SD",
        as.character(HomeClubCode) == "LAC" & as.character(season) <= "2016" ~ "LA",
        TRUE ~ as.character(HomeClubCode)), 
    VisitorClubCode = 
      case_when(
        as.character(VisitorClubCode) == "HOU" ~ "HST",
        as.character(VisitorClubCode) == "LAR" ~ "SL",
        as.character(VisitorClubCode) == "WSH" ~ "WAS", 
        as.character(VisitorClubCode) == "CLE" ~ "CLV",
        as.character(VisitorClubCode) == "BAL" ~ "BLT",
        as.character(VisitorClubCode) == "ARI" ~ "ARZ",
        as.character(VisitorClubCode) == "LAC" & as.character(season) >= "2017" ~ "SD",
        as.character(VisitorClubCode) == "LAC" & as.character(season) <= "2016" ~ "LA",
        TRUE ~ as.character(VisitorClubCode)))

#NFL_team_city

#Stadium_data

#Stadium_data_coordinates

#Visit_team_data 
