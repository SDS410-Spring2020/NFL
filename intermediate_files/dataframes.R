library(tidyverse)
library(janitor)
library(lubridate)
#games
games <-read.csv("data/games_Smith.csv") %>% 
  clean_names() %>%
  mutate(game_date = str_replace(game_date, "T00:00:00Z", ""),
         game_date = as.Date(game_date, "%Y-%m-%d")) 
#pbp
pbp<-read.csv("data/pbp_Smith.csv") %>% 
  clean_names() %>% 
  mutate(
    game_clock = as.numeric(as.period(ms(game_clock), unit = "sec")),
    game_clock = 
      case_when(quarter == 1 ~ game_clock,
                quarter == 2 ~ game_clock + 900,
                quarter == 3 ~ game_clock + 1800,
                quarter == 4 ~ game_clock + 2700,
                quarter == 5 ~ game_clock + 3600))


#data (combined)
data <- games %>% 
  left_join(pbp, by="game_key")
#note: NAs for 2007-2010 in pbp

#fulldata_with_thurs 
fulldata_with_thurs <- data %>% 
  mutate(is_thurs = ifelse(game_day == "Thursday", 1, 0)) %>% 
  filter(game_day == "Thursday" | game_day == "Sunday") %>% 
  mutate(is_thurs = as.factor(is_thurs),
         stadium = as.character(stadium),
         home_team = as.character(home_team),
         visit_team = as.character(visit_team),
         location = as.character(location),
         game_key = as.factor(game_key), 
         season = as.factor(season),
         week = as.factor(week),
         game_weather = as.character(game_weather),
         wind_speed = as.character(wind_speed),
         wind_direction = as.character(wind_direction),
         quarter = as.factor(quarter),
         play_description = as.character(play_description),
         down = as.factor(down),
         is_scoring_play = as.factor(is_scoring_play))

#nfl_elo_2019 (cleaned)
nfl_elo_2019 <- read.csv("nfl_elo_latest.csv") %>% 
  clean_names() %>% 
  rename(
    home_club_code = team1, 
    visitor_club_code = team2)
  mutate(
    game_date = as.Date(date, "%Y-%m-%d"),
    date = NULL, 
    home_club_code = 
      case_when(
        as.character(home_club_code) == "HOU" ~ "HST",
        as.character(home_club_code) == "LAR" ~ "SL",
        as.character(home_club_code) == "WSH" ~ "WAS", 
        as.character(home_club_code) == "CLE" ~ "CLV",
        as.character(home_club_code) == "BAL" ~ "BLT",
        as.character(home_club_code) == "ARI" ~ "ARZ",
        as.character(home_club_code) == "LAC" & as.character(season) >= "2017" ~ "SD",
        as.character(home_club_code) == "LAC" & as.character(season) <= "2016" ~ "LA",
        TRUE ~ as.character(home_club_code)), 
    visitor_club_code = 
      case_when(
        as.character(visitor_club_code) == "HOU" ~ "HST",
        as.character(visitor_club_code) == "LAR" ~ "SL",
        as.character(visitor_club_code) == "WSH" ~ "WAS", 
        as.character(visitor_club_code) == "CLE" ~ "CLV",
        as.character(visitor_club_code) == "BAL" ~ "BLT",
        as.character(visitor_club_code) == "ARI" ~ "ARZ",
        as.character(visitor_club_code) == "LAC" & as.character(season) >= "2017" ~ "SD",
        as.character(visitor_club_code) == "LAC" & as.character(season) <= "2016" ~ "LA",
        TRUE ~ as.character(visitor_club_code)))

#nfl_elo (cleaned)
nfl_elo <- read.csv("nfl_elo.csv") %>% 
  clean_names() %>% 
  rename(
    home_club_code = team1, 
    visitor_club_code = team2) %>% 
  mutate(
    game_date = as.Date(date, "%Y-%m-%d"), 
    date = NULL, 
    home_club_code = 
      case_when(
        as.character(home_club_code) == "HOU" ~ "HST",
        as.character(home_club_code) == "LAR" ~ "SL",
        as.character(home_club_code) == "WSH" ~ "WAS", 
        as.character(home_club_code) == "CLE" ~ "CLV",
        as.character(home_club_code) == "BAL" ~ "BLT",
        as.character(home_club_code) == "ARI" ~ "ARZ",
        as.character(home_club_code) == "LAC" & as.character(season) >= "2017" ~ "SD",
        as.character(home_club_code) == "LAC" & as.character(season) <= "2016" ~ "LA",
        TRUE ~ as.character(home_club_code)), 
    visitor_club_code = 
      case_when(
        as.character(visitor_club_code) == "HOU" ~ "HST",
        as.character(visitor_club_code) == "LAR" ~ "SL",
        as.character(visitor_club_code) == "WSH" ~ "WAS", 
        as.character(visitor_club_code) == "CLE" ~ "CLV",
        as.character(visitor_club_code) == "BAL" ~ "BLT",
        as.character(visitor_club_code) == "ARI" ~ "ARZ",
        as.character(visitor_club_code) == "LAC" & as.character(season) >= "2017" ~ "SD",
        as.character(visitor_club_code) == "LAC" & as.character(season) <= "2016" ~ "LA",
        TRUE ~ as.character(visitor_club_code)))


for_elo <- data %>% 
  select(game_key,
         home_team,
         visit_team, 
         season,
         season_type,
         game_number,
         week,
         game_date,
         game_day,
         home_club_code,
         visitor_club_code,
         possession_team,
         ep) %>% 
  distinct(game_key, .keep_all = TRUE)

#full_with_elo 
full_with_elo= merge(nfl_elo, 
                     for_elo, 
                     by.x= c("home_club_code", "visitor_club_code", "game_date", "season"), 
                     by.y= c("home_club_code", "visitor_club_code", "game_date", "season"))

full_with_elo <- full_with_elo %>% 
  mutate(
    season = season.x,
    season.y = NULL,
    season.x = NULL, 
    is_home = ifelse(as.character(home_club_code) == as.character(possession_team), 1, 0),
    is_visitor =
      case_when(is_home == 1 ~ 0,
                is_home == 0 ~ 1),
    ep1 = 
      case_when(
        is_home == 1 ~ ep, 
        is_home == 0 ~ 0),
    ep2 = 
      case_when(
        is_visitor == 1 ~ ep,
        is_visitor == 0 ~ 0))
#NFL_team_city

#Stadium_data

#Stadium_data_coordinates
stadium_data_coordinates <- read.csv("stadium_data_coordinates.csv") %>%
  clean_names()

#Visit_team_data 
visit_team_data <- read.csv("visit_team_data.csv") %>%
  clean_names() 

