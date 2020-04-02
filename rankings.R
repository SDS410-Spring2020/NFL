#ep_rankings.R 
#compares game performance on thursday vs sunday games by expected point

#load tidyverse
library(tidyverse)
library(lubridate)

#reading in the two nfl data frames 
#games <- read.csv("data/games_Smith.csv")
#pbp<- read.csv("data/pbp_Smith.csv")

#reading in nfl elo data from FiveThirtyEight
#nfl_elo_2019 <- read.csv("nfl_elo_latest.csv")
#nfl_elo <- read.csv("nfl_elo.csv")

#create merged df 
#combined_df <- games %>% 
#  inner_join(pbp, by="GameKey")

#Data frames used in this script: 
# - data
# - nfl_elo
# - nfl_elo_2019 
# - full_data_wtih_thurs

#add a thursday column
fulldata_with_thurs <- combined_df %>% 
  mutate(is_thurs = ifelse(Game_Day == "Thursday", 1, 0))

#clean dates and gameclock 
combined_all <- fulldata_with_thurs %>% 
  select(Game_Date, Season, GameKey, is_thurs, HomeClubCode, VisitorClubCode, Home_Team, Visit_Team, PossessionTeam, ep, PlayResult, Game_Day, Quarter, GameClock) %>%
  mutate(Game_Date = str_replace(Game_Date, "T00:00:00Z", ""),
         Game_Date = as.Date(Game_Date, "%Y-%m-%d"), 
         GameClock = as.numeric(as.period(ms(GameClock), unit = "sec"))) %>%
  rename(date = Game_Date) %>% 
  group_by(GameKey, Quarter) %>% 
  mutate(GameClock = 
           case_when(Quarter == 1 ~ GameClock,
                     Quarter == 2 ~ GameClock + 900,
                     Quarter == 3 ~ GameClock + 1800,
                     Quarter == 4 ~ GameClock + 2700,
                     Quarter == 5 ~ GameClock + 3600))
s
#data for visualization1 below
ep_blt_vs_pit <- fulldata_with_thurs %>% 
  filter(HomeClubCode %in% c("BLT","PIT"),
         VisitorClubCode %in% c("BLT", "PIT"),
         Season == "2010") %>%
  select(GameKey, date, Season, GameClock, PossessionTeam, ep) 

#visualization1: change of ep for BLT vs. PIT during their games in 2010   
blt_vs_pit_2010 <- ggplot(data = ep_blt_vs_pit, aes(x = GameClock, y = ep, color = PossessionTeam)) +
  geom_line() + 
  facet_wrap(facets = "date", nrow = 5) + 
  ggtitle(label = "Change of ep for BLT vs. PIT Games in 2010.", subtitle = "Shows Offensive Team Production") 
blt_vs_pit_2010

# function that shows the change of ep for an given team during all their home games
# for an given year
#example: team_dep("BLT", "2010")
team_dep <- function(team, year) { 
  dep_data <- combined_all %>% 
    filter(HomeClubCode == team,
           Season == year) %>% 
    select(GameKey, date, Season, GameClock, PossessionTeam, ep)
  
  dep_plot <- ggplot(data = dep_data, aes(x = GameClock, y = ep, color = PossessionTeam)) +
    geom_line() + 
    facet_wrap(facets = "date", nrow = 5) + 
    ggtitle(label = "Change of Expected Points in Games", subtitle = "Shows Offensive Team Production") 
  
  return(dep_plot) 
}

function_name <- function(x) {
  output <- x * 2
  return(output)
}
function_name(5)
#creating variables ep1 and ep2: expected points for home team and expected points for visitor team 
ep_both_teams <- combined_all %>% 
  mutate(is_home = ifelse(as.character(HomeClubCode) == as.character(PossessionTeam), 1, 0),
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

#clean nfl_elo data with correct team codes 
new_nfl_elo <- nfl_elo %>% 
  mutate(
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

#merge new_nfl_elo and combined_all 
full_with_elo= merge(new_nfl_elo, combined_all, 
            by.x=c("HomeClubCode", "VisitorClubCode", "date"), by.y=c("HomeClubCode", "VisitorClubCode", "date"))

#filter only afc teams 
afc <- full_with_elo %>% 
  filter(HomeClubCode %in% c("BUF", "MIA", "NEP", "NYJ",
                             "BLT", "CIN", "CLE", "PIT",
                             "HOU", "IND", "JAC", "TEN",
                             "DEN", "KC", "OAK"),
         VisitorClubCode %in% c("BUF", "MIA", "NEP", "NYJ",
                                "BLT", "CIN", "CLE", "PIT",
                                "HOU", "IND", "JAC", "TEN",
                                "DEN", "KC", "OAK"))

#data for vis2 and vis3 below
pit_full <- combined_all %>%
  filter(HomeClubCode == "PIT" | VisitorClubCode == "PIT",
         Season == "2019") 

pit_nfl_elo <- new_nfl_elo %>% 
  filter(HomeClubCode == "PIT" | VisitorClubCode == "PIT",
         season == "2019") 

pit_2019 <- pit_full %>% 
  inner_join(pit_nfl_elo, by = "date")

pit_visitor_2019 <- pit_2019 %>%
  filter(VisitorClubCode.x == "PIT")

pit_home_2019 <- pit_2019 %>%
  filter(HomeClubCode.x == "PIT")

#vis2: Pittsburg's elo rating before the game as visitor team
pit_visitor_elo <- ggplot(data = pit_visitor_2019, aes(x = date, y = elo2_pre)) +
  geom_line() + 
  geom_point(aes(color = is_thurs), size = 2, alpha = 0.6) +
  theme_bw()+
  geom_text(aes(label=HomeClubCode.x), hjust=0, vjust=0) +
  ggtitle(label = "Pittsburg's Elo Rating Before Games in 2019", subtitle = "as the visitor team.") 
pit_visitor_elo

#vis3: Pittsburg's elo rating before the game as home team
pit_home_elo <- ggplot(data = pit_home_2019, aes(x = date, y = elo1_pre)) +
  geom_line() + 
  geom_point(aes(color = is_thurs), size = 2, alpha = 0.6) +
  theme_bw()+
  geom_text(aes(label=VisitorClubCode.x), hjust=0, vjust=0) +
  ggtitle(label = "Pittsburg's Elo Rating Before Games in 2019", subtitle = "as the home team.") 
pit_home_elo
