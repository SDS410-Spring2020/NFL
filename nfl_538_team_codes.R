#editing team codes in 538 data to match the team codes in our data 

#reading in nfl elo data from FiveThirtyEight
nfl_elo_2019 <- read.csv("nfl_elo_latest.csv")
nfl_elo <- read.csv("nfl_elo.csv")

#wrangle nfl elo data 
nfl_elo <- nfl_elo %>% 
  filter(season >= "2010") %>% 
  mutate(date = as.Date(date, "%Y-%m-%d")) %>% 
  rename(HomeClubCode = team1, 
         VisitorClubCode = team2)

fivethirtyeight_nfl_codes <- distinct(nfl_elo, HomeClubCode)

#reading in scraped data of team codes from nfl website 
club_codes <- read.csv("nfl_club_codes.csv")

#wrangle scraped data 
club_codes <- club_codes %>% 
  rename(code = NFL,
         team = X)

#reading in the two nfl data frames 
games <- read.csv("data/games_Smith.csv")
pbp<- read.csv("data/pbp_Smith.csv")

#create merged df 
combined_df <- games %>% 
  inner_join(pbp, by="GameKey")

#add a thursday column
fulldata_with_thurs <- combined_df %>% 
  mutate(is_thurs = ifelse(Game_Day == "Thursday", 1, 0))

#create df of team codes from our data 
unique_mydata_codes <- distinct(fulldata_with_thurs, PossessionTeam)

#adding id column to scraped data and unique 538 data
club_codes <- tibble::rowid_to_column(club_codes, "ID")
fivethirtyeight_nfl_codes <- tibble::rowid_to_column(fivethirtyeight_nfl_codes, "ID")

#create tibble 
vector1 <- seq(1, 34, 1)
vector2 <- seq(1, 34, 1)
vector3 <- seq(1, 34, 1)
m <- tibble(vector1, vector2, vector3) %>% 
  rename(mydata = vector1,
         in_known_code = vector2,
         in_538 = vector3)

index <- 0 
for (i in unique_mydata_codes$PossessionTeam) {
  club <- i  
  print(club)
  in_known_code <- ifelse(club %in% club_codes$code, 1, 0)
  in_538 <- ifelse(club %in% fivethirtyeight_nfl_codes$HomeClubCode, 1, 0)
  m$mydata[index] <- club
  m$in_known_code[index] <- in_known_code
  m$in_538[index] <- in_538
  index <- index + 1 
}

five38 <- tibble(vector1, vector2, vector3) %>% 
  rename(five38_data = vector1,
         in_mydata = vector2, 
         in_known_code = vector3)

index <- 0 
for (i in fivethirtyeight_nfl_codes$HomeClubCode) {
  club <- i  
  print(club)
  in_mydata <- ifelse(club %in% unique_mydata_codes$PossessionTeam, 1, 0)
  in_known_code <- ifelse(club %in% club_codes$code, 1, 0)
  five38$five38_data[index] <- club
  five38$in_mydata[index] <- in_mydata
  five38$in_known_code[index] <- in_known_code
  index <- index + 1 
}

known_code <- tibble(vector1, vector2, vector3) %>% 
  rename(known_code_data = vector1,
         in_mydata = vector2, 
         in_538 = vector3)

index <- 0 
for (i in club_codes$code) {
  club <- i  
  print(club)
  in_mydata <- ifelse(club %in% unique_mydata_codes$PossessionTeam, 1, 0)
  in_538 <- ifelse(club %in% fivethirtyeight_nfl_codes$HomeClubCode, 1, 0)
  known_code$known_code_data[index] <- club
  known_code$in_mydata[index] <- in_mydata
  known_code$in_538[index] <- in_known_code
  index <- index + 1 
}

missing_codes_in_mydata <- m %>% 
  filter(in_known_code == 0 | in_538 == 0) 

missing_codes_in_known_code <- known_code %>% 
  filter(in_mydata == 0 | in_538 == 0) 

missing_codes_in_538 <- five38 %>% 
  filter(in_mydata == 0 | in_known_code == 0)

#changing code names in 538 
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

#check if we have changed all the codes in 538 correctly 
fivethirtyeight_nfl_codes <- distinct(new_nfl_elo, HomeClubCode)

vector1 <- seq(1, 34, 1)
vector2 <- seq(1, 34, 1)
vector3 <- seq(1, 34, 1)
five38 <- tibble(vector1, vector2, vector3) %>% 
  rename(five38_data = vector1,
         in_mydata = vector2, 
         in_known_code = vector3)

index <- 0 
for (i in fivethirtyeight_nfl_codes$HomeClubCode) {
  club <- i  
  print(club)
  in_mydata <- ifelse(club %in% unique_mydata_codes$PossessionTeam, 1, 0)
  in_known_code <- ifelse(club %in% club_codes$code, 1, 0)
  five38$five38_data[index] <- club
  five38$in_mydata[index] <- in_mydata
  five38$in_known_code[index] <- in_known_code
  index <- index + 1 
}

#Woohoo! 