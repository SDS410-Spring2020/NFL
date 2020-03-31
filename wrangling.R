library(janitor)

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

fulldata_with_thurs <- data %>% 
  mutate(is_thurs = ifelse(game_day == "Thursday", 1, 0))
