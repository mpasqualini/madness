library(here)
library(dplyr)
library(tidyselect)
library(stringr)
library(ggplot2)
library(ggthemr)
library(readr)

ggthemr("dust")
# reading files ----

# women

wevents15 <- read.csv(here("data/raw/2020-Womens-Data", "WEvents2015.csv"))
wevents16 <- read.csv(here("data/raw/2020-Womens-Data", "WEvents2016.csv"))
wevents17 <- read.csv(here("data/raw/2020-Womens-Data", "WEvents2017.csv"))
wevents18 <- read.csv(here("data/raw/2020-Womens-Data", "WEvents2018.csv"))
wevents19 <- read.csv(here("data/raw/2020-Womens-Data", "WEvents2019.csv"))
wplayers <- read.csv(here("data/raw/2020-Womens-Data", "WPlayers.csv"))

# women stage1
wteams <- read.csv(here("data/raw/2020-Womens-Data/WDataFiles_Stage1", "WTeams.csv"))
wgamecities <- read.csv(here("data/raw/2020-Womens-Data/WDataFiles_Stage1", "WGameCities.csv"))
wcompact <- read.csv(here("data/raw/2020-Womens-Data/WDataFiles_Stage1", "WNCAATourneyCompactResults.csv"))
wdetailed <- read.csv(here("data/raw/2020-Womens-Data/WDataFiles_Stage1", "WNCAATourneyDetailedResults.csv"))
wseeds <- read.csv(here("data/raw/2020-Womens-Data/WDataFiles_Stage1", "WNCAATourneySeeds.csv"))
wslots <- read.csv(here("data/raw/2020-Womens-Data/WDataFiles_Stage1", "WNCAATourneySlots.csv"))
wcompactRegular <- read.csv(here("data/raw/2020-Womens-Data/WDataFiles_Stage1", "WRegularSeasonCompactResults.csv"))
wdetailedRegular <- read.csv(here("data/raw/2020-Womens-Data/WDataFiles_Stage1", "WRegularSeasonDetailedResults.csv"))
wseasons <- read.csv(here("data/raw/2020-Womens-Data/WDataFiles_Stage1", "WSeasons.csv"))
wtconferences <- read.csv(here("data/raw/2020-Womens-Data/WDataFiles_Stage1", "WTeamConferences.csv"))
wteamspell <- read.csv(here("data/raw/2020-Womens-Data/WDataFiles_Stage1", "WTeamSpellings.csv"))

# men 
# general

cities <- read.csv(here("data/raw/2020-Womens-Data/WDataFiles_Stage1", "Cities.csv"))
conferences <- read.csv(here("data/raw/2020-Womens-Data/WDataFiles_Stage1", "Conferences.csv"))

# exploring ----

wteamspell %>% sample_n(10)

wevents15 <- wevents15 %>% 
                left_join(wteams, by = c("WTeamID" = "TeamID")) %>% 
                left_join(wteams, by = c("LTeamID" = "TeamID")) %>% 
                rename("WTeamName" = "TeamName.x",
                       "LTeamName" = "TeamName.y")

wevents15 %>% 
  select(WTeamName, LTeamName, WTeamID) %>% 
    distinct() %>% 
      group_by(WTeamName) %>% 
      tally() %>% 
      collect() %>% 
      filter(n > 19) %>% 
      ggplot() +
        geom_col(aes(x = WTeamName, y = n))
# Seeds

wseeds <- wseeds %>% left_join(wteams, by = "TeamID") %>% 
          mutate(Region = as.factor(str_sub(Seed, 1, 1)),
                 SeedNo = as.numeric(str_sub(Seed, 2))) 

wseeds %>% write_rds(here("data/processed/women/wseeds.rds"))

wseeds %>% 
  filter(SeedNo == 1) %>% 
    group_by(TeamName) %>% 
      count(sort = TRUE) %>% 
        filter(n > 3) %>% 
        ggplot(aes(reorder(x = TeamName, X = -n, sum), n)) +
          geom_col() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()) +
            xlab("Team") +
            ylab("Number of times in #1 seed")

wseeds %>% 
  filter(SeedNo == 16) %>% 
  group_by(TeamName) %>% 
  count(sort = TRUE) %>% 
  filter(n >= 3) %>% 
  ggplot(aes(reorder(x = TeamName, X = -n, sum), n)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("Team") +
  ylab("Number of times in #16 seed")
          
last_game <- wcompact %>% 
  left_join(wteams, by = c("WTeamID" = "TeamID")) %>%
  left_join(wteams, by = c("LTeamID" = "TeamID")) %>% 
  rename("WTeamName" = "TeamName.x", "LTeamName" = "TeamName.y") %>% 
    group_by(Season) %>% 
      mutate(LastGameDayNum = max(DayNum)) %>% 
        select(Season, LastGameDayNum, WTeamID, LTeamID, WScore, LScore)
