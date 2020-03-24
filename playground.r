library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(tidyselect)
library(stringr)
library(ggplot2)
library(ggthemr)
library(ggridges)
library(readr)
library(ggpubr)

source(here("src", "reading_file.r"))
source(here("src", "metrics.r"))

ggthemr("dust")

# reading files ----

# women
women_historical <- reading_file(here("data/raw/2020-Womens-Data"))
women_stage1 <- reading_file(here("data/raw/2020-Womens-Data/WDataFiles_Stage1"))

# men 
men_historical <- reading_file(here("data/raw/2020-Mens-Data"))
men_stage1 <- reading_file(here("data/raw/2020-Mens-Data/MDataFiles_Stage1"))

# exploring men

mteams <- pluck(men_stage1, 18)
mseeds <- pluck(men_stage1, 9)
mwinners <- pluck(men_stage1, 6) %>% filter(DayNum == 154) %>% left_join(mteams, by = c("WTeamID" = "TeamID"))

mwinners %>% 
  group_by(TeamName) %>% 
  tally(sort = TRUE)

mevents15 <- pluck(men_historical, 1)
mevents16 <- pluck(men_hmevents19ical, 2)
mevents17 <- pluck(men_historical, 3)
mevents18 <- pluck(men_historical, 4)
mevents19 <- pluck(men_historical, 5)

# exploring seeds ----
# TODO relation between seeds and winners

mseeds <- men_stage1 %>% pluck(9) %>% 
            mutate(Region = as.factor(str_sub(Seed, 1, 1)), 
                   SeedNo = as.numeric(str_sub(Seed, 2, 3))) %>% 
            left_join(mteams, by = "TeamID")

mseeds %>% write_rds(here("data/processed/men/mseeds.rds"))

mseeds %>% 
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

mwinners %>% 
  group_by(TeamName) %>% 
  count(sort = TRUE) %>% 
  filter(n > 2) %>% 
  ggplot(aes(reorder(x = TeamName, X = -n, sum), n)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("Team") +
  ylab("Number of times winning the competition")

# exploring WScore ----

compact_results <- men_stage1 %>% pluck(6)

a <- compact_results %>% 
  pivot_longer(cols = contains("Score"), names_to = "resolution")

a %>% 
  ggplot(aes(y = as.factor(Season), x = value, fill = resolution)) +
  geom_density_ridges() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("Score") +
  ylab("Season")

# there's no big difference between the winner and loser score
# but if you score > 80 points in the game, you'll probably be the winner

compact_results <- compact_results %>% 
  mutate(DifScores = WScore - LScore) 

compact_results %>% ggplot(aes(x = DifScores, y = as.factor(Season))) + 
                      geom_density_ridges()


a %>% 
  ggplot(aes( x = value, fill = resolution)) +
  geom_density() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("Score") +
  ylab("Season")

a %>% 
  ggplot(aes(y = value, x = as.factor(Season), colour = resolution)) +
    geom_boxplot()

compact_regular <- pluck(men_stage1, 11) %>% mutate(WLoc = as.factor(WLoc), 
                                                    DifScore = WScore - LScore)

compact_regular %>% 
  group_by(WLoc) %>% 
    summarise(n = n()) %>% 
      mutate(freq = n/sum(n)) %>% 
        ggplot(aes(x = relevel(WLoc, ref = "H"), y = freq)) +
          geom_bar(stat = "identity") +
          theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
          scale_y_continuous(labels = scales::percent) +
          xlab("Location of the winning team") +
          ylab("Relative frequency")

compact_regular %>% 
  ggplot(aes(x = DifScore, fill = WLoc)) +
    geom_bar(stat = "bin") + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    xlab("Difference between winning and losing scores") +
    ylab("Count")

compact_regular %>% 
  ggplot(aes(x = WLoc, y = DifScore)) +
  geom_boxplot() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ylab("Difference between winning and losing scores") +
  xlab("Location of the winning team")


# in the regular season, there's a slightly difference 
# between the difference in wscore and lscore according to
# the location of the winner (home/visitor/neutral)

compact_regular %>% 
  group_by(TeamName) %>% 
    tally(sort = TRUE) %>% 
      ggplot(aes(x = TeamName, y = n)) +
        geom_point()

# TODO get the winners per season

mwinners %>% 
  group_by(TeamName) %>% 
    count(sort = TRUE)

last_game <- wcompact %>% 
  filter(DayNum == 153) %>% 
  left_join(wteams, by = c("WTeamID" = "TeamID"))

# rivals 

compact_regular <- compact_regular %>%
                    left_join(mteams, by = c("WTeamID" = "TeamID")) %>% 
                    left_join(mteams, by = c("LTeamID" = "TeamID")) %>% 
                        rename("WTeamName" = "TeamName.x", "LTeamName" = "TeamName.y",
                               "WFirstD1Season" = "FirstD1Season.x", "WLastD1Season" = "LastD1Season.x",
                               "LFirstD1Season" = "FirstD1Season.y", "LLastD1Season" = "LastD1Season.y")
rivals <- compact_regular %>% 
            group_by(WTeamName, LTeamName) %>% 
              tally(sort = TRUE)

rivals2 <- rivals %>% filter(n > 56)

ggballoonplot(data = rivals2, x = "WTeamName", y = "LTeamName", size = "n")

rivals %>% 
  filter(n > 55) %>% 
    ggplot(aes(x = WTeamName, y = LTeamName, fill = n)) +
      geom_tile() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())

detailed <- pluck(men_stage1, 7)

detailed <- detailed %>% 
              mutate(WPoss = pmap_dbl(list(WFGA, WFGA3, WFTA, WTO, WOR), possession),
                     LPoss = pmap_dbl(list(LFGA, LFGA3, LFTA, LTO, LOR), possession))
