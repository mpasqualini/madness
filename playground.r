library(here)
library(dplyr)
library(purrr)
library(tidyselect)
library(stringr)
library(ggplot2)
library(ggthemr)
library(ggridges)
library(readr)

source(here("src", "reading_file.r"))

ggthemr("dust")

# reading files ----

# women
women_historical <- reading_file(here("data/raw/2020-Womens-Data"))
women_stage1 <- reading_file(here("data/raw/2020-Womens-Data/WDataFiles_Stage1"))

# men 
men_historical <- reading_file(here("data/raw/2020-Mens-Data"))
men_stage1 <- reading_file(here("data/raw/2020-Mens-Data/MDataFiles_Stage1"))

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

# exploring winners ----
# TODO get the winners per season

last_game <- wcompact %>% 
  filter(DayNum == 153) %>% 
  left_join(wteams, by = c("WTeamID" = "TeamID"))

# exploring men

mteams <- pluck(men_stage1, 18)
mseeds <- pluck(men_stage1, 9)
mwinners <- pluck(men_stage1, 6) %>% filter(DayNum == 154) %>% left_join(mteams, by = c("WTeamID" = "TeamID"))

mwinners %>% 
  group_by(TeamName) %>% 
  tally(sort = TRUE)

mevents15 <- pluck(men_historical, 1)
mevents16 <- pluck(men_historical, 2)
mevents17 <- pluck(men_historical, 3)
mevents18 <- pluck(men_historical, 4)
mevents19 <- pluck(men_historical, 5)


mevents17 %>% 
  ggplot() +
  geom_density(aes(x = WFinalScore)) +
  geom_density(aes(x = LFinalScore), col = "black")
