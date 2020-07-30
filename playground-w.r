library(here)
library(tidyverse)
library(tidyselect)
library(ggthemr)
library(ggridges)
library(ggpubr)
library(plotly)
library(jpeg)
library(RCurl)
library(gganimate)
library(gifski)

source(here("src", "functions.r"))
source(here("src", "metrics.r"))

ggthemr("dust")

file_renderer(dir = ".", prefix = "gganim_plot", overwrite = FALSE)

# reading files ----

women_historical <- reading_file(here("data/raw/2020-Womens-Data"))
w <- reading_file(here("data/raw/2020-Womens-Data"))
women_stage1 <- reading_file(here("data/raw/2020-Womens-Data/WDataFiles_Stage1"))

wplay_stage2 <- reading_file(here("data/raw/WPlayByPlay_Stage2"))

# wrangling ----

hist_women <- women_historical[1:5] %>% bind_rows() 

wplayby <- wplay_stage2[1:6] %>% bind_rows() 

wteams <- women_stage1 %>% pluck(12)

wseeds <- women_stage1 %>% pluck(6)

wregular <- women_stage1 %>% pluck(9)

wncaa <- women_stage1 %>% pluck(5)

wplayers <- women_historical %>% pluck(6)

hist_women <- hist_women %>% left_join(wteams, by = c("WTeamID" = "TeamID")) %>% 
  left_join(wteams, by = c("LTeamID" = "TeamID")) %>% 
  left_join(wteams, by = c("EventTeamID" = "TeamID"))

hist_women <- hist_women %>% rename("WTeamName" = "TeamName.x", "LTeamName" = "TeamName.y", 
                                    "EventTeamName" = "TeamName")



# the columns are equal in the regular season and the ncaa tourney, so we can join them!
# but how to identify which observation is from regular season and which is from ncaa? 
# by the DayNum. we know that the regular season ends in the DayNum 132 and the ncaa 
# tournament starts in DayNum 134

detailed_results <- bind_rows(wregular, wncaa)

# we will be working with data since 2010 season (2010 is the first year that we have
# detailed results for the women championship)

detailed_results <- detailed_results %>% mutate(tourney = ifelse(DayNum <= 132, "regular", 
                                                                 "ncaa"))

detailed_results <- detailed_results %>% mutate(DiffScore = WScore - LScore)
# detailed_results <- detailed_results %>% mutate_at(vars(ends_with("TeamID")), as.character)

# eda ----

detailed_results %>% head() %>% glimpse()


detailed_results %>% group_by(Season, WTeamID) %>% tally(sort = TRUE)


# winning 6 games in a DayNum > 132 means you won the NCAA championship
# connecticut has won ncaa 5 times since 2010

detailed_results %>% group_by(Season, WTeamID) %>% filter(tourney == "ncaa") %>% tally(sort=TRUE) %>% filter(n == 6) %>%
  arrange(Season) %>% left_join(wteams, by = c("WTeamID" = "TeamID"))

detailed_results %>% group_by(Season, WTeamID, LTeamID) %>% filter(tourney == "regular" & Season == 2010) %>% tally(sort=TRUE) %>%
  arrange(Season) %>% left_join(wteams, by = c("WTeamID" = "TeamID")) %>% left_join(wteams, by = c("LTeamID" = "TeamID"))

# score ----

detailed_results %>% 
  pivot_longer(cols = ends_with("Score"), names_to = "Team", values_to = "Score") %>% 
  filter(Team != "DiffScore") %>% 
  ggplot(aes(x = Team, y = Score, colour = Team)) +
  geom_boxplot() +
  facet_grid(~Season)


# score mean along seasons. something happened in 2014!
detailed_results %>% 
  pivot_longer(cols = ends_with("Score"), names_to = "Team", values_to = "Score") %>% 
  group_by(Season, Team) %>% summarise(Mean = mean(Score)) %>% 
  ggplot(aes(x = as.factor(Season), y = Mean, colour = Team, group = Team)) +
  geom_line()

detailed_results %>% 
  group_by(Season, WLoc) %>% tally() %>% mutate(freq = n/sum(n)) %>% 
  ggplot(aes(x = as.factor(Season), y = freq, fill = WLoc)) +
  geom_bar(stat = "identity", position = "stack")

detailed_results %>% 
  group_by(WLoc) %>% tally() %>% mutate(freq = n/sum(n)) %>% 
  ggplot(aes(x = 2, y = freq, fill = WLoc)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_polar("y", start = 0) +
  xlim(0.5, 2.5) +
  theme_void()

detailed_results %>% 
  pivot_longer(cols = ends_with("Score"), names_to = "Team", values_to = "Score") %>% 
  ggplot(aes(y = as.factor(Season), x = Score, fill = Team)) +
  geom_density_ridges() +
  geom_vline(xintercept = 64.25, linetype = "dotted") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("Score") +
  ylab("Season") +
  facet_wrap(~tourney)

detailed_results %>% 
  ggplot(aes(x = tourney)) +
  geom_bar()

# there's a lot more outliers in the regular season than in
# the ncaa games
detailed_results %>% 
  pivot_longer(cols = ends_with("Score"), names_to = "Team", values_to = "Score") %>% 
  ggplot(aes(x = as.factor(Season), y = Score, fill = Team)) +
  geom_boxplot() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("Score") +
  ylab("Season") +
  facet_wrap(~tourney)

detailed_results %>% 
  ggplot(aes(x = NumOT)) +
  geom_histogram()

# TODO: fix this lollipop chart
detailed_results %>% 
  mutate(DiffScore = WScore - LScore) %>% 
  ggplot(aes(x = NumOT, y = DiffScore)) +
  geom_segment(aes(x = NumOT, y = 0, xend = NumOT, yend = DiffScore))


# there's no big difference between the winner score and the loser score
# along the seasons but there's a lot of outliers here
detailed_results %>% 
  ggplot(aes(x = as.factor(Season), y = DiffScore)) +
  geom_boxplot()

# game-by-game ----

hist_women <- hist_women %>% mutate(tourney = ifelse(DayNum <= 132, "regular", "ncaa"))

hist_women %>%
  filter((EventTeamID == 3163) & (EventType == "miss2" | EventType == "made2")) %>% 
  group_by(Season, DayNum) %>% 
  ggplot(aes(x = X, y = Y, colour = EventType)) +
  geom_point() +
  facet_grid(~DayNum)


fig <- hist_women %>% filter((tourney == "ncaa") & (EventType == "miss2" | EventType == "made2")) %>% 
  plot_ly(
    x = ~X,
    y = ~Y,
    frame = ~DayNum,
    type = 'scatter',
    mode = 'markers',
    color = ~EventType,
    showlegend = F
  ) %>% 
  animation_opts(transition = 0)

z <- hist_women %>% filter(EventType == "miss2" | EventType == "made2") %>% 
      select(EventType, X, Y) %>% pivot_longer(-EventType, names_to = "value", values_to = "value", names_repair = "unique") %>% 
      rename("Coord" = "value...2", "Value" = "value...3")

hist_women %>% filter((EventTeamID == WTeamID) & (X > 0 | Y > 0) & (EventType == "miss2" | EventType == "made2")) %>% 
  group_by(DayNum, EventType) %>% tally() %>% mutate(perc = n/sum(n)) %>% ungroup()

hist_women %>% filter((X > 0 | Y > 0) & 
                        (EventType == "miss2" | EventType == "made2" | 
                           EventType == "miss3" | EventType == "made3")) %>% 
  group_by(DayNum, EventType) %>% tally() %>% 
  mutate(pa2 = ifelse(EventType == "made2", sum(n), 0),
         pa3 = ifelse(EventType == "made3", sum(n), 0),
         fga = ifelse((EventType == "miss2" | EventType == "miss3"), sum(n), 0)) %>% 
  ungroup() %>% group_by(DayNum)


data_court <- hist_women %>%
  filter((X > 0 | Y > 0) & (EventType == "miss2" | EventType == "made2" | EventType == "miss3" | EventType == "made3"))  %>% 
  mutate(RX = ifelse(X > 50, (-X)+100, X),
         Shot = ifelse((EventType == "made2" | EventType == "made3"), "Made", "Missed")) %>% 
  group_by(Season, DayNum, RX, Y, Shot) %>% tally() %>% 
  pivot_wider(names_from = Shot, values_from = n, values_fill = 0) %>% 
  mutate(Points_per_shot = map2_dbl(Made, Missed, points_per_shot))

fig2 <- hist_women %>%
  filter((X > 0 | Y > 0) & (EventType == "miss2" | EventType == "made2" | EventType == "miss3" | EventType == "made3"))  %>% 
  mutate(RX = ifelse(X > 50, (-X)+100, X),
         Shot = ifelse((EventType == "made2" | EventType == "made3"), "Made", "Missed")) %>% 
  group_by(DayNum, RX, Y, Shot) %>% tally() %>% glimpse()
  plot_ly(
    x = ~RX,
    y = ~Y,
    frame = ~DayNum,
    type = 'histogram2d',
    #mode = 'none',
    color = ~Shot,
    showlegend = T
  ) %>% 
  animation_opts(transition = 0)

fig3 <- data_court %>% 
  plot_ly(
    x = ~RX,
    y = ~Y,
    #frame = ~DayNum,
    type = 'scatter',
    mode = 'markers',
    color = ~Points_per_shot,
    showlegend = T
  )

image_file <- "nba_court.jpg"
txt <- RCurl::base64Encode(readBin(image_file, "raw", file.info(image_file)[1, "size"]), "txt")

# shout out to 
# http://w3.salemstate.edu/~mluna/projects/2019_01Spring/GPH875/Coyne_Directed_Study.html

court <- rasterGrob(readJPEG("nba_court.jpg"),
                    width=unit(1,"npc"), height=unit(1,"npc"))

p1 <- ggplot(data_court, aes(x = RX, y = Y)) +
  annotation_custom(court, -5, 55, -5, 105) +
  geom_point(aes(colour = Points_per_shot)) +
  scale_colour_viridis_c() +
  labs(colour = "Points per shot")

p1 <- 
  p1 + transition_time(DayNum) +
  labs(title = "DayNum: {as.integer(frame_time)}") +
  shadow_mark(alpha = 0.1, size = 0.5)

women_contour <- 
  hist_women %>% filter((X > 0 | Y > 0) & 
                          (EventType == "made1" | EventType == "made2" | 
                           EventType == "made3")) %>% 
  mutate(RX = ifelse(X > 50, (-X)+100, X), Y) %>% 
  ggplot(aes(RX, Y))

women_contour + stat_density_2d(aes(fill = ..level..), geom = "polygon") +
  scale_fill_gradientn(colors = c("#FFEDA0", "#FEB24C", "#F03B20"))


a <- hist_women %>% filter((WTeamName == "Baylor") & (EventTeamID == WTeamID) & (X > 0 | Y > 0) & 
                             (EventType == "miss2" | EventType == "made2")) %>%
  mutate(RX = X %% 50, RY = Y %% 50) %>% 
  group_by(DayNum, EventType, X, RX, Y, RY) %>% tally() %>% mutate(perc = n/sum(n)) %>% ungroup()      



hist_women %>% filter(X > 0 | Y > 0) %>% group_by(Season, DayNum, X, Y) %>% 
  mutate(pm2 = ifelse(EventType == "made2", 1, 0),
         pm3 = ifelse(EventType == "made3", 1, 0)) %>%  tally(sort = TRUE)

