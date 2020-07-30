library(here)
library(tidyverse)
library(tidyselect)
library(gganimate)
library(gifski)

source(here("src", "functions.r"))
source(here("src", "metrics.r"))

file_renderer(dir = ".", prefix = "gganim_plot", overwrite = FALSE)

# reading files ----

women_historical <- reading_file(here("data/raw/2020-Womens-Data"))
hist_women <- women_historical[1:5] %>% bind_rows() 

# wrangling and stuff ----

hist_women <- hist_women %>% mutate(tourney = ifelse(DayNum <= 132, "regular", "ncaa"))

data_court <- 
  hist_women %>%
  filter((X > 0 | Y > 0) & (EventType == "miss2" | EventType == "made2" | EventType == "miss3" | EventType == "made3"))  %>% 
  mutate(RX = ifelse(X > 50, (-X)+100, X),
         Shot = ifelse((EventType == "made2" | EventType == "made3"), "Made", "Missed")) %>% 
  group_by(Season, DayNum, RX, Y, Shot) %>% tally() %>% 
  pivot_wider(names_from = Shot, values_from = n, values_fill = 0) %>% 
  mutate(Points_per_shot = map2_dbl(Made, Missed, points_per_shot))

# court image ----
# shout out to 
# http://w3.salemstate.edu/~mluna/projects/2019_01Spring/GPH875/Coyne_Directed_Study.html

court <- rasterGrob(readJPEG("nba_court.jpg"),
                    width=unit(1,"npc"), height=unit(1,"npc"))

# plot ----

p1 <- ggplot(data_court, aes(x = RX, y = Y)) +
  annotation_custom(court, -5, 55, -5, 105) +
  geom_point(aes(colour = Points_per_shot)) +
  scale_colour_viridis_c() +
  labs(x = "", 
       y = "",
       colour = "Points per shot") +
  theme_void()

p1 <- 
  p1 + transition_time(DayNum) +
  labs(title = " 2018-19 WNCAA Basketball season | DayNum: {as.integer(frame_time)}")

animate(p1, renderer = gifski_renderer())
