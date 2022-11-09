library(tidyverse)
library(ggplot2)
library(mosaic)
library(scales)
library(dplyr)
library(forcats)
library(magrittr)
library(readr)
library(stringr)
library(lubridate)
library(purrr)
library(readxl)
library(gganimate)

### Read in CSV file:
scores <- read_csv("spreadspoke_scores.csv")

### Filter to include only games from the 2000-2020 Seasons:
scores2 <- scores %>% filter(between(schedule_season, 2000, 2020))

### Various Data Cleaning:
scores2 <- scores2 %>% mutate(schedule_date = mdy(schedule_date)) #Date Formatting
scores2 <- scores2 %>%
  mutate(month = month(schedule_date))

### Adding a new column to indicate whether or not the home team won:
scores2 <- scores2 %>% mutate(home_win = case_when(
  score_home > score_away & stadium_neutral == FALSE ~ TRUE,
  stadium_neutral == TRUE ~ NA,
  TRUE ~ FALSE
))  
scores2 <- scores2 %>% filter(!is.na(home_win))

#Preliminary analysis indicates a 56.6% proportion of home wins:
prop(~home_win, scores2, success = "TRUE")


#Create New Data frame that contains the home win proportion by various factors:
by_season <- scores2 %>% 
  group_by(schedule_season) %>%
  summarize(homewin_pct = sum(home_win == TRUE)/n()) 

by_team <- scores2 %>%
  group_by(team_home) %>%
  summarize(homewin_pct = sum(home_win == TRUE)/n()) %>%
  arrange(homewin_pct)

by_week <- scores2 %>%
  group_by(schedule_week) %>%
  summarize(homewin_pct = sum(home_win == TRUE)/n()) %>%
  arrange(schedule_week)

by_is_playoffs <- scores2 %>%
  group_by(schedule_playoff) %>%
  summarize(homewin_pct = sum(home_win == TRUE)/n())

by_month <- scores2 %>%
  group_by(month) %>%
  summarize(homewin_pct = sum(home_win == TRUE)/n())

###Create Graphs to Visualize the data tables from above:

by_season_plot <- by_season %>% 
  ggplot(aes(x = schedule_season, y = homewin_pct)) +
  labs(title = "Percentage of Games Won by Home Team in Each Season",
       x = 'Season',
       y = 'Home Win Percentage') +
  geom_point() + 
  theme_grey()

by_season_plot  


by_team_plot <- by_team %>% 
  ggplot(aes(x = reorder(team_home, +homewin_pct), y = homewin_pct)) +
  labs(title = "Percentage of Home Games Won by Each NFL Team (2000-2020)",
       x = 'Team',
       y = 'Home Win Percentage') +
  geom_bar(stat="identity", fill = "darkblue") + 
  geom_hline(yintercept = 0.5) +
  theme_grey() +
  theme(axis.text.x=element_text(angle=90, hjust=1))

by_team_plot
