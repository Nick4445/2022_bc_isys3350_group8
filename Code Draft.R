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


scores <- read_csv("spreadspoke_scores.csv")

scores2 <- scores %>% filter(between(schedule_season, 2000, 2020))

scores2 <- scores2 %>% mutate(home_win = case_when(
  score_home > score_away ~ TRUE,
  TRUE ~ FALSE
))    

prop(~home_win, scores2, success = "TRUE")

