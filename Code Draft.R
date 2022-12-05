library(tidyverse)
library(ggplot2)
library(mosaic)
library(scales)
library(dplyr)
library(forcats)
library(caret)
library(fastDummies)
library(magrittr)
library(readr)
library(stringr)
library(e1071)
library(lubridate)
library(purrr)
library(readxl)
library(gganimate)
library(MatchIt)
options(scipen = 10)

### Read in CSV file:
scores <- read_csv("spreadspoke_scores.csv")

### Filter to include only games from the 2000-2022 Seasons:
scores <- scores %>% filter(between(schedule_season, 2000, 2020))

### Various Data Cleaning:
scores <- scores %>% mutate(schedule_date = mdy(schedule_date)) #Date Formatting
scores <- scores %>%
  mutate(month = month(schedule_date))

### Adding a new column to indicate whether or not the home team won:
scores <- scores %>% mutate(home_win = case_when(
  score_home > score_away & stadium_neutral == FALSE ~ TRUE,
  stadium_neutral == TRUE ~ NA,
  TRUE ~ FALSE
))  
scores <- scores %>% filter(!is.na(home_win))

scores <- scores %>% mutate(home_win_binary = case_when(
  home_win == TRUE ~ 1,
  home_win == FALSE ~ 0
))

### Adding a new column to indicate whether or not the away team won:
scores <- scores %>% mutate(away_win = case_when(
  score_home < score_away & stadium_neutral == FALSE ~ TRUE,
  stadium_neutral == TRUE ~ NA,
  TRUE ~ FALSE
))  
scores <- scores %>% filter(!is.na(away_win))

scores <- scores %>% mutate(away_win_binary = case_when(
  away_win == TRUE ~ 1,
  away_win == FALSE ~ 0
))

#Preliminary analysis indicates a 56.6% proportion of home wins:
prop(~home_win, scores, success = "TRUE")


#Create New Data frame that contains the home win proportion by various factors:
by_season <- scores %>% 
  group_by(schedule_season) %>%
  summarize(homewin_pct = sum(home_win == TRUE)/n()) 

by_week <- scores %>%
  group_by(schedule_week) %>%
  summarize(homewin_pct = sum(home_win == TRUE)/n()) %>%
  arrange(schedule_week)

by_is_playoffs <- scores %>%
  group_by(schedule_playoff) %>%
  summarize(homewin_pct = sum(home_win == TRUE)/n())

by_month <- scores %>%
  group_by(month) %>%
  summarize(homewin_pct = sum(home_win == TRUE)/n())

by_home_team <- scores %>%
  group_by(team_home) %>%
  summarize(homewin_pct = sum(home_win == TRUE)/n()) %>%
  arrange(homewin_pct)

by_away_team <- scores %>%
  group_by(team_away) %>%
  summarize(awaywin_pct = sum(away_win == TRUE)/n()) %>%
  arrange(awaywin_pct)

by_team <- merge(x = by_home_team, y = by_away_team, 
                 by.x = "team_home", by.y = "team_away", all.x = TRUE)

by_team <- by_team %>% mutate(home_away_differential = homewin_pct - awaywin_pct)

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
  theme(axis.text.x=element_text(angle=60, hjust=1))

by_team_plot


team_differential_plot <- by_team %>% 
  ggplot(aes(x = reorder(team_home, +home_away_differential), y = home_away_differential)) +
  labs(title = "Difference in % of Home v. Away Games Won by Each NFL Team",
       x = 'Team',
       y = 'Home v. Away Win Differential Percentage') +
  geom_bar(stat="identity", fill = "darkblue") + 
  theme_grey() +
  theme(axis.text.x=element_text(angle=60, hjust=1))

team_differential_plot

# Model to identify other factors in home wins


model1 <- glm(home_win_binary ~ as.factor(stadium) + weather_temperature + weather_wind_mph + month +
                spread_favorite,
              family = binomial(link = "logit"),
              data = scores)
summary(model1)


team_names <- data.frame(unique(scores$team_home))


#Attendance Data

attendance <- read_csv("attendance_final.csv")

scores_attendance <- full_join(scores, attendance, by = "team_home", all.x = TRUE)


#Neural Network 

scores_factor <- lapply(scores,as.factor) %>% data.frame()


partition <- sample(c('train','test'), size = nrow(scores_factor), replace = TRUE, prob = c(.75,.25))

scores_nn <- mutate(scores_factor, partition)

d_scores_train <- filter(scores_nn, partition == 'train') %>%
  select(-partition)
d_scores_test <- filter(scores_nn, partition == 'test') %>% 
  select(-partition) 

set.seed(1) 

scores_nb <- naiveBayes(home_win_binary~. ,data=d_scores_train)

scores_nb_train <- mutate(d_scores_train, 
                       Prediction = predict(scores_nb, d_scores_train,type="class")) #dplyr

#Compute/display the Percentage Correct in the training data to evaluate accuracy.
mean(~(home_win_binary == Prediction), data=scores_nb_train)
tally(~(home_win_binary == Prediction), data=scores_nb_train) %>% addmargins() #mosaic
#Classification Table of percentages of training data.
tally(home_win_binary ~ Prediction, data=scores_nb_train) %>% 
  prop.table(margin=1) %>% round(2) #mosaic      



### TEST THE NAIVE BAYES MODEL WITH TEST DATA

#Evaluate Accuracy:  How good is the model on the test data? 
#Add the predicted values to the test data frame.
#round(): round estimated probabilities to get 0 or 1.
scores_nb_test <- mutate(d_scores_test, 
                      Prediction = predict(scores_nb, d_scores_test,type="class")) #dplyr

#Compute/display the Percentage Correct in test data to evaluate accuracy.
mean(~(home_win_binary == Prediction), data=scores_nb_test)
tally(~(home_win_binary == Prediction), data=scores_nb_test) %>% addmargins() #mosaic
#Classification Table of percentages of training data.
tally(home_win_binary ~ Prediction, data=scores_nb_test) %>% 
  prop.table(margin=1) %>% round(2) #mosaic      









#### BROKEN
scores_nn3 <- nnet(home_win_binary~., data = d_scores_train, 
                 size=3,linout=T,decay=0.05)


d_scores_nn3_train <- predict(scores_nn3, d_scores_train) 
RMSE(d_scores_nn3_train, d_scores_train$home_win_binary)

scores_nn3_train <- mutate(d_scores_train, 
                              Prediction = predict(scores_nn3, d_scores_train)) %>% round
mean(~(home_win_binary == Prediction), data=scores_nn3_train)
#Classification Table of raw counts.
tally(quality ~ Prediction, data=wine_pred_nn2_train) %>% addmargins() #mosaic
#Classification Table of percentages of test data.
tally(quality ~ Prediction, data=wine_pred_nn2_train) %>% 
  prop.table(margin=1) %>% round(3)








