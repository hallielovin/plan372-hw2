#load in libraries 
library(ggplot2)
library(tidyverse)
library(dplyr)

#load in data 
data = read.csv("restaurant_inspections.csv")

#QUESTION 1
#make a histogram of the overall inspection scores
scores_hist <- hist(data$SCORE)
## all the scores or distributed between 80 and 100

#QUESTION 2
#Use the restaurant open date and inspection date to determine the age of restaurants
inspection_date <- data$DATE_
open_date <- data$RESTAURANTOPENDATE
require(lubridate)
age <- trunc((open_date %--% inspection_date)/years(1))

#Add a new column to the data that has the age of the restaurant 
data <- cbind(data, age)

#Make a scatter plot to show the relationship between restaurant age and score
age_score <- group_by(na.omit(data), age) %>% 
  summarize(score=mean(SCORE))

age_score_plot <- ggplot(age_score, aes(age, score)) + geom_point() 

cor(age_score$age, age_score$score)

## moderately positive correaltion -- older restaurant, higher inspection score, somewhat of a correlation but not high 

