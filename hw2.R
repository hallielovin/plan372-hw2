#load in libraries 
library(ggplot2)
library(tidyverse)

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

#Make a line plot to show the relationship between restaurant age and score
age_score <- group_by(data, age) %>% 
  summarize(score=mean(SCORE))

ggplot(age_score, aes(x=age, y=score)) + geom_line() 
## it seems like the average score for restaurants in different age categories varies by the age of the restauarnat but there is not any true correlation-- it does, however, seem that after a restaurant is open for 30 years or more their score is better 

