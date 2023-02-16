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

age_score_plot 

cor(age_score$age, age_score$score)

## moderately positive correaltion -- older restaurant, higher inspection score, somewhat of a correlation but not high 

#QUESTION 3
#Look at what the different cities are to get a grasp on where misspellings exist
unique(data$CITY)

#Recode the cities that are different to look the same as their capitalized partner
data$CITY <- recode(data$CITY, "Cary" = "CARY", "Fuquay-Varina" = "FUQUAY VARINA", "Garner" = "GARNER","Raleigh" = "RALEIGH", "Zebulon"= "ZEBULON", "Wake Forest" = "WAKE FOREST", "MORRISVILE" = "MORRISVILLE", "RTP" = "RESEARCH TRIANGLE PARK", "Apex" = "APEX", "Holly Springs" = "HOLLY SPRINGS", "Fuquay Varina" = "FUQUAY VARINA", "HOLLY SPRING" = "HOLLY SPRINGS", "Morrisville" = "MORRISVILLE" )

#Check to make sure that this corrected the issues 
unique(data$CITY)

#Make a bar graph showing inspection scores and cities
group_by(na.omit(data), CITY) %>%
  summarize(score= mean(SCORE)) %>%
  ggplot(aes(CITY, score)) + geom_col() + theme(axis.text.x = element_text(angle=90))
