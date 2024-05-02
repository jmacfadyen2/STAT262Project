# Get the Data

tuition_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv')

library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)


pri_tution_cost<- tuition_cost %>%
  filter(type == "Private") %>%
  filter(degree_length == "4 Year") %>%
  drop_na(state)
ggplot(pri_tution_cost, aes(out_of_state_tuition))+geom_histogram()


#Expected Value
mean(pri_tution_cost$out_of_state_tuition)

#Bootstrap

#Set up an empty data set with 2 columns: simulation number, bootstrap mean#

boot.samples<-data.frame(sim=1:1000,min_tuition=NA)

head(boot.samples)

#For each row in the data set, draw a bootstrap sample from the original data and find min_tuition

for(i in 1:1000){
  boot.samples$min_tuition[i]<-min(sample(pri_tution_cost$out_of_state_total,size=30,replace=TRUE))
}

head(boot.samples)

#Histogram#
boot.hist<-ggplot(boot.samples, aes(min_tuition)) + geom_histogram(binwidth=1000)

#See the plot#
boot.hist

