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
ggplot(pri_tution_cost, aes(out_of_state_total))+geom_histogram()


#Expected Value
Ex<-mean(pri_tution_cost$out_of_state_total)

Mx<-min(pri_tution_cost$out_of_state_total)

#Bootstrap

#Set up an empty data set with 2 columns: simulation number, bootstrap mean#

boot.samples<-data.frame(sim=1:10000,min_tuition=NA)

head(boot.samples)

#For each row in the data set, draw a bootstrap sample from the original data and find min_tuition

for(i in 1:10000){
  boot.samples$min_tuition[i]<-min(sample(pri_tution_cost$out_of_state_total,size=30,replace=TRUE))
}

head(boot.samples)

mean_min_tuition<-mean(boot.samples$min_tuition)

#Histogram#
boot.hist<-ggplot(boot.samples, aes(min_tuition)) + geom_histogram(binwidth=1000)+ geom_vline(xintercept = mean_min_tuition, color = "red", linetype = "dashed")

#See the plot#
boot.hist
#What does our histogram tell us??


# Load required library
library(ggplot2)

# Generate random numbers from exponential distribution
data <- data.frame(x = rexp(1000, rate = 1/(Ex/10000)) ) # Change the rate parameter as needed

# Plot the density of the generated data using ggplot2
ggplot(data, aes(x)) +
  geom_density(fill = "blue", alpha = 0.5) +
  theme_minimal() +
  labs(x = "Values", y = "Density", title = "Exponential Distribution")




