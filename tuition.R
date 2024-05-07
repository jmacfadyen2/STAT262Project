# Get the Data

tuition_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv')

library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)

###############################

#Filtering the colleges we want to study: 4 Year Private Colleges in the 50 States of the US
pri_tution_cost<- tuition_cost %>%
  filter(type == "Private") %>%
  filter(degree_length == "4 Year") %>%
  drop_na(state)

#mean of out of state total
mean_out_of_state_total<-mean(pri_tution_cost$out_of_state_total)

#standard of out of state total
sd_out_state_total<-sd(pri_tution_cost$out_of_state_total)

#Plot of our Raw Data
ggplot(pri_tution_cost, aes(out_of_state_total))+geom_histogram()+geom_vline(xintercept = mean_out_of_state_total, color = "red") + labs(x = "Out of State Total Costs", y = "Frequency", title = "Frequency of Out of State Total Costs in a 4 Year Private College")


##########################################


#Bootstrap

#Set up an empty data set with 2 columns: simulation number, bootstrap min#

boot.samples<-data.frame(sim=1:10000,min_total_cost=NA)

head(boot.samples)

#For each row in the data set, draw a bootstrap sample from the original data and find min_total_cost

for(i in 1:10000){
  boot.samples$min_total_cost[i]<-min(sample(pri_tution_cost$out_of_state_total,size=30,replace=TRUE))
}

head(boot.samples)

#mean min tuition
mean_min_total_cost<-mean(boot.samples$min_total_cost)

#standard deviation of min tuition
sd(boot.samples$min_total_cost)


#Histogram#
boot.hist<-ggplot(boot.samples, aes(min_total_cost)) + geom_histogram(binwidth=1000)+ geom_vline(xintercept = mean_min_total_cost, color = "red") + labs(x = "Minimum Total Cost", y = "Frequency", title = "Bootstrap Samples of Minimum Cost")
boot.hist



#############################################CDF
#The CDF of the boot.samples$min_total_cost
cdf <- ecdf(boot.samples$min_total_cost)
#plot CDF
plot(cdf, xlab='x', ylab='CDF', main='CDF of Boot.Samples Data')

##################### Exponential Distribution
beta <- 2
# Generate random numbers from exponential distribution
data <- data.frame(x = rexp(10000, rate = 40228.46/30)) # Change the rate parameter as needed
# Plot the density of the generated data using ggplot2
ggplot(data, aes(x)) +
  geom_density(fill = "blue", alpha = 0.5) +
  theme_minimal() +
  labs(x = "Values", y = "Density", title = "Exp. Distr. w/ rate = ( mean of out of state total) / n")

################################Generic beta distr.
# Set the parameters for the beta distribution
alpha <- 2  # Shape parameter (alpha > 0)
beta <- 5   # Shape parameter (beta > 0)

# Generate random numbers from beta distribution
data <- data.frame(x = rbeta(1000, shape1 = alpha, shape2 = beta))

ggplot(data, aes(x)) +
  geom_density(fill = "blue", alpha = 0.5) +
  theme_minimal() +
  labs(x = "Values", y = "Density", title = "Generic Beta Distribution (α = 2, β = 5)")


#####################################################################################

################################################Boot.Samples Density Distribution
ggplot(boot.samples, aes(min_total_cost)) +
  geom_density(fill = "blue", alpha = 0.5) +
  theme_minimal() +
  labs(x = "Values", y = "Density", title = "Boot.Samples Graphed as an Unknown Density Distribution")





