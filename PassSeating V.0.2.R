#Libraries
#install.packages("purrr")
#install.packages("denstrip")
#install.packages("prob")
#install.packages("tidyr")
#install.packages("dplyr")
library(purrr) 
library(prob)
library(denstrip)
library(tidyr)

## Take two on the seating optimization problem

# In this attempt I will try speeding up the operation by using te dplyr package and by vectorizing the code
# We will also improve the algoritm by allocating orders in the vehicle in such a manner 

## Global variables

#Vehicle params
vehicle.seats <- 122

#Route params - the number of stops and the path to the ticket distribution table
route.path <- "orders_hist.csv"
route.orders.distr<-read.csv(route.path,header=TRUE,colClasses = c("character","numeric"), sep=";")
route.stops<-max(as.numeric(unlist(strsplit(route.orders.distr$route,":"))))-1
barplot(height=route.orders.distr$prob, names.arg = route.orders.distr$route, ylab = "Probability", xlab = "Route")

#Test params
test.repetitions <- 10000

## Initialize Supporting Data structures

### Create the probability matrix based on the distribution
route.orders.distr[,-3] %>%
separate("routes",c("from","to"),sep=":") %>%
transform(from=as.numeric(from), to=as.numeric(to)) %>%
mutate(to=to-1) %>% 
spread(to,prob) 


  