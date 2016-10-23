#Libraries
#install.packages("purrr")
library(purrr)
library(prob)
library(denstrip)
library(tidyr)

#Initialize global test variables
inputs<-list(
  seats=122,
  sample_size=170,
  stops=7,
  path="orders_hist.csv"
)

#Retrieve route passengers orders probability distribution or generates a random one
if(is.character(inputs$path)){
  orders_distr<-read.csv(inputs$path,header=TRUE,colClasses = c("character","numeric"), sep=";")
  inputs$stops=max(as.numeric(unlist(strsplit(orders_distr$route,":"))))-1
}else{
  result <- expand.grid(1:inputs$stops,1:inputs$stops)
  result <-result[result[,2]>result[,1],]
  r_routes<-apply(result,1,function(x){paste(x[1],":",x[2])})
  samp<-abs(rnorm(length(r_routes)-1))
  r_prob<-samp/sum(samp)
  orders_distr<-data.frame(r_routes,r_prob,1)
  names(orders_distr) <- c("route","prob","price")
}

barplot(height=orders_distr$prob, names.arg = orders_distr$route, ylab = "Probability", xlab = "Route")

#Make probability table
prob_matrix<-separate(orders_distr,"routes",c("from","to"),sep=":")
prob_matrix<-transform(prob_matrix,from=as.numeric(from), to=as.numeric(to))
prob_matrix[2]<-prob_matrix[2]-1

#This fills the table, so we can calculate the probability of having an order for an interval by 
#making the sum of the area defined by the X,Y cordinates of an interval 
prob_matrix2<-matrix(data=0,nrow=inputs$stops,ncol = inputs$stops)
for(i in 1:nrow(prob_matrix)){
  prob_matrix2[prob_matrix[i,1],prob_matrix[i,2]]<-prob_matrix[i,3]
}

#Create random order sample according to given distribution
create_sample_orders<-function(sample_size=inputs$sample_size,routes=orders_distr$route,probability=orders_distr$prob,inp=inputs,output="sample"){

#Create sample from distribution    
orders_sample<-sample(routes, size=sample_size, replace=TRUE, prob=probability)

  #Return list of orders / or show sample
  if(output=="plot"){
    orders_tbl<-table(orders_sample)
    return(barplot(height = orders_tbl,  ylab = "Number of Orders", xlab = "Route"))
  }else{
    orders_list<-map(strsplit(orders_sample,":"),as.numeric)
  #This change will revert the stops to intervals eg instead of seat 1,2 to routes 1-2,2-3
    orders_list<-map(orders_list,function(x){c(x[1],x[2]-1)})
  }
} 

#Create the default order list
route_orders<-create_sample_orders()

#Create the default empty vehicle
vehicle<-matrix(data=0,ncol=inputs$stops,nrow=inputs$seats)

#This function checks which seats are available for the given order and returns their position or returns false if no free seats are available
is_free<-function(vehicle,order){
  #turn the seats in bool values
  free_positions<-if(length(order[1]:order[2])>1){rowSums(vehicle[,order[1]:order[2]])==0
                  }else{
                    vehicle[,order[1]:order[2]]==0
                  }
  
  if (length(order)==1){ 
    warning("There is just one number in the interval - check the data")
    return(FALSE)
  }else{
    free_seats<-which(free_positions)
  }
    if(length(free_seats)==0) FALSE else free_seats
}

#This function mimics the original algoritm
find_seat_orig<-function(vehicle,order){
#It looks for empty seats
  free_seats<-is_free(vehicle,order)
#If there are empty seats it returns the first one 
  if (is.numeric(free_seats)){
    seat<-free_seats[1]
  }else{
#Otherwise returns false
    return(FALSE)
  }

}

##Probability loss algoritm

#This function takes an interval and calculates the according probabilty sum
sum_probability<-function(vec,prob=prob_matrix2){
  if(all(is.na(vec))){
  1 
  }else{
  intervals<-seqToIntervals(vec)
  sum(apply(intervals,1,function(x){sum(prob[x[1]:x[2],x[1]:x[2]])}))
  }
}

#Here we start the function that returns the best seat
find_seat_probLoss<-function(vehicle=vehicle,order=order){
  
  #Find out if there are free seats for this order
  free_seats<-is_free(vehicle,order)
  
  if (is.numeric(free_seats)){
    
    #initialize some variables we need
    vehicle_inv<-(vehicle*-1)+1
    for(i in seq_along(vehicle_inv[1,])){
      vehicle_inv[vehicle_inv[,i]==1,i]<-i
    }
    prob_loss<-c()
  
    seatsBefore<-replace(vehicle_inv,vehicle_inv==0,NaN)
    seatsAfter<-seatsBefore
    seatsAfter[,order[1]:order[2]]<-NaN
    
    #here we calculate the probability loss
    for (i in seq_along(free_seats)){
      before<-sum_probability(seatsBefore[free_seats[i],])
      after<-sum_probability(seatsAfter[free_seats[i],])
      prob_loss<-c(prob_loss,(before-after))
    }
    #now we find the seat that lost less probability that will be filled
    position<-free_seats[which.min(prob_loss)]
    
  }else{
    #Otherwise returns false
    return(FALSE)
  }
}

#Tests the chosen algoritm and returns the results
fill_vehicle<-function(seats=inputs$seats,stops=inputs$stops,orders=route_orders,method=find_seat_orig,plot=FALSE,reorder=TRUE){
  
  #create the order list
  if(reorder){orders<-create_sample_orders(inputs$sample_size)}
  
  #create the result variables
  seated_people<-data.frame(data=0,nrow=stops,ncol=3)
  names(seated_people) <- c("order","seated","not_seated")
  result<-c()
  money<-0
  
  #takes the orders ans sits people by the chosen method
  for (i in seq_along(orders)) {
    order<-unlist(orders[i])
    j<-i-1
    free_seat<-method(vehicle,order)
  #if we have a numeric position returned we proceed writing the result
    if (is.numeric(free_seat)){
      vehicle[free_seat,order[1]:order[2]]<-i
      if (plot){
        plot(row~col,data=which(vehicle==0|1,arr.ind = T),xlab = "intervals between stops",ylab="seat number")
        points(row~col,data=which(vehicle!=0,arr.ind = T),pch=19)
        points(expand.grid(order[1]:order[2],free_seat),col="green",pch=19)
      }
  #We find how much we earned
      price_pos<-which(orders_distr$routes==paste(order[1], ":", (order[2]+1), sep=""))
      money<-money+as.numeric(orders_distr[price_pos,3])
      
  #We record the current state of how many passengers were sucesfully seated and not
      if(i>1){
        seated_people[i,]<-c(i, seated_people[j,2]+1,seated_people[j,3])
      }else{
        seated_people[i,]<-c(i,1,0)
      }
    }else{
      
      if (plot){
        plot(row~col,data=which(vehicle==0|1,arr.ind = T),xlab = "intervals between stops",ylab="seat number")
        points(row~col,data=which(vehicle!=0,arr.ind = T),pch=19)
        points(expand.grid(order[1]:order[2],1),col="red",pch=19)
      }
      
      if(i>1){  
        seated_people[i,]<-c(i, seated_people[j,2],seated_people[j,3]+1)
      }else{
        seated_people[i,]<-c(i,0,1)
      }
    }
  }
  
  #Now make measurements and pass the results
  seated_people$effectivity<-(seated_people$not_seated/seated_people$seated)
  effective_breaking_point<-which.min(seated_people$effectivity<0.1)
  util<-sum(vehicle!=0)/(seats*stops)
  empty_seats<-colSums(vehicle==0)
  
  test_results<-list(
    "seat_order"=seated_people,
    "break_point"=effective_breaking_point,
    "utilization"=util,
    "empty_seats"=empty_seats,
    "earned"=money
  )
  
  #seated_people$effectivity
  
}

#Function that performs multiple tests and returns the results as a list
perform_test<-function(count,seats=inputs$seats,stops=inputs$stops,distr=orders_distr,method=find_seat_orig){
  
  result<-data.frame()
   output<-replicate(count,c(result,fill_vehicle(seats,stops,orders_sample,method,reorder=TRUE)))
   output<-as.data.frame(t(output))
}


