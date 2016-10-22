#Libraries
#install.packages("purrr")
library(purrr)


#Initialize test variables
inputs<-list(
  seats=5,
  sample_size=5,
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

#Create random order sample according to given distribution
create_sample_orders<-function(routes=orders_distr$route,probability=orders_distr$prob,inp=inputs,output="sample"){

#Create sample from distribution    
orders_sample<-sample(routes, size=inp$sample_size, replace=TRUE, prob=probability)

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

#This function checks which seats are available for the given order and returns their position or returns false if no free seats are available
is_free<-function(vehicle,order){
  #turn the seats in bool values
  free_positions<-rowSums(vehicle[,order[1]:order[2]])==0
  
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


#tests the choden algoritm and returns the results
fill_vehicle<-function(seats=inputs$seats,stops=inputs$stops,distr=orders_distr,find_seat=find_seat_orig){
  
  #create the empty vehicle
  vehicle<-matrix(data=0,ncol=stops,nrow=seats)
  
  #create the order list
  orders<-create_sample_orders()
  
  #create the result variables
  sitting_plot<-c()
  seated_people<-data.frame(data=0,nrow=stops,ncol=3)
  names(seated_people) <- c("order","seated","not_seated")
  result<-c()
  
  #takes the orders ans sits people by the chosen method
  for (i in seq_along(orders)) {
    order<-unlist(orders[i])
    j<-i-1
    free_seat<-find_seat(vehicle,order)
    if (is.numeric(free_seat)){
      vehicle[free_seat,order[1]:order[2]]<-i
      
      plot(row~col,data=which(vehicle==0|1,arr.ind = T),xlab = "intervals between stops",ylab="seat number")
      points(row~col,data=which(vehicle!=0,arr.ind = T),pch=19)
      points(expand.grid(order[1]:order[2],free_seat),col="green",pch=19)
      
      sitting_plot<-c(recordPlot())
      if(i>1){
        seated_people[i,]<-c(i, seated_people[j,2]+1,seated_people[j,3])
      }else{
        seated_people[i,]<-c(i,1,0)
      }
    }else{
      
      if(i>1){  
        seated_people[i,]<-c(i, seated_people[j,2],seated_people[j,3]+1)
      }else{
        seated_people[i,]<-c(i,0,1)
      }
    }
  }
  
  #Now make measurements and pass the results
  seated_people$effectivity<-(seated_people$seated/seated_people$order)
  effective_breaking_point<-which.min(seated_people$effectivity>0,5)
  util<-sum(vehicle!=0)/(seats*stops)
  
  test_results
  <-list(
                seat_order=seated_people,
                break_point=effective_breaking_point,
                vehicle=vehicle,
                plots=sitting_plot,
                utilization=util
                )
  
}



  
  
 