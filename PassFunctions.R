##This function creates the orders sample

# Args:
#   routes - the possible route combinations
#   size - the size of the sample
#   probability - the probability of occurence of each sample
#
# Output: a list of all possible passengers orders from stop 1 to stop to

create.orders <- function(size=sample.size) {
  orders.sample <- list(mode = "numeric", length = sample.size)
  sample(
    route.orders.distr$routes,
    size = size,
    replace = TRUE,
    prob = route.orders.distr$prob
  ) %>%
    strsplit(":") %>% map(as.numeric) %>%
    #This change will revert the stops to intervals eg instead of seat 1,2 to routes 1-2,2-3
    map(function(x) {
      c(x[1], x[2] - 1)
    })
  
}

#Shows the sistribution of a given or random sample
plot.orders<-function(orders=NULL){
  if(is.null(orders)){orders<-create.orders()}
  orders_tbl<-table(paste(map(orders,1),":",map(orders,2),sep=""))
  return(barplot(height = orders_tbl,  ylab = "Number of Orders", xlab = "Route"))
}


#This function checks which seats are available for the given order and returns their position or returns false if no free seats are available
find.free.seats <- function(vehicle, order) {
  #turn the seats in bool values
  free_positions <-
    if(length(order[1]:order[2]) > 1) { rowSums(vehicle[, order[1]:order[2]]) == 0} else {vehicle[, order[1]:order[2]] == 0}
    
  free_seats<-which(free_positions)
  
  #This will return False if no seats are available otherwise it will return the unique rows where is possible to seat someone
  
    ifelse(
      length(free_seats) == 0,
      FALSE ,
      ifelse (
        length(free_seats) == 1,
        free_seats,
        match(data.frame(t(unique(vehicle[free_seats, ]))), data.frame(t(vehicle)))))
}

#This function mimics the original algoritm
assign.seat<-function(vehicle,order){
  #It looks for empty seats
  free_seats<-find.free.seats(vehicle,order)
  #If there are empty seats it returns the first one 
  ifelse (is.numeric(free_seats),seat<-free_seats[1],FALSE)
  }
  

##Probability loss algoritm

#This function takes an interval and calculates the according probabilty sum

# Args:
#   interval - the sequence when the seat is empty
#   probability.matrix - the frobability that overlapping orders will happen
#
# Output: a list of all possible passengers orders from stop 1 to stop to
sum.probability <- function(interval) {
  ifelse(all(is.na(interval)),
         1,
         intervals <-
           seqToIntervals(interval) %>% apply(1, function(x) {
             sum(probability.matrix[x[1]:x[2], x[1]:x[2]])
           }) %>% sum())
}

#Here we start the function that returns the best seat
assign.seat2<-function(vehicle,order){
  
  #Find out if there are free seats for this order
  free_seats<-find.free.seats(vehicle,order)
  
  if (is.numeric(free_seats)){
    
    #initialize some variables we need
    vehicle_inv<-t(matrix(data=1:ncol(vehicle),nrow=ncol(vehicle),ncol = nrow(vehicle)))
    vehicle_inv[vehicle!=1]<-NA
    
    prob_loss<-c()
    
    seatsBefore<-replace(vehicle_inv,vehicle_inv==0,NA)
    seatsAfter<-seatsBefore
    seatsAfter[,ord[1]:ord[2]]<-NA
    
    #here we calculate the probability loss
    for (i in seq_along(free_seats)){
      before<-sum.probability(seatsBefore[free_seats[i],])
      after<-sum.probability(seatsAfter[free_seats[i],])
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
fill.vehicle<-function(seats=vehicle.seats,stops=route.stops,size=sample.size,orders=NULL,method=assign.seat,plot=FALSE){
  
  #Create the default empty vehicle
  vehicle<-matrix(data=0,ncol=stops,nrow=seats)
  
  if(is.null(orders)){ orders<-create.orders(size)}
  
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
      price_pos<-which(route.orders.distr==paste(order[1], ":", (order[2]+1), sep=""))
      money<-money+as.numeric(route.orders.distr[price_pos,3])
      
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
    #"seat_order"=seated_people,
    "break_point"=effective_breaking_point,
    "utilization"=util,
    "empty_seats"=empty_seats,
    "earned"=money
  )
  
}
