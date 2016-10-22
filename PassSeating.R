#Retrieve route passengers orders probability distribution
orders_distr<-read.csv("orderHist.csv",header=TRUE,colClasses = c("character","numeric"), sep=";")
barplot(height=orders_distr$prob, names.arg = orders_distr$route, ylab = "Probability", xlab = "Route")


#Initialize test variables
inputs<-list(
  seats=5,
  sample_size=5,
  stops=as.numeric(max(unlist(strsplit(orders_distr$route,":"))))-1
)

#Create random order sample according to given distribution
create_sample<-function(orders_distr=orders_distr,inputs=inputs,output="sample"){
  
  #Create sample from distribution    
  orders_sample<-sample(orders_distr$route, size=inputs$sample_size, replace=TRUE, prob=orders_distr$prob)
  
  #Return list of orders / or show sample
  if(output=="plot"){
    orders_tbl<-table(orders_sample)
    return(barplot(height = orders_tbl,  ylab = "Number of Orders", xlab = "Route"))
  }else{
    orders_list<-map(strsplit(orders_sample,":"),as.numeric)
    
    
  }
}

#This function checks which seats are available for the given order and returns their position or returns false if no free seats are available
is_free<-function(vehicle,order){
  #turn the seats in bool values
  free_positions<-vehicle!=0
  
  if (length(order)==1){ 
    free_seats<-which(free_positions[,order[1]]==0) 
  }else{
    free_seats<-which((free_positions[,order[1]:order[2]])==0)
  }
  
  if(length(free_seats)==0) FALSE else free_seats
  
}


#This function mimics the original algoritm

fill_vehicle_orig<-function(seats,stops,orders_distr, algoritm){
  
  #create the empty vehicle
  vehicle<-matrix(data=0,nrow=input$seats,ncol=input$stops)
  
  #create the order list
  orders<-create_sample(orders_distr)
  
  #fill the vehicle with people
  
  for (i in 1:seq_along(orders)){
    
    order<-orders[[i]]
    free_seats<-is_free(vehicle,order)
    
    if (!free_seats){vehicle[free_seats[1],order[1]:order[2]]
    }
  }
  
  
}

result<-c(result,sum(sedadla!=0)/(seats*stops))


result<-NULL
result2<-NULL
i<-NULL
j<-NULL
c<-NULL
c<-c+1
for (i in 1:1000){
  
  
  
  
  sedadla2<-matrix(data=0,nrow=122,ncol=8)
  
  
  
  
  for (objednavka2 in objednavky){
    objednavka<-as.numeric(objednavka2)
    j<-j+1
    if (sum(rowSums(sedadla2[,objednavka[1]:objednavka[2]])==0)>0){
      if (length(objednavka)==1){ 
        pozice<-which(sedadla2[,objednavka[1]:objednavka[1]]==0) }
      else{
        pozice<-which(rowSums(sedadla2[,objednavka[1]:objednavka[2]])==0)}
      if (length(pozice)!=1){
        sedadla3<-sedadla2
        sedadla3[pozice,objednavka[1]:objednavka[2]]<-1
        entropie<-rowSums(!(sedadla3[pozice,-8]==sedadla3[pozice,-1]))
        #entropie<-abs(rowSums(sedadla3[pozice,])-8)
        minEnt<-which.min(entropie)
        sedadla2[pozice[minEnt],objednavka[1]:objednavka[2]]<-1
      }
      else{
        sedadla2[pozice[1],objednavka[1]:objednavka[2]]<-1
      }
    }else{
      result2<-c(result2,sum(sedadla2)/(122*8))
      break}
  }
}

hist(result, breaks=50, main="Klasický model")
mainresult<-mean(result)
pocetLidi<-matrix(data=0,nrow=1,ncol=2)
pocetLidi[c,1]<-i
pocetLidi[c,2]<-j

hist(result2, breaks=50,main="Entropický model")
mainresult<-c(mainresult,mean(result2))