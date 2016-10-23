orig<-perform_test(100)
new<-perform_test(100,method = find_seat_probLoss)

print(mean(unlist(orig$utilization)))
print(mean(unlist(new$utilization)))
print(mean(unlist(orig$break_point)))
print(mean(unlist(new$break_point)))
print(mean(unlist(orig$earned)))
print(mean(unlist(new$earned)))

hist(unlist(orig$utilization))
hist(unlist(new$utilization))


a<-fill_vehicle(orders=route_orders,method=find_seat_probLoss)
b<-fill_vehicle(orders=route_orders)

a$utilization-b$utilization
a$earned-b$earned

##1000 trials
orig2<-perform_test(1000)
new2<-perform_test(1000,method = find_seat_probLoss)

print(mean(unlist(orig2$utilization)))
print(mean(unlist(new2$utilization)))
print(mean(unlist(orig2$break_point)))
print(mean(unlist(new2$break_point)))
print(mean(unlist(orig2$earned)))
print(mean(unlist(new2$earned)))

hist(unlist(orig2$utilization))
hist(unlist(new2$utilization))
