orig<-perform_test(10000)
new<-perform_test(10000,method = find_seat_probLoss)

print(mean(unlist(orig$utilization)))
print(mean(unlist(new$utilization)))
print(mean(unlist(orig$break_point)))
print(mean(unlist(new$break_point)))
print(mean(unlist(orig$earned)))
print(mean(unlist(new$earned)))

hist(unlist(orig$utilization))
hist(unlist(new$utilization))


system.time(perform_test(10))
system.time(perform_test(1000,method = find_seat_probLoss))

system.time(
for(i in 1:10){
  fill_vehicle(reorder = TRUE)
}
)

a<-perform_test(1)

output2<-replicate(10,c(result,fill_vehicle(method=find_seat_orig)))

a<-fill_vehicle(method=find_seat_orig)
