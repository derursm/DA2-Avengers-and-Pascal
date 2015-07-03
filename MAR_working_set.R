#MAR_working_set

#Retrieve the Value where 10 hotels have a bigger distances to the cathedral: 
#evaluating feature X099_DistanceCathedralLeSeu

i<-0.5
hotels<-nrow(working_set)
while(hotels>10){
  i<-i+0.5
  hotels<-sum(working_set$X099_DistanceCathedralLeSeu>i)
}

print(i) #check the distance measure for X099_DistanceCathedralLeSeu

mar_working_set<-working_set
mar_working_set[which(working_set$X099_DistanceCathedralLeSeu>i),c("X003_Postcode","X012_NoRooms")]<-NA

sum(is.na(mar_working_set))

View(mar_working_set)
save(mar_working_set,file="mar_working_set.Rdata")


#additional test
mar_test <- mar_working_set[which(is.na(mar_working_set$X012_NoRooms)),]
View(mar_test)

##### plotting the two features of MAR 
mar_na_postcode <- which(is.na(mar_working_set[,c("X003_Postcode")]))
mar_na_norooms <- which(is.na(mar_working_set[,c("X012_NoRooms")]))
mar_na_x <- as.numeric(working_set[mar_na_postcode,c("X003_Postcode")])
mar_na_y <- as.numeric(working_set[mar_na_norooms,c("X012_NoRooms")])

#saving the plot
setEPS()
postscript("mar_working_set_plot.eps")
plot(working_set$X003_Postcode,working_set$X012_NoRooms, xlab="X003_Postcode", ylab="X012_NoRooms", main="MAR WORKING SET", pch=21, cex=1)
dev.off()

