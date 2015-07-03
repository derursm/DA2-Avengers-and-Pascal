#MCAR for 10 observations 

#writing the MCAR function to delete 10 observations
MCAR <- function(data, column_name, value.delete = 10, setseed=FALSE){
  # introduce MCAR in a dataset
  if(value.delete<=0)
    stop("the number of deleting values needs to be positive")
  if(sum(is.na(data))!=0)
    stop("the data matrix in entry should be complete")

  ifelse(setseed,source("Seed.R"),NA)                                              #set the seed via Seed.R
  del_index<-sample(nrow(data),value.delete)                                       #creating the index to delete 
  data[del_index,column_name]<-NA                                                  #delete the observations from the feature
  return(data)
}

working_set_mcar<-MCAR(working_set,c("X102_DistanceToBeach","X004_City"),10,setseed=T)   #running MCAR for the numerical feature "X102_DistanceToBeach"

#visual check of MCAR 
View(working_set_mcar)

#merge the MCAR data 
mcar_working_set <- working_set_mcar

sum(is.na(mcar_working_set))                                  #check the number of NA values
View(mcar_working_set)                                        #visual check of the MCAR NA values
save(mcar_working_set,file="mcar_working_set.Rdata")

#additional test
mcar_test <- mcar_working_set[which(is.na(mcar_working_set$X004_City)),]
View(mcar_test)


##### plotting the two features of MCAR 
mcar_na_distbeach <- which(is.na(mcar_working_set[,c("X102_DistanceToBeach")]))
mcar_na_city <- which(is.na(mcar_working_set[,c("X004_City")]))
mcar_na_x <- working_set[mcar_na_distbeach,c("X102_DistanceToBeach")]
mcar_na_y <- as.factor(working_set[,c("X004_City")])[mcar_na_city]

#saving the plot
setEPS()
postscript("mcar_working_set_plot.eps")
plot(working_set$X102_DistanceToBeach, as.factor(working_set$X004_City), xlab="X102_DistanceToBeach", ylab="X004_City", main="MCAR WORKING SET", pch=21, cex=1)
points(x=mcar_na_x, y=mcar_na_y,pch=1,cex=0.5,col="red", lwd=3)
dev.off()

#=== MCAR with working_set_sort_payOption ===#
##### mcar of the working set with sorted payoption 
mcar_working_set_sort_payoption<-MCAR(working_set_sort_payOption,c("X102_DistanceToBeach","X004_City"),10,setseed=T)   #running MCAR for the numerical feature "X102_DistanceToBeach"
sum(is.na(mcar_working_set_sort_payoption))
save(mcar_working_set_sort_payoption,file="mcar_working_set_sort_payoption.Rdata")
#####

#======#
##=== START: MCAR for X103_LocationBeach (factor) & X102_DistanceToBeach (numeric)===

mcar_working_set_lb<-MCAR(working_set,c("X102_DistanceToBeach","X103_LocationBeach"),10,setseed=T)

sum(is.na(mcar_working_set_lb))                                  #check the number of NA values
View(mcar_working_set_lb)                                        #visual check of the MCAR NA values
save(mcar_working_set_lb,file="mcar_working_set_lb.Rdata")

#additional test
mcar_test_lb <- mcar_working_set_lb[which(is.na(mcar_working_set_lb$X103_LocationBeach)),]
View(mcar_test_lb)


##### plotting the two features of MCAR 
mcar_na_distbeach <- which(is.na(mcar_working_set_lb[,c("X102_DistanceToBeach")]))
mcar_na_locbeach <- which(is.na(mcar_working_set_lb[,c("X103_LocationBeach")]))
mcar_na_x <- working_set[mcar_na_distbeach,c("X102_DistanceToBeach")]
mcar_na_y <- as.factor(working_set[,c("X103_LocationBeach")])[mcar_na_locbeach]

#saving the plot
setEPS()
postscript("mcar_working_set_lb_plot.eps")
dotplot(working_set$X102_DistanceToBeach~working_set$X103_LocationBeach, xlab="X102_DistanceToBeach", ylab="X103_LocationBeach", main="MCAR LB WORKING SET", pch=19, cex=1,col=c("blue","red"), groups=is.na(mcar_working_set$X102_DistanceToBeach))
dev.off()


