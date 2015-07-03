#MNAR_working-set; Steps taken in here:

#Delete Values based on the combination of features X100_DistanceAlcudiaOldTown and X046_PayOptions
#The combination ist build by a linear combination of the two features, therefore a numerical representation 
#of the X046_PayOptions feature x is added with the X100_DistanceAlcudiaOldTown feature y (result=num_hash(x)+y)

#The numiercal hash is build by representing every character of a string by their ASCII code and after that sum the ascii codes.
#The sum of the ASCII codes are devided by the length of the string to eliminate differences in length of the string 
#and to get smaller values. Smaller values have the advantage that the two features have nearly the same influence on the endresult. 
#The 10 NA-values are deleted at the 10 biggest values of the sum of two features.


#Function to get the ASCII representation of a character
asc<- function(x) strtoi(charToRaw(x),16L)

#Function to get the sum of ASCII representations of characters devided by the length of a string
hash<-function(x){
  x<-as.character(x)
  char_array<-strsplit(x,"")[[1]]
  return(sum(sapply(char_array,asc))/nchar(x))
}

#vector of the sums of the hash representation and the numerical feature
sum_hash_vec_distance<-sapply(working_set$X046_PayOptions,hash)+working_set$X100_DistanceAlcudiaOldTown

#create copy of our working_set 
mnar_working_set<-working_set

#Delete values in X100_DistanceAlcudiaOldTown and X046_PayOptions where the sum of the hash and the numerical feature are the 10 biggest
mnar_working_set[which(sum_hash_vec_distance %in% sort(sum_hash_vec_distance)[1:10]),c("X100_DistanceAlcudiaOldTown","X046_PayOptions")]<-NA


#additional test
mnar_test <- mnar_working_set[which(is.na(mnar_working_set$X100_DistanceAlcudiaOldTown)),]
View(mnar_test)
sum(is.na(mnar_test))

View(mnar_working_set)
save(mnar_working_set,file="mnar_working_set.Rdata")


##### plotting the two features of MNAR 
mnar_na_distalcudia <- which(is.na(mnar_working_set[,c("X100_DistanceAlcudiaOldTown")]))
mnar_na_payoptions <- which(is.na(mnar_working_set[,c("X046_PayOptions")]))
mnar_na_x <- working_set[mnar_na_distalcudia,c("X100_DistanceAlcudiaOldTown")]
mnar_na_y <- as.factor(working_set[,c("X046_PayOptions")])[mnar_na_payoptions]

#saving the plot
setEPS()
postscript("mnar_working_set_plot.eps")
plot(working_set$X100_DistanceAlcudiaOldTown,as.factor(working_set$X046_PayOptions), xlab="X100_DistanceAlcubdiaOldTown", ylab="X046_PayOptions", main="MNAR WORKING SET", pch=21, cex=1)
points(x=mnar_na_x, y=mnar_na_y,pch=1,cex=0.5,col="red", lwd=3)
dev.off()


#=== MNAR with working_set_sort_payOption ===#
##### mnar of the working set with sorted payoption 
sum_hash_vec_distance<-sapply(working_set_sort_payOption$X046_PayOptions,hash)+working_set_sort_payOption$X100_DistanceAlcudiaOldTown
mnar_working_set_sort_payoption<-working_set_sort_payOption
mnar_working_set_sort_payoption[which(sum_hash_vec_distance %in% sort(sum_hash_vec_distance)[1:10]),c("X100_DistanceAlcudiaOldTown","X046_PayOptions")]<-NA
#####tests
sum(is.na(mnar_working_set_sort_payoption))
par(mfcol=c(2,1))
barplot(sort(table(mnar_working_set_sort_payoption$X046_PayOptions)),las=2)
#####
save(mnar_working_set_sort_payoption,file="mnar_working_set_sort_payoption.Rdata")
#####
#======#
