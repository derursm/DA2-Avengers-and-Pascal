source("DA2_avengers_require.R")
#source("DA2_avengers_install.R")

#first contact with the data
## Idea to load dataset directly from GIT: 
#install.packages("devtools")
#require(devtools)
#source_gist("??????")      (replace id)

#Create missing values with the MCAR,MAR and MNAR approach
data(output_all) #data set from group one 
is.na(output_all) #analysis of NA values

#Analysis of the data set for the best output 
output_all[complete.cases(output_all[which(output_all$Tripadvisor),names(output_all)]),names(output_all)]

sum(output_all$Tripadvisor[which(!is.na(output_all$Tripadvisor))])
sum(output_all$Booking[which(!is.na(output_all$Booking))])
sum(output_all$Expedia[which(!is.na(output_all$Expedia))])


#Compute mean of the count of NA values per feature to decide which features to choose
summe<-0
for (i in 1:103){
  summe<-summe+sum(is.na(output_all[,i]))
  mean_na<-summe/length(names(output_all))
}
mean_na

na_count<-c()
for (i in 1:length(names(output_all))){
  na_count<-c(na_count,sum(is.na(output_all[,i])))
}

#=== Splitting the data set 'output_all' ===#
tripadvisor_noNA <- output_all[complete.cases(output_all[which(output_all$Tripadvisor),names(output_all)]),names(output_all)]
expedia_noNA <- output_all[complete.cases(output_all[which(output_all$Expedia),names(output_all)]),names(output_all)]
booking_noNA <- output_all[complete.cases(output_all[which(output_all$Booking),names(output_all)]),names(output_all)]


##### proof of potential valuable observations
test_tripadvisor <- which(output_all[,c("Tripadvisor")])
test_expedia <- which(output_all[,c("Expedia")])
test_booking <- which(output_all[,c("Booking")])

length(test_tripadvisor)
length(test_expedia)
length(test_booking)
######

#####tripadvisor data to work with
tripadvisor<-output_all[which(output_all$Tripadvisor),names(output_all)]
#####
#======#

#creating the vector of features to choose
urs_decision<-which(na_count<mean_na)

#creating the working_set for DA2-Avengers with all choosen features and observations
result_decision<-tripadvisor[urs_decision]
#result_expedia_decision <- expedia[urs_decision]
#result_booking_decision <- booking[urs_decision]

#Select only observations which don't contain NAs in any feature
working_set <-result_decision[which(complete.cases(result_decision)),names(result_decision)]
#str(test_expedia) <- result_expedia_decision[which(complete.cases(result_expedia_decision)),names(result_expedia_decision)]
#str(test_booking) <- result_booking_decision[which(complete.cases(result_booking_decision)),names(result_booking_decision)]

#####finally we will choose tripadvisor 
working_set_backup<-working_set


#Set empty Strings as NA
for (i in 1:ncol(working_set_backup)){
  working_set_backup[which(working_set_backup[,i]==""),i]<-NA
}

#Select observations which don't have empty strings
new_working_set<-working_set_backup[complete.cases(working_set_backup),names(working_set_backup)]

working_set<-new_working_set

#=== Outlier detection ===#   
plot(working_set_backup$X102_DistanceToBeach)
distbeach_var <- var(working_set_backup$X102_DistanceToBeach)
chisq.out.test(working_set_backup$X102_DistanceToBeach, distbeach_var, opposite=F) #require("outliers")

working_set$X102_DistanceToBeach[which(working_set$X102_DistanceToBeach==288.21221210)] <- 0.35928

working_set$X102_DistanceToBeach[which(working_set$X102_DistanceToBeach==0.35928)]

plot(working_set$X102_DistanceToBeach)

##### Check if replacement worked
which(working_set$X102_DistanceToBeach==288.21221210, arr.ind=T) #checking if the old value has still an index 
which(working_set$X102_DistanceToBeach==0.35928, arr.ind=T) #getting the index of the value 
#####
#======#

##### importing the beach location
locationBeachURL <- getURL("https://raw.githubusercontent.com/derursm/DA2-Avengers-and-Pascal/master/X103LocationBeach.csv")
locationBeach <- read.csv(text=locationBeachURL)
working_set[, "X103_LocationBeach"] <- locationBeach
#####


#=== Cast character to factor where plausible ===#
working_set$X003_Postcode<-as.factor(working_set$X003_Postcode)
working_set$X004_City<-as.factor(working_set$X004_City)
working_set$X006_Country<-as.factor(working_set$X006_Country)
working_set$X014_CheckIn<-as.factor(working_set$X014_CheckIn)
working_set$X046_PayOptions<-as.factor(working_set$X046_PayOptions)
working_set$X103_LocationBeach<-as.factor(working_set$X103_LocationBeach)
#======#

#=== Split and sort the X046_PayOptions feature ===#
working_set_split <- working_set

payoption_split_df <- strsplit(as.character(working_set_split$X046_PayOptions),split=";") 
payoption_split_sort <- lapply(payoption_split_df,trim) # We asume that "EC Card" and "Maestro" are different payment options
payoption_split_sort <- lapply(payoption_split_sort,sort)
payoption_column<-sapply(payoption_split_sort,function(x) paste(x,collapse="; "))
working_set_sort_payOption<-working_set
working_set_sort_payOption$X046_PayOptions<-payoption_column
save(working_set_sort_payOption,file="working_set_sort_payOption.Rdata")
#======#

#=== Save working_set to environment ===#
save(working_set,file="working_set.Rdata")
#======#


