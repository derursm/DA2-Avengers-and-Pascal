source("DA2_avengers_require.R")
source("Seed.R")

## === START: MCAR Data preparation ===

mcar_class_set = mcar_working_set
# reduce size of dataset for classification and persue classification for cities based on distances:
mcar_class_set = mcar_class_set[, c("X003_Postcode","X004_City","X010_Lon", "X011_Lat", "X012_NoRooms", "X099_DistanceCathedralLeSeu", "X100_DistanceAlcudiaOldTown", "X098_DistanceSerraDeTramuntana")]

# Having Postcode as numeric proved as a more performant solution than having a factor w/ a huge amount of levels
# We can assume Postcode and NoRooms of being numeric, because there is a dependency between the single levels
# e.g. the area of postcode 7540 is closer to 7541 than 7000
mcar_class_set[ , "X003_Postcode"] <- as.numeric(mcar_class_set[ , "X003_Postcode"])
mcar_class_set[ , "X012_NoRooms"] <- as.numeric(mcar_class_set[ , "X012_NoRooms"])

#Updating the levels. Otherwise error: Can't have empty classes in y 
mcar_class_set[ , "X004_City"] <- as.character(mcar_class_set[ , "X004_City"])
mcar_class_set[ , "X004_City"] <- as.factor(mcar_class_set[ , "X004_City"])

#Copying the original dataset (for means of error calculation later on)
class_working_set <- working_set
class_working_set[ , "X004_City"] <- as.character(class_working_set[ , "X004_City"])

#Drop NA rows for model creation
mcar_class_set_woNA <- na.omit(mcar_class_set) 

# Creating an index to identify the missing values
mcar_class_set_names <- colnames(mcar_class_set)
IndexOfMissing_city <- which(is.na(mcar_class_set$X004_City))
str(mcar_class_set_woNA)
str(mcar_class_set)
## === END: MCAR Data preparartion ===


##=== START: Classification via Tree for MCAR=== 
tr_class_MCAR = rpart(X004_City ~ ., data=mcar_class_set_woNA)

#####Predictions
mcar_tree_pred_all = predict(tr_class_MCAR, mcar_class_set[,!(mcar_class_set_names %in% "X004_City")], type = "class")
mcar_tree_pred_all_error <- mean(as.character(mcar_tree_pred_all) != as.character(class_working_set$X004_City)) #NA, in case of missing values

mcar_tree_pred_ten = predict(tr_class_MCAR, mcar_class_set[IndexOfMissing_city,!(mcar_class_set_names %in% "X004_City")], type = "class")
mcar_tree_pred_ten_error <- mean(as.character(mcar_tree_pred_ten) != as.character(class_working_set$X004_City[IndexOfMissing_city])) #NA, in case of missing values

#== Some further checks:
cbind(as.character(mcar_tree_pred_ten),as.character(class_working_set$X004_City[IndexOfMissing_city]))
#==

#== Plotting:
setEPS()
postscript("Cla_City_Imputed_mcar_Tree.eps")
par(mar=c(1,3,5,0.2))
plot(tr_class_MCAR, main="MCAR Classification Tree for City", sub=paste("Prediction Error for all: ",round(mcar_tree_pred_all_error,4),"                          Prediction error for ten: ",round(mcar_tree_pred_ten_error,4)))
text(tr_class_MCAR, pretty = 0, cex=0.8)
dev.off()

##### City vs. Distance to Beach
#Plot of original values of City and Ditance to Beach, the colored points indicate the values which are deleted with the MCAR method
setEPS()
postscript("Class_MCAR_City_DistB_org.eps")
dotplot(class_working_set$X004_City~class_working_set$X102_DistanceToBeach, pch=19,main="City vs. DistanceToBeach",col=c("blue","red"),groups=is.na(mcar_class_set$X004_City))
dev.off()
#In comparison to the plot before, now the imputed data of the feature city is used. The classification predicts less cities. 
# A possible reason for this observation could be that in some cities only a few hotels are located and therefore can not be predicted.
setEPS()
postscript("Class_MCAR_City_DistB_pred.eps")
dotplot(mcar_tree_pred_all~class_working_set$X102_DistanceToBeach, pch=19,main="City vs. DistanceToBeach",col=c("blue","red"),groups=is.na(mcar_class_set$X004_City))
dev.off()
#==

##=== END: Classification via Tree ===
# Interesting: Setting the factorial feature "postcode" as numerical brings an advantage of 0.1


##=== START: Classification via Forest for MCAR===

mcar_forest_class = randomForest(X004_City ~ ., data=mcar_class_set_woNA, ntree=50)

mcar_forest_pred_all = predict(mcar_forest_class, mcar_class_set[,!(mcar_class_set_names %in% "X004_City")], type = "class") 
mcar_forest_pred_all_error <- mean(as.character(mcar_forest_pred_all) != as.character(class_working_set$X004_City)) #NA, in case of missing values

mcar_forest_pred_ten = predict(mcar_forest_class, mcar_class_set[IndexOfMissing_city,!(mcar_class_set_names %in% "X004_City")], type = "class") 
mcar_forest_pred_ten_error <- mean(as.character(mcar_forest_pred_ten) != as.character(class_working_set$X004_City[IndexOfMissing_city])) #NA, in case of missing values

#== Some further checks:
cbind(as.character(mcar_forest_pred_ten),as.character(class_working_set$X004_City[IndexOfMissing_city]))
#==

#== Plotting:
setEPS()
postscript("Cla_City_Imputed_mcar_Forest.eps")
varImpPlot(mcar_forest_class, main="Classification via Forest for City", sub=paste("Prediction Error for all: ",round(mcar_forest_pred_all_error,4),"                          Prediction error for ten: ",round(mcar_forest_pred_ten_error,4))) # Interpretation necessary
dev.off()

##=== END: Classification via Forest ===#

## === START: MAR Data preparation ===
# reduce size of dataset for classification and persue classification for postcode based on distances:
mar_class_set = mar_working_set[, c("X003_Postcode", "X010_Lon", "X011_Lat", "X102_DistanceToBeach", "X099_DistanceCathedralLeSeu", "X100_DistanceAlcudiaOldTown", "X098_DistanceSerraDeTramuntana")]

#Updating the levels. Otherwise error: Can't have empty classes in y 
mar_class_set[,"X003_Postcode"] <- as.character(mar_class_set$X003_Postcode)
mar_class_set[,"X003_Postcode"] <- as.factor(mar_class_set$X003_Postcode)

# Creating an index to identify the missing values
mar_class_set_names <- colnames(mar_class_set)
IndexOfMissing_postcode <- which(is.na(mar_class_set$X003_Postcode))

#Drop NA rows for model creation
mar_class_set_woNA <- na.omit(mar_class_set) 

## === END: MAR Data preparartion ===

##=== START: Classification via Tree for MAR=== 
tr_class_MAR = rpart(X003_Postcode ~ ., data=mar_class_set_woNA)

#####Predictions
mar_tree_pred_all <- predict(tr_class_MAR, mar_class_set[,!(mar_class_set_names %in% "X003_Postcode")], type="class")
mar_tree_pred_all_error <- mean(as.character(mar_tree_pred_all) != as.character(class_working_set$X003_Postcode))  #NA, in case of missing values

mar_tree_pred_ten <- predict(tr_class_MAR, mar_class_set[IndexOfMissing_postcode,!(mar_class_set_names %in% "X003_Postcode")], type="class")
mar_tree_pred_ten_error <- mean(as.character(mar_tree_pred_ten) != as.character(class_working_set$X003_Postcode[IndexOfMissing_postcode]))  #NA, in case of missing values

#== Some further checks:
cbind(as.character(mar_tree_pred_ten),as.character(class_working_set$X003_Postcode[IndexOfMissing_postcode]))
#==

#== Plotting:
setEPS()
postscript("Cla_Postcode_Imputed_mar_Tree.eps")
plot(tr_class_MAR, main="MAR Classification Tree for Postcode", sub=paste("Prediction Error for all: ",round(mar_tree_pred_all_error,4),"Prediction error for ten: ",round(mar_tree_pred_ten_error,4)))
text(tr_class_MAR, pretty = 0, cex=0.8)
dev.off()

##=== END: Classification via Tree ===
# Some remarks to the result:
# Obviously the error is very high (1). This is due the fact that the deletion depends on 
# X099_DistanceCathedralLeSeu which indirectly depends on the postcode as well
# Briefly said: we only test one "real" observation
# This is not imputed correctly 
# --> So the error = 1
# But: "7589" is really close to "7590" --> the method isn't as bad as it seems

##=== START: Classification via Forest for MAR===

mar_forest_class = randomForest(X003_Postcode ~., data=mar_class_set_woNA, ntree = 50)

mar_forest_pred_all = predict(mar_forest_class, mar_class_set[,!(mar_class_set_names %in% "X003_Postcode")], type = "class") 
mar_forest_pred_all_error <- mean(as.character(mar_forest_pred_all) != as.character(class_working_set$X003_Postcode))

mar_forest_pred_ten <- predict(mar_forest_class, mar_class_set[IndexOfMissing_postcode,!(mar_class_set_names %in% "X003_Postcode")], type = "class") 
mar_forest_pred_ten_error <- mean(as.character(mar_forest_pred_ten) != as.character(class_working_set$X003_Postcode[IndexOfMissing_postcode]))

#== Some further checks:
cbind(as.character(mar_forest_pred_ten),as.character(class_working_set$X003_Postcode[IndexOfMissing_postcode]))
#==

#== Plotting:
setEPS()
postscript("Cla_Postcode_Imputed_mar_Forest.eps")
varImpPlot(mar_forest_class, main="Classification via Forest for Postcode", sub=paste("Prediction Error for all: ",round(mar_forest_pred_all_error,4),"                          Prediction error for ten: ",round(mar_forest_pred_ten_error,4))) # Interpretation necessary) # Interpretation necessary
dev.off()
#==

##=== END: Classification via Forest ===


## === START: MNAR Data preparation ===
# reduce size of dataset for classification and persue classification for Pay Options based on distances
mnar_class_set = mnar_working_set[, c("X046_PayOptions", "X012_NoRooms", "X102_DistanceToBeach", "X099_DistanceCathedralLeSeu", "X098_DistanceSerraDeTramuntana")]

# Set Pay Options as factor since we want to use it for classification
mnar_class_set[ , "X046_PayOptions"] <- as.factor(mnar_class_set[ , "X046_PayOptions"])

#Updating the levels. Otherwise error: Can't have empty classes in y 
mnar_class_set[ , "X046_PayOptions"] <- as.character(mnar_class_set[ , "X046_PayOptions"])
mnar_class_set[ , "X046_PayOptions"] <- as.factor(mnar_class_set[ , "X046_PayOptions"])

# Creating an index to identify the missing values
mnar_class_set_names <- colnames(mnar_class_set)
IndexOfMissing_PayOptions <- which(is.na(mnar_class_set$X046_PayOptions))

# Drop NA rows for model creation
mnar_class_set_woNA <- na.omit(mnar_class_set)

## === END: MNAR Data preparartion ===

##=== START: Classification via Tree for MNAR=== 
tr_class_MNAR = rpart(X046_PayOptions ~ ., data=mnar_class_set_woNA)

mnar_tree_pred_all = predict(tr_class_MNAR, mnar_class_set[,!(mnar_class_set_names %in% "X046_PayOptions")], type = "class")
mnar_tree_pred_all_error <- mean(as.character(mnar_tree_pred_all) != as.character(class_working_set$X046_PayOptions))

mnar_tree_pred_ten = predict(tr_class_MNAR, mnar_class_set[IndexOfMissing_PayOptions,!(mnar_class_set_names %in% "X046_PayOptions")], type = "class")
mnar_tree_pred_ten_error <- mean(as.character(mnar_tree_pred_ten) != as.character(class_working_set$X046_PayOptions[IndexOfMissing_PayOptions])) 


#== Some further checks:
cbind(as.character(mnar_tree_pred_ten),as.character(class_working_set$X046_PayOptions[IndexOfMissing_PayOptions]))
#==

#== Plotting:
setEPS()
postscript("Cla_PayOptions_Imputed_mnar_Tree.eps")
plot(tr_class_MNAR, main="MNAR Classification Tree for Pay Options", sub=paste("Prediction Error for all: ",round(mnar_tree_pred_all_error,4),"Prediction error for ten: ",round(mnar_tree_pred_ten_error,4)))
text(tr_class_MNAR, pretty = 0, cex=0.8)
dev.off()

##=== END: Classification via Tree ===
# Some remarks to the result:
# Bad overall result is not surprising --> There's no real sense in trying to determine payoptions by distances
# Additionally - even if we assume remote hotels or small hotels have less payoptions - the payoptions have to be matched exactly. This is very unlikely
# This is why we added a classification with sorted pay options later


##=== START: Classification via Forest for MNAR=== 
mnar_forest_class = randomForest(X046_PayOptions ~ ., data=mnar_class_set_woNA, ntree = 50, na.action=na.omit)

mnar_forest_pred_all = predict(mnar_forest_class, mnar_class_set[,!(mnar_class_set_names %in% "X046_PayOptions")], type = "class")
mnar_forest_pred_all_error <- mean(as.character(mnar_forest_pred_all) != as.character(class_working_set$X046_PayOptions)) 

mnar_forest_pred_ten = predict(mnar_forest_class, mnar_class_set[IndexOfMissing_PayOptions,!(mnar_class_set_names %in% "X046_PayOptions")], type = "class")
mnar_forest_pred_ten_error <- mean(as.character(mnar_forest_pred_ten) != as.character(class_working_set$X046_PayOptions[IndexOfMissing_PayOptions]))

#== Some further checks:
cbind(as.character(mnar_forest_pred_ten),as.character(class_working_set$X046_PayOptions[IndexOfMissing_PayOptions]))
#==

#== Plotting:
setEPS()
postscript("Cla_PayOptions_Imputed_mnar_Forest.eps")
varImpPlot(mnar_forest_class, main="Classification via Forest for Pay Options", sub=paste("Prediction Error for all: ",round(mnar_forest_pred_all_error,4),"                          Prediction error for ten: ",round(mnar_forest_pred_ten_error,4)))
dev.off()
##=== END: Classification via Forest ===#


## === START: MNAR Data preparation with working_set_sort_payOption ===#
# Checking if necessary datasets are available
str(working_set_sort_payOption)
str(mnar_working_set_sort_payoption)

# Copying the original data for means of error calculation
class_set_payoption <- working_set_sort_payOption

# reduce size of dataset for classification and persue classification for Pay Options based on distances
mnar_class_set_payoption <- mnar_working_set_sort_payoption[, c("X046_PayOptions", "X012_NoRooms", "X102_DistanceToBeach", "X099_DistanceCathedralLeSeu", "X098_DistanceSerraDeTramuntana")]

#Updating the levels. Otherwise error: Can't have empty classes in y 
mnar_class_set_payoption[ , "X046_PayOptions"] <- as.character(mnar_class_set_payoption[ , "X046_PayOptions"])
mnar_class_set_payoption[ , "X046_PayOptions"] <- as.factor(mnar_class_set_payoption[ , "X046_PayOptions"])

# Creating an index to identify the missing values
mnar_class_set_payoption_names <- colnames(mnar_class_set_payoption)
IndexOfMissing_PayOptions <- which(is.na(mnar_class_set_payoption$X046_PayOptions))

# Drop NA rows for model creation
mnar_class_set_payoption_woNA <- na.omit(mnar_class_set_payoption) 

## === END: MNAR Data preparartion ===#


##=== START: Classification via Tree for MNAR sort_payOption ===#
tr_class_sort_payoption_MNAR = rpart(X046_PayOptions ~ ., data=mnar_class_set_payoption_woNA)

mnar_tree_payoption_pred_all = predict(tr_class_sort_payoption_MNAR, mnar_class_set_payoption[,!(mnar_class_set_payoption_names %in% "X046_PayOptions")], type = "class")
mnar_tree_payoption_pred_all_error <- mean(as.character(mnar_tree_payoption_pred_all) != as.character(class_set_payoption$X046_PayOptions))

mnar_tree_payoption_pred_ten = predict(tr_class_sort_payoption_MNAR, mnar_class_set_payoption[IndexOfMissing_PayOptions,!(mnar_class_set_payoption_names %in% "X046_PayOptions")], type = "class")
mnar_tree_payoption_pred_ten_error <- mean(as.character(mnar_tree_payoption_pred_ten) != as.character(class_set_payoption$X046_PayOptions[IndexOfMissing_PayOptions])) 


#== Some further checks:
cbind(as.character(mnar_tree_payoption_pred_ten),as.character(class_working_set$X046_PayOptions[IndexOfMissing_PayOptions]))
#==

#== Plotting:
setEPS()
postscript("Cla_PayOptions_sorted_Imputed_mnar_Tree.eps")
plot(tr_class_MNAR, main="MNAR Classification Tree for sorted Pay Options", sub=paste("Prediction Error for all: ",round(mnar_tree_payoption_pred_all_error,4),"Prediction error for ten: ",round(mnar_tree_payoption_pred_ten_error,4)))
text(tr_class_MNAR, pretty = 0, cex=0.8)
dev.off()


##=== START: Classification via Forest for MNAR=== 
mnar_forest_class_payoption = randomForest(X046_PayOptions ~ ., data=mnar_class_set_payoption_woNA, ntree = 50, na.action=na.omit)

mnar_forest_payoption_pred_all = predict(mnar_forest_class_payoption, mnar_class_set_payoption[,!(mnar_class_set_payoption_names %in% "X046_PayOptions")], type = "class")
mnar_forest_payoption_pred_all_error <- mean(as.character(mnar_forest_payoption_pred_all) != as.character(class_set_payoption$X046_PayOptions)) 

mnar_forest_payoption_pred_ten = predict(mnar_forest_class_payoption, mnar_class_set_payoption[IndexOfMissing_PayOptions,!(mnar_class_set_payoption_names %in% "X046_PayOptions")], type = "class")
mnar_forest_payoption_pred_ten_error <- mean(as.character(mnar_forest_payoption_pred_ten) != as.character(class_set_payoption$X046_PayOptions[IndexOfMissing_PayOptions]))

#== Some further checks:
cbind(as.character(mnar_forest_payoption_pred_ten),as.character(class_set_payoption$X046_PayOptions[IndexOfMissing_PayOptions]))
#==

#== Plotting:
setEPS()
postscript("Cla_PayOptions_sorted_Imputed_mnar_Forest.eps")
varImpPlot(mnar_forest_class_payoption, main="Classification via Forest for Sorted Pay Options", sub=paste("Prediction Error for all: ",round(mnar_forest_payoption_pred_all_error,4),"Prediction error for ten: ",round(mnar_forest_payoption_pred_ten_error,4)))
dev.off()
##=== END: Classification via Forest ===
