source("Seed.R")

## === START: MCAR Data preparartion ===

# Copying the working set
mcar_regcla_set = mcar_working_set
# reduce size of dataset for classification and persue classification for cities based on distances:
mcar_regcla_set = mcar_regcla_set[, c("X003_Postcode","X004_City","X010_Lon", "X011_Lat", "X012_NoRooms", "X099_DistanceCathedralLeSeu", "X100_DistanceAlcudiaOldTown", "X098_DistanceSerraDeTramuntana")]
# Now we're doing classification w/ the imputed values. So we include distToBeach_imp
mcar_regcla_set = cbind(mcar_regcla_set,distToBeach_imp)

# Having Postcode as numeric proved as a more performant solution than having a factor w/ a huge amount of levels
mcar_regcla_set[ , "X003_Postcode"] <- as.numeric(mcar_regcla_set[ , "X003_Postcode"])
mcar_regcla_set[ , "X012_NoRooms"] <- as.numeric(mcar_regcla_set[ , "X012_NoRooms"])

#Updating the levels. Otherwise error: Can't have empty classes in y 
mcar_regcla_set[ , "X004_City"] <- as.character(mcar_regcla_set[ , "X004_City"])
mcar_regcla_set[ , "X004_City"] <- as.factor(mcar_regcla_set[ , "X004_City"])

#Copying the original dataset (for means of error calculation later on)
regcla_working_set <- working_set
regcla_working_set[ , "X004_City"] <- as.character(regcla_working_set[ , "X004_City"])

#Drop NA rows for model creation
mcar_regcla_set_woNA <- na.omit(mcar_regcla_set) 

# Creating an index to identify the missing values
mcar_regcla_set_names <- colnames(mcar_regcla_set)
IndexOfMissing_city <- which(is.na(mcar_regcla_set$X004_City))

str(mcar_regcla_set_woNA)
str(mcar_regcla_set)
## === END: MCAR Data preparartion ===


##=== START: Classification via Tree for MCAR=== 
tr_regcla_MCAR = rpart(X004_City ~ ., data=mcar_regcla_set_woNA)

#####Predictions
mcar_tree_pred_all = predict(tr_regcla_MCAR, mcar_regcla_set[,!(mcar_regcla_set_names %in% "X004_City")], type = "class")
mcar_tree_pred_all_error <- mean((as.data.frame(mcar_tree_pred_all)[,1]) != regcla_working_set$X004_City) #NA, in case of missing values

mcar_tree_pred_ten = predict(tr_regcla_MCAR, mcar_regcla_set[IndexOfMissing_city,!(mcar_regcla_set_names %in% "X004_City")], type = "class")
mcar_tree_pred_ten_error <- mean((as.data.frame(mcar_tree_pred_ten)[,1]) != regcla_working_set$X004_City[IndexOfMissing_city]) #NA, in case of missing values

#== Some further checks:
cbind(as.character(as.data.frame(mcar_tree_pred_ten)[,1]),as.character(regcla_working_set$X004_City[IndexOfMissing_city]))
#==

#== Plotting:
setEPS()
postscript("Cla_City_Imputed_mcar_Tree.eps")
plot(tr_regcla_MCAR, main="MCAR Classification Tree for City", sub=paste("Prediction Error for all: ",round(mcar_tree_pred_all_error,4),"                          Prediction error for ten: ",round(mcar_tree_pred_ten_error,4)))
text(tr_regcla_MCAR, pretty = 0, cex=0.8)
dev.off()
#==

##=== END: Classification via Tree ===
# Interesting: Setting the factorial feature "postcode" as numerical brings an advantage of 0.1


##=== START: Classification via Forest for MCAR===
#####Model creation
mcar_forest_regcla = randomForest(X004_City ~ ., data=mcar_regcla_set_woNA, ntree=50)

#####Predictions
mcar_forest_pred_all = predict(mcar_forest_regcla, mcar_regcla_set[,!(mcar_regcla_set_names %in% "X004_City")], type = "class") 
mcar_forest_pred_all_error <- mean((as.data.frame(as.character(mcar_forest_pred_all))[,1]) != regcla_working_set$X004_City) #NA, in case of missing values

mcar_forest_pred_ten = predict(mcar_forest_regcla, mcar_regcla_set[IndexOfMissing_city,!(mcar_regcla_set_names %in% "X004_City")], type = "class") 
mcar_forest_pred_ten_error <- mean((as.data.frame(as.character(mcar_forest_pred_ten))[,1]) != regcla_working_set$X004_City[IndexOfMissing_city]) #NA, in case of missing values

#== Some further checks:
cbind(as.character(as.data.frame(as.character(mcar_forest_pred_ten))[,1]),regcla_working_set$X004_City[IndexOfMissing_city])
#==

#== Plotting:
setEPS()
postscript("Cla_City_Imputed_mcar_Forest.eps")
varImpPlot(mcar_forest_regcla, main="Classification via Forest for City", sub=paste("Prediction Error for all: ",round(mcar_forest_pred_all_error,4),"                          Prediction error for ten: ",round(mcar_forest_pred_ten_error,4))) # Interpretation necessary
dev.off()

#==

##=== END: Classification via Forest ===#


## === START: MAR Data preparartion ===
# Copying the working set
# "X004_City" caused massive performance problems here. Probably it has too many levels.
# So we removed it
mar_regcla_set = mar_working_set[, c("X003_Postcode", "X010_Lon", "X011_Lat", "X102_DistanceToBeach", "X099_DistanceCathedralLeSeu", "X100_DistanceAlcudiaOldTown", "X098_DistanceSerraDeTramuntana")]

# Now we're doing classification w/ the imputed values. So we include rooms_imp
mcar_regcla_set = cbind(mcar_regcla_set,rooms_imp)

#Updating the levels. Otherwise error: Can't have empty classes in y 
mar_regcla_set[ , "X003_Postcode"] <- as.character(mar_regcla_set[ , "X003_Postcode"])
mar_regcla_set[ , "X003_Postcode"] <- as.factor(mar_regcla_set[ , "X003_Postcode"])

# Creating an index to identify the missing values
mar_regcla_set_names <- colnames(mar_regcla_set)
IndexOfMissing_postcode <- which(is.na(mar_regcla_set$X003_Postcode))

#Drop NA rows for model creation
mar_regcla_set_woNA <- na.omit(mar_regcla_set) 

## === END: MAR Data preparation ===

##=== START: Classification via Tree for MAR=== 
tr_regcla_MAR = rpart(X003_Postcode ~ ., data=mar_regcla_set_woNA)

#####Predictions
mar_tree_pred_all <- predict(tr_regcla_MAR, mar_regcla_set[,!(mar_regcla_set_names %in% "X003_Postcode")], type="class")
mar_tree_pred_all_error <- mean((as.data.frame(mar_tree_pred_all)[,1]) != as.character(regcla_working_set$X003_Postcode))  #NA, in case of missing values

mar_tree_pred_ten <- predict(tr_regcla_MAR, mar_regcla_set[IndexOfMissing_postcode,!(mar_regcla_set_names %in% "X003_Postcode")], type="class")
mar_tree_pred_ten_error <- mean((as.data.frame(mar_tree_pred_ten)[,1]) != as.character(regcla_working_set$X003_Postcode[IndexOfMissing_postcode]))  #NA, in case of missing values

#== Some further checks:
cbind(as.character(as.data.frame(mar_tree_pred_ten)[,1]),as.character(regcla_working_set$X003_Postcode[IndexOfMissing_postcode]))
#==

#== Plotting:
setEPS()
postscript("Cla_Postcode_Imputed_mar_Tree.eps")
plot(tr_regcla_MAR, main="MAR Classification Tree for Postcode", sub=paste("Prediction Error for all: ",round(mar_tree_pred_all_error,4),"Prediction error for ten: ",round(mar_tree_pred_ten_error,4)))
text(tr_regcla_MAR, pretty = 0, cex=0.8)
dev.off()
##=== END: Classification via Tree ===


##=== START: Classification via Forest for MAR===

#####Model Creation
mar_forest_regcla = randomForest(X003_Postcode ~., data=mar_regcla_set_woNA, ntree = 50)

#####Prediction
mar_forest_pred_all = predict(mar_forest_regcla, mar_regcla_set[,!(mar_regcla_set_names %in% "X003_Postcode")], type = "class") 
mar_forest_pred_all_error <- mean((as.data.frame(mar_forest_pred_all)[,1]) != as.numeric(regcla_working_set$X003_Postcode))

mar_forest_pred_ten <- predict(mar_forest_regcla, mar_regcla_set[IndexOfMissing_postcode,!(mar_regcla_set_names %in% "X003_Postcode")], type = "class") 
mar_forest_pred_ten_error <- mean(as.character(as.data.frame(mar_forest_pred_ten)[,1]) != (as.character(as.data.frame(regcla_working_set$X003_Postcode[IndexOfMissing_postcode])[,1])))

#== Some further checks:
cbind(as.character(as.data.frame(mar_forest_pred_ten)[,1]),(as.character(as.data.frame(regcla_working_set$X003_Postcode[IndexOfMissing_postcode])[,1])))
#==

#== Plotting:
setEPS()
postscript("Cla_Postcode_Imputed_mar_Forest.eps")
varImpPlot(mar_forest_regcla, main="Classification via Forest for Postcode", sub=paste("Prediction Error for all: ",round(mar_forest_pred_all_error,4),"                          Prediction error for ten: ",round(mar_forest_pred_ten_error,4))) # Interpretation necessary) # Interpretation necessary
dev.off()

#==

##=== END: Classification via Forest ===


## === START: MNAR Data preparation ===
# Copying the working set
# reduce size of dataset for classification and persue classification for PayOptions manly based on distances:
mnar_regcla_set = mnar_working_set[, c("X046_PayOptions", "X012_NoRooms", "X102_DistanceToBeach", "X099_DistanceCathedralLeSeu", "X098_DistanceSerraDeTramuntana")]

# Set Pay Options as factor since we want to use it for classification
mnar_regcla_set[ , "X046_PayOptions"] <- as.factor(mnar_regcla_set[ , "X046_PayOptions"])

# Now we're doing classification w/ the imputed values. So we include distToBeach_imp
mnar_regcla_set = cbind(mnar_regcla_set,old_town_imp)

#Updating the levels. Otherwise error: Can't have empty classes in y 
mnar_regcla_set[ , "X046_PayOptions"] <- as.character(mnar_regcla_set[ , "X046_PayOptions"])
mnar_regcla_set[ , "X046_PayOptions"] <- as.factor(mnar_regcla_set[ , "X046_PayOptions"])

# Creating an index to identify the missing values
mnar_regcla_set_names <- colnames(mnar_regcla_set)
IndexOfMissing_PayOptions <- which(is.na(mnar_regcla_set$X046_PayOptions))

# Drop NA rows for model creation
mnar_regcla_set_woNA <- na.omit(mnar_regcla_set) 

## === END: MNAR Data preparartion ===

##=== START: Classification via Tree for MNAR=== 

#####Model Creation
tr_regcla_MNAR = rpart(X046_PayOptions ~ ., data=mnar_regcla_set_woNA)

#####Prediction
mnar_tree_pred_all = predict(tr_regcla_MNAR, mnar_regcla_set[,!(mnar_regcla_set_names %in% "X046_PayOptions")], type = "class")
mnar_tree_pred_all_error <- mean((as.data.frame(mnar_tree_pred_all)[,1]) != as.character(regcla_working_set$X046_PayOptions))

mnar_tree_pred_ten = predict(tr_regcla_MNAR, mnar_regcla_set[IndexOfMissing_PayOptions,!(mnar_regcla_set_names %in% "X046_PayOptions")], type = "class")
mnar_tree_pred_ten_error <- mean(as.character(as.data.frame(mnar_tree_pred_ten)[,1]) != as.character(regcla_working_set$X046_PayOptions[IndexOfMissing_PayOptions]))


#== Some further checks:
cbind(as.character(as.data.frame(mnar_tree_pred_ten)[,1]),as.character(as.data.frame(regcla_working_set$X046_PayOptions[IndexOfMissing_PayOptions])[,1]))
#==

#== Plotting:
setEPS()
postscript("Cla_PayOptions_Imputed_mnar_Tree.eps")
plot(tr_regcla_MNAR, main="MNAR Classification Tree for Pay Options", sub=paste("Prediction Error for all: ",round(mnar_tree_pred_all_error,4),"Prediction error for ten: ",round(mnar_tree_pred_ten_error,4)))
text(tr_regcla_MNAR, pretty = 0, cex=0.8)
dev.off()
##=== END: Classification via Tree ===

##=== START: Classification via Forest for MNAR=== 
#####Model Creation
mnar_forest_regcla = randomForest(X046_PayOptions ~ ., data=mnar_regcla_set_woNA, ntree = 50, na.action=na.omit)

#####Prediction
mnar_forest_pred_all = predict(mnar_forest_regcla, mnar_regcla_set[,!(mnar_regcla_set_names %in% "X046_PayOptions")], type = "class")
mnar_forest_pred_all_error <- mean(as.character(as.data.frame(mnar_forest_pred_all)[,1]) != as.character(as.data.frame(regcla_working_set$X046_PayOptions)[,1])) 

mnar_forest_pred_ten = predict(mnar_forest_regcla, mnar_regcla_set[IndexOfMissing_PayOptions,!(mnar_regcla_set_names %in% "X046_PayOptions")], type = "class")
mnar_forest_pred_ten_error <- mean((as.data.frame(mnar_forest_pred_ten)[,1]) != as.character(as.data.frame(regcla_working_set$X046_PayOptions[IndexOfMissing_PayOptions])[,1]))

#== Some further checks:
cbind(as.character((as.data.frame(mnar_forest_pred_ten)[,1])),as.character(as.data.frame(regcla_working_set$X046_PayOptions[IndexOfMissing_PayOptions])[,1]))
#==

#== Plotting:
setEPS()
postscript("Cla_PayOptions_Imputed_mnar_Tree.eps")
varImpPlot(mnar_forest_regcla, main="Classification via Forest for Pay Options", sub=paste("Prediction Error for all: ",round(mnar_forest_pred_all_error,4),"                          Prediction error for ten: ",round(mnar_forest_pred_ten_error,4)))
dev.off()

##=== END: Classification via Forest ===