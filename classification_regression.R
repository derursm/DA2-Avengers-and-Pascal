source("Seed.R")

## === START: MCAR Data preparation ===

mcar_class_set_lb = mcar_working_set_lb
# reduce size of dataset for classification and persue classification for cities based on distances:
mcar_class_set_lb = mcar_class_set_lb[, c("X003_Postcode","X103_LocationBeach","X010_Lon", "X011_Lat", "X012_NoRooms", "X099_DistanceCathedralLeSeu", "X100_DistanceAlcudiaOldTown", "X098_DistanceSerraDeTramuntana")]
# Having Postcode as numeric proved as a more performant solution than having a factor w/ a huge amount of levels
mcar_class_set_lb[ , "X003_Postcode"] <- as.numeric(mcar_class_set_lb[ , "X003_Postcode"])
mcar_class_set_lb[ , "X012_NoRooms"] <- as.numeric(mcar_class_set_lb[ , "X012_NoRooms"])
#Updating the levels. Otherwise error: Can't have empty classes in y 
mcar_class_set_lb[ , "X004_City"] <- as.character(mcar_class_set_lb[ , "X004_City"])
mcar_class_set_lb[ , "X004_City"] <- as.factor(mcar_class_set_lb[ , "X004_City"])
class_working_set_lb <- working_set
class_working_set_lb[ , "X004_City"] <- as.character(class_working_set_lb[ , "X004_City"])

mcar_class_set_lb_woNA <- na.omit(mcar_class_set_lb) #For classification --> Drop NA rows


mcar_class_set_lb_names <- colnames(mcar_class_set_lb)
IndexOfMissing_loc <- which(is.na(mcar_class_set_lb$X103_LocationBeach))
str(mcar_class_set_lb_woNA)
str(mcar_class_set_lb)
## === END: MCAR Data preparation ===


##=== START: Classification via Tree for MCAR=== 
mcar_tree_lb = rpart(X103_LocationBeach ~ ., data=mcar_class_set_lb_woNA)
str(mcar_class_set_lb_woNA)
#####Predictions
mcar_tree_pred_all_lb = predict(mcar_tree_lb, mcar_class_set_lb[,!(mcar_class_set_lb_names %in% "X103_LocationBeach")], type = "class")
mcar_tree_pred_all_lb_error <- mean((as.data.frame(mcar_tree_pred_all_lb)[,1]) != class_working_set_lb$X103_LocationBeach) #NA, in case of missing values

mcar_tree_pred_ten_lb = predict(mcar_tree_lb, mcar_class_set_lb[IndexOfMissing_loc,!(mcar_class_set_lb_names %in% "X103_LocationBeach")], type = "class")
mcar_tree_pred_ten_lb_error <- mean((as.data.frame(mcar_tree_pred_ten_lb)[,1]) != class_working_set_lb$X103_LocationBeach[IndexOfMissing_loc]) #NA, in case of missing values

#== Some further checks:
cbind(as.character(as.data.frame(mcar_tree_pred_ten_lb)[,1]),as.character(class_working_set_lb$X103_LocationBeach[IndexOfMissing_loc]))
#==

#== Plotting:  
setEPS()
postscript("Class_MCAR_tree_lb.eps")
plot(mcar_tree_lb, main="MCAR Classification Tree for Region", sub=paste("Prediction Error for all: ",round(mcar_tree_pred_all_lb_error,4),"                          Prediction error for ten: ",round(mcar_tree_pred_ten_lb_error,4)))
text(mcar_tree_lb, pretty = 0, cex=0.8)
dev.off()
#==

#== Assignment for Regression:
lb_imp_tree <- class_working_set_lb$X103_LocationBeach
lb_imp_tree[IndexOfMissing_loc] <- mcar_tree_pred_ten_lb

##=== END: Classification via Tree ===

##=== START: Classification via Forest for MCAR===

mcar_forest_class_lb = randomForest(X103_LocationBeach ~ ., data=mcar_class_set_lb_woNA, ntree=50)

mcar_forest_pred_all_lb = predict(mcar_forest_class_lb, mcar_class_set_lb[,!(mcar_class_set_lb_names %in% "X103_LocationBeach")], type = "class") 
mcar_forest_pred_all_lb_error <- mean((as.data.frame(as.character(mcar_forest_pred_all_lb))[,1]) != class_working_set_lb$X103_LocationBeach) #NA, in case of missing values

mcar_forest_pred_ten_lb = predict(mcar_forest_class_lb, mcar_class_set_lb[IndexOfMissing_loc,!(mcar_class_set_lb_names %in% "X103_LocationBeach")], type = "class") 
mcar_forest_pred_ten_lb_error <- mean(as.character(mcar_forest_pred_ten_lb) != class_working_set_lb$X103_LocationBeach[IndexOfMissing_loc]) #NA, in case of missing values

#== Some further checks:
cbind(as.character(mcar_forest_pred_ten_lb),as.character(class_working_set_lb$X103_LocationBeach[IndexOfMissing_loc]))
#==

#== Plotting:
setEPS()
postscript("Class_MCAR_forest_lb.eps")
varImpPlot(mcar_forest_class_lb, main="Classification via Forest for Location", sub=paste("Prediction Error for all: ",round(mcar_forest_pred_all_lb_error,4),"                          Prediction error for ten: ",round(mcar_forest_pred_ten_lb_error,4))) # Interpretation necessary
dev.off()
#==


##=== START: Regression based on imputed data for MCAR===

##### Preparation

mcar_reg_set_lb = mcar_working_set_lb
mcar_reg_set_lb = mcar_reg_set_lb[, c("X003_Postcode","X010_Lon", "X011_Lat", "X012_NoRooms", "X099_DistanceCathedralLeSeu", "X100_DistanceAlcudiaOldTown", "X098_DistanceSerraDeTramuntana", "X102_DistanceToBeach")]
# Having Postcode as numeric proved as a more performant solution than having a factor w/ a huge amount of levels
mcar_reg_set_lb = cbind(mcar_reg_set_lb,lb_imp_tree)
reg_working_set_lb <- working_set
reg_working_set_lb[ , "X004_City"] <- as.character(reg_working_set_lb[ , "X004_City"])

mcar_reg_set_lb_woNA <- na.omit(mcar_reg_set_lb)

mcar_reg_set_lb_names <- colnames(mcar_reg_set_lb)
IndexOfMissing_loc <- which(is.na(mcar_reg_set_lb$lb_imp_tree))
str(mcar_reg_set_lb_woNA)
str(mcar_reg_set_lb)

##### Tree
# We chose a tree here, because it's empirically better performing than a linear model
# However, a forest would be even better but causes too much trouble (e.g. can't handle factor "city" - and we don't want to strip down the data anymore)
# Apply Forest
mcar_fit_tree_lb = rpart(X102_DistanceToBeach ~ ., data = mcar_reg_set_lb_woNA)

# Predict all values of DistanceToBeach based on all other features + Calculate MSE
mcar_fit_tree_lb_pred_all<-predict(mcar_fit_tree_lb,mcar_reg_set_lb[,!(mcar_reg_set_lb_names %in% c("X102_DistanceToBeach"))])
mcar_mse_tree_all <- mse(mcar_fit_tree_lb_pred_all,working_set$X102_DistanceToBeach)

# Predict 10 missing values of DistanceToBeach based on all other features + Calculate MSE
mcar_fit_tree_lb_pred_ten<-predict(mcar_fit_tree_lb,mcar_reg_set_lb[IndexOfMissing_beach,!(mcar_reg_set_lb_names %in% c("X102_DistanceToBeach"))])
mcar_mse_tree_ten <- mse(mcar_fit_tree_lb_pred_ten,working_set$X102_DistanceToBeach[IndexOfMissing_beach])

# Look at predicted NA values
cbind(mcar_fit_tree_lb_pred_ten, working_set$X102_DistanceToBeach[IndexOfMissing_beach])

# Plot
mcar_tree_imputed <- cbind(IndexOfMissing_beach,mcar_fit_tree_lb_pred_ten)
mcar_tree_original <- cbind(IndexOfMissing_beach,working_set$X102_DistanceToBeach[IndexOfMissing_beach])
setEPS()
postscript("Reg_DistBeach_Imputed_tree.eps")
plot(mcar_working_set$X102_DistanceToBeach, ylab="Distance to Beach", main="Missing and imputed Values in Distance to Beach", sub=paste("MSE for all:",round(mcar_mse_tree_all,4),"                 MSE for ten:",round(mcar_mse_tree_ten,4)))
points(mcar_tree_imputed, col="red", pch=17)
points(mcar_tree_original, col="blue", pch=19)
legend('topright', c("imputed", "original") , col=c('red', 'blue'), pch=c(17,19), bty='o', pt.cex=2,text.font=2)
dev.off()
