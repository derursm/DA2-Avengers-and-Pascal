#=== Import Datasets (from files in workspace (~.Rdata). They're created by "mcar_working_set" etc.===
load("mcar_working_set.Rdata")
load("mar_working_set.Rdata")
load("mnar_working_set.Rdata")
source("Seed.R")

#=== START: MCAR ===

##### Prepare Datasets
# Copying the working set and removing unsuitable columns. 
# E.g. unsuitable means factors having too many or too few levels (e.g. "X006_Country" is always Spain). 
mcar_working_set_names <- colnames(mcar_working_set)
mcar_reg_set <- mcar_working_set[,!(mcar_working_set_names %in% c("X001_Name","X002_Street","X006_Country","X004_City","X014_CheckIn","X046_PayOptions"))]
mcar_reg_set_names<-names(mcar_reg_set)

#Visual Check
View(mcar_reg_set)

# Creating a dataset without observations which have NA values in one or more features
mcar_reg_set_woNA <- na.omit(mcar_reg_set)
mcar_reg_set_woNA_names <- colnames(mcar_reg_set_woNA)
str(mcar_reg_set_woNA)

#Since randomForest can't handle factors with more than 53 levels, we remove them.
mcar_reg_set_woNA_ranF <- mcar_reg_set_woNA[,!(mcar_reg_set_woNA_names %in% c("X003_Postcode"))]
str(mcar_reg_set_woNA_ranF)

##### Find NA's
mcar_missing_beach = is.na(mcar_reg_set$X102_DistanceToBeach)
IndexOfMissing_beach <- which(is.na(mcar_reg_set$X102_DistanceToBeach))
str(mcar_reg_set)

##### Linear Model
# Apply Linear Model
mcar_fit_lm = lm(X102_DistanceToBeach ~ ., data = mcar_reg_set_woNA)

# Predict all values of DistanceToBeach based on all other features + Calculate MSE
mcar_fit_lm_pred_all<-predict(mcar_fit_lm, mcar_reg_set)
mcar_mse_lm_all <- mse(mcar_fit_lm_pred_all,working_set$X102_DistanceToBeach)

# Double-check this MSE - minor differences can occur because of rounding errors
mean((mcar_fit_lm$residuals)^2) 

# Predict 10 missing values of DistanceToBeach based on all other features + Calculate MSE
mcar_fit_lm_pred_ten<-predict(mcar_fit_lm,mcar_reg_set[IndexOfMissing_beach,])
mcar_mse_lm_ten <- mse(mcar_fit_lm_pred_ten,working_set$X102_DistanceToBeach[IndexOfMissing_beach])

# Look at predicted NA values
cbind(mcar_fit_lm_pred_ten, working_set$X102_DistanceToBeach[IndexOfMissing_beach])

# Plot
mcar_lm_imputed <- cbind(IndexOfMissing_beach,mcar_fit_lm_pred_ten)
mcar_lm_original <- cbind(IndexOfMissing_beach,working_set$X102_DistanceToBeach[IndexOfMissing_beach])

emf("DistanceToBeach_lm.emf",width=8,height=8,bg="transparent",custom.lty=F)
plot(mcar_working_set$X102_DistanceToBeach, ylab="Distance to Beach", main="Missing and imputed Values in Distance to Beach", sub=paste("MSE for all:",round(mcar_mse_lm_all,4),"                 MSE for ten:",round(mcar_mse_lm_ten,4)))
points(mcar_lm_imputed, col="red", pch=17)
points(mcar_lm_original, col="blue", pch=19)
par(cex=.9)
legend('topright', c("imputed", "original") , col=c('red', 'blue'), pch=c(17,19), bty='o', pt.cex=2,text.font=2)
dev.off()

# Prepare for classification based on regression (happens later in another file)
# Therefore, replace NA's in the original data with imputed values and save them in a vector + check
distToBeach_imp_lm <- working_set$X102_DistanceToBeach
distToBeach_imp_lm[IndexOfMissing_beach] <- mcar_fit_lm_pred_ten
cbind(distToBeach_imp_lm,working_set$X102_DistanceToBeach)


##### Regression Tree
# Apply Tree
mcar_fit_tree = rpart(X102_DistanceToBeach ~ ., data = mcar_reg_set_woNA)
# Predict all values of DistanceToBeach based on all other features + Calculate MSE
mcar_fit_tree_pred_all<-predict(mcar_fit_tree,mcar_reg_set[,!(mcar_reg_set_names %in% c("X102_DistanceToBeach"))])
mcar_mse_tree_all <- mse(mcar_fit_tree_pred_all,working_set$X102_DistanceToBeach)

# Predict 10 missing values of DistanceToBeach based on all other features + Calculate MSE
mcar_fit_tree_pred_ten<-predict(mcar_fit_tree,mcar_reg_set[IndexOfMissing_beach,!(mcar_reg_set_names %in% c("X102_DistanceToBeach"))])
mcar_mse_tree_ten <- mse(mcar_fit_tree_pred_ten,working_set$X102_DistanceToBeach[IndexOfMissing_beach])

# Look at predicted NA values
cbind(mcar_fit_tree_pred_ten, working_set$X102_DistanceToBeach[IndexOfMissing_beach])

# Plot
mcar_tree_imputed <- cbind(IndexOfMissing_beach,mcar_fit_tree_pred_ten)
mcar_tree_original <- cbind(IndexOfMissing_beach,working_set$X102_DistanceToBeach[IndexOfMissing_beach])
emf("DistanceToBeach_tree.emf",width=8,height=8,bg="transparent",custom.lty=F)
plot(mcar_working_set$X102_DistanceToBeach, ylab="Distance to Beach", main="Missing and imputed Values in Distance to Beach", sub=paste("MSE for all:",round(mcar_mse_tree_all,4),"                 MSE for ten:",round(mcar_mse_tree_ten,4)))
points(mcar_tree_imputed, col="red", pch=17)
points(mcar_tree_original, col="blue", pch=19)
legend('topright', c("imputed", "original") , col=c('red', 'blue'), pch=c(17,19), bty='o', pt.cex=2,text.font=2)
dev.off()

# Prepare for classification based on regression (happens later in another file)
# Therefore, replace NA's in the original data with imputed values and save them in a vector + check
distToBeach_imp_tree <- working_set$X102_DistanceToBeach
distToBeach_imp_tree[IndexOfMissing_beach] <- mcar_fit_tree_pred_ten
cbind(distToBeach_imp_tree,working_set$X102_DistanceToBeach)


##### Random Forest
# Apply Forest
mcar_fit_ranF = randomForest(X102_DistanceToBeach ~ ., data = mcar_reg_set_woNA_ranF)

# Predict all values of DistanceToBeach based on all other features + Calculate MSE
mcar_fit_ranF_pred_all<-predict(mcar_fit_ranF,mcar_reg_set[,!(mcar_reg_set_names %in% c("X102_DistanceToBeach"))])
mcar_mse_ranF_all <- mse(mcar_fit_ranF_pred_all,working_set$X102_DistanceToBeach)

# Predict 10 missing values of DistanceToBeach based on all other features + Calculate MSE
mcar_fit_ranF_pred_ten<-predict(mcar_fit_ranF,mcar_reg_set[IndexOfMissing_beach,!(mcar_reg_set_names %in% c("X102_DistanceToBeach"))])
mcar_mse_ranF_ten <- mse(mcar_fit_ranF_pred_ten,working_set$X102_DistanceToBeach[IndexOfMissing_beach])

# Look at predicted NA values
cbind(mcar_fit_ranF_pred_ten, working_set$X102_DistanceToBeach[IndexOfMissing_beach])

# Plot
mcar_ranF_imputed <- cbind(IndexOfMissing_beach,mcar_fit_ranF_pred_ten)
mcar_ranF_original <- cbind(IndexOfMissing_beach,working_set$X102_DistanceToBeach[IndexOfMissing_beach])
emf("DistanceToBeach_ranF.emf",width=8,height=8,bg="transparent",custom.lty=F)
plot(mcar_working_set$X102_DistanceToBeach, ylab="Distance to Beach", main="Missing and imputed Values in Distance to Beach", sub=paste("MSE for all:",round(mcar_mse_ranF_all,4),"                 MSE for ten:",round(mcar_mse_ranF_ten,4)))
points(mcar_ranF_imputed, col="red", pch=17)
points(mcar_ranF_original, col="blue", pch=19)
legend('topright', c("imputed", "original") , col=c('red', 'blue'), pch=c(17,19), bty='o', pt.cex=2,text.font=2)
dev.off()

# Prepare for classification based on regression (happens later in another file)
# Therefore, replace NA's in the original data with imputed values and save them in a vector + check
distToBeach_imp_ranF <- working_set$X102_DistanceToBeach
distToBeach_imp_ranF[IndexOfMissing_beach] <- mcar_fit_ranF_pred_ten
cbind(distToBeach_imp_ranF,working_set$X102_DistanceToBeach)
       
       
#=== START: MAR ===

##### Prepare Datasets
# Copying the working set and removing unsuitable columns. 
# E.g. unsuitable means factors having too many or too few levels (e.g. "X006_Country" is always Spain).
mar_working_set_names <- colnames(mar_working_set)
mar_reg_set <- mar_working_set[,!(mar_working_set_names %in% c("X001_Name","X002_Street", "X003_Postcode","X004_City","X006_Country","X014_CheckIn","X046_PayOptions"))]
mar_reg_set_names <- colnames(mar_reg_set)
mar_reg_set_woNA <- na.omit(mar_reg_set) 
mar_reg_set_woNA_names <- colnames(mar_reg_set_woNA)
str(mar_reg_set_woNA)

#Since randomForest can't handle factors with more than 53 levels, we remove them.
mar_reg_set_woNA_ranF <- mar_reg_set_woNA[,!(mar_reg_set_woNA_names %in% c("X003_Postcode"))]
str(mar_reg_set)

##### Find NA's
mar_missing_rooms = is.na(mar_working_set$X012_NoRooms)
IndexOfMissing_rooms <- which(is.na(mar_working_set$X012_NoRooms))

##### Linear Model
# Apply Linear Model
mar_fit_lm = lm(X012_NoRooms ~ ., data = mar_reg_set_woNA)

# Predict all values of DistanceToBeach based on all other features + Calculate MSE
mar_fit_lm_pred_all<-round(predict(mar_fit_lm,mar_reg_set[,!(mar_reg_set_names %in% c("X012_NoRooms"))]),0) # Rounded because rooms must be Integer
mar_mse_lm_all <- mse(mar_fit_lm_pred_all,working_set$X012_NoRooms)

# Double-check this MSE - minor differences can occur because of rounding errors
mean((mar_fit_lm$residuals)^2) 

# Predict 10 missing values of NoRooms based on all other features + Calculate MSE
mar_fit_lm_pred_ten<-round(predict(mar_fit_lm,mar_reg_set[IndexOfMissing_rooms,!(mar_reg_set_names %in% c("X012_NoRooms"))]),0) # Rounded because rooms must be Integer
mar_mse_lm_ten <- mse(mar_fit_lm_pred_ten,working_set$X012_NoRooms[IndexOfMissing_rooms])

# Look at predicted NA values
cbind(mar_fit_lm_pred_ten, working_set$X012_NoRooms[IndexOfMissing_rooms])

# Plot
mar_lm_imputed <- cbind(IndexOfMissing_rooms,mar_fit_lm_pred_ten)
mar_lm_original <- cbind(IndexOfMissing_rooms,working_set$X012_NoRooms[IndexOfMissing_rooms])
emf("Rooms_lm.emf",width=8,height=8,bg="transparent",custom.lty=F)
plot(mar_working_set$X012_NoRooms, ylab="Number of Rooms", main="Missing and imputed Values in Number of Rooms", sub=paste("MSE for all:",round(mar_mse_lm_all,4),"                 MSE for ten:",round(mar_mse_lm_ten,4)))
points(mar_lm_imputed, col="red", pch=17)
points(mar_lm_original, col="blue", pch=19)
legend('topright', c("imputed", "original") , col=c('red', 'blue'), pch=c(17,19), bty='o', pt.cex=2,text.font=2)
dev.off()


# Prepare for classification based on regression (happens later in another file)
# Therefore, replace NA's in the original data with imputed values and save them in a vector + check
rooms_imp_lm <- working_set$X012_NoRooms
rooms_imp_lm[IndexOfMissing_rooms] <- mar_fit_lm_pred_ten
cbind(rooms_imp_lm,working_set$X012_NoRooms)

##### Regression Tree
# Apply Tree
mar_fit_tree = rpart(X012_NoRooms ~ ., data = mar_reg_set_woNA)
# Predict all values of NoRooms based on all other features + Calculate MSE
mar_fit_tree_pred_all<-round(predict(mar_fit_tree,mar_reg_set[,!(mar_reg_set_names %in% c("X012_NoRooms"))]),0)
mar_mse_tree_all <- mse(mar_fit_tree_pred_all,working_set$X012_NoRooms)

# Predict 10 missing values of NoRooms based on all other features + Calculate MSE
mar_fit_tree_pred_ten<-round(predict(mar_fit_tree,mar_reg_set[IndexOfMissing_rooms,!(mar_reg_set_names %in% c("X012_NoRooms"))]),0)
mar_mse_tree_ten <- mse(mar_fit_tree_pred_ten,working_set$X012_NoRooms[IndexOfMissing_rooms])

# Look at predicted NA values
cbind(mar_fit_tree_pred_ten, working_set$X012_NoRooms[IndexOfMissing_rooms])

# Plot
mar_tree_imputed <- cbind(IndexOfMissing_rooms,mar_fit_tree_pred_ten)
mar_tree_original <- cbind(IndexOfMissing_rooms,working_set$X012_NoRooms[IndexOfMissing_rooms])
emf("Rooms_tree.emf",width=8,height=8,bg="transparent",custom.lty=F)
plot(mar_working_set$X012_NoRooms, ylab="Number of Rooms", main="Missing and imputed Values in Number of Rooms", sub=paste("MSE for all:",round(mar_mse_tree_all,4),"                 MSE for ten:",round(mar_mse_tree_ten,4)))
points(mar_tree_imputed, col="red", pch=17)
points(mar_tree_original, col="blue", pch=19)
legend('topright', c("imputed", "original") , col=c('red', 'blue'), pch=c(17,19), bty='o', pt.cex=2,text.font=2)
dev.off()

# Prepare for classification based on regression (happens later in another file)
# Therefore, replace NA's in the original data with imputed values and save them in a vector + check
rooms_imp_tree <- working_set$X012_NoRooms
rooms_imp_tree[IndexOfMissing_rooms] <- mar_fit_tree_pred_ten
cbind(rooms_imp_tree,working_set$X012_NoRooms)


##### Random Forest
# Apply Forest
mar_fit_ranF = randomForest(X012_NoRooms ~ ., data = mar_reg_set_woNA_ranF)

# Predict all values of NoRooms based on all other features + Calculate MSE
mar_fit_ranF_pred_all<-round(predict(mar_fit_ranF,mar_reg_set[,!(mar_reg_set_names %in% c("X012_NoRooms"))]),0)
mar_mse_ranF_all <- mse(mar_fit_ranF_pred_all,working_set$X012_NoRooms)

# Predict 10 missing values of NoRooms based on all other features + Calculate MSE
mar_fit_ranF_pred_ten<-round(predict(mar_fit_ranF,mar_reg_set[IndexOfMissing_rooms,!(mar_reg_set_names %in% c("X012_NoRooms"))]),0)
mar_mse_ranF_ten <- mse(mar_fit_ranF_pred_ten,working_set$X012_NoRooms[IndexOfMissing_rooms])

# Look at predicted NA values
cbind(mar_fit_ranF_pred_ten, working_set$X012_NoRooms[IndexOfMissing_rooms])

# Plot
mar_ranF_imputed <- cbind(IndexOfMissing_rooms,mar_fit_ranF_pred_ten)
mar_ranF_original <- cbind(IndexOfMissing_rooms,working_set$X012_NoRooms[IndexOfMissing_rooms])
emf("Rooms_ranF.emf",width=8,height=8,bg="transparent",custom.lty=F)
plot(mar_working_set$X012_NoRooms, ylab="NoRooms", main="Missing and imputed Values in Number of Rooms", sub=paste("MSE for all:",round(mar_mse_ranF_all,4),"                 MSE for ten:",round(mar_mse_ranF_ten,4)))
points(mar_ranF_imputed, col="red", pch=17)
points(mar_ranF_original, col="blue", pch=19)
legend('topright', c("imputed", "original") , col=c('red', 'blue'), pch=c(17,19), bty='o', pt.cex=2,text.font=2)
dev.off()

# Prepare for classification based on regression (happens later in another file)
# Therefore, replace NA's in the original data with imputed values and save them in a vector + check
rooms_imp_ranF <- working_set$X012_NoRooms
rooms_imp_ranF[IndexOfMissing_rooms] <- mar_fit_ranF_pred_ten
cbind(rooms_imp_ranF,working_set$X012_NoRooms)


#=== START: MNAR ===

##### Prepare Datasets
# Copying the working set and removing unsuitable columns. 
# E.g. unsuitable means factors having too many or too few levels (e.g. "X006_Country" is always Spain).
mnar_working_set_names <- colnames(mnar_working_set)
mnar_reg_set <- mnar_working_set[,!(mnar_working_set_names %in% c("X001_Name","X002_Street", "X003_Postcode","X004_City","X006_Country","X014_CheckIn","X046_PayOptions"))]
mnar_reg_set_names<-colnames(mnar_reg_set)
mnar_reg_set_woNA <- na.omit(mnar_reg_set) 
mnar_reg_set_woNA_names <- colnames(mnar_reg_set_woNA)
str(mnar_reg_set_woNA)

#Since randomForest can't handle factors with more than 53 levels, we remove them.
mnar_reg_set_woNA_ranF <- mnar_reg_set_woNA #[,!(mnar_reg_set_woNA_names %in% c("X003_Postcode","X004_City"))]
str(mnar_reg_set_woNA_ranF)

##### Find NA's
mnar_old_town = is.na(mnar_working_set$X100_DistanceAlcudiaOldTown)
IndexOfMissing_old_town <- which(is.na(mnar_working_set$X100_DistanceAlcudiaOldTown))

##### Linear Model
# Apply Linear Model
mnar_fit_lm = lm(X100_DistanceAlcudiaOldTown ~ ., data = mnar_reg_set_woNA)

# Predict all values of DistanceAlcudiaOldTown based on all other features + Calculate MSE
mnar_fit_lm_pred_all<-predict(mnar_fit_lm,mnar_reg_set[,!(mnar_reg_set_names %in% c("X100_DistanceAlcudiaOldTown"))])
mnar_mse_lm_all <- mse(mnar_fit_lm_pred_all,working_set$X100_DistanceAlcudiaOldTown)
# Double-check this MSE - minor differences can occur because of rounding errors
mean((mnar_fit_lm$residuals)^2) 

# Predict 10 missing values of DistanceAlcudiaOldTown based on all other features + Calculate MSE
mnar_fit_lm_pred_ten<-predict(mnar_fit_lm,mnar_reg_set[IndexOfMissing_old_town,!(mnar_reg_set_names %in% c("X100_DistanceAlcudiaOldTown"))])
mnar_mse_lm_ten <- mse(mnar_fit_lm_pred_ten,working_set$X100_DistanceAlcudiaOldTown[IndexOfMissing_old_town])

# Look at predicted NA values
cbind(mnar_fit_lm_pred_ten, working_set$X100_DistanceAlcudiaOldTown[IndexOfMissing_old_town])

# Plot
mnar_lm_imputed <- cbind(IndexOfMissing_old_town,mnar_fit_lm_pred_ten)
mnar_lm_original <- cbind(IndexOfMissing_old_town,working_set$X100_DistanceAlcudiaOldTown[IndexOfMissing_old_town])
emf("OldTown_lm.emf",width=8,height=8,bg="transparent",custom.lty=F)
plot(mnar_working_set$X100_DistanceAlcudiaOldTown, ylab="Distance Alcudia Old Town", main="Missing and imputed Values in Distance to Alcudia Old Town", sub=paste("MSE for all:",round(mnar_mse_lm_all,4),"                 MSE for ten:",round(mnar_mse_lm_ten,4)))
points(mnar_lm_imputed, col="red", pch=17)
points(mnar_lm_original, col="blue", pch=19)
legend('topright', c("imputed", "original") , col=c('red', 'blue'), pch=c(17,19), bty='o', pt.cex=2,text.font=2)
dev.off()

# Prepare for classification based on regression (happens later in another file)
# Therefore, replace NA's in the original data with imputed values and save them in a vector + check
old_town_imp_lm <- working_set$X100_DistanceAlcudiaOldTown
old_town_imp_lm[IndexOfMissing_old_town] <- mnar_fit_lm_pred_ten
cbind(old_town_imp_lm,working_set$X100_DistanceAlcudiaOldTown)


##### Regression Tree
# Apply Tree
mnar_fit_tree = rpart(X100_DistanceAlcudiaOldTown ~ ., data = mnar_reg_set_woNA)
# Predict all values of DistanceAlcudiaOldTown based on all other features + Calculate MSE
mnar_fit_tree_pred_all<-predict(mnar_fit_tree,mnar_reg_set[,!(mnar_reg_set_names %in% c("X100_DistanceAlcudiaOldTown"))])
mnar_mse_tree_all <- mse(mnar_fit_tree_pred_all,working_set$X100_DistanceAlcudiaOldTown)

# Predict 10 missing values of DistanceAlcudiaOldTown based on all other features + Calculate MSE
mnar_fit_tree_pred_ten<-predict(mnar_fit_tree,mnar_reg_set[IndexOfMissing_old_town,!(mnar_reg_set_names %in% c("X100_DistanceAlcudiaOldTown"))])
mnar_mse_tree_ten <- mse(mnar_fit_tree_pred_ten,working_set$X100_DistanceAlcudiaOldTown[IndexOfMissing_old_town])

# Look at predicted NA values
cbind(mnar_fit_tree_pred_ten, working_set$X100_DistanceAlcudiaOldTown[IndexOfMissing_old_town])

# Plot
mnar_tree_imputed <- cbind(IndexOfMissing_old_town,mnar_fit_tree_pred_ten)
mnar_tree_original <- cbind(IndexOfMissing_old_town,working_set$X100_DistanceAlcudiaOldTown[IndexOfMissing_old_town])
emf("OldTown_tree.emf",width=8,height=8,bg="transparent",custom.lty=F)
plot(mnar_working_set$X100_DistanceAlcudiaOldTown, ylab="Distance Alcudia Old Town", main="Missing and imputed Values in Distance to Alcudia Old Town", sub=paste("MSE for all:",round(mnar_mse_tree_all,4),"                 MSE for ten:",round(mnar_mse_tree_ten,4)))
points(mnar_tree_imputed, col="red", pch=17)
points(mnar_tree_original, col="blue", pch=19)
legend('topright', c("imputed", "original") , col=c('red', 'blue'), pch=c(17,19), bty='o', pt.cex=2,text.font=2)
dev.off()

# Prepare for classification based on regression (happens later in another file)
# Therefore, replace NA's in the original data with imputed values and save them in a vector + check
old_town_imp_tree <- working_set$X100_DistanceAlcudiaOldTown
old_town_imp_tree[IndexOfMissing_old_town] <- mnar_fit_tree_pred_ten
cbind(old_town_imp_tree,working_set$X100_DistanceAlcudiaOldTown)

##### Random Forest
# Apply Forest
mnar_fit_ranF <- randomForest(X100_DistanceAlcudiaOldTown ~ ., data = mnar_reg_set_woNA_ranF)

# Predict all values of DistanceAlcudiaOldTown based on all other features + Calculate MSE
mnar_fit_ranF_pred_all<- predict(mnar_fit_ranF, mnar_reg_set[,!(mnar_reg_set_names %in% c("X100_DistanceAlcudiaOldTown"))])
mnar_mse_ranF_all <- mse(mnar_fit_ranF_pred_all,working_set$X100_DistanceAlcudiaOldTown)

# Predict 10 missing values of DistanceAlcudiaOldTown based on all other features + Calculate MSE
mnar_fit_ranF_pred_ten<-predict(mnar_fit_ranF,mnar_reg_set[IndexOfMissing_old_town,!(mnar_reg_set_names %in% c("X100_DistanceAlcudiaOldTown"))])
mnar_mse_ranF_ten <- mse(mnar_fit_ranF_pred_ten,working_set$X100_DistanceAlcudiaOldTown[IndexOfMissing_old_town])

# Look at predicted NA values
cbind(mnar_fit_ranF_pred_ten, working_set$X100_DistanceAlcudiaOldTown[IndexOfMissing_old_town])

# Plot
mnar_ranF_imputed <- cbind(IndexOfMissing_old_town,mnar_fit_ranF_pred_ten)
mnar_ranF_original <- cbind(IndexOfMissing_old_town,working_set$X100_DistanceAlcudiaOldTown[IndexOfMissing_old_town])
emf("OldTown_ranF.emf",width=8,height=8,bg="transparent",custom.lty=F)
plot(mnar_working_set$X100_DistanceAlcudiaOldTown, ylab="Distance Alcudia Old Town", main="Missing and imputed Values in Distance to Alcudia Old Town", sub=paste("MSE for all:",round(mnar_mse_ranF_all,4),"                 MSE for ten:",round(mnar_mse_ranF_ten,4)))
points(mnar_ranF_imputed, col="red", pch=17)
points(mnar_ranF_original, col="blue", pch=19)
legend('topright', c("imputed", "original") , col=c('red', 'blue'), pch=c(17,19), bty='o', pt.cex=2,text.font=2)
dev.off()

# Prepare for classification based on regression (happens later in another file)
# Therefore, replace NA's in the original data with imputed values and save them in a vector + check
old_town_imp_ranF <- working_set$X100_DistanceAlcudiaOldTown
old_town_imp_ranF[IndexOfMissing_old_town] <- mnar_fit_ranF_pred_ten
cbind(old_town_imp_ranF,working_set$X100_DistanceAlcudiaOldTown)

#=== START: Preparation for Classification ===
distToBeach_imp <- distToBeach_imp_ranF
rooms_imp <- rooms_imp_ranF
old_town_imp <- old_town_imp_ranF