
source("DA2_avengers_require.R")
source("DA2_avengers_install.R")


#=== Merge N/As of factor and numerical feature===
mcar_merged <- mcar_working_set

og_city <- working_set[which(is.na(mcar_merged$X004_City)),c("X004_City")]
og_dist <- working_set[which(is.na(mcar_merged$X102_DistanceToBeach)),c("X102_DistanceToBeach")]
mcar_merged$X004_City <- as.character(mcar_merged$X004_City)
mcar_merged$X004_City <- as.factor(mcar_merged$X004_City)
#=== END: Merging ===

#=== Lists patterns of missing data --> 10 are missing for Feature X102 and x004. Total of 20 NAs===
md.pattern(mcar_merged[, c("X004_City", "X100_DistanceAlcudiaOldTown", "X102_DistanceToBeach")]) #
#======#


#=== START: Apply visualization techniques for missing data===

marginplot(mcar_merged[, c("X100_DistanceAlcudiaOldTown", "X102_DistanceToBeach")], ylim = c(-1, 20))
sum(is.na(mcar_merged[,c("X004_City")]))

marginplot(mcar_merged[, c("X100_DistanceAlcudiaOldTown", "X102_DistanceToBeach")], ylim = c(-1, 3))  #zoomed in
marginplot(mcar_merged[, c("X004_City", "X102_DistanceToBeach")], ylim = c(-1, 5)) #proof for full-functional mcar method
###=end review before deleting####


##### MCAR: 
city_unique <- unique(mcar_merged$X004_City)
city_num <- seq(0,length(city_unique)-1,1)
city_ma <- cbind(city_unique,city_num)

city_ma_all <- merge(mcar_merged$X004_City,city_ma, by.x = "x", by.y = "city_unique")
city_ma_all[which(is.na(city_ma_all$x)),"city_num"] <- NA

city_beach_ma <- cbind(mcar_merged$X102_DistanceToBeach, city_ma_all[,"city_num"]) # vectors have a different length | x(without outlier) =  "mcar_merged_non_out" 

barplot(table(mcar_working_set$X004_City), las=2)

#####

##### MNAR: 
payoption_comb_unique <- unique(mnar_working_set$X046_PayOptions)
payoption_comb_num <- seq(0,length(payoption_comb_unique)-1,1)
payoption_comb_ma <- cbind(payoption_comb_unique,payoption_comb_num)

payoption_comb_all <- merge(mnar_merged$X046_PayOptions, payoption_comb_ma, by.x ="x",by.y="payoption_comb_unique")
payoption_comb_all[which(is.na(payoption_comb_all$x)),"payoption_comb_num"] <- NA 

oldtown_payoption_comb_ma <- cbind(payoption_comb_all[,"payoption_comb_num"], mnar_merged$X100_DistanceAlcudiaOldTown)
marginplot(oldtown_payoption_comb_ma)
#####

####
par(mfrow=c(2,2))
marginplot(city_beach_ma)
marginplot(oldtown_payoption_comb_ma)
marginplot(postcode_rooms_ma)
####


#analysis for both features values at the same time
MCAR_mice_100_dual_pmm <- mice(mcar_merged, method='pmm', m=100, seed=06041992)
#For City: 4 correct, 9.14% hitrate
#For Distance to Beach: 1 correct, 38% hitrate, mse = 12.88144
MCAR_mice_100_dual_cart <- mice(mcar_merged, method='cart', m=100, seed=06041992)
#For City: 4 correct, hitrate: 44.3% hitrate
#For Distance to Beach: 3 correct, hitrate: 49.8%, mse = 7.181396
MCAR_mice_100_dual_rf <- mice(mcar_merged, method='rf', m=100, seed=06041992)
#For city: 7 correct, hitrate: 56.2%
#For Distance to Beach: 5 correct, hitrate: 52.8% ,mse = 3.263968


#mice only for the city feature with various methods. lda (Linear Discriminant Analysis). Works exceptionally well!
#run one of the following mice mehtods before you compare it with city_imp100_hits 
MCAR_mice_100_city_pmm <- mice(mcar_merged[,-which(colnames(mcar_merged)=="X102_DistanceToBeach")], method='pmm', m=100, seed=06041992)
#last iteration: 7 correct, hitrate = 68.5%
MCAR_mice_100_city_cart <- mice(mcar_merged[,-which(colnames(mcar_merged)=="X102_DistanceToBeach")], method='cart', m=100, seed=06041992)
#last iteration: 4 correct hitrate = 44.6%
MCAR_mice_100_city_rf <- mice(mcar_merged[,-which(colnames(mcar_merged)=="X102_DistanceToBeach")], method='rf', m=100, seed=06041992)

#mice only for numerical feature 

MCAR_mice_5 <- mice(mcar_merged[,-which(colnames(mcar_merged)=="X004_City")], method='pmm', m=5, seed=06041992)
MCAR_mice_10 <- mice(mcar_merged[,-which(colnames(mcar_merged)=="X004_City")], method='pmm', m=10, seed=06041992)
MCAR_mice_50 <- mice(mcar_merged[,-which(colnames(mcar_merged)=="X004_City")], method='pmm', m=50, seed=06041992)
system.time(MCAR_mice_100 <- mice(mcar_merged[,-which(colnames(mcar_merged)=="X004_City")], method='pmm', m=100, seed=06041992))

MCAR_mice_100_dist_pmm <- mice(mcar_merged[,-which(colnames(mcar_merged)=="X004_City")], method='pmm', m=100, seed=06041992)
MCAR_mice_100_dist_cart <- mice(mcar_merged[,-which(colnames(mcar_merged)=="X004_City")], method='cart', m=100, seed=06041992)
#last iteration= 6 correct, mse = 3.897546
MCAR_mice_100_dist_rf <- mice(mcar_merged[,-which(colnames(mcar_merged)=="X004_City")], method='rf', m=100, seed=06041992)
#last itearation: 4 correct, mse = 5.13405

#=====CHOOSE YOUR MICE OBJECT=======
MCAR_mice_100 <- MCAR_mice_100_dual_pmm

###====START Compare imputations to original data by calculating deviation===
# Get original data from working_set, which need to be compared to imputed values from MICE:
orig_data <- matrix(working_set[which(rownames(working_set) %in% rownames(MCAR_mice_5$imp$X102_DistanceToBeach)), "X102_DistanceToBeach"])

orig_mean_values <- mean(orig_data)
orig_var_values <- var(orig_data)

# For comparison generate matrix with origuinal value in size of the mize imputation dimension (5 / 10 / 50 / 100 imputations):
MCAR_orig_data_matrix_5 = matrix(orig_data, nrow = nrow(orig_data), ncol = ncol(MCAR_mice_5$imp$X102_DistanceToBeach))
ratio_matrix_5 = matrix(0, nrow = nrow(MCAR_orig_data_matrix_5), ncol = ncol(MCAR_orig_data_matrix_5))

MCAR_orig_data_matrix_10 = matrix(orig_data, nrow = nrow(orig_data), ncol = ncol(MCAR_mice_10$imp$X102_DistanceToBeach))
ratio_matrix_10 = matrix(0, nrow = nrow(MCAR_orig_data_matrix_10), ncol = ncol(MCAR_orig_data_matrix_10))

MCAR_orig_data_matrix_50 = matrix(orig_data, nrow = nrow(orig_data), ncol = ncol(MCAR_mice_50$imp$X102_DistanceToBeach))
ratio_matrix_50 = matrix(0, nrow = nrow(MCAR_orig_data_matrix_50), ncol = ncol(MCAR_orig_data_matrix_50))

MCAR_orig_data_matrix_100 = matrix(orig_data, nrow = nrow(orig_data), ncol = ncol(MCAR_mice_100$imp$X102_DistanceToBeach))
ratio_matrix_100 = matrix(0, nrow = nrow(MCAR_orig_data_matrix_100), ncol = ncol(MCAR_orig_data_matrix_100))


# Calculate and compare values to original data
for(i in 1:nrow(ratio_matrix_5)){
  for(j in 1:ncol(ratio_matrix_5)){
    ratio_matrix_5[i,j] <- as.matrix(MCAR_mice_5$imp$X102_DistanceToBeach[i,j]) / MCAR_orig_data_matrix_5[i,j]
  }
}

for(i in 1:nrow(ratio_matrix_10)){
  for(j in 1:ncol(ratio_matrix_10)){
    ratio_matrix_10[i,j] <- as.matrix(MCAR_mice_10$imp$X102_DistanceToBeach[i,j]) / MCAR_orig_data_matrix_10[i,j]
  }
}

for(i in 1:nrow(ratio_matrix_50)){
  for(j in 1:ncol(ratio_matrix_50)){
    ratio_matrix_50[i,j] <- as.matrix(MCAR_mice_50$imp$X102_DistanceToBeach[i,j]) / MCAR_orig_data_matrix_50[i,j]
  }
}

for(i in 1:nrow(ratio_matrix_100)){
  for(j in 1:ncol(ratio_matrix_100)){
    ratio_matrix_100[i,j] <- as.matrix(MCAR_mice_100$imp$X102_DistanceToBeach[i,j]) / MCAR_orig_data_matrix_100[i,j]
  }
}


MCAR_comp_matrix5 <- matrix(0, nrow = 2, ncol = ncol(ratio_matrix_5))
rownames(MCAR_comp_matrix5) <- c("imp_mean", "imp_variance")

MCAR_comp_matrix10 <- matrix(0, nrow = 2, ncol = ncol(ratio_matrix_10))
rownames(MCAR_comp_matrix10) <- c("imp_mean", "imp_variance")

MCAR_comp_matrix50 <- matrix(0, nrow = 2, ncol = ncol(ratio_matrix_50))
rownames(MCAR_comp_matrix50) <- c("imp_mean", "imp_variance")

MCAR_comp_matrix100 <- matrix(0, nrow = 2, ncol = ncol(ratio_matrix_100))
rownames(MCAR_comp_matrix100) <- c("imp_mean", "imp_variance")

for(i in 1:ncol(MCAR_comp_matrix5)){
  MCAR_comp_matrix5[1, i] <- mean(ratio_matrix_5[, i]) / orig_mean_values
  MCAR_comp_matrix5[2, i] <- var(ratio_matrix_5[, i]) / orig_var_values
}

for(i in 1:ncol(MCAR_comp_matrix10)){
  MCAR_comp_matrix10[1, i] <- mean(ratio_matrix_10[, i]) / orig_mean_values
  MCAR_comp_matrix10[2, i] <- var(ratio_matrix_10[, i]) / orig_var_values
}

for(i in 1:ncol(MCAR_comp_matrix50)){
  MCAR_comp_matrix50[1, i] <- mean(ratio_matrix_50[, i]) / orig_mean_values
  MCAR_comp_matrix50[2, i] <- var(ratio_matrix_50[, i]) / orig_var_values
}

for(i in 1:ncol(MCAR_comp_matrix100)){
  MCAR_comp_matrix100[1, i] <- mean(ratio_matrix_100[, i]) / orig_mean_values
  MCAR_comp_matrix100[2, i] <- var(ratio_matrix_100[, i]) / orig_var_values
}


###====END Compare imputations to original data by calculating deviations ===


###====START WORKFLOW FOR COMPUTING CITY SIMILARITIES====
city_imp100_hits <- 0
city_imp100_hitm <- matrix(NA,nrow = 10, ncol=100)
for(i in 1:10){
  for(j in 1:100){
    if (as.matrix(MCAR_mice_100$imp$X004_City[i,j]) == og_city[i]){
      city_imp100_hits <- city_imp100_hits + 1
      city_imp100_hitm[i,j] <- 1
    }
  }
}
city_imp100_hits/1000
solpattern <- md.pattern(t(city_imp100_hitm)) #solution patterns
View(solpattern) 

#precision rate
city_imp100_hits/1000

#what if we would take the factors which occur the most in between 100 iterations of imputations?
decision_for_city <- matrix(nrow=10,ncol=3)
for(i in 1:10){
  tableObj <- table(t(MCAR_mice_100$imp$X004_City[i,]))
  og_count <- 0
  for(j in 1:length(tableObj)) 
    if(names(tableObj)[j]==og_city[i])
      og_count <- as.numeric(tableObj[which(rownames(tableObj)==og_city[i])])
  decision_for_city[i,1] <- og_count
  decision_for_city[i,2] <- names(which.max(table(t(MCAR_mice_100$imp$X004_City)[,i])))
  decision_for_city[i,3] <- as.numeric(sort(table(t(MCAR_mice_100$imp$X004_City[i,])), decreasing=T)[1])
}
decision_for_city

#compute a matrix that returns 1 for a correct imputation, 0 elseway
plainmatrix <- matrix(0,nrow=10, ncol=1) 
for(i in 1:10) { 
  if(decision_for_city[i,2]==og_city[i]) 
  plainmatrix[i]=1
}
plainmatrix

#build up matrix, consisting of: row index of observations in question, proposed value, actual value, and an indicator for matching
comparison_for_city <- cbind( indicies <- which(is.na(mcar_merged$X004_City)), as.character(og_city), decision_for_city, plainmatrix)
colnames(comparison_for_city) <- c("Index", "Original Value","Count","Decision","Count", "Hit")
View(comparison_for_city)
###====END WORKFLOW FOR COMPUTING CITY SIMILARITIES====

##====start SOME VISUALISATIONS FOR CITY SIMILARITIES====
#construct a barplot for the resulting observation for the city feature
mcar_city_with_dfc <- mcar_working_set$X004_City
j <- 1
for(i in 1:length(mcar_city_with_dfc)){
  if(is.na(mcar_city_with_dfc[i])){
    mcar_city_with_dfc[i] <- decision_for_city[j,2] 
    j <- j + 1  
  }  
}
comparison_for_city
##Histogram for keeping track of the distribution's difference
par(mfcol = c(5,1))
for(i in 1:5)  
  barplot(sort(table(t(MCAR_mice_100$imp$X004_City)[,i]), decreasing=T)[1:5])
par(mfcol = c(1,1))
##====end SOME VISUALISATIONS FOR CITY SIMILARITIES====

#======START WORKFLOW FOR COMPUTING DISTANCE SIMILARITIES ========
# *an imputation is considered a hit if its value is as close as 0.5km to the original value
dist_imp100_hits <- 0
dist_imp100_hitm <- matrix(NA,nrow = 10, ncol=100)
for(i in 1:10){
  for(j in 1:100){
    if ( MCAR_mice_100$imp$X102_DistanceToBeach[i,j] > (og_dist[i] - 0.5) && MCAR_mice_100$imp$X102_DistanceToBeach[i,j] < (og_dist[i] + 0.5)) {
      dist_imp100_hits <- dist_imp100_hits + 1
      dist_imp100_hitm[i,j] <- 1
    }
  }
}
#precision rate
View(dist_imp100_hitm)
View(md.pattern(t(dist_imp100_hitm)))
dist_imp100_hits/1000


#what if we would take the mean of the corresponding distances?
decision_for_dist <- matrix(nrow=10,ncol=1)
for(i in 1:10){
  decision_for_dist[i,] <- rowMeans(MCAR_mice_100$imp$X102_DistanceToBeach)[i]
}
decision_for_dist

#compute a matrix that returns 1 for a correct imputation, 0 elseway
plainmatrix <- matrix(0,nrow=10, ncol=1) 
diff_dist_m <- matrix(0,nrow=10, ncol=1)
for(i in 1:10) { 
  if((decision_for_dist[i] < (og_dist[i] + 0.5)) && (decision_for_dist[i] > (og_dist[i] - 0.5)))
    plainmatrix[i]=1
  diff_dist_m[i] <- abs(og_dist[i] - decision_for_dist[i])  
}

#build up matrix, consisting of: row index of observations in question, proposed value, actual value, and an indicator for matching
comparison_for_dist <- cbind( indicies <- which(is.na(mcar_merged$X102_DistanceToBeach)), og_dist, decision_for_dist, diff_dist_m, plainmatrix)
View(comparison_for_dist)

#computation for mse
ss <- 0
for(i in 1:10){
  ss <- ss + diff_dist_m[i]^2 
}
mse <- ss/10 
mse
#======END WORKFLOW FOR COMPUTING DISTANCE SIMILARITIES ========

#=====START SOME VISUALISATIONS FOR DISTANCES=======

original_points <-  working_set[which(rownames(mcar_working_set) %in% rownames(MCAR_mice_100$imp$X102_DistanceToBeach)), c("X102_DistanceToBeach","X100_DistanceAlcudiaOldTown")]
decision_points <- cbind(decision_for_dist, mcar_working_set$X100_DistanceAlcudiaOldTown[(which(is.na(mcar_merged$X102_DistanceToBeach)))])

plot(mcar_working_set[,c("X102_DistanceToBeach","X100_DistanceAlcudiaOldTown")], xlim=c(0,9))
points(original_points, col="blue",pch=19, cex=1.5)
points(decision_points, col="red", pch=19, cex=1.5)
legend("topright", c("Original Values", "Imputed Values"), pch=c(19,19), col=c("blue","red"))

#=====END SOME VISUALISATIONS FOR DISTANCES========


###====START Compare imputations to original data by calculating mean, variance ===

# Check mean & variance for original dataset:
orig_mean <- mean(working_set[, c("X102_DistanceToBeach")])
orig_var <- var(working_set[, c("X102_DistanceToBeach")])

#Generate a matrix with the column of missing values imputed with MICE in the size of m (number of MICE imputations)
MCAR_value_matrix_5 = matrix(mcar_merged$X102_DistanceToBeach, nrow = nrow(mcar_merged), ncol = ncol(MCAR_mice_5$imp$X102_DistanceToBeach))
MCAR_value_matrix_10 = matrix(mcar_merged$X102_DistanceToBeach, nrow = nrow(mcar_merged), ncol = ncol(MCAR_mice_10$imp$X102_DistanceToBeach))
MCAR_value_matrix_50 = matrix(mcar_merged$X102_DistanceToBeach, nrow = nrow(mcar_merged), ncol = ncol(MCAR_mice_50$imp$X102_DistanceToBeach))
MCAR_value_matrix_100 = matrix(mcar_merged$X102_DistanceToBeach, nrow = nrow(mcar_merged), ncol = ncol(MCAR_mice_100$imp$X102_DistanceToBeach))
#And assign rownames
rownames(MCAR_value_matrix_5) <- rownames(mcar_merged)
rownames(MCAR_value_matrix_10) <- rownames(mcar_merged)
rownames(MCAR_value_matrix_50) <- rownames(mcar_merged)
rownames(MCAR_value_matrix_100) <- rownames(mcar_merged)


#Now insert the imputed values from the MICE package into each row of generated matrix:
for(i in 1:nrow(MCAR_value_matrix_5)){
  for(j in 1:ncol(MCAR_value_matrix_5)){
    for(z in 1:nrow(MCAR_mice_5$imp$X102_DistanceToBeach)){
      if (rownames(MCAR_mice_5$imp$X102_DistanceToBeach)[z] == rownames(MCAR_value_matrix_5)[i]){
        MCAR_value_matrix_5[i,j] <- as.matrix(MCAR_mice_5$imp$X102_DistanceToBeach[z,j])}}}}

#Now insert the imputed values from the MICE package into each row of generated matrix:
for(i in 1:nrow(MCAR_value_matrix_10)){
  for(j in 1:ncol(MCAR_value_matrix_10)){
    for(z in 1:nrow(MCAR_mice_10$imp$X102_DistanceToBeach)){
      if (rownames(MCAR_mice_10$imp$X102_DistanceToBeach)[z] == rownames(MCAR_value_matrix_10)[i]){
        MCAR_value_matrix_10[i,j] <- as.matrix(MCAR_mice_10$imp$X102_DistanceToBeach[z,j])}}}}

#Now insert the imputed values from the MICE package into each row of generated matrix:
for(i in 1:nrow(MCAR_value_matrix_50)){
  for(j in 1:ncol(MCAR_value_matrix_50)){
    for(z in 1:nrow(MCAR_mice_50$imp$X102_DistanceToBeach)){
      if (rownames(MCAR_mice_50$imp$X102_DistanceToBeach)[z] == rownames(MCAR_value_matrix_50)[i]){
        MCAR_value_matrix_50[i,j] <- as.matrix(MCAR_mice_50$imp$X102_DistanceToBeach[z,j])}}}}

#Now insert the imputed values from the MICE package into each row of generated matrix:
for(i in 1:nrow(MCAR_value_matrix_100)){
  for(j in 1:ncol(MCAR_value_matrix_100)){
    for(z in 1:nrow(MCAR_mice_100$imp$X102_DistanceToBeach)){
      if (rownames(MCAR_mice_100$imp$X102_DistanceToBeach)[z] == rownames(MCAR_value_matrix_100)[i]){
        MCAR_value_matrix_100[i,j] <- as.matrix(MCAR_mice_100$imp$X102_DistanceToBeach[z,j])}}}}

#Now check how good mice captured the original mean and variance
#As sapply does not work for some reason this is a workaroundwith a comparison-matrix  old: sapply(value_matrix, function(y) c(VM_mean = mean(y), VM_variance = var(y)))
MCAR_comparison_matrix5 = matrix(0, nrow = 2, ncol = ncol(MCAR_value_matrix_5))
rownames(MCAR_comparison_matrix5) <- c("imp_mean", "imp_variance")

MCAR_comparison_matrix10 = matrix(0, nrow = 2, ncol = ncol(MCAR_value_matrix_10))
rownames(MCAR_comparison_matrix10) <- c("imp_mean", "imp_variance")

MCAR_comparison_matrix50 = matrix(0, nrow = 2, ncol = ncol(MCAR_value_matrix_50))
rownames(MCAR_comparison_matrix50) <- c("imp_mean", "imp_variance")

MCAR_comparison_matrix100 = matrix(0, nrow = 2, ncol = ncol(MCAR_value_matrix_100))
rownames(MCAR_comparison_matrix100) <- c("imp_mean", "imp_variance")

# Compare to original mean --> 1 = perfect match, while > 1 or < 1 represent deviations
for(i in 1:ncol(MCAR_comparison_matrix5)){
  MCAR_comparison_matrix5[1, i] <- mean(MCAR_value_matrix_5[, i]) / orig_mean
  MCAR_comparison_matrix5[2, i] <- var(MCAR_value_matrix_5[, i]) / orig_var
}

for(i in 1:ncol(MCAR_comparison_matrix10)){
  MCAR_comparison_matrix10[1, i] <- mean(MCAR_value_matrix_10[, i]) / orig_mean
  MCAR_comparison_matrix10[2, i] <- var(MCAR_value_matrix_10[, i]) / orig_var
}

for(i in 1:ncol(MCAR_comparison_matrix50)){
  MCAR_comparison_matrix50[1, i] <- mean(MCAR_value_matrix_50[, i]) / orig_mean
  MCAR_comparison_matrix50[2, i] <- var(MCAR_value_matrix_50[, i]) / orig_var
}

for(i in 1:ncol(MCAR_comparison_matrix100)){
  MCAR_comparison_matrix100[1, i] <- mean(MCAR_value_matrix_100[, i]) / orig_mean
  MCAR_comparison_matrix100[2, i] <- var(MCAR_value_matrix_100[, i]) / orig_var
}

best_mean5 <- MCAR_comparison_matrix5[1, which.min(abs(MCAR_comparison_matrix5[1,1:ncol(MCAR_comparison_matrix5)]-1))] # Best imputation performed for mean
best_var5 <- MCAR_comparison_matrix5[2, which.min(abs(MCAR_comparison_matrix5[2,1:ncol(MCAR_comparison_matrix5)]-1))] # Best imputation performed for variance
best_val5 <- MCAR_comparison_matrix5[, which.min(abs(MCAR_comparison_matrix5[1,1:ncol(MCAR_comparison_matrix5)]-1) + abs(MCAR_comparison_matrix5[2,1:ncol(MCAR_comparison_matrix5)]-1))] # Best imputation with mean & variance combined
bestcol5 <- which.min(abs(MCAR_comparison_matrix5[1,1:ncol(MCAR_comparison_matrix5)]-1) + abs(MCAR_comparison_matrix5[2,1:ncol(MCAR_comparison_matrix5)]-1))

best_mean10 <- MCAR_comparison_matrix10[1, which.min(abs(MCAR_comparison_matrix10[1,1:ncol(MCAR_comparison_matrix10)]-1))] # Best imputation performed for mean
best_var10 <- MCAR_comparison_matrix10[2, which.min(abs(MCAR_comparison_matrix10[2,1:ncol(MCAR_comparison_matrix10)]-1))] # Best imputation performed for variance
best_val10 <-MCAR_comparison_matrix10[, which.min(abs(MCAR_comparison_matrix10[1,1:ncol(MCAR_comparison_matrix10)]-1) + abs(MCAR_comparison_matrix10[2,1:ncol(MCAR_comparison_matrix10)]-1))] # Best imputation with mean & variance combined
bestcol10 <- which.min(abs(MCAR_comparison_matrix10[1,1:ncol(MCAR_comparison_matrix10)]-1) + abs(MCAR_comparison_matrix10[2,1:ncol(MCAR_comparison_matrix10)]-1))

best_mean50 <- MCAR_comparison_matrix50[1, which.min(abs(MCAR_comparison_matrix50[1,1:ncol(MCAR_comparison_matrix50)]-1))] # Best imputation performed for mean
best_var50 <- MCAR_comparison_matrix50[2, which.min(abs(MCAR_comparison_matrix50[2,1:ncol(MCAR_comparison_matrix50)]-1))] # Best imputation performed for variance
best_val50 <-MCAR_comparison_matrix50[, which.min(abs(MCAR_comparison_matrix50[1,1:ncol(MCAR_comparison_matrix50)]-1) + abs(MCAR_comparison_matrix50[2,1:ncol(MCAR_comparison_matrix50)]-1))] # Best imputation with mean & variance combined
bestcol50 <- which.min(abs(MCAR_comparison_matrix50[1,1:ncol(MCAR_comparison_matrix50)]-1) + abs(MCAR_comparison_matrix50[2,1:ncol(MCAR_comparison_matrix50)]-1))

best_mean100 <- MCAR_comparison_matrix100[1, which.min(abs(MCAR_comparison_matrix100[1,1:ncol(MCAR_comparison_matrix100)]-1))] # Best imputation performed for mean
best_var100 <- MCAR_comparison_matrix100[2, which.min(abs(MCAR_comparison_matrix100[2,1:ncol(MCAR_comparison_matrix100)]-1))] # Best imputation performed for variance
best_val100 <-MCAR_comparison_matrix100[, which.min(abs(MCAR_comparison_matrix100[1,1:ncol(MCAR_comparison_matrix100)]-1) + abs(MCAR_comparison_matrix100[2,1:ncol(MCAR_comparison_matrix100)]-1))] # Best imputation with mean & variance combined
bestcol100 <- which.min(abs(MCAR_comparison_matrix100[1,1:ncol(MCAR_comparison_matrix100)]-1) + abs(MCAR_comparison_matrix100[2,1:ncol(MCAR_comparison_matrix100)]-1))



# get rownames to add additional columns for plot
# We consider the best values for each imputation regarding mean and variance
# Also update overview table (comparison_for_dist) in the following steps
best_imp5 <- as.matrix(MCAR_mice_5$imp$X102_DistanceToBeach[,bestcol5])
comparison_for_dist <- cbind(comparison_for_dist, best_imp5)
colnames(comparison_for_dist)[ncol(comparison_for_dist)] <- "Best_of5"
mse_best5 <- mse(sim = comparison_for_dist[, ncol(comparison_for_dist)], obs = comparison_for_dist[, 2])
rownames(best_imp5) <- rownames(MCAR_mice_5$imp$X102_DistanceToBeach)

best_imp10 <- as.matrix(MCAR_mice_10$imp$X102_DistanceToBeach[,bestcol10])
comparison_for_dist <- cbind(comparison_for_dist, best_imp10)
colnames(comparison_for_dist)[ncol(comparison_for_dist)] <- "Best_of10"
mse_best10 <- mse(sim = comparison_for_dist[, ncol(comparison_for_dist)], obs = comparison_for_dist[, 2])
rownames(best_imp10) <- rownames(MCAR_mice_10$imp$X102_DistanceToBeach)

best_imp50 <- as.matrix(MCAR_mice_50$imp$X102_DistanceToBeach[,bestcol50])
comparison_for_dist <- cbind(comparison_for_dist, best_imp50)
colnames(comparison_for_dist)[ncol(comparison_for_dist)] <- "Best_of50"
mse_best50 <- mse(sim = comparison_for_dist[, ncol(comparison_for_dist)], obs = comparison_for_dist[, 2])
rownames(best_imp50) <- rownames(MCAR_mice_50$imp$X102_DistanceToBeach)

best_imp100 <- as.matrix(MCAR_mice_100$imp$X102_DistanceToBeach[,bestcol100])
comparison_for_dist <- cbind(comparison_for_dist, best_imp100)
colnames(comparison_for_dist)[ncol(comparison_for_dist)] <- "Best_of100"
mse_best100 <- mse(sim = comparison_for_dist[, ncol(comparison_for_dist)], obs = comparison_for_dist[, 2])
rownames(best_imp100) <- rownames(MCAR_mice_10$imp$X102_DistanceToBeach)

# Put all calculated MSEs into an array to recheck
c(mse_best5, mse_best10, mse_best50, mse_best100)

# Now add an additional column for plotting
best_imp5 <- cbind(best_imp5, working_set[which(rownames(working_set) %in% rownames(best_imp5)), "X100_DistanceAlcudiaOldTown"])

best_imp10 <- cbind(best_imp10, working_set[which(rownames(working_set) %in% rownames(best_imp10)), "X100_DistanceAlcudiaOldTown"])

best_imp50 <- cbind(best_imp50, working_set[which(rownames(working_set) %in% rownames(best_imp50)), "X100_DistanceAlcudiaOldTown"])

best_imp100 <- cbind(best_imp100, working_set[which(rownames(working_set) %in% rownames(best_imp100)), "X100_DistanceAlcudiaOldTown"])


#Plot best results for comparison by adding an additional numeric feature
orig_data_add_column <- working_set[which(rownames(working_set) %in% rownames(MCAR_mice_5$imp$X102_DistanceToBeach)), c("X102_DistanceToBeach", "X100_DistanceAlcudiaOldTown")]


par(mfrow=c(2,2))
plot(working_set[, c("X102_DistanceToBeach" , "X100_DistanceAlcudiaOldTown")], xlim = c(0, 9), main = "5 Imputations")
points(orig_data_add_column, col = "blue", pch = 19, cex =2)
points(best_imp5, col = "orange", pch = 15, cex =2)
plot(working_set[, c("X102_DistanceToBeach" , "X100_DistanceAlcudiaOldTown")], xlim = c(0, 9), main = "10 Imputations")
points(orig_data_add_column, col = "blue", pch = 19, cex =2)
points(best_imp10, col = "green", pch = 16, cex =2)
plot(working_set[, c("X102_DistanceToBeach" , "X100_DistanceAlcudiaOldTown")], xlim = c(0, 9), main = "50 Imputations")
points(orig_data_add_column, col = "blue", pch = 19, cex =2)
points(best_imp50, col = "violet", pch = 17, cex =2)
plot(working_set[, c("X102_DistanceToBeach" , "X100_DistanceAlcudiaOldTown")], xlim = c(0, 9), main = "100 Imputations")
points(orig_data_add_column, col = "blue", pch = 19, cex =2)
points(best_imp100, col = "red", pch = 19, cex =2)


#By how much get the results of mean and variance better with an increased number of computations?:
#Results greater 2 indicate here that the mean variance worsens
(abs(1-best_mean5) + abs(1-best_mean10)) / abs(1-best_mean5)
(abs(1-best_mean10) + abs(1-best_mean50)) / abs(1-best_mean10)
(abs(1-best_mean50) + abs(1-best_mean100)) / abs(1-best_mean50)

(abs(1-best_var5) + abs(1-best_var10)) / abs(1-best_var5) #as variance worsens 
(abs(1-best_var10) + abs(1-best_var50)) / abs(1-best_var10)
(abs(1-best_var50) + abs(1-best_var100)) / abs(1-best_var50)


#Means and variances of imputed data (Backup):
sapply(MCAR_mice_100$imp$X102_DistanceToBeach, function(x) c(mean = mean(x), variance = var(x)))
View(MCAR_value_matrix_100)

###====END Compare imputations to original data by calculating mean, variance ===
