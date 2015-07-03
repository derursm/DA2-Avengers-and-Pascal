source("DA2_avengers_require.R")
#source("DA2_avengers_install.R")


mar_merged <- mar_working_set

og_POSTCODE <- working_set[which(is.na(mar_merged$X003_Postcode)),c("X003_Postcode")]
og_norooms <- working_set[which(is.na(mar_merged$X012_NoRooms)),c("X012_NoRooms")]
mar_merged[,c("X003_Postcode")] <- as.factor(mar_merged[, c("X003_Postcode")])

# Test for correct N/As
which(is.na(mar_merged[,c("X003_Postcode")]))
which(is.na(mar_merged[,c("X012_NoRooms")]))
which(is.na(mar_merged[,c("X012_NoRooms")])&is.na(mar_merged[,c("X003_Postcode")]))
postcode_rooms_ma <- cbind(mar_merged$X003_Postcode, mar_merged$X012_NoRooms)
marginplot(postcode_rooms_ma)

barplot(sort(table(mar_merged$X003_Postcode)), ylim=c(0,40), las=2, main="Distribution of Postcodes")
hist((mar_merged$X012_NoRooms), ylim=c(0,40), xlim=c(0,800), breaks=100, main="Distribution of Postcodes")
#####


#=== START: MAR MICE IMPUTATIONS OF NUMERIC MISSING VALUES & COMPARISON TO ORIGINAL VALUES ===
MAR_mice_100_dual_pmm <- mice(mar_merged, method='pmm', m=100, seed=06041992)
#For City: 0 correct, 0% hitrate. But you may take a look at the barplot!
#For NoRooms: 0 correct, 0.3% hitrate, mse_median = 6366, mse_mean = 3563.3
MAR_mice_100_dual_sample <- mice(mar_merged, method='sample', m=100, seed=06041992)
#For City: 0 correct, 0% hitrate. Not even close.
#For NoRooms 0 correct, 0.4% hitrate, mse_median = 3978.6 , mse_mean = 4304.7 
MAR_mice_100_dual_cart <- mice(mar_merged, method='cart', m=100, seed=06041992)
#For City: 0 correct, 0% hitrate. Always Imputes 7589 instead of 7590.
#For NoROoms: 0 correct, 0.4% hitrate, mse_median = 7972.8, mse_mean = 6397.9  9th value guessed correct 4 times. look at distribution!
MAR_mice_100_dual_rf <- mice(mar_merged, method='rf', m=100, seed=06041992)
#For City: very similar to cart but some minor diversity in imputations
#For NoRooms: 0 correct, 1,3% hitrate, mse_median = 6589.5, mse_mean = 5044.4

#TODO: WOULD BE INTERESTING TO LOOK AT THE SOLUTIONS WITH A TOLERANCE OF 1-5 NEAREST ZIP CODES

#mice only for the POSTCODE feature with various methods. lda (Linear Discriminant Analysis). Works exceptionally well!
#run one of the following mice mehtods before you compare it with POSTCODE_imp100_hits 
MAR_mice_100_postcode_lda <- mice(mar_merged[,-which(colnames(mar_merged)=="X012_NoRooms")], method='lda', m=100, seed=06041992)
#For City: same as cart (dual imp) with only one exception: 7580
MAR_mice_100_postcode_sample <- mice(mar_merged[,-which(colnames(mar_merged)=="X012_NoRooms")], method='sample', m=100, seed=06041992)
# meh
MAR_mice_100_postcode_cart <- mice(mar_merged[,-which(colnames(mar_merged)=="X012_NoRooms")], method='cart', m=100, seed=06041992)
#see above
MAR_mice_100_postcode_rf <- mice(mar_merged[,-which(colnames(mar_merged)=="X012_NoRooms")], method='rf', m=100, seed=06041992)
#see above

#mice only for numerical feature 
MAR_mice_5 <- mice(mar_merged[,-which(colnames(mar_merged)=="X003_Postcode")], method='pmm', m=5, seed=06041992)
MAR_mice_10 <- mice(mar_merged[,-which(colnames(mar_merged)=="X003_Postcode")], method='pmm', m=10, seed=06041992)
MAR_mice_50 <- mice(mar_merged[,-which(colnames(mar_merged)=="X003_Postcode")], method='pmm', m=50, seed=06041992)
system.time(MAR_mice_100_norooms_pmm <- mice(mar_merged[,-which(colnames(mar_merged)=="X003_Postcode")], method='pmm', m=100, seed=06041992))
#For NoRooms: 0 correct, 0,5% hitrate, mse_median = 4535.1, mse_mean = 4772.3 (range for imputations quite big!)
MAR_mice_100_norooms_cart <- mice(mar_merged[,-which(colnames(mar_merged)=="X003_Postcode")], method='cart', m=100, seed=06041992)
#For NoRooms: 0 correct, 1,3% hitrate, "MSE Modal: 15059.4", "MSE Mean: 5461.1", "MSE Median: 4463.9"
#predicted the first value correct 13 times!
MAR_mice_100_norooms_rf <- mice(mar_merged[,-which(colnames(mar_merged)=="X003_Postcode")], method='rf', m=100, seed=06041992)
#For NoRooms: 0 correct, 0,8% hitrate,"MSE Modal: 13530", "MSE Mean: 5930.6", "MSE Median: 7468.7"
#predicted the 4th value correct 5 times !


#=====CHOSE YOUR MICE OBJECT======
MAR_mice_100 <- MAR_mice_100_norooms_pmm
#=====CHOSE YOUR MICE OBJECT======


###====START WORKFLOW FOR COMPUTING POSTCODE SIMILARITIES====
# Execute only when postcode is imputed by MICE
POSTCODE_imp100_hits <- 0
POSTCODE_imp100_hitm <- matrix(NA,nrow = 10, ncol=100)
for(i in 1:10){
  for(j in 1:100){
    if (as.matrix(MAR_mice_100$imp$X003_Postcode[i,j]) == og_POSTCODE[i]){
      POSTCODE_imp100_hits <- POSTCODE_imp100_hits + 1
      POSTCODE_imp100_hitm[i,j] <- 1
    }
  }
}
POSTCODE_imp100_hits/1000
solpattern <- md.pattern(t(POSTCODE_imp100_hitm)) #solution patterns
#View(solpattern) 

#precision rate
POSTCODE_imp100_hits/1000

#what if we would take the factors which occur the most in between 100 iterations of imputations?
decision_for_POSTCODE <- matrix(nrow=10,ncol=3)
for(i in 1:10){
  tableObj <- table(t(MAR_mice_100$imp$X003_Postcode[i,]))
  og_count <- 0
  for(j in 1:length(tableObj)) 
    if(names(tableObj)[j]==og_POSTCODE[i])
      og_count <- as.numeric(tableObj[which(rownames(tableObj)==og_POSTCODE[i])])
  decision_for_POSTCODE[i,1] <- og_count
  decision_for_POSTCODE[i,2] <- names(which.max(table(t(MAR_mice_100$imp$X003_Postcode)[,i])))
  decision_for_POSTCODE[i,3] <- as.numeric(sort(table(t(MAR_mice_100$imp$X003_Postcode[i,])), decreasing=T)[1])
}
decision_for_POSTCODE

#compute a matrix that returns 1 for a correct imputation, 0 elseway
plainmatrix <- matrix(0,nrow=10, ncol=1) 
for(i in 1:10) { 
  if(decision_for_POSTCODE[i,2]==og_POSTCODE[i]) 
    plainmatrix[i]=1
}
plainmatrix

#build up matrix, consisting of: row index of observations in question, proposed value, actual value, and an indicator for matching
comparison_for_POSTCODE <- cbind( indicies <- which(is.na(mar_merged$X003_Postcode)), as.character(og_POSTCODE), decision_for_POSTCODE, plainmatrix)
colnames(comparison_for_POSTCODE) <- c("Index","Original Value", "Count in Imp","Most freq. Imputation","Count", "Hit")
#View(comparison_for_POSTCODE)

barplot(sort(table(mar_merged$X003_Postcode), decreasing=T)[1:15], ylim=c(0,40), las=2, main="Barplot for distribution of Postcodes before Imputation") #as a reminder
barplot(sort(table(t(MAR_mice_100$imp$X003_Postcode)), decreasing=T)[1:10], ylim=c(0,800), las=2, main="Distribution of considered Imputations")

#distribution for considered imputations per observation
par(mfcol = c(2,5))
for(i in 1:10)
  barplot(sort(table(t(MAR_mice_100$imp$X003_Postcode[i,])), decreasing=T)[1:10], 
          las=2, ylim=c(0,80),
          main=paste("Imp. for Obs", which(is.na(mar_merged$X003_Postcode))[i]))
par(mfcol = c(1,1))
###====END WORKFLOW FOR COMPUTING POSTCODE SIMILARITIES====


###===START WORKFLOW FOR COMPUTING NO. ROOMS SIMILARITES====
# Execute only when number of rooms is imputed by MICE
norooms_imp100_hits <- 0
norooms_imp100_hitm <- matrix(NA,nrow = 10, ncol=100)
for(i in 1:10){
  for(j in 1:100){
    if ( MAR_mice_100$imp$X012_NoRooms[i,j] == og_norooms[i] ) {
      norooms_imp100_hits <- norooms_imp100_hits + 1
      norooms_imp100_hitm[i,j] <- 1
    }
  }
}
#precision rate
#View(norooms_imp100_hitm)
#View(md.pattern(t(norooms_imp100_hitm)))
norooms_imp100_hits/1000


#what if we would take the mean of the corresponding distances?
#compute a matrix that returns 1 for a correct imputation, 0 elseway
decision_for_norooms_modal <- matrix(0,nrow=10, ncol=1)
decision_for_norooms_mean <- matrix(nrow=10,ncol=1)
decision_for_norooms_median <- matrix(nrow=10,ncol=1)
hits_modal <- matrix(0,nrow=10, ncol=1)
hits_mean <- matrix(0,nrow=10, ncol=1)
hits_median <- matrix(0,nrow=10, ncol=1) 
diff_norooms_modal <- matrix(0,nrow=10, ncol=1)
diff_norooms_mean <- matrix(0,nrow=10, ncol=1)
diff_norooms_median <- matrix(0,nrow=10, ncol=1)
for(i in 1:10) { 
  decision_for_norooms_modal[i,] <- as.numeric(names(sort(table((t(MAR_mice_100$imp$X012_NoRooms[i,]))), decreasing=T)[1]))
  decision_for_norooms_mean[i,] <- floor(rowMeans(as.matrix(MAR_mice_100$imp$X012_NoRooms)))[i]
  decision_for_norooms_median[i,] <- floor(rowMedians(as.matrix(MAR_mice_100$imp$X012_NoRooms)))[i]
  if(decision_for_norooms_modal[i] == og_norooms[i])
    hits_modal[i]=1
  if(decision_for_norooms_mean[i] == og_norooms[i])
    hits_mean[i]=1
  if(decision_for_norooms_median[i] == og_norooms[i])
    hits_median[i]=1
  diff_norooms_modal[i] <- abs(og_norooms[i] - decision_for_norooms_modal[i])
  diff_norooms_mean[i] <- abs(og_norooms[i] - decision_for_norooms_mean[i])  
  diff_norooms_median[i] <- abs(og_norooms[i] - decision_for_norooms_median[i])  
}
decision_for_norooms_modal
decision_for_norooms_mean
decision_for_norooms_median

#build up matrix, consisting of: row index of observations in question, proposed value, actual value, and an indicator for matching
comparison_for_norooms <- 
  cbind( indicies <- which(is.na(mar_merged$X012_NoRooms)), og_norooms, 
         decision_for_norooms_modal, diff_norooms_modal,
         decision_for_norooms_mean, diff_norooms_mean, 
         decision_for_norooms_median, diff_norooms_median, 
         hits_modal,hits_mean, hits_median)
colnames(comparison_for_norooms) <- c("Row in Dataset", "OG Value", 
                                      "Imputation (Modal)","Error",
                                      "Imputation (Mean)", "Error",
                                      "Imputation (Median)","Error",
                                      "Hits (Modal)","Hits (Mean)","Hits (Median)")
#View(comparison_for_norooms)

#computation for mse
ss_modal <- 0 
ss_mean <- 0
ss_median <- 0
for(i in 1:10){
  ss_modal <- ss_modal + diff_norooms_modal[i]^2 
  ss_mean <- ss_mean + diff_norooms_mean[i]^2 
  ss_median <- ss_median + diff_norooms_median[i]^2
}
mse_modal <- ss_modal/10
mse_mean <- ss_mean/10 
mse_median <- ss_median/10
paste("MSE Modal:", mse_modal)
paste("MSE Mean:", mse_mean)
paste("MSE Median:", mse_median)

mse(sim=comparison_for_norooms[,3],obs = comparison_for_norooms[,2])
mse(sim=comparison_for_norooms[,5],obs = comparison_for_norooms[,2])
mse(sim=comparison_for_norooms[,7],obs = comparison_for_norooms[,2])

#=====END WORKFLOW FOR COMPUTING NO. ROOMS SIMILARITES=========

#===Visualisations for NoRooms=====
#distribution for considered imputations per observation
# how many times does each categorical value (postcode) occur:
hist((mar_merged$X012_NoRooms), ylim=c(0,20), xlim=c(0,800), breaks=250, main="Distribution of Postcodes")

#Histograms
par(mfcol = c(2,5))
for(i in 1:10)
  hist(as.numeric(MAR_mice_100$imp$X012_NoRooms[i,]),
       xlim=c(0,500), ylim=c(0,30), breaks=100,
       main=paste("Imp. for Obs", which(is.na(mar_merged$X012_NoRooms))[i], "( OG:", og_norooms[i],", Mean/Median:", decision_for_norooms_mean[i],"/", decision_for_norooms_median[i],")"))
par(mfcol = c(1,1))

#Barplots
par(mfcol = c(2,5))
for(i in 1:10)
  barplot(sort(table(t(MAR_mice_100$imp$X012_NoRooms[i,])), decreasing=T)[1:10], 
          ylim=c(0,30), las = 2, 
          main=paste("Imp. for Obs", which(is.na(mar_merged$X012_NoRooms))[i],"( OG:", og_norooms[i],", Mean/Median:", decision_for_norooms_mean[i],"/", decision_for_norooms_median[i],")" ))
par(mfcol = c(1,1))
###===END WORKFLOW FOR COMPUTING NO. ROOMS SIMILARITES====


#==START dotplots for different metrics
orig_data_add_column <- working_set[which(rownames(working_set) %in% rownames(MAR_mice_100$imp$X012_NoRooms)), c("X012_NoRooms", "X102_DistanceToBeach")]

par(mfcol=c(2,2))
plot(working_set[, c("X012_NoRooms" , "X102_DistanceToBeach")], main = "Modal Value", ylim = c(0, 1), xlim = c(0, 300))
points(orig_data_add_column, col = "blue", pch = 19, cex =2)
points(cbind(decision_for_norooms_modal, orig_data_add_column[,2]), col = "brown", pch = 15, cex =2)

plot(working_set[, c("X012_NoRooms" , "X102_DistanceToBeach")], main = "Mean Value", ylim = c(0, 1), xlim = c(0, 300))
points(orig_data_add_column, col = "blue", pch = 19, cex =2)
points(cbind(decision_for_norooms_mean, orig_data_add_column[,2]), col = "brown", pch = 15, cex =2)

plot(working_set[, c("X012_NoRooms" , "X102_DistanceToBeach")], main = "Median Value", ylim = c(0, 1), xlim = c(0, 300))
points(orig_data_add_column, col = "blue", pch = 19, cex =2)
points(cbind(decision_for_norooms_median, orig_data_add_column[,2]), col = "brown", pch = 15, cex =2)

#
plot(working_set[, c("X012_NoRooms" , "X102_DistanceToBeach")], main = "Best Mean/Variance Fit", ylim = c(0, 1), xlim = c(0, 250)) 
points(orig_data_add_column, col = "blue", pch = 19, cex =2)
points(best_imp100, col = "brown", pch = 15, cex =2)
#==END dotplots for different metrics


###====START Compare imputations to original data by calculating mean, variance ===

# Check mean & variance for original dataset:
orig_mean <- mean(working_set[, c("X012_NoRooms")])
orig_var <- var(working_set[, c("X012_NoRooms")])

#Generate a matrix with the column of missing values imputed with MICE in the size of m (number of MICE imputations)
MAR_value_matrix_5 = matrix(mar_merged$X012_NoRooms, nrow = nrow(mar_merged), ncol = ncol(MAR_mice_5$imp$X012_NoRooms))
MAR_value_matrix_10 = matrix(mar_merged$X012_NoRooms, nrow = nrow(mar_merged), ncol = ncol(MAR_mice_10$imp$X012_NoRooms))
MAR_value_matrix_50 = matrix(mar_merged$X012_NoRooms, nrow = nrow(mar_merged), ncol = ncol(MAR_mice_50$imp$X012_NoRooms))
MAR_value_matrix_100 = matrix(mar_merged$X012_NoRooms, nrow = nrow(mar_merged), ncol = ncol(MAR_mice_100$imp$X012_NoRooms))
#And assign rownames
rownames(MAR_value_matrix_5) <- rownames(mar_merged)
rownames(MAR_value_matrix_10) <- rownames(mar_merged)
rownames(MAR_value_matrix_50) <- rownames(mar_merged)
rownames(MAR_value_matrix_100) <- rownames(mar_merged)

#Now insert the imputed values from the MICE package into each row of generated matrix:
for(i in 1:nrow(MAR_value_matrix_5)){
  for(j in 1:ncol(MAR_value_matrix_5)){
    for(z in 1:nrow(MAR_mice_5$imp$X012_NoRooms)){
      if (rownames(MAR_mice_5$imp$X012_NoRooms)[z] == rownames(MAR_value_matrix_5)[i]){
        MAR_value_matrix_5[i,j] <- as.matrix(MAR_mice_5$imp$X012_NoRooms[z,j])}}}}

#Now insert the imputed values from the MICE package into each row of generated matrix:
for(i in 1:nrow(MAR_value_matrix_10)){
  for(j in 1:ncol(MAR_value_matrix_10)){
    for(z in 1:nrow(MAR_mice_10$imp$X012_NoRooms)){
      if (rownames(MAR_mice_10$imp$X012_NoRooms)[z] == rownames(MAR_value_matrix_10)[i]){
        MAR_value_matrix_10[i,j] <- as.matrix(MAR_mice_10$imp$X012_NoRooms[z,j])}}}}

#Now insert the imputed values from the MICE package into each row of generated matrix:
for(i in 1:nrow(MAR_value_matrix_50)){
  for(j in 1:ncol(MAR_value_matrix_50)){
    for(z in 1:nrow(MAR_mice_50$imp$X012_NoRooms)){
      if (rownames(MAR_mice_50$imp$X012_NoRooms)[z] == rownames(MAR_value_matrix_50)[i]){
        MAR_value_matrix_50[i,j] <- as.matrix(MAR_mice_50$imp$X012_NoRooms[z,j])}}}}

#Now insert the imputed values from the MICE package into each row of generated matrix:
for(i in 1:nrow(MAR_value_matrix_100)){
  for(j in 1:ncol(MAR_value_matrix_100)){
    for(z in 1:nrow(MAR_mice_100$imp$X012_NoRooms)){
      if (rownames(MAR_mice_100$imp$X012_NoRooms)[z] == rownames(MAR_value_matrix_100)[i]){
        MAR_value_matrix_100[i,j] <- as.matrix(MAR_mice_100$imp$X012_NoRooms[z,j])}}}}

#Now check how good mice captured the original mean and variance
#For this reason establish the  comparison-matrix:
MAR_comparison_matrix5 = matrix(0, nrow = 2, ncol = ncol(MAR_value_matrix_5))
rownames(MAR_comparison_matrix5) <- c("imp_mean", "imp_variance")

MAR_comparison_matrix10 = matrix(0, nrow = 2, ncol = ncol(MAR_value_matrix_10))
rownames(MAR_comparison_matrix10) <- c("imp_mean", "imp_variance")

MAR_comparison_matrix50 = matrix(0, nrow = 2, ncol = ncol(MAR_value_matrix_50))
rownames(MAR_comparison_matrix50) <- c("imp_mean", "imp_variance")

MAR_comparison_matrix100 = matrix(0, nrow = 2, ncol = ncol(MAR_value_matrix_100))
rownames(MAR_comparison_matrix100) <- c("imp_mean", "imp_variance")

# Compare to original mean --> 1 = perfect match, while > 1 or < 1 represent deviations
for(i in 1:ncol(MAR_comparison_matrix5)){
  MAR_comparison_matrix5[1, i] <- mean(MAR_value_matrix_5[, i]) / orig_mean
  MAR_comparison_matrix5[2, i] <- var(MAR_value_matrix_5[, i]) / orig_var
}

for(i in 1:ncol(MAR_comparison_matrix10)){
  MAR_comparison_matrix10[1, i] <- mean(MAR_value_matrix_10[, i]) / orig_mean
  MAR_comparison_matrix10[2, i] <- var(MAR_value_matrix_10[, i]) / orig_var
}

for(i in 1:ncol(MAR_comparison_matrix50)){
  MAR_comparison_matrix50[1, i] <- mean(MAR_value_matrix_50[, i]) / orig_mean
  MAR_comparison_matrix50[2, i] <- var(MAR_value_matrix_50[, i]) / orig_var
}

for(i in 1:ncol(MAR_comparison_matrix100)){
  MAR_comparison_matrix100[1, i] <- mean(MAR_value_matrix_100[, i]) / orig_mean
  MAR_comparison_matrix100[2, i] <- var(MAR_value_matrix_100[, i]) / orig_var
}

best_mean5 <- MAR_comparison_matrix5[1, which.min(abs(MAR_comparison_matrix5[1,1:ncol(MAR_comparison_matrix5)]-1))] # Best imputation performed for mean
best_var5 <- MAR_comparison_matrix5[2, which.min(abs(MAR_comparison_matrix5[2,1:ncol(MAR_comparison_matrix5)]-1))] # Best imputation performed for variance
best_val5 <- MAR_comparison_matrix5[, which.min(abs(MAR_comparison_matrix5[1,1:ncol(MAR_comparison_matrix5)]-1) + abs(MAR_comparison_matrix5[2,1:ncol(MAR_comparison_matrix5)]-1))] # Best imputation with mean & variance combined
bestcol5 <- which.min(abs(MAR_comparison_matrix5[1,1:ncol(MAR_comparison_matrix5)]-1) + abs(MAR_comparison_matrix5[2,1:ncol(MAR_comparison_matrix5)]-1))

best_mean10 <- MAR_comparison_matrix10[1, which.min(abs(MAR_comparison_matrix10[1,1:ncol(MAR_comparison_matrix10)]-1))] # Best imputation performed for mean
best_var10 <- MAR_comparison_matrix10[2, which.min(abs(MAR_comparison_matrix10[2,1:ncol(MAR_comparison_matrix10)]-1))] # Best imputation performed for variance
best_val10 <-MAR_comparison_matrix10[, which.min(abs(MAR_comparison_matrix10[1,1:ncol(MAR_comparison_matrix10)]-1) + abs(MAR_comparison_matrix10[2,1:ncol(MAR_comparison_matrix10)]-1))] # Best imputation with mean & variance combined
bestcol10 <- which.min(abs(MAR_comparison_matrix10[1,1:ncol(MAR_comparison_matrix10)]-1) + abs(MAR_comparison_matrix10[2,1:ncol(MAR_comparison_matrix10)]-1))

best_mean50 <- MAR_comparison_matrix50[1, which.min(abs(MAR_comparison_matrix50[1,1:ncol(MAR_comparison_matrix50)]-1))] # Best imputation performed for mean
best_var50 <- MAR_comparison_matrix50[2, which.min(abs(MAR_comparison_matrix50[2,1:ncol(MAR_comparison_matrix50)]-1))] # Best imputation performed for variance
best_val50 <-MAR_comparison_matrix50[, which.min(abs(MAR_comparison_matrix50[1,1:ncol(MAR_comparison_matrix50)]-1) + abs(MAR_comparison_matrix50[2,1:ncol(MAR_comparison_matrix50)]-1))] # Best imputation with mean & variance combined
bestcol50 <- which.min(abs(MAR_comparison_matrix50[1,1:ncol(MAR_comparison_matrix50)]-1) + abs(MAR_comparison_matrix50[2,1:ncol(MAR_comparison_matrix50)]-1))

best_mean100 <- MAR_comparison_matrix100[1, which.min(abs(MAR_comparison_matrix100[1,1:ncol(MAR_comparison_matrix100)]-1))] # Best imputation performed for mean
best_var100 <- MAR_comparison_matrix100[2, which.min(abs(MAR_comparison_matrix100[2,1:ncol(MAR_comparison_matrix100)]-1))] # Best imputation performed for variance
best_val100 <-MAR_comparison_matrix100[, which.min(abs(MAR_comparison_matrix100[1,1:ncol(MAR_comparison_matrix100)]-1) + abs(MAR_comparison_matrix100[2,1:ncol(MAR_comparison_matrix100)]-1))] # Best imputation with mean & variance combined
bestcol100 <- which.min(abs(MAR_comparison_matrix100[1,1:ncol(MAR_comparison_matrix100)]-1) + abs(MAR_comparison_matrix100[2,1:ncol(MAR_comparison_matrix100)]-1))



# get rownames to add additional columns for plot
# We consider the best values for each imputation regarding mean and variance
# Also update overview table (comparison_for_norooms) in the following steps
best_imp5 <- as.matrix(MAR_mice_5$imp$X012_NoRooms[,bestcol5])
comparison_for_norooms <- cbind(comparison_for_norooms, best_imp5)
colnames(comparison_for_norooms)[ncol(comparison_for_norooms)] <- "Best_of5"
mse_best5 <- mse(sim = comparison_for_norooms[, ncol(comparison_for_norooms)], obs = comparison_for_norooms[, 2])
rownames(best_imp5) <- rownames(MAR_mice_5$imp$X012_NoRooms)

best_imp10 <- as.matrix(MAR_mice_10$imp$X012_NoRooms[,bestcol10])
comparison_for_norooms <- cbind(comparison_for_norooms, best_imp10)
colnames(comparison_for_norooms)[ncol(comparison_for_norooms)] <- "Best_of10"
mse_best10 <- mse(sim = comparison_for_norooms[, ncol(comparison_for_norooms)], obs = comparison_for_norooms[, 2])
rownames(best_imp10) <- rownames(MAR_mice_10$imp$X012_NoRooms)

best_imp50 <- as.matrix(MAR_mice_50$imp$X012_NoRooms[,bestcol50])
comparison_for_norooms <- cbind(comparison_for_norooms, best_imp50)
colnames(comparison_for_norooms)[ncol(comparison_for_norooms)] <- "Best_of50"
mse_best50 <- mse(sim = comparison_for_norooms[, ncol(comparison_for_norooms)], obs = comparison_for_norooms[, 2])
rownames(best_imp50) <- rownames(MAR_mice_50$imp$X012_NoRooms)

best_imp100 <- as.matrix(MAR_mice_100$imp$X012_NoRooms[,bestcol100])
comparison_for_norooms <- cbind(comparison_for_norooms, best_imp100)
colnames(comparison_for_norooms)[ncol(comparison_for_norooms)] <- "Best_of100"
mse_best100 <- mse(sim = comparison_for_norooms[, ncol(comparison_for_norooms)], obs = comparison_for_norooms[, 2])
rownames(best_imp100) <- rownames(MAR_mice_10$imp$X012_NoRooms)

# Put all calculated MSEs into an array to recheck
c(mse_best5, mse_best10, mse_best50, mse_best100)
# MSE is for all values pretty high with PMM . This can be concluded by the fact that PMM method tries to represent also the mean
#  of the original dataset. However, as only the smallest values are removed there is always at least one heavy error value.

# Now add the additional column for later plotting
best_imp5 <- cbind(best_imp5, working_set[which(rownames(working_set) %in% rownames(best_imp5)), "X102_DistanceToBeach"])

best_imp10 <- cbind(best_imp10, working_set[which(rownames(working_set) %in% rownames(best_imp10)), "X102_DistanceToBeach"])

best_imp50 <- cbind(best_imp50, working_set[which(rownames(working_set) %in% rownames(best_imp50)), "X102_DistanceToBeach"])

best_imp100 <- cbind(best_imp100, working_set[which(rownames(working_set) %in% rownames(best_imp100)), "X102_DistanceToBeach"])


#Plot best results for comparison by adding an additional numeric feature
orig_data_add_column <- working_set[which(rownames(working_set) %in% rownames(MAR_mice_5$imp$X012_NoRooms)), c("X012_NoRooms", "X102_DistanceToBeach")]


par(mfrow=c(2,2))
plot(working_set[, c("X012_NoRooms" , "X102_DistanceToBeach")], main = "5 Imputations", ylim = c(0, 1), xlim = c(0, 250))
points(orig_data_add_column, col = "blue", pch = 19, cex =2)
points(best_imp5, col = "brown", pch = 15, cex =2)

plot(working_set[, c("X012_NoRooms" , "X102_DistanceToBeach")], main = "10 Imputations", ylim = c(0, 1), xlim = c(0, 250)) 
points(orig_data_add_column, col = "blue", pch = 19, cex =2)
points(best_imp10, col = "green", pch = 16, cex =2)

plot(working_set[, c("X012_NoRooms" , "X102_DistanceToBeach")], main = "50 Imputations", ylim = c(0, 1), xlim = c(0, 250)) 
points(orig_data_add_column, col = "blue", pch = 19, cex =2)
points(best_imp50, col = "violet", pch = 17, cex =2)

plot(working_set[, c("X012_NoRooms" , "X102_DistanceToBeach")], main = "100 Imputations", ylim = c(0, 1), xlim = c(0, 250)) 
points(orig_data_add_column, col = "blue", pch = 19, cex =2)
points(best_imp100, col = "red", pch = 19, cex =2)



#By how much get the results of mean and variance better with an increased number of computations?:
(abs(1-best_mean5) + abs(1-best_mean10)) / abs(1-best_mean5)-1
(abs(1-best_mean10) + abs(1-best_mean50)) / abs(1-best_mean10)-1
(abs(1-best_mean50) + abs(1-best_mean100)) / abs(1-best_mean50)-10 #as variance worsens 

(abs(1-best_var5) + abs(1-best_var10)) / abs(1-best_var5)-10 
(abs(1-best_var10) + abs(1-best_var50)) / abs(1-best_var10)-1
(abs(1-best_var50) + abs(1-best_var100)) / abs(1-best_var50)-1
###====END Compare imputations to original data by calculating mean, variance ===



#=== END: MAR MICE IMPUTATIONS OF NUMERIC MISSING VALUES & COMPARISON TO ORIGINAL VALUES ===



