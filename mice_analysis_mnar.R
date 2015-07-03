# MNAR
# - numerical: X100_DistanceAlcudiaOldTown
# - factorial: X046_PayOptions

#===START: SETTING UP WORK ENVIRONMENT=====
source("DA2_avengers_require.R")
#source("DA2_avengers_install.R")

mnar_merged <- mnar_working_set_sort_payoption

table(mnar_merged$X046_PayOptions)
og_Oldtown <- working_set_sort_payOption[which(is.na(mnar_merged$X100_DistanceAlcudiaOldTown)),c("X100_DistanceAlcudiaOldTown")]
og_Payoption <- working_set_sort_payOption[which(is.na(mnar_merged$X046_PayOptions)),c("X046_PayOptions")]
mnar_merged[,c("X046_PayOptions")] <- as.factor(mnar_merged[, c("X046_PayOptions")])

sum(is.na(mnar_merged))
#everything's working? 14  41  55  85  99 103 114 125 134 245
which(is.na(mnar_merged[,c("X100_DistanceAlcudiaOldTown")]))
which(is.na(mnar_merged[,c("X046_PayOptions")]))
which(is.na(mnar_merged[,c("X046_PayOptions")])&is.na(mnar_merged[,c("X100_DistanceAlcudiaOldTown")]))
#===END: SETTING UP WORK ENVIRONMENT=====

#====START: SOME VISUALISATIONS===== 
postcode_rooms_ma <- cbind(mnar_merged$X100_DistanceAlcudiaOldTown, mnar_merged$X046_PayOptions)
marginplot(postcode_rooms_ma)

par(mfcol = c(2,1))
#barplot(sort(table(mnar_merged$X046_PayOptions)), ylim=c(0,100), las=2, main="Distribution of Postcodes")
barplot(sort(table(mnar_merged$X046_PayOptions)), ylim=c(0,100), las=2, main="Distribution of Postcodes")
barplot(sort(table(mnar_working_set_sort_payoption$X046_PayOptions)), ylim=c(0,100), las=2, main="Distribution of Postcodes")
barplot(sort(table(working_set_sort_payOption$X046_PayOptions)), ylim=c(0,100), las=2, main="Distribution of Postcodes")
par(mfcol = c(1,1))

par(mfcol = c(2,1))
hist((mnar_merged$X100_DistanceAlcudiaOldTown), ylim=c(0,40), xlim=c(0,800), breaks=100, main="Distribution of Postcodes")
#SEVERE PROBLEM: PAYOPTIONS HAVE TO BE PRE SORTED OR THERE HAS TO BE SOME OTHER WAY TO MAKE THE RANK OF PAYOPTIONS LESS IMPORTANT
#====END: SOME VISUALISATIONS===== 


#=== START: MNAR MICE IMPUTATIONS OF NUMERIC MISSING VALUES & COMPARISON TO ORIGINAL VALUES ===
MNAR_mice_100_dual_pmm <- mice(mnar_merged, method='pmm', m=100, seed=06041992)
#For Payoption: 3 correct, only the "mastercard;visa" patterns were guessed right.
#For Oldtown: only value for sixth observation was guess right 61 times which was the only value that was guessed right
#leading to a hitrate of 6.1%, mse = 194.4914. You may also have a look onto the barplots. On numerous occations the imputations
#are restricted on even less than 10 different values. Even here some values get repeated although these values seem to be very specific.
#Based on the plot the algorithm wasnt as bad as the mse would suggest. the "noise" that gets added to the values is in two cases
#just rediciulously big.

MNAR_mice_100_dual_sample <- mice(mnar_merged, method='sample', m=100, seed=06041992)
#For Payoption: 0 correct, but the "MasterCard;Visa" entries were pretty frequent
#For Oldtown: Results are pretty bad, but again the sixth value was guessed right 2 times. mse = 1970.904
MNAR_mice_100_dual_cart <- mice(mnar_merged, method='cart', m=100, seed=06041992)
#For Payoption: 4 correct, 29.7% hitrate. Even "American Express; MasterCard; Visa" detected. But wasnt as frequent as the other hits.
#For Oldtown. None correct, Hitrate at 4.2%. mse = 18.90, again the sixth value was 42 times as close as +-500m, eventually
#it was as close as ~1km
MNAR_mice_100_dual_rf <- mice(mnar_merged, method='rf', m=100, seed=06041992)
#For Payoption: 4 correct, 33.4% hitrate, also detected "American Express; MasterCard; Visa", original values were coming along 
#quite often but algorithm still tends to over-guess with "mastercard;visa". "EC-card" wasnt even guessed once
#For Oldtown: 1 correct, sixth value guessed correct again. mse = 31.335 Even though the tollerance would have to be quite high
#in a practical sense, the resulting points in the plot seem to fit very well in the corresponding area of the original values.
#As the lower end based on mnar fell out. the algorithm still is not able to reconstruct the lower end. although the number
#of correct hits for oldtown distances was worse, the points for cart were a bit "nearer" to the original ones (-> mse)

#mice only for the PayOption feature with various methods.
MNAR_mice_100_payoption_pmm <- mice(mnar_merged[,-which(colnames(mnar_merged)=="X100_DistanceAlcudiaOldTown")], method='pmm', m=100, seed=06041992)
#FOr Payoption: 3 correct, global hitrate = 30.8%. overguessed with Mastercard;Visa
MNAR_mice_100_payoption_lda <- mice(mnar_merged[,-which(colnames(mnar_merged)=="X100_DistanceAlcudiaOldTown")], method='lda', m=100, seed=06041992)
#does not work for some reason
MNAR_mice_100_payoption_sample <- mice(mnar_merged[,-which(colnames(mnar_merged)=="X100_DistanceAlcudiaOldTown")], method='sample', m=100, seed=06041992)
#For Payoption: Surprisingly good! 6 correct, hitrate = 30.7% overguessed a bit with "American Express; MasterCard; Visa"
#on the other hand it mostly guessed with the most frequent elements. maybe just a bit of luck that there were so much high frequent
#payoptions.
MNAR_mice_100_payoption_cart <- mice(mnar_merged[,-which(colnames(mnar_merged)=="X100_DistanceAlcudiaOldTown")], method='cart', m=100, seed=06041992)
#For Payoption: 6 correct, But the only two "exceptions" werent very predominant in the set of imputations. mostly the frequent 
#payoptions were guessed right. but even there there were some misses. through all imputation attempts 35.7% were correct
MNAR_mice_100_payoption_rf <- mice(mnar_merged[,-which(colnames(mnar_merged)=="X100_DistanceAlcudiaOldTown")], method='rf', m=100, seed=06041992)
#For Payoption: 5 correct, the EC Card observation wasnt taken into consideration again. basically a slightly better version of the 
#dual attempt.

#mice only for numerical feature 
system.time(MNAR_mice_5 <- mice(mnar_merged[,-which(colnames(mnar_merged)=="X046_PayOptions")], method='pmm', m=5, seed=06041992))
system.time(MNAR_mice_10 <- mice(mnar_merged[,-which(colnames(mnar_merged)=="X046_PayOptions")], method='pmm', m=10, seed=06041992))
system.time(MNAR_mice_50 <- mice(mnar_merged[,-which(colnames(mnar_merged)=="X046_PayOptions")], method='pmm', m=50, seed=06041992))
system.time(MNAR_mice_100 <- mice(mnar_merged[,-which(colnames(mnar_merged)=="X046_PayOptions")], method='pmm', m=100, seed=06041992))
MNAR_mice_100_oldtown_pmm <- mice(mnar_merged[,-which(colnames(mnar_merged)=="X046_PayOptions")], method='pmm', m=100, seed=06041992)
#For Oldtown: only the sixth value was guessed right very well. But produced three extreme misses. hitrate = 4.3%
MNAR_mice_100_oldtown_norm <- mice(mnar_merged[,-which(colnames(mnar_merged)=="X046_PayOptions")], method='norm', m=100, seed=06041992)
MNAR_mice_100_oldtown_norm.nob <- mice(mnar_merged[,-which(colnames(mnar_merged)=="X046_PayOptions")], method='norm.nob', m=100, seed=06041992)
MNAR_mice_100_oldtown_norm.boot <- mice(mnar_merged[,-which(colnames(mnar_merged)=="X046_PayOptions")], method='norm.boot', m=100, seed=06041992)
MNAR_mice_100_oldtown_norm.predict <- mice(mnar_merged[,-which(colnames(mnar_merged)=="X046_PayOptions")], method='norm.predict', m=100, seed=06041992)
MNAR_mice_100_oldtown_mean <- mice(mnar_merged[,-which(colnames(mnar_merged)=="X046_PayOptions")], method='mean', m=100, seed=06041992)
MNAR_mice_100_oldtown_quad <- mice(mnar_merged[,-which(colnames(mnar_merged)=="X046_PayOptions")], method='quadratic', m=100, seed=06041992)
MNAR_mice_100_oldtown_cart <- mice(mnar_merged[,-which(colnames(mnar_merged)=="X046_PayOptions")], method='cart', m=100, seed=06041992)
MNAR_mice_100_oldtown_rf <- mice(mnar_merged[,-which(colnames(mnar_merged)=="X046_PayOptions")], method='rf', m=100, seed=06041992)

###===Choose your mice object first=====
MNAR_mice_100 <- MNAR_mice_100_oldtown_pmm

###====START WORKFLOW FOR COMPUTING Payoption SIMILARITIES====
Payoption_imp100_hits <- 0
Payoption_imp100_hitm <- matrix(NA,nrow = 10, ncol=100)
for(i in 1:10){
  for(j in 1:100){
    if (as.matrix(MNAR_mice_100$imp$X046_PayOptions[i,j]) == og_Payoption[i]){
      Payoption_imp100_hits <- Payoption_imp100_hits + 1
      Payoption_imp100_hitm[i,j] <- 1
    }
  }
}
Payoption_imp100_hits/1000
solpattern <- md.pattern(t(Payoption_imp100_hitm)) #solution patterns
View(solpattern) 

#precision rate
Payoption_imp100_hits/1000

#what if we would take the factors which occur the most in between 100 iterations of imputations?
decision_for_Payoption <- matrix(nrow=10,ncol=3)
for(i in 1:10){
  tableObj <- table(t(MNAR_mice_100$imp$X046_PayOptions[i,]))
  og_count <- 0
  for(j in 1:length(tableObj)) 
    if(names(tableObj)[j]==og_Payoption[i])
      og_count <- as.numeric(tableObj[which(rownames(tableObj)==og_Payoption[i])])
  decision_for_Payoption[i,1] <- og_count
  decision_for_Payoption[i,2] <- names(which.max(table(t(MNAR_mice_100$imp$X046_PayOptions)[,i])))
  decision_for_Payoption[i,3] <- as.numeric(sort(table(t(MNAR_mice_100$imp$X046_PayOptions[i,])), decreasing=T)[1])
}
decision_for_Payoption

#compute a matrix that returns 1 for a correct imputation, 0 elseway
plainmatrix <- matrix(0,nrow=10, ncol=1) 
for(i in 1:10) { 
  if(decision_for_Payoption[i,2]==og_Payoption[i]) 
    plainmatrix[i]=1
}
plainmatrix

#build up matrix, consisting of: row index of observations in question, proposed value, actual value, and an indicator for matching
comparison_for_Payoption <- cbind( indicies <- which(is.na(mnar_merged$X046_PayOptions)), as.character(og_Payoption), decision_for_Payoption, plainmatrix)
colnames(comparison_for_Payoption) <- c("Index", "Original Value", "Count", "Final Imputation", "Count","Hit")
View(comparison_for_Payoption)


#distribution for considered imputations per observation
par(mfcol = c(2,5))
for(i in 1:10)
  barplot(sort(table(t(MNAR_mice_100$imp$X046_PayOptions[i,])), decreasing=T)[1:10], 
          las=2, ylim=c(0,80),
          main=paste("Imp. for Obs", which(is.na(mnar_merged$X046_PayOptions))[i]))
par(mfcol = c(1,1))
###====END WORKFLOW FOR COMPUTING Payoption SIMILARITIES====

###==START WORKFLOW FOR COMPUTING Distance to Alcudia Oldtown===

dist_imp100_hits <- 0
dist_imp100_hitm <- matrix(NA,nrow = 10, ncol=100)
for(i in 1:10){
  for(j in 1:100){
    if ( MNAR_mice_100$imp$X100_DistanceAlcudiaOldTown[i,j] > (og_Oldtown[i] - 0.5) && MNAR_mice_100$imp$X100_DistanceAlcudiaOldTown[i,j] < (og_Oldtown[i] + 0.5)) {
      dist_imp100_hits <- dist_imp100_hits + 1
      dist_imp100_hitm[i,j] <- 1
    }
  }
}
#precision rate
#View(dist_imp100_hitm)
#View(md.pattern(t(dist_imp100_hitm)))
dist_imp100_hits/1000


#what if we would take the mean of the corresponding distances?
decision_for_dist <- matrix(nrow=10,ncol=1)
for(i in 1:10){
  decision_for_dist[i,] <- rowMeans(MNAR_mice_100$imp$X100_DistanceAlcudiaOldTown)[i]
}
decision_for_dist

#compute a matrix that returns 1 for a correct imputation, 0 elseway
plainmatrix <- matrix(0,nrow=10, ncol=1) 
diff_dist_m <- matrix(0,nrow=10, ncol=1)
for(i in 1:10) { 
  if((decision_for_dist[i] < (og_Oldtown[i] + 0.5)) && (decision_for_dist[i] > (og_Oldtown[i] - 0.5)))
    plainmatrix[i]=1
  diff_dist_m[i] <- abs(og_Oldtown[i] - decision_for_dist[i])  
}

#build up matrix, consisting of: row index of observations in question, proposed value, actual value, and an indicator for matching
comparison_for_dist <- cbind( indicies <- which(is.na(mnar_merged$X100_DistanceAlcudiaOldTown)), og_Oldtown, decision_for_dist, diff_dist_m, plainmatrix)
View(comparison_for_dist)

#computation for mse
ss <- 0
for(i in 1:10){
  ss <- ss + diff_dist_m[i]^2 
}
mse <- ss/10 
mse
###==END WORKFLOW FOR COMPUTING Distance to Alcudia Oldtown===

###====START: Some visualisations===
#plots for each of the observations
par(mfcol=c(2,1))
barplot(sort(table(t(MNAR_mice_100$imp$X046_PayOptions[4,])), decreasing=T)[1:5],las=2, ylim=c(0,50), 
        main=paste("Distribution for Payoption Imputation for Observation", 4 ,"", "\nOriginal Value:", 
                   og_Payoption[4], "\nFinal Imputation:", as.character(decision_for_Payoption[4,2]),
                   "\nMethod:", as.character(MNAR_mice_100$method[1])))
par(mfcol=c(1,1))

#global imputation distribution (Top 5)
par(mfcol=c(2,1))
barplot(sort(table(t(MNAR_mice_100$imp$X046_PayOptions)), decreasing=T)[1:5], las=2, ylim=c(0,500),
        main=paste("Distrbution for Payoption Imputations - Method:", as.character(MNAR_mice_100$method[1])))
par(mfcol=c(1,1))

#reminder:
par(mfcol=c(2,1))
barplot(sort(table(t(mnar_merged$X046_PayOptions)), decreasing=T)[1:5], las=2, ylim=c(0,100),
        main="Global Distribution for Payoption")
par(mfcol=c(1,1))

hist((mnar_merged$X100_DistanceAlcudiaOldTown), ylim=c(0,20), xlim=c(0,800), breaks=250, main="Distribution of Postcodes")

#Histograms
par(mfcol = c(2,5))
for(i in 1:10)
  hist(as.numeric(MNAR_mice_100$imp$X100_DistanceAlcudiaOldTown[i,]),
       xlim=c(0,80), ylim=c(0,30), breaks=100,
       main=paste("Imp. for Obs", which(is.na(mnar_merged$X100_DistanceAlcudiaOldTown))[i], "( OG:", round(og_Oldtown[i],digits=2),", Decision:", round(decision_for_dist[i],digits = 2),")"))
par(mfcol = c(1,1))

#Barplots
par(mfcol = c(2,5))
for(i in 1:10)
  barplot(sort(table(t(round(MNAR_mice_100$imp$X100_DistanceAlcudiaOldTown[i,],digits = 3))), decreasing=T)[1:10], 
          ylim=c(0,35), las = 2, 
          main=paste("Imp. for Obs", which(is.na(mnar_merged$X100_DistanceAlcudiaOldTown))[i],"( OG:", og_norooms[i],", Decision:", decision_for_dist[i],")" ))
par(mfcol = c(1,1))

original_points <-  working_set_sort_payOption[which(rownames(working_set_sort_payOption) %in% rownames(MNAR_mice_100$imp$X100_DistanceAlcudiaOldTown)), c("X100_DistanceAlcudiaOldTown", "X102_DistanceToBeach")]
decision_points <- cbind(decision_for_dist, working_set_sort_payOption$X102_DistanceToBeach[which(is.na(mnar_merged$X046_PayOptions))])

plot(working_set_sort_payOption[,c("X100_DistanceAlcudiaOldTown","X102_DistanceToBeach")], ylim=c(0,1.5), 
     main="PMM Imputations (Mean Results)")
points(original_points, col="blue",pch=16, cex=2)
points(decision_points, col="red", pch=16, cex=2)
legend("topright", c("Original Values", "Imputed Values"), pch=c(19,19), col=c("blue","red"))

decision_for_dist
###====END: SOme Visualisations====

###====START Compare imputations to original data by calculating mean, variance ===

# Check mean & variance for original dataset:
orig_mean <- mean(working_set[, c("X100_DistanceAlcudiaOldTown")])
orig_var <- var(working_set[, c("X100_DistanceAlcudiaOldTown")])

#Generate a matrix with the column of missing values imputed with MICE in the size of m (number of MICE imputations)
MNAR_value_matrix_5 = matrix(mnar_merged$X100_DistanceAlcudiaOldTown, nrow = nrow(mnar_merged), ncol = ncol(MNAR_mice_5$imp$X100_DistanceAlcudiaOldTown))
MNAR_value_matrix_10 = matrix(mnar_merged$X100_DistanceAlcudiaOldTown, nrow = nrow(mnar_merged), ncol = ncol(MNAR_mice_10$imp$X100_DistanceAlcudiaOldTown))
MNAR_value_matrix_50 = matrix(mnar_merged$X100_DistanceAlcudiaOldTown, nrow = nrow(mnar_merged), ncol = ncol(MNAR_mice_50$imp$X100_DistanceAlcudiaOldTown))
MNAR_value_matrix_100 = matrix(mnar_merged$X100_DistanceAlcudiaOldTown, nrow = nrow(mnar_merged), ncol = ncol(MNAR_mice_100$imp$X100_DistanceAlcudiaOldTown))
#And assign rownames
rownames(MNAR_value_matrix_5) <- rownames(mnar_merged)
rownames(MNAR_value_matrix_10) <- rownames(mnar_merged)
rownames(MNAR_value_matrix_50) <- rownames(mnar_merged)
rownames(MNAR_value_matrix_100) <- rownames(mnar_merged)

#Now insert the imputed values from the MICE package into each row of generated matrix:
for(i in 1:nrow(MNAR_value_matrix_5)){
  for(j in 1:ncol(MNAR_value_matrix_5)){
    for(z in 1:nrow(MNAR_mice_5$imp$X100_DistanceAlcudiaOldTown)){
      if (rownames(MNAR_mice_5$imp$X100_DistanceAlcudiaOldTown)[z] == rownames(MNAR_value_matrix_5)[i]){
        MNAR_value_matrix_5[i,j] <- as.matrix(MNAR_mice_5$imp$X100_DistanceAlcudiaOldTown[z,j])}}}}

#Now insert the imputed values from the MICE package into each row of generated matrix:
for(i in 1:nrow(MNAR_value_matrix_10)){
  for(j in 1:ncol(MNAR_value_matrix_10)){
    for(z in 1:nrow(MNAR_mice_10$imp$X100_DistanceAlcudiaOldTown)){
      if (rownames(MNAR_mice_10$imp$X100_DistanceAlcudiaOldTown)[z] == rownames(MNAR_value_matrix_10)[i]){
        MNAR_value_matrix_10[i,j] <- as.matrix(MNAR_mice_10$imp$X100_DistanceAlcudiaOldTown[z,j])}}}}

#Now insert the imputed values from the MICE package into each row of generated matrix:
for(i in 1:nrow(MNAR_value_matrix_50)){
  for(j in 1:ncol(MNAR_value_matrix_50)){
    for(z in 1:nrow(MNAR_mice_50$imp$X100_DistanceAlcudiaOldTown)){
      if (rownames(MNAR_mice_50$imp$X100_DistanceAlcudiaOldTown)[z] == rownames(MNAR_value_matrix_50)[i]){
        MNAR_value_matrix_50[i,j] <- as.matrix(MNAR_mice_50$imp$X100_DistanceAlcudiaOldTown[z,j])}}}}

#Now insert the imputed values from the MICE package into each row of generated matrix:
for(i in 1:nrow(MNAR_value_matrix_100)){
  for(j in 1:ncol(MNAR_value_matrix_100)){
    for(z in 1:nrow(MNAR_mice_100$imp$X100_DistanceAlcudiaOldTown)){
      if (rownames(MNAR_mice_100$imp$X100_DistanceAlcudiaOldTown)[z] == rownames(MNAR_value_matrix_100)[i]){
        MNAR_value_matrix_100[i,j] <- as.matrix(MNAR_mice_100$imp$X100_DistanceAlcudiaOldTown[z,j])}}}}

#Now check how good mice captured the original mean and variance
#As sapply does not work for some reason this is a workaroundwith a comparison-matrix  old: sapply(value_matrix, function(y) c(VM_mean = mean(y), VM_variance = var(y)))
MNAR_comparison_matrix5 = matrix(0, nrow = 2, ncol = ncol(MNAR_value_matrix_5))
rownames(MNAR_comparison_matrix5) <- c("imp_mean", "imp_variance")

MNAR_comparison_matrix10 = matrix(0, nrow = 2, ncol = ncol(MNAR_value_matrix_10))
rownames(MNAR_comparison_matrix10) <- c("imp_mean", "imp_variance")

MNAR_comparison_matrix50 = matrix(0, nrow = 2, ncol = ncol(MNAR_value_matrix_50))
rownames(MNAR_comparison_matrix50) <- c("imp_mean", "imp_variance")

MNAR_comparison_matrix100 = matrix(0, nrow = 2, ncol = ncol(MNAR_value_matrix_100))
rownames(MNAR_comparison_matrix100) <- c("imp_mean", "imp_variance")

# Compare to original mean --> 1 = perfect match, while > 1 or < 1 represent deviations
for(i in 1:ncol(MNAR_comparison_matrix5)){
  MNAR_comparison_matrix5[1, i] <- mean(MNAR_value_matrix_5[, i]) / orig_mean
  MNAR_comparison_matrix5[2, i] <- var(MNAR_value_matrix_5[, i]) / orig_var
}

for(i in 1:ncol(MNAR_comparison_matrix10)){
  MNAR_comparison_matrix10[1, i] <- mean(MNAR_value_matrix_10[, i]) / orig_mean
  MNAR_comparison_matrix10[2, i] <- var(MNAR_value_matrix_10[, i]) / orig_var
}

for(i in 1:ncol(MNAR_comparison_matrix50)){
  MNAR_comparison_matrix50[1, i] <- mean(MNAR_value_matrix_50[, i]) / orig_mean
  MNAR_comparison_matrix50[2, i] <- var(MNAR_value_matrix_50[, i]) / orig_var
}

for(i in 1:ncol(MNAR_comparison_matrix100)){
  MNAR_comparison_matrix100[1, i] <- mean(MNAR_value_matrix_100[, i]) / orig_mean
  MNAR_comparison_matrix100[2, i] <- var(MNAR_value_matrix_100[, i]) / orig_var
}

best_mean5 <- MNAR_comparison_matrix5[1, which.min(abs(MNAR_comparison_matrix5[1,1:ncol(MNAR_comparison_matrix5)]-1))] # Best imputation performed for mean
best_var5 <- MNAR_comparison_matrix5[2, which.min(abs(MNAR_comparison_matrix5[2,1:ncol(MNAR_comparison_matrix5)]-1))] # Best imputation performed for variance
best_val5 <- MNAR_comparison_matrix5[, which.min(abs(MNAR_comparison_matrix5[1,1:ncol(MNAR_comparison_matrix5)]-1) + abs(MNAR_comparison_matrix5[2,1:ncol(MNAR_comparison_matrix5)]-1))] # Best imputation with mean & variance combined
bestcol5 <- which.min(abs(MNAR_comparison_matrix5[1,1:ncol(MNAR_comparison_matrix5)]-1) + abs(MNAR_comparison_matrix5[2,1:ncol(MNAR_comparison_matrix5)]-1))

best_mean10 <- MNAR_comparison_matrix10[1, which.min(abs(MNAR_comparison_matrix10[1,1:ncol(MNAR_comparison_matrix10)]-1))] # Best imputation performed for mean
best_var10 <- MNAR_comparison_matrix10[2, which.min(abs(MNAR_comparison_matrix10[2,1:ncol(MNAR_comparison_matrix10)]-1))] # Best imputation performed for variance
best_val10 <-MNAR_comparison_matrix10[, which.min(abs(MNAR_comparison_matrix10[1,1:ncol(MNAR_comparison_matrix10)]-1) + abs(MNAR_comparison_matrix10[2,1:ncol(MNAR_comparison_matrix10)]-1))] # Best imputation with mean & variance combined
bestcol10 <- which.min(abs(MNAR_comparison_matrix10[1,1:ncol(MNAR_comparison_matrix10)]-1) + abs(MNAR_comparison_matrix10[2,1:ncol(MNAR_comparison_matrix10)]-1))

best_mean50 <- MNAR_comparison_matrix50[1, which.min(abs(MNAR_comparison_matrix50[1,1:ncol(MNAR_comparison_matrix50)]-1))] # Best imputation performed for mean
best_var50 <- MNAR_comparison_matrix50[2, which.min(abs(MNAR_comparison_matrix50[2,1:ncol(MNAR_comparison_matrix50)]-1))] # Best imputation performed for variance
best_val50 <-MNAR_comparison_matrix50[, which.min(abs(MNAR_comparison_matrix50[1,1:ncol(MNAR_comparison_matrix50)]-1) + abs(MNAR_comparison_matrix50[2,1:ncol(MNAR_comparison_matrix50)]-1))] # Best imputation with mean & variance combined
bestcol50 <- which.min(abs(MNAR_comparison_matrix50[1,1:ncol(MNAR_comparison_matrix50)]-1) + abs(MNAR_comparison_matrix50[2,1:ncol(MNAR_comparison_matrix50)]-1))

best_mean100 <- MNAR_comparison_matrix100[1, which.min(abs(MNAR_comparison_matrix100[1,1:ncol(MNAR_comparison_matrix100)]-1))] # Best imputation performed for mean
best_var100 <- MNAR_comparison_matrix100[2, which.min(abs(MNAR_comparison_matrix100[2,1:ncol(MNAR_comparison_matrix100)]-1))] # Best imputation performed for variance
best_val100 <-MNAR_comparison_matrix100[, which.min(abs(MNAR_comparison_matrix100[1,1:ncol(MNAR_comparison_matrix100)]-1) + abs(MNAR_comparison_matrix100[2,1:ncol(MNAR_comparison_matrix100)]-1))] # Best imputation with mean & variance combined
bestcol100 <- which.min(abs(MNAR_comparison_matrix100[1,1:ncol(MNAR_comparison_matrix100)]-1) + abs(MNAR_comparison_matrix100[2,1:ncol(MNAR_comparison_matrix100)]-1))



# get rownames to add additional columns for plot
# We consider the best values for each imputation regarding mean and variance
# Also update overview table (comparison_for_dist) in the following steps
best_imp5 <- as.matrix(MNAR_mice_5$imp$X100_DistanceAlcudiaOldTown[,bestcol5])
comparison_for_dist <- cbind(comparison_for_dist, best_imp5)
colnames(comparison_for_dist)[ncol(comparison_for_dist)] <- "Best_of5"
mse_best5 <- mse(sim = comparison_for_dist[, ncol(comparison_for_dist)], obs = comparison_for_dist[, 2])
rownames(best_imp5) <- rownames(MNAR_mice_5$imp$X100_DistanceAlcudiaOldTown)

best_imp10 <- as.matrix(MNAR_mice_10$imp$X100_DistanceAlcudiaOldTown[,bestcol10])
comparison_for_dist <- cbind(comparison_for_dist, best_imp10)
colnames(comparison_for_dist)[ncol(comparison_for_dist)] <- "Best_of10"
mse_best10 <- mse(sim = comparison_for_dist[, ncol(comparison_for_dist)], obs = comparison_for_dist[, 2])
rownames(best_imp10) <- rownames(MNAR_mice_10$imp$X100_DistanceAlcudiaOldTown)

best_imp50 <- as.matrix(MNAR_mice_50$imp$X100_DistanceAlcudiaOldTown[,bestcol50])
comparison_for_dist <- cbind(comparison_for_dist, best_imp50)
colnames(comparison_for_dist)[ncol(comparison_for_dist)] <- "Best_of50"
mse_best50 <- mse(sim = comparison_for_dist[, ncol(comparison_for_dist)], obs = comparison_for_dist[, 2])
rownames(best_imp50) <- rownames(MNAR_mice_50$imp$X100_DistanceAlcudiaOldTown)

best_imp100 <- as.matrix(MNAR_mice_100$imp$X100_DistanceAlcudiaOldTown[,bestcol100])
comparison_for_dist <- cbind(comparison_for_dist, best_imp100)
colnames(comparison_for_dist)[ncol(comparison_for_dist)] <- "Best_of100"
mse_best100 <- mse(sim = comparison_for_dist[, ncol(comparison_for_dist)], obs = comparison_for_dist[, 2])
rownames(best_imp100) <- rownames(MNAR_mice_10$imp$X100_DistanceAlcudiaOldTown)

# Put all calculated MSEs into an array to recheck
c(mse_best5, mse_best10, mse_best50, mse_best100)

#colnames(comparison_for_dist) <- c("Imputation (Modal)","Imputation (Mean)","Imputation (Median)",
#                                      "Best_of5","Best_of10","Best_of50","Best_of100")

# Now add the additional column for later plotting
best_imp5 <- cbind(best_imp5, working_set[which(rownames(working_set) %in% rownames(best_imp5)), "X102_DistanceToBeach"])

best_imp10 <- cbind(best_imp10, working_set[which(rownames(working_set) %in% rownames(best_imp10)), "X102_DistanceToBeach"])

best_imp50 <- cbind(best_imp50, working_set[which(rownames(working_set) %in% rownames(best_imp50)), "X102_DistanceToBeach"])

best_imp100 <- cbind(best_imp100, working_set[which(rownames(working_set) %in% rownames(best_imp100)), "X102_DistanceToBeach"])


#Plot best results for comparison by adding an additional numeric feature
orig_data_add_column <- working_set[which(rownames(working_set) %in% rownames(MNAR_mice_5$imp$X100_DistanceAlcudiaOldTown)), c("X100_DistanceAlcudiaOldTown", "X102_DistanceToBeach")]


par(mfrow=c(2,2))
plot(working_set[, c("X100_DistanceAlcudiaOldTown" , "X102_DistanceToBeach")], main = "5 Imputations", ylim = c(0, 1.5))
points(orig_data_add_column, col = "blue", pch = 19, cex =2)
points(best_imp5, col = "brown", pch = 15, cex =2)
    
plot(working_set[, c("X100_DistanceAlcudiaOldTown" , "X102_DistanceToBeach")], main = "10 Imputations", ylim = c(0, 1.5)) 
points(orig_data_add_column, col = "blue", pch = 19, cex =2)
points(best_imp10, col = "green", pch = 16, cex =2)
    
plot(working_set[, c("X100_DistanceAlcudiaOldTown" , "X102_DistanceToBeach")], main = "50 Imputations", ylim = c(0, 1.5)) 
points(orig_data_add_column, col = "blue", pch = 19, cex =2)
points(best_imp50, col = "violet", pch = 17, cex =2)
    
plot(working_set[, c("X100_DistanceAlcudiaOldTown" , "X102_DistanceToBeach")], main = "100 Imputations", ylim = c(0, 1.5)) 
points(orig_data_add_column, col = "blue", pch = 19, cex =2)
points(best_imp100, col = "red", pch = 19, cex =2)
    
    
    
#By how much get the results of mean and variance better with an increased number of computations?:
(abs(1-best_mean5) + abs(1-best_mean10)) / abs(1-best_mean5)-1
(abs(1-best_mean10) + abs(1-best_mean50)) / abs(1-best_mean10)-1
(abs(1-best_mean50) + abs(1-best_mean100)) / abs(1-best_mean50)-1 #as variance worsens 
    
(abs(1-best_var5) + abs(1-best_var10)) / abs(1-best_var5)-10 
(abs(1-best_var10) + abs(1-best_var50)) / abs(1-best_var10)-1
(abs(1-best_var50) + abs(1-best_var100)) / abs(1-best_var50)-1
    
    
    #Means and variances of imputed data (Backup):
    sapply(MNAR_mice_100$imp$X100_DistanceAlcudiaOldTown, function(x) c(mean = mean(x), variance = var(x)))
    View(MNAR_value_matrix_100)
    
    # Boxplots as backup:
    par(mfrow=c(2,2))
    boxplot(MNAR_value_matrix_5[,1])
    boxplot(MNAR_value_matrix_10[,1])
    boxplot(MNAR_value_matrix_50[,1])
    boxplot(MNAR_value_matrix_100[,1])
    
    
    ###====END Compare imputations to original data by calculating mean, variance ===
    

