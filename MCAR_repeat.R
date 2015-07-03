source("DA2_avengers_require.R")

system("mkdir Datasets")

# Introduce Missing values completely at random
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

#Compute imputation based on a regression  with a linear model and compute the mse
lm_reg<-function(data, complete_data, column){
  data_noNA<-data[which(complete.cases(data)),]
  fit_lm = lm(as.formula(paste(column," ~ .",sep="")), data = data_noNA)
  fit_lm_pred<-predict(fit_lm, data[,!(names(data) %in% c(column))])
  mse_lm <- mse(fit_lm_pred,complete_data[,column])
  return(mse_lm)
}

#Compute imputation based on a regression tree and compute the mse
tree_reg<-function(data, complete_data, column,j){
  data_noNA<-data[which(complete.cases(data)),]
  fit_tree = rpart(as.formula(paste(column," ~ .",sep="")), data = data_noNA)
  assign(paste("Tree",j,sep="_"),fit_tree)
  save(list=paste("Tree",j,sep="_"),file=(paste("Datasets/Tree",paste(j,".Rdata",sep=""),sep="_")))
  fit_tree_pred<-predict(fit_tree,data[,!(names(data) %in% c(column))])
  mse_tree <- mse(fit_tree_pred,complete_data[,column])
  return(mse_tree)
}

#Compute imputation based on a regression with a random Forrest and compute the mse
ranF_reg<-function(data, complete_data, column){
  data_noNA<-data[which(complete.cases(data)),]
  fit_ranF = randomForest(as.formula(paste(column," ~ .",sep="")), data = data_noNA)
  fit_ranF_pred<-predict(fit_ranF,data[,!(names(data) %in% c(column))])
  mse_ranF <- mse(fit_ranF_pred,complete_data[,column])
  return(mse_ranF)
}

#Introduce 10 missing values into a feature based on the MCAR pattern and impute them with regressions using a linear model, a regression tree 
#and a random Forrest and collect the mse of the method 
MCAR_repeat<- function(data,j){
  assign(paste("mcar_na",j,sep="."),MCAR(data,"X102_DistanceToBeach"))
  save(list=paste("mcar_na",j,sep="."),file=(paste("Datasets/mcar_na",paste(j,".Rdata",sep=""),sep="_")))
  lm_mse<-lm_reg(get(paste("mcar_na",j,sep=".")),data,"X102_DistanceToBeach")
  tree_mse<-tree_reg(get(paste("mcar_na",j,sep=".")),data,"X102_DistanceToBeach",j)
  ranF_mse<-ranF_reg(get(paste("mcar_na",j,sep=".")),data,"X102_DistanceToBeach")
  result<-c("lm_mse"=lm_mse,"tree_mse"=tree_mse,"ranF_mse"=ranF_mse)
  return(result)
}

#Create the working data without factorial features
working_set_noFact<-working_set[,!(names(working_set) %in% c("X001_Name","X002_Street","X006_Country","X003_Postcode","X004_City","X014_CheckIn","X046_PayOptions","X103_LocationBeach"))]

#Create 1000 times datasets with 10 missing values in the same feature and collect the mse of the methods in a dataset
source("Seed.R")
registerDoParallel(cores=4)
result<-foreach(i=1:1000, .combine=rbind) %do% MCAR_repeat(working_set_noFact,i)

#Plot the three mse features
scatter3d(result[,1],result[,2],result[,3], surface=F,point.col="blue",xlab="lm_mse",ylab="tree_mse",zlab="ranF_mse")

#Split of the three outliers resulted from the scatterplot above
result_lowTree<-result[which(result[,2] %in% sort(result[,2],F)[1:3]),]
result_highTree<-result[which(result[,2] %in% sort(result[,2],F)[1:2]),]

filenames_lowMSE_Tree<-sapply(row.names(result_lowTree),function(x) gsub("result\\.","Tree_",x))
filenames<-list.files(path="Datasets")
for(i in filenames){
  load(paste("Datasets",paste(i,".Rdata",sep=""),sep="/"))
}


# #=== Analysis Datasets (no result)===#
# registerDoParallel(cores=4)
# files<-list.files(path="Datasets")
# rownames<-sapply(files,function(x) gsub(".Rdata","",x))
# #number_lowMSE<-as.integer(sapply(grep("mcar_na",ls(),value=T),function(x) substr(x,regexpr("[0-9]+",x)[1],regexpr("[0-9]+",x)[1]+attr(regexpr("[0-9]+",x),"match.length")[1])))
# 
# for(i in list.files(path="Datasets")){
#   filename<-paste("Datasets",i,sep="/")
#   print(filename)
#   data<-load(filename)
# }
# 
# data<-grep("mcar_na",ls(),value=T)
# 
# result_1<-sapply(data,function(x) max(working_set[which(is.na(get(x)$X102_DistanceToBeach)),"X102_DistanceToBeach"]))
# 
# sapply(number_lowMSE)
# 
# plot(1:1000,result_1,xlab="Index", ylab="X102_DistanceToBeach")
# points(x=which(unlist(attributes(result_1)) %in% rownames_lowMSE), y=result_1[which(unlist(attributes(result_1)) %in% rownames_lowMSE)],pch=1,cex=0.5,col="red", lwd=3)
# 
# 
# rownames_lowMSE<-sapply(row.names(result_lowTree),function(x) gsub("result\\.","mcar_na.",x))
# 
# which(unlist(attributes(result_1)) %in% rownames_lowMSE)


#=== Tree Analysis ===#
#Load the filenames of the Tree Data
Tree_filenames<-grep("Tree",unlist(list.files(path="Datasets")),value=T)

filenames_lowMSE_Tree
#Tree_571
#Tree_624
#Tree_800
#The three Datasets for the Tree Models with very low mse
load("Datasets/Tree_571.Rdata")
load("Datasets/Tree_624.Rdata")
load("Datasets/Tree_800.Rdata")
#The two Datasets for the Tree Models with very high mse
load("Datasets/Tree_473.Rdata")
load("Datasets/Tree_573.Rdata")


#Generate the plots of the different Trees
emf("Tree_800.emf",width=8,height=8,bg="transparent",custom.lty=F)
plot(Tree_800,margin=0.2,main="MCAR Regression Tree for DistanceToBeach with small MSE")
text(Tree_800,pretty=0,cex=0.8)
dev.off()

emf("Tree_571.emf",width=8,height=8,bg="transparent",custom.lty=F)
plot(Tree_571,margin=0.2,main="MCAR Regression Tree for DistanceToBeach with small MSE")
text(Tree_571,pretty=0,cex=0.8)
dev.off()

emf("Tree_624.emf",width=8,height=8,bg="transparent",custom.lty=F)
plot(Tree_624,margin=0.2,main="MCAR Regression Tree for DistanceToBeach with small MSE")
text(Tree_624,pretty=0,cex=0.8)
dev.off()

emf("Tree_473.emf",width=8,height=8,bg="transparent",custom.lty=F)
plot(Tree_473,margin=0.2,main="MCAR Regression Tree for DistanceToBeach with high MSE",nspace=5)
text(Tree_473,pretty=0,cex=0.8)
dev.off()

emf("Tree_573.emf",width=8,height=8,bg="transparent",custom.lty=F)
plot(Tree_573,margin=0.2,main="MCAR Regression Tree for DistanceToBeach with high MSE")
text(Tree_573,pretty=0,cex=0.8)
dev.off()
