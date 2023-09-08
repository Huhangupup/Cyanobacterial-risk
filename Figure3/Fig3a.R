setwd("D:/R/lake/factors/importtance")

#Load R package 
library(randomForest)
library(rfPermute)
library(ggplot2)

#Load data
factor <- read.csv("demmo.csv",row.names = 1)
data <- factor[,1:24]

#Set random seeds so that the results can be reproduced
set.seed(123)

#Run the random forest model
rf_results<-rfPermute(data$RISK~., data =data, importance = TRUE, ntree = 1000)

#View random forest primary results
rf_results$rf

#The interpretation rate of the predictor was extracted
predictor_var<- data.frame(importance(rf_results, scale = TRUE), check.names = FALSE)

#The prediction variables were classified by significance
predictor_sig<-as.data.frame((rf_results$pval)[,,2])
colnames(predictor_sig)<-c("sig","other")

#Combine significance factor and interpretation rate tables
df_pre<-cbind(predictor_var,predictor_sig)
df_pre$sig[df_pre$sig<=0.05]<-"*"
df_pre$sig[df_pre$sig>=0.05]<-" "
df_pre$IncNodePurity[df_pre$sig=="*"]<-"#36648B"
df_pre$IncNodePurity[df_pre$sig==" "]<-"gray40"

#output file
write.csv(df_pre,"RISK25_pr.csv")

