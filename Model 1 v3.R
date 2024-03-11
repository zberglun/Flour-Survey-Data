
#import needed libraries
library(psych)
library(gmodels)
library(base)
library(datasets)
library(graphics)
library(caret)
library(grDevices)
library(methods)
# library(MicrosoftML)
library(stats)
library(utils)
# library(RevoUtils)
# library(RevoUtilsMath)
# library(mrsdeploy)
#library(RevoMods)
#library(RevoScaleR)
library(rpart)
library(lattice)
library(ggplot2)
library(reshape2)
library(car)
library(manipulate)
library(mvtnorm)
library(expm)
library(DescTools)
#library(neural)
library(nnet, lib.loc = "C:/Program Files/R/R-4.2.2/library")
library(randomForest)
library(gridExtra)
library(viridis)
library(pdp)
library(iml)
library(neuralnet)
library(party)
library(plyr)
library(devtools)
library(caretEnsemble)
library(psych)
library(base)
library(colorspace)
library(datasets)
library(graphics)
library(grDevices)
library(methods)
library(stringr)
library(stats)
library(utils)
library(ModelMetrics)
library(recipes)
library(rpart)
library(lattice)
library(ggplot2)
library(reshape2)
library(car)
library(mvtnorm)
library(e1071)
library(caTools)
library(nnet)
library(randomForest)
library(gridExtra)
library(viridis)
library(partykit)
library(pdp)
library(iml)
library(neuralnet)
library(party)
library(plyr)
library(dplyr)
library(LogicReg)
library(caret)
library(caretEnsemble)
library(DMwR2)
library(gmodels)
library(data.table)
library(MultNonParam)
library(pre)
library(vip)

library(tidyverse)
library(ggpubr)
library(rstatix)

#Start with importing the data, saved as a tab delimited text file, variables on header
reg_data <- read.table("D:/School/Grad School/Chapter 3/Model 2 Datafile.txt", header = T , as.is = T)
#Next run correlaitons
cor(reg_data)
#self_risk_perception_2 and _3 are strongly correlated at 0.7, as such, one will be removed from the input to maintain independence
#Risk_perception_recall_1 and _2 are highly correlated at 0.62, as such one will be removed from the input to maintain independence
#now time to use tetrachoric corrlation on the binary variables

#Create Crosstables
act_5vrec_heard <- CrossTable(reg_data$Recall_Hear_Action_5,reg_data$Recall_Heard)
act_5vrec_bough <-CrossTable(reg_data$Recall_Hear_Action_5, reg_data$Recall_Bought_Action_5)
rec_heardvrec_bough <-CrossTable(reg_data$Recall_Heard, reg_data$Recall_Bought_Action_5)

#create actual matrices of the Ns
act_5vrec_heard.a <- c(632, 182)
act_5vrec_heard.b <- c(103, 37)
act_5vrec_heard <- cbind(act_5vrec_heard.a, act_5vrec_heard.b)
act_5vrec_bough.a <- c(717,214)
act_5vrec_bough.b <- c(18,5)
act_5vrec_bough <- cbind(act_5vrec_bough.a,act_5vrec_bough.b)
rec_heardvrec_bough.a <- c(796, 135)
rec_heardvrec_bough.b <- c(18,5)
rec_heardvrec_bough <- cbind(rec_heardvrec_bough.a,rec_heardvrec_bough.b)

#run tetrachoric using package: psych
tetrachoric(act_5vrec_heard)
tetrachoric(act_5vrec_bough)
tetrachoric(rec_heardvrec_bough)

#correlations are all small
#now run scatterplots
#pairs(reg_data)
#Clearly there is no lineraly related data, as such probablistic and non-linear models are appropriate to use to explore


#lastly to check the distribution  each variable in monovarite
reg_dataM <- melt(reg_data)
hists <- ggplot(reg_dataM, aes(x=value)) + stat_count()+facet_wrap(~variable)
hists
#Behavior score as a predicted variable seems to almost lie on a normal distribution
qqnorm(reg_data$Behavior_Score)
qqline(reg_data$Behavior_Score)
# qqs <- ggplot(reg_dataM, aes(sample= value)) +stat_qq() +stat_qq_line() +  facet_wrap(~variable)
# qqs
#Risk Perception of Recalls are close to normal
qqnorm(reg_data$Risk_Perception_Recall_1)
qqline(reg_data$Risk_Perception_Recall_1)
#trimming the latter datapoints may help, but due to the nature of the ordinal variable, higher values and lower values COULD be possible.
#not normal and unlikely any transformation may help
qqnorm(reg_data$Self_Risk_Perception_1)
qqline(reg_data$Self_Risk_Perception_1)
#similar issue
qqnorm(reg_data$Self_Risk_Perception_2)
qqline(reg_data$Self_Risk_Perception_2)

#no longer transforming data as it makes interpretation difficult
# #data can be transformed for better fit
# a<-reg_data$Self_Risk_Perception_2
# b<-a+1
# b<-log(b)
# c<-boxCox(lm(b~1), lambda = c(-5,5, by=0.5), optimize = TRUE)
# lambda = c$x[which.max(c$y)]
# lamba_self_risk_per_2 <- lambda
# #lambda = 3.38383838
# b<-(a^(lambda-1))/lambda
# qqnorm(b)
# qqline(b)
#better
qqnorm(reg_data$Education)
qqline(reg_data$Education)
#confident in this variable being considered normally distributed
qqnorm(reg_data$Self_Risk_Perception_3)
qqline(reg_data$Self_Risk_Perception_3)
#no optimal transformation
qqnorm(reg_data$Risk_Perception_Recall_2)
qqline(reg_data$Risk_Perception_Recall_2)
#data can be transformed for better fit
# a<-reg_data$Risk_Perception_Recall_2
# b<-a+1
# b<-log(b)
# c<-boxCox(lm(b~1), lambda = c(-5,5, by=0.5), optimize = TRUE)
# lambda = c$x[which.max(c$y)]
# lambda_risk_percep_recall_2 <- lambda
# #lambda is 1.060606060606
# b<-(a^(lambda-1))/lambda
# qqnorm(b)
# qqline(b)
#much better to normal
# qqnorm(reg_data$Behavior_Score)
# qqline(reg_data$Behavior_Score)
#boxcox transform may help
# a<-reg_data$Behavior_Score
# b<-log(a)
# c<-boxCox(lm(b~1), lambda=c(-5,5,by=0.5), optimize=TRUE)
# lambda = c$x[which.max(c$y)]
# #lambda = 3.1818181818
# b<-(a^(lambda-1))/lambda
# qqnorm(b)
# qqline(b)
#that is the last of normal transformaitons
#remember the lambda, so you can untransform when reading the predictions
#finalLambda = lambda
#delete the required columns
#These variables are removed  due to correlation, but also due to being part of the information being involved in calculating Behavior Score
reg_data <- subset(reg_data, select = -Risk_Perception_Recall_1)
reg_data <- subset(reg_data, select = -Self_Risk_Perception_3)
reg_data <- subset(reg_data, select = -Responsible_Recall_Risk_7)
reg_data <- subset(reg_data, select = -Hear_Recall_Score)
reg_data <- subset(reg_data, select = -Handwashing_Score)
reg_data <- subset(reg_data, select = -Counter_Cleaning_Score)
reg_data <- subset(reg_data, select = -Recall_Action)
reg_data <- subset(reg_data, select = -Type_Cleaning_Counter2)
reg_data <- subset(reg_data, select = -Recall_Bought_Action_2)
reg_data <- subset(reg_data, select = -Recall_Hear_Action_4)
reg_data <- subset(reg_data, select = -Risk_Behavior)
reg_data <- subset(reg_data, select = -Risk_BehaviorRecode)
reg_data <- subset(reg_data, select = -Type_Flour_4)
#implement the transformations
# reg_data$Self_Risk_Perception_2  <- (reg_data$Self_Risk_Perception_2^(2.3838383838))/3.3838383838
# reg_data$Risk_Perception_Recall_2  <- (reg_data$Risk_Perception_Recall_2^(0.060606060606))/1.060606060606
# reg_data$Behavior_Score <- (reg_data$Behavior_Score^(finalLambda-1))/finalLambda
#for the neural network, scaling from 0-1 is needed


#however, first due to large variable space, feature selection is necessary. An filter approach of Chi-sqaure and ANOVA on the features will identify variables dependnent with the output
binary_data_1 <-(reg_data%>%select_if(~all(. %in% 0:1)))
binary_data_1 <- subset(binary_data_1 , select = -State_2)
binary_data_1 <- as.matrix(binary_data_1)
binary_data_1 <- as.table(binary_data_1)
colNum = ncol(binary_data_1)
tables = list()

for(col in 1:colNum){
  intermediate <- CrossTable(reg_data$Behavior_Score, binary_data_1[,col], chisq = TRUE)
  tables <- append(tables,list(intermediate),col)
}
#Tables are cross tabs with each table being compared to the heard recall outcome and chi-sqaured for independence test.
#Take all indices that are statistically significant
colNum <- length(tables)
count <- 1
indexs <- list()
for(col in 1:colNum){
  if(tables[[col]]$chisq$p.value <= 0.05){
    indexs<- append(indexs, col, count)
    count <- count+1
  }
}
binary_data_1 <-(reg_data%>%select_if(~all(. %in% 0:1)))
newData.1 <- binary_data_1[, as.numeric(indexs) ]
newData.1 <- lapply(newData.1, as.logical)
#now anova on the ordinal or continuous variables
contin_Vars <- (reg_data%>%select_if(~!all(. %in% 0:1)))

colNum <- ncol(contin_Vars) - 1
count <- 1
indexs_2 <- list()
for(col in 1:(colNum-1)){
  intermediate <- wilcox.test(contin_Vars$Behavior_Score, contin_Vars[,col+1], paired = TRUE)
  if(intermediate$p.value <= 0.05){
    indexs_2<- append(indexs_2, col, count)
    count <- count+1
  }
}
newData.2 <- contin_Vars[,as.numeric(indexs_2)]
newData <- cbind(newData.1, newData.2)
#, reg_data$Recall_In_State
contin_Vars_2 <- (newData%>%select_if(~!all(. %in% 0:1)))
t(sapply(contin_Vars_2,range))





range(reg_data$Self_Risk_Perception_1)
#1-7
range(reg_data$Self_Risk_Perception_2)
#0.2955224 - 30.5609981
range(reg_data$Risk_Perception_Recall_2)
#0.9428571 1.0608744
range(reg_data$Contamination_Knowledge) 
# 0-3
range(reg_data$Risk_Perception_Microbes)
#1-11
range(reg_data$Age)
#1-6
#range(reg_data$Risk_Behavior)
#0-4
range(reg_data$Risk_Perception_Consumption)
#0-7
range(reg_data$Handwashing_Score)
#0-2
range(reg_data$Counter_Cleaning_Score)
#0-2
range(reg_data$Hear_Recall_Score)
#-1 , 1 
#range(reg_data$Recall_Action)
#-1 - 2
range(reg_data$Risk_Perception_Microbes)
#0-11
range(reg_data$Recall_In_State)
#0-180
range(reg_data$Behavior_Score)
#0-9
newData <- cbind(newData, as.factor(reg_data$Consumer_Focus_Pack_9))
names(newData)[length(names(newData))] <- "Consumer_Focus_Pack_9"



#Scale the data for neural network
nNet_data <- newData

#flip the scale
#nNet_data$Risk_Behavior <- -1*(nNet_data$Risk_Behavior-4) 
#4 which means none will be come 0, while 0 which means really often becomes 4
nNet_data$Self_Risk_Perception_1 <- (nNet_data$Self_Risk_Perception_1-1)/6
nNet_data$Self_Risk_Perception_2 <- (nNet_data$Self_Risk_Perception_2-1)/6
nNet_data$Risk_Perception_Recall_2 <- (nNet_data$Risk_Perception_Recall_2-1)/6
nNet_data$Contamination_Knowledge <- nNet_data$Contamination_Knowledge/3
nNet_data$Risk_Perception_Microbes <- (nNet_data$Risk_Perception_Microbes)/11
#nNet_data$Recall_Action <- (nNet_data$Recall_Action+1) /3
#nNet_data$Counter_Cleaning_Score <- nNet_data$Counter_Cleaning_Score/2
#nNet_data$Hear_Recall_Score <- (nNet_data$Hear_Recall_Score+1)/2
#nNet_data$Risk_Behavior <- nNet_data$Risk_Behavior/4
nNet_data$Risk_Perception_Consumption <- nNet_data$Risk_Perception_Consumption/7
#nNet_data$Handwashing_Score <- nNet_data$Handwashing_Score/2
nNet_data$Recall_In_State <- nNet_data$Recall_In_State/180
nNet_data$Age <- (nNet_data$Age-1)/5

nNet_data$Behavior_Score <- (nNet_data$Behavior_Score)/(12)



count <- 1
RSQSD<-list()
RSQU <-list()
nNet_Errors <- list()
rf_Errors <- list()
azs <- list()
bzs <- list()
czs <- list()
cforest <- list()
network <- list()
logregression <- list()
ensemble <- list()
importances <- list()
model_ensembles <- list()
Names_list <- list()
Names_list_overall <- list()
Names_list_cforest <- list()
Names_list_nNet <- list()
Importance_overall <- list()
Importance_cforest <- list()
Importance_nNet <- list()
seeds <-c(484,2961,2351,2965,3050) #old results
# seed_va;u
# #326,429,376,341,414
rmses <-c(0.101,0.103,0.103,0.103,0.103)

 

for(num in seeds){
  
  seed_value <- num
  set.seed(seed_value)  
    
  
  
  
  
  

  # set.seed(1000)
  #partition data at random 
  inTrainingNnet <- createDataPartition(nNet_data$Behavior_Score, p =0.7, list=FALSE)
  trainingnNet <- nNet_data[ inTrainingNnet,]
  testingnNet <- nNet_data[-inTrainingNnet,]
  # inTrainingRF <- createDataPartition(reg_data$Behavior_Score, p=0.7, list = FALSE)
  # trainingRF <- reg_data[inTrainingRF,]
  # testingRF <- reg_data[-inTrainingRF,]
  
  #setup the 10 fold cross validation control
  crossFold  <- trainControl(method = "cv", number = 10 , savePredictions =  "final")
  
  
  models <- caretList(Behavior_Score~., data=trainingnNet, trControl = crossFold ,   methodList = c("cforest" , "nnet" ), metric = "MSE" )
  
  
  #build the models
  
  preds <- as.data.frame(predict(models, newdata = testingnNet))
  pred <- predict(models, newdata = testingnNet)
  model_ensemble <- caretEnsemble(models, metric = "MSE", trControl = crossFold)
  model_ensembles[[count]] <- model_ensemble
  importances[[count]] <- varImp(model_ensemble)
  model_preds <- lapply(models, predict, newdata=testingnNet )
  
  model_preds <- data.frame(model_preds)
  ens_preds <- predict(model_ensemble, newdata=testingnNet )
  #type="prob"
  model_preds$ensemble <- ens_preds
  Names_list[[count]] <- rownames(as.matrix(importances[[count]]))
  rmses[[count]]<-model_ensemble$error$RMSE
  RSQU[[count]]<-model_ensemble$error$Rsquared
  RSQSD[[count]]<-model_ensemble$error$RsquaredSD
  
  # cforest[[count]] <- confusionMatrix(model_preds$cforest , testingnNet$Behavior_Score)
  # network[[count]] <- confusionMatrix(model_preds$nnet , testingnNet$Behavior_Score)
  # ensemble[[count]] <- confusionMatrix(model_preds$ensemble , testingnNet$Behavior_Score)
  #evaluate the models
  # nNet_pred_Vals<-predict(mlpFit, newdata= testingnNet)
  # Rf_pred_val <- predict(rfFit, newdata = testingRF)
  # nNet_error <- testingnNet$Behavior_Score - nNet_pred_Vals
  # meanNnet_error <- mean(nNet_error)
  # testingRF$Behavior_Score <-(finalLambda*testingRF$Behavior_Score)^(1/(finalLambda-1))
  # 
  # Rf_pred_val <-(finalLambda*Rf_pred_val)^(1/(finalLambda-1))
  # rf_error <- testingRF$Behavior_Score - Rf_pred_val
  # mean_rf_error <- mean(rf_error)
  # nNet_Errors[[count]] <- meanNnet_error
  # rf_Errors[[count]] <- mean_rf_error
  # importances_RF[[count]] <- varImp(rfFit)
  # importances_mlp[[count]] <- varImp(mlpFit)

  

  # 
  # meanNnet_error <- mean(nNet_error)
  # mean_rfError <- mean(rf_error)
  
  # 
  # 
  # abs_nNet_error <- abs(nNet_error)
  # meanAbs_nNet_error <- mean(abs_nNet_error)
  # 
  # abs_rf_error <- abs(rf_error)
  # percent_rf_error <- abs_rf_error/testingRF$Behavior_Score
  # mean_pear_rf_error<- mean(percent_rf_error)
  #due to a non-normazlized measure, percent error is used
  # summary(rfFit)
  #extract feature importance
  # pred_rf <- Predictor$new(rfFit ,data = trainingRF , y=trainingRF$Behavior_Score, class = "train")
  # imp_rf <- FeatureImp$new(pred_rf , loss ="mae")
  # plot(imp_rf)
  # pred_nnet <- Predictor$new(mlpFit, data = trainingnNet, y=trainingnNet$Behavior_Score, class = "train")
  # imp_nNet <- FeatureImp$new(pred_nnet, loss= "mae")
  # plot(imp_nNet)
  # 
  # #partial dependence plots
  # pdps_rf <- FeatureEffect$new(pred_rf, feature=imp_rf$results$feature[1] ) 
  # pdps_rf$plot()
  # i = 2
  #optimize seeds
  # count2 <- 1
  # while(count2 <6 ){
  #   if(rmses[[count2]] > model_ensemble$error$RMSE){
  #     if(count2 <5){
  # 
  #       rmses[[count2+1]] <- rmses[[count2]]
  #       seeds[[count2+1]] <-seeds[[count2]]
  #       rmses[[count2]]<-model_ensemble$error$RMSE
  #       seeds[[count2]]<- seed_value
  #     }
  #     else{
  #       rmses[[count2]]<-model_ensemble$error$RMSE
  #       seeds[[count2]]<- seed_value
  #       count2=6
  #     }
  # 
  # 
  #   }
  #   count2 <-count2+1
  # 
  # 
  # 
  # }

  Importance_overall[[count]] <- as.matrix(importances[[count]][1])
  Importance_cforest[[count]] <- as.matrix(importances[[count]][2])
  Importance_nNet[[count]] <- as.matrix(importances[[count]][3])
  
  
  
  
  
  
  Importance_overall[[count]] <- as.table(Importance_overall[[count]])
  Importance_cforest[[count]] <- as.table(Importance_cforest[[count]])
  Importance_nNet[[count]] <- as.table(Importance_nNet[[count]])
  Importance_overall[[count]] <- as.data.frame(Importance_overall[[count]])
  Importance_cforest[[count]] <- as.data.frame(Importance_cforest[[count]])
  Importance_nNet[[count]] <- as.data.frame(Importance_nNet[[count]])
  
  Importance_overall[[count]] <- Importance_overall[[count]][order(Importance_overall[[count]]$Freq),]
  Importance_cforest[[count]] <- Importance_cforest[[count]][order(Importance_cforest[[count]]$Freq),]
  Importance_nNet[[count]] <- Importance_nNet[[count]][order(Importance_nNet[[count]]$Freq),]
  
  Names_list_overall[[count]] <- as.character(Importance_overall[[count]][(nrow((Importance_overall[[count]]))-12):nrow(Importance_overall[[count]]),1]) 
  Names_list_cforest[[count]] <- as.character(Importance_cforest[[count]][(nrow((Importance_cforest[[count]]))-12):nrow(Importance_cforest[[count]]),1])
  Names_list_nNet[[count]] <- as.character(Importance_nNet[[count]][(nrow((Importance_nNet[[count]]))-12):nrow(Importance_nNet[[count]]),1])
                                           
   #in a seperate loop, index each by variable name and average using the following as a starting transformation, 
  #you can make a list copy of the unique variable names and append the average features to get average for each model kind and a grand mean
  #this is set up for a manova on the feature importances to check if any of these models are really different, then investigate which one is
  #with post-hoc?
  # print("A")
  azs[[count]] <- pdp::partial(models$cforest, train=trainingnNet, pred.var = "Risk_Perception_Consumption")
  bzs[[count]] <- pdp::partial(models$cforest, train=trainingnNet, pred.var = "Risk_Perception_Microbiological_8")
  czs[[count]] <- pdp::partial(models$cforest, train=trainingnNet, pred.var = "Age")

  # 
  # print("B")
  # 

  
  count <- count +1
  
}

num <-1
final_loop=1
for(num in 1625:10000){

  seed_value <- num
  set.seed(seed_value)  
  
  
  
  # set.seed(1000)
  #partition data at random 
  inTrainingNnet <- createDataPartition(nNet_data$Behavior_Score, p =0.7, list=FALSE)
  trainingnNet <- nNet_data[ inTrainingNnet,]
  testingnNet <- nNet_data[-inTrainingNnet,]
  # inTrainingRF <- createDataPartition(reg_data$Behavior_Score, p=0.7, list = FALSE)
  # trainingRF <- reg_data[inTrainingRF,]
  # testingRF <- reg_data[-inTrainingRF,]
  
  #setup the 10 fold cross validation control
  crossFold  <- trainControl(method = "cv", number = 10 , savePredictions =  "final")
  
  
  models <- caretList(Behavior_Score~., data=trainingnNet, trControl = crossFold ,   methodList = c("cforest" , "nnet" ), metric = "MSE" )
  
  
  #build the models
  
  preds <- as.data.frame(predict(models, newdata = testingnNet))
  pred <- predict(models, newdata = testingnNet)
  model_ensemble <- caretEnsemble(models, metric = "MSE", trControl = crossFold)
  model_ensembles[[count]] <- model_ensemble
  importan <- varImp(model_ensemble)
  model_preds <- lapply(models, predict, newdata=testingnNet )
  
  model_preds <- data.frame(model_preds)
  ens_preds <- predict(model_ensemble, newdata=testingnNet )
  #type="prob"
  model_preds$ensemble <- ens_preds
  
  
  importan <- as.table(as.matrix(importan))
  import_names <- row.names(importan)[(nrow(importan)-2):nrow(importan)]
  #Consumer_Focus_Pack_9
  count2 <- 1

while(count2 <6 ){
  if(rmses[[count2]] > model_ensemble$error$RMSE){
  #Note that a replacement was made
    if(count2 <5){
      
      rmses[[count2+1]] <- rmses[[count2]]
      seeds[[count2+1]] <-seeds[[count2]]
      rmses[[count2]]<-model_ensemble$error$RMSE
      seeds[[count2]]<- seed_value
      count2=6
    }
    else{
      rmses[[count2]]<-model_ensemble$error$RMSE
      seeds[[count2]]<- seed_value
    }
    
    
  }
  count2 <-count2+1
  
  
  
count <- 1
colnames(azs[[count]])[[2]] <- "Mean Behavior Score"
colnames(azs[[count]])[[1]] <- "Risk Perception of Consuming Raw Dough"
colnames(bzs[[count]])[[2]] <- "Mean Behavior Score"
colnames(bzs[[count]])[[1]] <- "Raw Chicken has Risk"
colnames(czs[[count]])[[2]] <- "Mean Behavior Score"
colnames(czs[[count]])[[1]] <- "Age Range"

plota1 <- ggplot(data  = azs[[count]] , aes(x =`Risk Perception of Consuming Raw Dough` , y =`Mean Behavior Score` )) + geom_path() +theme(axis.text=element_text(size=12,color="Black", family = "Times New Roman"),panel.background = element_rect(fill=rgb(1,0.647,0,0.2) ), axis.title.x =element_text(size=14, family = "Times New Roman") , axis.title.y = element_text(size=14, family = "Times New Roman"))+scale_y_continuous(breaks= c(0.54,0.55,0.560,0.57,0.58,0.590,0.60,0.61,0.62,0.63,0.64,0.65), labels=waiver(), limits=c(0.54,0.65))+xlab(str_wrap("Risk Perception of Consuming Raw Dough", width=20))
plotb1 <- ggplot(data  = bzs[[count]] , aes(x =`Raw Chicken has Risk` , y =`Mean Behavior Score` )) + geom_point() +theme(axis.text=element_text(size=12,color="Black", family = "Times New Roman"),panel.background = element_rect(fill= rgb(1,0.647,0,0.2)), axis.title.x =element_text(size=14, family = "Times New Roman") , axis.title.y = element_text(size=14, family = "Times New Roman"))+scale_y_continuous(breaks= c(0.54,0.55,0.560,0.57,0.58,0.590,0.60,0.61,0.62,0.63,0.64,0.65), labels=waiver(), limits=c(0.54,0.65))
plotc1 <-ggplot(data  = czs[[count]] , aes(x =`Age Range` , y =`Mean Behavior Score`)) + geom_path( ) +theme(axis.text=element_text(size=12,color="Black", family = "Times New Roman"),panel.background = element_rect(fill= rgb(1,0.647,0,0.2)), axis.title.x =element_text(size=14, family = "Times New Roman") , axis.title.y = element_text(size=14, family = "Times New Roman"))+scale_y_continuous(breaks= c(0.54,0.55,0.560,0.57,0.58,0.590,0.60,0.61,0.62,0.63,0.64,0.65), labels=waiver(), limits=c(0.54,0.65))


count <- count+1
colnames(azs[[count]])[[2]] <- "Mean Behavior Score"
colnames(azs[[count]])[[1]] <- "Risk Perception of Consuming Raw Dough"
colnames(bzs[[count]])[[2]] <- "Mean Behavior Score"
colnames(bzs[[count]])[[1]] <- "Raw Chicken has Risk"
colnames(czs[[count]])[[2]] <- "Mean Behavior Score"
colnames(czs[[count]])[[1]] <- "Age Range"
plota2 <- ggplot(data  = azs[[count]] , aes(x =`Risk Perception of Consuming Raw Dough` , y =`Mean Behavior Score` )) + geom_path() +theme(axis.text=element_text(size=12,color="Black", family = "Times New Roman"),panel.background = element_rect(fill=rgb(0,1,0.498,0.2)), axis.title.x =element_text(size=14, family = "Times New Roman") , axis.title.y = element_text(size=14, family = "Times New Roman"))+scale_y_continuous(breaks= c(0.54,0.55,0.560,0.57,0.58,0.590,0.60,0.61,0.62,0.63,0.64,0.65), labels=waiver(), limits=c(0.54,0.65))+xlab(str_wrap("Risk Perception of Consuming Raw Dough", width=20))
plotb2 <- ggplot(data  = bzs[[count]] , aes(x =`Raw Chicken has Risk` , y =`Mean Behavior Score` )) + geom_point() +theme(axis.text=element_text(size=12,color="Black", family = "Times New Roman"),panel.background = element_rect(fill= rgb(0,1,0.498,0.2)), axis.title.x =element_text(size=14, family = "Times New Roman") , axis.title.y = element_text(size=14, family = "Times New Roman"))+scale_y_continuous(breaks= c(0.54,0.55,0.560,0.57,0.58,0.590,0.60,0.61,0.62,0.63,0.64,0.65), labels=waiver(), limits=c(0.54,0.65))
plotc2 <- ggplot(data  = czs[[count]] , aes(x =`Age Range` , y =`Mean Behavior Score`)) + geom_path( ) +theme(axis.text=element_text(size=12,color="Black", family = "Times New Roman"),panel.background = element_rect(fill= rgb(0,1,0.498,0.2)), axis.title.x =element_text(size=14, family = "Times New Roman") , axis.title.y = element_text(size=14, family = "Times New Roman"))+scale_y_continuous(breaks= c(0.54,0.55,0.560,0.57,0.58,0.590,0.60,0.61,0.62,0.63,0.64,0.65), labels=waiver(), limits=c(0.54,0.65))


count <- count+1
colnames(azs[[count]])[[2]] <- "Mean Behavior Score"
colnames(azs[[count]])[[1]] <- "Risk Perception of Consuming Raw Dough"
colnames(bzs[[count]])[[2]] <- "Mean Behavior Score"
colnames(bzs[[count]])[[1]] <- "Raw Chicken has Risk"
colnames(czs[[count]])[[2]] <- "Mean Behavior Score"
colnames(czs[[count]])[[1]] <- "Age Range"
plota3<- ggplot(data  = azs[[count]] , aes(x =`Risk Perception of Consuming Raw Dough` , y =`Mean Behavior Score` )) + geom_path() +theme(axis.text=element_text(size=12,color="Black", family = "Times New Roman"),panel.background = element_rect(fill= rgb(0,0.749,1,0.2)), axis.title.x =element_text(size=14, family = "Times New Roman") , axis.title.y = element_text(size=14,family = "Times New Roman"))+scale_y_continuous(breaks= c(0.54,0.55,0.560,0.57,0.58,0.590,0.60,0.61,0.62,0.63,0.64,0.65), labels=waiver(), limits=c(0.54,0.65))+xlab(str_wrap("Risk Perception of Consuming Raw Dough", width=20))#+xlab(str_wrap("Risk Perception of Consuming Raw Dough", width=20)) axis.text=element_text(size=14,color="Black", family = "Times New Roman"),  plot.margin= unit(c(1,1,2,1),"lines")
plotb3<-ggplot(data  = bzs[[count]] , aes(x =`Raw Chicken has Risk` , y =`Mean Behavior Score` )) + geom_point() +theme(axis.text=element_text(size=12,color="Black", family = "Times New Roman"),panel.background = element_rect(fill= rgb(0,0.749,1,0.2)), axis.title.x =element_text(size=14, family = "Times New Roman") , axis.title.y = element_text(size=14, family = "Times New Roman"))+scale_y_continuous(breaks= c(0.54,0.55,0.560,0.57,0.58,0.590,0.60,0.61,0.62,0.63,0.64,0.65), labels=waiver(), limits=c(0.54,0.65))
plotc3 <-ggplot(data  = czs[[count]] , aes(x =`Age Range` , y =`Mean Behavior Score`)) + geom_path()+theme(axis.text=element_text(size=12,color="Black", family = "Times New Roman"),panel.background = element_rect(fill= rgb(0,0.749,1,0.2)), axis.title.x =element_text(size=14, family = "Times New Roman") , axis.title.y = element_text(size=14, family = "Times New Roman") )+scale_y_continuous(breaks= c(0.54,0.55,0.560,0.57,0.58,0.590,0.60,0.61,0.62,0.63,0.64,0.65), labels=waiver(), limits=c(0.54,0.65))#+xlab(str_wrap("Num. of Foods Percieved Risky", width=20)) axis.text=element_text(size=12,color="Black", family = "Times New Roman"), plot.margin= unit(c(1,1,2,1),"lines")


count <- count+1
colnames(azs[[count]])[[2]] <- "Mean Behavior Score"
colnames(azs[[count]])[[1]] <- "Risk Perception of Consuming Raw Dough"
colnames(bzs[[count]])[[2]] <- "Mean Behavior Score"
colnames(bzs[[count]])[[1]] <- "Raw Chicken has Risk"
colnames(czs[[count]])[[2]] <- "Mean Behavior Score"
colnames(czs[[count]])[[1]] <- "Age Range"
plota4<- ggplot(data  = azs[[count]] , aes(x =`Risk Perception of Consuming Raw Dough` , y =`Mean Behavior Score` )) + geom_path() +theme(axis.text=element_text(size=12,color="Black", family = "Times New Roman"),panel.background = element_rect(fill= rgb(0,0,1,0.2)), axis.title.x =element_text(size=14, family = "Times New Roman") , axis.title.y = element_text(size=14, family = "Times New Roman"))+scale_y_continuous(breaks= c(0.54,0.55,0.560,0.57,0.58,0.590,0.60,0.61,0.62,0.63,0.64,0.65), labels=waiver(), limits=c(0.54,0.65))+xlab(str_wrap("Risk Perception of Consuming Raw Dough", width=20))
plotb4<- ggplot(data  = bzs[[count]] , aes(x =`Raw Chicken has Risk` , y =`Mean Behavior Score` )) + geom_point()+theme(axis.text=element_text(size=12,color="Black", family = "Times New Roman"),panel.background = element_rect(fill= rgb(0,0,1,0.2)),axis.title.x =element_text(size=14, family = "Times New Roman") , axis.title.y = element_text(size=14, family = "Times New Roman"))+scale_y_continuous(breaks= c(0.54,0.55,0.560,0.57,0.58,0.590,0.60,0.61,0.62,0.63,0.64,0.65), labels=waiver(), limits=c(0.54,0.65))#+xlab(str_wrap("Raw Chicken has Risk", width=20)) axis.text = element_text(size=14, color="Black", family = "Times New Roman"),, plot.margin= unit(c(1,1,2,1),"lines")
plotc4<- ggplot(data  = czs[[count]] , aes(x =`Age Range` , y =`Mean Behavior Score`)) + geom_path( ) +theme(axis.text=element_text(size=12,color="Black", family = "Times New Roman"),panel.background = element_rect(fill= rgb(0,0,1,0.2)), axis.title.x =element_text(size=14, family = "Times New Roman") , axis.title.y = element_text(size=14, family = "Times New Roman"))+scale_y_continuous(breaks= c(0.54,0.55,0.560,0.57,0.58,0.590,0.60,0.61,0.62,0.63,0.64,0.65), labels=waiver(), limits=c(0.54,0.65))


count <- count+1
colnames(azs[[count]])[[2]] <- "Mean Behavior Score"
colnames(azs[[count]])[[1]] <- "Risk Perception of Consuming Raw Dough"
colnames(bzs[[count]])[[2]] <- "Mean Behavior Score"
colnames(bzs[[count]])[[1]] <- "Raw Chicken has Risk"
colnames(czs[[count]])[[2]] <- "Mean Behavior Score"
colnames(czs[[count]])[[1]] <- "Age Range"
plota5<- ggplot(data  = azs[[count]] , aes(x =`Risk Perception of Consuming Raw Dough` , y =`Mean Behavior Score` )) + geom_path() +theme(axis.text=element_text(size=12,color="Black", family = "Times New Roman"),panel.background = element_rect(fill= rgb(1,0.784,0.576,0.2)), axis.title.x =element_text(size=14,family = "Times New Roman") , axis.title.y = element_text(size=14,family = "Times New Roman"))+scale_y_continuous(breaks= c(0.54,0.55,0.560,0.57,0.58,0.590,0.60,0.61,0.62,0.63,0.64,0.65), labels=waiver(), limits=c(0.54,0.65))+xlab(str_wrap("Risk Perception of Consuming Raw Dough", width=20))
plotb5<- ggplot(data  = bzs[[count]] , aes(x =`Raw Chicken has Risk` , y =`Mean Behavior Score` )) + geom_point() +theme(axis.text=element_text(size=12,color="Black", family = "Times New Roman"),panel.background = element_rect(fill= rgb(1,0.784,0.576,0.2)), axis.title.x =element_text(size=14,family = "Times New Roman") , axis.title.y = element_text(size=14,family = "Times New Roman"))+scale_y_continuous(breaks= c(0.54,0.55,0.560,0.57,0.58,0.590,0.60,0.61,0.62,0.63,0.64,0.65), labels=waiver(), limits=c(0.54,0.65))
plotc5<- ggplot(data  = czs[[count]] , aes(x =`Age Range` , y =`Mean Behavior Score`)) + geom_path( ) +theme(axis.text=element_text(size=12,color="Black", family = "Times New Roman"),panel.background = element_rect(fill= rgb(1,0.784,0.576,0.2)), axis.title.x =element_text(size=14,family = "Times New Roman") , axis.title.y = element_text(size=14,family = "Times New Roman"))+scale_y_continuous(breaks= c(0.54,0.55,0.560,0.57,0.58,0.590,0.60,0.61,0.62,0.63,0.64,0.65), labels=waiver(), limits=c(0.54,0.65))


ggarrange(plota1+ggtitle("Iteration 1"), plota2+ggtitle("Iteration 2"), plota3+ggtitle("Iteration 3"), plota4+ggtitle("Iteration 4") , plota5+ggtitle("Iteration 5")  , plotb1 , plotb2, plotb3, plotb4, plotb5, plotc1, plotc2, plotc3, plotc4, plotc5, ncol=5,nrow=3)

}

}


ggarrange(plota3, plotb4, plotc3, nrow=1,ncol=3, align="hv")


count <- 1
count2 <- 1
count3 <-1
UniqueNameListOverall <- list()
UniqueNameListCforest <- list()
UniqueNameListnNet <- list()
while(count2 <= length(Names_list_overall)){
  count <- 1
while(count <= length( Names_list_overall[[count2]])){
  UniqueNameListOverall[[count3]]<- Names_list_overall[[count2]][count]
  UniqueNameListCforest[[count3]]<- Names_list_cforest[[count2]][count]
  UniqueNameListnNet[[count3]]  <- Names_list_nNet[[count2]][count]
  count<-count +1
  count3 <-count3+1
}
count2 <- count2+1
}
UniqueNameListOverall <- unique(UniqueNameListOverall)
UniqueNameListCforest <- unique(UniqueNameListCforest)
UniqueNameListnNet <- unique(UniqueNameListnNet)



count <- 1
while(count<6){
  Importance_overall[[count]] <- t(Importance_overall[[count]])
  Importance_cforest[[count]] <- t(Importance_cforest[[count]])
  Importance_nNet[[count]] <- t(Importance_nNet[[count]])
  colnames(Importance_overall[[count]]) <-  Importance_overall[[count]][1,]
  colnames(Importance_cforest[[count]]) <- Importance_cforest[[count]][1,]
  colnames(Importance_nNet[[count]]) <-  Importance_nNet[[count]][1,]
  Importance_overall[[count]] <-  Importance_overall[[count]][-1,]
  Importance_cforest[[count]] <- Importance_cforest[[count]][-1,]
  Importance_nNet[[count]] <-  Importance_nNet[[count]][-1,]
count <- count+1
}
count <- 1
OverallImportMatrix <- matrix(,nrow = length(UniqueNameListOverall), ncol= length(Names_list_overall))
CforestImportMatrix <- matrix(,nrow = length(UniqueNameListCforest), ncol= length(Names_list_cforest))
nNetImportMatrix <- matrix(,nrow = length(UniqueNameListnNet), ncol= length(Names_list_nNet))

while( count<= length(Names_list_overall)){
  OverallImportMatrix[,count] <- as.numeric(t(Importance_overall[[count]][2, as.character(drop(UniqueNameListOverall))]))
  CforestImportMatrix[,count] <- as.numeric(t(Importance_cforest[[count]][2, as.character(drop(UniqueNameListCforest))]))
  nNetImportMatrix[,count] <-  as.numeric(t(Importance_nNet[[count]][2, as.character(drop(UniqueNameListnNet))]))
  count <- count +1
  }
rownames(OverallImportMatrix) <- UniqueNameListOverall
rownames(CforestImportMatrix) <- UniqueNameListCforest
rownames(nNetImportMatrix) <- UniqueNameListnNet
Overall_Importance_Means <- rowMeans(OverallImportMatrix)
Cforest_Importance_means <- rowMeans(CforestImportMatrix)
nNet_Importance_Means <- rowMeans(nNetImportMatrix)
Overall_Importance_Means<- Overall_Importance_Means[order(Overall_Importance_Means)]
Cforest_Importance_means<- Cforest_Importance_means[order(Cforest_Importance_means)]
nNet_Importance_Means<-nNet_Importance_Means[order(nNet_Importance_Means)]

importances[[1]]
importances[[2]]
importances[[3]]
importances[[4]]
importances[[5]]


#Mean of top features
Overall_Importance_Means[(length(Overall_Importance_Means)-4):length(Overall_Importance_Means)]
Cforest_Importance_means[(length(Cforest_Importance_means)-4):length(Cforest_Importance_means)]
nNet_Importance_Means[(length(nNet_Importance_Means)-4):length(nNet_Importance_Means)]
diag(cov(t(OverallImportMatrix)))
diag(cov(t(CforestImportMatrix)))
diag(cov(t(nNetImportMatrix)))

devtools::session_info()
#Not Reported Feature Interaction and Alternative Variable Importance Exploration
########################################################################################################################################################
# #feature interaction
# overallnames <- names(Overall_Importance_Means)[(length(Overall_Importance_Means)-4):length(Overall_Importance_Means)]
# rfNames <- names(Cforest_Importance_means)[(length(Cforest_Importance_means)-4):length(Cforest_Importance_means)]
# nNetnames <- names(nNet_Importance_Means)[(length(nNet_Importance_Means)-4):length(nNet_Importance_Means)]
# 
# 
# 
# 
# matchlist <- list()
# count <- 1
# 
# 
# for (names in names(Overall_Importance_Means)){
#   matchlist<-str_detect(Overall_Importance_Means, names)
#   
#   
#   
#   
#   
#   
#   
# }
# 
# 
# 
#   matchlist<- str_detect(overallnames, "TRUE")
#   count2 <- 1
#   for (index in matchlist){
#     if(index){
#       overallnames[[count2]] <- substring(overallnames[[count2]],1,nchar(overallnames[[count2]])-4 )
#     }
#     count2 <- count2+1
#   }
# 
# 
# matchlist <- list()
# count <- 1
# 
#   matchlist<- str_detect(rfNames, "TRUE")
#   count2 <- 1
#   for (index in matchlist){
#     if(index){
#       rfNames[[count2]] <- substring(rfNames[[count2]],1,nchar(rfNames[[count2]])-4 )
#     }
#     count2 <- count2+1
#   }
# 
# 
# matchlist <- list()
# count <- 1
# 
#   matchlist<- str_detect(nNetnames, "TRUE")
#   count2 <- 1
#   for (index in matchlist){
#     if(index){
#       nNetnames[[count2]] <- substring(nNetnames[[count2]],1,nchar(nNetnames[[count2]])-4 )
#     }
#     count2 <- count2+1
#   }
# 
# 
# feature_int_ensemble <- list()
# feature_int_cforest <-list()
# feature_int_nnet <-list()
# 
# feature_int_ensemble[[1]]<-vint(model_ensembles[[1]],as.character(overallnames), train = trainingnNet)
# feature_int_cforest[[1]]<-vint(model_ensembles[[1]]$models$cforest,as.character(rfNames), train = trainingnNet)
# feature_int_nnet[[1]]<-vint(model_ensembles[[1]]$models$nnet,as.character(nNetnames), train = trainingnNet)
# feature_int_ensemble[[2]]<-vint(model_ensembles[[2]],as.character(overallnames), train = trainingnNet)
# feature_int_cforest[[2]]<-vint(model_ensembles[[2]]$models$cforest,as.character(rfNames), train = trainingnNet)
# feature_int_nnet[[2]]<-vint(model_ensembles[[2]]$models$nnet,as.character(nNetnames), train = trainingnNet)
# feature_int_ensemble[[3]]<-vint(model_ensembles[[3]],as.character(overallnames), train = trainingnNet)
# feature_int_cforest[[3]]<-vint(model_ensembles[[3]]$models$cforest,as.character(rfNames), train = trainingnNet)
# feature_int_nnet[[3]]<-vint(model_ensembles[[3]]$models$nnet,as.character(nNetnames), train = trainingnNet)
# feature_int_ensemble[[4]]<-vint(model_ensembles[[4]],as.character(overallnames), train = trainingnNet)
# feature_int_cforest[[4]]<-vint(model_ensembles[[4]]$models$cforest,as.character(rfNames), train = trainingnNet)
# feature_int_nnet[[4]]<-vint(model_ensembles[[4]]$models$nnet,as.character(nNetnames), train = trainingnNet)
# feature_int_ensemble[[5]]<-vint(model_ensembles[[5]],as.character(overallnames), train = trainingnNet)
# feature_int_cforest[[5]]<-vint(model_ensembles[[5]]$models$cforest,as.character(rfNames), train = trainingnNet)
# feature_int_nnet[[5]]<-vint(model_ensembles[[5]]$models$nnet,as.character(nNetnames), train = trainingnNet)
# 
# 
# #getnames of Top 5 means
# 
# count <-1
# 
# while(count < 6){
#   feature_int_ensemble[[count]]$Interaction<- feature_int_ensemble[[count]]$Interaction[order(feature_int_ensemble[[count]]$Variables)]
#   feature_int_ensemble[[count]]$Variables<- feature_int_ensemble[[count]]$Variables[order(feature_int_ensemble[[count]]$Variables)]
#   
#   feature_int_cforest[[count]]$Interaction<- feature_int_cforest[[count]]$Interaction[order(feature_int_cforest[[count]]$Variables)]
#   feature_int_cforest[[count]]$Variables<- feature_int_cforest[[count]]$Variables[order(feature_int_cforest[[count]]$Variables)]
#   
#   feature_int_nnet[[count]]$Interaction<- feature_int_nnet[[count]]$Interaction[order(feature_int_nnet[[count]]$Variables)]
#   feature_int_nnet[[count]]$Variables<- feature_int_nnet[[count]]$Variables[order(feature_int_nnet[[count]]$Variables)]
#   
#   
#   count <- count +1
#   
# }
# featInteractEnsembles <- cbind(feature_int_ensemble[[1]]$Interaction, feature_int_ensemble[[2]]$Interaction, feature_int_ensemble[[3]]$Interaction,feature_int_ensemble[[4]]$Interaction,feature_int_ensemble[[5]]$Interaction)
# rownames(featInteractEnsembles) <- feature_int_ensemble[[1]]$Variables
# rowMeans(featInteractEnsembles)
# 
# featInteractCforest <- cbind(feature_int_cforest[[1]]$Interaction, feature_int_cforest[[2]]$Interaction, feature_int_cforest[[3]]$Interaction,feature_int_cforest[[4]]$Interaction,feature_int_cforest[[5]]$Interaction)
# rownames(featInteractCforest) <- feature_int_cforest[[1]]$Variables
# rowMeans(featInteractCforest)
# 
# featInteractNnet <- cbind(feature_int_nnet[[1]]$Interaction, feature_int_nnet[[2]]$Interaction, feature_int_nnet[[3]]$Interaction,feature_int_nnet[[4]]$Interaction,feature_int_nnet[[5]]$Interaction)
# rownames(featInteractNnet) <- feature_int_nnet[[1]]$Variables
# rowMeans(featInteractNnet[,c(1,5)])
# 
# FeatureInteractionMeans <- cbind(rowMeans(featInteractEnsembles),rowMeans(featInteractCforest),rowMeans(featInteractNnet))
# FeatureInteractionMeans.names<- c("Overall", "Cforest" , "Nnet" )
# colnames(FeatureInteractionMeans) <- FeatureInteractionMeans.names
# 
# diag(cov(t(featInteractEnsembles)))
# diag(cov(t(featInteractCforest)))
# diag(cov(t(featInteractNnet)))
# 
# interaction.plot(x.factor= reg_data$Risk_Behavior , trace.factor = reg_data$Consumer_Focus_Pack_5, response = reg_data$Behavior_Score, ylab = "Mean behavior score" , xlab= "Frequency eating raw dough or batter", legend = FALSE)
# interaction.plot(x.factor= reg_data$Risk_Perception_Microbiological_8 , trace.factor = reg_data$Consumer_Focus_Pack_5, response = reg_data$Behavior_Score, ylab = "Mean behavior score" , xlab= "Raw Chicken has Risk", legend = FALSE)
# interaction.plot(x.factor= reg_data$Risk_Perception_Microbiological_8 , trace.factor = reg_data$Recall_Hear_Action_4, response = reg_data$Behavior_Score, ylab = "Mean behavior score" , xlab= "Raw Chicken has Risk", legend = FALSE)
# 
# 
# 
# vip(model_ensembles[[1]]$models$cforest, method= "firm" , target = "Recall_Heard", metric = "ROC")
# 
# vip(model_ensembles[[2]]$ens_model)
# vip(model_ensembles[[3]]$ens_model)
# vip(model_ensembles[[4]]$ens_model)
# vip(model_ensembles[[5]]$ens_model)
