#import needed libraries
library(psych)
library(base)
library(colorspace)
library(datasets)
library(graphics)
library(grDevices)
library(digest)
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
library(gmodels)
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
library(vip)
library(party)
library(plyr)
library(dplyr)
library(LogicReg)
library(caret)
library(caretEnsemble)
library(pdp)
library(agricolae)
library(DALEX)
#Start with importing the data, saved as a tab delimited text file, variables on header
reg_data <- read.table("D:/School/Grad School/Chapter 3/Model 2 Datafile.txt", header = T , as.is = T)
#Next run correlaitons
corrs <- cor(reg_data)
#self_risk_perception_2 and _3 are strongly correlated at 0.7, as such, one will be removed from the input to maintain independence
#Risk_perception_recall_1 and _2 are highly correlated at 0.62, as such one will be removed from the input to maintain independence
#Ethnicity 1 is moderate correlated to 2 and 3, which indicates it as the most frequent ethnicity
#Responsible Recall Risk 7 and 8 are correlated at 0.6
#repeat using Spearman's
corrs_spearman <- cor(reg_data, method = "spearman")
#no new information is gained
#try with kendall tau
corrs_kendall <- cor(reg_data, method = "kendall")
# no new information

#since datasets are the same, following sets are skip[ped]

# #now time to use tetrachoric corrlation on the binary variables
# 
# #Create Crosstables
# act_5vrec_heard <- CrossTable(reg_data$Recall_Hear_Action_5,reg_data$Recall_Heard)
# act_5vrec_bough <-CrossTable(reg_data$Recall_Hear_Action_5, reg_data$Recall_Bought_Action_5)
# rec_heardvrec_bough <-CrossTable(reg_data$Recall_Heard, reg_data$Recall_Bought_Action_5)
# 
# #create actual matrices of the Ns
# act_5vrec_heard.a <- c(632, 182)
# act_5vrec_heard.b <- c(103, 37)
# act_5vrec_heard <- cbind(act_5vrec_heard.a, act_5vrec_heard.b)
# act_5vrec_bough.a <- c(717,214)
# act_5vrec_bough.b <- c(18,5)
# act_5vrec_bough <- cbind(act_5vrec_bough.a,act_5vrec_bough.b)
# rec_heardvrec_bough.a <- c(796, 135)
# rec_heardvrec_bough.b <- c(18,5)
# rec_heardvrec_bough <- cbind(rec_heardvrec_bough.a,rec_heardvrec_bough.b)
# 
# #run tetrachoric using package: psych
# tetrachoric(act_5vrec_heard)
# tetrachoric(act_5vrec_bough)reg_data$Self_Risk_Perception_1 <- logit(reg_data$Self_Risk_Perception_1)
# tetrachoric(rec_heardvrec_bough)

#correlations are all small
#Due to dimension size, scatter plots are not available, previous investigation on the continuous data show low linearity
#many new variables are binary. Also, pearson's correlation suggests many variables with veyr low correlation
#Clearly there is no lineraly related data, as such probablistic and non-linear models are appropriate to use to explore


#lastly to check the distribution  each variable in monovarite
reg_dataM <- melt(reg_data)
hists <- ggplot(reg_dataM, aes(x=value)) + stat_count()+facet_wrap(~variable)
hists
#Behavior score as a predicted variable seems to lie on a normal distribution
#Recipe Media 5 is also on normal for binary


qqs <- ggplot(reg_dataM, aes(sample= value)) +stat_qq() +stat_qq_line() +  facet_wrap(~variable)
qqs
#outbreaks in state disrupt the other plot's scale
reg_dataM <- melt(subset(reg_data, select = -Recall_In_State))
qqs <- ggplot(reg_dataM, aes(sample= value)) +stat_qq() +stat_qq_line() +  facet_wrap(~variable)
qqs


qqnorm(reg_data$Responsible_Recall_Risk_7)
qqline(reg_data$Responsible_Recall_Risk_7)
qqnorm(reg_data$Responsible_Recall_Risk_8)
qqline(reg_data$Responsible_Recall_Risk_8)
#both variables are far from normal
var(reg_data$Responsible_Recall_Risk_7)
var(reg_data$Responsible_Recall_Risk_8)
#Responsible_Recall_Risk_8 has lower variance, suggesting it has a better representation of the true value

# Commented as investgations were conducted in previous model 
#futher investigations into normality is warranted
# #Behavior is very close to normal distribution, logit transformations of the variables may help
# qqnorm(reg_data$Risk_Perception_Recall_1)
# qqline(reg_data$Risk_Perception_Recall_1)
# #trimming the latter datapoints may help, but due to the nature of the ordinal variable, higher values and lower values COULD be possible.
# #not normal and unlikely any transformation may help
# qqnorm(reg_data$Self_Risk_Perception_1)
# qqline(reg_data$Self_Risk_Perception_1)
# #similar issue
# qqnorm(reg_data$Self_Risk_Perception_2)
# qqline(reg_data$Self_Risk_Perception_2)
# #data can be transformed for better fit
# a<-reg_data$Self_Risk_Perception_2
# b<-a+1
# b<-log(b)
# c<-boxCox(lm(b~1), lambda = c(-5,5, by=0.5), optimize = True)
# lambda = c$x[which.max(c$y)]
# lamba_self_risk_per_2 <- lamba
# #lambda = 3.38383838
# b<-(a^(lambda-1))/lambda
# qqnorm(b)
# qqline(b)
# #better
# qqnorm(reg_data$Education)
# qqline(reg_data$Education)
# #confident in this variable being considered normally distributed
# qqnorm(reg_data$Self_Risk_Perception_3)
# qqline(reg_data$Self_Risk_Perception_3)
# #no optimal transformation
# qqnorm(reg_data$Risk_Perception_Recall_2)
# qqline(reg_data$Risk_Perception_Recall_2)
# #data can be transformed for better fit

#transform the variables
a<-reg_data$Risk_Perception_Recall_2
b<-a+1
b<-log(b)
c<-boxCox(lm(b~1), lambda = c(-5,5, by=0.5), optimize =TRUE)
lambda = c$x[which.max(c$y)]
lambda_risk_percep_recall_2 <- lambda
#lambda is 1.060606060606
b<-(a^(lambda-1))/lambda
qqnorm(b)
qqline(b)
#much better to normal
# qqnorm(reg_data$Behavior_Score)
# qqline(reg_data$Behavior_Score)
#boxcox transform may help
# a<-reg_data$Behavior_Score
# b<-log(a)
# c<-boxCox(lm(b~1), lambda=c(-5,5,by=0.5), optimize=TRUE)
# lambda = c$x[which.max(c$y)]
#lambda = 3.1818181818
# b<-(a^(lambda-1))/lambda
# qqnorm(b)
# qqline(b)
#that is the last of normal transformaitons
#remember the lambda, so you can untransform when reading the predictions
finalLambda = lambda
#delete the required columns
reg_data <- subset(reg_data, select = -Risk_Perception_Recall_1)
reg_data <- subset(reg_data, select = -Self_Risk_Perception_3)
reg_data <- subset(reg_data, select = -Responsible_Recall_Risk_7)
reg_data <- subset(reg_data, select = -Risk_Behavior)
reg_data <- subset(reg_data, select = -Risk_BehaviorRecode)

#implement the transformations
# reg_data$Self_Risk_Perception_2  <- (reg_data$Self_Risk_Perception_2^(2.3838383838))/3.3838383838
# reg_data$Risk_Perception_Recall_2  <- (reg_data$Risk_Perception_Recall_2^(0.060606060606))/1.060606060606
#reg_data$Behavior_Score <- (reg_data$Behavior_Score^(finalLambda-1))/finalLambda

#for the neural network, scaling from 0-1 is needed



#however, first due to large variable space, feature selection is necessary. An filter approach of Chi-sqaure and ANOVA on the features will identify variables dependnent with the output
binary_data_1 <-(reg_data%>%select_if(~all(. %in% 0:1)))
binary_data_1 <- subset(binary_data_1 , select = -State_2)
binary_data_1 <- as.matrix(binary_data_1)
binary_data_1 <- as.table(binary_data_1)
colNum = ncol(binary_data_1)-1
tables = list()

for(col in 1:colNum){
  intermediate <- CrossTable(binary_data_1[,1], binary_data_1[,col+1], chisq = TRUE)
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
newData.1 <- sapply(newData.1, as.logical)
#now anova on the ordinal or continuous variables
contin_Vars <- (reg_data%>%select_if(~!all(. %in% 0:1)))
contin_Vars$Recall_Heard <- binary_data_1$Recall_Heard
colNum <- ncol(contin_Vars) - 1
count <- 1
indexs_2 <- list()
for(col in 1:colNum){
  intermediate <- pairwise.wilcox.test(contin_Vars[,col] , contin_Vars$Recall_Heard)
  
  if(intermediate$p.value <= 0.05){
    indexs_2<- append(indexs_2, col, count)
    count <- count+1
  }
}
newData.2 <- contin_Vars[,as.numeric(indexs_2)]
newData <- cbind(newData.1, newData.2)
#, reg_data$Recall_In_State
contin_Vars_2 <- (newData%>%select_if(~!all(. %in% 0:1)))

#view the range of continuous variables
t(sapply(contin_Vars_2,range))


range(reg_data$Self_Risk_Perception_2)
#0.2955224 - 30.5609981
range(reg_data$Risk_Perception_Recall_2)
#0.9428571 1.0608744
range(reg_data$Contamination_Knowledge)
#0-3
range(reg_data$Age)
#1-6
range(reg_data$Education)
#1-5
range(reg_data$Recall_Action)
#-1 - 2
range(reg_data$Counter_Cleaning_Score)
# 0-2
range(reg_data$Hear_Recall_Score)
#-1 , 1 


#Scale the data 
nNet_data <- newData


colnames(nNet_data)[colNum-1] <- "Recall_In_State"
nNet_data$Self_Risk_Perception_2 <- (nNet_data$Self_Risk_Perception_2-1)/(6)
nNet_data$Risk_Perception_Recall_2 <- (nNet_data$Risk_Perception_Recall_2-1)/(6)
nNet_data$Contamination_Knowledge <- nNet_data$Contamination_Knowledge/3
nNet_data$Age <- (nNet_data$Age-1)/5
nNet_data$Education <- (nNet_data$Education-1)/3
nNet_data$Recall_Action <- (nNet_data$Recall_Action+1) /3
nNet_data$Counter_Cleaning_Score <- nNet_data$Counter_Cleaning_Score/2
nNet_data$Hear_Recall_Score <- (nNet_data$Hear_Recall_Score+1)/2
#nNet_data$Risk_Behavior <- nNet_data$Risk_Behavior/4
nNet_data$Risk_Perception_Consumption <- nNet_data$Risk_Perception_Consumption/7
nNet_data$Risk_Perception_Microbes <- nNet_data$Risk_Perception_Microbes/11
#nNet_data$Behavior_Score <- nNet_data$Behavior_Score/12
nNet_data$Recall_In_State <- nNet_data$Recall_In_State/180
#initalize variables
cforest <- list()
network <- list()
logregression <- list()
ensemble <- list()
importances <- list()
modellists <- list()
model_ensembles <- list()
#collect the row names for each importances
Names_list <- list()
Names_list_overall <- list()
Names_list_cforest <- list()
Names_list_nNet <- list()
Names_list_logReg <- list()
azs <- list()
bzs <- list()
czs <- list()
dzs <- list()
Importance_overall <- list()
Importance_cforest <- list()
Importance_nNet <- list()
Importance_logReg <- list()
aucs <- list()

#set.seed(1000)
 
count <- 1
seeds <- list()
seeds <- c(3279,2975,8118, 7664, 3012 )
#seeds <- c(3279,1928,6128,4242,8531) old seeds
# while(count <6){
 #seed_value <- sample(1:9999,1)
   #set.seed(seed_value)
 # <- .Random.seed
for(num in seeds){

  set.seed(num)
  
  
#partition data at random 
  inTrainingNnet <- createDataPartition(nNet_data$Recall_Heard, p =0.7, list=FALSE)
  trainingnNet <- nNet_data[ inTrainingNnet,]
  testingnNet <- nNet_data[-inTrainingNnet,]
#seed_5<- .Random.seed
  inTrainingRF <- createDataPartition(reg_data$Recall_Heard, p=0.7, list = FALSE)
  trainingRF <- reg_data[inTrainingRF,]
  testingRF <- reg_data[-inTrainingRF,]
  trainingnNet$Recall_Heard <- as.factor(trainingnNet$Recall_Heard)
  testingnNet$Recall_Heard <- as.factor(testingnNet$Recall_Heard)
  trainingRF$Recall_Heard <- as.factor(trainingRF$Recall_Heard)
  testingRF$Recall_Heard <- as.factor(testingRF$Recall_Heard)
  nNet_data$Recall_Heard <- as.factor(nNet_data$Recall_Heard)

#setup the 10 fold cross validation control
#set.seed(1005)
#seed6 <- .Random.seed
  crossFold  <- trainControl(method = "cv", number = 10 , savePredictions =  "final",classProbs = TRUE, summaryFunction = twoClassSummary )

#crossFold$sampling = "up"
  levels(trainingnNet$Recall_Heard) <- c("no", "yes")
  levels(testingnNet$Recall_Heard) <- c("no", "yes")
  levels(nNet_data$Recall_Heard) <- c("no", "yes")
#model_weights <- ifelse(nNet_data$Recall_Heard == "yes", ((1/table(nNet_data$Recall_Heard)[2]) * 0.5),
                       # ((1/table(nNet_data$Recall_Heard)[1]) * 0.5))

  models <- caretList(Recall_Heard~., data=trainingnNet, trControl = crossFold ,   methodList = c("cforest" , "nnet" ,"multinom"), metric = "ROC" )
#seed7<- .Random.seed
  preds <- as.data.frame(predict(models, newdata = testingnNet))
  modellists[[count]]  <- models
  pred <- predict(models, newdata = testingnNet)
  model_ensemble <- caretEnsemble(models, metric = "ROC", trControl = crossFold)
  model_ensembles[[count]] <- model_ensemble
  model_preds <- lapply(models, predict, newdata=testingnNet )
#type="prob"
  model_preds <- data.frame(model_preds)
  ens_preds <- predict(model_ensemble, newdata=testingnNet )
#type="prob"
  model_preds$ensemble <- ens_preds
  cforest[[count]] <- caret::confusionMatrix(model_preds$cforest , testingnNet$Recall_Heard)
  network[[count]] <- caret::confusionMatrix(model_preds$nnet , testingnNet$Recall_Heard)
  logregression[[count]] <- caret::confusionMatrix(model_preds$multinom , testingnNet$Recall_Heard)
  ensemble[[count]] <- caret::confusionMatrix(model_preds$ensemble , testingnNet$Recall_Heard)
  importances[[count]] <- varImp(model_ensemble)
  Names_list[[count]] <- rownames(as.matrix(importances[[count]]))
  #if( ensemble[[count]]$overall["AccuracyPValue"]<=0.05){
   # seeds<- append(seeds, seed_value,count )
   #count <- count+1
  #}
  azs[[count]] <- pdp::partial(models$cforest, train=trainingnNet, pred.var = "Risk_Perception_Recall_2", type= "classification", prob=TRUE )
  bzs[[count]] <- pdp::partial(models$cforest, train=trainingnNet, pred.var = "Consumer_Focus_Pack_4" ,type= "classification", prob=TRUE)
  czs[[count]] <- pdp::partial(models$cforest, train=trainingnNet, pred.var = "Age",type= "classification", prob=TRUE)
  dzs[[count]] <- pdp::partial(models$cforest, train=trainingnNet, pred.var = "Risk_Perception_Consumption",type= "classification", prob=TRUE)
  azs[[count]]$yhat <-  -azs[[count]]$yhat+1
  bzs[[count]]$yhat <-  -bzs[[count]]$yhat+1
  czs[[count]]$yhat <-  -czs[[count]]$yhat+1
  dzs[[count]]$yhat <-  -dzs[[count]]$yhat+1
#}
  # summary(model_ensemble)
  
   #create Partials for later analysis, previous investigation reveals that cforests typically play a larger importance in the overall ensemble

  # 
  #this is an alternative version for getting plots, granted this is based off the models
  # #After analyzing the variable importances, harvesting these to create multivariate plots
  # featIntPDP_1[[count]] <- pdp::partial(models$cforest, train=trainingnNet, pred.var = c("Age" , "Consumer_Focus_Pack_4"), type= "classification", prob=TRUE)
  # pdp::plotPartial(featIntPDP_1 ,, ylab =list("Pays attention to Lot Number", fontsize = 9))
  # 
 Importance_overall[[count]] <- as.matrix(importances[[count]][1])
  Importance_cforest[[count]] <- as.matrix(importances[[count]][2])
  Importance_nNet[[count]] <- as.matrix(importances[[count]][3])
  Importance_logReg[[count]] <- as.matrix(importances[[count]][4])
  
  Importance_overall[[count]] <- as.table(Importance_overall[[count]])
  Importance_cforest[[count]] <- as.table(Importance_cforest[[count]])
  Importance_nNet[[count]] <- as.table(Importance_nNet[[count]])
  Importance_logReg[[count]] <- as.table(Importance_logReg[[count]])
  
  Importance_overall[[count]] <- as.data.frame(Importance_overall[[count]])
  Importance_cforest[[count]] <- as.data.frame(Importance_cforest[[count]])
  Importance_nNet[[count]] <- as.data.frame(Importance_nNet[[count]])
  Importance_logReg[[count]]<- as.data.frame(Importance_logReg[[count]])
  
  Importance_overall[[count]] <- Importance_overall[[count]][order(Importance_overall[[count]]$Freq),]
  Importance_cforest[[count]] <- Importance_cforest[[count]][order(Importance_cforest[[count]]$Freq),]
  Importance_nNet[[count]] <- Importance_nNet[[count]][order(Importance_nNet[[count]]$Freq),]
  Importance_logReg[[count]] <- Importance_logReg[[count]][order(Importance_logReg[[count]]$Freq),]
  
  Names_list_overall[[count]] <- as.character(Importance_overall[[count]][(nrow((Importance_overall[[count]]))-13):nrow(Importance_overall[[count]]),1]) 
  Names_list_cforest[[count]] <- as.character(Importance_cforest[[count]][(nrow((Importance_cforest[[count]]))-13):nrow(Importance_cforest[[count]]),1])
  Names_list_nNet[[count]] <- as.character(Importance_nNet[[count]][(nrow((Importance_nNet[[count]]))-13):nrow(Importance_nNet[[count]]),1])
  Names_list_logReg[[count]]<- as.character(Importance_logReg[[count]][(nrow((Importance_logReg[[count]]))-13):nrow(Importance_logReg[[count]]),1])
  model_preds_2 <- lapply(models, predict, newdata=testingnNet , type="prob" )
  model_preds_2 <- data.frame(model_preds_2)
  ens_preds_2 <- predict(model_ensemble, newdata=testingnNet ,type="prob")
  model_preds_2$ensemble <- ens_preds_2
  aucs[[count]] <- caTools::colAUC(model_preds_2, testingnNet$Recall_Heard)
  
  
   count <- count +1 
}

count <- 1
count2 <- 1
count3 <-1
UniqueNameListOverall <- list()
UniqueNameListCforest <- list()
UniqueNameListlogReg <- list()
UniqueNameListnNet <- list()
while(count2 <= length(Names_list_overall)){
  count <- 1
  while(count <= length( Names_list_overall[[count2]])){
    UniqueNameListOverall[[count3]]<- Names_list_overall[[count2]][count]
    UniqueNameListCforest[[count3]]<- Names_list_cforest[[count2]][count]
    UniqueNameListnNet[[count3]]  <- Names_list_nNet[[count2]][count]
    UniqueNameListlogReg[[count3]]  <- Names_list_logReg[[count2]][count]
    count<-count +1
    count3 <-count3+1
  }
  count2 <- count2+1
}
UniqueNameListOverall <- unique(UniqueNameListOverall)
UniqueNameListCforest <- unique(UniqueNameListCforest)
UniqueNameListnNet <- unique(UniqueNameListnNet)
UniqueNameListlogReg <- unique(UniqueNameListlogReg)


count <- 1
while(count<6){
  Importance_overall[[count]] <- t(Importance_overall[[count]])
  Importance_cforest[[count]] <-t(Importance_cforest[[count]])
  Importance_nNet[[count]] <- t(Importance_nNet[[count]])
  Importance_logReg[[count]] <- t(Importance_logReg[[count]])
  colnames(Importance_overall[[count]]) <-  Importance_overall[[count]][1,]
  colnames(Importance_cforest[[count]]) <- Importance_cforest[[count]][1,]
  colnames(Importance_nNet[[count]]) <-  Importance_nNet[[count]][1,]
  colnames(Importance_logReg[[count]]) <-  Importance_logReg[[count]][1,]
  Importance_overall[[count]] <-  Importance_overall[[count]][-1,]
  Importance_cforest[[count]] <- Importance_cforest[[count]][-1,]
  Importance_nNet[[count]] <-  Importance_nNet[[count]][-1,]
  Importance_logReg[[count]] <- Importance_logReg[[count]][-1,]
  count <- count+1
}
count <- 1
OverallImportMatrix <- matrix(,nrow = length(UniqueNameListOverall), ncol= length(Names_list_overall))
CforestImportMatrix <- matrix(,nrow = length(UniqueNameListCforest), ncol= length(Names_list_cforest))
nNetImportMatrix <- matrix(,nrow = length(UniqueNameListnNet), ncol= length(Names_list_nNet))
logRegImportMatrix  <- matrix(,nrow = length(UniqueNameListlogReg), ncol= length(Names_list_logReg))

while( count<= length(Names_list_overall)){
  OverallImportMatrix[,count] <- as.numeric(t(Importance_overall[[count]][2, as.character(drop(UniqueNameListOverall))]))
  CforestImportMatrix[,count] <- as.numeric(t(Importance_cforest[[count]][2, as.character(drop(UniqueNameListCforest))]))
  nNetImportMatrix[,count] <-  as.numeric(t(Importance_nNet[[count]][2, as.character(drop(UniqueNameListnNet))]))
  logRegImportMatrix[,count]<-  as.numeric(t(Importance_logReg[[count]][2, as.character(drop(UniqueNameListlogReg))]))
  count <- count +1
}
rownames(OverallImportMatrix) <- UniqueNameListOverall
rownames(CforestImportMatrix) <- UniqueNameListCforest
rownames(nNetImportMatrix) <- UniqueNameListnNet
rownames(logRegImportMatrix) <- UniqueNameListlogReg

Overall_Importance_Means <- rowMeans(OverallImportMatrix)
Cforest_Importance_means <- rowMeans(CforestImportMatrix)
nNet_Importance_Means <- rowMeans(nNetImportMatrix)
logReg_Importance_Means <- rowMeans(logRegImportMatrix)
Overall_Importance_Means<- Overall_Importance_Means[order(Overall_Importance_Means)]
Cforest_Importance_means<- Cforest_Importance_means[order(Cforest_Importance_means)]
nNet_Importance_Means<-nNet_Importance_Means[order(nNet_Importance_Means)]
logReg_Importance_Means <- logReg_Importance_Means[order(logReg_Importance_Means )]

Overall_Importance_Means[(length(Overall_Importance_Means)-12):length(Overall_Importance_Means)]
Cforest_Importance_means[(length(Cforest_Importance_means)-12):length(Cforest_Importance_means)]
nNet_Importance_Means[(length(nNet_Importance_Means)-12):length(nNet_Importance_Means)]
logReg_Importance_Means[(length(logReg_Importance_Means)-12):length(logReg_Importance_Means)]
diag(cov(t(OverallImportMatrix)))
diag(cov(t(CforestImportMatrix)))
diag(cov(t(nNetImportMatrix)))
diag(cov(t(logRegImportMatrix)))
model_preds_2 <- lapply(models, predict, newdata=testingnNet , type="prob" )
model_preds_2 <- data.frame(model_preds_2)
ens_preds_2 <- predict(model_ensemble, newdata=testingnNet ,type="prob")
model_preds_2$ensemble <- ens_preds_2
caTools::colAUC(model_preds_2, testingnNet$Recall_Heard)



cf_accs_p<- rbind(cforest[[1]]$overall["AccuracyPValue"] , cforest[[2]]$overall["AccuracyPValue"],cforest[[3]]$overall["AccuracyPValue"],cforest[[4]]$overall["AccuracyPValue"],cforest[[5]]$overall["AccuracyPValue"])
nnet_accs_p<- rbind(network[[1]]$overall["AccuracyPValue"] , network[[2]]$overall["AccuracyPValue"],network[[3]]$overall["AccuracyPValue"],network[[4]]$overall["AccuracyPValue"],network[[5]]$overall["AccuracyPValue"])
logReg_accs_p <- rbind(logregression[[1]]$overall["AccuracyPValue"] , logregression[[2]]$overall["AccuracyPValue"],logregression[[3]]$overall["AccuracyPValue"],logregression[[5]]$overall["AccuracyPValue"],logregression[[5]]$overall["AccuracyPValue"])
ensem_accs_p <- rbind(ensemble[[1]]$overall["AccuracyPValue"] , ensemble[[2]]$overall["AccuracyPValue"],ensemble[[3]]$overall["AccuracyPValue"],ensemble[[5]]$overall["AccuracyPValue"],ensemble[[5]]$overall["AccuracyPValue"])

cf_accs<- rbind(cforest[[1]]$overall["Accuracy"] , cforest[[2]]$overall["Accuracy"],cforest[[3]]$overall["Accuracy"],cforest[[5]]$overall["Accuracy"],cforest[[5]]$overall["Accuracy"])
nnet_accs<- rbind(network[[1]]$overall["Accuracy"] , network[[2]]$overall["Accuracy"],network[[3]]$overall["Accuracy"],network[[5]]$overall["Accuracy"],network[[5]]$overall["Accuracy"])
logReg_accs <- rbind(logregression[[1]]$overall["Accuracy"] , logregression[[2]]$overall["Accuracy"],logregression[[3]]$overall["Accuracy"],logregression[[5]]$overall["Accuracy"],logregression[[5]]$overall["Accuracy"])
ensem_accs <- rbind(ensemble[[1]]$overall["Accuracy"] , ensemble[[2]]$overall["Accuracy"],ensemble[[3]]$overall["Accuracy"],ensemble[[5]]$overall["Accuracy"],ensemble[[5]]$overall["Accuracy"])

cf_accs_up<- rbind(cforest[[1]]$overall["AccuracyUpper"] , cforest[[2]]$overall["AccuracyUpper"],cforest[[3]]$overall["AccuracyUpper"],cforest[[5]]$overall["AccuracyUpper"],cforest[[5]]$overall["AccuracyUpper"])
nnet_accs_up<- rbind(network[[1]]$overall["AccuracyUpper"] , network[[2]]$overall["AccuracyUpper"],network[[3]]$overall["AccuracyUpper"],network[[5]]$overall["AccuracyUpper"],network[[5]]$overall["AccuracyUpper"])
logReg_accs_up <- rbind(logregression[[1]]$overall["AccuracyUpper"] , logregression[[2]]$overall["AccuracyUpper"],logregression[[3]]$overall["AccuracyUpper"],logregression[[5]]$overall["AccuracyUpper"],logregression[[5]]$overall["AccuracyUpper"])
ensem_accs_up <- rbind(ensemble[[1]]$overall["AccuracyUpper"] , ensemble[[2]]$overall["AccuracyUpper"],ensemble[[3]]$overall["AccuracyUpper"],ensemble[[5]]$overall["AccuracyUpper"],ensemble[[5]]$overall["AccuracyUpper"])

cf_accs_bounds<- cf_accs_up - cf_accs
nnet_accs_bounds<- nnet_accs_up - nnet_accs
logReg_accs_bounds<- logReg_accs_up - logReg_accs
ensem_accs_bounds<- ensem_accs_up - ensem_accs

mean(cf_accs)
mean(nnet_accs)
mean(logReg_accs)
mean(ensem_accs)

mean(cf_accs_p)
mean(nnet_accs_p)
mean(logReg_accs_p)
mean(ensem_accs_p)

importances[[1]]
importances[[2]]
importances[[3]]
importances[[4]]
importances[[5]]



count <- 1
colnames(azs[[count]])[[2]] <- "p(Awareness of Recall)"
colnames(azs[[count]])[[1]] <- "Liklihood of recall on their flour"
colnames(bzs[[count]])[[2]] <- "p(Awareness of Recall)"
colnames(bzs[[count]])[[1]] <- "Pays attention to lot number"
colnames(czs[[count]])[[2]] <- "p(Awareness of Recall)"
colnames(dzs[[count]])[[2]] <- "p(Awareness of Recall)"
colnames(dzs[[count]])[[1]] <- "Risk from raw dough"
a.1<- ggplot(data  = azs[[count]] , aes(x =`Liklihood of recall on their flour` , y =`p(Awareness of Recall)` )) + geom_path() +theme(axis.text=element_text(size=12,family="Times New Roman",color="Black"), panel.background = element_rect(fill=rgb(1,0.647,0,0.2)), axis.title.x =element_text(size=14,family="Times New Roman") , axis.title.y = element_text(size=14,family="Times New Roman"))+scale_y_continuous(breaks= c(0.1,0.12,0.14,0.16,0.18,0.2,0.22,0.24,0.26,0.28,0.3,0.32,0.34,0.36,0.38,0.40), labels=waiver(), limits=c(0.1,0.40))
b.1<-ggplot(data  = bzs[[count]] , aes(x =`Pays attention to lot number` , y =`p(Awareness of Recall)` )) + geom_point() +theme(axis.text=element_text(size=12,family="Times New Roman",color="Black"),panel.background = element_rect(fill=rgb(1,0.647,0,0.2)), axis.title.x =element_text(size=14,family="Times New Roman") , axis.title.y = element_text(size=14,family="Times New Roman"))+scale_y_continuous(breaks= c(0.1,0.12,0.14,0.16,0.18,0.2,0.22,0.24,0.26,0.28,0.3,0.32,0.34,0.36,0.38,0.40), labels=waiver(), limits=c(0.1,0.40))
c.1<-ggplot(data  = czs[[count]] , aes(x =Age, y =`p(Awareness of Recall)`)) + geom_path( ) +theme(axis.text=element_text(size=12,family="Times New Roman",color="Black"),panel.background = element_rect(fill=rgb(1,0.647,0,0.2)), axis.title.x =element_text(size=14,family="Times New Roman"), axis.title.y = element_text(size=14,family="Times New Roman"))+scale_y_continuous(breaks= c(0.1,0.12,0.14,0.16,0.18,0.2,0.22,0.24,0.26,0.28,0.3,0.32,0.34,0.36,0.38,0.40), labels=waiver(), limits=c(0.1,0.40))+xlab("Age range")
#ggplot(data  = dzs[[count]] , aes(x =`Risk from raw dough` , y =`p(Awareness of Recall)`)) + geom_path( ) +theme( axis.title.x =element_text(size=14) , axis.title.y = element_text(size=14))+scale_y_continuous(breaks= c(0.1,0.12,0.14,0.16,0.18,0.2,0.22,0.24,0.26,0.28,0.3,0.32,0.34,0.36,0.38), labels=waiver(), limits=c(0.1,0.38))


count <- count+1
colnames(azs[[count]])[[2]] <- "p(Awareness of Recall)"
colnames(azs[[count]])[[1]] <- "Liklihood of recall on their flour"
colnames(bzs[[count]])[[2]] <- "p(Awareness of Recall)"
colnames(bzs[[count]])[[1]] <- "Pays attention to lot number"
colnames(czs[[count]])[[2]] <- "p(Awareness of Recall)"
colnames(dzs[[count]])[[2]] <- "p(Awareness of Recall)"
colnames(dzs[[count]])[[1]] <- "Risk from raw dough"

a.2<-ggplot(data  = azs[[count]] , aes(x =`Liklihood of recall on their flour` , y =`p(Awareness of Recall)` )) + geom_path() +theme(axis.text=element_text(size=12,family="Times New Roman",color="Black"),panel.background = element_rect(fill=rgb(0,1,0.498,0.2)), axis.title.x =element_text(size=14,family="Times New Roman") , axis.title.y = element_text(size=14,family="Times New Roman"))+scale_y_continuous(breaks= c(0.1,0.12,0.14,0.16,0.18,0.2,0.22,0.24,0.26,0.28,0.3,0.32,0.34,0.36,0.38,0.40), labels=waiver(), limits=c(0.1,0.40))
b.2<-ggplot(data  = bzs[[count]] , aes(x =`Pays attention to lot number` , y =`p(Awareness of Recall)` )) + geom_point() +theme(axis.text=element_text(size=12,family="Times New Roman",color="Black"),panel.background = element_rect(fill=rgb(0,1,0.498,0.2)), axis.title.x =element_text(size=14,family="Times New Roman") , axis.title.y = element_text(size=14,family="Times New Roman"))+scale_y_continuous(breaks= c(0.1,0.12,0.14,0.16,0.18,0.2,0.22,0.24,0.26,0.28,0.3,0.32,0.34,0.36,0.38,0.40), labels=waiver(), limits=c(0.1,0.40))
c.2<-ggplot(data  = czs[[count]] , aes(x =Age, y =`p(Awareness of Recall)`)) + geom_path( ) +theme(axis.text=element_text(size=12,family="Times New Roman",color="Black"),panel.background = element_rect(fill=rgb(0,1,0.498,0.2)), axis.title.x =element_text(size=14,family="Times New Roman") , axis.title.y = element_text(size=14,family="Times New Roman"))+scale_y_continuous(breaks= c(0.1,0.12,0.14,0.16,0.18,0.2,0.22,0.24,0.26,0.28,0.3,0.32,0.34,0.36,0.38,0.40), labels=waiver(), limits=c(0.1,0.40))+xlab("Age range")
#ggplot(data  = dzs[[count]] , aes(x =`Risk from raw dough` , y =`p(Awareness of Recall)`)) + geom_path( ) +theme( axis.title.x =element_text(size=14) , axis.title.y = element_text(size=14))+scale_y_continuous(breaks= c(0.1,0.12,0.14,0.16,0.18,0.2,0.22,0.24,0.26,0.28,0.3,0.32,0.34,0.36,0.38), labels=waiver(), limits=c(0.1,0.38))


count <- count+1
colnames(azs[[count]])[[2]] <- "p(Awareness of Recall)"
colnames(azs[[count]])[[1]] <- "Liklihood of recall on their flour"
colnames(bzs[[count]])[[2]] <- "p(Awareness of Recall)"
colnames(bzs[[count]])[[1]] <- "Pays attention to lot number"
colnames(czs[[count]])[[2]] <- "p(Awareness of Recall)"
colnames(dzs[[count]])[[2]] <- "p(Awareness of Recall)"
colnames(dzs[[count]])[[1]] <- "Risk from raw dough"

a.3<-ggplot(data  = azs[[count]] , aes(x =`Liklihood of recall on their flour` , y =`p(Awareness of Recall)` )) + geom_path() +theme(axis.text=element_text(size=12,family="Times New Roman",color="Black"),panel.background = element_rect(fill= rgb(0,0.749,1,0.2)), axis.title.x =element_text(size=14,family="Times New Roman") , axis.title.y = element_text(size=14,family="Times New Roman"))+scale_y_continuous(breaks= c(0.1,0.12,0.14,0.16,0.18,0.2,0.22,0.24,0.26,0.28,0.3,0.32,0.34,0.36,0.38,0.40), labels=waiver(), limits=c(0.1,0.40))
b.3<-ggplot(data  = bzs[[count]] , aes(x =`Pays attention to lot number` , y =`p(Awareness of Recall)` )) + geom_point() +theme(axis.text=element_text(size=12,family="Times New Roman",color="Black"), panel.background = element_rect(fill= rgb(0,0.749,1,0.2)), axis.title.x =element_text(size=14, family="Times New Roman") , axis.title.y = element_text(size=14, family="Times New Roman"))+scale_y_continuous(breaks= c(0.1,0.12,0.14,0.16,0.18,0.2,0.22,0.24,0.26,0.28,0.3,0.32,0.34,0.36,0.38,0.40), labels=waiver(), limits=c(0.1,0.40)) #,axis.text=element_text(size=12,family="Times New Roman"),color="Black")
c.3<-ggplot(data  = czs[[count]] , aes(x =Age, y =`p(Awareness of Recall)`)) + geom_path( ) +theme(axis.text=element_text(size=12,family="Times New Roman",color="Black"),panel.background = element_rect(fill= rgb(0,0.749,1,0.2)), axis.title.x =element_text(size=14,family="Times New Roman") , axis.title.y = element_text(size=14,family="Times New Roman"))+scale_y_continuous(breaks= c(0.1,0.12,0.14,0.16,0.18,0.2,0.22,0.24,0.26,0.28,0.3,0.32,0.34,0.36,0.38,0.40), labels=waiver(), limits=c(0.1,0.40))+xlab("Age range")
#ggplot(data  = dzs[[count]] , aes(x =`Risk from raw dough` , y =`p(Awareness of Recall)`)) + geom_path( ) +theme( axis.title.x =element_text(size=14) , axis.title.y = element_text(size=14))+scale_y_continuous(breaks= c(0.1,0.12,0.14,0.16,0.18,0.2,0.22,0.24,0.26,0.28,0.3,0.32,0.34,0.36,0.38), labels=waiver(), limits=c(0.1,0.38))


count <- count+1
colnames(azs[[count]])[[2]] <- "p(Awareness of Recall)"
colnames(azs[[count]])[[1]] <- "Liklihood of recall on their flour"
colnames(bzs[[count]])[[2]] <- "p(Awareness of Recall)"
colnames(bzs[[count]])[[1]] <- "Pays attention to lot number"
colnames(czs[[count]])[[2]] <- "p(Awareness of Recall)"
colnames(dzs[[count]])[[2]] <- "p(Awareness of Recall)"
colnames(dzs[[count]])[[1]] <- "Risk from raw dough"

a.4<-ggplot(data  = azs[[count]] , aes(x =`Liklihood of recall on their flour` , y =`p(Awareness of Recall)` )) + geom_path() +theme(axis.text=element_text(size=12,family="Times New Roman",color="Black"),panel.background = element_rect(fill= rgb(0,0,1,0.2)), axis.title.x =element_text(size=14, family="Times New Roman") , axis.title.y = element_text(size=14, family="Times New Roman"))+scale_y_continuous(breaks= c(0.1,0.12,0.14,0.16,0.18,0.2,0.22,0.24,0.26,0.28,0.3,0.32,0.34,0.36,0.38,0.40), labels=waiver(), limits=c(0.1,0.40))#, axis.text=element_text(size=28,family="Times New Roman",color="Black")
b.4<-ggplot(data  = bzs[[count]] , aes(x =`Pays attention to lot number` , y =`p(Awareness of Recall)` )) + geom_point() +theme(axis.text=element_text(size=12,family="Times New Roman",color="Black"),panel.background = element_rect(fill= rgb(0,0,1,0.2)), axis.title.x =element_text(size=14,family="Times New Roman") , axis.title.y = element_text(size=14,family="Times New Roman"))+scale_y_continuous(breaks= c(0.1,0.12,0.14,0.16,0.18,0.2,0.22,0.24,0.26,0.28,0.3,0.32,0.34,0.36,0.38,0.40), labels=waiver(), limits=c(0.1,0.40))
c.4<-ggplot(data  = czs[[count]] , aes(x =Age, y =`p(Awareness of Recall)`)) + geom_path()+theme(axis.text=element_text(size=12,family="Times New Roman",color="Black"),panel.background = element_rect(fill= rgb(0,0,1,0.2)), axis.title.x =element_text(size=14, family="Times New Roman") , axis.title.y = element_text(size=14, family="Times New Roman"))+scale_y_continuous(breaks= c(0.1,0.12,0.14,0.16,0.18,0.2,0.22,0.24,0.26,0.28,0.3,0.32,0.34,0.36,0.38,0.40), labels=waiver(), limits=c(0.1,0.40))+xlab("Age range")#,axis.text = element_text(size=14, family="Times New Roman",color="Black")
#ggplot(data  = dzs[[count]] , aes(x =`Risk from raw dough` , y =`p(Awareness of Recall)`)) + geom_path( ) +theme( axis.title.x =element_text(size=14) , axis.title.y = element_text(size=14))+scale_y_continuous(breaks= c(0.1,0.12,0.14,0.16,0.18,0.2,0.22,0.24,0.26,0.28,0.3,0.32,0.34,0.36,0.38), labels=waiver(), limits=c(0.1,0.38))


count <- count+1
colnames(azs[[count]])[[2]] <- "p(Awareness of Recall)"
colnames(azs[[count]])[[1]] <- "Liklihood of recall on their flour"
colnames(bzs[[count]])[[2]] <- "p(Awareness of Recall)"
colnames(bzs[[count]])[[1]] <- "Pays attention to lot number"
colnames(czs[[count]])[[2]] <- "p(Awareness of Recall)"
colnames(dzs[[count]])[[2]] <- "p(Awareness of Recall)"
colnames(dzs[[count]])[[1]] <- "Risk from raw dough"

a.5<-ggplot(data  = azs[[count]] , aes(x =`Liklihood of recall on their flour` , y =`p(Awareness of Recall)` )) + geom_path() +theme(axis.text=element_text(size=12,family="Times New Roman",color="Black"),panel.background = element_rect(fill= rgb(1,0.784,0.576,0.2)), axis.title.x =element_text(size=14,family="Times New Roman") , axis.title.y = element_text(size=14,family="Times New Roman"))+scale_y_continuous(breaks= c(0.1,0.12,0.14,0.16,0.18,0.2,0.22,0.24,0.26,0.28,0.3,0.32,0.34,0.36,0.38,0.40), labels=waiver(), limits=c(0.1,0.40))
b.5<-ggplot(data  = bzs[[count]] , aes(x =`Pays attention to lot number` , y =`p(Awareness of Recall)` )) + geom_point() +theme(axis.text=element_text(size=12,family="Times New Roman",color="Black"),panel.background = element_rect(fill= rgb(1,0.784,0.576,0.2)), axis.title.x =element_text(size=14,family="Times New Roman") , axis.title.y = element_text(size=14,family="Times New Roman"))+scale_y_continuous(breaks= c(0.1,0.12,0.14,0.16,0.18,0.2,0.22,0.24,0.26,0.28,0.3,0.32,0.34,0.36,0.38,0.40), labels=waiver(), limits=c(0.1,0.40))#+#(str_wrap())
c.5<-ggplot(data  = czs[[count]] , aes(x =Age, y =`p(Awareness of Recall)`)) + geom_path( ) +theme(axis.text=element_text(size=12,family="Times New Roman",color="Black"),panel.background = element_rect(fill= rgb(1,0.784,0.576,0.2)), axis.title.x =element_text(size=14,family="Times New Roman") , axis.title.y = element_text(size=14,family="Times New Roman"))+scale_y_continuous(breaks= c(0.1,0.12,0.14,0.16,0.18,0.2,0.22,0.24,0.26,0.28,0.3,0.32,0.34,0.36,0.38,0.40), labels=waiver(), limits=c(0.1,0.40))+xlab("Age range")
#ggplot(data  = dzs[[count]] , aes(x =`Risk from raw dough` , y =`p(Awareness of Recall)`)) + geom_path( ) +theme( axis.title.x =element_text(size=14) , axis.title.y = element_text(size=14))+scale_y_continuous(breaks= c(0.1,0.12,0.14,0.16,0.18,0.2,0.22,0.24,0.26,0.28,0.3,0.32,0.34,0.36,0.38), labels=waiver(), limits=c(0.1,0.38))

ggarrange(a.1+ggtitle("Iteration 1"), a.2+ggtitle("Iteration 2"), a.3+ggtitle("Iteration 3"), a.4+ggtitle("Iteration 4") ,a.5+ggtitle("Iteration 5")  , c.1, c.2, c.3, c.4, c.5, b.1 , b.2, b.3, b.4, b.5, ncol=5,nrow=3)

ggarrange(a.4, b.3, c.4,ncol=3,nrow=1,align="hv" )

devtools::session_info()
#Not Reported Variable Interaction Exploration
#################################################################################################################################################### 

# overallnames <- names(Overall_Importance_Means)[(length(Overall_Importance_Means)-4):length(Overall_Importance_Means)]
# rfNames <- names(Cforest_Importance_means)[(length(Cforest_Importance_means)-4):length(Cforest_Importance_means)]
# nNetnames <- names(nNet_Importance_Means)[(length(nNet_Importance_Means)-4):length(nNet_Importance_Means)]
# logRegNames <- names(logReg_Importance_Means)[(length(logReg_Importance_Means)-4):length(logReg_Importance_Means)]
# 
# 
# matchlist <- list()
# count <- 1
# #First loop isn't needed, will remove later
# for (varname in colnames(newData.1)){
#   matchlist<- str_detect(overallnames,"TRUE")
#   count2 <- 1
#   for (index in matchlist){
#     if(index){
#       overallnames[[count2]] <- substring(overallnames[[count2]],1,nchar(overallnames[[count2]])-4 )
#     }
#     count2 <- count2+1
#   }
# }
# 
# matchlist <- list()
# count <- 1
# for (varname in colnames(newData.1)){
#   matchlist<- str_detect(rfNames, "TRUE")
#   count2 <- 1
#   for (index in matchlist){
#     if(index){
#       rfNames[[count2]] <- substring(rfNames[[count2]],1,nchar(rfNames[[count2]])-4 )
#     }
#     count2 <- count2+1
#   }
# }
# 
# matchlist <- list()
# count <- 1
# for (varname in colnames(newData.1)){
#   matchlist<- str_detect(nNetnames, "TRUE")
#   count2 <- 1
#   for (index in matchlist){
#     if(index){
#       nNetnames[[count2]] <- substring(nNetnames[[count2]],1,nchar(nNetnames[[count2]])-4 )
#     }
#     count2 <- count2+1
#   }
# }
# 
# matchlist <- list()
# count <- 1
# for (varname in colnames(newData.1)){
#   matchlist<- str_detect(logRegNames, "TRUE")
#   count2 <- 1
#   for (index in matchlist){
#     if(index){
#       logRegNames[[count2]] <- substring(logRegNames[[count2]],1,nchar(logRegNames[[count2]])-4 )
#     }
#     count2 <- count2+1
#   }
# }
# 

# 
# 
# feature_int_ensemble <- list()
# feature_int_cforest <-list()
# feature_int_nnet <-list()
# feature_int_logReg <- list()
# 
# 
# 
# feature_int_ensemble[[1]]<-vint(modellists[[1]],as.character(overallnames), train = trainingnNet,match.type ="classification")
# feature_int_cforest[[1]]<-vint(model_ensembles[[1]]$models$cforest,as.character(rfNames), train = trainingnNet,match.type ="classification")
# feature_int_nnet[[1]]<-vint(model_ensembles[[1]]$models$nnet,as.character(nNetnames), train = trainingnNet,match.type ="classification")
# feature_int_logReg[[1]]<-vint(model_ensembles[[1]]$models$multinom,as.character(logRegNames), train = trainingnNet,match.type ="classification")
# 
# feature_int_ensemble[[2]]<-vint(modellists[[2]],as.character(overallnames), train = trainingnNet,match.type ="classification")
# feature_int_cforest[[2]]<-vint(model_ensembles[[2]]$models$cforest,as.character(rfNames), train = trainingnNet,match.type ="classification")
# feature_int_nnet[[2]]<-vint(model_ensembles[[2]]$models$nnet,as.character(nNetnames), train = trainingnNet,match.type ="classification")
# feature_int_logReg[[2]]<-vint(model_ensembles[[2]]$models$multinom,as.character(logRegNames), train = trainingnNet,match.type ="classification")
# feature_int_ensemble[[3]]<-vint(modellists[[3]],as.character(overallnames), train = trainingnNet,match.type ="classification")
# feature_int_cforest[[3]]<-vint(model_ensembles[[3]]$models$cforest,as.character(rfNames), train = trainingnNet,match.type ="classification")
# feature_int_nnet[[3]]<-vint(model_ensembles[[3]]$models$nnet,as.character(nNetnames), train = trainingnNet,match.type ="classification")
# feature_int_logReg[[3]]<-vint(model_ensembles[[3]]$models$multinom,as.character(logRegNames), train = trainingnNet,match.type ="classification")
# 
# feature_int_ensemble[[4]]<-vint(modellists[[4]],as.character(overallnames), train = trainingnNet,match.type ="classification")
# feature_int_cforest[[4]]<-vint(model_ensembles[[4]]$models$cforest,as.character(rfNames), train = trainingnNet,match.type ="classification")
# feature_int_nnet[[4]]<-vint(model_ensembles[[4]]$models$nnet,as.character(nNetnames), train = trainingnNet,match.type ="classification")
# feature_int_logReg[[4]]<-vint(model_ensembles[[4]]$models$multinom,as.character(logRegNames), train = trainingnNet,match.type ="classification")
# 
# feature_int_ensemble[[5]]<-vint(modellists[[5]],as.character(overallnames), train = trainingnNet,match.type ="classification")
# feature_int_cforest[[5]]<-vint(model_ensembles[[5]]$models$cforest,as.character(rfNames), train = trainingnNet,match.type ="classification")
# feature_int_nnet[[5]]<-vint(model_ensembles[[5]]$models$nnet,as.character(nNetnames), train = trainingnNet,match.type ="classification")
# feature_int_logReg[[5]]<-vint(model_ensembles[[5]]$models$multinom,as.character(logRegNames), train = trainingnNet,match.type ="classification")
# 
# 
# feature_int_ensemble
# 

#pdps
#az <- partial(models$cforest, train=trainingnNet, pred.var = "Age", type= "classification" , which.class = "no" , prob=TRUE)




# count <-1
# 
# while(count < 6){
# feature_int_ensemble[[count]]$Interaction<- feature_int_ensemble[[count]]$Interaction[order(feature_int_ensemble[[count]]$Variables)]
# feature_int_ensemble[[count]]$Variables<- feature_int_ensemble[[count]]$Variables[order(feature_int_ensemble[[count]]$Variables)]
# 
# feature_int_cforest[[count]]$Interaction<- feature_int_cforest[[count]]$Interaction[order(feature_int_cforest[[count]]$Variables)]
# feature_int_cforest[[count]]$Variables<- feature_int_cforest[[count]]$Variables[order(feature_int_cforest[[count]]$Variables)]
# 
# feature_int_nnet[[count]]$Interaction<- feature_int_nnet[[count]]$Interaction[order(feature_int_nnet[[count]]$Variables)]
# feature_int_nnet[[count]]$Variables<- feature_int_nnet[[count]]$Variables[order(feature_int_nnet[[count]]$Variables)]
# 
# feature_int_logReg[[count]]$Interaction<- feature_int_logReg[[count]]$Interaction[order(feature_int_logReg[[count]]$Variables)]
# feature_int_logReg[[count]]$Variables<- feature_int_logReg[[count]]$Variables[order(feature_int_logReg[[count]]$Variables)]
# 
# count <- count +1
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
# featInteractLogReg <- cbind(feature_int_logReg[[1]]$Interaction, feature_int_logReg[[2]]$Interaction, feature_int_logReg[[3]]$Interaction,feature_int_logReg[[4]]$Interaction,feature_int_logReg[[5]]$Interaction)
# rownames(featInteractLogReg) <- feature_int_logReg[[1]]$Variables
# rowMeans(featInteractLogReg[,c(2:4)])
# 
# FeatureInteractionMeans <- cbind(rowMeans(featInteractEnsembles),rowMeans(featInteractCforest),rowMeans(featInteractNnet),rowMeans(featInteractLogReg))
# FeatureInteractionMeans.names<- c("Overall", "Cforest" , "Nnet" , "LogReg")
# colnames(FeatureInteractionMeans) <- FeatureInteractionMeans.names
# 
# diag(cov(t(featInteractEnsembles)))
# diag(cov(t(featInteractCforest)))
# diag(cov(t(featInteractNnet)))
# diag(cov(t(featInteractLogReg)))
# 
# 
# vip(model_ensembles[[1]]$models$cforest, method ="firm" , target = "Recall_Heard" , metric = "ROC")
# vip(model_ensembles[[1]]$models$nnet, method ="firm" , target = "Recall_Heard" , metric = "ROC")
# vip(model_ensembles[[1]]$models$multinom, method ="firm" , target = "Recall_Heard" , metric = "ROC")
# vip(model_ensembles[[2]]$models$cforest, method ="firm" , target = "Recall_Heard" , metric = "ROC")
# vip(model_ensembles[[2]]$models$nnet, method ="firm" , target = "Recall_Heard" , metric = "ROC")
# vip(model_ensembles[[2]]$models$multinom, method ="firm" , target = "Recall_Heard" , metric = "ROC")
# vip(model_ensembles[[3]]$models$cforest, method ="firm" , target = "Recall_Heard" , metric = "ROC")
# vip(model_ensembles[[3]]$models$nnet, method ="firm" , target = "Recall_Heard" , metric = "ROC")
# vip(model_ensembles[[3]]$models$multinom, method ="firm" , target = "Recall_Heard" , metric = "ROC")
# vip(model_ensembles[[4]]$models$cforest, method ="firm" , target = "Recall_Heard" , metric = "ROC")
# vip(model_ensembles[[4]]$models$nnet, method ="firm" , target = "Recall_Heard" , metric = "ROC")
# vip(model_ensembles[[4]]$models$multinom, method ="firm" , target = "Recall_Heard" , metric = "ROC")
# vip(model_ensembles[[5]]$models$cforest, method ="firm" , target = "Recall_Heard" , metric = "ROC")
# vip(model_ensembles[[5]]$models$nnet, method ="firm" , target = "Recall_Heard" , metric = "ROC")
# vip(model_ensembles[[5]]$models$multinom, method ="firm" , target = "Recall_Heard" , metric = "ROC")
# 
# 
# 
# 
# al(az)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #interaction.plot(x.factor= reg_data$Age , trace.factor = reg_data$Consumer_Focus_Pack_4, response = reg_data$Recall_Heard, ylab = "p(aware of recall)" , xlab= "Age", trace.label =  "Pays attention to Lot NUmber")
# interaction.plot(x.factor= reg_data$Age , trace.factor = reg_data$Consumer_Focus_Pack_4, response = reg_data$Recall_Heard, ylab = "p(aware of recall)" , xlab= "Age", legend = FALSE)
# interaction.plot(x.factor=reg_data$Risk_Perception_Consumption, trace.factor = reg_data$Consumer_Focus_Pack_4, response = reg_data$Recall_Heard, xlab = "Risk Eating Raw Dough" , ylab = "p(aware of recall)" , legend = FALSE)
# interaction.plot(x.factor= reg_data$Risk_Perception_Recall_2 , trace.factor = reg_data$Consumer_Focus_Pack_4, response = reg_data$Recall_Heard, xlab = "Liklihood of their flour in a recall" , ylab = "p(aware of recall)", legend = FALSE)
# 
# 
# az <- partial(models$cforest, train=trainingnNet, pred.var = c ("Age", "Consumer_Focus_Pack_4"), type= "classification" , which.class = "no" , prob=TRUE)
# plotPartial(az)



# 
# #inner loop
# matchlist <- list()
# count <- 1
# for (varname in colnames(newData.1)){
#   matchlist<- str_detect(rfNames, varname)
#   count2 <- 1
#  for (index in matchlist){
#    if(index){
#      rfNames[[count2]] <- substring(rfNames[[count2]],1,nchar(rfNames[[count2]])-1 )
#    }
#    count2 <- count2+1
#   }
# }
# 
# 
# 
# 
# 
# 
#theres potential interaction at 0 cases, but strong evidence supports that both young and Yes for
#variables suggest they are aware of recalls.






# Log Regression (2:4)
# State_51*Responsible_Recall_Risk_10 
#  1.3626762511 

#Cforest
#  Consumer_Focus_Pack_4*Age 
# 0.032430986 
# Consumer_Focus_Pack_4*Risk_Perception_Recall_2 
# 0.029668165 

#Nnet (1 and 5) 
# Consumer_Focus_Pack_4*Age       
# 0.13935530


#build the models
# crossFold  <- trainControl(method = "cv", number = 10 , savePredictions =  "final",classProbs = TRUE,summaryFunction = twoClassSummary )
# 
# #crossFold$sampling = "up"
# levels(trainingnNet$Recall_Heard) <- c("no", "yes")
# levels(testingnNet$Recall_Heard) <- c("no", "yes")
# levels(nNet_data$Recall_Heard) <- c("no", "yes")
# #model_weights <- ifelse(nNet_data$Recall_Heard == "yes", ((1/table(nNet_data$Recall_Heard)[2]) * 0.5),
# # ((1/table(nNet_data$Recall_Heard)[1]) * 0.5))
# models <- caretList(Recall_Heard~., data=nNet_data, trControl = crossFold ,   methodList = c("cforest" , "nnet" ,"multinom"), metric = "ROC" )
# preds <- as.data.frame(predict(models, newdata = nNet_data))
# pred <- predict(models, newdata = nNet_data)
# model_ensemble <- caretEnsemble(models, metric = "ROC", trControl = crossFold)
# 
# 
# model_preds <- lapply(models, predict, newdata=nNet_data )
# #type="prob"
# model_preds <- data.frame(model_preds)
# ens_preds <- predict(model_ensemble, newdata=nNet_data )
# #type="prob"
# model_preds$ensemble <- ens_preds
# confusionMatrix(model_preds$cforest , nNet_data$Recall_Heard)
# confusionMatrix(model_preds$nnet , nNet_data$Recall_Heard)
# confusionMatrix(model_preds$multinom , nNet_data$Recall_Heard)
# confusionMatrix(model_preds$ensemble , nNet_data$Recall_Heard)
# 
# model_preds_2 <- lapply(models, predict, newdata=nNet_data , type="prob" )
# model_preds_2 <- data.frame(model_preds_2)
# ens_preds_2 <- predict(model_ensemble, newdata=nNet_data ,type="prob")
# model_preds_2$ensemble <- ens_preds_2
# caTools::colAUC(model_preds_2, nNet_data$Recall_Heard)
# summary(model_ensemble)
# varImp(model_ensemble)

#first RF
#rfFit <- train(Recall_Heard~., data = trainingRF, method ="cforest",trControl = crossFold, metric = "Kappa" )

#then nNet as a multilayer perceptron
#mlpFit <-train(Recall_Heard~., data = trainingnNet , method = "nnet", trControl = crossFold, metric = "Kappa")

#lastly log regression

#logFit <-train(Recall_Heard~., data = trainingnNet , method = "multinom", trControl = crossFold, metric = "Kappa")
# 
# 
# #evaluate the models
# nNet_pred_Vals<-predict(mlpFit, newdata= testingnNet)
# nNet_error <- testingnNet$Recall_Heard - nNet_pred_Vals
# meanNnet_error <- mean(nNet_error)
# meanNnet_error
# abs_nNet_error <- abs(nNet_error)
# meanAbs_nNet_error <- mean(abs_nNet_error)
# 
# Rf_pred_val <- predict(rfFit, newdata = testingRF)
# rf_error <- testingRF$Recall_Heard - Rf_pred_val
# abs_rf_error <- abs(rf_error)
# percent_rf_error <- abs_rf_error/testingRF$Recall_Heard
# mean_pear_rf_error<- mean(percent_rf_error)
# #due to a non-normazlized measure, percent error is used
# 
# #extract feature importance
# pred_rf <- Predictor$new(rfFit ,data = trainingRF , y=trainingRF$Recall_Heard, class = "train")
# #imp_rf <- FeatureImp$new(pred_rf , loss ="mae")
# plot(imp_rf)
# pred_nnet <- Predictor$new(mlpFit, data = trainingnNet, y=trainingnNet$Recall_Heard, class = "train")
# imp_nNet <- FeatureImp$new(pred_nnet, loss= "mae")
# plot(imp_nNet)
# 
# #partial dependence plots
# pdps_rf <- FeatureEffect$new(pred_rf, feature=imp_rf$results$feature[1] ) 
# pdps_rf$plot()
# i = 2
# 
# while(i<=length(imp_rf$results$feature)) {
#   pdps_rf$set.feature(imp_rf$results$feature[i])
#   pdps_rf$plot()
#   i <- i + 1
#   if(i == 6){
#     i = 1000
#   }
# }
# 
# pdps_nnet <- FeatureEffect$new(pred_nnepret, feature=imp_nNet$results$feature[1] ) 
# pdps_nnet$plot()
# i = 2
# while(i<=length(imp_rf$results$feature)) {
#   
#   pdps_nnet$set.feature(imp_nNet$results$feature[i])
#   pdps_nnet$plot()
#   print(i)
#   i <- i + 1
#   if(i == 6){
#     i = 1000
#   }
# }
# i = 2
# 
#https://topepo.github.io/caret/feature-selection-using-univariate-filters.html - cite this book
#https://www.tandfonline.com/doi/pdf/10.1207/s15327906mbr1503_4?casa_token=_1N_ti0TZq4AAAAA:B4AfQHmOPbdgd9AVsmD5gx1ZBZvo-buDIiD9UIw0wzQQRLP6Clmhqdz3OJU_tdVjeQKTCy86M1uKGME 