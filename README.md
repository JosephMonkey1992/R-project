# R-project
Classification model, logistics, ROC, AUC, SVM, K-fold validation.
## Top Interview
[2.Importing data](#2Importing data)  
[3.Logistics Regression](#3Logistics Regression)  
[4.Hold-out validation for Logistics](#4Hold-out validation for Logistics)  
[5.Classification Tree Model](#5Classification Tree Model)  
[6.Performance Visualization with ROC](#6Performance Visualization with ROC)
[7.Random Forest](#7Random Forest)
[8.Support Vector Machine (SVM)](#8Support Vector Machine (SVM))
### 2.Importing data
basic plotting and delete unnecessary attributes
```R
rm(list = ls())
getwd()
library(readxl)
library(ggplot2)
Data <- read_excel("WA_Fn-UseC_-HR-Employee-Attrition.xlsx")
head(Data)
summary(Data)  
summary(Data$Attrition)   
table(Data$Attrition) 
##plotting
ggplot(Data%>%filter(Attrition==1),aes(x=Gender))+geom_bar()
ggplot(Data%>%filter(Attrition==1),aes(x=JobInvolvement))+geom_bar()
ggplot(Data%>%filter(Attrition==1),aes(x=JobSatisfaction))+geom_bar()
##delete 9,10,22 attributes
Data.log <-  Data[,c(-9,-10,-22)]
colnames(Data.log) <- c("age","attrition","travel","dayrate","department","distance","education","edu_field","env_sta","gender","hour_rate","involvement","level","role","job_sta","mar_sta","month_inc","month_rate","no_companiesworked","overtime","percentsalhike","performance","relation_sta","standhours","stocklevel","years","training","worklifebalance","year_employeed","year_inrole","year_sinceprom","year_withcurmang")
```
### 3.5Logistics Regression
we have previously changed the attrition into binary.
```R
library(glmnet)
log.1<-glm(formula = attrition ~ age+travel+dayrate+department+distance+education+edu_field+env_sta+gender+hour_rate+involvement+level+role+job_sta+mar_sta+month_inc+month_rate+no_companiesworked+overtime+percentsalhike+performance+relation_sta+standhours+stocklevel+years+training+worklifebalance+year_employeed+year_inrole+year_sinceprom+year_withcurmang,data=Data.log,family= "binomial")
summary(log.1)
```
### 4.Hold-out validation for Logistics
Divide into train/test subset by 20% threshold->build train model and predict
```R
set.seed(1)   
index <- sample(nrow(Data.log), nrow(Data.log)*0.2)
test <- Data.log[index,]       
train <-Data.log[-index,]  
##build train set model.
log.training<-glm(formula=attrition ~ age+travel+distance+env_sta+gender+involvement+job_sta+mar_sta+no_companiesworked+overtime+relation_sta+years+training+worklifebalance+year_employeed+year_inrole+year_sinceprom+year_withcurmang,data=train,family= "binomial")
summary(log.training)
##use train set model to predit in test
test$logit_pred_prob<-predict(log.training,test,type="response")
test$logit_pred_class<-ifelse(test$logit_pred_prob>0.5,"1","0")
table(test$attrition==test$logit_pred_class)
table(test$logit_pred_class,test$attrition, dnn=c("predicted","actual"))
```
### 5.Classification Tree Model
import dataset->
```R
##import data set
library(rpart)
library(rpart.plot)
getwd()
setwd("C:/Users/Boyua/Desktop/data science")
data<-read_excel("WA_Fn-UseC_-HR-Employee-Attrition.xlsx",sheet = "WA_Fn-UseC_-HR-Employee-Attriti")
head(data)
##tree plot-from where we can see the significant attributes and nodes
tree_model<-rpart(Attrition~.,           # model formula
                data=data,                     # dataset
                method="class",                   # "class" indicates a classification tree model 
                control=rpart.control(cp=0,maxdepth=4))   # tree control parameters. 
rpart.plot(tree_model)   # tree plot
summary(tree_model)
##set train/test subsets by 30%
set.seed(1)   # set a random seed 
index <- sample(1470, 441) # random selection of indices. 
index

##build train and test set
test <-data[index,c("Attrition","OverTime","TotalWorkingYears","WorkLifeBalance","YearsWithCurrManager","JobRole","HourlyRate","MonthlyIncome","StockOptionLevel","DailyRate","YearsInCurrentRole")]       
# save 30% as a test dataset
training <-data[-index,c("Attrition","OverTime","TotalWorkingYears","WorkLifeBalance","YearsWithCurrManager","JobRole","HourlyRate","MonthlyIncome","StockOptionLevel","DailyRate","YearsInCurrentRole")]   ##save the rest as a training set
##K-fold validation
set.seed(1)   # set a random seed 
##build a train model with significant factors.
full_tree<-rpart(Attrition~OverTime+TotalWorkingYears+WorkLifeBalance+YearsWithCurrManager+JobRole+HourlyRate+MonthlyIncome+StockOptionLevel+DailyRate+YearsInCurrentRole,
                     data=training, 
                     method="class",
                     control=rpart.control(cp=0))

rpart.plot(full_tree) ##plot the full tree.
##have a glimpse of the xerror and setermine the best cp (with lowest xerror)
printcp(full_tree)   # xerror, xstd - cross validation results
plotcp(full_tree)
##Select the one with lowest xerror.
min_xerror<-full_tree$cptable[which.min(full_tree$cptable[,"xerror"]),]
min_xerror
##prunnned the tree
min_xerror_tree<-prune(full_tree, cp=min_xerror[1])
rpart.plot(min_xerror_tree)

##get prediction
bp_tree<-min_xerror_tree
test$ct_bp_pred_prob<-predict(bp_tree,test)[,2]
test$ct_bp_pred_class=ifelse(test$ct_bp_pred_prob>0.5,"Yes","No")
##true positive rate etc.
table(test$ct_bp_pred_class==test$Attrition)  # error rate
table(test$ct_bp_pred_class,test$Attrition, dnn=c("predicted","actual"))  # confusion table on test data
```
### 6.Performance Visualization with ROC
ROC for the two models (CT & Logistics)
```R
library(pROC)
##ROC for classification tree model
ct_roc<-roc(test$Attrition,test$ct_bp_pred_prob,auc=TRUE)
plot(ct_roc,print.auc=TRUE,col="blue")
##ROC for logistics model
logit_roc<-roc(test$attrition,test$logit_pred_prob,auc=TRUE)
plot(logit_roc,print.auc=TRUE,col="red")
```
### 7.Random Forest
```R
set.seed(1)
#install.packages("randomForest")
library(randomForest)
rf_model<-randomForest(default~income+balance+student,              # model formula
                       data=Default,ntree=500, cutoff=c(0.5,0.5))
head(rf_model$votes)       # indicates the % of trees that voted for each class
head(rf_model$predicted)   # the class favored by more trees (i.e. majority vote wins)

varImpPlot(rf_model)  # importance of variables 
head(rf_model$vote)
Default$rf_vote<-predict(rf_model,type="prob")[,2]
head(Default)

##hold-out validation vs. OOB errors
set.seed(1)   # set a random seed 
index <- sample(10000, 2000) # random selection of indices. 
index
test <- Default[index,c("default","student","balance","income")]       # save 20% as a test dataset
training <-Default[-index,c("default","student","balance","income")]   # save the rest as a training set

rf_training_model<-randomForest(default~income+balance+student,              # model formula
                       data=training, 
                       ntree=500,                     # dataset
                       cutoff=c(0.5,0.5), 
                       mtry=2,
                       importance=TRUE)
rf_training_model
##make prediction and see the results
test$rf_pred_prob<-predict(rf_training_model,test,type="prob")[,2]
test$rf_pred_class<-predict(rf_training_model,test,type="class")
glimpse(test)
table(test$default==test$rf_pred_class) 
```
### 8.Support Vector Machine (SVM)
how to use SVM, and the hyperparameter tunning for SVM.
```R
#install.packages("e1071")
library(e1071)
library(tidyverse)
library(ggplot2)
model_svm<-svm(formula= default ~ balance+income, # model formula 
               data=Default,                   # dataset
               kernel="linear", # this is the form of the decision boundary, linear in this case
               cost=0.01)        # there are paremeters that are used to tune the model 
model_svm ##The model may not converge, and it is not uncommon. Also, note that it is not an error. It is less desire, but it provides classification results. To improve performance, you may try different cost parameters, or you may even try other kernel functions, other than "linear". Other option is normalizing data. But we will move on with this result.
##visualize support vectors using `plot()` function.
plot(model_svm, Default,income~balance)
##Support vectors are the observations that determine the margin. 
head(model_svm$index)  #the support vectors of the model
##Conceptually, we interpret decision values as the distance between the observation and the decision boundary. The positive fitted value indicate one class, and negative value indicates the other class.
dv<-data.frame(model_svm$decision.values)
ggplot(dv,aes(x=No.Yes)) +
  geom_histogram(binwidth=1000, colour="black",fill="white")
head(model_svm$fitted)  # predicted (fitted) class

predicted_svm<-predict(model_svm,Default,decision.values = TRUE)
head(attr(predicted_svm, "decision.values"))
##prediction
Default$svm_pred_class <- predict(model_svm, Default) 
Default$svm_dv<-attr(predicted_svm, "decision.values")

##Hyperparameter tuning
##We can tune SVM models using `tune` function. Set a range of search values for the parameter. It builds an SVM model for each possible combination of parameter values and evaluate accuracy. It will return the parameter combination that yields the best accuracy. 
svm_tune <- tune(svm,                            # find a best set of parameters for the svm model           
                 default~student+balance+income,         
                 data = training,
                 kernel="linear", 
                 ranges = list(cost = 10^(-5:0))) # specifying the ranges of parameters  
                                                  # in the penalty function to be examined
                                                  # you may wish to increase the search space like 
                                                  

print(svm_tune)                              # best parameters for the model
best_svm_mod <- svm_tune$best.model
test$svm_pred_class <- predict(best_svm_mod, test) # save the predicted class by the svm model
test$svm_dv<-as.numeric(attr(predict(best_svm_mod, test, decision.values = TRUE),"decision.values"))
```
