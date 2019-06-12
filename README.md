# R-project
Classification model, logistics, AUC etc.
## Top Interview
[2.Add-Two-Numbers](#2add-two-numbers)  
[5.Longest-Palindromic-Substring](#5longest-palindromic-substring)  
[146.LRU-Cache](#146lru-cache)  
[380.Insert-Delete-GetRandom-O(1)](#380insert-delete-getrandom-o1)  
[454.4Sum-II](#4544sum-ii)  
### 2.Add-Two-Numbers
basic plotting and delete unnecessary attributes
```R
rm(list = ls())
library(readxl)
library(ggplot2)
Data <- read_excel("Project_DS_Log.xlsx")
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
