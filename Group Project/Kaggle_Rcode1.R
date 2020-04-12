##########################
## Kaggle Project Code ##  
#########################

rm(list = ls())
library(Boruta)
library(lubridate) ## for dates
library(caret) ## for training
library(randomForest) ## random forest
library(e1071) ## naive bayes and svm
library(foreach) ## parallel computing
library(doParallel) ## parallel computing
library(DMwR) ## library for SMOTE (supersampling)

setwd("/work")
source('Kaggle_Rcode1_Function.R')

## Total number of observations in train 184903890
## Total number of observations in test 18790469

#registerDoParallel(cores = 5)
test = read.csv(file = 'test.csv', header = T, sep = ',')
system.time(results <- predictions(test))
