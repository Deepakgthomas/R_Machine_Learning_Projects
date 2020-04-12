install.packages("bigmemory")
install.packages("biganalytics")
install.packages("biglm")
install.packages("foreach")
install.packages("lubridate")
install.packages("randomForest")
library(bigmemory)
library(bigmemory.sri)
library(biganalytics)
library(biglm)
library(lubridate)
library(randomForest)
library(caret)
library(DMwR)
setwd(work_dir)
## selecting 5% of the minority instances and 5% of the majority instances from the whole training dataset
feature_minority <- minority[sample(nrow(minority), 22300,replace=FALSE, prob=NULL),]
## combining majority and minority instances to create a dataset
feature <- rbind(feature_majority,feature_minority)
## dividing the data into featuretrain and featuretest
train_index <- createDataPartition(feature_data$is_attributed, p=.7, list=FALSE, times =1)
featuretrain <- feature[train_index,]
featuretest <- feature[-train_index,]

## converting the class attribute to factor
featuretest$is_attributed <- factor(featuretest$is_attributed)
featuretrain$is_attributed <- factor(featuretrain$is_attributed)

#Sampling the train data using smote 
## trying different values for perc.over and perc.under
featuretrain = SMOTE(is_attributed ~ ., featuretrain, perc.over = 200, perc.under = 150,K=5)
featuretest$attributed_time <- as.factor(featuretest$attributed_time)
## creating new features hour and date from the click_time attribute
featuretrain$hour <- hour(featuretrain$click_time)
featuretrain$date <- substr(featuretrain$click_time,9,10)
featuretest$hour <- hour(featuretest$click_time)
featuretest$date <- substr(featuretest$click_time,9,10)
featuretest$date<- as.numeric(featuretest$date)
featuretrain$date<- as.numeric(featuretrain$date)
featuretrain <- x2

## deleting click_time attribute and as_attributed from train and test
x2[,6]<- NULL
x2[,6]<- NULL
x2<- x2[,c(1:5,7,6,8)]
x2<- x2[,c(1:6,8,7)]
x2$date<- as.numeric(x2$date)
featuretest[,6]<- NULL
featuretest[,6]<- NULL
featuretest<- featuretest[,c(1:5,7,6,8)]
featuretest<- featuretest[,c(1:6,8,7)]


rf <- randomForest(is_attributed~., x2)
prediction <- predict(rf, featuretest)
confusionMatrix(prediction,featuretest$is_attributed)
#downsampling
x3<- featuretrain
## deleting click_time  and as_attributed from train
x3[,6]<- NULL
x3[,6]<- NULL
x3<- x3[,c(1:5,7,6,8)]
x3<- x3[,c(1:6,8,7)]
# downsampling the training data
x3 <- downSample(x3, x3$is_attributed)
# deleting redundant class attribute
x3[,9]<-NULL
rf <- randomForest(is_attributed~., x3)
prediction <- predict(rf, featuretest)
confusionMatrix(prediction,featuretest$is_attributed)


# creating new features by dividing the hour into morning, afternoon, evening and night
 x3$morning = ifelse(x3$hour %in% 6:11, 1, 0)
 x3$afternoon = ifelse(x3$hour %in% 12:17, 1, 0)
 x3$evening = ifelse(x3$hour %in% 18:23, 1, 0)
 x3$night = ifelse(x3$hour %in% 0:5, 1, 0)
 View(x3)
 x3 <- x3[,c(1:7,9:12,8)]
 featuretest$morning = ifelse(featuretest$hour %in% 6:11, 1, 0)
 featuretest$afternoon = ifelse(featuretest$hour %in% 12:17, 1, 0)
 featuretest$evening = ifelse(featuretest$hour %in% 18:23, 1, 0)
 featuretest$night = ifelse(featuretest$hour %in% 0:5, 1, 0)
 View(featuretest)
 featuretest <- featuretest[,c(1:7,9:12,8)]
 rf <- randomForest(is_attributed~., smote_tuning)
 prediction <- predict(rf, featuretest)
 cm <- confusionMatrix(prediction,featuretest$is_attributed)
 cm$byClass['Balanced Accuracy']
 # creating new feature combining each numerical attribute in the data to create unique value for each instance
 x3$feature1 = as.numeric(paste0(x3$app, x3$device))
 x3$feature2 = as.numeric(paste0(x3$app, x3$os))
 x3$feature3 = as.numeric(paste0(x3$app, x3$channel))
 x3$feature4 = as.numeric(paste0(x3$device, x3$os))
 x3$feature5 = as.numeric(paste0(x3$device, x3$channel))
 x3$feature6 = as.numeric(paste0(x3$os, x3$channel))
 featuretest$feature1 = as.numeric(paste0(featuretest$app, featuretest$device))
 featuretest$feature2 = as.numeric(paste0(featuretest$app, featuretest$os))
 featuretest$feature3 = as.numeric(paste0(featuretest$app, featuretest$channel))
 featuretest$feature4 = as.numeric(paste0(featuretest$device, featuretest$os))
 featuretest$feature5 = as.numeric(paste0(featuretest$device, featuretest$channel))
 featuretest$feature6 = as.numeric(paste0(featuretest$os, featuretest$channel))
 #performing SMOTE with all the new features 
 x3_smote <- SMOTE(is_attributed ~ ., x3, perc.over = 200, perc.under =400)
 rf <- randomForest(is_attributed~., x3_smote)
 prediction_x3 <- predict(rf, featuretest)
 cm <- confusionMatrix(prediction_x3,featuretest$is_attributed)
 cm$byClass['Balanced Accuracy']
 
 