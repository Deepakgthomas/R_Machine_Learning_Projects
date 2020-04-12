## using 5% of the majority and minority instances to create a new dataset
tuning_minority <- minority[sample(nrow(minority), 22300,replace=FALSE, prob=NULL),]
tuning <- rbind(tuning_majority,tuning_minority)
train_index <- createDataPartition(tuning$is_attributed, p=.7, list=FALSE, times =1)
tuningtrain <- tuning[train_index,]
tuningtest <- tuning[-train_index,]
# Incorporating new features into the train and test dateset
tuningtrain$is_attributed <- as.factor(tuningtrain$is_attributed)
tuningtest$is_attributed <- as.factor(tuningtest$is_attributed)
tuningtrain$hour <- Hour(tuningtrain$click_time)
tuningtrain$date <- substr(tuningtrain$click_time,9,10)
tuningtest$hour <- hour(tuningtest$click_time)
tuningtest$date <- substr(tuningtest$click_time,9,10)
tuningtrain$morning = ifelse(tuningtrain$hour %in% 6:11, 1, 0)
tuningtrain$afternoon = ifelse(tuningtrain$hour %in% 12:17, 1, 0)
tuningtrain$evening = ifelse(tuningtrain$hour %in% 18:23, 1, 0)
tuningtrain$night = ifelse(tuningtrain$hour %in% 0:5, 1, 0)
tuningtest$morning = ifelse(tuningtest$hour %in% 6:11, 1, 0)
tuningtest$afternoon = ifelse(tuningtest$hour %in% 12:17, 1, 0)
tuningtest$evening = ifelse(tuningtest$hour %in% 18:23, 1, 0)
tuningtest$night = ifelse(tuningtest$hour %in% 0:5, 1, 0)
tuningtrain$feature1 = as.numeric(paste0(tuningtrain$app, tuningtrain$device))
tuningtrain$feature2 = as.numeric(paste0(tuningtrain$app, tuningtrain$os))
tuningtrain$feature3 = as.numeric(paste0(tuningtrain$app, tuningtrain$channel))
tuningtrain$feature4 = as.numeric(paste0(tuningtrain$device, tuningtrain$os))
tuningtrain$feature5 = as.numeric(paste0(tuningtrain$device, tuningtrain$channel))
tuningtrain$feature6 = as.numeric(paste0(tuningtrain$os, tuningtrain$channel))
tuningtest$feature1 = as.numeric(paste0(tuningtest$app, tuningtest$device))
tuningtest$feature2 = as.numeric(paste0(tuningtest$app, tuningtest$os))
tuningtest$feature3 = as.numeric(paste0(tuningtest$app, tuningtest$channel))
tuningtest$feature4 = as.numeric(paste0(tuningtest$device, tuningtest$os))
tuningtest$feature5 = as.numeric(paste0(tuningtest$device, tuningtest$channel))
tuningtest$feature6 = as.numeric(paste0(tuningtest$os, tuningtest$channel))
#deleting click_time and as_attributed from train and test date
tuningtrain$click_time <- NULL
tuningtest$click_time <- NULL
tuningtrain$as_attributed<- NULL
tuningtest$as_attributed <- NULL
## rearranging the columns in the train and test data
tuningtrain <- tuningtrain[,c(1:5,7:18,6)]
tuningtest <- tuningtest[,c(1:5,7:18,6)]
tuningtrain$date <- as.integer(tuningtrain$date)
tuningtest$date <- as.integer(tuningtest$date)
x2 <- tuningtrain
##tuninig the train data
smote_tuning = SMOTE(is_attributed ~ ., x2, perc.over = 200, perc.under =80)
#tuning the mtry value
RFtune <- tuneRF(smote_tuning[,1:17],smote_tuning[,18], stepFactor = 1)
BA = NULL
accuracy = NULL
# tuning tthe ntree value for balanced accuracy
for (i in seq(from=1, to=1501, by=25)){
  model <- randomForest(is_attributed~., smote_tuning, mtry =4, ntree=i)
  prediction <- predict(model, tuningtest)
  cm <- confusionMatrix(prediction, tuningtest$is_attributed)
  BA[i]= cm$byClass['Balanced Accuracy']
  accuracy[i] = cm$overall['Accuracy']
}

## using 180000 majority instances and 120000 minority we check the performance of our model with and without the features, without performing any sampling using the tuned parameters
# without features
data1$is_attributed <- as.factor(data1$is_attributed)
data1[,7] <- NULL
data1$hour <- hour(data1$click_time)
data1$date <- substr(data1$click_time,9,10)
data1$date <- as.factor(data1$date)
data1$click_time <- NULL
train_index <- createDataPartition(data1$is_attributed, p=.7, list=FALSE, times =1)
train <- data1[train_index,]
test <- data1[-train_index,]
model <- randomForest(is_attributed~., train, mtry =4, ntree=426)
prediction <- predict(model, test)
cm <- confusionMatrix(prediction,test$is_attributed)
#using features
data1$morning = ifelse(data1$hour %in% 6:11, 1, 0)
data1$afternoon = ifelse(data1$hour %in% 12:17, 1, 0)
data1$evening = ifelse(data1$hour %in% 18:23, 1, 0)
data1$night = ifelse(data1$hour %in% 0:5, 1, 0)
data1$feature1 = as.numeric(paste0(data1$app, data1$device))
data1$feature2 = as.numeric(paste0(data1$app, data1$os))
data1$feature3 = as.numeric(paste0(data1$app, data1$channel))
data1$feature4 = as.numeric(paste0(data1$device, data1$os))
data1$feature5 = as.numeric(paste0(data1$device, data1$channel))
data1$feature6 = as.numeric(paste0(data1$os, data1$channel))
data1$click_time <- NULL
data1 <- data1[,c(1:5,7:18,6)]
train_index <- createDataPartition(data1$is_attributed, p=.7, list=FALSE, times =1)
train <- data1[train_index,]
test <- data1[-train_index,]
model <- randomForest(is_attributed~., train, mtry =4, ntree=426)
prediction <- predict(model, test)
cm <- confusionMatrix(prediction,test$is_attributed)
