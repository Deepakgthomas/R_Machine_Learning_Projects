### Training Model
install.packages("bigmemory")
install.packages("biganalytics")
install.packages("biglm")
install.packages("foreach")
install.packages("DBI")
install.packages("DMwR")
library(bigmemory)
library(bigmemory.sri)
library(biganalytics)
library(biglm)
library(foreach)
library(DBI)
library(ff)
library(lubridate)
library(caret)
library(DMwR)
# selecting 13000000 majority instances from a whole data which is about 10% of the whole training data set
training_majority <- subset(final_training, is_attributed==0)
training_majority <- training_majority[sample(nrow(training_majority), 13000000, replace=FALSE, prob=NULL),]
# selecting 10% of the minority instances from the whole training data set
training_minority <- minority[sample(nrow(minority), 45684, replace=FALSE, prob=NULL),]
# combing the majority and minority instances to form the whole data set
training <- rbind(training_majority, training_minority)
data <- training
data$is_attributed <- as.factor(data$is_attributed)
# deleting as_attribute
data[,7] <- NULL
#seperating click time into hour and date
data$hour <- hour(data$click_time)
data$date <- substr(data$click_time,9,10)
# dividing the hour into 4 features i.e. morning, afternoon, evening and night
data$morning = ifelse(data$hour %in% 6:11, 1, 0)
data$afternoon = ifelse(data$hour %in% 12:17, 1, 0)
data$evening = ifelse(data$hour %in% 18:23, 1, 0)
data$night = ifelse(data$hour %in% 0:5, 1, 0)
# creating unique features for each instances by combing different combinations of app,device, channel, os
data$feature1 = as.numeric(paste0(data$app, data$device))
data$feature2 = as.numeric(paste0(data$app, data$os))
data$feature3 = as.numeric(paste0(data$app, data$channel))
data$feature4 = as.numeric(paste0(data$device, data$os))
data$feature5 = as.numeric(paste0(data$device, data$channel))
data$feature6 = as.numeric(paste0(data$os, data$channel))
# deleting click_time as we already have hour and date
data$click_time <- NULL
data <- data[,c(1:5,7:18,6)]
data$date <- as.factor(data$date)
# dividing the dat set into train and test data
train_index <- createDataPartition(data$is_attributed, p=.7, list=FALSE, times =1)
train <- data[train_index,]
test <- data[-train_index,]

#downsampling
down_sample <- downSample(train,train$is_attributed)
model <- randomForest(is_attributed~., down_sample, mtry =4, ntree=426)
prediction <- predict(model, test)
cm <- confusionMatrix(prediction,test$is_attributed)

# performing different combinations of perc.over and perc.under to do smote sampling
smote_tuning = SMOTE(is_attributed~., train, perc.over = 200, perc.under = 150)
model <- randomForest(is_attributed~., smote_tuning, mtry =4, ntree=426)
prediction <- predict(model, test)
cm <- confusionMatrix(prediction,test$is_attributed)



