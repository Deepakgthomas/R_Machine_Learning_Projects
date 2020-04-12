##################################################
## This function does the random forest tune-up ##
##################################################

rf.tune = function(data.in, parameters, data.test){
  
  ## Here we find the number of iterations
  n = dim(parameters)[1]
  
  ## Here we start looping through all the parameter combinations
  rf.stats <- foreach(i = 1:n, .combine = rbind) %dopar% {	
    
    param = parameters[i,]
    rf.md = randomForest(is_attributed ~ ., ntree = param[[1]], mtry = param[[2]], data = data.in)	
    result = predict(rf.md, data.test)
    result = confusionMatrix(result, data.test$is_attributed)[[3]]
    result = result[[1]]
    data.frame(ntree = param[1], mtry = param[2], Accuracy = result) 
    
  }
  results = rf.stats[which.max(rf.stats$Accuracy),]
  return(results)
}

###########################################################
## This function creates the features and fit two models ##
########## Naive Bayes, and Random Forest #################
###########################################################

train_test = function(train, test){
  
  ## here we conduct super-sampling of the minority class (is_attributed = 1)
  train$is_attributed = factor(train$is_attributed)
  train$ip = factor(as.character(train$ip))
  train$app = factor(as.character(train$app))
  train$device = factor(as.character(train$device))
  train$os = factor(as.character(train$os))
  train$channel = factor(as.character(train$channel))
  train = SMOTE(is_attributed ~ ., train, perc.over = 1000, perc.under = 100)
  train$ip = as.numeric(as.character(train$ip))
  train$app = as.numeric(as.character(train$app))
  train$device = as.numeric(as.character(train$device))
  train$os = as.numeric(as.character(train$os))
  train$channel = as.numeric(as.character(train$channel))
  
  ## here we create hour in training
  train$Hour = hour(train$click_time)
  
  ## here we create another feature: morning, afternoon or night for training
  train$morning = ifelse(train$Hour %in% 6:11, 1, 0)
  train$afternoon = ifelse(train$Hour %in% 12:17, 1, 0)
  train$evening = ifelse(train$Hour %in% 18:23, 1, 0)
  train$night = ifelse(train$Hour %in% 0:5, 1, 0)
  
  ## here we create other features combining app, device, os, channel in training
  train$feature1 = as.numeric(paste0(train$app, train$device))
  train$feature2 = as.numeric(paste0(train$app, train$os))
  train$feature3 = as.numeric(paste0(train$app, train$channel))
  train$feature4 = as.numeric(paste0(train$device, train$os))
  train$feature5 = as.numeric(paste0(train$device, train$channel))
  train$feature6 = as.numeric(paste0(train$os, train$channel))
  
  ## here we do feature selection
  boruta.train = Boruta(is_attributed ~ ip + app + device + os + channel + Hour + morning + afternoon + evening + night + feature1 + feature2 + feature3 + feature4 + feature5 + feature6, data = train)
  boruta_signif = names(boruta.train$finalDecision[boruta.train$finalDecision %in% c("Confirmed", "Tentative")])
  boruta_signif = c(boruta_signif, 'is_attributed')
  
  ## here we split train data into training and testing 
  n = dim(train)[1]; to.sample = round(0.7*n)
  intrain = sample(1:n, to.sample, replace = F)  
  training = train[intrain,]
  testing = train[-intrain,]
  
  ## here we only select significant variables based on Boruta algorithm
  training = training[,names(training) %in% boruta_signif]
  testing = testing[,names(training) %in% boruta_signif]
  
  # Here we fit the Naive Bayes model to training
  md.naive.Bayes = naiveBayes(is_attributed ~ ., data = training) 
  pred.naive.Bayes = predict(md.naive.Bayes, testing)
  result.naive.Bayes = confusionMatrix(pred.naive.Bayes, testing$is_attributed)[[3]]
  result.naive.Bayes = result.naive.Bayes[[1]]
  
  ######################################################
  ## Here we fit the Random Forest modelt to training ##
  ######################################################
  
  ## Here we tune up the parameters
  m = dim(training)[2] - 3
  # tune.grid = expand.grid(ntree = seq(500, 2000, by = 500), mtry = 1:m)
  tune.grid = expand.grid(ntree = 500, mtry = 1:m)
  tune.par = rf.tune(training, tune.grid, testing)
  
  ## Here we fit the random forest used tunep-up parameters
  md.rf = randomForest(is_attributed ~ ., ntree = tune.par[[1]], mtry = tune.par[[2]], data = training)
  pred.rf = predict(md.rf, testing)
  result.rf = confusionMatrix(pred.rf, testing$is_attributed)[[3]]
  result.rf = result.rf[[1]]
  
  ############################################################
  ## Here we select the best model based on better accuracy ##
  ############################################################
  
  accuracy.results = c(result.naive.Bayes, result.rf)
  # print(accuracy.results)
  to.select = which.max(accuracy.results)
  
  if(to.select == 1){
    
    out = predict(md.naive.Bayes, test)
    
  } else {
    
    out = predict(md.rf, test)
    
  }
  # out = predict(md.rf, test)
  return(out)
}


####################################################
## This function loops trough the data in chuncks ##
####################################################

predictions = function(test){
  
  ## Here we create hour in testing
  test$Hour = hour(test$click_time)
  
  ## here we create another feature: morning, afternoon or night for testing
  test$morning = ifelse(test$Hour %in% 6:11, 1, 0)
  test$afternoon = ifelse(test$Hour %in% 12:17, 1, 0)
  test$evening = ifelse(test$Hour %in% 18:23, 1, 0)
  test$night = ifelse(test$Hour %in% 0:5, 1, 0)	
  
  ## here we create other features combining app, device, os, channel in testing
  test$feature1 = as.numeric(paste0(test$app, test$device))
  test$feature2 = as.numeric(paste0(test$app, test$os))
  test$feature3 = as.numeric(paste0(test$app, test$channel))
  test$feature4 = as.numeric(paste0(test$device, test$os))
  test$feature5 = as.numeric(paste0(test$device, test$channel))
  test$feature6 = as.numeric(paste0(test$os, test$channel))
   
  
  ## Create a loop that will proceed to next 1000000 when it is done.
  ## create a index that is going to hold where we are at. 
  
  trainfile <- 'train.csv'
  
  index <- 0
  chunksize <- 1000000
  con <- file(description = trainfile, open = 'r')
  train <- read.table(con, nrows=chunksize, header=T, fill=TRUE, sep=",")
  actualcolumnnames <- names(train)
  
  
  repeat {
    
    index <- index + 1
    # print(paste('Processing rows:' , index * chunksize))
    
    ## Here we store the predictions at each iterations 
    # out.tot[,index] = train_test(train,test)
    out = train_test(train,test)
	print(paste0('This is chunk ', index))
    file.chunk = paste0('chunk', index, '.txt')
    write.table(out, file = file.chunk, row.names = F)
    
    
    if (nrow(train) != chunksize){
      print('Processed all files!')
      break
    }
    
    train <- read.table(con, nrows = chunksize, skip = 0, header = FALSE, fill = TRUE, sep = ",",
                        col.names = actualcolumnnames)
    if (index > 183)
      break
     
  }
  close(con)
  # return(out.tot)
}
