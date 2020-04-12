credit<-read.csv("E:\\credit-g.csv")
library(RWeka)
library(caret)
library(c50)
library(e1071)
summary(credit)
set.seed(123)#setting seed to reproduce results
#indepedent data set
t1<-createDataPartition(credit$class,p=.67,list=FALSE,times=1)
t1_train<-credit[t1,]
t1_test<-credit[-t1,]
#decision tree
DT<-J48(class~.,data=t1_train)
pr1<-predict(DT,t1_test)
pr1_1<-predict(DT,t1_train)
#confusion matrix for test error
confusionMatrix(pr1,t1_test$class)
#confusion matrix for training error
confusionMatrix(pr1_1,t1_train$class)
#naive bayes
nb<-naiveBayes(class~.,data=t1_train)
pr2<-predict(nb,t1_test,threshold=0.0001,eps=0)
pr2_2<-predict(nb,t1_train,threshold=0.0001,eps=0)
#confusion matrix for test error
confusionMatrix(pr2,t1_test$class)
#confusion matrix for training error
confusionMatrix(pr2_2,t1_train$class)
#kNN
Knn<-knn3(class~.,data=t1_train)
pr3<-predict(Knn,t1_test,type='class')
pr3_3<-predict(Knn,t1_train,type='class')
#confusion matrix for test error
confusionMatrix(pr3,t1_test$class)
#confusion matrix for training error
confusionMatrix(pr3_3,t1_train$class)
#10 fold cross validatation
c1=trainControl(method="repeatedcv", number=10,savePred=T,classProb=T)
Tree_grid<-expand.grid(k=(1:12))
#knn
model1<-train(class~.,data=credit,method="knn",trControl=c1,tuneGrid=Tree_grid)
plot(model1)
c2=expand.grid(C=(1:4)*0.05,M=1:4)
#DT
model2<-train(class~.,data=credit,method="J48",trControl=c1,tuneGrid=c2)
plot(model2)
#naive bayes
model3<-train(class~.,data=credit,method="nb",trControl=c1)
plot(model3)
#5 fold cross validatation
c1_2=trainControl(method="repeatedcv", number=5,savePred=T,classProb=T)
Tree_grid<-expand.grid(k=(1:12))
#knn
model1_2<-train(class~.,data=credit,method="knn",trControl=c1_2,tuneGrid=Tree_grid)
plot(model1_2)
c2_2=expand.grid(C=(1:4)*0.05,M=1:4)
#decision tree
model2_2<-train(class~.,data=credit,method="J48",trControl=c1_2,tuneGrid=c2_2)
plot(model2_2)
#nb
model3_2<-train(class~.,data=credit,method="nb",trControl=c1_2)
plot(model3_2)
#20 fold cross validatation
c1_3=trainControl(method="repeatedcv", number=20,savePred=T,classProb=T)
Tree_grid<-expand.grid(k=(1:12))
#knn
model1_3<-train(class~.,data=credit,method="knn",trControl=c1_3,tuneGrid=Tree_grid)
plot(model1_3)
#Decision Tree
c2_3=expand.grid(C=(1:4)*0.05,M=1:4)
model2_3<-train(class~.,data=credit,method="J48",trControl=c1_3,tuneGrid=c2_3)
plot(model2_3)
#naive bayes
model3_3<-train(class~.,data=credit,method="nb",trControl=c1_3)
plot(model3_3)
