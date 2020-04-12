####Read the data####
credit<-read.csv("E:\\credit-g.csv")
summary(credit)
####Feature Engineering of the Dataset####
up_trn=upSample(x=credit[,-ncol(credit)],y=credit$class,yname="Class")
summary(up_trn)
dwn_trn=downSample(x=credit[,-ncol(credit)],y=credit$class,yname="Class")
summary(dwn_trn)

up_trn=data.frame(up_trn)
up_trn=data.matrix(up_trn)


####Tuning Random Forest over mtry####
RFtune<-train(y=credit[,21],x=credit[,1:20],tuneGrid=data.frame(mtry=1:10),method="rf",ntree=1000,trControl=trainControl(method="oob"))
RFtune$results
plot(1:10,RFtune$results[,1])
RFtune1<-train(y=credit[,21],x=credit[,1:20],tuneGrid=data.frame(mtry=11:20),method="rf",ntree=1000,trControl=trainControl(method="oob"))
RFtune1$results
plot(RFtune1$results[,1])
plot(1:10,RFtune1$results[,1],xlab="mtry")
RFtune2<-train(y=credit[,21],x=credit[,1:20],tuneGrid=data.frame(mtry=20:25),method="rf",ntree=1000,trControl=trainControl(method="oob"))
RFtune2$results
plot(RFtune2$results[,1])
plot(1:10,RFtune$results2[,1],xlab="mtry")
####CV for RF without engineering input####
cl<-trainControl(method="cv",savePred=T,classProb=T)
crrf_3<-train(class~.,data=credit,method="rf",trControl=c1)
plot(credit$class,crrf_3$predicted)
####Indepedent test data for RF#####
t1<-createDataPartition(credit$class,p=.67,list=FALSE,times=1)
t1_train<-credit[t1,]
t1_test<-credit[-t1,]
mdl_3<-randomForest(class~.,data=t1_train)
prY1<-predict(mdl_3,t1_test)
table(prY1,t1_test$class)
confusionMatrix(t1_test$class,prY1)
####Random Forest with Input Engineering####
crrf_1=randomForest(Class~.,data=up_trn,type="classification",ntree=1000,mtry=3)
crrf_2=randomForest(Class~.,data=dwn_trn,type="classification",ntree=1000,mtry=3)
#####Indepedent test data for RF with downsampled data#####
t34<-createDataPartition(dwn_trn$Class,p=.67,list=FALSE,times=1)
t34_train<-dwn_trn[t34,]
t34_test<-dwn_trn[-t34,]
mdl_84<-randomForest(class~.,data=credit)
prY84<-predict(mdl_84,t34_test)
confusionMatrix(prY84,t34_test$Class)
#####CV using Upsampling for RF####
fae_1=NULL
c1=trainControl(method="cv", number=10,savePred=T,classProb=T)

fld_1<-split(sample(nrow(up_trn),nrow(up_trn),replace=FALSE),as.factor(1:5))
for (r in 1:5){
  trdata_1<-up_trn[fld_1[[r]],]
  tedtdata_1<-up_trn[-fld_1[[r]],]
  dt_1<-train(Class~.,data=trdata_1,method="rf",trControl=c1)
  fae_1[r]=(table(predict(dt_1,tedtdata_1),tedtdata_1$Class)[1,1]+table(predict(dt_1,tedtdata_1),tedtdata_1$Class)[2,2])/length(tedtdata_1$Class)
}
mean(fae_1)

####CV using Downsampling for RF#####
fae_4=NULL
c4=trainControl(method="cv", number=10,savePred=T,classProb=T)

fld_4<-split(sample(nrow(dwn_trn),nrow(dwn_trn),replace=FALSE),as.factor(1:5))
for (r in 1:5){
  trdata_4<-dwn_trn[fld_4[[r]],]
  tedtdata_4<-dwn_trn[-fld_4[[r]],]
  dt_4<-train(Class~.,data=trdata_4,method="rf",trControl=c4)
  fae_4[r]=(table(predict(dt_4,tedtdata_4),tedtdata_4$Class)[1,1]+table(predict(dt_4,tedtdata_4),tedtdata_4$Class)[2,2])/length(tedtdata_4$Class)
}
mean(fae_4)
####Support Vectors####
crsm=svm(class~.,data=credit)
prY=predict(crsm,credit)
table(credit$class,prY)
confusionMatrix(prY,credit$class)
library(caret)
####Indepedent data set for Support Vectors#####
set.seed(12)
trnI<-createDataPartition(credit$class,p=.67,list=FALSE,times=1)
trndta<-credit[trnI,]
tstdta<-credit[-trnI,]
mdl_1<-svm(class~.,data=trndta)
prY2<-predict(mdl_1,tstdta)
confusionMatrix(prY2,tstdta$class)
summary(mdl_1)
#####Tuning the model to obtain best parameters####
tun_1<-tune.svm(class~.,data=trndta,kernel="radial",gamma=10^(-8:-1),cost=10^(1:3))
summary(tun_1)
plot(tun_1)
ggplot(tun_1)
tun_2<-train(class~.,data=tstdta,method="svmRadial",gamma=1e-02,cost=10)
summary(tun_2)
ggplot(tun_2)
plot(tun_2)
sGrid<-expand.grid()
mdl_2<-train(class~.,data=credit,method="svmPoly")
plot(mdl_2)
ggplot(mdl_2)
mdl_3<-train(class~.,data=credit,method="svmLinear2")
plot(mdl_3)
ggplot(mdl_3)

mdl_4<-train(class~.,data=credit,method="svmLinear")
plot(mdl_4)
ggplot(mdl_4)
mdl_5<-train(class~.,data=credit,method="svmPoly")
plot(mdl_5)
ggplot(mdl_5)
mdl_6<-train(class~.,data=credit,method="svmRadial")
plot(mdl_6)
ggplot(mdl_6)
####Grid search for SVM#####
sGrd<-expand.grid(gamma=10^(-8:-1),cost=10^(-1:3))
mdl_43<-train(class~.,data=credit,method="svmRadial",tunrGrid=sGrd)
ggplot(mdl_43)
####Input Engineering for SVM#####

crsm_1=svm(Class~.,data=up_trn)
prY3=predict(crsm_1,up_trn)
table(up_trn$Class,prY3)
confusionMatrix(prY3,up_trn$Class)
crsm_12=svm(Class~.,data=dwn_trn)
prY32=predict(crsm_12,dwn_trn)
table(dwn_trn$Class,prY32)
confusionMatrix(prY32,dwn_trn$Class)
####Indepedent Test Data Set for upsampling SVM#####
t2<-createDataPartition(up_trn$Class,p=.67,list=FALSE,times=1)
t2_train<-up_trn[t2,]
t2_test<-up_trn[-t2,]
mdl_7<-svm(Class~.,data=t2_train)
prY7<-predict(mdl_7,t2_test)
confusionMatrix(prY7,t2_test$Class)
#####CV for upsampling SVM#####
fae_2=NULL
c2=trainControl(method="cv",number=10,savePred=T,classProb=T)

fld_2<-split(sample(nrow(up_trn),nrow(up_trn),replace=FALSE),as.factor(1:5))
for (r in 1:5){
  trdata_2<-up_trn[fld_2[[r]],]
  tedtdata_2<-up_trn[-fld_2[[r]],]
  dt_2<-train(Class~.,data=trdata_2,method="svmRadial",trControl=c2)
  fae_2[r]=(table(predict(dt_2,tedtdata_2),tedtdata_2$Class)[1,1]+table(predict(dt_2,tedtdata_2),tedtdata_2$Class)[2,2])/length(tedtdata_2$Class)
}
mean(fae_2)
#####Indepedent Test Data Set for downSampling SVM#####
t3<-createDataPartition(dwn_trn$Class,p=.67,list=FALSE,times=1)
t3_train<-dwn_trn[t3,]
t3_test<-dwn_trn[-t3,]
mdl_8<-svm(Class~.,data=t3_train)
prY8<-predict(mdl_8,t3_test)
confusionMatrix(prY8,t3_test$Class)
#####CV for downsampling SVM#####
fae_3=NULL
c3=trainControl(method="cv",number=10,savePred=T,classProb=T)

fld_3<-split(sample(nrow(dwn_trn),nrow(dwn_trn),replace=FALSE),as.factor(1:5))
for (r in 1:5){
  trdata_3<-dwn_trn[fld_3[[r]],]
  tedtdata_3<-dwn_trn[-fld_3[[r]],]
  dt_3<-train(Class~.,data=trdata_3,method="svmRadial",trControl=c3)
  fae_3[r]=(table(predict(dt_3,tedtdata_3),tedtdata_3$Class)[1,1]+table(predict(dt_3,tedtdata_3),tedtdata_3$Class)[2,2])/length(tedtdata_3$Class)
}
mean(fae_3)
#####10 fold cross validatation for knn, nb and decision tree#####
c41=trainControl(method="repeatedcv", number=10,savePred=T,classProb=T)
Tree_grid<-expand.grid(k=(1:40))
#knn
model41<-train(class~.,data=credit,method="knn",trControl=c41,tuneGrid=Tree_grid)
plot(model41)
c42=expand.grid(C=(1:4)*0.05,M=1:4)
#DT
model42<-train(class~.,data=credit,method="J48",trControl=c41,tuneGrid=c42)
plot(model42)
#naive bayes
model43<-train(class~.,data=credit,method="nb",trControl=c41)
plot(model43)
