credit<-read.csv("E:\\credit-g.csv")
library(RWeka)
library(caret)
library(c50)
library(e1071)
summary(credit)
set.seed(123)
####Feature selection with Boruta Algorithm####
BCredit<-Boruta(class ~.,data=credit)
BCredit
jpeg("vte2.jpeg", res=200, height=2000, width=4000) 
plot(BCredit)
dev.off()
####CV with and without using features selected by Baruto####
model2<-train(class~.,data=credit,method="J48",trControl=c1,tuneGrid=c2)
model2_a<-train(class~checking_status+duration+credit_amount+credit_history+other_payment_plans+age+savings_status+purpose+other_parties+employment+installment_commitment,data=credit,method="J48",trControl=c1,tuneGrid=c2)
model3_a<-train(class~checking_status+duration+credit_amount+credit_history+other_payment_plans+age+savings_status+purpose+other_parties+employment+installment_commitment+foreign_worker+housing+property_magnitude,data=credit,method="J48",trControl=c1,tuneGrid=c2)

####Derived Attributes####
credit2<-read.csv("E:\\credit-g.csv")
credit2[,1]<-as.factor(credit2[,1])
credit2[,3]<-as.factor(credit2[,3])
credit2[,4]<-as.factor(credit2[,4])
credit2[,6]<-as.factor(credit2[,6])
credit2[,7]<-as.factor(credit2[,7])
credit2[,10]<-as.factor(credit2[,10])
credit2[,14]<-as.factor(credit2[,14])
credit3<-data.frame(credit2[,1],credit2[,3],credit2[,2],credit2[,4],credit2[,5],credit2[,6],credit2[,7],credit2[,8],credit2[,10],credit2[,13],credit2[,14],credit2[,15],credit2[,16],credit2[,17],credit2[,18],credit2[,19],credit2[,20],credit2[,9],credit2[,11],credit2[,12],credit2[,21],credit2[,22])
names(credit3)<-c("checking","duration","history","purpose","amt","savings","emply","install","parties","age","plans","status","home","prop","house","exist","job","depend","phone","foreign","class","derived_attr")
summary(credit3)
c1=trainControl(method="repeatedcv", number=10,savePred=T,classProb=T)
c2=expand.grid(C=(1:4)*0.05,M=1:4)


model2_b<-train(class~checking+duration+history+purpose+amt+savings+emply+install+parties+age+plans+status+home+prop+house+exist+job+depend+phone+foreign,data=credit3,method="J48",trControl=c1,tuneGrid=c2)
plot(model2_b)
model3_b<-train(class~checking+duration+history+purpose+amt+savings+emply+install+parties+age+plans+status+home+prop+house+exist+job+depend+phone+foreign+derived_attr,data=credit3,method="J48",trControl=c1,tuneGrid=c2)
plot(model3_b)
####Artifical Balancing of the Data Set####
#Upsampling#
up_trn=upSample(x=(credit[,1:20]),y=as.factor(credit[,21]),yname="Class")
summary(up_trn)
#DownSampling#
dwn_trn=downSample(x=(credit[,1:20]),y=as.factor(credit[,21]),yname="Class")
summary(dwn_trn)


#CV using upsampling
fae_1=NULL

fld_1<-split(sample(nrow(up_trn),nrow(up_trn),replace=FALSE),as.factor(1:5))
for (r in 1:5){
  trdata_1<-up_trn[fld_1[[r]],]
  tedtdata_1<-up_trn[-fld_1[[r]],]
  dt_1<-train(Class~.,data=trdata_1,method="J48",trControl=c1,tuneGrid=c2)
  fae_1[r]=(table(predict(dt_1,tedtdata_1),tedtdata_1$Class)[1,1]+table(predict(dt_1,tedtdata_1),tedtdata_1$Class)[2,2])/length(tedtdata_1$Class)
}
plot(fae_1)
mean(fae_1)
#CV using downsampling
fae_2=NULL

fld_2<-split(sample(nrow(dwn_trn),nrow(dwn_trn),replace=FALSE),as.factor(1:5))
for (r in 1:5){
  trdata_2<-dwn_trn[fld_2[[r]],]
  tedtdata_2<-dwn_trn[-fld_2[[r]],]
  dt_2<-train(Class~.,data=trdata_2,method="J48",trControl=c1,tuneGrid=c2)
  fae_2[r]=(table(predict(dt_2,tedtdata_2),tedtdata_2$Class)[1,1]+table(predict(dt_2,tedtdata_2),tedtdata_2$Class)[2,2])/length(tedtdata_2$Class)
}
plot(fae_2)

mean(fae_2)
####SMOTE####
fae=NULL
s_train<-SMOTE(class~.,data=credit)
summary(s_train)
#CV using SMOTE

fld<-split(sample(nrow(s_train),nrow(s_train),replace=FALSE),as.factor(1:5))
for (r in 1:5){
  trdata<-s_train[fld[[r]],]
  tedtdata<-s_train[-fld[[r]],]
  dt<-train(class~.,data=trdata,method="J48",trControl=c1,tuneGrid=c2)
  fae[r]=(table(predict(dt,tedtdata),tedtdata$class)[1,1]+table(predict(dt,tedtdata),tedtdata$class)[2,2])/length(tedtdata$class)
}
mean(fae)
plot(fae)
