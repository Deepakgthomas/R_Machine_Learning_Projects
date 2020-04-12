library(arules)
library(arulesViz)
library(RWeka)
####Decision Trees####
d=read.csv("~/IE 583/vote.csv")
d[]=lapply(d,factor)
set.seed(123)
trIndex<-createDataPartition(d$Class,p=.7,list=FALSE,times=1)
tr1<-d[trIndex,]# Training Data
tr2<-d[-trIndex,] # Test Data
DT=J48(Class~.,data=tr1)
jpeg("vote.jpeg", res=80, height=800, width=1600) 
plot(DT) 
dev.off()
####Association Rule Mining####
pr=predict(DT,tr2)
rles = apriori(d)# All rules from Apriori Algorithm
summary(rles)
plot(rles)
better_rules<-rles[quality(rles)$confidence>0.8]#Setting confidence limit
better_rules1<-better_rules[quality(better_rules)$support>0.05]#Setting support limit
better_rules2<-better_rules1[quality(better_rules1)$lift>2.69]#Setting lift limit
plot(better_rules2_1,method="graph",control=list(type="itemsets"))
inspect(better_rules2_1) # Inspecting all rules
plot(better_rules2_1,method="grouped",control=list(k=20))
better_rules2_1<-better_rules2[!is.redundant(better_rules2)]#Removing Redundant Rules
arules::inspect(better_rules2_1)
####ARM rules for grouping into Democrats and Republicans####
better1_rules<-rles[quality(rles)$confidence>0.95]
better1_rules1<-better1_rules[quality(better1_rules)$support>0.3]
better1_rules2<-better1_rules1[quality(better1_rules1)$lift>1.62]
better_rules2_2<-better1_rules2[!is.redundant(better1_rules2)] # Removing Redundant Rules
####Grouping as Democrats####
Drles <- subset(better_rules2_2, subset = rhs %in% "Class=democrat")
Drles_1<-Drles[!is.redundant(Drles)]
inspect(Drles_1)
plot(Drles_1, method = "grouped")
plot(Drles_1)
####Grouping as Republicans####
Rrles<- subset(better_rules2_2, subset = rhs %in% "Class=republican")
Rrles_1<-Rrles[!is.redundant(Rrles)]
inspect(Rrles_1)# Inspecting rules with consequent republicans
plot(Rrlest_1)
plot(Rrles_1,method ="graph")
