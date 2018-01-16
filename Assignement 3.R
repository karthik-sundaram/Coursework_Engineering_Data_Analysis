library(ISLR)
library(MASS)
attach(Smarket)
?Smarket
attach(Weekly)
?Weekly

str(Weekly)
dim(Weekly)
summary(Weekly)

cor(Weekly[,-9])
plot(Volume)

plot (Volume~year)

mean<-aggregate(Volume~Year, data=Weekly,FUN=sum)
plot(mean)

trend<-aggregate(Direction~Year,data=Weekly,FUN=sum)
plot(trend)

logistic.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Weekly, family=binomial)
summary(logistic.fit)

log.prob=predict(logistic.fit,type="response")
log.prob

logistic.pred=rep("Down",1089)
logistic.pred[log.prob>0.5]="Up"

table(logistic.pred,Direction)



#LOGistic
train=(Year<2009)
Week.200910=Weekly[!train,]
Direction.200910=Direction[!train]

log1.fit=glm(Direction~Lag2,data=Weekly,family=binomial,subset=train)
summary(log1.fit)
log1.prob=predict(log1.fit,Week.200910,type="response")


log1.pred=rep("Down",length(log1.prob))
log1.pred[log1.prob>0.5]="Up"

table(log1.pred,Direction.200910)
mean(log1.pred==Direction.200910)



#LDA
train=(Year<2009)
Week.200910=Weekly[!train,]
Direction.200910=Direction[!train]

lda1.fit=lda(Direction~Lag2,data=Weekly,subset=train)
#summary(lda1.fit)
lda1.prob=predict(lda1.fit,Week.200910)


lda1.class=lda1.prob$class

table(lda1.class,Direction.200910)
mean(lda1.class==Direction.200910)



#QDA
train=(Year<2009)
Week.200910=Weekly[!train,]
Direction.200910=Direction[!train]

lda1.fit=qda(Direction~Lag2,data=Weekly,subset=train)
#summary(lda1.fit)
lda1.prob=predict(lda1.fit,Week.200910)


lda1.class=lda1.prob$class

table(lda1.class,Direction.200910)
mean(lda1.class==Direction.200910)


#KNN
library(class)
train=(Year<2009)
Direction.200910=Direction[!train]
train.X=cbind(Lag2)[train,]
test.X=cbind(Lag2)[!train,]
train.Direction=Direction[train]

set.seed(1)
knn.pred=knn(data.frame(train.X),data.frame(test.X),train.Direction,k=50)

table(knn.pred,Direction.200910)
mean(knn.pred==Direction.200910)

library(class)
train=(Year<2009)
Direction.200910=Direction[!train]
train.X <- as.matrix(Lag2[train])
test.X <- as.matrix(Lag2[!train])
train.Direction <- Direction[train]
set.seed(1)
pred.knn <- knn(train.X, test.X, train.Direction, k = 1)
table(pred.knn, Direction.200910)


#LOGISTIC2
train=(Year<2009)
Week.200910=Weekly[!train,]
Direction.200910=Direction[!train]
log1.fit=glm(Direction~Lag2:Lag1,data=Weekly,family=binomial,subset=train)
summary(log1.fit)
log1.prob=predict(log1.fit,Week.200910,type="response")
log1.pred=rep("Down",length(log1.prob))
log1.pred[log1.prob>0.5]="Up"
table(log1.pred,Direction.200910)
mean(log1.pred==Direction.200910)


#LOGISTIC2
train=(Year<2009)
Week.200910=Weekly[!train,]
Direction.200910=Direction[!train]

log1.fit=glm(Direction~Lag2+I(Lag2^2),data=Weekly,family=binomial,subset=train)
summary(log1.fit)
log1.prob=predict(log1.fit,Week.200910,type="response")

log1.pred=rep("Down",length(log1.prob))
log1.pred[log1.prob>0.5]="Up"

table(log1.pred,Direction.200910)
mean(log1.pred==Direction.200910)


#LDA1
train=(Year<2009)
Week.200910=Weekly[!train,]
Direction.200910=Direction[!train]

lda1.fit=lda(Direction~Lag2+I(Lag2^2)+I(Lag2^3),Data=Weekly,subset=train)
#summary(lda1.fit)
lda1.prob=predict(lda1.fit,Week.200910)


lda1.class=lda1.prob$class

table(lda1.class,Direction.200910)
mean(lda1.class==Direction.200910)



#QDA
train=(Year<2009)
Week.200910=Weekly[!train,]
Direction.200910=Direction[!train]

lda1.fit=qda(Direction~Lag2+I(Lag2^2),data=Weekly,subset=train)
#summary(lda1.fit)
lda1.prob=predict(lda1.fit,Week.200910)


lda1.class=lda1.prob$class

table(lda1.class,Direction.200910)
mean(lda1.class==Direction.200910)


#KNN
library(class)
train=(Year<2009)
Direction.200910=Direction[!train]
train.X=cbind(Lag2)[train,]
test.X=cbind(Lag2)[!train,]
train.Direction=Direction[train]

maxk=0
max_K=1
mean_K=0
for (i in 1:length(train.X))
  {
set.seed(1)
knn.pred=knn(data.frame(train.X),data.frame(test.X),train.Direction,k=i)
mean_K=mean(knn.pred==Direction.200910)
if(mean_K>maxk)
 {   maxk=mean_K
     max_K=i
}
}
print(maxk)
print(max_K)

knn.pred=knn(data.frame(train.X),data.frame(test.X),train.Direction,k=max_K)

table(knn.pred,Direction.200910)
mean(knn.pred==Direction.200910)



LR.fit=glm(Direction~Lag2,family=binomial,data=Weekly)
LR.pred=predict(LR.fit,type="response")

roc.curve=function(s,print=FALSE)
{
  Ps=(LR.pred>s)*1
  FP=sum((Ps==1)*(Direction=="Down"))/sum(Direction=="Down")
  TP=sum((Ps==1)*(Direction=="Up"))/sum(Direction=="Up")
  if(print==TRUE){
    print(table(Observed=Direction,Predicted=Ps))
  }
  vect=c(FP,TP)
  names(vect)=c("FPR","TPR")
  return(vect)
}
  threshold=0.5
  roc.curve(threshold,print=TRUE)


ROC.curve=Vectorize(roc.curve)
M.ROC=ROC.curve(seq(0,1,by=0.01))
plot(M.ROC[1,],M.ROC[2,],col="grey",lwd=2,type="l",xlab="Falsepositive Rate",ylab="True positive rate")



LR.fit=lda(Direction~Lag2,data=Weekly)
LR.pred=predict(LR.fit,type="response")


train=(Year<2009)
Week.200910=Weekly[!train,]
Direction.200910=Direction[!train]
LR.fit=lda(Direction~Lag2,Data=Weekly,subset=train)
#summary(lda1.fit)
lda1.prob=predict(LR.fit,Week.200910)
LR.pred=lda1.prob$posterior[,2]



roc.curve=function(s,print=FALSE)
{
  Ps=(LR.pred>s)*1
  FP=sum((Ps==1)*(Direction.200910=="Down"))/sum(Direction.200910=="Down")
  TP=sum((Ps==1)*(Direction.200910=="Up"))/sum(Direction.200910=="Up")
  if(print==TRUE){
    print(table(Observed=Direction.200910,Predicted=Ps))
  }
  vect=c(FP,TP)
  names(vect)=c("FPR","TPR")
  return(vect)
}
threshold=0.5
roc.curve(threshold,print=TRUE)


ROC.curve=Vectorize(roc.curve)
M.ROC=ROC.curve(seq(0,1,by=0.01))
plot(M.ROC[1,],M.ROC[2,],col="grey",lwd=2,type="l",xlab="False positive Rate",ylab="True positive rate")

auc(Direction,Weekly)

rm(list = ls())