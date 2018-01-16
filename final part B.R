
attach(Bike_data)
fix(Bike_data)
set.seed(1)
train=sample(711,600)
#str(Bike_data)
#summary(Bike_data)
count01=rep("High",nrow(Bike_data))
count01[count<median(count)]="Low"
Bike_data$count01=count01

Bike_data$season=as.factor(Bike_data$season)
Bike_data$year=as.factor(Bike_data$year)
Bike_data$month=as.factor(Bike_data$month)
Bike_data$holiday=as.factor(Bike_data$holiday)
Bike_data$weekday=as.factor(Bike_data$weekday)
Bike_data$weathersit=as.factor(Bike_data$weathersit)
Bike_data$count01=as.factor(Bike_data$count01)
count01=as.factor(count01)
bike_test=Bike_data[-train,]
count_resp=count01[-train]
#cor(Bike_data)
Bike_data=Bike_data[,-11]
#aggregate(count, by=list(Bike_data$month), FUN=median)[2]

#plot(month,count)
#
#bikeD=Bike_data[,-12]
#count01=rep("1",nrow(Bike_data))
#count01[count<median(count)]="0"
#bikeD$count01=count01
#bikeD$count01=as.factor(bikeD$count01)
#str(bikeD)
#
#bike_test_log=bikeD[-train,]
#bike_test=Bike_data[-train,]
#logistic
contrasts(bike_test$count01)
Bike_data=Bike_data[,-11]
log1.fit=glm(count01~.,data=Bike_data,family=binomial,subset=train)
summary(log1.fit)
log1.prob=predict(log1.fit,bike_test,type="response")

log1.pred=rep("Low",length(log1.prob))
log1.pred[log1.prob<0.5]="High"

table(log1.pred,count_resp)
mean(log1.pred==count_resp)


str(bike_test)

#ROC curve

log1.fit=glm(count01~.,data=Bike_data,family=binomial,subset=train)
log1.pred=predict(log1.fit,bike_test,type="response")
contrasts(bike_test$count01)
roc.curve=function(s,print=FALSE)
{
  Ps=(log1.pred>s)*1
  FP=sum((Ps==1)*(bike_test$count01=="High"))/sum(Bike_data$count01=="High")
  TP=sum((Ps==1)*(bike_test$count01=="Low"))/sum(Bike_data$count01=="Low")
  if(print==TRUE){
    print(table(Observed=bike_test$count01,Predicted=Ps))
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



auc(count01,Bike_data)


#LDA
library(MASS)
lda.fit=lda(count01~.,data=Bike_data,family=binomial,subset=train)
summary(lda.fit)
log1.prob=predict(lda.fit,bike_test,type="response")
lda1.class=log1.prob$class
table(lda1.class,count_resp)
mean(lda1.class==count_resp)



#QDA
str(Bike_data)
summary(Bike_data$month)
library(MASS)
qda.fit=qda(count01~.-weathersit,data=Bike_data,family=binomial,subset=train)
summary(qda.fit)
qda.prob=predict(qda.fit,bike_test,type="response")
qda.class=qda.prob$class

table(qda.class,count_resp)
mean(qda.class==count_resp)


#KNN
library(class)
train.X=cbind(season,year,month,weekday,weathersit,temp,holiday,atemp,hum,windspeed)[train,]
test.X=cbind(season,year,month,weekday,weathersit,temp,holiday,atemp,hum,windspeed)[-train,]
train.Direction=count01[train]

maxk=0
max_K=1
mean_K=0
for (i in 1:100)
{
  set.seed(1)
  knn.pred=knn(data.frame(train.X),data.frame(test.X),train.Direction,k=i)
  mean_K=mean(knn.pred==count_resp)
  if(mean_K>maxk)
  {   maxk=mean_K
  max_K=i
  }
}
print(maxk) #87.39
print(max_K) #6


set.seed(1)
knn.pred=knn(data.frame(train.X),data.frame(test.X),train.Direction,k=6)

table(knn.pred,count_resp)
mean(knn.pred==count_resp)

#tree
library(tree)
tree.car=tree(count01~.,Bike_data,subset=train)
summary(tree.car)
plot(tree.car)
text(tree.car ,pretty =0)
tree.car.pred=predict(tree.car,bike_test,type="class")
table(tree.car.pred,count_resp)
mean(tree.car.pred==count_resp)

set.seed(10)
cv.carseats <- cv.tree(tree.car,FUN=prune.misclass)
(cv.carseats)
plot(cv.carseats$size, cv.carseats$dev, type = "b")
#tree.min=which.min(cv.carseats$dev)
#points(tree.min, cv.carseats$dev[tree.min],col="red", cex = 2, pch = 20)

prune.car=prune.misclass(tree.car,best=7)
plot(prune.car)
text(prune.car,pretty=0)

prune.car.pred=predict(prune.car,bike_test,type="class")
table(prune.car.pred,count_resp)
mean(prune.car.pred==count_resp)


#aggregate(Bike_data$count, by=list(Bike_data$weathersit), FUN=mean)[2]

#bagging
library(randomForest)
library(MASS)
set.seed(1)
bag.car=randomForest(count01~.,data=Bike_data,subset=train,mtry=10,ntree=500,importance=TRUE)
bag.car
bag.car.pred=predict(bag.car,newdata=Bike_data[-train,])
table(bag.car.pred,count_resp)
mean(bag.car.pred==count_resp)
importance(bag.car)
varImpPlot(bag.car)

test.mean1=rep(0,10)
test.mean2=rep(0,200)

#best mtry
for (i in 1:10)
{
  #set.seed(1)
  #train=sample(1:nrow(Boston),nrow(Boston)/2)
  #boston.test=Boston[-train,"medv"]
  set.seed(1)
  bag.car=randomForest(count01~.,data=Bike_data,subset=train,mtry=i,ntree=100,importance=TRUE)
  #bag.boston
  bag.car.pred=predict(bag.car,newdata=Bike_data[-train,])
  test.mean1[i]=mean(bag.car.pred==count_resp)
}

plot(test.mean1)
for (i in 1:200)
{
  #train=sample(1:nrow(Boston),nrow(Boston)/2)
  #boston.test=Boston[-train,"medv"]
  #set.seed(1)
  set.seed(1)
  bag.car=randomForest(count01~.,data=Bike_data,subset=train,mtry=10,ntree=i,importance=TRUE)
  #bag.boston
  bag.car.pred=predict(bag.car,newdata=Bike_data[-train,])
  test.mean2[i]=mean(bag.car.pred==count_resp)
}
plot(test.mean2,xlab="ntree",ylab="Test error")
which.min(test.mean2)

#randomforest
library(randomForest)
library(MASS)
set.seed(1)
bag.car=randomForest(count01~.,data=Bike_data,subset=train,mtry=3,ntree=100,importance=TRUE)
bag.car
bag.car.pred=predict(bag.car,newdata=Bike_data[-train,])
table(bag.car.pred,count_resp)
mean(bag.car.pred==count_resp)
importance(bag.car)
varImpPlot(bag.car)

#boosting
library(gbm)
set.seed(1)
#bikeD=Bike_data[,-12]
#count01=rep("1",nrow(Bike_data))
#count01[count<median(count)]="0"
#bikeD$count01=count01
#bikeD$count01=as.factor(bikeD$count01)
#str(bikeD)

boost.boston=gbm(unclass(count01)-1~.,data=Bike_data[train,],distribution="bernoulli",n.trees=5000,interaction.depth =4)

summary(boost.boston)
par(mfrow=c(1,2))
plot(boost.boston)
plot(boost.boston)

pred.boost=predict(boost.boston,Bike_data[-train,],n.trees=5000,type="response")

log1.pred=rep("High",length(pred.boost))
log1.pred[pred.boost>0.5]="Low"

table(log1.pred,count_resp)
mean(log1.pred==count_resp)

  
#svm
#---linear
library(e1071)
tune.out=tune(svm,count01~.,data=Bike_data[train,],kernel="linear",ranges=list(cost=c(0.01,0.1,1,5,10,100)),scale=FALSE)
summary(tune.out)#error min=cost(1)
svm.fit=svm(count01~.,data=Bike_data,subset=train,kernel="linear",cost=10,scale=FALSE)
summary(svm.fit)
pred=predict(svm.fit,newdata=Bike_data[-train,])

table(pred,count_resp)
mean(pred==count_resp)


#---radial
library(e1071)
tune.out=tune(svm,count01~.,data=Bike_data[train,],kernel="radial",ranges=list(cost=c(0.01,0.1,1,5,10,100),gamma=c(0.5,1,2,3,4)),scale=FALSE)
summary(tune.out)
svm.fit=svm(count01~.,data=Bike_data,subset=train,kernel="radial",gamma=0.5,cost=100,scale=FALSE)
summary(svm.fit)

pred=predict(svm.fit,newdata=Bike_data[-train,])

table(pred,count_resp)
mean(pred==count_resp)



#---polynomial
library(e1071)
tune.out=tune(svm,count01~.,data=Bike_data[train,],kernel="polynomial",ranges=list(cost=c(0.01,0.1,1,5,10,100),degree=c(2,3,4,5)),scale=FALSE)
summary(tune.out)
svm.fit=svm(count01~.,data=Bike_data,subset=train,kernel="polynomial",cost=100, degree=2,scale=FALSE)
pred=predict(svm.fit,newdata=Bike_data[-train,])

table(pred,count_resp)
mean(pred==count_resp)
