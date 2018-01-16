library(ISLR)
attach(Auto)
Auto
Auto$gas_median=0
Auto$gas_median[mpg>median(mpg)]=1

str(Auto)
install.packages("e1071")
library(e1071)
Auto$gas_median=as.factor(Auto$gas_median)



#svm.fit=svm(gas_median~.-mpg,data=Auto,kernel="linear",cost=0.1,scale=FALSE)
#summary(svm.fit)
set.seed(1)
tune.out=tune(svm,gas_median~.,data=Auto,kernel="linear",ranges=list(cost=c(0.01,0.1,1,5,10,100)))
summary(tune.out)

bestmod=tune.out$best.model
summary(bestmod)

pred=predict(bestmod,Auto)
table(pred,Auto$gas_median)

#for (i in 1:10)
#{
#  svm.fit=tune(gas_median~.-mpg,data=Auto,kernel="linear",cost=0.0001*10^i)
#}


set.seed(1)
tune.out <- tune(svm, gas_median ~ ., data = Auto, kernel = "polynomial", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100), degree = c(2, 3, 4,5)))
summary(tune.out)

bestmod=tune.out$best.model
summary(bestmod)

pred=predict(bestmod,Auto)
table(pred,Auto$gas_median)



set.seed(1)
tune.out <- tune(svm, gas_median ~ ., data = Auto, kernel = "radial", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100), gamma = c(0.5,1,2,3,4)))
summary(tune.out)

bestmod=tune.out$best.model
summary(bestmod)

pred=predict(bestmod,Auto)
table(pred,Auto$gas_median)


#2
attach(OJ)
train=sample(1:nrow(OJ),800)

svm.fit=svm(Purchase~.,data=OJ,subset=train,kernel="linear",cost=0.01,scale=FALSE)
summary(svm.fit)

pred=predict(svm.fit,newdata=OJ[train,])
train_purchase=Purchase[train]
table(pred,train_purchase)


pred=predict(svm.fit,newdata=OJ[-train,])
test_purchase=Purchase[-train]
table(pred,test_purchase)


train.OJ=OJ[train,]
tune.out=tune(svm,Purchase~.,data=train.OJ,kernel="linear",ranges=list(cost=10^seq(-2, 1, by = 0.5)))
summary(tune.out)
bestmod=tune.out$best.model
summary(bestmod)


pred=predict(bestmod,newdata=OJ[train,])
train_purchase=Purchase[train]
table(pred,train_purchase)

pred=predict(bestmod,newdata=OJ[-train,])
test_purchase=Purchase[-train]
table(pred,test_purchase)




#radial 
svm.fit=svm(Purchase~.,data=OJ,subset=train,kernel="radial",cost=0.01) #deafault gamma value used
summary(svm.fit)

pred=predict(svm.fit,newdata=OJ[train,])
train_purchase=Purchase[train]
table(pred,train_purchase)


pred=predict(svm.fit,newdata=OJ[-train,])
test_purchase=Purchase[-train]
table(pred,test_purchase)


train.OJ=OJ[train,]
tune.out=tune(svm,Purchase~.,data=train.OJ,kernel="radial",ranges=list(cost=10^seq(-2, 1, by = 0.5),gamma=c(0.1,0.5,1,2,3,4)))
summary(tune.out)
bestmod=tune.out$best.model
summary(bestmod)


pred=predict(bestmod,newdata=OJ[train,])
train_purchase=Purchase[train]
table(pred,train_purchase)

pred=predict(bestmod,newdata=OJ[-train,])
test_purchase=Purchase[-train]
table(pred,test_purchase)




#polynomial
svm.fit=svm(Purchase~.,data=OJ,subset=train,kernel="polynomial",cost=0.01) #deafault gamma value used
summary(svm.fit)

pred=predict(svm.fit,newdata=OJ[train,])
train_purchase=Purchase[train]
table(pred,train_purchase)


pred=predict(svm.fit,newdata=OJ[-train,])
test_purchase=Purchase[-train]
table(pred,test_purchase)


train.OJ=OJ[train,]
tune.out=tune(svm,Purchase~.,data=train.OJ,kernel="polynomial",ranges=list(cost=10^seq(-2, 1, by = 0.5)),degree=2)
summary(tune.out)
bestmod=tune.out$best.model
summary(bestmod)


pred=predict(bestmod,newdata=OJ[train,])
train_purchase=Purchase[train]
table(pred,train_purchase)

pred=predict(bestmod,newdata=OJ[-train,])
test_purchase=Purchase[-train]
table(pred,test_purchase)
str(OJ)
