install.packages("tree")
library(ISLR)
library(tree)
attach(Carseats)
set.seed(1)
train=sample(1:nrow(Carseats),nrow(Carseats)/2)
tree.car=tree(Sales~.,Carseats,subset=train)
summary(tree.car)
plot(tree.car)
text(tree.car ,pretty =0)

tree.car.pred=predict(tree.car,newdata=Carseats[-train,])
Sales.test=Sales[-train]
mean((tree.car.pred-Sales.test)^2)


set.seed(2)
cv.carseats <- cv.tree(tree.car)
plot(cv.carseats$size, cv.carseats$dev, type = "b")
tree.min=which.min(cv.carseats$dev)
points(tree.min, cv.carseats$dev[tree.min],col="red", cex = 2, pch = 20)

prune.car=prune.tree(tree.car,best=8)
plot(prune.car)
text(prune.car,pretty=0)

tree.car.pred=predict(prune.car,newdata=Carseats[-train,])
Sales.test=Sales[-train]
mean((tree.car.pred-Sales.test)^2)


#Bagging
install.packages("randomForest")
library(randomForest)
library(MASS)
set.seed(1)
bag.car=randomForest(Sales~.,data=Carseats,subset=train,mtry=10,ntree=100,importance=TRUE)
bag.car
bag.car.pred=predict(bag.car,newdata=Carseats[-train,])
Sales.test=Sales[-train]
mean((bag.car.pred-Sales.test)^2)
importance(bag.car)
varImpPlot(bag.car)


#randomForest
set.seed(3)
forest.car=randomForest(Sales~.,data=Carseats,subset=train,mtry=3,ntree=100,importance=TRUE)
forest.car
forest.car.pred=predict(forest.car,newdata=Carseats[-train,])
Sales.test=Sales[-train]
mean((forest.car.pred-Sales.test)^2)
importance(forest.car)
varImpPlot(forest.car)



#2a)
for (i in 1:13)
{
#set.seed(1)
train=sample(1:nrow(Boston),nrow(Boston)/2)
boston.test=Boston[-train,"medv"]
set.seed(1)
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=i,ntree=100,importance=TRUE)
#bag.boston
boston.pred=predict(bag.boston,newdata=Boston[-train,])
Test.mean2[i]=mean((boston.pred-boston.test)^2)
}

plot(Test.mean2,xlab="mtry",ylab="Test error")

#2b)
for (i in 5:200)
{
  set.seed(1)
  train=sample(1:nrow(Boston),nrow(Boston)/2)
  boston.test=Boston[-train,"medv"]
  set.seed(1)
  bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=6,ntree=i,importance=TRUE)
  #bag.boston
  boston.pred=predict(bag.boston,newdata=Boston[-train,])
  Test.mean[i]=mean((boston.pred-boston.test)^2)
}

plot(Test.mean,xlab="ntree",ylab="Test error")

rm(list=ls())