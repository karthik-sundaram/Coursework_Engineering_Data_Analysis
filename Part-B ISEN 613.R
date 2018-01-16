library(ISLR)
setwd("C:/Users/Karthik/Desktop")
train<-read.csv("Data_training.csv")
test<-read.csv("Data_test.csv")


str(train)
summary(train)

str(test)
summary(test)

var(train$survival)
barplot(train$survival)
cor(train[,-c(6,7)])
pairs(train)
plot(train$gender,train$survival,main="Gender vs Survival Days")
plot(train$prog,train$survival,main="prog vs Survival Days")
plot(train$enzy,train$survival,main="enzy vs Survival Days")
plot(train$alco,train$survival,main="alco vs Survival Days")
plot(train$bldclot,train$survival,main="bldclot vs Survival Days")
plot(train$age,train$survival,main="age vs Survival Days")
plot(train$age,train$survival,main="liverfunc vs Survival Days")


pairs(train)
library(ISLR)
setwd("C:/Users/Karthik/Desktop")
train<-read.csv("Data_training.csv")
test<-read.csv("Data_test.csv")


str(train)
summary(train)

str(test)
summary(test)

var(train$survival)
barplot(train$survival)
cor(train[,-c(6,7)])
pairs(train)
plot(train$gender,train$survival)
plot(train$prog,train$survival)
plot(train$enzy,train$survival)
plot(train$alco,train$survival)
plot(train$bldclot,train$survival)
plot(train$age,train$survival)
plot(train$liverfunc,train$survival)

train$gender=factor(train$gender)
train$alco=factor(train$alco)

test$gender=factor(test$gender)
test$alco=factor(test$alco)

#
train$gender[train$gender==0]="male"
train$gender[train$gender==1]="female"
table(train$gender)
train$alco[train$alco==0]="none"
train$alco[train$alco==1]="moderate"
train$alco[train$alco==2]="severe"
table(train$alco)

test$gender[test$gender==0]="male"
test$gender[test$gender==1]="female"
table(test$gender)
test$alco[test$alco==0]="none"
test$alco[test$alco==1]="moderate"
test$alco[test$alco==2]="severe"
table(test$alco)
#

{
lm.fit=lm(survival~.,data=train)
summary(lm.fit) #train R^2 [Multiple R-squared:  0.7809,	Adjusted R-squared:  0.7465 F-statistic: 22.72 on 8 and 51 DF,  p-value: 2.62e-14]

#lm.pred.train=predict(lm.fit,train)
#lm.pred.train
#mean((lm.pred.train-train$survival)^2) #train MSE

lm.pred.test=predict(lm.fit,test)
lm.pred.test
mean((lm.pred.test-test$survival)^2) #test MSE [26543.53]

plot(lm.fit,which=1) #detected funnnel/heteroskedasticity
plot(lm.fit,which=2)
plot(lm.fit,which=3)
plot(lm.fit,which=4)
plot(lm.fit,which=5)
------------------------------------------------
lm.fit.test=lm(survival~.,data=test)
summary(lm.fit.test) #test R^2 [Multiple R-squared:  0.836,	Adjusted R-squared:  0.8024 F-statistic: 24.85 on 8 and 39 DF,  p-value: 4.857e-13]

}

{
  lm.fit=lm(survival~.-gender,data=train)
  summary(lm.fit) #train R^2 
  
  lm.pred.train=predict(lm.fit,train)
  lm.pred.train
  mean((lm.pred.train-train$survival)^2) #train MSE
  
  lm.pred.test=predict(lm.fit,test)
  lm.pred.test
  mean((lm.pred.test-test$survival)^2) #test MSE
  
  plot(lm.fit,which=1) #detected funnnel/heteroskedasticity
  ------------------------------------------------
    lm.fit.test=lm(survival~.-gender,data=test)
  summary(lm.fit.test) #test R^2  [Multiple R-squared:  0.8355,	Adjusted R-squared:  0.8067 ]
  
}
plot(lm.fit,which=1)
plot(lm.fit,which=3)

{
  lm.fit=lm(log(survival)~-bldclot-age+prog+I(prog^2)+enzy+I(enzy^2)+liverfunc+gender+alco,data=train)
  summary(lm.fit) #train R^2
  lm.pred.train=predict(lm.fit,train)
  lm.pred.train
  mean((lm.pred.train-train$survival)^2) #train MSE
  
  lm.pred.test=predict(lm.fit,test)
  lm.pred.test
  mean(((lm.pred.test)-log(test$survival))^2) #test MSE
  
  
  ------------------------------------------------
    lm.fit.test=lm(survival~.,data=test)
  summary(lm.fit.test) #test R^2
}


{
  lm.fit=lm(survival~bldclot+log(bldclot)+prog+log(prog)+enzy+log(enzy)+liverfunc+age+gender+alco,data=train)
  summary(lm.fit) #train R^2
  lm.pred.train=predict(lm.fit,train)
  lm.pred.train
  mean((lm.pred.train-train$survival)^2) #train MSE
  
  lm.pred.test=predict(lm.fit,test)
  lm.pred.test
  mean((lm.pred.test-test$survival)^2) #test MSE
  
  
  ------------------------------------------------
    lm.fit.test=lm(survival~.,data=test)
  summary(lm.fit.test) #test R^2
}

{
  lm.fit=lm(survival~bldclot+log(bldclot)+prog+log(prog)+enzy+log(enzy)+liverfunc+age+gender+alco,data=train)
  summary(lm.fit) #train R^2
  lm.pred.train=predict(lm.fit,train)
  lm.pred.train
  mean((lm.pred.train-train$survival)^2) #train MSE
  
  lm.pred.test=predict(lm.fit,test)
  lm.pred.test
  mean((lm.pred.test-test$survival)^2) #test MSE
  
  lm.pred.test=predict(lm.fit,test)
  lm.pred.test
  mean((lm.pred.test-test$survival)^2) #test MSE
  
  ------------------------------------------------
    lm.fit.test=lm(survival~.,data=test)
  summary(lm.fit.test) #test R^2
}

{
  lm.fit=lm(log(survival)~.,data=train)
  summary(lm.fit) #train R^2
  lm.pred.train=predict(lm.fit,train)
  lm.pred.train
  mean((lm.pred.train-log(train$survival))^2) #train MSE
  
  lm.pred.test=predict(lm.fit,test)
  lm.pred.test
  mean((exp(lm.pred.test)-test$survival)^2) #test MSE
  
  plot(lm.fit,which=1)
  plot(lm.fit,which=3)
  test$survival_pred=exp(lm.pred.test)
  
  ------------------------------------------------
    lm.fit.test=lm(log(survival)~.,data=test)
  summary(lm.fit.test) #test R^2
}

cor(train[,-c(6,7)])
pairs(train)

{
  lm.fit=lm(log(survival)~.-liverfunc-bldclot-age+gender:enzy +enzy:bldclot+alco:prog,data=train)
  summary(lm.fit) #train R^2
  plot(lm.fit,which=1)
  lm.fit=lm(log(survival)~.-liverfunc-bldclot-age+gender:enzy +enzy:bldclot+alco:prog,data=test)
  summary(lm.fit) #test R^2
  
  #library(car)
  #vif(lm.fit)
  plot(lm.fit,which=5)
  lm.pred.train=predict(lm.fit,train)
  lm.pred.train
  mean((lm.pred.train-log(train$survival))^2) #train MSE
  
  lm.pred.test=predict(lm.fit,test)
  lm.pred.test
  mean((lm.pred.test-log(test$survival))^2) #test MSE
  
  lm.pred.test2=predict(lm.fit,test)
  lm.pred.test2
  mean((exp(lm.pred.test2)-test$survival)^2) #test MSE
  
  plot(lm.fit,which=1)
  plot(lm.fit,which=3)
  test$survival_pred=exp(lm.pred.test)
  
  ------------------------------------------------
    lm.fit.test=lm(log(survival)~.,data=test)
  summary(lm.fit.test) #test R^2
}


plot(train$survival,prog)

plot(train$liverfunc,train$survival)

train$gender=factor(train$gender)
train$alco=factor(train$alco)

test$gender=factor(test$gender)
test$alco=factor(test$alco)

#
train$gender[train$gender==0]="male"
train$gender[train$gender==1]="female"
table(train$gender)
train$alco[train$alco==0]="none"
train$alco[train$alco==1]="moderate"
train$alco[train$alco==2]="severe"
table(train$alco)

test$gender[test$gender==0]="male"
test$gender[test$gender==1]="female"
table(test$gender)
test$alco[test$alco==0]="none"
test$alco[test$alco==1]="moderate"
test$alco[test$alco==2]="severe"
table(test$alco)
#

{
lm.fit=lm(survival~.,data=train)
summary(lm.fit) #train R^2 [Multiple R-squared:  0.7809,	Adjusted R-squared:  0.7465 F-statistic: 22.72 on 8 and 51 DF,  p-value: 2.62e-14]

#lm.pred.train=predict(lm.fit,train)
#lm.pred.train
#mean((lm.pred.train-train$survival)^2) #train MSE

lm.pred.test=predict(lm.fit,test)
lm.pred.test
mean((lm.pred.test-test$survival)^2) #test MSE [26543.53]

plot(lm.fit,which=1) #detected funnnel/heteroskedasticity
plot(lm.fit,which=2)
plot(lm.fit,which=3)
plot(lm.fit,which=4)
plot(lm.fit,which=5)
------------------------------------------------
lm.fit.test=lm(survival~.,data=test)
summary(lm.fit.test) #test R^2 [Multiple R-squared:  0.836,	Adjusted R-squared:  0.8024 F-statistic: 24.85 on 8 and 39 DF,  p-value: 4.857e-13]

}

{
  lm.fit=lm(survival~.-gender,data=train)
  summary(lm.fit) #train R^2 
  
  #lm.pred.train=predict(lm.fit,train)
  #lm.pred.train
  #mean((lm.pred.train-train$survival)^2) #train MSE
  
  lm.pred.test=predict(lm.fit,test)
  lm.pred.test
  mean((lm.pred.test-test$survival)^2) #test MSE
  
  plot(lm.fit,which=1) #detected funnnel/heteroskedasticity
  ------------------------------------------------
    lm.fit.test=lm(survival~.-gender,data=test)
  summary(lm.fit.test) #test R^2  [Multiple R-squared:  0.8355,	Adjusted R-squared:  0.8067 ]
  
}
plot(lm.fit,which=1)
plot(lm.fit,which=3)

{
  lm.fit=lm(log(survival)~bldclot+I(bldclot^2)+prog+I(prog^2)+enzy+I(enzy^2)+liverfunc+age+gender+alco,data=train)
  summary(lm.fit) #train R^2
  lm.pred.train=predict(lm.fit,train)
  lm.pred.train
  mean((lm.pred.train-train$survival)^2) #train MSE
  
  lm.pred.test=predict(lm.fit,test)
  lm.pred.test
  mean(((lm.pred.test)-log(test$survival))^2) #test MSE
  
  
  ------------------------------------------------
    lm.fit.test=lm(survival~.,data=test)
  summary(lm.fit.test) #test R^2
}


{
  lm.fit=lm(survival~bldclot+log(bldclot)+prog+log(prog)+enzy+log(enzy)+liverfunc+age+gender+alco,data=train)
  summary(lm.fit) #train R^2
  lm.pred.train=predict(lm.fit,train)
  lm.pred.train
  mean((lm.pred.train-train$survival)^2) #train MSE
  
  lm.pred.test=predict(lm.fit,test)
  lm.pred.test
  mean((lm.pred.test-test$survival)^2) #test MSE
  
  
  ------------------------------------------------
    lm.fit.test=lm(survival~.,data=test)
  summary(lm.fit.test) #test R^2
}

{
  lm.fit=lm(survival~bldclot+log(bldclot)+prog+log(prog)+enzy+log(enzy)+liverfunc+age+gender+alco,data=train)
  summary(lm.fit) #train R^2
  lm.pred.train=predict(lm.fit,train)
  lm.pred.train
  mean((lm.pred.train-train$survival)^2) #train MSE
  
  lm.pred.test=predict(lm.fit,test)
  lm.pred.test
  mean((lm.pred.test-test$survival)^2) #test MSE
  
  lm.pred.test=predict(lm.fit,test)
  lm.pred.test
  mean((lm.pred.test-test$survival)^2) #test MSE
  
  ------------------------------------------------
    lm.fit.test=lm(survival~.,data=test)
  summary(lm.fit.test) #test R^2
}

{
  lm.fit=lm(log(survival)~.,data=train)
  summary(lm.fit) #train R^2
  
  lm.pred.train=predict(lm.fit,train)
  mean((lm.pred.train-log(train$survival))^2) #train MSE in log scale
  lm.pred.test=predict(lm.fit,train)
  mean((exp(lm.pred.test)-train$survival)^2) #train MSE
  lm.pred.test=predict(lm.fit,test)
  mean((lm.pred.test-log(test$survival))^2) #test MSE in log scale
  lm.pred.test=predict(lm.fit,test)
  mean((exp(lm.pred.test)-test$survival)^2) #test MSE
  
  plot(lm.fit,which=1)
  plot(lm.fit,which=3)

  #test R^2
  lm.fit.test=lm(log(survival)~.,data=test)
  summary(lm.fit.test) #test R^2
}

cor(train[,-c(6,7)])
pairs(train)

{
  lm.fit=lm(log(survival)~.-bldclot-age+gender:enzy +enzy:bldclot+alco:prog,data=train)
  summary(lm.fit) #train R^2
  plot(lm.fit,which=1)
  lm.fit.test=lm(log(survival)~.-bldclot-age+gender:enzy +enzy:bldclot+alco:prog,data=test)
  summary(lm.fit.test) #test R^2
  
  #library(car)
  #vif(lm.fit)
  lm.pred.train=predict(lm.fit,train)
  mean((lm.pred.train-log(train$survival))^2) #train MSE
  lm.pred.test=predict(lm.fit,test)
  mean((lm.pred.test-log(test$survival))^2) #test MSE in log scale
  lm.pred.test=predict(lm.fit,test)
  mean((exp(lm.pred.test)-test$survival)^2) #test MSE 
  
  plot(lm.fit,which=1)
  plot(lm.fit,which=2)

}


plot(train$survival,prog)
