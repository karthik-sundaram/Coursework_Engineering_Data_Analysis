library(ISLR)
attach(Default)

#Q1a
log.fit1=glm(default~income+balance,family = binomial)
summary(log.fit1)


#Q1b      
str(Default)

set.seed(1)
train=sample(10000,5000)
log.fit=glm(default~income+balance,family = binomial,subset=train)
summary(log.fit)
log.prob=predict(log.fit,Default[-train,])
log.pred=rep("No",length(log.prob))
log.pred[log.prob>0.5]="Yes"
default.test=default[-train]
table(log.pred,default.test)
print(1-mean(log.pred==default.test))


#Q1c
set.seed(2)
train=sample(10000,5000)
log.fit=glm(default~income+balance,family = binomial,subset=train)
summary(log.fit)
log.prob=predict(log.fit,Default[-train,])
log.pred=rep("No",length(log.prob))
log.pred[log.prob>0.5]="Yes"
default.test=default[-train]
table(log.pred,default.test)
print(1-mean(log.pred==default.test))

set.seed(3)
train=sample(10000,5000)
log.fit=glm(default~income+balance,family = binomial,subset=train)
summary(log.fit)
log.prob=predict(log.fit,Default[-train,])
log.pred=rep("No",length(log.prob))
log.pred[log.prob>0.5]="Yes"
default.test=default[-train]
table(log.pred,default.test)
print(1-mean(log.pred==default.test))

set.seed(4)
train=sample(10000,5000)
log.fit=glm(default~income+balance,family = binomial,subset=train)
summary(log.fit)
log.prob=predict(log.fit,Default[-train,])
log.pred=rep("No",length(log.prob))
log.pred[log.prob>0.5]="Yes"
default.test=default[-train]
table(log.pred,default.test)
print(1-mean(log.pred==default.test))








#Q1d
log.fit2=glm(default~income+balance+student,family = binomial)
summary(log.fit2)

set.seed(1)
train=sample(10000,5000)
log.fit=glm(default~income+balance+student,family = binomial,subset=train)
summary(log.fit)
log.prob=predict(log.fit,Default[-train,])
log.pred=rep("No",length(log.prob))
log.pred[log.prob>0.5]="Yes"
default.test=default[-train]
table(log.pred,default.test)
print(1-mean(log.pred==default.test))



set.seed(2)
train=sample(10000,5000)
log.fit=glm(default~income+balance+student,family = binomial,subset=train)
summary(log.fit)
log.prob=predict(log.fit,Default[-train,])
log.pred=rep("No",length(log.prob))
log.pred[log.prob>0.5]="Yes"
default.test=default[-train]
table(log.pred,default.test)
print(1-mean(log.pred==default.test))


set.seed(3)
train=sample(10000,5000)
log.fit=glm(default~income+balance+student,family = binomial,subset=train)
summary(log.fit)
log.prob=predict(log.fit,Default[-train,])
log.pred=rep("No",length(log.prob))
log.pred[log.prob>0.5]="Yes"
default.test=default[-train]
table(log.pred,default.test)
print(1-mean(log.pred==default.test))


set.seed(4)
train=sample(10000,5000)
log.fit=glm(default~income+balance+student,family = binomial,subset=train)
summary(log.fit)
log.prob=predict(log.fit,Default[-train,])
log.pred=rep("No",length(log.prob))
log.pred[log.prob>0.5]="Yes"
default.test=default[-train]
table(log.pred,default.test)
print(1-mean(log.pred==default.test))









#Q2a
set.seed(10)
x=rnorm(200)
y=x-2*x^2+rnorm(200)

 #Q2b
plot(x,y)

#Q2c

df=data.frame(x,y)
library(boot)

#model (i)

reg.fit=lm(y~x,data=df)
summary(reg.fit)

set.seed(12)

cv.error=rep(0,4)
for (i in 1:4)
{
  reg.fit=glm(y~poly(x,i),data=df)
  summary(reg.fit)
  cv.error[i]=cv.glm(df,reg.fit)$delta[1]
}
cv.error
plot(cv.error)


set.seed(13)
cv.err=cv.glm(df,reg.fit)
cv.err$delta

cv.error=rep(0,4)
for (i in 1:4)
{
  reg.fit=glm(y~poly(x,i),data=df)
  cv.error[i]=cv.glm(df,reg.fit)$delta[1]
}
cv.error


#Q2f
set.seed(12)
cv.error.5=rep(0,4)
for(i in 1:4)
{glm.fit=glm(y~poly(x,i),data=df)
  cv.error.5[i]=cv.glm(df,glm.fit,K=5)$delta[1]
}
cv.error.5
  
#Q2g
set.seed(12)
cv.error.10=rep(0,4)
for(i in 1:4)
{glm.fit=glm(y~poly(x,i),data=df)
cv.error.10[i]=cv.glm(df,glm.fit,K=10)$delta[1]
}
cv.error.10

