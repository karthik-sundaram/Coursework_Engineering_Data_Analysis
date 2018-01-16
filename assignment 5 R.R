library(ISLR)    
attach(College)
names(College)
dim(College)
sum(is.na(College$Apps))

install.packages("leaps")
library(leaps)
regfit.full=regsubsets(Apps~.,data=College,nvmax=17)
reg.summary=summary(regfit.full)

par(mfrow=c(2,2))
plot(reg.summary$cp,xlab="No. of predictors",ylab="Cp")
which.min(reg.summary$cp)
points(12,reg.summary$cp[12],col="red",cex=2,pch=20)

plot(reg.summary$bic,xlab="No. of predictors",ylab="BIC")
which.min(reg.summary$bic)
points(10,reg.summary$bic[10],col="red",cex=2,pch=20)


plot(reg.summary$adjr2,xlab="No. of predictors",ylab="Adjusted Rsq")
which.max(reg.summary$adjr2)
points(13,reg.summary$adjr2[13],col="red",cex=2,pch=20)


coef(regfit.full,10)
coef(regfit.full,12)
coef(regfit.full,13)


#(b)
regfit.fwd=regsubsets(Apps~.,data=College,nvmax=17,method="forward")
reg.summary2=summary(regfit.fwd)

par(mfrow=c(2,2))
plot(reg.summary2$cp,xlab="No. of predictors",ylab="Cp")
which.min(reg.summary2$cp)
points(12,reg.summary2$cp[12],col="red",cex=2,pch=20)


plot(reg.summary2$bic,xlab="No. of predictors",ylab="BIC")
which.min(reg.summary2$bic)
points(10,reg.summary2$bic[10],col="red",cex=2,pch=20)


plot(reg.summary2$adjr2,xlab="No. of predictors",ylab="Adjusted Rsq")
which.max(reg.summary2$adjr2)
points(13,reg.summary2$adjr2[13],col="red",cex=2,pch=20)


coef(regfit.full,10)




regfit.bwd=regsubsets(Apps~.,data=College,nvmax=17,method="backward")
reg.summary2=summary(regfit.bwd)

par(mfrow=c(2,2))
plot(reg.summary2$cp,xlab="No. of predictors",ylab="Cp")
which.min(reg.summary2$cp)
points(12,reg.summary2$cp[12],col="red",cex=2,pch=20)


plot(reg.summary2$bic,xlab="No. of predictors",ylab="BIC")
which.min(reg.summary2$bic)
points(10,reg.summary2$bic[10],col="red",cex=2,pch=20)


plot(reg.summary2$adjr2,xlab="No. of predictors",ylab="Adjusted Rsq")
which.max(reg.summary2$adjr2)
points(13,reg.summary2$adjr2[13],col="red",cex=2,pch=20)

coef(regfit.full,10)

#c
install.packages("glmnet")
library(glmnet)

x=model.matrix(Apps~.,data=College)[,-1]
y=College$Apps


#set.seed(1)
#train=sample(1:nrow(x),nrow(x)/2)
#test=(-train)
#y.test=y[test]

grid=10^seq(10,-2,length=100)

#lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
#dim(coef(lasso.mod))

lasso.mod=glmnet(x,y,alpha=1,lambda=grid)
dim(coef(lasso.mod))

set.seed(1)
#cv.out=cv.glmnet(x[train,],y[train],alpha=1)
cv.out=cv.glmnet(x,y,alpha=1)

plot(cv.out)
bestlam=cv.out$lambda.min
bestlam

out=glmnet(x,y,alpha=1)
lasso.coeff=predict(out,type="coefficients",s=bestlam)[1:18,]
lasso.coeff
lasso.coeff[lasso.coeff!=0]


#d

x=model.matrix(Apps~.,College)[,-1]
y=College$Apps


#set.seed(1)
#train=sample(1:nrow(x),nrow(x)/2)
#test=(-train)
#y.test=y[test]

grid=10^seq(10,-2,length=100)

#lasso.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid)
lasso.mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(lasso.mod))


set.seed(1)
#cv.out=cv.glmnet(x[train,],y[train],alpha=0)
cv.out=cv.glmnet(x,y,alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam

out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:18,]


#e

#e-i)
set.seed(1)
train=sample(777,388)
regfit.full=regsubsets(Apps~.,data=College[train,],nvmax=17)
reg.summary=summary(regfit.full)

#Cp

 
which.min(reg.summary$cp)
coef(regfit.full,id=10)

lm_bestfit=lm(Apps~Private+Accept+Enroll+Top10perc+Top25perc+Outstate+Room.Board+PhD+Expend+Grad.Rate,data=College,subset=train)
lm_pred=predict(lm_bestfit,College[-train,])
test_error_cp=mean((Apps[-train]-lm_pred)^2)
test_error_cp

#BIC
 
which.min(reg.summary$bic)
coef(regfit.full,id=7)

lm_bestfit=lm(Apps~Private+Accept+Enroll+Top10perc+Top25perc+Outstate+Room.Board,data=College,subset=train)
lm_pred=predict(lm_bestfit,College[-train,])

test_error_bic=mean((Apps[-train]-lm_pred)^2)
test_error_bic

#Adj R^2


which.max(reg.summary$adjr2)
coef(regfit.full,id=10)

lm_bestfit=lm(Apps~Private+Accept+Enroll+Top10perc+Top25perc+Outstate+Room.Board+PhD+Expend+Grad.Rate,data=College,subset=train)
lm_pred=predict(lm_bestfit,College[-train,])

test_error_adjr2=mean((Apps[-train]-lm_pred)^2)
test_error_adjr2

#e-ii)
x=model.matrix(Apps~.,data=College)[,-1]
y=College$Apps


set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]

#cross-validation
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
#plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
#grid=10^seq(10,-2,length=100)

#train data fitting
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=bestlam)
#dim(coef(lasso.mod))

#test prediction
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)


lasso.coeff=predict(lasso.mod,type="coefficients",s=bestlam)[1:18,]
lasso.coeff

#e-iii)
x=model.matrix(Apps~.,data=College)[,-1]
y=College$Apps


set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]

#cross-validation
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
#plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
#grid=10^seq(10,-2,length=100)

#train data fitting
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=bestlam)
#dim(coef(lasso.mod))

#test prediction
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)


