install.packages("leaps")
library(leaps)

library(ISLR)
Hitters=na.omit(Hitters)

regfit.full=regsubsets(Salary~.,Hitters)
regfwd.full=regsubsets(Salary~.,Hitters,nvmax=19,method="forward")
regbwd.full=regsubsets(Salary~.,Hitters,nvmax=19,method="backward")

reg.summary=summary(regfit.full)
reg.summary1=summary(regfwd.full)
reg.summary2=summary(regbwd.full)


plot(reg.summary$bic)
which.min(reg.summary$bic)

plot(reg.summary1$bic)
which.min(reg.summary1$bic)

plot(reg.summary2$bic)
which.min(reg.summary2$bic)


set.seed(1)
train=sample(c(TRUE,FALSE),nrow(Hitters),rep=TRUE)
test=!train
?Hitters

model.matrix(Salary~.,Hitters)[,-1]


?cv.glmnet