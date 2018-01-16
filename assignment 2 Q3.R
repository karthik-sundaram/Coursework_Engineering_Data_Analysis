library(car)
data("Carseats")
fix(Carseats)
model1=lm(Sales~Price+Urban+US, data=Carseats)
summary(model1)
?Carseats

model2=lm(Sales~Price+US, data=Carseats)
summary(model2)
plot(model2) #on pressing enter, all the plots come one by one
plot(model2,which=4)


rm(list = ls())