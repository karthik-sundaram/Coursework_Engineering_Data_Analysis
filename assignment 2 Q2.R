fix(Auto)
pairs(~.,data=Auto,main="Simple Scatterplot Matrix")
data(Auto)
Auto$name<-NULL
cor(Auto)

lm.fit3<-lm(mpg~.,data=Auto)
summary(lm.fit3)
plot(lm.fit3,which=1)
plot(lm.fit3,which=5)
plot(lm.fit3,which=2)
plot(lm.fit3,which=3)
plot(lm.fit3,which=4)
install.packages("VIF")
library(car)
vif(lm.fit3)

lm.fit4 = lm(mpg ~displacement*cylinders*horsepower*weight*acceleration*year*origin, data = Auto)
summary(lm.fit)


model1.1 = lm(mpg ~.+displacement:weight, data = Auto)
summary(model1.1)
model1.2 = lm(mpg ~.+displacement:cylinders+displacement:weight+year:origin+acceleration:horsepower, data=Auto)
summary(model1.2)
model1.3 = lm(mpg ~.+year:origin+displacement:weight+acceleration:horsepower+acceleration:weight, data=Auto)
summary(model1.3)