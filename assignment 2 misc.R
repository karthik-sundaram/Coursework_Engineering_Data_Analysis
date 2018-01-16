fix(Auto)
lm.fit=lm(mpg~horsepower+log(horsepower),data=Auto) 
summary(lm.fit)
predict(lm.fit,data.frame(horsepower=98),interval="confidence")
predict(lm.fit,data.frame(horsepower=98),interval="prediction")

plot(Auto$horsepower,Auto$mpg,pch=20,col="black")
lines(lowess(Auto$mpg~Auto$horsepower),col="red",lwd=3)


plot(lm.fit)

plot(lm.fit,which=1)
plot(lm.fit,which=5)
plot(lm.fit,which=2)
plot(lm.fit,which=3)
plot(lm.fit,which=4)

which.max(hatvalues(lm.fit))
plot(lm.fit)



lm.fit2=lm(mpg~horsepower+I(horsepower^2),data=Auto) 
summary(lm.fit2)
predict(lm.fit2,data.frame(horsepower=98),interval="confidence")
predict(lm.fit2,data.frame(horsepower=98),interval="prediction")
plot(Auto$horsepower,Auto$mpg,pch=20,col="black")

lines(Auto$horsepower,Auto$mpg,lwd=3,col="red",type="l")
plot(Auto$horsepower, fitted(lm(Auto$mpg~Auto$horsepower+I(Auto$horsepower^2))), type="l")

plot(Auto$horsepower~Auto$mpg)
lines(lowesss(Auto$horsepower~Auto$mpg))
lines(predict(lm(Auto$mpg~Auto$horsepower+I(Auto$horsepower^2))))

lines(Auto$horsepower,predict(lm(mpg~horsepower+I(horsepower^2),data=Auto)))