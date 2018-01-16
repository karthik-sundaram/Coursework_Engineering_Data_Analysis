attach(Auto)
lm.fit=lm(mpg~I(horsepower^0.5),data=Auto)
summary(lm.fit)
{plot(Auto$horsepower^0.5,Auto$mpg,pch=20,col="black")
  abline(lm.fit,lwd=3,col='red')} 
predict(lm.fit,data.frame(horsepower=98),interval="confidence")
predict(lm.fit,data.frame(horsepower=98),interval="prediction")
plot(lm.fit,which=3)