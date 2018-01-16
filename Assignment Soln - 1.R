
rm(list = ls())  # Clean up
set.seed(1)
a<-rnorm(25,0,1)
x=matrix(a,nrow=5,ncol=5,byrow=TRUE)
y=matrix(a,nrow=5,ncol=5,byrow=FALSE)


h<-rnorm(100,0,1)
hist(h)

install.packages("ISLR")
Auto
?Auto

?pairs
pairs(~mpg+displacement+horsepower+weight+acceleration,data=Auto, 
      main="Simple Scatterplot Matrix")
plot(Auto$mpg~Auto$horsepower)
abline(lm(Auto$mpg~Auto$horsepower),col="red")