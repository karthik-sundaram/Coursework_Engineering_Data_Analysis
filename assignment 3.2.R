attach(Auto)

mpg01=rep(0,length(mpg))
mpg01[mpg>median(mpg)]=1
df=data.frame(Auto,mpg01)

pairs(df[,-9])

cor(df[,-9])

boxplot(weight ~ mpg01, data = Auto, main = "Weight vs mpg01")
boxplot(displacement ~ mpg01, data = Auto, main = "Displacement vs mpg01")
boxplot(horsepower ~ mpg01, data = Auto, main = "Hosepower vs mpg01")
boxplot(acceleration ~ mpg01, data = Auto, main = "Acceleration vs mpg01")
boxplot(year ~ mpg01, data = Auto, main = "Year vs mpg01")
boxplot(origin ~ mpg01, data = Auto, main = "Origin vs mpg01")
boxplot(cylinders ~ mpg01, data = Auto, main = "Cylinders vs mpg01")

summary(df)

train.dat=year<79
test.dat=year>=79


library(MASS)
Auto_test=df[test.dat,]
mpg01_test=mpg01[test.dat]
log.fit=glm(mpg01~weight+displacement+horsepower+cylinders,data=df,family=binomial,subset=train.dat)
summary(log.fit)
log1.prob=predict(log.fit,Auto_test,type="response")
log1.pred=rep(0,length(log1.prob))
log1.pred[log1.prob>0.5]=1
table(log1.pred,mpg01_test)
mean(log1.pred==mpg01_test)


library(class)
Auto_test=df[test.dat,]
mpg01_test=mpg01[test.dat]
train.X=cbind(weight,displacement,horsepower,cylinders)[train.dat,]
test.X=cbind(weight,displacement,horsepower,cylinders)[test.dat,]
train.mpg01=mpg01[train.dat]

  knn.pred=knn(data.frame(train.X),data.frame(test.X),train.mpg01,k=1)
  table(knn.pred,mpg01_test)
  mean(knn.pred==mpg01_test)
  knn.pred=knn(data.frame(train.X),data.frame(test.X),train.mpg01,k=10)
  table(knn.pred,mpg01_test)
  mean(knn.pred==mpg01_test)
  knn.pred=knn(data.frame(train.X),data.frame(test.X),train.mpg01,k=20)
  table(knn.pred,mpg01_test)
  mean(knn.pred==mpg01_test)
  knn.pred=knn(data.frame(train.X),data.frame(test.X),train.mpg01,k=30)
  table(knn.pred,mpg01_test)
  mean(knn.pred==mpg01_test)
  knn.pred=knn(data.frame(train.X),data.frame(test.X),train.mpg01,k=40)
  table(knn.pred,mpg01_test)
  mean(knn.pred==mpg01_test)
  knn.pred=knn(data.frame(train.X),data.frame(test.X),train.mpg01,k=50)
  table(knn.pred,mpg01_test)
  mean(knn.pred==mpg01_test)
  knn.pred=knn(data.frame(train.X),data.frame(test.X),train.mpg01,k=100)
  table(knn.pred,mpg01_test)
  mean(knn.pred==mpg01_test)
  knn.pred=knn(data.frame(train.X),data.frame(test.X),train.mpg01,k=150)
  table(knn.pred,mpg01_test)
  mean(knn.pred==mpg01_test)
  knn.pred=knn(data.frame(train.X),data.frame(test.X),train.mpg01,k=200)
  table(knn.pred,mpg01_test)
  mean(knn.pred==mpg01_test)
  
  
  





