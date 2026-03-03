install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

?iris
View(iris)
summary(iris)
table(iris$Species)

set.seed(1212)
s = sample(c(1:150), size = 100)
iris.train = iris[s,]
iris.test = iris[-s,]

table(iris.train$Species)
iris.train

?rpart
names(iris)
m1 = rpart(iris.train$Species~., data = iris.train, method = "class")
m1
rpart.plot(m1)
iris.test$predict = predict(m1, iris.test, type = "class")
View(iris.test)
table(iris.test$Species, iris.test$predict)

