install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# Load and explore data
data(iris)
summary(iris)
table(iris$Species)

# Train/Test Split
set.seed(1212)
s = sample(c(1:150), size = 100)
iris.train = iris[s,]
iris.test = iris[-s,]

table(iris.train$Species)

# Build the model using the training data
m1 = rpart(Species ~ ., data = iris.train, method = "class")
print(m1)
rpart.plot(m1)

# Predict on the test data
iris.test$predict = predict(m1, iris.test, type = "class")

# Evaluate results
table(iris.test$Species, iris.test$predict)