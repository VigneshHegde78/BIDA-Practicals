# Data definition
weight = c(63, 64, 66, 69, 71, 71, 72, 73, 75, 92)
height = c(127, 121, 142, 157, 162, 156, 169, 165, 181, 208)

mydata = data.frame(height, weight)

# Build the linear model
m1 = lm(formula = weight ~ height, data = mydata)
print(m1)
summary(m1)

# Plot the best fit line
plot(height, weight, main = "Best fit line")
abline(m1, col = "Purple")

# Predict weight for a new height
a = data.frame(height = 170)
result = predict(m1, newdata = a)
print(result)