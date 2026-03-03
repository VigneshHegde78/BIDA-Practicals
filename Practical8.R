weight = c(63, 64, 66, 69, 71, 71, 72, 73, 75, 29)
height = c(127, 121, 142, 157, 162, 156, 169, 165, 181, 208)

mydata = data.frame(height, weight)
m1 = lm(formula = weight ~ height, data = mydata)
m1
summary(m1)

plot(height, weight, main = "Best fit line")
abline(m1, col = "Purple")
a = data.frame(height = 170)
result = predict(m1, newdata = a)
print(result)
