install.packages(c("tidyverse", "FactoMineR", "factoextra"))
library(tidyverse)
library(FactoMineR)
library(factoextra)

data(iris)
iris1 = iris[,-5]
iris_label = iris[,5]
set.seed(2321)
wss = NULL

for (i in 1:10) {
  fit = kmeans(iris1, centers = i)
  wss = c(wss, fit$tot.withinss)
}

# FIXED: type = "0" (zero) changed to type = "o" (the letter O, for overplotted)
plot(1:10, wss, type = "o", xlab = "Number of Clusters", ylab = "Within groups sum of squares")

fviz_nbclust(iris1, kmeans, nstart=100, method="wss") + geom_vline(xintercept = 3, linetype = 1)

m2 = kmeans(iris1, 3)
print(m2$cluster)

# FIXED: Typo 'irsi$species' changed to 'iris$Species' (R is case-sensitive)
table(iris$Species, m2$cluster)

fviz_cluster(m2, data = iris1, geom = c("point"), ellipse.type = "euclid")