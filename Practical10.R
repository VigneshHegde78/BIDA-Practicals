install.packages("tidyverse")
install.packages("FactoMineR")
install.packages("factoextra")


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
plot(1:10, wss, type = "0")
fviz_nbclust(iris1, kmeans, nstart=100, method="wss")+geom_vline(xintercept = 3, linetype = 1)
m2 = kmeans(iris1, 3)
View(m2)
m2$cluster
table(irsi$species, m2$cluster)
fviz_cluster(m2, data = iris1, geom = c("point"), ellipse.type = "euclid")