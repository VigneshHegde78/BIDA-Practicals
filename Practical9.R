install.packages("dplyr")
install.packages("mlbench")
install.packages("caTools")
install.packages("ROCR")
install.packages("caret")

library(dplyr)
library(mlbench)
library(caTools)
library(ROCR)
library(caret)
library(ggplot2)
data("PimaIndiansDiabetes")
?PimaIndiansDiabetes
View(PimaIndiansDiabetes)
data1 = PimaIndiansDiabetes
str(data1)
summary(data1)
names(data1)
data1$diabetes = gsub("Pos", "1", data1$diabetes)
data1$diabetes = gsub("Neg", "0", data1$diabetes)
data1$diabetes = as.factor(data1$diabetes)
summary(data1)
attach(data1)

split <- sample.split(data1,SplitRatio = 0.8) # split data at 80%
split

train_reg <- subset(data1,split == "True") # new dataset with 80% data
test_reg <- subset(data1,split == "False") # new dataset with 20% data

#Training Model
logistic_model <- glm(diabetes ~ pregnant + glucose + pressure + triceps + insulin + mass + pedigree + age, family = "binomial")
logistic_model

summary(logistic_model) # summary of the model


# Predict test data based on model
test_reg$predict_reg <- predict(logistic_model, test_reg, type = "response")

View(test_reg)


# Based on probabilities derived, convert to 0 or 1 i.e No or Yes
test_reg$predict_reg1 <- ifelse(test_reg$predict_reg >0.5, 1, 0)
View(test_reg)

# Plot logistic regression (sigmoid) curve
ggplot(test_reg, aes(x=pregnant + glucose + pressure + triceps + insulin + mass + pedigree + age, y=predict_reg1)) + 
  geom_point(alpha=.5)+ stat_smooth(method = "glm",se=FALSE, method.args = list(family = binomial))

# Evaluatng model accuracy using confusion matrix
confmat = table(test_reg$diabetes, test_reg$predict_reg1)
confusionMatrix(confmat, positive = "1", mode = "everything")

# Deriving ROC - AUC
RocPred <- prediction(test_reg$predict_reg1, test_reg$diabetes)
ROCPer <- performance(RocPred, measure = "tpr", x.measure = "fpr")

auc <- performance(RocPred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# Plotting ROC curve
plot(ROCPer)
plot(ROCPer, calorize = TRUE, print.cutoffs.at = seq(0.1, by = 0.1), main = "ROC CURVE")
abline(a = 0, b = 1)


auc <- round(auc, 4)
legend(.6, .4, auc, title = "AUC",cex = 1)