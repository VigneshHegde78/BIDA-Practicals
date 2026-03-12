install.packages(c("dplyr", "mlbench", "caTools", "ROCR", "caret", "ggplot2"))
library(dplyr)
library(mlbench)
library(caTools)
library(ROCR)
library(caret)
library(ggplot2)

data("PimaIndiansDiabetes")
data1 = PimaIndiansDiabetes

# Convert target variable to binary factor (Added ignore.case to prevent case sensitivity bugs)
data1$diabetes = gsub("Pos", "1", data1$diabetes, ignore.case = TRUE)
data1$diabetes = gsub("Neg", "0", data1$diabetes, ignore.case = TRUE)
data1$diabetes = as.factor(data1$diabetes)
summary(data1)

# attach(data1) # Note: Using attach() is generally discouraged as it can cause variable masking.

# Train/Test Split
set.seed(123) # Good practice to set a seed for reproducibility
split <- sample.split(data1$diabetes, SplitRatio = 0.8)

# FIXED: 'True' and 'False' must be boolean/logical (TRUE / FALSE), not strings
train_reg <- subset(data1, split == TRUE) 
test_reg <- subset(data1, split == FALSE)

# Training Model
# FIXED: Added 'data = train_reg' so it actually trains on the split!
logistic_model <- glm(diabetes ~ pregnant + glucose + pressure + triceps + insulin + mass + pedigree + age, 
                      data = train_reg, 
                      family = "binomial")
summary(logistic_model) 

# Predict test data based on model
test_reg$predict_reg <- predict(logistic_model, test_reg, type = "response")

# Convert probabilities to 0 or 1
test_reg$predict_reg1 <- ifelse(test_reg$predict_reg > 0.5, 1, 0)

# Plot logistic regression curve
ggplot(test_reg, aes(x=pregnant + glucose + pressure + triceps + insulin + mass + pedigree + age, y=predict_reg1)) + 
  geom_point(alpha=.5) + 
  stat_smooth(method = "glm", se=FALSE, method.args = list(family = binomial))

# Evaluate model accuracy using confusion matrix
confmat = table(test_reg$diabetes, test_reg$predict_reg1)
confusionMatrix(confmat, positive = "1", mode = "everything")

# Deriving ROC - AUC
RocPred <- prediction(test_reg$predict_reg1, test_reg$diabetes)
ROCPer <- performance(RocPred, measure = "tpr", x.measure = "fpr")

auc <- performance(RocPred, measure = "auc")
auc <- auc@y.values[[1]]
print(paste("AUC:", auc))

# Plotting ROC curve
# FIXED: Typo "calorize" changed to "colorize"
plot(ROCPer, colorize = TRUE, print.cutoffs.at = seq(0.1, by = 0.1), main = "ROC CURVE")
abline(a = 0, b = 1)

auc <- round(auc, 4)
# FIXED: legend syntax improved for clarity
legend(0.6, 0.4, legend = paste("AUC =", auc), title = "AUC", cex = 1)