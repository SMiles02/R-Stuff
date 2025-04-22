# Load required packages
library(mlbench)   # For the BreastCancer dataset
library(caret)     # For training models and evaluating performance

# Load the dataset and remove missing values
data(BreastCancer)
dat = na.omit(BreastCancer)
dat$Id = NULL  # Remove ID column as it's not a useful predictor

# Set seed for reproducibility and split into training/validation sets
set.seed(4061)
i.train = sample(1:nrow(dat), 600, replace=FALSE)
dat.train = dat[i.train,]
dat.validation = dat[-i.train,]

# Ensure predictors are numeric and response is a factor
# These lines were commented out because caret often handles this automatically
# dat.train[, 1:9] = lapply(dat.train[, 1:9], as.numeric)
# dat.validation[, 1:9] = lapply(dat.validation[, 1:9], as.numeric)
# dat.train$Class = as.factor(dat.train$Class)
# dat.validation$Class = as.factor(dat.validation$Class)

# (a) Fit a Random Forest model with 10-fold cross-validation

# Define 10-fold cross-validation control
ctrl = trainControl(method = "cv", number = 10)

# Train the Random Forest model using caret's train()
rf.model = train(
  Class ~ ., data = dat.train,
  method = "rf",        # Random Forest method
  trControl = ctrl      # Use 10-fold cross-validation
)

# Display trained model summary (includes mtry = number of variables tried at each split)
rf.model

# Generate predictions on the validation set
rf.pred = predict(rf.model, newdata = dat.validation)

# Compute confusion matrix and extract accuracy
rf.conf = confusionMatrix(rf.pred, dat.validation$Class)
rf.conf$overall['Accuracy']  # This is the test set prediction accuracy

# (b) Fit a Support Vector Machine with a Linear Kernel

svm.linear = train(
  Class ~ ., data = dat.train,
  method = "svmLinear",  # SVM with linear kernel
  trControl = ctrl       # 10-fold CV
)

# Predict and compute accuracy on validation set
svm.linear.pred = predict(svm.linear, newdata = dat.validation)
confusionMatrix(svm.linear.pred, dat.validation$Class)$overall['Accuracy']

# (c) Fit a Support Vector Machine with a Radial Basis Function (RBF) Kernel

svm.radial = train(
  Class ~ ., data = dat.train,
  method = "svmRadial",  # SVM with RBF (non-linear) kernel
  trControl = ctrl       # 10-fold CV
)

# Predict and compute accuracy on validation set
svm.radial.pred = predict(svm.radial, newdata = dat.validation)
confusionMatrix(svm.radial.pred, dat.validation$Class)$overall['Accuracy']

# (d) Model comparison explanation

# From GPT:
# Among the three models evaluated:
# Random Forest achieved a validation accuracy of 96.4%
# SVM with a linear kernel achieved a validation accuracy of 93.98%
# SVM with a radial basis kernel (RBF) achieved the highest validation accuracy of 97.59%
# Based on these results, the SVM with a radial basis kernel is deemed the best model for this task, as it achieved the highest test set prediction accuracy. This suggests that it is better able to capture the potentially non-linear relationships between the predictors and the tumour class. The linear SVM performed slightly worse, possibly due to its inability to model complex decision boundaries. While the random forest also performed well, it was outperformed slightly by the RBF SVM in terms of predictive accuracy.

# After fix prompt with answer key:
# While the SVM with radial basis kernel achieved the highest validation accuracy (97.6%), followed by random forest (96.4%) and linear SVM (93.98%), the differences between these accuracies are relatively small. Given the size of the validation set, the confidence intervals (CIs) around these accuracy estimates likely overlap, meaning we cannot confidently say that any one model is significantly better than the others.
# Therefore, no single model can be deemed definitively better based on validation accuracy alone.

# (e) Variable importance analysis

# Get variable importance for Random Forest (most reliable here)
importance1 = varImp(rf.model)
plot(importance1)
print(importance1)

# Get variable importance for linear SVM (based on absolute value of coefficients)
importance2 = varImp(svm.linear)
plot(importance2)
print(importance2)

# Attempt to get variable importance for radial SVM
# Note: This doesn't work meaningfully — caret will fallback to same method as linear SVM
importance3 = varImp(svm.radial)
plot(importance3)
print(importance3)

# So importance3 just replicates importance2 — for meaningful variable importance,
# use the random forest results from importance1.
