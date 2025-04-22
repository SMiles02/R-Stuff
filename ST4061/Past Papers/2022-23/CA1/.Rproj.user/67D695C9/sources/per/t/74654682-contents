set.seed(4061)

library(ISLR)
library(gbm)
library(randomForest)

x.train = Khan$xtrain
x.test = Khan$xtest
y.train = as.factor(Khan$ytrain)
y.test = as.factor(Khan$ytest)

y.train
y.test

### (a) Tabular summary of class distribution in training and test sets

# Frequency of each class in the training set
table(y.train)

# Frequency of each class in the test set
table(y.test)

# → This helps us understand if the data is imbalanced or not.
# If class distributions are very uneven, it can affect model performance.
# Note: Differences between training and test distributions may suggest non-stratified sampling.

### (b) Nature of the problem

# This is a classification problem
# because the response variable y contains **categorical values** (class labels).

### (c) Fit a Random Forest model

# Train a random forest classifier using default hyperparameters
# Inputs: x.train = predictor variables; y.train = class labels
rfo = randomForest(x.train, y.train)

# Print model summary to check performance on training data
rfo
# → Includes OOB (Out-Of-Bag) error estimate, class error rates, and number of trees used

### (d) Test set prediction and accuracy

# Predict class probabilities on test data (optional, used if we want to evaluate uncertainty)
rfpr = predict(rfo, x.test, type = "prob")

# Predict actual class labels on test data
rfp = predict(rfo, x.test)

# Create a confusion matrix of predicted vs actual values
tb = table(rfp, y.test)

# Calculate test set accuracy
sum(diag(tb) / sum(tb))
# → Accuracy = (correct predictions) / (total predictions)
# → diag(tb) gives diagonal elements (true positives), sum(tb) gives total observations

### (e) Variable importance: features with importance > 0.4

# Identify variables with MeanDecreaseGini importance > 0.4
is = which(rfo$importance > 0.4)

# Get the names of the important features
rownames(rfo$importance)[is]

# (Optional) Plot the distribution of those importances
# hist(rfo$importance[is])
# barplot((rfo$importance[is,]), las=2)

### (f) Interpretation of variable importance

# Variable importance is measured in terms of a mean decrease of the Gini index. This metric evaluates the average drop in Gini index achieved whenever the corresponding feature is used in a split of a tree throughout a random forest. This indicates how much purer the nodes would become after using the variable to further partition the data.

# MeanDecreaseGini measures how much including a variable decreases the Gini impurity
# across all trees in the forest.
# Higher values indicate that the variable is more useful for improving class separation
# in the splits of decision trees.

### (g) Fit Gradient Boosting Model (GBM) and evaluate test accuracy

# Train GBM model on training data
# Use distribution = "multinomial" because it's a multi-class classification problem
gb.out = gbm(y.train ~ ., data = as.data.frame(x.train), distribution = "multinomial")

# Get fitted values on training set (not used here, but useful for diagnostics)
gb.fitted = predict(gb.out, n.trees = gb.out$n.trees)

# Predict on test set using the number of trees used in training
gb.pred = predict(gb.out, as.data.frame(x.test), n.trees = gb.out$n.trees)

# gb.pred returns a 3D array of probabilities for each class; we use which.max to pick the most likely class
gbp = apply(gb.pred, 1, which.max)

# Create a confusion matrix of actual vs predicted
tb = table(y.test, gbp)
tb

# Compute and print prediction accuracy
sum(diag(tb)) / sum(tb)

# → Again, this gives the percentage of correct predictions on the test set