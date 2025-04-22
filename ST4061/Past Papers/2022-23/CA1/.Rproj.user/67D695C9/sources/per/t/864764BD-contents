library(MASS)
library(caret)
ca.train = read.csv("ca_train.csv",stringsAsFactors=TRUE)
ca.test = read.csv("ca_test.csv",stringsAsFactors=TRUE)

# 1
# Fit an LDA model using all predictors in the training data to predict 'y'
lda.o = lda(y ~ ., data = ca.train)  # . means "use all other variables as predictors"
# Use the fitted LDA model to predict class labels for the test data
lda.p = predict(lda.o, newdata = ca.test)$class  # Extract only the predicted class labels
# Generate and display the confusion matrix comparing predicted vs actual labels
(ltb = confusionMatrix(lda.p, ca.test$y)$table)  # Returns a cross-tabulation of predictions vs truth


# 2
qda.o = lda(y~., data=ca.train)
qda.p = predict(qda.o, newdata=ca.test)$class
(qtb = confusionMatrix(qda.p, ca.test$y)$table)

ltb[1, 1]
ltb[, 1]

`# 3
# Specificity = how well the model identifies actual negatives
# Answer: both models are highly specific, it is incorrect to say one is more specific/accurate than the other
ltb[1, 1] / sum(ltb[, 1])
qtb[1, 1] / sum(qtb[, 1])