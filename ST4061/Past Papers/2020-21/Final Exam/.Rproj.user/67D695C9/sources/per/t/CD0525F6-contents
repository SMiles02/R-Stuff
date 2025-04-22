
# (a) and (b)
dat = read.csv(file="dodgysales.csv", stringsAsFactors=TRUE)
n = nrow(dat)
P = ncol(dat) - 1  # Number of predictors

dat.train = dat[i.train,]
dat.validation = dat[-i.train,]

# (c) i
# Min-max normalisation
minmax = function(x) (x - min(x)) / (max(x) - min(x))
dat.s = as.data.frame(lapply(dat, function(x) if(is.numeric(x)) minmax(x) else x))
summary(dat.s$Sales)
summary(dat.s$BudgOp)
table(dat.s$Training)

# Train-validation split
set.seed(6041)
i.train = sample(1:n, floor(.7 * n))
dat.s.train = dat.s[i.train,]
dat.s.validation = dat.s[-i.train,]

# (c) ii: Fit neural networks
library(nnet)
set.seed(6041)
nn3 = nnet(Sales ~ ., data = dat.s.train, size = 3, linout = TRUE)
nn8 = nnet(Sales ~ ., data = dat.s.train, size = 8, linout = TRUE)

mean(nn3$residuals^2)  # Training MSE for 3 neurons
mean(nn8$residuals^2)  # Training MSE for 8 neurons

# (c) iii: Validation MSEs
ytest = dat.s.validation$Sales
p3 = predict(nn3, dat.s.validation)
p8 = predict(nn8, dat.s.validation)

mean((p3 - ytest)^2)
mean((p8 - ytest)^2)

# (d) Gradient Boosting Model
library(gbm)
set.seed(6041)
gbmo = gbm(Sales ~ ., data = dat.train, n.trees = 100)
gbmp = predict(gbmo, newdata = dat.validation, n.trees = 100)
mean((gbmo$fit - dat.train$Sales)^2)       # Training MSE
mean((gbmp - dat.validation$Sales)^2)      # Validation MSE

# (e) GLM
set.seed(6041)
glmo = glm(Sales ~ ., data = dat.train)
glmp = predict(glmo, dat.validation)
mean((glmo$fitted.values - dat.train$Sales)^2)
mean((glmp - dat.validation$Sales)^2)

# (f) Ridge Regression
library(glmnet)
x = model.matrix(Sales ~ ., dat.train)[, -1]
y = dat.train$Sales
x.test = model.matrix(Sales ~ ., dat.validation)[, -1]
y.test = dat.validation$Sales

set.seed(6041)
ridge.fit = glmnet(x, y, alpha = 0)
ridgep = predict(ridge.fit, s = 0.01, newx = x.test)

mean((predict(ridge.fit, s = 0.01, newx = x) - y)^2)  # Training
mean((ridgep - y.test)^2)  # Validation

# (g) Compare errors with scaling awareness
# Reverse scaling prediction if needed for NN

# >> compare with NN.... WATCH OUT FOR SCALING!!
# pov = po*(max(dat$Sales)-min(dat$Sales))+min(dat$Sales)
# mean((pov-dat.validation$Sales)^2)
# comparable errors....

# (h) RFE with random forest
library(caret)
set.seed(6041)
control = rfeControl(functions = rfFuncs, method = "cv", number = 10)
fit.rfe = rfe(dat.train[, -1], dat.train$Sales, sizes = c(1:P), rfeControl = control)
fit.rfe$optVariables

# (i) RFE with stepwise logistic regression
set.seed(6041)
control.glm = rfeControl(functions = lrFuncs, method = "cv", number = 10)
fit.glm.rfe = rfe(dat.train[, -1], dat.train$Sales, sizes = c(1:P), rfeControl = control.glm)
fit.glm.rfe$optVariables

# (j) Open to interpretation
