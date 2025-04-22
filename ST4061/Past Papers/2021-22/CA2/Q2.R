# Load required libraries
library(randomForest)  # For Random Forest model
library(gbm)           # For Gradient Boosting Machine model

# Load the dataset
dat = read.csv(file="CA2_2021-22.dat", stringsAsFactors=TRUE)

# (1) Check for high correlation among numerical features
M = round(cor(dat), 3)    # Compute the correlation matrix, round to 3 decimals
diag(M) = 0               # Set diagonal to 0 to ignore self-correlations
max(abs(M))               # Find the highest absolute off-diagonal correlation value
# → If > 0.9, look at the matrix M to see which variables are strongly correlated

# (3) If performing log-transformed Sale_Price modelling:
# dat$Sale_Price = log(dat$Sale_Price)

# (2) 10-fold cross-validation setup
n = nrow(dat)                             # Number of observations
K = 10                                    # Number of folds
folds = cut(1:n, K, labels=FALSE)         # Create fold indices
set.seed(4061)                            # Set seed for reproducibility

# Initialise RMSE storage vectors
# rmse.*: untransformed Sale_Price models
# rmse.*.os: models trained on log(Sale_Price) but RMSE converted back to original Sale_Price scale
rmse.glm = rmse.gbm = rmse.rf = numeric(K)
rmse.glm.os = rmse.gbm.os = rmse.rf.os = numeric(K)

# Start cross-validation loop
for(k in 1:K){
  # Split data into training and test sets
  itrain = which(folds != k)
  dtrain = dat[itrain,]
  dtest = dat[-itrain,]
  
  # GLM Model
  glmo = glm(Sale_Price ~ ., data=dtrain)                      # Fit GLM using all predictors
  glmo.p = predict(glmo, newdata=dtest, type="response")       # Predict on test data
  rmse.glm[k] = sqrt(mean((glmo.p - dtest$Sale_Price)^2))      # RMSE in untransformed scale
  rmse.glm.os[k] = sqrt(mean((exp(glmo.p) - exp(dtest$Sale_Price))^2))  # RMSE if trained on log scale
  
  # Random Forest Model
  rfo = randomForest(Sale_Price ~ ., data=dtrain)              # Fit RF model
  rfo.p = predict(rfo, newdata=dtest)                          # Predict on test data
  rmse.rf[k] = sqrt(mean((rfo.p - dtest$Sale_Price)^2))        # RMSE in untransformed scale
  rmse.rf.os[k] = sqrt(mean((exp(rfo.p) - exp(dtest$Sale_Price))^2))    # RMSE if trained on log scale
  
  # Gradient Boosting Machine (GBM) Model
  gbmo = gbm(Sale_Price ~ ., data=dtrain, distribution="gaussian")  # Fit GBM with Gaussian loss
  gbmo.p = predict(gbmo, newdata=dtest, n.trees=100)                # Predict using first 100 trees
  rmse.gbm[k] = sqrt(mean((gbmo.p - dtest$Sale_Price)^2))           # RMSE in untransformed scale
  rmse.gbm.os[k] = sqrt(mean((exp(gbmo.p) - exp(dtest$Sale_Price))^2))  # RMSE if trained on log scale
}

# (2b) Boxplot of test RMSEs for the models trained on untransformed Sale_Price
boxplot(rmse.glm, rmse.rf, rmse.gbm,
        names = c("GLM", "RF", "GBM"),
        main = "Test RMSEs (Original Sale_Price Scale)")

# (2a) Mean test RMSEs for untransformed Sale_Price
c(mean(rmse.glm), mean(rmse.rf), mean(rmse.gbm))

# (3b) Boxplot of test RMSEs for log-transformed Sale_Price (converted back to original scale)
boxplot(rmse.glm.os, rmse.rf.os, rmse.gbm.os,
        names = c("GLM_log", "RF_log", "GBM_log"),
        main = "Test RMSEs (log(Sale_Price) Models, RMSE Back on Original Scale)")

# (3a) Mean test RMSEs for log-transformed Sale_Price (converted back to original scale)
c(mean(rmse.glm.os), mean(rmse.rf.os), mean(rmse.gbm.os))

# (Optional) Feature importance from Random Forest
io = order(importance(rfo), decreasing = TRUE)       # Rank features by importance
cbind(importance(rfo)[io,])                          # Print sorted feature importance

# (Optional) Summary of the GBM model – includes variable influence
summary(gbmo)
