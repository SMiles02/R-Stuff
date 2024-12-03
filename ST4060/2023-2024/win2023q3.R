# Load necessary libraries
library(splines)  # For B-splines
library(MASS)     # For the Boston dataset

# Set the random seed
set.seed(4060)

# Load the data
x <- Boston$nox
y <- Boston$medv

# (a) Fit a B-spline to the data
# Define knots at the specified quantiles
knots <- quantile(x, probs = c(0.15, 0.40, 0.60, 0.70, 0.85))

# Fit a B-spline model
b_spline_model <- lm(y ~ bs(x, knots = knots))

# Display B-spline coefficient estimates
cat("B-spline coefficient estimates:\n")
print(coef(b_spline_model))

# (b) Generate predictions for new x values
newx <- c(0.4, 0.5, 0.6)
predicted_b_spline <- predict(b_spline_model, newdata = data.frame(x = newx))

cat("\nPredicted values for new x values using the B-spline:\n")
print(predicted_b_spline)

# (c) Fit a P-spline (smoothing spline) to the data
# Fit the smoothing spline
p_spline_model <- smooth.spline(x, y, cv = TRUE)

# (i) Quote the P-spline penalized criterion (RSS)
cat("\nP-spline penalized criterion (RSS):\n")
print(p_spline_model$cv.crit)

# (ii) Provide a plot comparing B-spline and P-spline
plot(x, y, main = "B-spline vs P-spline",
     xlab = "x (NOX)", ylab = "y (MEDV)", pch = 16, col = "black")
lines(x, predict(b_spline_model), col = "red", lwd = 2)
lines(p_spline_model, col = "blue", lwd = 2)
legend("topright", legend = c("B-spline (red)", "P-spline (blue)"), col = c("red", "blue"), lwd = 2)

# (d) Generate predictions for new x values from P-spline
predicted_p_spline <- predict(p_spline_model, x = newx)$y

cat("\nPredicted values for new x values using the P-spline:\n")
print(predicted_p_spline)

# Compare the predictions
cat("\nComparison of predictions (B-spline vs P-spline):\n")
comparison <- data.frame(
  newx = newx,
  B_spline = predicted_b_spline,
  P_spline = predicted_p_spline
)
print(comparison)

# (e) Implement 5-fold cross-validation of the P-spline
library(caret)

set.seed(4060)

# Define folds for cross-validation
folds <- createFolds(y, k = 5, list = TRUE)

# Initialize RMSE storage
rmse_values <- numeric(5)

# Perform 5-fold cross-validation
for (i in 1:5) {
  # Split data into training and testing sets
  train_idx <- setdiff(1:length(y), folds[[i]])
  test_idx <- folds[[i]]
  
  # Train P-spline on training data
  p_spline_cv <- smooth.spline(x[train_idx], y[train_idx])
  
  # Predict on testing data
  predicted <- predict(p_spline_cv, x[test_idx])$y
  
  # Compute RMSE
  rmse_values[i] <- sqrt(mean((y[test_idx] - predicted)^2))
}

# Calculate and display the overall RMSE
cv_rmse <- mean(rmse_values)

cat("\n5-fold cross-validation RMSE for P-spline:\n")
print(cv_rmse)
