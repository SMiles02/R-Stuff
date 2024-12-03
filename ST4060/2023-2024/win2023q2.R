# Load the required library and data
library(MASS)
x <- Animals  # Load the Animals dataset

# Set the random seed for reproducibility
set.seed(4060)

# Question (a): Bootstrap estimates of mean brain and body weights
# ---------------------------------------------------
B <- 1000  # Number of bootstrap resamples
n <- nrow(x)

# Bootstrap mean brain weight
mean_brain_weight <- replicate(B, mean(sample(x$brain, size = n, replace = TRUE)))

# Bootstrap mean body weight
mean_body_weight <- replicate(B, mean(sample(x$body, size = n, replace = TRUE)))

# Estimates
bootstrap_mean_brain <- mean(mean_brain_weight)
bootstrap_mean_body <- mean(mean_body_weight)
cat("Bootstrap estimate of mean brain weight:", bootstrap_mean_brain, "\n")
cat("Bootstrap estimate of mean body weight:", bootstrap_mean_body, "\n")
# ---------------------------------------------------
# Explanation:
# We use bootstrap resampling to estimate the means of brain and body weights. 
# The bootstrap replicates the process of sampling from the original data with replacement.
# ---------------------------------------------------

# Question (b): Bootstrap estimate of mean brain-to-body weight ratio
# ---------------------------------------------------
ratio <- x$brain / x$body
mean_ratio <- replicate(B, mean(sample(ratio, size = n, replace = TRUE)))
bootstrap_mean_ratio <- mean(mean_ratio)
cat("Bootstrap estimate of mean brain-to-body weight ratio:", bootstrap_mean_ratio, "\n")
# ---------------------------------------------------
# Explanation:
# For each bootstrap sample, we calculate the mean brain-to-body weight ratio. 
# This provides a robust estimate of the mean ratio accounting for sample variability.
# ---------------------------------------------------

# Question (c): Bootstrap estimate of bias of mean body weight
# ---------------------------------------------------
sample_mean_body <- mean(x$body)
bootstrap_bias_body <- mean(mean_body_weight) - sample_mean_body
cat("Bootstrap estimate of bias for mean body weight:", bootstrap_bias_body, "\n")
# ---------------------------------------------------
# Explanation:
# Bias is computed as the difference between the bootstrap mean estimate 
# and the sample mean of body weight.
# ---------------------------------------------------

# Question (d): Bootstrap 95% confidence interval for mean body weight
# ---------------------------------------------------
conf_int <- quantile(mean_body_weight, probs = c(0.025, 0.975))
cat("Bootstrap 95% confidence interval for mean body weight:", conf_int, "\n")
# ---------------------------------------------------
# Explanation:
# The quantile method is used to compute the naive bootstrap confidence interval.
# The lower and upper bounds represent the 2.5th and 97.5th percentiles of the bootstrap distribution.
# ---------------------------------------------------

# Question (e): Explanation for large confidence interval width
# ---------------------------------------------------
# The width of the bootstrap confidence interval is large because of the high variability 
# in the body weights of the animals. The wide range of body weights in the data 
# (e.g., small animals vs. large animals) contributes to this variability.

# Inspect variability in original data
cat("Range of body weights in the dataset:", range(x$body), "\n")
# ---------------------------------------------------
# Explanation:
# The range of body weights indicates large variability in the data. 
# This variability inflates the standard error and widens the confidence interval.
# ---------------------------------------------------
