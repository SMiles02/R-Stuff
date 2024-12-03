# Set the seed for reproducibility
set.seed(6015)

# Parameters for the Gamma distribution
a <- 3    # Shape parameter
b <- 2    # Rate parameter
N <- 100  # Sample size per simulation
M <- 1000 # Number of Monte Carlo simulations

# Initialize a vector to store sample means
sample_means <- numeric(M)

# Monte Carlo simulation
for (i in 1:M) {
  # Generate a random sample from the Gamma distribution
  sample <- rgamma(n = N, shape = a, rate = b)
  
  # Calculate and store the sample mean
  sample_means[i] <- mean(sample)
}

# (a) Monte Carlo estimate of the expected value of the sample mean
mean_sample_mean <- mean(sample_means)

# (b) Monte Carlo estimate of the standard error of the sample mean
standard_error <- sd(sample_means)

# Output results
cat("Monte Carlo estimate of the expected value of the sample mean:", mean_sample_mean, "\n")
cat("Monte Carlo estimate of the standard error of the sample mean:", standard_error, "\n")
