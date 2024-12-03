# Set the seed for reproducibility
set.seed(6040)

# Parameters for the simulation
M <- 1000       # Number of Monte Carlo repetitions
N <- 1000       # Sample size

# Initialize a vector to store results
bootstrap_percent <- numeric(M)

# Monte Carlo simulation
for (i in 1:M) {
  # Generate a bootstrap resample
  resample <- sample(1:N, size = N, replace = TRUE)
  
  # Count the number of unique data points in the resample
  unique_count <- length(unique(resample))
  
  # Calculate the percentage of unique points in the resample
  bootstrap_percent[i] <- unique_count / N * 100
}

# Calculate the mean percentage of unique data points
mean_unique_percentage <- mean(bootstrap_percent)

# Display results
cat("Mean percentage of unique data points in bootstrap resamples:", mean_unique_percentage, "%\n")
summary(bootstrap_percent)
