# Question 1(a)
set.seed(4060)

# Parameters
theta <- 3
sigma <- 1.5
N <- 30
M <- 1000

# Monte Carlo estimation
contains_theta <- numeric(M)

for (i in 1:M) {
  sample_data <- rnorm(N, mean = theta, sd = sigma)
  sample_mean <- mean(sample_data)
  sample_sd <- sd(sample_data)
  lower_bound <- sample_mean - 1.96 * (sample_sd / sqrt(N))
  upper_bound <- sample_mean + 1.96 * (sample_sd / sqrt(N))
  contains_theta[i] <- ifelse(theta >= lower_bound & theta <= upper_bound, 1, 0)
}

# Monte Carlo estimate of p
p_95 <- mean(contains_theta)
print(p_95)



# Question 1(b)
set.seed(4060)

# Monte Carlo estimation with 1.645 quantile
contains_theta_1645 <- numeric(M)

for (i in 1:M) {
  sample_data <- rnorm(N, mean = theta, sd = sigma)
  sample_mean <- mean(sample_data)
  sample_sd <- sd(sample_data)
  lower_bound <- sample_mean - 1.645 * (sample_sd / sqrt(N))
  upper_bound <- sample_mean + 1.645 * (sample_sd / sqrt(N))
  contains_theta_1645[i] <- ifelse(theta >= lower_bound & theta <= upper_bound, 1, 0)
}

# Monte Carlo estimate of p for 1.645 quantile
p_90 <- mean(contains_theta_1645)
print(p_90)


# Question 1(c)
cat("The estimate p_95 represents the proportion of confidence intervals, constructed with 95% confidence level, that contain the true value theta.",
    "Similarly, p_90 represents the proportion of confidence intervals, constructed with 90% confidence level, that contain the true value theta.\n")

# Question 1(d)
cat("If the sample size N is increased to 100, the width of the confidence intervals would decrease because the standard error decreases with increasing sample size.",
    "As a result, the confidence intervals would be more likely to contain the true value of theta, leading to a higher estimated value of p.")

