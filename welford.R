welford_var <- function(x) {
  n <- 0
  mean <- 0.0
  M2 <- 0.0

  for (xi in x) {
    n <- n + 1
    delta <- xi - mean
    mean <- mean + delta / n
    delta2 <- xi - mean
    M2 <- M2 + delta * delta2
  }

  if (n < 2) {
    return(NA)  # return NA for variance if sample size is too small
  } else {
    return(M2 / (n - 1))
  }
}

# Generating data
set.seed(123)
n <- 10000
mean_value <- 10^8
variance <- 1
std_dev <- sqrt(variance)
x <- rnorm(n, mean = mean_value, sd = std_dev)

# Print variance using the welford_var function
computed_variance_welford <- welford_var(x)
print(computed_variance_welford)

# For comparison, print the variance using R's built-in function
print(var(x))
