# Two-Pass Variance Computation
var_two_pass <- function(x) {
  # first: compute the mean
  n <- length(x)
  mean_val <- sum(x) / n

  # second: compute the sum of squared differences from the mean
  sum_sq_diffs <- sum((x - mean_val)^2)

  return(sum_sq_diffs / (n - 1))
}
set.seed(123)
n <- 10000
mean_value <- 10^8
variance <- 1
std_dev <- sqrt(variance)
x <- rnorm(n, mean = mean_value, sd = std_dev)

cat("var function output:", var(x), "\n")
cat("two pass variance output:", var_two_pass(x))
