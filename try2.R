library(ggplot2)

var_two_pass <- function(x) {
  n <- length(x)
  mean_val <- sum(x) / n
  sum_sq_diffs <- sum((x - mean_val)^2)
  return(sum_sq_diffs / (n - 1))
}

set.seed(123)
n <- 10000
mean_value <- 10^8
variance <- 1
std_dev <- sqrt(variance)

num_datasets <- 100
differences <- numeric(num_datasets)

for (i in 1:num_datasets) {
  x <- rnorm(n, mean = mean_value, sd = std_dev)
  differences[i] <- var_two_pass(x) - var(x)
}

df <- data.frame(dataset = 1:num_datasets, difference = differences)
ggplot(df, aes(x = dataset, y = difference)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Difference between var_two_pass and R's var function",
       x = "Dataset number",
       y = "Difference in variance")
