library(ggplot2)

cauchy_dist <- function() {

  x <- c(-2.8, 3.4, 1.2, -0.3, -2.6)
  n <- length(x)
  a <- pi

  theta_values <- seq(-4, 4, by = 0.01) #-4 to 4

  # Calculate log likelihood and derivative for each theta value
  calculation_dist <- sapply(theta_values, function(b) -n * log(a) - sum(log(1 + (x - b)^2)))
  derivative_dist <- sapply(theta_values, function(b) sum(2 * (x - b) / (1 + (x - b)^2)))

  # Plot log likelihood
  plot(theta_values, calculation_dist, type = "l", col = "blue",
       main = "Log Likelihood Function and its Derivative",
       xlab = expression(theta), ylab = "Value")
  lines(theta_values, derivative_dist, col = "red")
  abline(h = 0, col = "green", lty = 2) # Line to visually see when the derivative is zero
  legend("topright", legend = c("Log Likelihood", "Derivative"),
         fill = c("blue", "red"))
}

# Call the function to display the plot
cauchy_dist()
