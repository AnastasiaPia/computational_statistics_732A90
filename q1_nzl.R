log_likeli <- function() {
  x <- c(-2.8, 3.4, 1.2, -0.3, -2.6)
  n <- length(x)
  theta_values <- seq(-4, 4, by = 0.01)

  log_likelihood <- sapply(theta_values, function(b) -n * log(pi) - sum(log(1 + (x - b)^2)))
  derivative <- sapply(theta_values, function(b) sum(2 * (x - b) / (1 + (x - b)^2)))

  # Plot log-likelihood
  plot(theta_values, log_likelihood, type = "l", col = "blue",
       main = "Log Likelihood Function",
       xlab = expression(theta), ylab = "Log Likelihood")

  # Plot the derivative
  plot(theta_values, derivative, type = "l", col = "red",
       main = "Derivative of the Log Likelihood Function",
       xlab = expression(theta), ylab = "Derivative")
  abline(h = 0, col = "purple", lty = 2)

  secant<-function(f,x0, x1,epsilon, max_iter){
iterations<-0
while(iterations<=max_iter){

}

  }
}

log_likeli()
