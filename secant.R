log_likeli <- function() {
  x <- c(-2.8, 3.4, 1.2,-0.3,-2.6)
  n <- length(x)
  theta_values <- seq(-4, 4, by = 0.01)

  derivative_func <- function(theta) {
    sum(2 * (x - theta) / (1 + (x - theta) ^ 2))
  }
  secant_method <-
    function(f,
             x0,
             x1,
             epsilon = 1e-8,
             max_iter = 100) {
      for (i in 1:max_iter) {
        fx1 <- f(x1)
        fx0 <- f(x0)
        if (fx1 - fx0 == 0) {
          stop("Division by zero in secant method.")
        }
        x2 <- x1 - fx1 * (x1 - x0) / (fx1 - fx0)
        if (abs(x2 - x1) < epsilon) {
          cat("Secant method converged at iteration:", i, "\n")
          return(x2)
        }
        x0 <- x1
        x1 <- x2
      }
      stop("Secant method did not converge within the specified number of iterations.")
    }

  # Find the root using the secant method with initial guesses
  root <- secant_method(derivative_func,-4, 4)


  return(root)
}
root <- log_likeli()
print(paste("Root:", root))
