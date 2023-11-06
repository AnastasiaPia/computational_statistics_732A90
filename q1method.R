bisection_method <- function(f, a, b, tol = 1e-6, max_iter = 1000) {
  if (f(a) * f(b) > 0) {
    stop("f(a) and f(b) must have different signs.")
  }

  iter <- 0
  while ((b - a)/2 > tol && iter < max_iter) {
    c <- (a + b) / 2
    if (f(c) == 0) {
      break
    } else if (f(c) * f(a) < 0) {
      b <- c
    } else {
      a <- c
    }
    iter <- iter + 1
  }
  return((a + b) / 2)
}

# Using the bisection method on the derivative of the log-likelihood
derivative <- function(theta) {
  x <- c(-2.8, 3.4, 1.2, -0.3, -2.6)
  return(sum(2 * (x - theta) / (1 + (x - theta)^2)))
}

root <- bisection_method(derivative, -4, 4)
print(root)
