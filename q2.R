library(ggplot2)
myvar <- function(x) {
  n <- length(x)
  return((sum(x^2) - (sum(x)^2) / n) / (n - 1))
}

# Generating data
set.seed(123)
n <- 10000
mean_value <- 10^8
std_dev <- sqrt(variance)
x <- rnorm(n, mean = mean_value, sd = std_dev)

# Print variance
computed_variance <- myvar(x)
print(computed_variance)

# For comparison, print the variance using R's built-in function
print(var(x))



difference_fnctns <- numeric(n)
for (i in 1:n) {
  x_sub <- x[1:i]
  difference_fnctns[i] <- myvar(x_sub) - var(x_sub)
}
options(digits=10) # Display more digits
print(head(x))
plot(1:n, difference_fnctns, type="l", col="red", main="myvar and R's var function difference", xlab="i", ylab="Yi")



#print(difference_fnctns)
 #Plotting
#plot(1:n, differences, type="l", col="red", main="myvar and R's var function difference", xlab="i", ylab="Yi")

