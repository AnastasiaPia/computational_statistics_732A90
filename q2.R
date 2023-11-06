options(digits =22)

myvar <- function(x) {
  n <- length(x)
  a<-sum(x^2)
  b<-sum(x)
  c<-(a-(b^2/ n)) / (n - 1)

  return(c)
}
set.seed(123)
n <- 10000
mean_value <- 10^8
variance<-1
std_dev <- sqrt(variance)
x <- rnorm(n, mean_value, std_dev)

difference_fnctns <- c()
for (i in 2:n) {
  x_sub <- x[1:i]
  y_sub <- myvar(x_sub) - var(x_sub)
  difference_fnctns <- c(difference_fnctns, y_sub)
}

plot(difference_fnctns)

#print(var(x))
#print(difference_fnctns)
#plot( difference_fnctns, type="p", col="red", main="myvar and R's var function difference", xlab="i", ylab="Yi")




