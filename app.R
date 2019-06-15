
# Components needed -------------------------------------------------------

## Read data and assign x and y for the training data
data <- mtcars

# Let's start out with 2 variables: mpg ~ cyl + weight
y <- data$mpg
x <- matrix(c(rep(1, length(y)), data$cyl, data$wt), byrow = F, ncol = 3) 

m <- length(y)

# Gradient descent --------------------------------------------------------

# Set parameters
alpha <- 0.01
n_iter <- 10000
theta <- c(rep(1, dim(x)[2]))
h = x %*% theta

j_hist <- c(rep(0, n_iter))

theta_hist <- matrix(rep(1, length(theta) * n_iter), 
                     ncol = length(theta))

theta_hist[1,] <- theta

# Compute initial RMSE
SSE = sum((h - y)^2)
MSE = SSE / length(y)
RMSE = sqrt(MSE)
j_hist[1] <- RMSE

# Run gradient descent
for (i in 2:n_iter) {
  
derivative = t(h - y) %*% x

theta = theta - (alpha / m * t(derivative))
h = x %*% theta

# Compute RMSE
SSE = sum((h - y)^2)
MSE = SSE / length(y)
RMSE = sqrt(MSE)
j_hist[i] <- RMSE

theta_hist[i,] <- theta

}

plot(j_hist)
plot(theta_hist[1:100,1])
plot(theta_hist[1:100,2])
plot(theta_hist[1:100,3])
# The parameter estimates get lost initially

# It's quite cool. It converges at around iteration 6000. Remember that 
# vanilla gradient descent with linear regression is really fussy about the 
# learning rate and the number of iterations. Another note is: Do not expect
# all of your parameter estimates to go in the right direction. Sometimes,
# in multidimensional space, your parameter estimates go in the wrong direction.
# It's quite tricky to set the number of iterations as well.

# Trust the algorithm to know what it's doing. Also, note that you can use this
# derivation for any regression type problem. Is that right? Yes, it's right,
# because it doesn't assume any form of the hypothesis. Therefore, I can just
# add a new lambda parameter.

# Truth -------------------------------------------------------------------

# Alright. Let's check the truth.
true_lm <- lm(y ~ x[,2] + x[,3])
summary(true_lm)

# This model is really amazing. It explains 82% of variability in the data.
# I think I picked the best variables to look at.

# 39, -1.5, -3.2 are the real parameter estimates. I need to try to achieve these


# Regularisation ----------------------------------------------------------
# In this case, a penalty term needs to be added somewhere. It can't be
# added at the hypothesis stage. The form of the hypothesis is the same.

# Set parameters
alpha <- 0.0001
n_iter <- 30000
lambda <- 0.5
theta <- c(rep(1, dim(x)[2]))
h = x %*% (theta + lambda * theta^2)

j_hist <- c(rep(0, n_iter))

theta_hist <- matrix(rep(1, length(theta) * n_iter), 
                     ncol = length(theta))

theta_hist[1,] <- theta

# Compute initial RMSE
SSE = sum((h - y)^2)
MSE = SSE / length(y)
RMSE = sqrt(MSE)
j_hist[1] <- RMSE

# Run gradient descent
for (i in 2:n_iter) {
  
  derivative = t(h - y) %*% x
  
  theta = theta - (alpha / m * t(derivative))
  h = x %*% (theta + lambda * theta^2)
  
  # Compute RMSE
  SSE = sum((h - y)^2)
  MSE = SSE / length(y)
  RMSE = sqrt(MSE)
  j_hist[i] <- RMSE
  
  theta_hist[i,] <- theta
  
}

plot(j_hist)
plot(theta_hist[,1])
plot(theta_hist[,2])
plot(theta_hist[,3])
