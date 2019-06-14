
# Components needed -------------------------------------------------------

## Read data and assign x and y for the training data
data <- mtcars

# Let's start out with 2 variables: mpg ~ cyl + weight
y <- data$mpg
x <- matrix(c(rep(1, length(y)), data$cyl, data$wt), byrow = F, ncol = 3) 

# Set number of iterations
n_iter <- 10
m <- length(y)

# Define theta based on the number of features in x
theta <- c(rep(1, dim(x)[2]))

# Create theta history, an empty matrix
theta_hist <- matrix(rep(1, length(theta) * n_iter), 
                         ncol = length(theta))

theta_hist[1,] <- theta

# Set learning rate alpha
alpha <- 0.1


# Gradient descent --------------------------------------------------------

# This seems to be working fine.
theta <- c(1, 2, 3)
h = x %*% theta

# theta = theta - alpha * derivative expression

# I will minimise a different expression and we'll see how good it is.

# Need to codify the derivative
derivative = (t(h - y) %*% x) / m

theta = theta - (alpha * t(derivative))
h = x %*% theta

# Compute RMSE
SSE = sum((h - y)^2)
MSE = SSE / length(y)
RMSE = sqrt(MSE)


# Solution ----------------------------------------------------------------

for (j in 1:p) {
  # vectorized version
  # (exactly the same with multivariate version)
  deriv = (t(X %*% theta_prev - y) %*% X[, j]) / m
  
  # update theta_j
  theta[j] = theta_prev[j] - (alpha * deriv)
}

# Let's see what is going on here.


# Truth -------------------------------------------------------------------

# Alright. Let's check the truth.
true_lm <- lm(data$mpg ~ data$cyl + data$wt)
summary(true_lm)

# This model is really amazing. It explains 82% of variability in the data.
# I think I picked the best variables to look at.

# 39, -1.5, -3.2 are the real parameter estimates. I need to try to achieve these


