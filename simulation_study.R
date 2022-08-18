source("Alpha_cut_regression.R")
source("choose_alpha_Alpha_cut_regression.R")
source("predict_Alpha_cut_regression.R")
source("plot_Alpha_cut_regression.R")
library(tidyverse)

######
# Final fuzzy RMSE
RMSE <- function(f_Y, f_Yhat, n) {sqrt(sum(rowSums((f_Y - f_Yhat)^2)) / n)}

# Final fuzzy MAPE
MAPE <- function(f_Y, f_Yhat, n) {
  f_diff <- f_Y - f_Yhat
  for (i in 1:3) {
    f_diff[,i] <- abs(f_diff[,i] / f_Y[,i])
  }
  MAPE <- 100 * sum(rowSums(f_diff))/ n
}

# Crisp RMSE
crisp_RMSE <- function(f_Y, crisp_Yhat, n) {sqrt(sum(f_Y[, 2] - crisp_Yhat)^2 / n)}

# Crisp MAPE
crisp_MAPE <- function(f_Y, crisp_Yhat, n) {
  100 * sum(abs((f_Y[, 2] - crisp_Yhat) / f_Y[, 2]) / n)
}

######
# Monte Carlo Simulations
B <- 200
N <- 100
rho <- 0.99
sigma <- 2
#sigma <- c(0.5, 1, 1.5, 2, 2.5)

fuzzyridge_RMSE <- NA
fuzzyridge_MAPE <- NA
fuzzylinear_RMSE <- NA
fuzzylinear_MAPE <- NA
ridge_RMSE <- NA
ridge_MAPE <- NA
linear_RMSE <- NA
linear_MAPE <- NA

set.seed(1)
# Crisp inputs
Z1 <- rnorm(N, mean = 50, sd = 1)
Z2 <- rnorm(N, mean = 50, sd = 1)
Z3 <- rnorm(N, mean = 50, sd = 1)
Z4 <- rnorm(N, mean = 50, sd = 1)

X1 <- sqrt(1-rho^2) * Z1 + rho * Z4
X2 <- sqrt(1-rho^2) * Z2 + rho * Z4
X3 <- sqrt(1-rho^2) * Z3 + rho * Z4
X4 <- sqrt(1-rho^2) * Z4 + rho * Z4
X <- cbind(X1, X2, X3, X4)

p <- ncol(X)

X_S_l <- matrix(rep(rep(0,N), p), nrow = N, ncol = p)
X_S_r <- matrix(rep(rep(0,N), p), nrow = N, ncol = p)

# left-point
A0_l <- 0
A1_l <- 0.1
A2_l <- 0.15
A3_l <- 0.2
A4_l <- 0.25
# mid-point
A0_m <- 0
A1_m <- 0.4
A2_m <- 0.45
A3_m <- 0.5
A4_m <- 0.55
# right-point
A0_r <- 0
A1_r <- 0.7
A2_r <- 0.75
A3_r <- 0.8
A4_r <- 0.85

set.seed(2)
for (i in 1:B) {
  # Y is a fuzzy variable. 
  epsilon_l <- rnorm(N, mean = 0, sd = sigma)
  epsilon_m <- rnorm(N, mean = 0, sd = sigma)
  epsilon_r <- rnorm(N, mean = 0, sd = sigma)
  
  y_l <- A0_l + A1_l*X1 + A2_l*X2 + A3_l*X3 + A4_l*X4 + epsilon_l %>% data.matrix()
  y_m <- A0_m + A1_m*X1 + A2_m*X2 + A3_m*X3 + A4_m*X4 + epsilon_r %>% data.matrix()
  y_r <- A0_r + A1_r*X1 + A2_r*X2 + A3_r*X3 + A4_r*X4 + epsilon_m %>% data.matrix()
  
  y_S_l <- y_m - y_l
  y_S_r <- y_r - y_m
  
  # symmetric f_Y:
  #y_S_r <- y_S_l
  #y_r <- y_m + y_S_r
  
  f_Y <- cbind(y_l, y_m, y_r)
  
  # Divide data set into train/test sets. 
  p <- ncol(X)
  
  index <- sample.int(N, N*0.2, replace = FALSE)
  
  X_train <- X[-index, ]
  y_train <- y_m[-index, ]
  X_S_l_train <- X_S_l[-index, ]
  X_S_r_train <- X_S_r[-index, ]
  y_S_l_train <- y_S_l[-index, ]
  y_S_r_train <- y_S_r[-index, ]
  f_Y_train   <- f_Y[-index, ]

  X_test   <- X[index, ]
  y_test   <- y_m[index, ]
  X_S_l_test <- X_S_l[index, ]
  X_S_r_test <- X_S_r[index, ]
  y_S_l_test <- y_S_l[index, ]
  y_S_r_test <- y_S_r[index, ]
  f_Y_test   <- f_Y[index, ]

  n_train   <- length(y_train)
  n_test    <- length(y_test)
  
  # 1. Fuzzy alpha-cut ridge regression
  ridgeFit <- FuzzyAlphaCutReg(X_train, y_train, alpha = 0, penalty = TRUE,
                               alpha_level_set = c(1, 0.75, 0.5, 0.25, 0),
                               X_left_spread = X_S_l_train, X_right_spread = X_S_r_train, 
                               y_left_spread = y_S_l_train, y_right_spread = y_S_r_train)
  ridgeFit_test <- predict_FuzzyAlphaCutReg(ridgeFit, 
                                            newX = X_test, 
                                            newX_left_spread = X_S_l_test, 
                                            newX_right_spread = X_S_r_test)
  
  # 2. Fuzzy alpha-cut linear regression
  linearFit <- FuzzyAlphaCutReg(X_train, y_train, alpha = 0, penalty = FALSE,
                                alpha_level_set = c(1, 0.75, 0.5, 0.25, 0),
                                X_left_spread = X_S_l_train, X_right_spread = X_S_r_train, 
                                y_left_spread = y_S_l_train, y_right_spread = y_S_r_train)
  linearFit_test <- predict_FuzzyAlphaCutReg(linearFit, 
                                             newX = X_test, 
                                             newX_left_spread = X_S_l_test, 
                                             newX_right_spread = X_S_r_test)
  
  Ridge_f_Yhat_test <- ridgeFit_test$f_Yhat
  Ridge_crisp_Yhat_test <- ridgeFit_test$crisp_Yhat
  Linear_f_Yhat_test <- linearFit_test$f_Yhat
  Linear_crisp_Yhat_test <- linearFit_test$crisp_Yhat
  
  fuzzyridge_RMSE <- c(fuzzyridge_RMSE, RMSE(f_Y_test, Ridge_f_Yhat_test, n_test)) 
  fuzzyridge_MAPE <- c(fuzzyridge_MAPE, MAPE(f_Y_test, Ridge_f_Yhat_test, n_test)) 
  fuzzylinear_RMSE <- c(fuzzylinear_RMSE, RMSE(f_Y_test, Linear_f_Yhat_test, n_test))
  fuzzylinear_MAPE <- c(fuzzylinear_MAPE, MAPE(f_Y_test, Linear_f_Yhat_test, n_test)) 
  ridge_RMSE <- c(ridge_RMSE, crisp_RMSE(f_Y_test, Ridge_crisp_Yhat_test, n_test)) 
  ridge_MAPE <- c(ridge_MAPE, crisp_MAPE(f_Y_test, Ridge_crisp_Yhat_test, n_test)) 
  linear_RMSE <- c(linear_RMSE, crisp_RMSE(f_Y_test, Linear_crisp_Yhat_test, n_test)) 
  linear_MAPE <- c(linear_MAPE, crisp_MAPE(f_Y_test, Linear_crisp_Yhat_test, n_test)) 
}

# Report summary results:
round(rbind(fuzzyridge_RMSE = mean(fuzzyridge_RMSE, na.rm = TRUE),
      fuzzylinear_RMSE = mean(fuzzylinear_RMSE, na.rm = TRUE),
      ridge_RMSE = mean(ridge_RMSE, na.rm = TRUE),
      linear_RMSE = mean(linear_RMSE, na.rm = TRUE),
      fuzzyridge_MAPE = mean(fuzzyridge_MAPE, na.rm = TRUE),
      fuzzylinear_MAPE = mean(fuzzylinear_MAPE, na.rm = TRUE),
      ridge_MAPE = mean(ridge_MAPE, na.rm = TRUE),
      linear_MAPE = mean(linear_MAPE, na.rm = TRUE)), 3)

# Report fitted values.
round(lassoFit_test$f_Yhat, 2)
round(linearFit_test$f_Yhat, 2)

# Spreads.
round(lassoFit_test$f_Yhat[,3] - lassoFit_test$f_Yhat[,2], 2)
round(lassoFit_test$f_Yhat[,2] - lassoFit_test$f_Yhat[,1], 2)

round(linearFit_test$f_Yhat[,3] - linearFit_test$f_Yhat[,2], 2)
round(linearFit_test$f_Yhat[,2] - linearFit_test$f_Yhat[,1], 2)


# Report parameter estimates: hat_Ak
round(lassoFit$f_Ak, 3)
round(linearFit$f_Ak, 2)

# Plot.
plot_FuzzyAlphaCutReg(f_Y = f_Y_test, f_Yhat = ridgeFit_test$f_Yhat, penalty = TRUE)
plot_FuzzyAlphaCutReg(f_Y = f_Y_test, f_Yhat = linearFit_test$f_Yhat, penalty = FALSE)
