source("Alpha_cut_regression.R")
source("choose_alpha_Alpha_cut_regression.R")
source("predict_Alpha_cut_regression.R")
source("plot_Alpha_cut_regression.R")

###
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

### Example: House Prices Data.
# Covariates x1, x2, x3,x4,x5 are crisp.
X1 <- c(1, 1, 1, 1 ,1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3)
X2 <- c(38.09, 62.10, 63.76, 74.52, 75.38, 52.99, 62.93, 
        72.04, 76.12, 90.26, 85.70, 95.27, 105.98, 79.25, 120.50)
X3 <- c(36.43, 26.50, 44.71, 38.09, 41.40, 26.49, 26.49, 33.12, 
        43.06, 42.64, 31.33, 27.64, 27.64, 66.81, 32.25)
X4 <- c(5, 6, 7, 8, 7, 4, 5, 6, 7, 7, 6, 6, 6, 6, 6)
X5 <- c(1, 1, 1, 1, 2, 2, 2, 3, 2, 2, 3, 3, 3, 3, 3)
X  <- cbind(X1, X2, X3, X4, X5)
X_S_l <- cbind(rep(0,15), rep(0,15), rep(0,15), rep(0,15), rep(0,15))
X_S_r <- cbind(rep(0,15), rep(0,15), rep(0,15), rep(0,15), rep(0,15))

y    <- c(6060, 7100, 8080, 8260, 8650, 8520, 9170, 10310, 
          10920, 12030, 13940, 14200, 16010, 16320, 16990) %>% data.matrix() # center of Dependent variable Y.
s    <- c(550, 50, 400, 150, 750, 450, 700, 200, 600,
          100, 350, 250, 300, 500, 650) %>% data.matrix() # spread of Dependent variable Y
y_S_l <- s
y_S_r <- s
f_Y <- cbind(y - y_S_l, y, y + y_S_r)

n <- length(y)
p <- ncol(X)

######## Try different alpha-level sets. 
# 1. Fuzzy alpha-cut ridge regression
set.seed(1)
AlphaLvlSet_ridgeFit <- AlphaLvlSet_FuzzyAlphaCutReg(X, y, alpha = 0, penalty = TRUE,
                                                     X_left_spread = X_S_l, X_right_spread = X_S_r, 
                                                     y_left_spread = y_S_l, y_right_spread = y_S_r)
opt_alpha_lvl_set <- AlphaLvlSet_ridgeFit[[1]]$optimal_alpha_level_set

myPDF("SEQUENCE1.pdf", width = 5, height = 4,
      mar = c(3.9, 3.9, 1, 1),
      mgp = c(2.8, 0.55, 0))
AlphaLvlSet_ridgeFit[[2]]
dev.off()

ridgeFit <- FuzzyAlphaCutReg(X, y, alpha = 0, penalty = TRUE,
                             alpha_level_set = opt_alpha_lvl_set,
                             X_left_spread = X_S_l, X_right_spread = X_S_r, 
                             y_left_spread = y_S_l, y_right_spread = y_S_r)
ridgeFit_test <- predict_FuzzyAlphaCutReg(ridgeFit, 
                                          newX = X, 
                                          newX_left_spread = X_S_l, 
                                          newX_right_spread = X_S_r)

set.seed(1)
AlphaLvlSet_linearFit <- AlphaLvlSet_FuzzyAlphaCutReg(X, y, alpha = 0, penalty = FALSE,
                                                     X_left_spread = X_S_l, X_right_spread = X_S_r, 
                                                     y_left_spread = y_S_l, y_right_spread = y_S_r)
opt_alpha_lvl_set <- AlphaLvlSet_linearFit[[1]]$optimal_alpha_level_set

myPDF("SEQUENCE2.pdf", width = 5, height = 4,
      mar = c(3.9, 3.9, 1, 1),
      mgp = c(2.8, 0.55, 0))
AlphaLvlSet_linearFit[[2]]
dev.off()

linearFit <- FuzzyAlphaCutReg(X, y, alpha = 0, penalty = FALSE,
                             alpha_level_set = opt_alpha_lvl_set,
                             X_left_spread = X_S_l, X_right_spread = X_S_r, 
                             y_left_spread = y_S_l, y_right_spread = y_S_r)
linearFit_test <- predict_FuzzyAlphaCutReg(linearFit, 
                                          newX = X, 
                                          newX_left_spread = X_S_l, 
                                          newX_right_spread = X_S_r)

f_Y_test <- f_Y
n_test <- n
Ridge_f_Yhat_test <- ridgeFit_test$f_Yhat
Ridge_crisp_Yhat_test <- ridgeFit_test$crisp_Yhat
Linear_f_Yhat_test <- linearFit_test$f_Yhat
Linear_crisp_Yhat_test <- linearFit_test$crisp_Yhat


fuzzyridge_RMSE <- RMSE(f_Y_test, Ridge_f_Yhat_test, n_test)
fuzzyridge_MAPE <- MAPE(f_Y_test, Ridge_f_Yhat_test, n_test) 
fuzzylinear_RMSE <- RMSE(f_Y_test, Linear_f_Yhat_test, n_test)
fuzzylinear_MAPE <- MAPE(f_Y_test, Linear_f_Yhat_test, n_test)
ridge_RMSE <- crisp_RMSE(f_Y_test, Ridge_crisp_Yhat_test, n_test)
ridge_MAPE <- crisp_MAPE(f_Y_test, Ridge_crisp_Yhat_test, n_test) 
linear_RMSE <- crisp_RMSE(f_Y_test, Linear_crisp_Yhat_test, n_test) 
linear_MAPE <- crisp_MAPE(f_Y_test, Linear_crisp_Yhat_test, n_test) 

round(rbind(fuzzyridge_RMSE,
            fuzzylinear_RMSE,
            ridge_RMSE,
            linear_RMSE,
            fuzzyridge_MAPE,
            fuzzylinear_MAPE,
            ridge_MAPE,
            linear_MAPE), 2)

# Report fitted values.
round(ridgeFit_test$f_Yhat, 2)
round(linearFit_test$f_Yhat, 2)

# Spreads.
round(ridgeFit_test$f_Yhat[,3] - ridgeFit_test$f_Yhat[,2], 2)
round(ridgeFit_test$f_Yhat[,2] - ridgeFit_test$f_Yhat[,1], 2)

round(linearFit_test$f_Yhat[,3] - linearFit_test$f_Yhat[,2], 2)
round(linearFit_test$f_Yhat[,2] - linearFit_test$f_Yhat[,1], 2)


# Report parameter estimates
round(ridgeFit$f_Ak, 2)
round(linearFit$f_Ak, 2)

# Plot.
myPDF("TRIPLOT1.pdf", width = 5, height = 4,
      mar = c(3.9, 3.9, 1, 1),
      mgp = c(2.8, 0.55, 0))
plot_FuzzyAlphaCutReg(f_Y = f_Y_test, f_Yhat = ridgeFit_test$f_Yhat, penalty = TRUE)
dev.off()

myPDF("TRIPLOT2.pdf", width = 5, height = 4,
      mar = c(3.9, 3.9, 1, 1),
      mgp = c(2.8, 0.55, 0))
plot_FuzzyAlphaCutReg(f_Y = f_Y_test, f_Yhat = linearFit_test$f_Yhat, penalty = FALSE)
dev.off()

############################################
############################################
############################################

### Example 1.
# mid-point of covariates.
X1 <- c(7,7,6,8,8,6,7,7,7,6,
        7,7,7,7,7,7,6,7,7,7,
        7,7,7,7,7,7,7,7,7,6)
X2 <- c(8,7,7,9,8,7,8,7,8,7,
        8,6,8,8,7,7,7,8,7,9,
        8,8,9,7,7,7,7,8,7,7)
X <- cbind(X1, X2)

# left spread of covariates.
X1_s_l <- c(0.5,0.5,0.25,0.75,0.75,0.25,0.5,0.5,0.5,0.25,
            0.5,0.5,0.5,0.5,0.5,0.5,0.25,0.5,0.5,0.5,
            0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.25)
X2_s_l <- c(0.75,0.5,0.5,0,0.75,0.5,0.75,0.5,0.75,0.5,
            0.75,0.25,0.75,0.75,0.5,0.5,0.5,0.75,0.5,0,
            0.75,0.75,0,0.5,0.5,0.5,0.5,0.75,0.5,0.5)
X_S_l <- cbind(X1_s_l, X2_s_l)

# right spread of covariates.
X1_s_r <- c(1.25,1.25,0.5,1,1,0.5,1.25,1.25,1.25,0.5,
            1.25,1.25,1.25,1.25,1.25,1.25,0.5,1.25,1.25,1.25,
            1.25,1.25,1.25,1.25,1.25,1.25,1.25,1.25,1.25,0.5)
X2_s_r <- c(1,1.25,1.25,1,1,1.25,1,1.25,1,1.25,
            1,0.5,1,1,1.25,1.25,1.25,1,1.25,1,
            1,1,1,1.25,1.25,1.25,1.25,1,1.25,1.25)
X_S_r <- cbind(X1_s_r, X2_s_r)

# Y is a fuzzy variable. 
y <- c(8,6,6,9,8,5,7,5,7,6,
       8,6,9,8,7,7,6,7,8,7,
       7,6,7,8,6,6,7,7,7,6) %>% data.matrix()
y_S_l <- c(0.75,0.25,0.25,0,0.75,0,0.5,0,0.5,0.25,
           0.75,0.25,0,0.75,0.5,0.5,0.25,0.5,0.75,0.5,
           0.5,0.25,0.5,0.75,0.25,0.25,0.5,0.5,0.5,0.25) %>% data.matrix()
y_S_r <- c(1,0.5,0.5,1,1,1,1.25,1,1.25,0.5,
           1,0.5,1,1,1.25,1.25,0.5,1.25,1,1.25,
           1.25,0.5,1.25,1,0.5,0.5,1.25,1.25,1.25,0.5) %>% data.matrix()
f_Y <- cbind(y - y_S_l, y, y + y_S_r)

n <- length(y)
p <- ncol(X)

######## Try different alpha-level sets. 
# 1. Fuzzy alpha-cut ridge regression
set.seed(1)
AlphaLvlSet_ridgeFit <- AlphaLvlSet_FuzzyAlphaCutReg(X, y, alpha = 0, penalty = TRUE,
                                                     X_left_spread = X_S_l, X_right_spread = X_S_r, 
                                                     y_left_spread = y_S_l, y_right_spread = y_S_r)
opt_alpha_lvl_set <- AlphaLvlSet_ridgeFit[[1]]$optimal_alpha_level_set
AlphaLvlSet_ridgeFit[[2]]

ridgeFit <- FuzzyAlphaCutReg(X, y, alpha = 0, penalty = TRUE,
                             alpha_level_set = opt_alpha_lvl_set,
                             X_left_spread = X_S_l, X_right_spread = X_S_r, 
                             y_left_spread = y_S_l, y_right_spread = y_S_r)
ridgeFit_test <- predict_FuzzyAlphaCutReg(ridgeFit, 
                                          newX = X, 
                                          newX_left_spread = X_S_l, 
                                          newX_right_spread = X_S_r)

set.seed(1)
AlphaLvlSet_linearFit <- AlphaLvlSet_FuzzyAlphaCutReg(X, y, alpha = 0, penalty = FALSE,
                                                      X_left_spread = X_S_l, X_right_spread = X_S_r, 
                                                      y_left_spread = y_S_l, y_right_spread = y_S_r)
opt_alpha_lvl_set <- AlphaLvlSet_linearFit[[1]]$optimal_alpha_level_set
AlphaLvlSet_linearFit[[2]]

linearFit <- FuzzyAlphaCutReg(X, y, alpha = 0, penalty = FALSE,
                              alpha_level_set = opt_alpha_lvl_set,
                              X_left_spread = X_S_l, X_right_spread = X_S_r, 
                              y_left_spread = y_S_l, y_right_spread = y_S_r)
linearFit_test <- predict_FuzzyAlphaCutReg(linearFit, 
                                           newX = X, 
                                           newX_left_spread = X_S_l, 
                                           newX_right_spread = X_S_r)

f_Y_test <- f_Y
n_test <- n
Ridge_f_Yhat_test <- ridgeFit_test$f_Yhat
Ridge_crisp_Yhat_test <- ridgeFit_test$crisp_Yhat
Linear_f_Yhat_test <- linearFit_test$f_Yhat
Linear_crisp_Yhat_test <- linearFit_test$crisp_Yhat


fuzzyridge_RMSE <- RMSE(f_Y_test, Ridge_f_Yhat_test, n_test)
fuzzyridge_MAPE <- MAPE(f_Y_test, Ridge_f_Yhat_test, n_test) 
fuzzylinear_RMSE <- RMSE(f_Y_test, Linear_f_Yhat_test, n_test)
fuzzylinear_MAPE <- MAPE(f_Y_test, Linear_f_Yhat_test, n_test)
ridge_RMSE <- crisp_RMSE(f_Y_test, Ridge_crisp_Yhat_test, n_test)
ridge_MAPE <- crisp_MAPE(f_Y_test, Ridge_crisp_Yhat_test, n_test) 
linear_RMSE <- crisp_RMSE(f_Y_test, Linear_crisp_Yhat_test, n_test) 
linear_MAPE <- crisp_MAPE(f_Y_test, Linear_crisp_Yhat_test, n_test) 

round(rbind(fuzzyridge_RMSE,
            fuzzylinear_RMSE,
            ridge_RMSE,
            linear_RMSE,
            fuzzyridge_MAPE,
            fuzzylinear_MAPE,
            ridge_MAPE,
            linear_MAPE), 2)