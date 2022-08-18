require(glmnet)
require(tidyverse)
require(reshape2)

FuzzyAlphaCutReg <- function(X, y, alpha = 0, penalty = TRUE, seed = seed, 
                             alpha_level_set,
                             X_left_spread, X_right_spread, 
                             y_left_spread, y_right_spread) {
  # Performs fuzzy alpha-cut regression. 
  #
  # Args:
  #   X: Matrix of crisp predictor variables. 
  #   y: A vector of the mid point of the fuzzy predicted variable. 
  #   left_spread: A vector of left spread of the fuzzy predicted variable. 
  #   right_spread: A vector of right spread of the fuzzy predicted variable. 
  #   alpha: If 0 and penalty = TRUE, performs fuzzy alpha-cut ridge regression. 
  #          If 1 and penalty = TRUE, performs fuzzy alpha-cut lasso regression. 
  #   penalty: If alpha = 0 and penalty = FALSE, perform fuzzy alpha-cut linear regression. 
  # 
  # Returns:
  #   The fuzzy regression coefficients.
  
  # Error handling: To be updated later...
  
  # --- STEP 1. -------------------------------------------------------------- #
  # Create an $\alpha$ - level of the dependent variable Y. 
  # The set of $\alpha$ - levels is given by $A = \{1, 0.75, 0.5, 0.25, 0\}$.
  # -------------------------------------------------------------------------- #
  
  X <- X %>% data.matrix()
  y <- y %>% data.matrix()
  
  n <- length(y)
  p <- ncol(X)
  
  X_s_l <- X_left_spread %>% data.matrix()
  X_s_r <- X_right_spread %>% data.matrix()
  y_s_l <- y_left_spread %>% data.matrix()
  y_s_r <- y_right_spread %>% data.matrix()
  
  f_Y <- cbind(y - y_s_l, y, y + y_s_r)
  f_X <- list(f_Xl = X - X_s_l, f_Xm = X, f_Xr = X + X_s_r)
  
  #A <- c(1, 0.75, 0.5, 0.25, 0)
  A <- alpha_level_set
  len_A <- length(A)
  
  l_Y <- matrix(NA, nrow = n, ncol = len_A)
  r_Y <- matrix(NA, nrow = n, ncol = len_A)
  colnames(l_Y) <- str_c("l_Y(",A,")")
  colnames(r_Y) <- str_c("r_Y(",A,")")
  
  l_X_temp <- rep(list(matrix(NA, nrow = n, ncol = len_A)), p)
  r_X_temp <- rep(list(matrix(NA, nrow = n, ncol = len_A)), p)
  names(l_X_temp) <- str_c("l_X",1:p)
  names(r_X_temp) <- str_c("r_X",1:p)
  
  for (a in 1:len_A) {
    l_Y[, a] <- y_s_l * A[a] + y - y_s_l
    r_Y[, a] <- -y_s_r * A[a] + y + y_s_r
    for (i in 1:p) {
      l_X_temp[[i]][, a] <- X_s_l[,i] * A[a] + X[,i] - X_s_l[,i]
      r_X_temp[[i]][, a] <- -X_s_r[,i] * A[a] + X[,i] + X_s_r[,i]
    }
  }
  
  # reshape l_X and r_X 
  l_X_temp <- melt(l_X_temp)
  r_X_temp <- melt(r_X_temp)
  
  l_X <- rep(list(matrix(NA, nrow = n, ncol = p)), len_A)
  r_X <- rep(list(matrix(NA, nrow = n, ncol = p)), len_A)
  names(l_X) <- str_c("l_X(",A,")")
  names(r_X) <- str_c("r_X(",A,")")
  
  for (a in 1:len_A) {
    l_X[[a]] <- matrix(l_X_temp[l_X_temp$Var2 == a, 3], nrow = n, ncol = p)
    r_X[[a]] <- matrix(r_X_temp[r_X_temp$Var2 == a, 3], nrow = n, ncol = p)
  }

  # --- STEP 2 - 5. -----------------------------------------------------------------------------------
  # Regress $y$ on $X$ to find the estimator $\hat{l_{A_{k}}}(\alpha)$, and $\hat{r_{A_{k}}}(\alpha)$ 
  # for each $k = 1, \cdots, p$ and $\alpha \in A$. 
  # First obtain the intermediate estimators $\bar{l_{A_{k}}}(\alpha)$, and $\bar{r_{A_{k}}}(\alpha)$.
  # ---------------------------------------------------------------------------------------------------

  # Intermediate fuzzy regression coefficients based on optimal lambda value. 
  l_opt_lambda <- matrix(NA, nrow = len_A, ncol = 1)
  r_opt_lambda <- matrix(NA, nrow = len_A, ncol = 1)
  
  bar.l_Ak <- matrix(NA, nrow = p + 1, ncol = len_A)
  bar.r_Ak <- matrix(NA, nrow = p + 1, ncol = len_A)
  
  set.seed(seed)
  for (a in 1:len_A) {
    # optimal lambda values
    l_opt_lambda[a] <- cv.glmnet(l_X[[a]], l_Y[, a], alpha = alpha)$lambda.min
    r_opt_lambda[a] <- cv.glmnet(r_X[[a]], r_Y[, a], alpha = alpha)$lambda.min
  }
  
  if (penalty) {
    l_opt_lambda <- l_opt_lambda
    r_opt_lambda <- r_opt_lambda
  } else {
    l_opt_lambda <- matrix(0, nrow = len_A, ncol = 1)
    r_opt_lambda <- matrix(0, nrow = len_A, ncol = 1)
  }
  
  set.seed(seed)
  for (a in 1:len_A) {
    # estimated fuzzy regression coefficients
    l_fit <- glmnet(l_X[[a]], l_Y[, a], alpha = alpha, lambda = l_opt_lambda[a])
    r_fit <- glmnet(r_X[[a]], r_Y[, a], alpha = alpha, lambda = r_opt_lambda[a])
    
    bar.l_Ak[, a] <- as.matrix(coef(l_fit))
    bar.r_Ak[, a] <- as.matrix(coef(r_fit))
  }
  
  # --- STEP 2 - 5. -------------------------------------------------------------------------------------------
  # Modify the intermediate estimators $\bar{l_{A_{k}}}(\alpha)$, and $\bar{r_{A_{k}}}(\alpha)$ so that 
  # the estimated coefficients form a pre-defined shape of the membership function. 
  # For each $A_k$, $k = 1, \cdots, p$, compare the $\alpha$ - levels with that of the previous level, 
  # i.e., use the *min* and *max* operators to obtain $\hat{l_{A_{k}}}(\alpha)$, and $\hat{r_{A_{k}}}(\alpha)$ 
  # for each $k = 1, \cdots, p$. Note that we do not modify the intercept.
  # ------------------------------------------------------------------------------------------------------------
  
  # Modify intermediate coefficients for the fuzzy regression
  hat.l_Ak <- bar.l_Ak
  hat.r_Ak <- bar.r_Ak
  
  for (i in 1:(p + 1)) {
    for (j in 2:len_A) {
      hat.l_Ak[i, j] <-  ifelse(hat.l_Ak[i, j] <= hat.l_Ak[i, j-1], hat.l_Ak[i, j], hat.l_Ak[i, j-1])
      hat.r_Ak[i, j] <-  ifelse(hat.r_Ak[i, j] >= hat.r_Ak[i, j-1], hat.r_Ak[i, j], hat.r_Ak[i, j-1])
    }
  }
  
  hat.lr_Ak <- data.frame(t(hat.l_Ak), t(hat.r_Ak))
  
  # --- STEP 6 ---------------------------------------------------------------------------------------------------------
  # Find the membership function $\mu_{A_{k}}(x)$ for the fuzzy regression coefficients 
  # $A_{k}$, $k = 1, \cdots, p$ by performing linear regression on the estimated $\alpha$ -level sets 
  # $\hat{A_{k}}(\alpha_j) = [\hat{l_{A_{k}}}(\alpha_j), \hat{r_{A_{k}}}(\alpha_j)]$, for all $\alpha \in A$. 
  # Give constraints so that the top of the pre-defined membership function satisfy the condition of $\alpha$ - level 1.
  # ---------------------------------------------------------------------------------------------------------------------
  
  # --- 1. Find each of the left and right slopes for the pre-defined membership function. 
  hat.lr_Ak <- data.frame(A = A, hat.lr_Ak)
  slopes <- matrix(NA, nrow = (p + 1), ncol = 3)
  
  for (i in 1:(p + 1)) {
    # left slope
    slopes[i, 1] <- lm(I(hat.lr_Ak[, 1] - hat.lr_Ak[1, 1]) ~ I(hat.lr_Ak[, (i+1)] - hat.lr_Ak[1, (i+1)]) + 0)$coef 
    # center value
    slopes[i, 2] <- hat.lr_Ak[1, (i + 1)]   
    # right slope
    slopes[i, 3] <- lm(I(hat.lr_Ak[, 1] - hat.lr_Ak[1, 1]) ~ I(hat.lr_Ak[, (i+p+2)] - hat.lr_Ak[1, (i+p+2)]) + 0)$coef 
  }
  
  # --- 2. Choose the left / right / both slopes based on RMSE.  
  # --- (1) Choose the left/right slope then make symmetric. Otherwise, keep the left and right slopes. 
  
  # Choose the left slope then make symmetric
  slopes.Left <- cbind(slopes[,1], slopes[,2], -slopes[,1])
  # Choose the right slope then make symmetric
  slopes.Right <- cbind(-slopes[,3], slopes[,2], slopes[,3])
  # Choose the left and right slopes. 
  slopes.LeftRight <- slopes
  
  # --- (2) Compute $\hat{l_{A_{k}}}(0)$, and $\hat{r_{A_{k}}}(0)$ for each methods using 
  # --- the obtained slope for the pre-defined membership function. 
  
  # The left slope chosen.
  f_Ak.Left <- matrix(NA, nrow = (p + 1), ncol = 3)
  
  for (i in 1:(p + 1)) {
    f_Ak.Left[i, 1] <- slopes.Left[i, 2] - (1 / slopes.Left[i, 1])
    f_Ak.Left[i, 2] <- slopes.Left[i, 2]
    f_Ak.Left[i, 3] <- slopes.Left[i, 2] - (1 / slopes.Left[i, 3]) 
    
    f_Ak.Left[is.na(f_Ak.Left)] <- f_Ak.Left[i, 2] # Fill in NA values
  }
  
  # The right slope chosen.
  f_Ak.Right <- matrix(NA, nrow = (p + 1), ncol = 3)
  
  for (i in 1:(p + 1)) {
    f_Ak.Right[i, 1] <- slopes.Right[i, 2] - (1 / slopes.Right[i, 1])
    f_Ak.Right[i, 2] <- slopes.Right[i, 2]
    f_Ak.Right[i, 3] <- slopes.Right[i, 2] - (1 / slopes.Right[i, 3]) 
    
    f_Ak.Right[is.na(f_Ak.Right)] <- f_Ak.Right[i, 2] # Fill in NA values
  }
  
  # left and right slope chosen.
  f_Ak.LeftRight <- matrix(NA, nrow = (p + 1), ncol = 3)
  
  for (i in 1:(p + 1)) {
    f_Ak.LeftRight[i, 1] <- slopes.LeftRight[i, 2] - (1 / slopes.LeftRight[i, 1])
    f_Ak.LeftRight[i, 2] <- slopes.LeftRight[i, 2]
    f_Ak.LeftRight[i, 3] <- slopes.LeftRight[i, 2] - (1 / slopes.LeftRight[i, 3]) 
    
    f_Ak.LeftRight[is.na(f_Ak.LeftRight)] <- f_Ak.LeftRight[i, 2] # Fill in NA values
  }
  
  # --- (3) Compute the fuzzy predicted values for each of the three methods. 
  # --- Note. We defined the fuzzy regression coefficients as symmetric. Howe
  # --- the fuzzy predicted values are symmetric as well. 
  
  # Calculate fuzzy predicted values for the left slope chosen.
  X.A0  <- cbind(rep(1, n) * f_Ak.Left[1, 1], rep(1, n) * f_Ak.Left[1, 2], rep(1, n) * f_Ak.Left[1, 3])
  l_X.Ak <- f_X$f_Xl %*% f_Ak.Left[2:(p+1), 1]
  m_X.Ak <- f_X$f_Xm %*% f_Ak.Left[2:(p+1), 2]
  r_X.Ak <- f_X$f_Xr %*% f_Ak.Left[2:(p+1), 3]
  
  X.Ak <- cbind(l_X.Ak, m_X.Ak, r_X.Ak)
  f_Yhat.Left <- X.A0 + X.Ak
  
  # Calculate fuzzy predicted values for the right slope chosen.
  X.A0  <- cbind(rep(1, n) * f_Ak.Right[1, 1], rep(1, n) * f_Ak.Right[1, 2], rep(1, n) * f_Ak.Right[1, 3])
  l_X.Ak <- f_X$f_Xl %*% f_Ak.Right[2:(p+1), 1]
  m_X.Ak <- f_X$f_Xm %*% f_Ak.Right[2:(p+1), 2]
  r_X.Ak <- f_X$f_Xr %*% f_Ak.Right[2:(p+1), 3]
  
  X.Ak <- cbind(l_X.Ak, m_X.Ak, r_X.Ak)
  f_Yhat.Right <- X.A0 + X.Ak
  
  # Calculate fuzzy predicted values for the left and right slope chosen.
  X.A0  <- cbind(rep(1, n) * f_Ak.LeftRight[1, 1], rep(1, n) * f_Ak.LeftRight[1, 2], rep(1, n) * f_Ak.LeftRight[1, 3])
  l_X.Ak <- f_X$f_Xl %*% f_Ak.LeftRight[2:(p+1), 1]
  m_X.Ak <- f_X$f_Xm %*% f_Ak.LeftRight[2:(p+1), 2]
  r_X.Ak <- f_X$f_Xr %*% f_Ak.LeftRight[2:(p+1), 3]
  
  X.Ak <- cbind(l_X.Ak, m_X.Ak, r_X.Ak)
  f_Yhat.LeftRight <- X.A0 + X.Ak

  # --- (4) Compute RMSE between the observed fuzzy values $Y$ and the fitted fuzzy values $\hat{Y}$ 
  # for each of the three methods, respectively.
  # The left slope chosen
  RMSE.Left <- sqrt(sum(rowSums((f_Y - f_Yhat.Left)^2)) / n)
  # The right slope chosen
  RMSE.Right <- sqrt(sum(rowSums((f_Y - f_Yhat.Right)^2)) / n)
  # The left and right slope chosen
  RMSE.LeftRight <- sqrt(sum(rowSums((f_Y - f_Yhat.LeftRight)^2)) / n)
  
  # --- Final output.
  # Estimated regression coefficient.
  if (RMSE.Left < RMSE.Right && RMSE.Left < RMSE.LeftRight) {
    f_Ak <- f_Ak.Left
  } else if (RMSE.Right < RMSE.Left && RMSE.Right < RMSE.LeftRight) {
    f_Ak <- f_Ak.Right
  } else {
    f_Ak <- f_Ak.LeftRight
  }
  
  rownames(f_Ak) <- str_c("A", seq(0, p))
  colnames(f_Ak) <- c("l_Ak(0)", "Ak(1)", "r_Ak(0)")
  
  # Fitted values.
  X.A0  <- cbind(rep(1, n) * f_Ak[1, 1], rep(1, n) * f_Ak[1, 2], rep(1, n) * f_Ak[1, 3])
  l_X.Ak <- f_X$f_Xl %*% f_Ak[2:(p+1), 1]
  m_X.Ak <- f_X$f_Xm %*% f_Ak[2:(p+1), 2]
  r_X.Ak <- f_X$f_Xr %*% f_Ak[2:(p+1), 3]
  
  X.Ak <- cbind(l_X.Ak, m_X.Ak, r_X.Ak)
  f_Yhat <- X.A0 + X.Ak
  
  # Estimated left spread.
  hat.left_spread <- f_Yhat[, 2] - f_Yhat[, 1]
  # Estimated right spread
  hat.right_spread <- f_Yhat[, 3] - f_Yhat[, 2]

  # Slopes.
  if (RMSE.Left < RMSE.Right && RMSE.Left < RMSE.LeftRight) {
    f_slopes <- slopes.Left
  } else if (RMSE.Right < RMSE.Left && RMSE.Right < RMSE.LeftRight) {
    f_slopes <- slopes.Right
  } else {
    f_slopes <- slopes.LeftRight
  }
  
  # Crisp outputs
  crisp_Yhat <- f_Yhat[, 2]
  crisp_Ak <- f_Ak[, 2]
  
  # Final fuzzy RMSE
  RMSE = sqrt(sum(rowSums((f_Y - f_Yhat)^2)) / n)
  
  # Final fuzzy MAPE
  f_diff <- f_Y - f_Yhat
  for (i in 1:3) {
    f_diff[,i] <- abs(f_diff[,i] / f_Y[,i])
  }
  MAPE <- 100 * sum(rowSums(f_diff))/ n
  
  # Crisp RMSE
  crisp_RMSE <- sqrt(sum(f_Y[, 2] - crisp_Yhat)^2 / n)
  # Crisp MAPE
  crisp_MAPE <- 100 * sum(abs((f_Y[, 2] - crisp_Yhat) / f_Y[, 2]) / n)
  
  ### Summary of intermediate coefficients for the fuzzy ridge regression. 
  hat.l_Ak_summary <- cbind(l_opt_lambda, t(hat.l_Ak))
  rownames(hat.l_Ak_summary) <- str_c("alpha = ", A)
  colnames(hat.l_Ak_summary) <- c("optimal lambda", str_c("l_A", seq(0, p)))
  
  hat.r_Ak_summary <- cbind(r_opt_lambda, t(hat.r_Ak))
  rownames(hat.r_Ak_summary) <- str_c("alpha = ", A)
  colnames(hat.r_Ak_summary) <- c("optimal lambda", str_c("r_A", seq(0, p)))

  Output <- list(n = n, p = p, penalty = penalty, 
                 f_Y = f_Y, f_Yhat = f_Yhat, 
                 hat.left_spread = hat.left_spread, hat.right_spread = hat.right_spread, 
                 crisp_Y = f_Y[, 2], crisp_Yhat = crisp_Yhat,
                 RMSE = RMSE,
                 MAPE = str_c(round(MAPE, 4), "%"), 
                 crisp_RMSE = crisp_RMSE,
                 crisp_MAPE = str_c(round(crisp_MAPE, 4), "%"), 
                 intermediate_l_Ak = hat.l_Ak_summary,
                 intermediate_r_Ak = hat.r_Ak_summary,
                 f_slopes = f_slopes, 
                 f_Ak = f_Ak,
                 crisp_Ak = crisp_Ak)
  return(Output)
}
