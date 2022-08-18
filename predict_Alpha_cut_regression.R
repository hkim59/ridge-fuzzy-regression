predict_FuzzyAlphaCutReg <- function(object, newX, newX_left_spread, newX_right_spread) {
  # Performs fuzzy alpha-cut regression. 
  #
  # Args:
  #   object: A FuzzyAlphaCutReg object. 
  #   newdata: A dataframe / matrix containing the values at which predictions
  #            are required.
  # 
  # Returns:
  #   The fuzzy regression coefficients and RSME, MAPE. 
  
  f_Ak <- object$f_Ak
  f_Y <- object$f_Y
  crisp_Ak <- object$crisp_Ak
  crisp_Yhat <- object$crisp_Yhat

  X <- newX %>% data.matrix()
  X_s_l <- newX_left_spread %>% data.matrix()
  X_s_r <- newX_right_spread %>% data.matrix()
  n <- nrow(X)
  p <- ncol(X)
  f_X <- list(f_Xl = X - X_s_l, f_Xm = X, f_Xr = X + X_s_r)
  
  # Fitted values.
  X.A0  <- cbind(rep(1, n) * f_Ak[1, 1], rep(1, n) * f_Ak[1, 2], rep(1, n) * f_Ak[1, 3])
  l_X.Ak <- f_X$f_Xl %*% f_Ak[2:(p+1), 1]
  m_X.Ak <- f_X$f_Xm %*% f_Ak[2:(p+1), 2]
  r_X.Ak <- f_X$f_Xr %*% f_Ak[2:(p+1), 3]
  
  X.Ak <- cbind(l_X.Ak, m_X.Ak, r_X.Ak)
  f_Yhat <- X.A0 + X.Ak
  
  # Crisp outputs
  crisp_Yhat <- cbind(rep(1, n) * crisp_Ak[1]) + f_X$f_Xm %*% crisp_Ak[2:(p+1)]
  
  Output <- list(f_Ak = f_Ak, 
                 crisp_Ak = crisp_Ak, 
                 f_Y = f_Y, 
                 f_Yhat = f_Yhat,
                 crisp_Yhat = crisp_Yhat)
  return(Output)
}
