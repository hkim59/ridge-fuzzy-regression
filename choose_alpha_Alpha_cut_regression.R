AlphaLvlSet_FuzzyAlphaCutReg <- function(X, y, alpha = 0, penalty = TRUE, seed = seed, 
                                         X_left_spread, X_right_spread, 
                                         y_left_spread, y_right_spread) {
  # Choosed the optimal alpha-level set for fuzzy alpha-cut regression 
  # base on RMSE.
  
  # alpha-level set candidates.
  A <- list(#rev(seq(0,1,by=0.01)),
            rev(seq(0,1,by=0.02)),
            rev(seq(0,1,by=0.025)),
            rev(seq(0,1,by=0.04)),
            rev(seq(0,1,by=0.05)),
            rev(seq(0,1,by=0.1)),
            rev(seq(0,1,by=0.15)),
            rev(seq(0,1,by=0.2)),
            rev(seq(0,1,by=0.25)),
            rev(seq(0,1,by=0.3)),
            rev(seq(0,1,by=0.5)),
            rev(seq(0,1,by=1))
            )
  
  RMSE <- function(f_Y, f_Yhat, n) {sqrt(sum(rowSums((f_Y - f_Yhat)^2)) / n)}
  RMSE_A <- matrix(rep(NA, length(A)), nrow = length(A), ncol = 1)

  for (i in 1:length(A)) {
    object <- FuzzyAlphaCutReg(X, y, alpha = alpha, penalty = penalty, seed = seed, 
                               alpha_level_set = A[[i]],
                               X_left_spread, X_right_spread, 
                               y_left_spread, y_right_spread)
    object_test <- predict_FuzzyAlphaCutReg(object, 
                                            newX = X, 
                                            newX_left_spread = X_left_spread, 
                                            newX_right_spread = X_right_spread)
    f_Y <- object$f_Y
    n   <- object$n
    f_Yhat <- object$f_Yhat
    
    RMSE_A[i,1] <- RMSE(f_Y, f_Yhat, n)
  }
  
  min_RMSE <- sort(RMSE_A, index.return = TRUE)$x[1]
  min_index <- sort(RMSE_A, index.return = TRUE)$ix[1]
  
  Output <- list(alpha_level_set = A,
                 RMSE = RMSE_A,
                 optimal_alpha_level_set = A[[min_index]], 
                 min_RMSE = min_RMSE)
  
  col <- ifelse(penalty, "firebrick2", "dodgerblue4")
  
  plot <- ggplot() +
    geom_line(aes(x=seq(1,length(A)), y = RMSE_A)) +
    geom_point(aes(x=seq(1,length(A)), y = RMSE_A)) +
    geom_point(aes(x=min_index, y = min_RMSE), color = col, size = 2) +
    scale_x_continuous(breaks=seq(1,length(A))) +
    labs(x = expression(alpha * "- level sequence")) +
    labs(y = expression("RMSE "[F])) +
    #ggtitle(expression("Optimal " * alpha * "- levels seqence")) +
    theme_classic()
  
  return(list(Output, plot))
}
