plot_FuzzyAlphaCutReg <- function(f_Y, f_Yhat, penalty) {
  
  #n <- object$n
  #p <- object$p
  #penalty <- object$penalty
  #f_Y <- object$f_Y
  #f_Yhat <- object$f_Yhat
  #crisp_Y <- object$crisp_Y
  #crisp_Yhat <- object$crisp_Yhat
  #f_Ak <- object$f_Ak
  #crisp_Ak <- object$crisp_Ak
  #f_slopes <- object$f_slopes
  
  penalty <- penalty
  f_Y <- f_Y
  f_Yhat <- f_Yhat
  n <- nrow(f_Yhat)
  color <- ifelse(penalty, "firebrick2", "dodgerblue4")
  
  # Plot of the fuzzy regression coefficients. 
  #par(mfrow = c(2,2))
  #
  #for (i in 1:(p + 1)) {
  #  left.slope <- function(x) {f_slopes[i,1] * (x - f_slopes[i,2]) + 1}
  #  right.slope <- function(x) {f_slopes[i,3] * (x - f_slopes[i,2]) + 1}
  #  
  #  min_x <- f_Ak[i, 1] 
  #  max_x <- f_Ak[i, 3] 
  #  
  #  x_left <- c(min_x, f_Ak[i, 2])
  #  x_right <- c(f_Ak[i, 2], max_x)
  #  x <- c(x_left, x_right)
  #  
  #  plot(x, c(left.slope(x_left), right.slope(x_right)), type = "l", 
  #       xlim = c(min_x, max_x), ylim = c(0, 1),
  #       main = str_c("l_A", i-1), ylab = " ", xlab = "values")
  #  abline(v = f_slopes[i,2], col = color)
  #}
  
  # The plot of the observed values and the fitted values
  index <- sort(f_Y[, 2], index = TRUE)$ix
  f_Y.sort <- f_Y[index, ]
  f_Yhat.sort <- f_Yhat[index, ]
  y_df <- data.frame(x = c(rbind(seq(1, n)-0.5, seq(1, n), seq(1, n)-0.5)), 
                    y = as.vector(t(f_Y.sort)),
                    grp = rep(1:n, each = 3))
  
  yhat_df <- data.frame(x = c(rbind(seq(1, n)-0.5, seq(1, n), seq(1, n)-0.5)),
                          y = as.vector(t(f_Yhat.sort)), 
                          grp = rep(1:n, each = 3))
  
  par(mfrow = c(1,1))
  q = ggplot()
  for(g in unique(y_df$grp)){
    dat1 = subset(y_df, grp == g)
    dat2 = subset(yhat_df, grp == g)
    
    q = q + geom_polygon(data = dat1, aes(x, y, alpha=0.6), colour="grey20", fill=NA) + 
      geom_polygon(data = dat2, aes(x, y, alpha=0.6), colour = color, fill=NA) +
      #ylim(4000, 20000) +
      scale_alpha(guide = 'none') +
      theme_bw() + 
      labs(x = " ", y = " ") 
  }
  suppressWarnings(print(q)) 
}

