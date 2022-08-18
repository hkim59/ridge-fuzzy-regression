library(tidyverse)
library(ggthemes)
library(openintro)

### STEP1.
y <- c(2,3,8)
alpha <- c(0,0,0)
df_point <- data.frame(y = y, alpha = alpha)
df_point_alpha <- data.frame(y = c(6.9,9.15), alpha = c(0,0))
df_tri1_l <- data.frame(x1 = 2, x2 = 0, y1 = 1, y2 = 0)
df_tri1_r <- data.frame(x1 = 2, x2 = 2.5, y1 = 1, y2 = 0)
df_line1  <- data.frame(x1 = 2, x2 = 2, y1 = 0.99, y2 = 0)
df_tri2_l <- data.frame(x1 = 3, x2 = 1.5, y1 = 1, y2 = 0)
df_tri2_r <- data.frame(x1 = 3, x2 = 5, y1 = 1, y2 = 0)
df_line2  <- data.frame(x1 = 3, x2 = 3, y1 = 0.99, y2 = 0)
df_tri3_l <- data.frame(x1 = 8, x2 = 5.8, y1 = 1, y2 = 0)
df_tri3_r <- data.frame(x1 = 8, x2 = 10.3, y1 = 1, y2 = 0)
df_line3  <- data.frame(x1 = 8, x2 = 8, y1 = 0.99, y2 = 0)
df_curve3_l <- data.frame(x1 = 8, x2 = 5.75, y1 = 0, y2 = 0)
df_curve3_r <- data.frame(x1 = 8, x2 = 10.35, y1 = 0, y2 = 0)
df_dots1 <- data.frame(y = seq(4.8, 6.3, length.out = 6), alpha = rep(0.75,3))
df_dots2 <- data.frame(y = seq(9.5, 10.5, length.out = 3), alpha = rep(0.75,3))

myPDF("STEP1.pdf", width = 5, height = 4,
      mar = c(3.9, 3.9, 1, 1),
      mgp = c(2.8, 0.55, 0))
ggplot() +
  geom_point(data = df_point, aes(x = y, y = alpha)) + 
  geom_point(data = df_point_alpha, aes(x = y, y = alpha), colour = "firebrick2") + 
  geom_segment(aes(x = 0, xend = 11 , y = 0, yend = 0), size = 0.5,
               arrow = arrow(length = unit(0.3,"cm"))) +
  geom_segment(aes(x = 0, xend = 0 , y = 0, yend = 1.2), size = 0.5,
               arrow = arrow(length = unit(0.3,"cm"))) +
  geom_segment(data = df_tri1_l, aes(x = x1, y = y1, xend = x2, yend = y2)) + 
  geom_segment(data = df_tri1_r, aes(x = x1, y = y1, xend = x2, yend = y2)) +
  geom_segment(data = df_line1, aes(x = x1, y = y1, xend = x2, yend = y2), linetype = 2) +  
  annotate("text", label = expression(y["1m"]), x = 2, y = -0.04, size = 4) +
  geom_segment(data = df_tri2_l, aes(x = x1, y = y1, xend = x2, yend = y2)) + 
  geom_segment(data = df_tri2_r, aes(x = x1, y = y1, xend = x2, yend = y2)) + 
  geom_segment(data = df_line2, aes(x = x1, y = y1, xend = x2, yend = y2), linetype = 2) +  
  annotate("text", label = expression(y["2m"]), x = 3, y = -0.04, size = 4) +
  geom_segment(data = df_tri3_l, aes(x = x1, y = y1, xend = x2, yend = y2)) + 
  geom_segment(data = df_tri3_r, aes(x = x1, y = y1, xend = x2, yend = y2)) + 
  geom_segment(data = df_line3, aes(x = x1, y = y1, xend = x2, yend = y2), linetype = 2) +  
  annotate("text", label = expression(y["im"]), x = 8, y = -0.04, size = 4) +
  geom_curve(data = df_curve3_l, aes(x = x1, y = y1, xend = x2, yend = y2), linetype = 2, curvature = -0.2) +  
  geom_curve(data = df_curve3_r, aes(x = x1, y = y1, xend = x2, yend = y2), linetype = 2, curvature = 0.2) +  
  annotate("text", label = expression(s["il"]^y), x = 6.5, y = -0.08, size = 4) +
  annotate("text", label = expression(s["ir"]^y), x = 9.5, y = -0.08, size = 4) +
  geom_segment(aes(x = 0, y = 0.5, xend = 9.1, yend = 0.5), linetype = 2, color = "firebrick2") +  
  geom_segment(aes(x = 9.15, y = 0.5, xend = 9.15, yend = 0), linetype = 2, color = "firebrick2") +  
  geom_segment(aes(x = 6.9, y = 0.5, xend = 6.9, yend = 0), linetype = 2, color = "firebrick2") +  
  annotate("text", label = expression(l["yi"](alpha)), x = 6.43, y = 0.05, size = 3.5, colour = "firebrick2") +
  annotate("text", label = expression(r["yi"](alpha)), x = 9.68, y = 0.05, size = 3.5, colour = "firebrick2") +
  geom_point(data = df_dots1, aes(x = y, y = alpha), size = 0.5) +
  geom_point(data = df_dots2, aes(x = y, y = alpha), size = 0.5) +
  scale_x_continuous(breaks=c()) +
  scale_y_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1)) +
  labs(x = expression(Y)) + 
  labs(y = expression(alpha)) + 
  theme(
    panel.background = element_rect(fill = NA),
    panel.grid = element_blank(),
    panel.ontop = TRUE
  )
dev.off()

### STEP2.
y <- c(7, 5.3, 8.0, 3.7, 8.7, 4.8, 7.7, 0, 10)
alpha <- c(1, 0.75, 0.75, 0.5, 0.5, 0.25, 0.25, 0, 0)
df_point_1 <- data.frame(y = y, alpha = alpha)
df_point_0 <- data.frame(y = y, alpha = rep(0,9))
df_tri_l <- data.frame(x1 = 7, x2 = 0, y1 = 1, y2 = 0)
df_tri_r <- data.frame(x1 = 7, x2 = 10, y1 = 1, y2 = 0)

myPDF("STEP2.pdf", width = 5, height = 4,
      mar = c(3.9, 3.9, 1, 1),
      mgp = c(2.8, 0.55, 0))
ggplot() +
  geom_point(data = df_point_1, aes(x = y, y = alpha)) +
  geom_segment(aes(x = y, y = alpha, xend = y, yend = rep(0,9)), linetype = 2) +  
  geom_point(data = df_point_0, aes(x = y, y = alpha), size = 1) +
  geom_segment(aes(x = 0, xend = 11 , y = 0, yend = 0), size = 0.5,
               arrow = arrow(length = unit(0.3,"cm"))) +
  geom_segment(aes(x = 0, xend = 0 , y = 0, yend = 1.2), size = 0.5,
               arrow = arrow(length = unit(0.3,"cm"))) +
  geom_segment(aes(x = 0, y = 1, xend = 7, yend = 1), linetype = 1, color = "dodgerblue4") +  
  geom_segment(aes(x = 0, y = 0.75, xend = 8, yend = 0.75), linetype = 1, color = "dodgerblue4") +  
  geom_segment(aes(x = 0, y = 0.5, xend = 8.7, yend = 0.5), linetype = 1, color = "dodgerblue4") +  
  geom_segment(aes(x = 0, y = 0.25, xend = 7.7, yend = 0.25), linetype = 1, color = "dodgerblue4") +  
  geom_segment(aes(x = 0, y = 0, xend = 10, yend = 0), linetype = 1, color = "dodgerblue4") +  
  annotate("text", label = expression(bar(l["Aj"])(1) * "=" * bar(r["Aj"])(1)), x = 7, y = 1.07, size = 3.5) +
  annotate("text", label = expression(bar(l["Aj"])(0.75)), x = 5.6, y = -0.07, size = 3.5) +
  geom_segment(aes(x = 8.4, xend = 8, y = -0.2, yend = 0), size = 0.5, color = "dodgerblue4",
               arrow = arrow(length = unit(0.2,"cm"))) +
  annotate("text", label = expression(bar(r["Aj"])(0.75)), x = 8.9, y = -0.25, size = 3.5) +
  annotate("text", label = expression(bar(l["Aj"])(0.5)), x = 3.5, y = -0.07, size = 3.5) +
  annotate("text", label = expression(bar(r["Aj"])(0.5)), x = 8.9, y = -0.07, size = 3.5) +
  geom_segment(aes(x = 4.6, xend = 4.8, y = -0.12, yend = 0), size = 0.5, color = "dodgerblue4",
               arrow = arrow(length = unit(0.2,"cm"))) +
  geom_segment(aes(x = 7.5, xend = 7.7, y = -0.12, yend = 0), size = 0.5, color = "dodgerblue4",
               arrow = arrow(length = unit(0.2,"cm"))) +
  annotate("text", label = expression(bar(l["Aj"])(0.25)), x = 4.5, y = -0.17, size = 3.5) +
  annotate("text", label = expression(bar(r["Aj"])(0.25)), x = 7.5, y = -0.17, size = 3.5) +
  annotate("text", label = expression(bar(l["Aj"])(0)), x = 0, y = -0.07, size = 3.5) +
  annotate("text", label = expression(bar(r["Aj"])(0)), x = 10.2, y = -0.07, size = 3.5) +
  scale_x_continuous(breaks=c()) +
  scale_y_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1)) +
  labs(x = " ") + 
  #labs(x = expression(A[j])) + 
  labs(y = expression(alpha)) +
  theme(
    panel.background = element_rect(fill = NA),
    panel.grid = element_blank(),
    panel.ontop = TRUE
  )
dev.off()

### STEP3.
y <- c(7, 5.3, 8.0, 3.7, 8.7, 0, 10)
alpha <- c(1, 0.75, 0.75, 0.5, 0.5, 0, 0)
df_point_1 <- data.frame(y = y, alpha = alpha)
df_point_0 <- data.frame(y = y, alpha = rep(0,7))
df_point_1_modified <- data.frame(y = c(3.7, 8.7), alpha = c(0.25, 0.25))

myPDF("STEP3.pdf", width = 5, height = 4,
      mar = c(3.9, 3.9, 1, 1),
      mgp = c(2.8, 0.55, 0))
ggplot() +
  geom_point(data = df_point_1, aes(x = y, y = alpha)) +
  geom_point(data = df_point_1_modified, aes(x = y, y = alpha), col = "firebrick2", lwd = 2) +
  geom_segment(aes(x = y, y = alpha, xend = y, yend = rep(0,7)), linetype = 2) +  
  geom_point(data = df_point_0, aes(x = y, y = alpha), size = 1) +
  geom_segment(aes(x = 0, xend = 11 , y = 0, yend = 0), size = 0.5,
               arrow = arrow(length = unit(0.3,"cm"))) +
  geom_segment(aes(x = 0, xend = 0 , y = 0, yend = 1.2), size = 0.5,
               arrow = arrow(length = unit(0.3,"cm"))) +
  geom_segment(aes(x = 0, y = 1, xend = 7, yend = 1), linetype = 1, color = "dodgerblue4") +  
  geom_segment(aes(x = 0, y = 0.75, xend = 8, yend = 0.75), linetype = 1, color = "dodgerblue4") +  
  geom_segment(aes(x = 0, y = 0.5, xend = 8.7, yend = 0.5), linetype = 1, color = "dodgerblue4") +  
  geom_segment(aes(x = 0, y = 0.25, xend = 8.7, yend = 0.25), linetype = 1, color = "dodgerblue4") +  
  geom_segment(aes(x = 0, y = 0, xend = 10, yend = 0), linetype = 1, color = "dodgerblue4") +  
  annotate("text", label = expression(tilde(l["Aj"])(1) * "=" * tilde(r["Aj"])(1)), x = 7, y = 1.07, size = 3.5) +
  annotate("text", label = expression(tilde(l["Aj"])(0.75)), x = 5.6, y = -0.07, size = 3.5) +
  annotate("text", label = expression(tilde(r["Aj"])(0.75)), x = 7.6, y = -0.07, size = 3.5) +
  annotate("text", label = expression(tilde(l["Aj"])(0.25) * "=" * tilde(l["Aj"])(0.5)), x = 3, y = -0.07, size = 3.5) +
  geom_segment(aes(x = 9.1, xend = 8.7, y = -0.15, yend = 0), size = 0.5, color = "dodgerblue4",
               arrow = arrow(length = unit(0.2,"cm"))) +
  annotate("text", label = expression(tilde(r["Aj"])(0.25) * "=" * tilde(r["Aj"])(0.5)), x = 8.9, y = -0.19, size = 3.5) +
  annotate("text", label = expression(tilde(l["Aj"])(0)), x = 0, y = -0.07, size = 3.5) +
  annotate("text", label = expression(tilde(r["Aj"])(0)), x = 10.2, y = -0.07, size = 3.5) +
  scale_x_continuous(breaks=c()) +
  scale_y_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1)) +
  labs(x = " ") + 
  #labs(x = expression(A[j])) + 
  labs(y = expression(alpha)) +
  theme(
    panel.background = element_rect(fill = NA),
    panel.grid = element_blank(),
    panel.ontop = TRUE
  )
dev.off()

### STEP3.
y <- c(7, 5.3, 8.0, 3.7, 8.7, 0, 10)
alpha <- c(1, 0.75, 0.75, 0.5, 0.5, 0, 0)
df_point_1 <- data.frame(y = y, alpha = alpha)
df_point_0 <- data.frame(y = y, alpha = rep(0,7))
df_point_1_modified <- data.frame(y = c(3.7, 8.7), alpha = c(0.25, 0.25))

ggplot() +
  geom_point(data = df_point_1, aes(x = y, y = alpha)) +
  geom_point(data = df_point_1_modified, aes(x = y, y = alpha), col = "firebrick2", lwd = 2) +
  geom_segment(aes(x = y, y = alpha, xend = y, yend = rep(0,7)), linetype = 2) +  
  geom_point(data = df_point_0, aes(x = y, y = alpha)) +
  geom_segment(aes(x = 0, xend = 11 , y = 0, yend = 0), size = 0.5,
               arrow = arrow(length = unit(0.3,"cm"))) +
  geom_segment(aes(x = 0, xend = 0 , y = 0, yend = 1.2), size = 0.5,
               arrow = arrow(length = unit(0.3,"cm"))) +
  geom_segment(aes(x = 0, y = 1, xend = 7, yend = 1), linetype = 1, color = "dodgerblue4") +  
  geom_segment(aes(x = 0, y = 0.75, xend = 8, yend = 0.75), linetype = 1, color = "dodgerblue4") +  
  geom_segment(aes(x = 0, y = 0.5, xend = 8.7, yend = 0.5), linetype = 1, color = "dodgerblue4") +  
  geom_segment(aes(x = 0, y = 0.25, xend = 8.7, yend = 0.25), linetype = 1, color = "dodgerblue4") +  
  geom_segment(aes(x = 0, y = 0, xend = 10, yend = 0), linetype = 1, color = "dodgerblue4") +  
  annotate("text", label = expression(tilde(l["Aj"])(1) * "=" * tilde(r["Aj"])(1)), x = 7, y = 1.07, size = 3.5) +
  annotate("text", label = expression(tilde(l["Aj"])(0.75)), x = 5.6, y = -0.07, size = 3.5) +
  annotate("text", label = expression(tilde(r["Aj"])(0.75)), x = 7.6, y = -0.07, size = 3.5) +
  annotate("text", label = expression(tilde(l["Aj"])(0.25) * "=" * tilde(l["Aj"])(0.5)), x = 3, y = -0.07, size = 3.5) +
  geom_segment(aes(x = 9.1, xend = 8.7, y = -0.15, yend = 0), size = 0.5, color = "dodgerblue4",
               arrow = arrow(length = unit(0.2,"cm"))) +
  annotate("text", label = expression(tilde(r["Aj"])(0.25) * "=" * tilde(r["Aj"])(0.5)), x = 8.9, y = -0.19, size = 3.5) +
  annotate("text", label = expression(tilde(l["Aj"])(0)), x = 0, y = -0.07, size = 3.5) +
  annotate("text", label = expression(tilde(r["Aj"])(0)), x = 10.2, y = -0.07, size = 3.5) +
  scale_x_continuous(breaks=c()) +
  scale_y_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1)) +
  labs(x = expression(A[j])) + 
  labs(y = expression(alpha)) +
  theme(
    panel.background = element_rect(fill = NA),
    panel.grid = element_blank(),
    panel.ontop = TRUE
  )

### STEP4.
y <- c(5.3, 8.0, 3.7, 8.7, 3.7, 8.7, 0, 10)
alpha <- c(0.75, 0.75, 0.5, 0.5, 0.25, 0.25, 0, 0)
df_point_1 <- data.frame(y = y, alpha = alpha)
df_point_0 <- data.frame(y = y, alpha = rep(0,8))
df_point_1_modified <- data.frame(y = c(7, 7, 1.7, 9.6), alpha = c(1, 0, 0, 0))
df_tri_l <- data.frame(x1 = 7, x2 = 1.7, y1 = 1, y2 = 0)
df_tri_r <- data.frame(x1 = 7, x2 = 9.6, y1 = 1, y2 = 0)

myPDF("STEP4.pdf", width = 5, height = 4,
      mar = c(3.9, 3.9, 1, 1),
      mgp = c(2.8, 0.55, 0))
ggplot() +
  geom_point(data = df_point_1, aes(x = y, y = alpha)) +
  geom_point(data = df_point_1_modified, aes(x = y, y = alpha), col = "firebrick2", lwd = 2) +
  geom_segment(aes(x = y, y = alpha, xend = y, yend = rep(0,8)), linetype = 2) +  
  geom_segment(aes(x = 7, y = 1, xend = 7, yend = 0), linetype = 2, col = "firebrick2") +  
  geom_point(data = df_point_0, aes(x = y, y = alpha), size = 1) +
  geom_segment(aes(x = 0, xend = 11 , y = 0, yend = 0), size = 0.5,
               arrow = arrow(length = unit(0.3,"cm"))) +
  geom_segment(aes(x = 0, xend = 0 , y = 0, yend = 1.2), size = 0.5,
               arrow = arrow(length = unit(0.3,"cm"))) +
  geom_segment(aes(x = 0, y = 1, xend = 7, yend = 1), linetype = 1, color = "dodgerblue4") +  
  geom_segment(aes(x = 0, y = 0.75, xend = 8, yend = 0.75), linetype = 1, color = "dodgerblue4") +  
  geom_segment(aes(x = 0, y = 0.5, xend = 8.7, yend = 0.5), linetype = 1, color = "dodgerblue4") +  
  geom_segment(aes(x = 0, y = 0.25, xend = 8.7, yend = 0.25), linetype = 1, color = "dodgerblue4") +  
  geom_segment(aes(x = 0, y = 0, xend = 10, yend = 0), linetype = 1, color = "dodgerblue4") +  
  annotate("text", label = expression(hat(l[Aj])(0)), x = 1.7, y = -0.07, size = 3.5, col = "firebrick2") +
  annotate("text", label = expression(hat(l[Aj])(1) * "=" * hat(r[Aj])(1)), x = 7, y = -0.07, size = 3.5, col = "firebrick2") +
  annotate("text", label = expression(hat(r[Aj])(0)), x = 9.6, y = -0.07, size = 3.5, col = "firebrick2") +
  geom_segment(data = df_tri_l, aes(x = x1, y = y1, xend = x2, yend = y2), col = "firebrick2", size = 1.2, alpha = 0.8) + 
  geom_segment(data = df_tri_r, aes(x = x1, y = y1, xend = x2, yend = y2), col = "firebrick2", size = 1.2, alpha = 0.8) +
    scale_x_continuous(breaks=c()) +
  scale_y_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1)) +
  labs(x = " ") +
  #labs(x = expression(A[j])) + 
  labs(y = expression(alpha)) +
  theme(
    panel.background = element_rect(fill = NA),
    panel.grid = element_blank(),
    panel.ontop = TRUE
  )
dev.off()
