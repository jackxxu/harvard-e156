data.x <- c(12, 14, 17, 18, 19)
data.y <- c(27, 21, 12, 15, 5)

# part a, mean of x
x_mean <- mean(data.x)
x_mean #=> 16

# part b, mean of y
y_mean <- mean(data.y)
y_mean #=> 16

# part c: sxx
sxx <- sum(unlist(lapply(data.x, function(i) { (i - x_mean) ^2 } )))
sxx #=> 34

# part d: sxy
sxy <- 0
for (i in 1:length(data.x)) {
  sxy <- sxy + (data.x[i] - x_mean) * (data.y[i] - y_mean)
}
sxy #=> -93

# part e: beta hat
b_hat <- sxy/sxx
b_hat #=> -2.735294

# part f: alpha hat
a_hat <- y_mean - (b_hat * x_mean)
a_hat #=> 59.76471

# part g:
y_hat_x_12 <- b_hat * 12 + a_hat
y_hat_x_12 #=> 26.94118

# part 4.a
y_hat <- b_hat * data.x + a_hat
y_hat #=> [26.941176 21.470588 13.264706 10.529412  7.794118]

# part 4.b
y_minus_y_hat <- data.y - y_hat
y_minus_y_hat #=> [0.05882353 -0.47058824 -1.26470588  4.47058824 -2.79411765]

# part 4.c
theta_sq_hat <- sum(unlist(lapply(y_minus_y_hat, function(i) { i^2 } ))) / (length(data.x) -2)
theta_sq_hat # => 9.872549