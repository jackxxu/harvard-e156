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

# part 4.c estimated variance
sigma_sq_hat <- sum(unlist(lapply(y_minus_y_hat, function(i) { i^2 } ))) / (length(data.y) -2)
sigma_sq_hat # => 9.872549

# part 5.a
var_beta_hat <- sigma_sq_hat/sxx
var_beta_hat #=> 0.2903691

# part 5.c
t <- b_hat / sqrt(var_beta_hat)
t # => -5.076084

# part 5.d
p <- pt(t, length(data.y) - 2) *2
p #=> 0.01476745

# part 6.a
a <- 0.05
v <- qt(a/2, length(data.y) - 2)
v #=> -3.182446
w <- qt(1-a/2, length(data.y) - 2)
w #=> 3.182446

# part 6.b
l <- b_hat + v * sqrt(var_beta_hat)
l #=> -4.450184
u <- b_hat + w * sqrt(var_beta_hat)
u #=> -1.020404

# part 7.a
syy <- sum(unlist(lapply(data.y, function(i) { (i - y_mean) ^2 } )))
syy #=> 284

# part 7.b
r_sqt <- sxy^2/(sxx * syy)
r_sqt #=> 0.8957125