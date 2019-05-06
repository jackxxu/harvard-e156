data.x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
data.y <- c(23.5, 20.7, 28.0, 15.4, 16.7, 14.6, 16.0, 12.7, 10.9)

# part a, mean of x
x_mean <- mean(data.x)
x_mean #=> 5

# part b, mean of y
y_mean <- mean(data.y)
y_mean #=> 17.61111

# part c: sxx
sxx <- sum(unlist(lapply(data.x, function(i) { (i - x_mean) ^2 } )))
sxx #=> 60

# part d: sxy
sxy <- 0
for (i in 1:length(data.x)) {
  sxy <- sxy + (data.x[i] - x_mean) * (data.y[i] - y_mean)
}
sxy #=> -99.2

# part e: beta hat
b_hat <- sxy/sxx
b_hat #=> -1.653333

# part f: alpha hat
a_hat <- y_mean - (b_hat * x_mean)
a_hat #=> 25.87778

# part g:
y_hat_x_12 <- b_hat * 12 + a_hat
y_hat_x_12 #=> 6.037778

# part 4.a
y_hat <- b_hat * data.x + a_hat
y_hat #=> [24.22444 22.57111 20.91778 19.26444 17.61111 15.95778 14.30444 12.65111 10.99778]

# part 4.b
y_minus_y_hat <- data.y - y_hat
y_minus_y_hat #=> [-0.72444444 -1.87111111  7.08222222 -3.86444444 -0.91111111 -1.35777778 1.69555556  0.04888889 -0.09777778]

# part 4.c estimated variance
sigma_sq_hat <- sum(unlist(lapply(y_minus_y_hat, function(i) { i^2 } ))) / (length(data.y) -2)
sigma_sq_hat # => 10.66832

# part 5.a
var_beta_hat <- sigma_sq_hat/sxx
var_beta_hat #=> 0.1778053

# part 5.c
t <- b_hat / sqrt(var_beta_hat)
t # => -3.920921

# part 5.d
p <- pt(t, length(data.y) - 2) *2
p #=> 0.005742241

# part 6.a
a <- 0.05
v <- qt(a/2, length(data.y) - 2)
v #=> -2.364624
w <- qt(1-a/2, length(data.y) - 2)
w #=> 2.364624

# part 6.b
l <- b_hat + v * sqrt(var_beta_hat)
l #=> -2.650424
u <- b_hat + w * sqrt(var_beta_hat)
u #=> -0.6562431

t <- b_hat / sqrt(var_beta_hat)
t # => -3.920921

p <- pt(t, length(data.y) - 2) *2
p #=> 0.005742241

# part 7.a
syy <- sum(unlist(lapply(data.y, function(i) { (i - y_mean) ^2 } )))
syy #=> 238.6889

# part 7.b
r_sqt <- sxy^2/(sxx * syy)
r_sqt #=> 0.6871316