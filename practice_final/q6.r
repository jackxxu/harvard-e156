data.x <- c(2, 4, 6, 8, 10, 12, 14, 16, 18)
data.y <- c(7.9, 9.3, 12.3, 17.7, 21.9, 22.5, 23.6, 28.4, 30.3)

x_mean <- mean(data.x)
x_mean #=> 10

y_mean <- mean(data.y)
y_mean #=> 19.32222

# part a: beta hat and alpha hat
sxx <- sum(unlist(lapply(data.x, function(i) { (i - x_mean) ^2 } )))
sxx #=> 240

sxy <- 0
for (i in 1:length(data.x)) {
  sxy <- sxy + (data.x[i] - x_mean) * (data.y[i] - y_mean)
}
sxy #=> 348.6

b_hat <- sxy/sxx
b_hat #=> 1.4525

# part f: alpha hat
a_hat <- y_mean - (b_hat * x_mean)
a_hat #=> 4.797222

# part b. estimated variance
y_hat <- b_hat * data.x + a_hat
y_hat #=> [7.702222 10.607222 13.512222 16.417222 19.322222 22.227222 25.132222 28.037222 30.942222]

y_minus_y_hat <- data.y - y_hat
y_minus_y_hat #=> [0.1977778 -1.3072222 -1.2122222  1.2827778  2.5777778  0.2727778 -1.5322222 0.3627778 -0.6422222]

sigma_sq_hat <- sum(unlist(lapply(y_minus_y_hat, function(i) { i^2 } ))) / (length(data.y) -2)
sigma_sq_hat # => 2.067722

# part c
var_beta_hat <- sigma_sq_hat/sxx
var_beta_hat #=> 0.008615509

t <- b_hat / sqrt(var_beta_hat)
t # => 15.64861

p <- (1-pt(t, length(data.y) - 2)) *2
p #=> 1.052818e-06

# part d
a <- 0.05
v <- qt(a/2, length(data.y) - 2)
v #=> -2.364624
w <- qt(1-a/2, length(data.y) - 2)
w #=> 2.364624

l <- b_hat + v * sqrt(var_beta_hat)
l #=> 1.233016
u <- b_hat + w * sqrt(var_beta_hat)
u #=> 1.671984

# part e
syy <- sum(unlist(lapply(data.y, function(i) { (i - y_mean) ^2 } )))
syy #=> 520.8156

r_sqt <- sxy^2/(sxx * syy)
r_sqt #=> 0.9722089