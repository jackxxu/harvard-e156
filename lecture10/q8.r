data.x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
data.y <- c(23.5, 20.7, 28.0, 15.4, 16.7, 14.6, 16.0, 12.7, 10.9)

x_mean <- mean(data.x)
x_mean #=> 5
y_mean <- mean(data.y)
y_mean #=> 17.61111

sxx <- sum(unlist(lapply(data.x, function(i) { (i - x_mean) ^2 } )))
sxx #=> 60
syy <- sum(unlist(lapply(data.y, function(i) { (i - x_mean) ^2 } )))
syy #=> 1670.05

sxy <- 0
for (i in 1:length(data.x)) {
  sxy <- sxy + (data.x[i] - x_mean) * (data.y[i] - y_mean)
}
sxy #=> -99.2

b_hat <- sxy/sxx
b_hat #=> -1.653333

a_hat <- y_mean - (b_hat * x_mean)
a_hat #=> 25.87778

theta_sq <- sum(unlist(lapply(data.y, function(i) { (i - y_mean)^2 } ))) / (length(data.y) -1)
var_beta_hat <- theta_sq/sxx
var_beta_hat #=> 0.4972685

a <- 0.05
v <- qt(a/2, length(data.y) - 2)
v #=> -2.364624
w <- qt(1-a/2, length(data.y) - 2)
w #=> 2.364624

r_sqt <- sxy^2/(sxx * syy)
r_sqt #=> 0.8957125

