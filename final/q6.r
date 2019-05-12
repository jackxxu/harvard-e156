data.x <- c(2, 4, 6, 8, 10, 12, 14)
data.y <- c(9.1, 8.5, 17.5, 23.9, 20.1, 24.5, 20.8)

x_mean <- mean(data.x)
x_mean #=> 8

y_mean <- mean(data.y)
y_mean #=> 17.77143

# part a: beta hat and alpha hat
sxx <- sum(unlist(lapply(data.x, function(i) { (i - x_mean) ^2 } )))
sxx #=> 112

sxy <- 0
for (i in 1:length(data.x)) {
  sxy <- sxy + (data.x[i] - x_mean) * (data.y[i] - y_mean)
}
sxy #=> 139.4

b_hat <- sxy/sxx
b_hat #=> 1.244643

# part f: alpha hat
a_hat <- y_mean - (b_hat * x_mean)
a_hat #=> 7.814286

# part b. estimated variance
y_hat <- b_hat * data.x + a_hat
y_hat #=> [10.30357 12.79286 15.28214 17.77143 20.26071 22.75000 25.23929]

y_minus_y_hat <- data.y - y_hat
y_minus_y_hat #=> [-1.2035714 -4.2928571  2.2178571  6.1285714 -0.1607143  1.7500000 -4.4392857]

sigma_sq_hat <- sum(unlist(lapply(y_minus_y_hat, function(i) { i^2 } ))) / (length(data.y) -2)
sigma_sq_hat # => 17.03021

# part c: Conduct a hypothesis test to determine if there is an association between X and Y
var_beta_hat <- sigma_sq_hat/sxx
var_beta_hat #=> 0.1520555

t <- b_hat / sqrt(var_beta_hat)
t # => 3.191859

p <- (1-pt(t, length(data.y) - 2)) *2
p #=> 0.02421457

# part d: Construct a 95% confidence interval for the slope coefficient β.
a <- 0.05
v <- qt(a/2, length(data.y) - 2)
v #=> -2.570582
w <- qt(1-a/2, length(data.y) - 2)
w #=> 2.570582

l <- b_hat + v * sqrt(var_beta_hat)
l #=> 0.2422627
u <- b_hat + w * sqrt(var_beta_hat)
u #=> 2.247023

# part e: What proportion of the total variance is explained by the linear regression line?
syy <- sum(unlist(lapply(data.y, function(i) { (i - y_mean) ^2 } )))
syy #=> 258.6543

r_sqt <- sxy^2/(sxx * syy)
r_sqt #=> 0.670792