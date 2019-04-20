mean_x = 7.5
mean_y = 14.26302
sxx <- 42
sxy <- 95.15092
syy <- 292.4565
sse = 76.89232
n = 8

# part a: beta hat
b_hat <- sxy / sxx
b_hat #=> 2.265498

# part b: alpha hat
a_hat <- mean_y - b_hat * mean_x
a_hat #=> -2.728216

# part d: theta_hat
theta_hat <- sse / (8-2)
theta_hat  #=> 12.81539

# part e: standard error of b_hat
se_b_hat <- sqrt(theta_hat/sxx)
se_b_hat  #=> 0.5523842

# part f: standard error of b_hat
t <- b_hat/se_b_hat
t #=> 23.20013

# part g: 
u <- qt(0.975, 6)
u #=> 2.446912

# part h
r_sqrd <- (sxy*sxy)/sxx/syy
r_sqrd #=> 0.7370813