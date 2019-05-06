n1 <- 1000
n2 <- 500
p1 <- 38/n1
p2 <- 31/n2
alpha <- 0.05

d <- p1 - p2
d #=> -0.024

p_hat <- (n1*p1 + n2*p2)/(n1 + n2) #=> 0.046

d_ste <- sqrt(p_hat*(1-p_hat)*(1/n1 + 1/n2))
d_ste #=> 0.01147397

z <- (p1 - p0) / d_ste
z #=> 2.004537

p <- (1-pnorm(z)) *2
p #=> 0.04501255

# problem 4

d_ste_2 <- sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
d_ste_2 #=> 0.01236398

l <- d + qnorm(alpha/2) * d_ste_2
l #=> -0.04823296
w <- d + qnorm(1- alpha/2) * d_ste_2
w #=> 0.0002329554