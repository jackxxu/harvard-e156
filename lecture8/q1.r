mu_0 <- 63
mu_a <- 57
n <- 14
var_p <- 64
alpha <- 0.05

# part a
sigma = sqrt(var_p)
sem <- sqrt(var_p / 14)
sem #-> 2.13809

# part b
L <- mu_0 + qnorm(alpha/2) * sem
L #=> 58.80942
U <- mu_0 - qnorm(alpha/2) * sem
U #-> 67.19058

# part c
type2_err_rt <- 1 - pnorm((L - mu_a)/sem)
type2_err_rt # -> 0.1986985
power  <- 1 - type2_err_rt
power #-> 0.8013015

# part d
d <- (mu_a - mu_0)/sigma
d #-> -0.75
