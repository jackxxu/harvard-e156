mu_0 <- 131
mu_a <- 122
n <- 14
var_p <- 144
alpha <- 0.05

# part a
sigma = sqrt(var_p)
sem <- sqrt(var_p / 14)
sem #-> 3.207135

# part b
L <- mu_0 + qnorm(alpha/2) * sem
L #=> 124.7141
U <- mu_0 - qnorm(alpha/2) * sem
U #-> 137.2859

# part c
type2_err_rt <- 1 - pnorm((L - mu_a)/sem)
type2_err_rt # -> 0.1986985
power  <- 1 - type2_err_rt
power #-> 0.8013015

# part d
d <- (mu_a - mu_0)/sigma
d #-> -0.75
