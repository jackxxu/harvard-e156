n <- 11
var_p <- 35
alpha <- 0.05

# part b
v <- qchisq(alpha/2,n-1)
v #=> 3.246973

w <- qchisq(1-alpha/2,n-1)
w #=> 20.48318

# part c
l <- var_p * v /(n-1)
l #=> 11.3644

u <- var_p * w /(n-1)
u #=> 71.69112

# part e
sample_variance <- 58.7
conf_intv_l <- sample_variance*(n-1)/w
conf_intv_l #=> 28.65766
conf_intv_u <- sample_variance*(n-1)/v
conf_intv_u #=> 180.7838