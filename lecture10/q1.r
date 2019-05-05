ua = 95
u0 = 85
var_p = 400

# part a: D

d <- (ua - u0)/sqrt(var_p)
d #=> 0.5

# part b: require sample size

(sqrt(var_p) * (qnorm(0.025) + qnorm(0.1)) / (ua -u0)) ^ 2 #=> 42.02969

# part c: D in Fahrenheit

c_2_f <- function(c) { return (9.0/5 * c + 32) }

ua_f = c_2_f(ua)
u0_f = c_2_f(u0)
var_p_f = (9.0/5)^2 * var_p
(ua_f - u0_f)/sqrt(var_p_f) #=> 0.5

# part a:
