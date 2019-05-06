μ0 <- 93
μA <- 96
α <- 0.05 
β <- 0.10 
σ2 <- 144

# part a 
n <- ((qnorm(α/2) + qnorm(β)) / (μ0 - μA) )^2 * σ2
n #=> 168.1188

# part b

μ0 <- 21
μA <- 22
α <- 0.05 
β <- 0.10 
σ2 <- 16

n <- ((qnorm(α/2) + qnorm(β)) / (μ0 - μA) )^2 * σ2
n #=> 168.1188