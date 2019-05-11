# observed = c(128.8, 135.3, 145.1, 120.3, 116.4, 125.4, 134.6, 134.1, 128.2)
# μ0 <- 125
# mean = mean(observed)
α <- 0.05
β <- 0.10
# size = length(observed)

# var_sample <- sum(unlist(lapply(observed, function(i) { (i - mean) ^2 } )))/(size - 1)
# d = (mean - μ0)/sigma

d = 0.1
n <- ((qnorm(α/2) + qnorm(β)) / d )^2
n #=> 1050.742 => 1051