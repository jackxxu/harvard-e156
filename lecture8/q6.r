dataset.observed <- c(32.8, 26.9, 31.2, 28.8, 33.1, 32.0, 33.6)

dataset.size <- length(dataset.observed)
dataset.mu_0 <- 32
alpha <- 0.05

# part b 
dataset.mean <- mean(dataset.observed)
dataset.mean #=> 31.2
dataset.var_sample <- sum(unlist(lapply(dataset.observed, function(i) { (i - dataset.mean) ^2 } )))/(dataset.size - 1)
dataset.var_sample #=> 6.136667

# part c :rejection region
dataset.v <- dataset.mu_0 + qt(alpha/2, dataset.size - 1)*sqrt(dataset.var_sample/dataset.size)
dataset.v #=> 29.70894
dataset.w <- dataset.mu_0 - qt(alpha/2, dataset.size - 1)*sqrt(dataset.var_sample/dataset.size)
dataset.w #=> 34.29106

# part d: p-value
dataset.t <- (dataset.mean - dataset.mu_0)/sqrt(dataset.var_sample/dataset.size)
dataset.t #=> -0.8544226

dataset.p <- pt(dataset.t, dataset.size-1) * 2
dataset.p #=> 0.4256648

# part e: confidence internal 

dataset.l <- dataset.mean + qt(alpha/2, dataset.size - 1)*sqrt(dataset.var_sample/dataset.size)
dataset.l #=> 28.90894
dataset.u <- dataset.mean - qt(alpha/2, dataset.size - 1)*sqrt(dataset.var_sample/dataset.size)
dataset.u #=> 33.49106
