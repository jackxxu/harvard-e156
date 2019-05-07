dataset.observed <- c(33.8, 36.6, 34.6, 39.3, 28.9, 34.9, 30.0, 31.4, 37.8)
dataset.size <- length(dataset.observed)
dataset.mu_0 <- 35
alpha <- 0.05

# part b
dataset.mean <- mean(dataset.observed)
dataset.mean #=> 34.14444
dataset.var_sample <- sum(unlist(lapply(dataset.observed, function(i) { (i - dataset.mean) ^2 } )))/(dataset.size - 1)
dataset.var_sample #=> 12.38528

# part c: standard error
dataset.se <- sqrt(dataset.var_sample/dataset.size)
dataset.se #=> 1.173091

# part d: t-value
dataset.t <- (dataset.mean - dataset.mu_0)/sqrt(dataset.var_sample/dataset.size)
dataset.t #=> -0.7293174

# part e: p-value
dataset.p = pt(dataset.t, dataset.size-1) * 2
dataset.p #=> 0.4866087

# part e: confidence internal
dataset.l <- dataset.mean + qt(alpha/2, dataset.size - 1)*sqrt(dataset.var_sample/dataset.size)
dataset.l #=> 31.43929
dataset.u <- dataset.mean - qt(alpha/2, dataset.size - 1)*sqrt(dataset.var_sample/dataset.size)
dataset.u #=> 36.8496
