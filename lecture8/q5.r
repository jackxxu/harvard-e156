dataset.observed <- c(28.7, 29.9, 30.7, 31.2, 23.6, 31.2, 27.4)
dataset.size <- length(dataset.observed)
dataset.mu_0 <- 32
alpha <- 0.05

# part b 
dataset.mean <- mean(dataset.observed)
dataset.mean #=> 28.95714
dataset.var_sample <- sum(unlist(lapply(dataset.observed, function(i) { (i - dataset.mean) ^2 } )))/(dataset.size - 1)
dataset.var_sample #=> 7.529524

# part c :rejection region
dataset.v <- dataset.mu_0 + qt(alpha/2, dataset.size - 1)*sqrt(dataset.var_sample/dataset.size)
dataset.v #=> 29.46223
dataset.w <- dataset.mu_0 - qt(alpha/2, dataset.size - 1)*sqrt(dataset.var_sample/dataset.size)
dataset.w #=> 34.53777

# part d: p-value
dataset.t <- (dataset.mean - dataset.mu_0)/sqrt(dataset.var_sample/dataset.size)
dataset.t #=> -2.93391

dataset.p = pt(dataset.t, dataset.size-1) * 2
dataset.p #=> 0.02615378


# part e: confidence internal 

dataset.l <- dataset.mean + qt(alpha/2, dataset.size - 1)*sqrt(dataset.var_sample/dataset.size)
dataset.l #=> 26.41937
dataset.u <- dataset.mean - qt(alpha/2, dataset.size - 1)*sqrt(dataset.var_sample/dataset.size)
dataset.u #=> 31.49492
