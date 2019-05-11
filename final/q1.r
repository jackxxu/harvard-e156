dataset.observed <- c(90.2, 95.1, 89.0, 99.0, 96.0, 89.1, 96.9, 98.4, 97.5, 92.2)
dataset.size <- length(dataset.observed)
dataset.mu_0 <- 90
alpha <- 0.05

# part b: Calculate the sample mean and sample variance for this data.
dataset.mean <- mean(dataset.observed)
dataset.mean #=> 94.34
dataset.var_sample <- sum(unlist(lapply(dataset.observed, function(i) { (i - dataset.mean) ^2 } )))/(dataset.size - 1)
dataset.var_sample #=> 15.08489

# part c: Conduct a hypothesis test for the null hypothesis in part (a).
dataset.se <- sqrt(dataset.var_sample/dataset.size)
dataset.se #=> 1.228206

dataset.t <- (dataset.mean - dataset.mu_0)/sqrt(dataset.var_sample/dataset.size)
dataset.t #=> 3.53361

dataset.p = (1 - pt(dataset.t, dataset.size-1)) * 2
dataset.p #=> 0.006377709

# part d: Construct a 95% confidence interval for the true mean customer satisfaction rating using this data.
dataset.l <- dataset.mean + qt(alpha/2, dataset.size - 1)*sqrt(dataset.var_sample/dataset.size)
dataset.l #=> 91.56161
dataset.u <- dataset.mean - qt(alpha/2, dataset.size - 1)*sqrt(dataset.var_sample/dataset.size)
dataset.u #=> 97.11839
