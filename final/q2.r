dataset.a.observed <- c(221.0, 231.8, 245.9, 218.7, 229.2, 231.3, 237.1, 227.6, 249.8, 228.6, 234.2, 239.8)
dataset.b.observed <- c(236.1, 229.6, 257.8, 216.9, 248.8, 240.4, 250.1, 244.3, 260.9, 228.0)
alpha <- 0.05

# part b: What are the observed sample means and sample variances for the two groups,
# as well as the observed sample mean difference between the two groups?
dataset.a.mean <- mean(dataset.a.observed)
dataset.a.mean #=> 232.9167
dataset.a.size <- length(dataset.a.observed) #=> 12
dataset.b.mean <- mean(dataset.b.observed)
dataset.b.mean #=> 241.29
dataset.b.size <- length(dataset.b.observed) #=> 10
dataset.a.var_sample <- sum(unlist(lapply(dataset.a.observed, function(i) { (i - dataset.a.mean) ^2 } )))/(dataset.a.size - 1)
dataset.a.var_sample #=> 84.4397
dataset.b.var_sample <- sum(unlist(lapply(dataset.b.observed, function(i) { (i - dataset.b.mean) ^2 } )))/(dataset.b.size - 1)
dataset.b.var_sample #=> 192.8988

dataset.mean_diff <- dataset.a.mean - dataset.b.mean
dataset.mean_diff #=> 8.373333

# part c: pooled variance
dataset.var_pooled <- dataset.a.var_sample * (dataset.a.size-1)/(dataset.a.size + dataset.b.size - 2) +
                      dataset.b.var_sample * (dataset.b.size-1)/(dataset.a.size + dataset.b.size - 2)
dataset.var_pooled #=> 133.2463

# part d: t stats
dataset.df_pooled <- dataset.a.size + dataset.b.size - 2
dataset.mean <- dataset.a.mean - dataset.b.mean
dataset.mean #=> -8.373333
dataset.t <- dataset.mean/sqrt(dataset.var_pooled*(1/(dataset.a.size) + 1/(dataset.b.size)))
dataset.t #=> -1.694143

# part e: p value
dataset.p <- (1 - pt(-dataset.t, dataset.df_pooled)) * 2
dataset.p #=> 0.105755

# part f:
dataset.l <- dataset.mean + qt(alpha/2, dataset.df_pooled)*sqrt(dataset.var_pooled*(1/(dataset.a.size) + 1/(dataset.b.size)))
dataset.l #=> -18.68325
dataset.u <- dataset.mean - qt(alpha/2, dataset.df_pooled)*sqrt(dataset.var_pooled*(1/(dataset.a.size) + 1/(dataset.b.size)))
dataset.u #=> 1.936579
