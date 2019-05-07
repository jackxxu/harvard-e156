dataset.a.observed <- c(139.8, 148.3, 151.2, 142.1, 127.6, 126.1, 145.8, 154.0, 134.0, 159.4)
dataset.b.observed <- c(158.4, 147.6, 149.3, 157.5, 162.6, 152.2, 137.5, 158.4, 175.0, 144.9)
alpha <- 0.05

# part b sample mean and sample variance
dataset.a.mean <- mean(dataset.a.observed)
dataset.a.mean #=> 142.83
dataset.a.size <- length(dataset.a.observed)
dataset.b.mean <- mean(dataset.b.observed)
dataset.b.mean #=> 154.34
dataset.b.size <- length(dataset.b.observed)
dataset.a.var_sample <- sum(unlist(lapply(dataset.a.observed, function(i) { (i - dataset.a.mean) ^2 } )))/(dataset.a.size - 1)
dataset.a.var_sample #=> 123.0734
dataset.b.var_sample <- sum(unlist(lapply(dataset.b.observed, function(i) { (i - dataset.b.mean) ^2 } )))/(dataset.b.size - 1)
dataset.b.var_sample #=> 109.5693

# part c: pooled variance
dataset.var_pooled <- dataset.a.var_sample * (dataset.a.size-1)/(dataset.a.size + dataset.b.size - 2) +
                      dataset.b.var_sample * (dataset.b.size-1)/(dataset.a.size + dataset.b.size - 2)
dataset.var_pooled #=> 116.3214

# part d: t stats
dataset.df_pooled <- dataset.a.size + dataset.b.size - 2
dataset.mean <- dataset.a.mean - dataset.b.mean
dataset.mean #=> -11.51
dataset.t <- dataset.mean/sqrt(dataset.var_pooled*(1/(dataset.a.size) + 1/(dataset.b.size)))
dataset.t #=> -2.38633

# part e: p value
dataset.p <- (1 - pt(-dataset.t, dataset.df_pooled)) * 2
dataset.p #=> 0.02820656

# part f:
dataset.l <- dataset.mean + qt(alpha/2, dataset.df_pooled)*sqrt(dataset.var_pooled*(1/(dataset.a.size) + 1/(dataset.b.size)))
dataset.l #=> -21.64339
dataset.u <- dataset.mean - qt(alpha/2, dataset.df_pooled)*sqrt(dataset.var_pooled*(1/(dataset.a.size) + 1/(dataset.b.size)))
dataset.u #=> -1.376611
