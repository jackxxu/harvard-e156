dataset.a.observed <- c(5037.1, 4913.9, 5148.2, 5061.8, 4949.4, 4945.9, 4965.5, 4997.9, 5009.4, 4876.1)
dataset.b.observed <- c(4731.7, 5038.0, 4701.8, 4853.1, 4902.8, 5211.9, 5342.1, 5082.8, 4937.8, 4757.0)

# part b sample mean and sample variance
dataset.a.mean <- mean(dataset.a.observed)
dataset.a.mean #=> 4990.52
dataset.a.size <- length(dataset.a.observed)
dataset.b.mean <- mean(dataset.b.observed)
dataset.b.mean #=> 4955.9
dataset.b.size <- length(dataset.b.observed)
dataset.a.var_sample <- sum(unlist(lapply(dataset.a.observed, function(i) { (i - dataset.a.mean) ^2 } )))/(dataset.a.size - 1)
dataset.a.var_sample #=> 6199.422
dataset.b.var_sample <- sum(unlist(lapply(dataset.b.observed, function(i) { (i - dataset.b.mean) ^2 } )))/(dataset.b.size - 1)
dataset.b.var_sample #=> 45071.02

# part c: pooled variance
dataset.var_pooled <- dataset.a.var_sample * (dataset.a.size-1)/(dataset.a.size + dataset.b.size - 2) + 
                      dataset.b.var_sample * (dataset.b.size-1)/(dataset.a.size + dataset.b.size - 2)
dataset.var_pooled #=> 25635.22

# part d: t stats
dataset.df_pooled <- dataset.a.size + dataset.b.size - 2
dataset.mean <- dataset.a.mean - dataset.b.mean
dataset.mean #=> 34.62
dataset.t <- dataset.mean/sqrt(dataset.var_pooled*(1/(dataset.a.size) + 1/(dataset.b.size)))
dataset.t #=> 0.4834967

# part e: p value
dataset.p <- pt(-dataset.t, dataset.df_pooled) * 2
dataset.p #=> 0.6345723

# part f: 
dataset.l <- dataset.mean + qt(alpha/2, dataset.df_pooled)*sqrt(dataset.var_pooled*(1/(dataset.a.size) + 1/(dataset.b.size)))
dataset.l #=> -115.8131
dataset.u <- dataset.mean - qt(alpha/2, dataset.df_pooled)*sqrt(dataset.var_pooled*(1/(dataset.a.size) + 1/(dataset.b.size)))
dataset.u #=> 185.0531
