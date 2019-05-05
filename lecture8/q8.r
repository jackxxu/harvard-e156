dataset.a.observed <- c(4843.9, 4969.9, 4712.5, 5246.1, 4831.1, 5108.7, 4947.0, 4921.0, 5309.9, 5031.8)
dataset.b.observed <- c(5418.7, 5384.6, 5127.8, 5064.5, 5138.6, 5268.5, 5370.7, 5162.5, 5109.3, 5217.0)
alpha <- 0.05 

# part b sample mean and sample variance
dataset.a.mean <- mean(dataset.a.observed)
dataset.a.mean #=> 4992.19
dataset.a.size <- length(dataset.a.observed)
dataset.b.mean <- mean(dataset.b.observed)
dataset.b.mean #=> 5226.22
dataset.b.size <- length(dataset.b.observed)
dataset.a.var_sample <- sum(unlist(lapply(dataset.a.observed, function(i) { (i - dataset.a.mean) ^2 } )))/(dataset.a.size - 1)
dataset.a.var_sample #=> 34925.21
dataset.b.var_sample <- sum(unlist(lapply(dataset.b.observed, function(i) { (i - dataset.b.mean) ^2 } )))/(dataset.b.size - 1)
dataset.b.var_sample #=> 16236.39

# part c: pooled variance
dataset.var_pooled <- dataset.a.var_sample * (dataset.a.size-1)/(dataset.a.size + dataset.b.size - 2) + 
                      dataset.b.var_sample * (dataset.b.size-1)/(dataset.a.size + dataset.b.size - 2)
dataset.var_pooled #=> 25580.8

# part d: t stats
dataset.df_pooled <- dataset.a.size + dataset.b.size - 2
dataset.mean <- dataset.a.mean - dataset.b.mean
dataset.mean #=> -234.03
dataset.t <- dataset.mean/sqrt(dataset.var_pooled*(1/(dataset.a.size) + 1/(dataset.b.size)))
dataset.t #=> -3.271896

# part e: p value
dataset.p <- pt(dataset.t, dataset.df_pooled) * 2
dataset.p #=> 0.004235972

# part f: 
dataset.l <- dataset.mean + qt(alpha/2, dataset.df_pooled)*sqrt(dataset.var_pooled*(1/(dataset.a.size) + 1/(dataset.b.size)))
dataset.l #=> -384.3034
dataset.u <- dataset.mean - qt(alpha/2, dataset.df_pooled)*sqrt(dataset.var_pooled*(1/(dataset.a.size) + 1/(dataset.b.size)))
dataset.u #=> -83.75665
