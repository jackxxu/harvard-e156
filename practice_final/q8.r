
dataset.x = c(0, 1, 2, 3, 4, 5, 6, 7)
dataset.observed <- c(8, 15, 11, 7, 6, 1, 2, 0)
dataset.total <- sum(dataset.observed)

# part a: estimator for mu
dataset.mean <- sum(dataset.x * dataset.observed)/sum(dataset.observed)
dataset.mean #=> 1.98

# part b: expected counts
dataset.total <- sum(dataset.observed)
dataset.rows_len <- length(dataset.x)
dataset.expected <- unlist(lapply(dataset.x, function(i) { dpois(i, dataset.mean) * dataset.total } ))
dataset.expected[dataset.rows_len] <- (1 -  ppois(dataset.rows_len-2, dataset.mean)) * dataset.total
dataset.expected #=> 6.9034619 13.6688545 13.5321659  8.9312295  4.4209586  1.7506996  0.5777309 0.2148991

# part b: pearson statistic
dataset.pearson_statistic <- (dataset.observed - dataset.expected)^2/dataset.expected
dataset.pearson_statistic
#=> 0.1741729 0.1296340 0.4738240 0.4175962 0.5639889 0.3218998 3.5013699 0.2148991

# part c: degree of freedom
dataset.degree_of_freedom <-  (length(dataset.observed) - 1) * (2 -1) - 1
dataset.degree_of_freedom #=>  6

# part d: pearson test
dataset.chi_sqred <- sum(dataset.pearson_statistic)
dataset.chi_sqred #=> 5.797385

dataset.chi_squared_probability <- 1- pchisq(dataset.chi_sqred, df = dataset.degree_of_freedom)
dataset.chi_squared_probability #=> 0.4462658