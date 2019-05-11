
dataset.x = c(0, 1, 2, 3, 4, 5, 6, 7)
dataset.observed <- c(5, 15, 33, 59, 46, 26, 13, 3)
dataset.total <- sum(dataset.observed)

p_hat <- 0.48137
size <- length(dataset.x)
# part a: calculate the expected counts for each category.

dataset.mean <- sum(dataset.x * dataset.observed)/sum(dataset.observed)
dataset.mean #=> 3.355

# part b: expected counts
dataset.total <- sum(dataset.observed)
dataset.rows_len <- length(dataset.x)
dataset.expected <- unlist(lapply(dataset.x, function(i) { dbinom(i, size=size-1, prob=p_hat) * dataset.total } ))
dataset.expected #=>  2.018522 13.114534 36.517035 56.489228 52.430866 29.198441  9.033578 1.197797

# part b: pearson statistic
dataset.pearson_statistic <- (dataset.observed - dataset.expected)^2/dataset.expected
dataset.pearson_statistic
#=> 4.4038229 0.2710719 0.3387333 0.1115961 0.7887726 0.3503620 1.7415585 2.7115923

# part c: degree of freedom
dataset.degree_of_freedom <-  (length(dataset.observed) - 1) * (2 -1) - 1
dataset.degree_of_freedom #=>  6

# part d: pearson test
dataset.chi_sqred <- sum(dataset.pearson_statistic)
dataset.chi_sqred #=> 10.71751

dataset.chi_squared_probability <- 1- pchisq(dataset.chi_sqred, df = dataset.degree_of_freedom)
dataset.chi_squared_probability #=> 0.09750946