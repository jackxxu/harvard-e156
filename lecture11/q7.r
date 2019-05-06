# part a: expected values
dataset.x = c(0, 1, 2, 3, 4, 5, 6)
dataset.observed <- c(5, 20, 19, 19, 19, 8, 10)
dataset.total <- sum(dataset.observed)
dataset.mean <- 2.98
dataset.total <- sum(dataset.observed)
dataset.rows_len <- length(dataset.x)
dataset.expected <- unlist(lapply(dataset.x, function(i) { dpois(i, dataset.mean) * dataset.total } ))
dataset.expected[dataset.rows_len] <- (1 -  ppois(dataset.rows_len-2, dataset.mean)) * dataset.total
dataset.expected #=> 5.079283 15.136264 22.553034 22.402681 16.689997  9.947238  8.191502

# part b: pearson statistic
dataset.pearson_statistic <- (dataset.observed - dataset.expected)^2/dataset.expected
dataset.pearson_statistic
#=> [1] 0.001237548 1.562864015 0.559749576 0.516823636 0.319719283 0.381184864
#=> [7] 0.399275230

# part c: degree of freedom
dataset.degree_of_freedom <-  (length(dataset.observed) - 1) * (2 -1) - 1
dataset.degree_of_freedom #=>  5

# part d: pearson test
dataset.chi_sqred <- sum(dataset.pearson_statistic)
dataset.chi_sqred #=> 3.740854

dataset.chi_squared_probability <- 1- pchisq(dataset.chi_sqred, df = dataset.degree_of_freedom)
dataset.chi_squared_probability #=> 0.5872962