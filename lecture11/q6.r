# part a: expected values
dataset.x = c(0, 1, 2, 3, 4, 5, 6)
dataset.observed <- c(5, 20, 19, 19, 19, 8, 10)
dataset.total <- sum(dataset.observed)
dataset.mean <- 3.5
dataset.total <- sum(dataset.observed)
dataset.rows_len <- length(dataset.x)
dataset.expected <- unlist(lapply(dataset.x, function(i) { dpois(i, dataset.mean) * dataset.total } ))
dataset.expected[dataset.rows_len] <- (1 -  ppois(dataset.rows_len-2, dataset.mean)) * dataset.total
dataset.expected #=> 3.019738 10.569084 18.495897 21.578547 18.881229 13.216860 14.238645

# part b: pearson statistic
dataset.pearson_statistic <- (dataset.observed - dataset.expected)^2/dataset.expected
dataset.pearson_statistic 
#=> [1] 1.2986013319 8.4153150077 0.0137392353 0.3081256660 0.0007471261
#=> [6] 2.0591598973 1.2617850366 

# part c: degree of freedom
dataset.degree_of_freedom <-  (length(dataset.observed) - 1) * (2 -1) 
dataset.degree_of_freedom #=>  6

# part d: pearson test
dataset.chi_sqred <- sum(dataset.pearson_statistic)
dataset.chi_sqred #=> 13.35747

dataset.chi_squared_probability <- 1- pchisq(dataset.chi_sqred, df = dataset.degree_of_freedom)
dataset.chi_squared_probability #=> 0.0376977