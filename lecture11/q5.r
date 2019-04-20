# part a: expected values
dataset.observed <- c(33, 41, 28, 33, 15)
dataset.total <- sum(dataset.observed)
dataset.proportions <- c(4, 4, 3, 3, 1)
dataset.proportion_ratios <- dataset.proportions / sum(dataset.proportions)
dataset.expected <- dataset.proportion_ratios * dataset.total
dataset.expected #=> 40 40 30 30 10

# part b: pearson statistic
dataset.pearson_statistic <- (dataset.observed - dataset.expected)^2/dataset.expected
dataset.pearson_statistic #=> 1.2250000 0.0250000 0.1333333 0.3000000 2.5000000

# part c: degree of freedom
dataset.degree_of_freedom <-  (length(dataset.observed) - 1) * (2 -1) 
dataset.degree_of_freedom #=>  4

# part d: pearson test
dataset.chi_sqred <- sum(dataset.pearson_statistic)
dataset.chi_sqred #=> 4.183333

dataset.chi_squared_probability <- 1- pchisq(dataset.chi_sqred, df = dataset.degree_of_freedom)
dataset.chi_squared_probability #=> 0.3817626