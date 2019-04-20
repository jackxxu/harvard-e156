# part a: expected values
dataset.x = c(0, 1, 2, 3)
dataset.observed.defective <- c(38, 31)
dataset.observed.non_defective <- c(962, 469)
dataset.observed.defective_total <- sum(dataset.observed.defective)
dataset.observed.total <- sum(dataset.observed.defective) + sum(dataset.observed.non_defective)
dataset.observed.defective_mean <- dataset.observed.defective_total /dataset.observed.total
dataset.observed.column1_total <- dataset.observed.defective[1] + dataset.observed.non_defective[1]
dataset.observed.column2_total <- dataset.observed.defective[2] + dataset.observed.non_defective[2]

dataset.expected.defective = dataset.observed.defective_mean * c(dataset.observed.column1_total, dataset.observed.column2_total)
dataset.expected.defective #=> 46 23

dataset.expected.non_defective = (1 - dataset.observed.defective_mean) * c(dataset.observed.column1_total, dataset.observed.column2_total)
dataset.expected.non_defective #=> 954 477

# part b: pearson statistic
dataset.pearson_statistic.defective <- (dataset.observed.defective - dataset.expected.defective)^2/dataset.expected.defective
dataset.pearson_statistic.defective #=> 1.391304 2.782609

dataset.pearson_statistic.non_defective <- (dataset.observed.non_defective - dataset.expected.non_defective)^2/dataset.expected.non_defective
dataset.pearson_statistic.non_defective #=> 0.06708595 0.13417191

# part c: degree of freedom
dataset.degree_of_freedom <-  (length(dataset.observed.defective) - 1) * (length(dataset.observed.non_defective) - 1)
dataset.degree_of_freedom

# part d: pearson test
dataset.chi_sqred <- sum(dataset.pearson_statistic.non_defective) + sum(dataset.pearson_statistic.defective)
dataset.chi_sqred #=> 4.375171

dataset.chi_squared_probability <- 1- pchisq(dataset.chi_sqred, df = dataset.degree_of_freedom)
dataset.chi_squared_probability #=> 0.03646617