# part a: two-sample test
n1 <- 180
n2 <- 200
p1 <- 16/n1
p2 <- 12/n2
alpha <- 0.05

d <- p1 - p2
d #=> 0.02888889

p_hat <- (n1*p1 + n2*p2)/(n1 + n2) #=> 0.07368421

d_ste <- sqrt(p_hat*(1-p_hat)*(1/n1 + 1/n2))
d_ste #=> 0.02684153

z <- d / d_ste
z #=> 1.076276

p <- (1-pnorm(z)) *2
p #=> 0.281804

# part b: contingency table
dataset.x = c(0, 1, 2, 3)
dataset.observed.defective <- c(16, 12)
dataset.observed.non_defective <- c(164, 188)
dataset.observed.defective_total <- sum(dataset.observed.defective)
dataset.observed.total <- sum(dataset.observed.defective) + sum(dataset.observed.non_defective)
dataset.observed.defective_mean <- dataset.observed.defective_total /dataset.observed.total
dataset.observed.column1_total <- dataset.observed.defective[1] + dataset.observed.non_defective[1]
dataset.observed.column2_total <- dataset.observed.defective[2] + dataset.observed.non_defective[2]

dataset.expected.defective = dataset.observed.defective_mean * c(dataset.observed.column1_total, dataset.observed.column2_total)
dataset.expected.defective #=> 13.26316 14.73684

dataset.expected.non_defective = (1 - dataset.observed.defective_mean) * c(dataset.observed.column1_total, dataset.observed.column2_total)
dataset.expected.non_defective #=> 166.7368 185.2632

dataset.pearson_statistic.defective <- (dataset.observed.defective - dataset.expected.defective)^2/dataset.expected.defective
dataset.pearson_statistic.defective #=> 0.5647452 0.5082707

dataset.pearson_statistic.non_defective <- (dataset.observed.non_defective - dataset.expected.non_defective)^2/dataset.expected.non_defective
dataset.pearson_statistic.non_defective #=> 0.04492291 0.04043062

dataset.degree_of_freedom <-  (length(dataset.observed.defective) - 1) * (length(dataset.observed.non_defective) - 1)
dataset.degree_of_freedom #=> 1

dataset.chi_sqred <- sum(dataset.pearson_statistic.non_defective) + sum(dataset.pearson_statistic.defective)
dataset.chi_sqred #=> 1.158369

dataset.chi_squared_probability <- 1- pchisq(dataset.chi_sqred, df = dataset.degree_of_freedom)
dataset.chi_squared_probability #=> 0.281804