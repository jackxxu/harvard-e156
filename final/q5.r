
data = list(
  c(15.9, 13.4, 18.2, 17.2, 20.8, 17.5, 10.8, 14.5),
  c(23.8, 23.4, 19.2, 17.4, 18.6, 17.1, 17.4, 17.9),
  c(16.5, 12.3, 12.2, 11.5, 17.7, 13.1, 17.0, 16.9),
  c(16.6, 13.6, 18.9, 17.7, 11.4, 18.8, 15.1, 18.2)
)

i <- length(data) #=> 4
j <- lengths(data[1]) #=> 8

# part a: Calculate the group-specific sample means, along with the grand mean.
data.means <- unlist(lapply(data, mean))
data.means #=> 16.0375 19.3500 14.6500 16.2875

data.grand_mean <- mean(data.means)
data.grand_mean # => 16.58125

# part b: sst and mst
data.sst <- j * sum(
                  unlist(
                    lapply(data.means, function (x) (x - data.grand_mean) ^2 )))
data.sst #=> 94.22125

data.mst <- data.sst/(i - 1)
data.mst #=> 31.40708

# part c: sse & mse

square.error <- function(v) {
  m = mean(v)
  value <- sum(unlist(lapply(v, function(i) (i - m)^2)))
  value
}

data.sse <- sum(
  unlist(
    lapply(data, function(t) square.error(unlist(t)))
  )
)
data.sse #=> 217.7475

data.mse <- data.sse/(i*(j - 1))
data.mse # => 7.776696

# part d: F statistic

data.f = data.mst / data.mse
data.f # => 4.038615

data.p = 1 - pf(data.f, i-1, i*(j-1))
data.p # => 0.01663508