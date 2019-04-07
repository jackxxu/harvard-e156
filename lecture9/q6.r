
data = list(
  c(23.7, 26.5, 22.6, 22.3, 26.2),
  c(23.7, 23.5, 20.7, 15.0, 22.2),
  c(21.7, 24.9, 25.3, 20.1, 24.2)
)
i <- length(data)
j <- lengths(data[1])

# part b:  group specific means
data.means <- unlist(lapply(data, mean))
data.means # => 24.26 21.02 23.24

# part c: value of sst
data.grand_mean <- mean(data.means)
data.grand_mean # => 22.84

data.sst <- j * sum(
                  unlist(
                    lapply(data.means, function (x) (x - data.grand_mean) ^2 )))
data.sst # => 27.444

# part d

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
data.sse #=> 86.912

# part e: sstot

data.sstot <- sum(
  unlist(lapply(unlist(data) - data.grand_mean, function(x) x^2))
)
data.sstot # => 114.356

data.sse + data.sst # => 114.356

data.sstot == data.sse + data.sst

# part f: mst

data.mst <- data.sst/(i - 1)
data.mst

# part g: mse

data.mse <- data.sse/(i*(j - 1))
data.mse # => 7.242667

# part h: F statistic

data.f = data.mst / data.mse
data.f # => 1.894606

# part i:

data.p = 1 - pf(data.f, 2, 4)
data.p # => 0.2637138