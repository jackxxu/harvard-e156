
data = list(
  c(189, 170, 206, 199, 225, 201, 152, 179, 231),
  c(239, 209, 195, 205, 194, 196, 199, 224, 194),
  c(184, 180, 226, 191, 220, 219, 202, 180, 218),
  c(215, 169, 223, 196, 218, 173, 155, 214, 182)
)

i <- length(data) #=> 4
j <- lengths(data[1]) #=> 9

data.means <- unlist(lapply(data, mean))
data.means #=> 240.1429 248.7143 242.2857 245.8571

# part b: value of sst & mst
data.grand_mean <- mean(data.means)
data.grand_mean # => 199.2222

# part b: sst and mst
data.sst <- j * sum(
                  unlist(
                    lapply(data.means, function (x) (x - data.grand_mean) ^2 )))
data.sst #=> 950.8889

data.mst <- data.sst/(i - 1)
data.mst #=> 316.963

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
data.sse #=> 14965.33

data.mse <- data.sse/(i*(j - 1))
data.mse # => 467.6667

# part d: F statistic

data.f = data.mst / data.mse
data.f # => 0.677754

# part e:

data.p = 1 - pf(data.f, i-1, i*(j-1))
data.p # => 0.57209