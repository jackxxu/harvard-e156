
data = list(
  c(240, 244, 238, 237, 244, 239, 239),
  c(248, 238, 250, 249, 254, 255, 247),
  c(240, 236, 248, 239, 248, 239, 246),
  c(245, 243, 245, 254, 239, 240, 255)
)

i <- length(data) #=> 4
j <- lengths(data[1]) #=> 7

data.means <- unlist(lapply(data, mean))
data.means #=> 240.1429 248.7143 242.2857 245.8571

# part b: value of sst & mst
data.grand_mean <- mean(data.means)
data.grand_mean # => 244.25

data.sst <- j * sum(
                  unlist(
                    lapply(data.means, function (x) (x - data.grand_mean) ^2 )))
data.sst #=> 302.6786

data.mst <- data.sst/(i - 1)
data.mst #=> 100.8929

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
data.sse #=> 620.5714

data.mse <- data.sse/(i*(j - 1))
data.mse # => 25.85714

# part d: F statistic

data.f = data.mst / data.mse
data.f # => 3.901934

# part e:

data.p = 1 - pf(data.f, i-1, i*(j-1))
data.p # => 0.02107437