i <- 5
j <- 15

data.sstot <- 1213
data.mse <- 14.7

data.sse <- data.mse * (i*(j - 1))
data.sse #=> 1029

data.sst <- data.sstot - data.sse
data.sst #=> 184

data.mst <- data.sst/(i - 1)
data.mst #=> 46

data.f <- data.mst / data.mse
data.f # => 3.129252

data.p <- 1 - pf(data.f, i-1, i*(j-1))
data.p #=> 0.0199242

