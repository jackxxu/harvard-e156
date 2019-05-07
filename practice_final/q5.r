data.x = c(65.3, 51.6, 63.4, 47.4, 58.1, 58.6, 65.9, 60.4)
data.y = c(18.6, 23.1, 50.7, 70.9, 28.1, 44.9, 24.5, 41.9)

sample.variance = function(t) {
  m = mean(t)
  value <- sum(unlist(lapply(t, function(i) (i - m)^2)))
  value/(length(t) - 1)
}

data.alpha = 0.05

data.u = qf(1-data.alpha/2, length(data.x)-1, length(data.y)-1)
data.l = qf(data.alpha/2, length(data.x)-1, length(data.y)-1)

data.u #=> 4.994909
data.l #=> 0.2002038

sample.f = sample.variance(data.x)/sample.variance(data.y) #=> 0.1367081

sample.p = 2 * pf(0.1375478, length(data.x)-1, length(data.y)-1 )
sample.p #-> 0.01790849