data.x = c(5037.1, 4913.9, 5148.2, 5061.8, 4949.4, 4945.9, 4965.5, 4997.9, 5009.4, 4876.1)
data.y = c(4731.7, 5038.0, 4701.8, 4853.1, 4902.8, 5211.9, 5342.1, 5082.8, 4937.8, 4757.0)

sample.variance = function(t) {
  m = mean(t)
  value <- sum(unlist(lapply(t, function(i) (i - m)^2)))
  value/(length(t) - 1)
}

data.alpha = 0.05

data.u = qf(1-data.alpha/2, length(data.x)-1, length(data.y)-1)
data.l = qf(data.alpha/2, length(data.x)-1, length(data.y)-1)

data.u
data.l

sample.variance(data.x)/sample.variance(data.y) #=> 0.1375478