data.x = c(17.4, 19.9, 22.0, 16.7, 21.7, 21.1, 21.3, 25.1, 16.5, 25.7, 18.2, 16.8)
data.y = c(25.6, 28.9, 28.5, 27.0, 24.8, 25.8, 32.2, 28.7, 26.0, 24.8)

sample.variance = function(t) {
  m = mean(t)
  value <- sum(unlist(lapply(t, function(i) (i - m)^2)))
  value/(length(t) - 1)
}

data.alpha = 0.05

data.u = qf(1-data.alpha/2, length(data.x)-1, length(data.y)-1)
data.l = qf(data.alpha/2, length(data.x)-1, length(data.y)-1)

data.u #=> 3.912074
data.l #=> 0.2787147

sample.f = sample.variance(data.x)/sample.variance(data.y) #=> 1.843939

