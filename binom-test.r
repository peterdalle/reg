# Exact binomial test
# Har använt formlerna från http://vassarstats.net/binomialX.html

# Notera att nedanstående bara är one-tailed. Därför blir p = 0.0795...

binom <- function(n=100, k=50, p=.5) {
  # n = number of trials
  # k = number of heads
  # p = probability of head on a single toss
  # q = probability head will not occur on a single toss
  q <- (1 - p)
  return((factorial(n) / (factorial(k) * factorial(n - k))) * (p ^ k) * (q ^ (n - k)))
}

binom(100, 50, 0.5)
