library('microbenchmark')

LoiBinomiale <- function(n, p) {
  k = 0
  u = runif(1)
  somme = 0
  while (u > somme) {
    k = k + 1
    somme = somme + choose(n, k) * p^k * (1-p)^(n-k)
  }
  x = k
  return(x)
}

Inversion <- function() {
  U = runif(1)
  Finv <- exp(sqrt(U)*log(2, 2)) - 1
  return (Finv)
}

Rejet <- function() {
  Y = runif(1)
  U = runif(1)
  c = 2/log(2, 2)^2
  f <- c * (log(1+Y, 2)/(1+Y))
  g <- 1
  while (U > f/c){
    Y = runif(1)
    U = runif(1)
  }
  return (Y)
}
