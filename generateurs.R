VonNeumann <- function(n, graine, p = 1) {
  x <- rep(graine, n*p+1)
  for (i in 2:(n*p+1)) {
    numbers <- strsplit(format(x[i-1]^2, scientific = FALSE), '')[[1]]
    while (length(numbers) > 4) { 
        numbers <- numbers[2:(length(numbers)-1)] 
    }
    x[i] <- as.numeric(numbers)%*%(10^seq(length(numbers)-1, 0, -1))
  }
  x <- matrix(x[2:(n*p+1)], nrow = n, ncol = p)
  return(x)
}

MersenneTwister <- function(n, graine, p = 1) {
  set.seed(graine, kind = 'Mersenne-Twister')
  x <- sample.int(2^32-1, n*p)
  x <- matrix(x, nrow = n, ncol = p)
  return(x)
}

CongruenceLineaire <- function(k, graine, a, b, m) {
  x <- c()
  x[1] = (a * graine + b) %% m
  for (i in 2:k) {
    x[i] = (a * x[i-1] + b) %% m
  }
  return(x)
}

RANDU <- function(k, graine) {
  return(CongruenceLineaire(k, graine, 65539, 0, 2^31))
}
  
StandardMinimal <- function(k, graine, a, b, m) {
  return(CongruenceLineaire(k, graine, 16807, 0, 2^31-1))
}

binary <- function(x) {
  if ((x < 2^31) & (x >= 0)) {
    return(as.integer(intToBits(as.integer(x))))
  }
  else {
    if (( x < 2^32) & (x > 0)) {
      return(c(binary(x - 2^31)[1:31], 1))
    }
    else {
      cat('Erreur dans binary : le nombre etudie n est pas un entier positif en 32 bits.\n')
      return(c())
    }
  }
}

