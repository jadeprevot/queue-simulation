source('generateurs.R')
library(randtoolbox)

Frequency <- function(x, nb) {
  Sn <- 0
  k <- length(x)
  for (i in 1:k) {
    bits <- binary(x[i])
    Sn <- Sn + sum(2 * bits[1:nb] - 1)
  }
  n <- sqrt(nb * k)
  Sobs <- abs(Sn) / n
  Pvaleur <- 2 * (1 - pnorm(Sobs))
  return(Pvaleur)
}

Runs <- function(x, nb) {
  Pvaleur <- 0.0

  k <- length(x)
  n <- nb * k

  pi <- 0
  for (i in 1:k) {
    bits <- binary(x[i])
    pi <- pi + sum(bits[1:nb])
  }
  pi <- pi / n

  tau <- 2 / sqrt(n)
  if (abs(pi - 0.5) >= tau) {
    return(Pvaleur)
  }

  Vn <- 0
  last <- NULL
  for (i in 1:k) {
    bits <- binary(x[i])
    if (!is.null(last)) {
      if (last != bits[1]) {
        Vn <- Vn + 1
      }
    }
    for (j in 1:(nb-1)) {
      if (bits[j] != bits[j+1]) {
        Vn <- Vn + 1
      }
    }
    last <- bits[nb]
  }
  Vn <- Vn + 1

  Pvaleur <- 2 * (1 - pnorm(abs(Vn - 2*n*pi*(1-pi)) / (2*sqrt(n)*pi*(1-pi))))

  return(Pvaleur)
}

Order <- function(x, nb) {
  Pvalue <- order.test(x, d = 4, echo = FALSE)$p.value
  
  return(Pvalue)
}

Test <- function(Generator, Test, Nsimu, Nrepet, nb) {
  seeds <- sample.int(100000, Nrepet)
  avgPvalue <- 0
  passRate <- 0
  for (i in 1:Nrepet) {
    Pvalue <- Test(as.vector(Generator(Nsimu, seeds[i])), nb)
    avgPvalue <- avgPvalue + Pvalue
    if (Pvalue > 0.01) {
      passRate <- passRate +1
    }
  }
  avgPvalue <- avgPvalue / Nrepet
  passRate <- passRate / Nrepet
  
  return(list(avgPvalue = avgPvalue, passRate = passRate))
}
