FileMM1 <- function(lambda, mu, D) {
  duree1 <- 0
  duree2 <- 0
  arrivee <- c()
  depart <- c()
  
  #arrivees
  while (TRUE) {
    v <- rexp(1, lambda)
    duree1 <- duree1 + v
    if (duree1 > D){
      break
    }
    arrivee <- c(arrivee, duree1)
  }
  
  #departs
  k <- length(arrivee)
  departPrecedent <- 0
  for (i in 1:k) {
    u <- rexp(1, mu)
    duree2 <- arrivee[i] + u
    if (departPrecedent < duree2){
      duree2 <- arrivee[i] + u
    }
    else {
      duree2 <- departPrecedent + u
    }
    if (duree2 > D) {
      break
    }
    departPrecedent <- duree2
    depart <- c(depart, duree2)
  }
  
  return (list(arrivees = arrivee, departs = depart))
}

FileEvolution <- function(arrivee, depart) {
  t <- 0
  nb <- 0
  k <- 1
  l <- 1
  m <- length(arrivee)
  n <- length(depart)
  temps <- c()
  nbClients <- c()
  while (k <= m || l <= n) {
    if (k > m  && l <= n) {
      t <- depart[l]
      nb <- nb - 1
      l <- l + 1
    }
    else if (k <= m && l > n) {
      t = arrivee[k]
      nb <- nb + 1
      k <- k + 1
    }
    else if (arrivee[k] < depart[l]) {
      t <- arrivee[k]
      nb <- nb + 1
      k <- k+1
    }
    else {
      t <- depart[l]
      nb <- nb - 1
      l <- l + 1
    }
    temps <- c(temps, t)
    nbClients <- c(nbClients, nb)
  }
  return (list(temps = temps, nbClients = nbClients))
}

Statistique <- function(lambda, mu, D) {
  alpha <- lambda / mu
  
  if (alpha >= 1){
    return (NULL)
  }
  
  x <- FileMM1(lambda, mu, D)
  
  clientsM = alpha / (1 - alpha)
  tempsClient <- 0
  k <- length(x$departs)
  
  for (i in 1:k) {
    tempsClient <- tempsClient + x$depart[i] - x$arrivee[i]
  }
  tempsClient <- tempsClient / k
  
  return (list(EN = clientsM, EW = tempsClient))
}
