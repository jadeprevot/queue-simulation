source('generateur.R')
source('tests.R')
source('lois.R')
source('files.R')

# Question 2.1 ################################################################

sVN <- 3454 
sMT <- 1502 
sR <- 2305 
sSM <- 7563 
Nsimu <- 1000
Nrepet <- 100

vn <- VonNeumann(Nsimu, sVN)
mt <- MersenneTwister(Nsimu, sMT)
r <- RANDU(Nsimu, sR)
sm <- StandardMinimal(Nsimu, sSM)

par(mfrow = c(2, 2))
hist(mt[, 1], xlab = 'Nombre aléatoire généré', ylab = 'Fréquence', main = 'Mersenne Twister')
hist(vn[, 1], xlab = 'Nombre aléatoire généré', ylab = 'Fréquence', main = 'Von Neumann')
hist(r, xlab = 'Nombre aléatoire généré', ylab = 'Fréquence', main = 'RANDU')
hist(sm, xlab = 'Nombre aléatoire généré', ylab = 'Fréquence', main = 'Standard Minimal')

# Question 2.2 ################################################################

par(mfrow = c(2, 2)) 
plot(mt[1:(Nsimu-1), 1], mt[2:Nsimu, 1], xlab = 'MT(i)', ylab = 'MT(i+1)', main = 'Mersenne Twister')
plot(vn[1:(Nsimu-1), 1], vn[2:Nsimu, 1], xlab = 'VN(i)', ylab='VN(i+1)', main = 'Von Neumann')
plot(r[1:(Nsimu-1)], r[2:Nsimu], xlab = 'R(i)', ylab = 'R(i+1)', main = 'RANDU')
plot(sm[1:(Nsimu-1)], sm[2:Nsimu], xlab = 'SM(i)', ylab = 'SM(i+1)', main = 'StandardMinimal')

# Question 3 ##################################################################

nbMT <- 32
nbVN <- 14
nbR <- 31
nbSM <- 31

mtFreq <- Test(MersenneTwister, Frequency, Nsimu, Nrepet, nbMT)
vnFreq <- Test(VonNeumann, Frequency, Nsimu, Nrepet, nbVN)
rFreq <- Test(RANDU, Frequency, Nsimu, Nrepet, nbR)
smFreq <- Test(StandardMinimal, Frequency, Nsimu, Nrepet, nbSM)

# Question 4 ##################################################################

mtRuns <- Test(MersenneTwister, Runs, Nsimu, Nrepet, nbMT)
vnRuns <- Test(VonNeumann, Runs, Nsimu, Nrepet, nbVN)
rRuns <- Test(RANDU, Runs, Nsimu, Nrepet, nbR)
smRuns <- Test(StandardMinimal, Runs, Nsimu, Nrepet, nbSM)

## Question 5 ##################################################################

mtOrder <- Test(MersenneTwister, Order, Nsimu, Nrepet, nbMT)
vnOrder <- Test(VonNeumann, Order, Nsimu, Nrepet, nbVN)
rOrder <- Test(RANDU, Order, Nsimu, Nrepet, nbR)
smOrder <- Test(StandardMinimal, Order, Nsimu, Nrepet, nbSM)

## Question Bonus 1 ############################################################

n <- 100
p <- 0.5
x <- c()
for (i in 1:Nsimu) {
  x[i] = LoiBinomiale(n, p)
}
par(mfrow = c(1, 2))
plot(table(x), xlab = '', ylab = '', main = 'Loi Binomiale')
plot(table(dnorm(x, n*p, sqrt(n*p*(1-p)))), xlab = '', ylab = '', main = 'Loi Normale')

## Question Bonus 2 ############################################################

microbenchmark(times=1000, Inversion(), Rejet())

x = c()
for (i in 1:1000){
  x[i] = Inversion()
}
hist(x, main = 'Simulation par inversion')

y = c()
for (i in 1:1000){
  y[i] = Rejet()
}
hist(y, main = 'Simulation par rejet')

## Question 7 ##################################################################

x1 <- FileMM1(6, 11, 12)
x2 <- FileMM1(10, 11, 12)
x3 <- FileMM1(11, 11, 12)
x4 <- FileMM1(15, 11, 12)

y1 <- FileEvolution(x1$arrivees, x1$departs)
y2 <- FileEvolution(x2$arrivees, x2$departs)
y3 <- FileEvolution(x3$arrivees, x3$departs)
y4 <- FileEvolution(x4$arrivees, x4$departs)

par(mfrow = c(2, 2))
plot(y1$temps, y1$nbClients, type = 's', xlab='Temps', ylab='Nombre de clients', main= '6 clients / heure')
plot(y2$temps, y2$nbClients, type = 's', xlab='Temps', ylab='Nombre de clients', main= '10 clients / heure')
plot(y3$temps, y3$nbClients, type = 's', xlab='Temps', ylab='Nombre de clients', main= '11 clients / heure')
plot(y4$temps, y4$nbClients, type = 's', xlab='Temps', ylab='Nombre de clients', main= '14 clients / heure')

## Question 8 ##################################################################

s1 <- Statistique(6, 11, 12)
s2 <- Statistique(10, 11, 12)
s3 <- Statistique(11, 11, 12)
s4 <- Statistique(15, 11, 12)
