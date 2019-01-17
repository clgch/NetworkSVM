### Goal: simulate a biologicial network the same way as in the article of Zhu, Shen and Pan (Bioinformatics 2009)
library(mvtnorm)


network_simulation <- function(scenario, n_sample) {
  X_t <- rmvnorm(n = n_sample, mean = rep(0, 5), sigma = diag(5))
  X <- matrix(0, n_sample, 55)
  
  beta <- rep(0, 55)
  beta[2:11] <- rep(5/sqrt(10), 10)
  beta[13:22] <- rep(-5/sqrt(10), 10)
  beta[1] <- 5
  beta[12] <- -5
  
  for (i in 1:5) {
    X[ ,i] <- X_t[ ,i]
    for (j in 1:10) {
      X[ ,i + i*j] <- rnorm(n = n_sample, mean = 0.7*X_t[i], sd = 0.5)
    }
  }
  s <- list(data = X, beta = beta)
  return(s)
}

