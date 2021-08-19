s6340.lda <- function(y, X) f
# y = training data response vector (a factor) X = training
# data predictor matrix
N <- length(y) # no of observations
K <- nlevels(y) # no of classes
p <- ncol(X) # no of predictors
n <- as.numeric(table(y)) # class frequencies
names(n) <- levels(y)
pi <- n/N # class proportions
# mean vector
mu <- matrix(unlist(by(X, y, colMeans)), byrow = T, ncol = p)
rownames(mu) <- levels(y)
colnames(mu) <- colnames(X)
# pooled covariance matrix
S <- by(X, y, cov)
Sigma <- Reduce("+", lapply(1:K, FUN = function(k) {f}
                            (n[k] - 1) * S[[k]]
                            }))/(N - K)
# its inverse
Sigma.inv <- solve(Sigma)
# delta functions
delta <- t(sapply(1:K, FUN = function(k) f
                  c(-(1/2) * drop(t(mu[k, ]) %*% Sigma.inv %*% mu[k, ]) +
                      log(pi[k]), t(mu[k, ]) %*% Sigma.inv)
                  g))
rownames(delta) <- levels(y)
colnames(delta) <- c("(Intercept)", colnames(X))
# pairwise difference of delta functions
idx.pair <- combn(K, 2)
delta.diff <- t(apply(idx.pair, MAR = 2, FUN = function(pair) f
                      delta[pair[1], ] - delta[pair[2], ]
                      g))
rownames(delta.diff) <- apply(idx.pair, MAR = 2, FUN = function(pair) f
                              paste0(levels(y)[pair[1]], "-", levels(y)[pair[2]])
                              g)
# multiply intecept difference by 1 to get the cutoff c
delta.diff[, 1] <- -delta.diff[, 1]
colnames(delta.diff)[1] <- "Cutoff"
# result
result <- list(N = N, n = n, pi = pi, mu = mu, Sigma = Sigma,
               delta = delta, disc = delta.diff)
return(result)
}