s6340.qda <- function(y, X) {
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
Sigma <- by(X, y, cov)
# Sigma <- Reduce('+', lapply(1:K, FUN = function(k) {(n[k] -# 1) * S[[k]]}))/(N - K) its inverse
Sigma.inv <- by(X, y, function(x) solve(cov(x)))
# delta functions
if (K >= 2) {
term1 <- 0.5 * (log(det(Sigma[[1]])) - log(det(Sigma[[2]])))
term2 <- 0.5 * (t(mu[1, ]) %*% Sigma.inv[[1]] %*% mu[1,] - t(mu[2, ]) %*% Sigma.inv[[2]] %*% mu[2, ])
term3 <- log(pi[2]/pi[1])
cutoff <- term1 + term2 + term3
rownames(cutoff) <- paste(levels(y)[1], "-", levels(y)[2],sep = "")
Sigma.inv.diff <- Sigma.inv[[1]] - Sigma.inv[[2]]
mu.Sigma.inv.diff <- t(mu[1, ]) %*% Sigma.inv[[1]] -t(mu[2, ]) %*% Sigma.inv[[2]]
# result
result <- list(N = N, n = n, pi = pi, mu = mu, Sigma = Sigma,
Sigma.inv = Sigma.inv, quad.coef.matrix = Sigma.inv.diff,
linear.coef.vector = mu.Sigma.inv.diff, cutoff = cutoff)
} else {
result <- list(N = N, n = n, pi = pi, mu = mu, Sigma = Sigma,
Sigma.inv = Sigma.inv)
}
return(result)
}
