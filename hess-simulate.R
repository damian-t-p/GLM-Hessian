library(magrittr)

n <- 1000
p <- 40

X <- matrix(rnorm(n*p), n, p)
beta <- rnorm(p)

sigmoid <- function(x) exp(x)/(exp(x) + 1)

y <- apply(X, 1, function(x) rbinom(1, 1, sigmoid(x %*% beta)))

beta.mle <- glm.fit(X, y, family=binomial()) %>% coef

mean.logistic <- function(X, beta) sigmoid(X %*% beta)
var.logistic <- function(X, beta) sigmoid(X %*% beta) * (1 - sigmoid(X %*% beta))

#t(X) %*% (y - mean.logistic(X, beta.mle))

(t(X) %*% diag(var.logistic(X, beta.mle) %>% c) %*% X %>% eigen(only.values = TRUE))$`values` %>% hist
(t(X) %*% diag(var.logistic(X, beta) %>% c) %*% X %>% eigen(only.values = TRUE))$`values` %>% hist