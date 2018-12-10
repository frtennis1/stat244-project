setwd("~/Documents/Francisco/Harvard/07 Senior Fall/STAT 244/final-project")

library(MASS)
library(psych)
library(glmnet)

# generate hyperparameters

n <- 1e3
p <- 100
num.significant <- 10

A <- matrix(runif(p^2)*2-1, ncol=p) 
sigma <- t(A) %*% A

# generate random data

X <- mvrnorm(n = n, Sigma = sigma, mu = rep(0, p))
beta <- rep(0, p)
beta[1:num.significant] = rnorm(num.significant)
eta <- as.vector(X %*% beta)

y <- as.numeric(runif(n) < logistic(eta))

# put together into a dataframe
# df <- as.data.frame(cbind(X, y))

l1.model <- glmnet(X, y, family = "binomial", alpha = 1)

plot.glmnet(l1.model, xvar = "lambda")

summary(l1.model)

