# Newton-raphson
# univariate 
set.seed(2025)


base_mu <- 5
base_sigma <- 4

x <- rnorm(1000, mean = base_mu, sd = base_sigma)

score_func <- function(mu, sigma2, x) {
  return((sum(x) - length(x) * mu) / sigma2)
}

hessian_func <- function(mu, sigma2, x) {
  return(-length(x) / sigma2)
}

newton_raphson_single <- function(max_iter, init_mu, tolerance, sigma2) {
  for (i in 1:max_iter) {
    new_mu <- init_mu - score_func(init_mu, sigma2, x) / hessian_func(init_mu, sigma2, x)
    if (abs(new_mu - init_mu) <= tolerance) {
      return(new_mu)
    }
    init_mu <- new_mu
  }
  return(init_mu)
}

newton_raphson_single(500, 5, 1e-8, base_sigma^2)



mu <- 5
sigma2 <- 4

x <- rnorm(300, mu, sqrt(sigma2))


score <- function(x, mu, sigma2) {
  score1 <- sum(x-mu) / sigma2
  score2 <- -length(x)/sigma2 + sum((x-mu)^2)/(2*sigma2)
  return(c(score1, score2))
}

hessian <- function(x, mu, sigma2) {
  h11 <- -length(x)/sigma2
  h12 <- -sum(x-mu) / sigma2^2
  h21 <- h12
  h22 <- n/(2*sigma2^2) - sum((x-mu)^2) / sigma2^3
  return(matrix(c(h11, h12, h21, h22), nrow = 2))
}

max_iter <- 500
tolerance <- 1e-8
init_mu <- 0
init_sigma2 <- 1
s
h

for (i in 1:max_iter) {
  s <- score(x, init_mu, init_sigma2)
  h <- hessian(x, init_mu, init_sigma2)
  solve(h, s)
  update <- solve(h, s)
  new_mew <- init_mu - update[1]
  new_sigma2 <- init_sigma2 - update[2]
}



# Negative log normal distribution

f <- function(x){
  (x - 2)^2
}

set.seed(123)
y <- rnorm(1000, 5, 2)

nll <- function(parameter, data)  {
  mu <- parameter[1]
  sigma2 <- parameter[2]
  -sum(dnorm(data, mean = mu, sd = sqrt(sigma2), log = TRUE))
}

optim(par = c(2,2), fn = nll, method = "BFGS", data = y)

profile <- function(fixed_mu) {
  opt <- optim(
    par = c(var(y)),  # only optimize sigma2
    fn = function(sigma2) nll(c(fixed_mu, sigma2), data = y),
    method = "BFGS"
  )
  return(opt$value)
}

m <- mean(y)

m_grid <- seq(m-2, m+2, length.out = 200)

prof_vals <- sapply(m_grid , FUN = function(x) {
  profile(x)
})

plot(mu_grid, prof_vals, type = 'l',
     lwd = 2)
abline(v = m, col = 'Blue')


# multivariate
 data("iris")

 head(iris)

 
library(tidyverse)
library(GGally)
library(ggcorrplot) 
library(MVN) 
library(ICSNP) 



iris %>%
  ggpairs(
    columns = 1:4,
    aes(color = Species, alpha = 0.6)
  )

ggplot(iris) + geom_point(aes(Sepal.Length, Petal.Length, color = Species)) + facet_wrap(~Species) + theme_minimal()


X <- iris%>%
  filter(Species == 'setosa') %>%
  select(Sepal.Length:Petal.Width)

result <- mvn(data = X, mvn_test = 'mardia')
result$multivariate_normality
