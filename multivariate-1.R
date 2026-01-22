library(tidyverse) # for data manipulation
library(GGally) # for multivariate data visualization (ggpairs())
library(ggcorrplot) # for correlation heatmap (ggcorrplot())
library(ICSNP) # for hotelling T^2 Test (HotellingsT2())
library(MVN) # for multivariate normality test (mvn())

#Loading Iris dataset
data(iris)

# Preliminary idea about dataset
#glimpse(iris)

# Multivariate data visualization (try to interpret it. Simpsons Paradox?)
iris %>%
  ggpairs(
    columns = 1:4,
    aes(color = Species, alpha = 0.7)
  )

# pair-wise scatter plot
ggplot(iris) +
  geom_point(aes(Sepal.Length, Petal.Length, color = Species)) +
  facet_wrap(~ Species) +
  theme_minimal()

# Correlation Heatmap
ggcorrplot(cor(iris[, -5]), lab = TRUE)

# Test of multivariate normality
X <- iris %>%
  filter(Species == "setosa") %>%
  select(Sepal.Length:Petal.Width)
result <- mvn(data = X, mvn_test = "mardia")
result$multivariate_normality

# Test of mean vector using hotelling T^2 (using package)
mu0 <- c(5.8, 3.0, 3.7, 1.1)
HotellingsT2(X, mu = mu0)

# Test of mean vector using hotelling T^2 (Manual Calculation)
Xmat <- as.matrix(X)
n <- nrow(Xmat)
p <- ncol(Xmat)
xbar <- colMeans(Xmat)
S <- cov(Xmat)
T2 <- n * t(xbar - mu0) %*% solve(S) %*% (xbar - mu0)
F_stat <- (n - p) / (p * (n - 1)) * T2
F_stat
p_value <- pf(F_stat, p, n - p, lower.tail = FALSE)
p_value

