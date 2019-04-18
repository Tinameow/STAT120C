# Simulating the Two-way ANOVA Model
###
# install.packages("dplyr")
# install.packages("ggplot2")
library(dplyr)
library(ggplot2)
###

# This function will return a data frame containing response values generated
# from a two-way ANOVA model with the specified parameters.
simulate_data <- function(mu, alpha, beta, delta, K, sigma) {
  I <- length(alpha)
  J <- length(beta)
  dat <- as.data.frame(expand.grid(i = 1:I, j = 1:J, k = 1:K, Y = NA))
  for (obs in 1:nrow(dat)) {
    dat$Y[obs] <- with(dat, mu + alpha[i[obs]] + beta[j[obs]] + delta[i[obs], j[obs]] + rnorm(1, 0, sigma))
  }
  return(dat)
}

###
# Simulate Data with no interactions
I <- 2 # Levels in Trt A
J <- 3 # Levels in Trt B
K <- 10 # Obs per cell

mu <- 1
alpha <- c(-0.5, 0.5)
beta <- c(-1, 0, 1)
delta <- matrix(c(0, 0, 0, 0, 0, 0), nrow = length(alpha), ncol = length(beta))
sigma <- 1

dat <- simulate_data(mu, alpha, beta, delta, K, sigma)
dat_grp_means <- dat %>% group_by(i, j) %>% summarize(grp_mean = mean(Y))

ggplot(dat_grp_means) + 
  geom_line(aes(x = factor(j), y = grp_mean, group = factor(i), linetype = factor(i), color = factor(i)), size = 2)
