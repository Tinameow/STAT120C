library(ggplot2)

dat <- read.delim("https://raw.githubusercontent.com/dspluta/STAT120C/master/Data/fev.txt")

## Problem 3 (a)

ggplot(dat) + 
  geom_point(aes(x = height, y = fev))

## Problem 3 (b)

mean_height <- mean(dat$height)

mean_fev <- mean(dat$fev)

sum((dat$height - mean_height)^2)

sum(dat$height * (dat$fev - mean_fev))

## Problem 3 (c)

#### Use the formulas given in class to calculate the estimates for beta_0 and beta_1

## Problem 3 (d)

ggplot(dat) + 
  geom_point(aes(x = height, y = fev)) + 
  geom_smooth(aes(x = height, y = fev), method = "lm", se = FALSE)


## Problem 3 (e)

fit <- lm(fev ~ height, data = fev)
summary(fit)
