## (a)

set.seed(123)
n <- 500
X <- rnorm(n, 3, 1)
Y <- rexp(n, 0.05 / X)
plot(X, Y)

## (b)

fit <- lm(Y ~ X)
par(mfrow = c(1, 2))
plot(X, Y)
abline(fit)
plot(fitted(fit), residuals(fit))

## (c)

# Variance stabilizing transformation for Exp(\mu) is log.

## (d)

fit_log <- lm(log(Y) ~ X)

par(mfrow = c(1, 2))
plot(X, log(Y))
abline(fit_log)
plot(fitted(fit_log), residuals(fit_log))

# Model assumptions appear to be better satisfied after applying the variance stabilizing transformation.