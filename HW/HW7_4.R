dat <- matrix(c(82, 13, 89, 27, 54, 7, 19, 9), nrow = 2)

apply(X = dat, c(1), sum)
apply(X = dat, c(2), sum)

apply(X = dat, c(1), sum) / sum(dat)
apply(X = dat, c(2), sum) / sum(dat)
sum(dat)

kronecker(matrix(apply(X = dat, c(2), sum) / sum(dat), nrow = 1), matrix(apply(X = dat, c(1), sum) / sum(dat), ncol = 1))
expected <- sum(dat) * kronecker(matrix(apply(X = dat, c(2), sum) / sum(dat), nrow = 1), matrix(apply(X = dat, c(1), sum) / sum(dat), ncol = 1))

test_stat <- sum((dat - expected)^2 / expected)

df <- (nrow(dat) - 1) * (ncol(dat) - 1) # Degrees of Freedom

P_val <- pchisq(test_stat, df = df, lower.tail = FALSE)

chisq.test(dat)


####

2 * sum(dat * log(sum(dat) * dat / (apply(X = dat, c(1), sum) * apply(X = dat, c(2), sum))))

lik <- 1

for (i in 1:2) {
  for (j in 1:4) {
    lik <- lik * (dat[i, j] * sum(dat) / (sum(dat[i, ]) * sum(dat[, j])))^dat[i, j]
  }
}
lik

log(lik)
2 * log(lik)
pchisq(8.447, 3, lower.tail = FALSE)
