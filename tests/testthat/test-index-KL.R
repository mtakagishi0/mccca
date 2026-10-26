# Krzanowski-Lai index (Krzanowski and Lai, 1988):
#   DIFF(k) = (k - 1)^(2/p) W(k - 1) - k^(2/p) W(k)
#   KL(k)   = |DIFF(k)| / |DIFF(k + 1)|
# where W(k) is the within-cluster sum of squares with k clusters.

wgss <- function(x, cl) {
  sum(sapply(split(seq_len(nrow(x)), cl), function(i) {
    xi <- x[i, , drop = FALSE]
    sum(sweep(xi, 2, colMeans(xi))^2)
  }))
}

kl_by_hand <- function(x, clall) {
  p <- ncol(x)
  k <- max(clall[, 2])
  W <- apply(clall, 2, function(cl) wgss(x, cl))
  diff_k  <- (k - 1)^(2 / p) * W[1] - k^(2 / p) * W[2]
  diff_k1 <- k^(2 / p) * W[2] - (k + 1)^(2 / p) * W[3]
  abs(diff_k) / abs(diff_k1)
}

test_that("the internal KL index equals the textbook formula", {
  set.seed(42)
  x <- rbind(matrix(rnorm(40, 0), 20), matrix(rnorm(40, 5), 20),
             matrix(rnorm(40, 10), 20))
  clall <- sapply(2:4, function(k) kmeans(x, k, nstart = 20)$cluster)

  expect_equal(mccca:::index.KLed(x, clall), kl_by_hand(x, clall))
})

test_that("the internal KL index agrees with clusterSim::index.KL", {
  set.seed(1)
  x <- matrix(rnorm(200), 100)
  clall <- sapply(2:4, function(k) kmeans(x, k, nstart = 20)$cluster)
  expect_equal(mccca:::index.KLed(x, clall), clusterSim::index.KL(x, clall))
})

test_that("the internal KL index handles a cluster with one observation", {
  x <- rbind(matrix(c(0, 0, 0.1, 0.1, 0.2, 0), ncol = 2, byrow = TRUE),
             matrix(c(5, 5, 5.1, 5.1, 5.2, 5), ncol = 2, byrow = TRUE),
             c(20, 20))
  clall <- cbind(c(1, 1, 1, 1, 1, 1, 2),
                 c(1, 1, 1, 2, 2, 2, 3),
                 c(1, 1, 4, 2, 2, 2, 3))
  expect_equal(mccca:::index.KLed(x, clall), kl_by_hand(x, clall))
})
