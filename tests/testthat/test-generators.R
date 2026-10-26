test_that("generate.onedata() returns data with the requested structure", {
  set.seed(1)
  g <- generate.onedata(N = 100, J = 5, Ktrue = 3, q.vec = c(3, 3, 4, 2, 3),
                        noise.prop = 0.3)

  expect_equal(dim(g$data.mat), c(100, 5))
  expect_length(g$clstr0.vec, 100)
  expect_true(all(g$clstr0.vec %in% 1:3))
  # each variable takes values in 1, ..., q_j
  maxcat <- sapply(g$data.mat, max)
  expect_true(all(maxcat <= c(3, 3, 4, 2, 3)))
  expect_true(all(sapply(g$data.mat, min) >= 1))
})

test_that("generate.ext() returns external variables with the requested categories", {
  set.seed(1)
  e <- generate.ext(50, extcate.vec = c(2, 3))

  expect_equal(dim(e), c(50, 2))
  expect_equal(unname(apply(e, 2, function(x) length(unique(x)))), c(2, 3))
  expect_true(all(e[, 1] %in% 1:2))
  expect_true(all(e[, 2] %in% 1:3))
})
