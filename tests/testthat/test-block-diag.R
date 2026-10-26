test_that("block.diag() places matrices on the diagonal and fills the rest with 0", {
  A <- matrix(1:6, nrow = 2)
  B <- matrix(c(1, 0, 1, 1, 0, 0, 1, 0), nrow = 4)

  expected <- rbind(
    c(1, 3, 5, 0, 0),
    c(2, 4, 6, 0, 0),
    c(0, 0, 0, 1, 0),
    c(0, 0, 0, 0, 0),
    c(0, 0, 0, 1, 1),
    c(0, 0, 0, 1, 0)
  )
  expect_equal(mccca:::block.diag(A, B), expected)
})

test_that("block.diag() works with one matrix, three matrices and data frames", {
  A <- matrix(1:4, 2)
  expect_equal(mccca:::block.diag(A), matrix(c(1, 2, 3, 4), 2))

  res <- mccca:::block.diag(1, A, matrix(5, 1, 2))
  expect_equal(dim(res), c(4, 5))
  expect_equal(res[1, 1], 1)
  expect_equal(res[2:3, 2:3], matrix(c(1, 2, 3, 4), 2))
  expect_equal(res[4, 4:5], c(5, 5))
  expect_equal(sum(res), 1 + 10 + 10)

  df <- data.frame(a = c(1, 0), b = c(0, 1))
  expect_equal(mccca:::block.diag(df, df), diag(4))
})

test_that("block.diag() handles empty, NA and logical input", {
  expect_equal(dim(mccca:::block.diag()), c(0, 0))
  # blocks with zero rows or columns add only zero rows or columns
  expect_equal(mccca:::block.diag(matrix(0, 2, 0), diag(2)),
               rbind(matrix(0, 2, 2), diag(2)))
  # NA is kept in its position
  res <- mccca:::block.diag(matrix(c(1, NA), 1), diag(1))
  expect_true(is.na(res[1, 2]))
  expect_equal(res[2, 3], 1)
  # logical values become 0/1
  expect_equal(mccca:::block.diag(matrix(TRUE, 1, 1), matrix(FALSE, 1, 1)),
               diag(c(1, 0)))
})

test_that("block.diag() rejects input that is not numeric or logical", {
  expect_error(mccca:::block.diag(NULL, diag(2)), "NULL")
  expect_error(mccca:::block.diag(diag(2), matrix("a", 1, 1)),
               "not a numeric or logical matrix")
  expect_error(mccca:::block.diag(data.frame(a = c("x", "y"))),
               "not a numeric or logical matrix")
  expect_error(mccca:::block.diag(list(1, 2)),
               "not a numeric or logical matrix")
})
