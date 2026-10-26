test_that("decideK() selects K = 3 for every class in the young data", {
  yd <- young_data()
  set.seed(1)
  res <- quiet(decideK(yd, Krange = c(2, 8), nstart = 20))

  expect_s3_class(res, "decideK")
  expect_equal(unname(res$cls.best), c(3, 3, 3, 3))
  expect_equal(names(res$cls.best), unname(yd$classlabel))
  expect_equal(res$index, "KL")
  expect_equal(res$cindexnm, 1)
  expect_false(any(res$skipped))

  # the selected K is the one that maximises the KL index in each class
  best_from_index <- sapply(res$ind.list, function(x) {
    as.integer(sub("clusters=", "", colnames(x)))[which.max(x[1, ])]
  })
  expect_equal(unname(best_from_index), unname(res$cls.best))
})

test_that("decideK() skips classes with fewer observations than subthr", {
  yd <- young_data()   # class sizes: 122, 283, 172, 416
  set.seed(1)
  res <- quiet(decideK(yd, Krange = c(2, 4), subthr = 150, nstart = 20))

  expect_equal(unname(res$skipped), c(TRUE, FALSE, FALSE, FALSE))
  # skipped classes get one cluster
  expect_equal(unname(res$cls.best[1]), 1)
})

test_that("decideK() uses the cluster validity index that is asked for", {
  yd <- young_data()
  for (ci in c("KL", "CH", "S", "DB")) {
    set.seed(1)
    res <- quiet(decideK(yd, Krange = c(2, 5), cindex = ci, nstart = 20))
    expect_equal(res$index, ci)
    expect_equal(res$cindexnm, match(ci, c("KL", "CH", "S", "DB")))
    expect_true(all(res$cls.best >= 2 & res$cls.best <= 5))
  }
  # cindexnm overrides cindex
  set.seed(1)
  res <- quiet(decideK(yd, Krange = c(2, 5), cindex = "KL", cindexnm = 2,
                       nstart = 20))
  expect_equal(res$index, "CH")
})

test_that("summary.decideK() returns the table of index values", {
  yd <- young_data()
  set.seed(1)
  res <- quiet(decideK(yd, Krange = c(2, 8), nstart = 20))
  tab <- quiet(summary(res))

  expect_equal(dim(tab), c(4, 7))
  expect_equal(colnames(tab), as.character(2:8))
  # values in the table are the stored index values
  expect_equal(unname(tab[1, ]), unname(res$ind.list[[1]][1, ]))
})

test_that("decideK() gives informative errors for invalid input", {
  yd <- young_data()
  expect_error(decideK(yd), "Krange")
  expect_error(decideK(yd, Krange = c(2, 4), cindex = "XX", nstart = 5),
               "not a valid cindex")
})
