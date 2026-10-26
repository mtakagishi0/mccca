test_that("summary and plot methods run on the example data", {
  md <- mealdrink_data()
  set.seed(1)
  res <- quiet(MCCCA(md, K.vec = c(2, 1, 2, 2), nstart = 20))

  expect_no_error(quiet(summary(md)))
  expect_no_error(quiet(summary(res)))

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_no_error(quiet(plot(res, classlabel = c("AF", "AM", "JF", "JM"))))

  yd <- young_data()
  set.seed(1)
  k <- quiet(decideK(yd, Krange = c(2, 5), nstart = 20))
  expect_no_error(quiet(plot(k)))
})
