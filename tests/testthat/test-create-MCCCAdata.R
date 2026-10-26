test_that("create.MCCCAdata() splits mealDrink into the correct classes", {
  md <- mealdrink_data()

  expect_s3_class(md, "mcccadata")
  expect_equal(md$C, 4)
  expect_equal(unname(md$classlabel),
               c("American&female", "American&male",
                 "Japanese&female", "Japanese&male"))
  # class sizes equal the counts of nationality x gender in the raw data
  expect_equal(as.vector(md$N.vec), c(105, 90, 80, 115))
  expect_equal(as.vector(md$N.vec),
               as.vector(table(paste(mealDrink$nationality, mealDrink$gender))))
  # three categories in each active variable (meal, drink)
  expect_equal(as.vector(md$q.vec), c(3, 3))
})

test_that("observation indices form a partition consistent with the class labels", {
  md <- mealdrink_data()
  N <- nrow(mealDrink)

  idx <- unlist(md$obs.index.n.list)
  expect_equal(unname(sort(idx)), seq_len(N))
  expect_equal(unname(lengths(md$obs.index.n.list)), as.vector(md$N.vec))
  expect_equal(as.vector(table(md$class.index.n.vec)), as.vector(md$N.vec))

  # every observation's class label matches its own external variables
  expected <- paste(mealDrink$nationality, mealDrink$gender, sep = "&")
  expect_equal(unname(md$class.label.n.vec), expected)

  # act.list contains exactly the rows of each class
  for (cc in seq_len(md$C)) {
    expect_equal(nrow(md$act.list[[cc]]), md$N.vec[[cc]])
  }
})

test_that("create.MCCCAdata() works for the young data (4 classes)", {
  yd <- young_data()
  expect_equal(yd$C, 4)
  expect_equal(as.vector(yd$N.vec), c(122, 283, 172, 416))
  expect_equal(sum(yd$N.vec), nrow(young))
  expect_equal(as.vector(yd$q.vec), c(3, 3, 3))
})

test_that("create.MCCCAdata() rejects active and external data of different sizes", {
  expect_error(quiet(create.MCCCAdata(young[1:10, 1:3], young[, 4:5])))
})

test_that("create.MCCCAdata() rejects missing values with an informative error", {
  md <- mealDrink
  md$meal[c(1, 5, 9)] <- NA
  expect_error(quiet(create.MCCCAdata(md[, 1:2], md[, 3:4])),
               "dat.act: meal \\(3\\)")

  md <- mealDrink
  md$gender[2] <- NA
  expect_error(quiet(create.MCCCAdata(md[, 1:2], md[, 3:4])),
               "dat.ext: gender \\(1\\)")

  # works after removing the incomplete rows
  md <- mealDrink
  md$meal[c(1, 5, 9)] <- NA
  ok <- stats::complete.cases(md)
  res <- quiet(create.MCCCAdata(md[ok, 1:2], md[ok, 3:4]))
  expect_equal(sum(res$N.vec), 387)
})

test_that("create.MCCCAdata() rejects active variables with one category", {
  md <- mealDrink
  md$const <- "same"
  expect_error(quiet(create.MCCCAdata(md[, c("meal", "drink", "const")],
                                      md[, 3:4])),
               "only one category: const")
})

test_that("create.MCCCAdata() checks that the numbers of rows agree", {
  expect_error(create.MCCCAdata(young[1:10, 1:3], young[, 4:5]),
               "same number of rows \\(10 and 993\\)")
})
