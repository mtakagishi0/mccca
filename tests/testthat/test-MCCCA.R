# In mealDrink, every respondent in a class gives one of a few meal-drink
# patterns, so the true class-specific clusters are known:
#   American&female : Meat-Fjuice (80), Fish-Tea (25)
#   American&male   : Meat-Fjuice (90)
#   Japanese&female : Rice-Tea (70),    Rice-Milk (10)
#   Japanese&male   : Rice-Tea (65),    Fish-Tea (50)

fit_mealdrink <- function(seed = 1, nstart = 20) {
  md <- mealdrink_data()
  set.seed(seed)
  quiet(MCCCA(md, K.vec = c(2, 1, 2, 2), nstart = nstart))
}

test_that("MCCCA() recovers the true clusters in mealDrink", {
  md <- mealdrink_data()
  res <- fit_mealdrink()

  expect_s3_class(res, "mccca")
  expect_equal(as.vector(res$K.vec), c(2, 1, 2, 2))
  expect_length(res$cluster.label.n.vec, nrow(mealDrink))

  # each class x meal-drink pattern is assigned to exactly one cluster ...
  pattern <- paste(mealDrink$meal, mealDrink$drink, md$class.label.n.vec)
  n_clusters_per_pattern <- tapply(res$cluster.label.n.vec, pattern,
                                   function(x) length(unique(x)))
  expect_true(all(n_clusters_per_pattern == 1))
  # ... and each cluster contains exactly one pattern
  n_patterns_per_cluster <- tapply(pattern, res$cluster.label.n.vec,
                                   function(x) length(unique(x)))
  expect_true(all(n_patterns_per_cluster == 1))

  # cluster sizes equal the true pattern counts
  expect_equal(sort(as.vector(table(res$cluster.label.n.vec))),
               c(10, 25, 50, 65, 70, 80, 90))
  # clusters stay inside their class
  expect_equal(as.vector(res$cluster.class.index), c(1, 1, 2, 3, 3, 4, 4))
})

test_that("clusters with the same response pattern get the same coordinates", {
  md <- mealdrink_data()
  res <- fit_mealdrink()

  pattern_of_cluster <- tapply(
    paste(mealDrink$meal, mealDrink$drink),
    res$cluster.label.n.vec,
    unique
  )
  # Meat-Fjuice appears in two classes, Rice-Tea in two, Fish-Tea in two
  for (p in c("Meat Fjuice", "Rice Tea", "Fish Tea")) {
    cl <- names(pattern_of_cluster)[pattern_of_cluster == p]
    expect_length(cl, 2)
    expect_equal(res$G[cl[1], ], res$G[cl[2], ], tolerance = 1e-6)
  }
})

test_that("MCCCA() returns reference values for mealDrink", {
  res <- fit_mealdrink()

  # objective value at the optimum (regression check; independent of the seed)
  expect_equal(res$optval, -1.578471, tolerance = 1e-6)
  # the objective never increased during the iterations
  expect_equal(res$count.inc, 0)
  # adjusted inertias
  expect_equal(unname(res$inertia[, 1]), c(1, 0.02463054), tolerance = 1e-6)
  expect_equal(unname(res$inertia[2, 3]), 1)
  # relative cluster sizes sum to 1 within each class
  rel <- tapply(res$csize[, 2], res$cluster.class.index, sum)
  expect_equal(unname(as.vector(rel)), rep(1, 4))
})

test_that("MCCCA() gives the same solution for different random seeds", {
  r1 <- fit_mealdrink(seed = 1)
  r2 <- fit_mealdrink(seed = 123)
  expect_equal(r1$optval, r2$optval, tolerance = 1e-8)
  expect_equal(unname(table(r1$cluster.label.n.vec)),
               unname(table(r2$cluster.label.n.vec)))
})

test_that("MCCCA() returns matrices of the documented dimensions", {
  res <- fit_mealdrink()
  K <- sum(res$K.vec)
  Q <- sum(res$q.vec)
  expect_equal(dim(res$G), c(K, 2))
  expect_equal(dim(res$Gg), c(K, 2))
  expect_equal(dim(res$B), c(Q, 2))
  expect_equal(dim(res$Bg), c(Q, 2))
  expect_equal(dim(res$Fn), c(nrow(mealDrink), 2))
})

test_that("MCCCA() reproduces the young data application (K = 3 in every class)", {
  yd <- young_data()
  set.seed(1)
  res <- quiet(MCCCA(yd, K.vec = c(3, 3, 3, 3), nstart = 50))

  expect_equal(res$optval, -0.8062528, tolerance = 1e-6)
  expect_equal(unname(round(res$inertia[, 2], 4)), c(0.7648, 0.2352))
  # sizes of the clusters within each class (order of clusters may differ)
  sizes <- split(res$csize[, 1], res$cluster.class.index)
  expect_equal(lapply(sizes, function(x) sort(unname(x))),
               list(`1` = c(35, 37, 50), `2` = c(82, 91, 110),
                    `3` = c(53, 59, 60), `4` = c(105, 125, 186)))
})

test_that("MCCCA() gives informative errors for invalid K.vec", {
  md <- mealdrink_data()
  expect_error(MCCCA(md), "K.vec")
  expect_error(MCCCA(md, K.vec = c(2, 2)), "length of K.vec")
  expect_error(MCCCA(md, K.vec = c(2, 1, 2, 500)), "K.vec")
})
