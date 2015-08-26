context("caret")

test_that("it preserves caret function createDataPartition", {
  x <- rgamma(50, 3, .5)
  inA <- createDataPartition(x, list = FALSE)
  expect_identical({ set.seed(1); caret::createDataPartition(x, list = FALSE) },
    { set.seed(1); createDataPartition(x, list = FALSE) })
})

test_that("it preserves caret function nearZeroVar", {
  expect_identical(caret::nearZeroVar(rep(0, 10)), nearZeroVar(rep(0, 10)))
  expect_identical(caret::nearZeroVar(c(rep(0, 10), 0.1, 0.2)), nearZeroVar(c(rep(0, 10), 0.1, 0.2)))
})

