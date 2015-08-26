context("caret")

test_that("it preserves caret function createDataPartition", {
  x <- rgamma(50, 3, .5)
  inA <- createDataPartition(x, list = FALSE)
  expect_identical({ set.seed(1); caret::createDataPartition(x, list = FALSE) },
    { set.seed(1); createDataPartition(x, list = FALSE) })
})
