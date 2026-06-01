test_that("count counts single column", {
  df <- data.frame(x = c(2, 1, 1, 2, 1))
  out <- count(df, "x")
  expect_equal(out, data.frame(x = c(1, 2), n = c(3, 2)), ignore_attr = TRUE)
})

test_that("count counts multi columns", {
  df <- data.frame(x = c(2, 1, 1, 2, 1), y = c("A", "A", "A", "B", "B"))
  out <- count(df, c("x", "y"))

  expected <- data.frame(
    x = c(1, 1, 2, 2),
    y = c("A", "B", "A", "B"),
    n = c(2, 1, 1, 1)
  )

  expect_equal(out, expected, ignore_attr = TRUE)
})

test_that("count outputs keys in same type input original data.frame", {
  # Test Factors
  df <- data.frame(x = as.factor(c(2, 1, 1, 2, 1)))
  out <- count(df, "x")
  expect_true(is.factor(out$x))
  expect_equal(levels(out$x), c("1", "2"))

  # Test Characters
  df <- data.frame(x = as.character(c(2, 1, 1, 2, 1)))
  out <- count(df, "x")
  expect_true(is.character(out$x))

  # Test Date
  df <- data.frame(x = as.Date(c(2, 1, 1, 2, 1)))
  out <- count(df, "x")
  expect_s3_class(out$x, "Date")
})

test_that("count includes NA values", {
  df <- data.frame(x = c(2, 1, 1, 2, NA))
  out <- count(df, "x")
  expected <- data.frame(x = c(1, 2, NA), n = c(2, 2, 1))

  expect_equal(out, expected, ignore_attr = TRUE)
})

test_that("output includes empty levels with drop = FALSE", {
  df <- data.frame(x = factor("b", levels = c("a", "b", "c")))
  out <- count(df, "x", drop = FALSE)

  expect_equal(out$n, c(0, 1, 0))
  expect_equal(out$x, as.factor(c("a", "b", "c")))
})

test_that("output excludes empty levels with drop = TRUE", {
  df <- data.frame(x = factor("b", levels = c("a", "b", "c")))
  out <- count(df, "x", drop = TRUE)

  expect_equal(out$n, c(1))
  expect_equal(out$x, factor("b", levels = c("a", "b", "c")))
})


test_that("count adds 'n' attribute describing column name", {
  df <- data.frame(x = c(2, 1, 1, 2, 1))
  out <- count(df, "x")
  expected <- data.frame(x = c(1, 2), n = c(3, 2))

  attr(expected, "n") <- "n"
  expect_equal(out, expected, ignore_attr = FALSE)

  # When name argument is supplied the n attribute changes
  out2 <- count(df, "x", name = "bob")
  expected2 <- data.frame(x = c(1, 2), bob = c(3, 2))
  attr(expected2, "n") <- "bob"
  expect_equal(out2, expected2, ignore_attr = FALSE)
})

test_that("count sorts numeric keys numerically, not lexicographically", {
  df <- data.frame(x = c(10, 2, 1, 10))

  out <- count(df, "x")

  expected <- data.frame(
    x = c(1, 2, 10),
    n = c(1, 1, 2)
  )

  expect_equal(out, expected, ignore_attr = TRUE)
})
