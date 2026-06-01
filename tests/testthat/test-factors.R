test_that("fct_relevel throws error when no ref is supplied", {
  x <- factor(c("a", "b", "c"), levels = c("b", "c", "a"))

  expect_error(fct_relevel(x))
})


test_that("fct_relevel moves a single level to the front by default", {
  x <- factor(c("a", "b", "c", "d"), levels = c("b", "c", "d", "a"))

  result <- fct_relevel(x, "a")

  expect_identical(levels(result), c("a", "b", "c", "d"))
  expect_identical(as.character(result), as.character(x))
})


test_that("fct_relevel moves multiple levels to the front in ref order", {
  x <- factor(c("a", "b", "c", "d"), levels = c("b", "c", "d", "a"))

  result <- fct_relevel(x, c("d", "b"))

  expect_identical(levels(result), c("d", "b", "c", "a"))
  expect_identical(as.character(result), as.character(x))
})


test_that("fct_relevel keeps x levels not present in ref at the end", {
  x <- factor(
    c("low", "medium", "high", "unknown"),
    levels = c("unknown", "medium", "low", "high")
  )

  result <- fct_relevel(x, c("low", "medium"))

  expect_identical(levels(result), c("low", "medium", "unknown", "high"))
})


test_that("fct_relevel ignores ref levels that are not present in x", {
  x <- factor(c("a", "b", "c"), levels = c("c", "b", "a"))

  result <- fct_relevel(x, c("z", "a", "y", "c"))

  expect_identical(levels(result), c("a", "c", "b"))
  expect_false("z" %in% levels(result))
  expect_false("y" %in% levels(result))
})


test_that("fct_relevel only uses duplicated ref levels once", {
  x <- factor(c("a", "b", "c"), levels = c("c", "b", "a"))

  result <- fct_relevel(x, c("a", "a", "c", "a"))

  expect_identical(levels(result), c("a", "c", "b"))
})


test_that("fct_relevel can insert ref levels after a position", {
  x <- factor(c("a", "b", "c", "d"), levels = c("a", "b", "c", "d"))

  result <- fct_relevel(x, c("d", "b"), after = 1L)

  expect_identical(levels(result), c("a", "d", "b", "c"))
  expect_identical(as.character(result), as.character(x))
})


test_that("fct_relevel can move ref levels to the end with after = Inf", {
  x <- factor(c("a", "b", "c", "d"), levels = c("a", "b", "c", "d"))

  result <- fct_relevel(x, c("d", "b"), after = Inf)

  expect_identical(levels(result), c("a", "c", "d", "b"))
  expect_identical(as.character(result), as.character(x))
})


test_that("fct_relevel clamps after values beyond the number of remaining levels", {
  x <- factor(c("a", "b", "c", "d"), levels = c("a", "b", "c", "d"))

  result <- fct_relevel(x, c("d", "b"), after = 100L)

  expect_identical(levels(result), c("a", "c", "d", "b"))
})


test_that("fct_relevel preserves ordered factors", {
  x <- ordered(
    c("low", "medium", "high"),
    levels = c("medium", "low", "high")
  )

  result <- fct_relevel(x, c("low", "medium", "high"))

  expect_s3_class(result, "ordered")
  expect_s3_class(result, "factor")
  expect_true(is.ordered(result))
  expect_identical(levels(result), c("low", "medium", "high"))
  expect_identical(as.character(result), as.character(x))
})


test_that("fct_relevel preserves missing values", {
  x <- factor(
    c("a", NA, "b", "c"),
    levels = c("c", "b", "a")
  )

  result <- fct_relevel(x, c("a", "b"))

  expect_identical(levels(result), c("a", "b", "c"))
  expect_identical(is.na(result), is.na(x))
  expect_identical(as.character(result), as.character(x))
})


test_that("fct_relevel preserves names", {
  x <- factor(
    c(first = "a", second = "b", third = "c"),
    levels = c("c", "b", "a")
  )

  result <- fct_relevel(x, c("a", "b"))

  expect_identical(names(result), names(x))
  expect_identical(levels(result), c("a", "b", "c"))
})


test_that("fct_relevel errors when x is not a factor", {
  expect_error(
    fct_relevel(c("a", "b", "c"), c("a", "b")),
    "`x` must be a factor",
    fixed = TRUE
  )
})


test_that("fct_relevel errors when ref is not character or factor", {
  x <- factor(c("a", "b", "c"))

  expect_error(
    fct_relevel(x, 1:2),
    "`ref` must be a character vector",
    fixed = TRUE
  )
})


test_that("fct_relevel validates after", {
  x <- factor(c("a", "b", "c"))

  expect_error(
    fct_relevel(x, "a", after = NA),
    "`after` must be a single non-missing whole number.",
    fixed = TRUE
  )

  expect_error(
    fct_relevel(x, "a", after = c(0, 1)),
    "`after` must be a single non-missing whole number.",
    fixed = TRUE
  )

  expect_error(
    fct_relevel(x, "a", after = -1),
    "`after` must be greater than or equal to 0.",
    fixed = TRUE
  )

  expect_error(
    fct_relevel(x, "a", after = 1.5),
    "`after` must be a whole number.",
    fixed = TRUE
  )
})
