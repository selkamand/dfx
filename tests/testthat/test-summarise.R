# summarise_vector_by ----------

test_that("summarise_vector_by follows factor order and drops unused levels", {
  by <- factor(c("a", "b", "a"), levels = c("b", "a", "unused"))

  expect_identical(
    summarise_vector_by(x = c(10, 20, 30), by = by, fun = sum),
    c(b = 20, a = 40)
  )
})

test_that("summarise_vector_by fills unused levels without calling fun", {
  by <- factor(c("a", "b", "a"), levels = c("b", "a", "unused"))
  calls <- 0L
  summarise <- function(x) {
    calls <<- calls + 1L
    sum(x)
  }

  expect_identical(
    summarise_vector_by(c(10, 20, 30), by, summarise, drop = FALSE),
    c(b = 20, a = 40, unused = NA_real_)
  )
  expect_identical(calls, 2L)
  expect_identical(
    summarise_vector_by(c(10, 20, 30), by, sum, drop = FALSE, empty = 0),
    c(b = 20, a = 40, unused = 0)
  )
})

test_that("summarise_vector_by sorts character groups and omits missing keys", {
  expect_identical(
    summarise_vector_by(c(1, 2, 3, 4), c("z", "a", NA, "z"), sum),
    c(a = 2, z = 5)
  )
})

test_that("summarise_vector_by accepts numeric and logical groups", {
  expect_identical(
    summarise_vector_by(1:5, c(10, 2, 10, NA_real_, 1), sum),
    c(`1` = 5, `2` = 2, `10` = 4)
  )
  expect_identical(
    summarise_vector_by(1:4, c(TRUE, FALSE, TRUE, NA), sum),
    c(`FALSE` = 2, `TRUE` = 4)
  )
})

test_that("summarise_vector_by retains an observed explicit NA level", {
  by <- factor(c("a", NA, "b"), levels = c("a", "b", NA), exclude = NULL)

  expect_identical(
    summarise_vector_by(1:3, by, sum),
    setNames(c(1, 3, 2), c("a", "b", NA_character_))
  )
})

test_that("summarise_vector_by forwards additional arguments", {
  expect_identical(
    summarise_vector_by(c(1, NA, 3), c("a", "a", "b"), function(x) {
      mean(x, na.rm = TRUE)
    }),
    c(a = 1, b = 3)
  )
})

test_that("summarise_vector_by accepts a list result template", {
  by <- factor(c("a", "a"), levels = c("a", "unused"))

  expect_identical(
    summarise_vector_by(
      1:2,
      by,
      function(x) list(range(x)),
      template = list(NULL),
      drop = FALSE
    ),
    list(a = 1:2, unused = NULL)
  )
})

test_that("summarise_vector_by derives a missing value from a character template", {
  by <- factor("a", levels = c("a", "unused"))

  expect_identical(
    summarise_vector_by(
      "hello",
      by,
      paste,
      template = character(1),
      drop = FALSE
    ),
    c(a = "hello", unused = NA_character_)
  )
})

test_that("summarise_vector_by validates inputs and scalar results", {
  expect_error(summarise_vector_by(1:2, c("a", "b")), "`fun` must be supplied")
  expect_error(summarise_vector_by(1:2, list("a", "b"), sum), "`by` must be")
  expect_error(summarise_vector_by(1:2, "a", sum), "same length")
  expect_error(summarise_vector_by(1:2, c("a", "b"), sum, drop = NA), "`drop`")
  expect_error(
    summarise_vector_by(1:2, c("a", "b"), sum, template = numeric(2)),
    "`template`"
  )
  expect_error(
    summarise_vector_by(1:2, c("a", "a"), function(x) x),
    "values must be length 1"
  )
  expect_error(
    summarise_vector_by(1:2, c("a", "b"), function(x) "x"),
    "must be type 'double'"
  )
  expect_error(
    summarise_vector_by(
      1,
      factor("a", levels = c("a", "b")),
      sum,
      drop = FALSE,
      empty = c(0, 1)
    ),
    "values must be length 1"
  )
  expect_error(
    summarise_vector_by(
      1,
      factor("a", levels = c("a", "b")),
      sum,
      drop = FALSE,
      empty = "x"
    ),
    "must be type 'double'"
  )
  expect_error(
    summarise_vector_by(1, "a", sum, empty = "x"),
    "must be type 'double'"
  )
})

test_that("summarise_vector_by requires an explicit raw empty value", {
  by <- factor("a", levels = c("a", "unused"))
  fun <- function(x) as.raw(sum(x))

  expect_identical(
    summarise_vector_by(1, by, fun, template = raw(1)),
    structure(as.raw(1), names = "a")
  )
  expect_error(
    summarise_vector_by(1, by, fun, template = raw(1), drop = FALSE),
    "`empty` must be supplied"
  )
  expect_identical(
    summarise_vector_by(
      1,
      by,
      fun,
      template = raw(1),
      drop = FALSE,
      empty = as.raw(0)
    ),
    structure(as.raw(c(1, 0)), names = c("a", "unused"))
  )
})
