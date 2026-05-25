test_that("n_distinct() counts empty inputs", {
  expect_equal(n_distinct(NULL), 0)
  expect_equal(n_distinct(data.frame()), 0)
})

test_that("n_distinct() counts unique values in simple vectors", {
  expect_equal(n_distinct(c(TRUE, FALSE, NA)), 3)
  expect_equal(n_distinct(c(1, 2, NA)), 3)
  expect_equal(n_distinct(c(1L, 2L, NA)), 3)
  expect_equal(n_distinct(c("x", "y", NA)), 3)
})

test_that("n_distinct() counts unique combinations (in data.frames)", {
  expect_equal(n_distinct(data.frame(c(1, 1, 1), c(2, 2, 2))), 1)
  expect_equal(n_distinct(data.frame(c(1, 1, 2), c(1, 2, 2))), 3)
})

test_that("n_distinct() counts unique combinations in matrices", {
  expect_equal(n_distinct(matrix(c(1, 2, 1, 2), ncol = 2)), 2)
  expect_equal(n_distinct(matrix(c(1, 1, 2, 1, 2, 2), ncol = 2)), 3)
  expect_equal(n_distinct(matrix(c(1, 1, 1, 1), ncol = 2)), 1)
})


test_that("n_distinct() can drop missing values", {
  expect_equal(n_distinct(NA, na.rm = TRUE), 0)
  expect_equal(n_distinct(c(NA, 0), na.rm = TRUE), 1)

  expect_equal(n_distinct(data.frame(c(NA, 0), c(0, NA)), na.rm = TRUE), 0)
  expect_equal(n_distinct(data.frame(c(NA, 0), c(0, 0)), na.rm = TRUE), 1)

  expect_equal(n_distinct(matrix(c(NA, 0, 0, NA), ncol = 2), na.rm = TRUE), 0)
  expect_equal(n_distinct(matrix(c(NA, 0, 0, 0), ncol = 2), na.rm = TRUE), 1)

})

test_that("n_distinct() counts unique values in vector-like S3 vectors", {
  expect_equal(
    n_distinct(as.Date(c("2024-01-01", "2024-01-02", NA))),
    3
  )

  expect_equal(
    n_distinct(as.POSIXct(c("2024-01-01 10:00:00", "2024-01-01 11:00:00", NA))),
    3
  )
})

test_that("n_distinct() counts unique values in factors", {
  expect_equal(n_distinct(factor(c("x", "y", "x"))), 2)
  expect_equal(n_distinct(factor(c("x", "y", NA))), 3)
})

test_that("n_distinct() can drop missing values from factors", {
  expect_equal(n_distinct(factor(c("x", "y", NA)), na.rm = TRUE), 2)
  expect_equal(n_distinct(factor(c(NA, NA)), na.rm = TRUE), 0)
})

test_that("n_distinct() counts unique values in lists", {
  expect_equal(n_distinct(list("x", "y", "x")), 2)
  expect_equal(n_distinct(list(1, 2, 1)), 2)
  expect_equal(n_distinct(list(NULL, "x", NULL)), 2)
})

test_that("n_distinct() counts unique nested list values", {
  expect_equal(
    n_distinct(list(
      list(a = 1),
      list(a = 2),
      list(a = 1)
    )),
    2
  )

  expect_equal(
    n_distinct(list(
      data.frame(x = 1),
      data.frame(x = 2),
      data.frame(x = 1)
    )),
    2
  )
})

test_that("n_distinct() generates useful errors", {
  expect_snapshot(error = TRUE, {
    n_distinct()
    n_distinct(mean)
  })
})
