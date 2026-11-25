test_that("distinct uses all columns by default and preserves row order", {
  data <- data.frame(
    id = c(2L, 1L, 2L, 2L),
    value = c("b", "a", "b", "c")
  )

  expected <- data.frame(
    id = c(2L, 1L, 2L),
    value = c("b", "a", "c")
  )

  expect_identical(distinct(data), expected)
})

test_that("distinct returns only the selected column by default", {
  data <- data.frame(
    id = c(2L, 1L, 2L),
    value = c("b", "a", "c")
  )

  expected <- data.frame(id = c(2L, 1L))

  expect_identical(distinct(data, columns = "id"), expected)
})

test_that("distinct keeps the first row for each key with keep_all = TRUE", {
  data <- data.frame(
    id = c(2L, 1L, 2L),
    value = c("b", "a", "c")
  )

  expected <- data.frame(
    id = c(2L, 1L),
    value = c("b", "a")
  )

  expect_identical(distinct(data, columns = "id", keep_all = TRUE), expected)
})

test_that("distinct deduplicates by the selected pair and returns columns in selection order", {
  data <- data.frame(
    id = c(2L, 2L, 1L, 2L),
    site = c("b", "a", "b", "b"),
    value = 11:14
  )

  # The first and last rows share a site and ID but differ in the unselected value.
  expected <- data.frame(
    site = c("b", "a", "b"),
    id = c(2L, 2L, 1L)
  )

  expect_identical(distinct(data, columns = c("site", "id")), expected)
})

test_that("distinct returns a single row unchanged", {
  data <- data.frame(id = 2L, value = "x")

  expect_identical(distinct(data), data)
})

test_that("distinct preserves the selected column type for zero rows", {
  data <- data.frame(id = integer(), group = factor(character(), levels = "a"))
  expected <- data.frame(group = factor(character(), levels = "a"))

  expect_identical(distinct(data, columns = "group"), expected)
})

test_that("distinct retains one row per missing key and distinguishes NA from NaN", {
  data <- data.frame(key = c(NA_real_, NaN, NA_real_, 1, NaN, 1), value = 1:6)
  expected <- data.frame(key = c(NA_real_, NaN, 1), value = c(1L, 2L, 4L))

  expect_identical(distinct(data, columns = "key", keep_all = TRUE), expected)
})

test_that("distinct preserves dates and unused factor levels", {
  data <- data.frame(
    day = as.Date(c("2024-01-02", "2024-01-01", "2024-01-02")),
    group = factor(c("b", "a", "b"), levels = c("a", "b", "c"))
  )
  expected <- data[1:2, , drop = FALSE]

  expect_identical(distinct(data), expected)
})

test_that("distinct matches list column keys by their contents", {
  data <- data.frame(
    id = 1:4,
    key = I(list(c(1L, 2L), 1L, c(1L, 2L), integer()))
  )
  expected <- data.frame(key = I(list(c(1L, 2L), 1L, integer())))

  expect_identical(distinct(data, columns = "key"), expected)
})

test_that("distinct keeps input column order with multiple keys and keep_all = TRUE", {
  data <- data.frame(
    id = c(2L, 2L, 1L, 2L),
    site = c("b", "a", "b", "b"),
    value = 11:14
  )
  expected <- data[1:3, , drop = FALSE]

  expect_identical(
    distinct(data, columns = c("site", "id"), keep_all = TRUE),
    expected
  )
})

test_that("distinct rejects inputs that are not data frames", {
  expect_snapshot(error = TRUE, cran = TRUE, {
    distinct(1:3)
  })
})

test_that("distinct rejects an unknown column", {
  data <- data.frame(id = 1L)

  expect_snapshot(error = TRUE, cran = TRUE, {
    distinct(data, columns = "unknown")
  })
})

test_that("distinct preserves custom row names on retained rows", {
  data <- data.frame(
    id = c(2L, 2L, 1L),
    value = c("b", "b", "a"),
    row.names = c("first", "duplicate", "last")
  )
  expected <- data[c(1L, 3L), , drop = FALSE]

  expect_identical(distinct(data), expected)
})

test_that("distinct keeps the first row of a zero-column data frame", {
  data <- data.frame(row.names = c("first", "second", "third"))
  expected <- data[1L, , drop = FALSE]

  expect_identical(distinct(data), expected)
  expect_identical(distinct(data, keep_all = TRUE), expected)
})

test_that("distinct rejects an empty column selection", {
  data <- data.frame(id = c(2L, 2L, 1L))

  expect_snapshot(error = TRUE, cran = TRUE, {
    distinct(data, columns = character(0))
  })
})

test_that("distinct rejects duplicate input column names", {
  data <- setNames(data.frame(a = 1L, b = 10L), c("id", "id"))

  expect_snapshot(error = TRUE, cran = TRUE, {
    distinct(data)
  })
})

test_that("distinct rejects duplicate entries in columns", {
  data <- data.frame(id = c(2L, 2L, 1L))

  expect_snapshot(error = TRUE, cran = TRUE, {
    distinct(data, columns = c("id", "id"))
  })
})

test_that("distinct warns about ignoring dplyr groups", {
  data <- structure(
    data.frame(team = c("a", "b"), id = c(1L, 1L)),
    class = c("grouped_df", "data.frame")
  )

  expect_warning(
    result <- distinct(data, columns = "id"),
    paste0(
      "dfx ignores grouping added by `dplyr::group_by()`. ",
      "For functions that support grouping, use the `by` argument."
    ),
    fixed = TRUE
  )
  expect_identical(names(result), "id")
  expect_identical(result$id, 1L)
})

test_that("distinct compares whole matrix rows and preserves the matrix column", {
  data <- data.frame(
    key = I(cbind(c(1L, 1L, 1L, 2L), c(3L, 3L, 4L, 3L)))
  )
  expected <- data[c(1L, 3L, 4L), , drop = FALSE]
  rownames(expected) <- NULL

  expect_identical(distinct(data), expected)
})

test_that("distinct compares a matrix key and an ordinary key together", {
  key <- rbind(first = c(1L, 3L), duplicate = c(1L, 3L), other = c(1L, 3L))
  data <- data.frame(group = c("a", "a", "b"), key = I(key))
  expected <- data[c(1L, 3L), , drop = FALSE]

  expect_identical(distinct(data, columns = c("group", "key")), expected)
})

test_that("distinct warns about ignoring dplyr row-wise evaluation", {
  data <- structure(
    data.frame(team = c("a", "b"), id = c(1L, 1L)),
    class = c("rowwise_df", "data.frame")
  )

  expect_warning(
    result <- distinct(data, columns = "id"),
    "^dfx ignores the row-wise evaluation mode set by `dplyr::rowwise\\(\\)`$"
  )
  expect_identical(names(result), "id")
  expect_identical(result$id, 1L)
})

test_that("distinct rejects a numeric keep_all flag", {
  data <- data.frame(id = 1L)

  expect_snapshot(error = TRUE, cran = TRUE, {
    distinct(data, keep_all = 1)
  })
})
