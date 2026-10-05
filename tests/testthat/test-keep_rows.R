make_keep_rows_data <- function() {
  data.frame(
    id = 1:4,
    label = c("a", NA_character_, "c", "d"),
    category = factor(
      c("low", "high", "low", "high"),
      levels = c("low", "high", "unused")
    ),
    row.names = c("row-a", "row-b", "row-c", "row-d")
  )
}

test_that("keep_rows selects TRUE rows in their original order", {
  data <- make_keep_rows_data()
  original <- data

  result <- keep_rows(data, c(TRUE, FALSE, TRUE, FALSE))

  expect_identical(result$id, c(1L, 3L))
  expect_identical(row.names(result), c("row-a", "row-c"))
  expect_identical(names(result), names(data))
  expect_identical(data, original)
})

test_that("keep_rows accepts all, no, and named selections", {
  data <- make_keep_rows_data()

  expect_identical(keep_rows(data, rep(TRUE, 4)), data)

  none <- keep_rows(data, rep(FALSE, 4))
  expect_s3_class(none, "data.frame")
  expect_identical(dim(none), c(0L, 3L))
  expect_identical(names(none), names(data))
  expect_identical(levels(none$category), levels(data$category))

  named_keep <- setNames(c(FALSE, TRUE, FALSE, TRUE), row.names(data))
  expect_identical(keep_rows(data, named_keep)$id, c(2L, 4L))
})

test_that("keep_rows retains data frame shape at small dimensions", {
  data <- make_keep_rows_data()

  one_column <- data["id"]
  one_column_result <- keep_rows(
    one_column,
    c(TRUE, FALSE, TRUE, FALSE)
  )
  expect_s3_class(one_column_result, "data.frame")
  expect_identical(dim(one_column_result), c(2L, 1L))
  expect_identical(one_column_result$id, c(1L, 3L))

  zero_rows <- data[FALSE, , drop = FALSE]
  expect_identical(keep_rows(zero_rows, logical()), zero_rows)

  zero_columns <- data[, FALSE, drop = FALSE]
  zero_column_result <- keep_rows(
    zero_columns,
    c(TRUE, FALSE, TRUE, FALSE)
  )
  expect_s3_class(zero_column_result, "data.frame")
  expect_identical(dim(zero_column_result), c(2L, 0L))
  expect_identical(row.names(zero_column_result), c("row-a", "row-c"))

  one_row <- data[1L, , drop = FALSE]
  expect_identical(keep_rows(one_row, TRUE), one_row)
  expect_identical(dim(keep_rows(one_row, FALSE)), c(0L, 3L))
})

test_that("keep_rows preserves column types and missing data values", {
  data <- make_keep_rows_data()
  data$day <- as.Date("2024-01-01") + 0:3
  data$payload <- list(1L, letters[1:2], NULL, list(key = "value"))

  result <- keep_rows(data, c(FALSE, TRUE, TRUE, FALSE))

  expect_identical(result$id, c(2L, 3L))
  expect_identical(result$label, c(NA_character_, "c"))
  expect_identical(result$category, data$category[2:3])
  expect_identical(levels(result$category), c("low", "high", "unused"))
  expect_identical(result$day, data$day[2:3])
  expect_identical(result$payload, data$payload[2:3])
})

test_that("keep_rows requires a data frame", {
  invalid_data <- list(NULL, 1:4, matrix(1:4, ncol = 1), list(id = 1:4))

  for (data in invalid_data) {
    expect_error(
      keep_rows(data, logical()),
      "`data` must be a data.frame",
      fixed = TRUE
    )
  }
})

test_that("keep_rows requires a logical vector without coercion", {
  data <- make_keep_rows_data()
  invalid_keep <- list(
    NULL,
    c(1L, 0L, 1L, 0L),
    c("TRUE", "FALSE", "TRUE", "FALSE"),
    list(TRUE, FALSE, TRUE, FALSE),
    matrix(c(TRUE, FALSE, TRUE, FALSE), ncol = 1L)
  )

  for (keep in invalid_keep) {
    expect_error(
      keep_rows(data, keep),
      "`keep` must be a logical vector",
      fixed = TRUE
    )
  }
})

test_that("keep_rows rejects every mask length other than the row count", {
  data <- make_keep_rows_data()
  invalid_keep <- list(logical(), TRUE, rep(TRUE, 3), rep(TRUE, 5))

  for (keep in invalid_keep) {
    expect_error(
      keep_rows(data, keep),
      "`keep` must have one value for each row in `data`",
      fixed = TRUE
    )
  }
})

test_that("keep_rows rejects missing mask values and reports their count", {
  data <- make_keep_rows_data()

  expect_error(
    keep_rows(data, c(TRUE, NA, FALSE, FALSE)),
    "`keep` contains 1 missing values",
    fixed = TRUE
  )
  expect_error(
    keep_rows(data, c(NA, TRUE, FALSE, NA)),
    "`keep` contains 2 missing values",
    fixed = TRUE
  )
})
