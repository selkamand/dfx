make_filter_data <- function() {
  data.frame(
    id = 1:4,
    value = c(10, NA_real_, 30, 40),
    row.names = c("row-a", "row-b", "row-c", "row-d")
  )
}

test_that("filter calls its predicate once with the whole data frame", {
  data <- make_filter_data()
  original <- data
  observed <- new.env(parent = emptyenv())
  observed$calls <- 0L

  result <- filter(data, function(d) {
    observed$calls <- observed$calls + 1L
    observed$data <- d
    d$id %in% c(1L, 3L)
  })

  expect_identical(observed$calls, 1L)
  expect_identical(observed$data, data)
  expect_identical(result$id, c(1L, 3L))
  expect_identical(row.names(result), c("row-a", "row-c"))
  expect_identical(data, original)
})

test_that("filter accepts all, no, and named predicate selections", {
  data <- make_filter_data()

  expect_identical(filter(data, function(d) rep(TRUE, nrow(d))), data)

  none <- filter(data, function(d) rep(FALSE, nrow(d)))
  expect_s3_class(none, "data.frame")
  expect_identical(dim(none), c(0L, 2L))
  expect_identical(names(none), names(data))

  named_result <- filter(data, function(d) {
    setNames(d$id %% 2L == 0L, row.names(d))
  })
  expect_identical(named_result$id, c(2L, 4L))
})

test_that("filter retains data frame shape at small dimensions", {
  data <- make_filter_data()

  one_column <- data["id"]
  one_column_result <- filter(one_column, function(d) d$id > 2L)
  expect_s3_class(one_column_result, "data.frame")
  expect_identical(dim(one_column_result), c(2L, 1L))
  expect_identical(one_column_result$id, c(3L, 4L))

  zero_rows <- data[FALSE, , drop = FALSE]
  expect_identical(
    filter(zero_rows, function(d) logical(nrow(d))),
    zero_rows
  )

  zero_columns <- data[, FALSE, drop = FALSE]
  zero_column_result <- filter(
    zero_columns,
    function(d) c(TRUE, FALSE, TRUE, FALSE)
  )
  expect_s3_class(zero_column_result, "data.frame")
  expect_identical(dim(zero_column_result), c(2L, 0L))
  expect_identical(row.names(zero_column_result), c("row-a", "row-c"))

  one_row <- data[1L, , drop = FALSE]
  expect_identical(filter(one_row, function(d) TRUE), one_row)
  expect_identical(
    dim(filter(one_row, function(d) FALSE)),
    c(0L, 2L)
  )
})

test_that("filter preserves column types and missing data values", {
  data <- make_filter_data()
  data$category <- factor(
    c("low", "high", "low", "high"),
    levels = c("low", "high", "unused")
  )
  data$day <- as.Date("2024-01-01") + 0:3
  data$payload <- list(1L, letters[1:2], NULL, list(key = "value"))

  result <- filter(data, function(d) d$id %in% c(2L, 3L))

  expect_identical(result$id, c(2L, 3L))
  expect_identical(result$value, c(NA_real_, 30))
  expect_identical(result$category, data$category[2:3])
  expect_identical(levels(result$category), c("low", "high", "unused"))
  expect_identical(result$day, data$day[2:3])
  expect_identical(result$payload, data$payload[2:3])
})

test_that("filter requires a data frame before evaluating its predicate", {
  invalid_data <- list(NULL, 1:4, matrix(1:4, ncol = 1), list(id = 1:4))

  for (data in invalid_data) {
    expect_error(
      filter(data, function(d) stop("predicate was called")),
      "`data` must be a data.frame",
      fixed = TRUE
    )
  }
})

test_that("filter requires a function", {
  data <- make_filter_data()

  for (fun in list(NULL, 1L, "predicate", list(TRUE))) {
    expect_error(
      filter(data, fun),
      "`fun` must be a function",
      fixed = TRUE
    )
  }
})

test_that("filter requires one explicit predicate parameter", {
  data <- make_filter_data()

  expect_error(
    filter(data, function() TRUE),
    "`fun` must have exactly one parameter",
    fixed = TRUE
  )
  expect_error(
    filter(data, function(d, other) TRUE),
    "`fun` must have exactly one parameter",
    fixed = TRUE
  )
  expect_error(
    filter(data, function(d, other = NULL) TRUE),
    "`fun` must have exactly one parameter",
    fixed = TRUE
  )
  expect_error(
    filter(data, function(d, ...) TRUE),
    "`fun` supports variable arguments",
    fixed = TRUE
  )
})

test_that("filter requires a logical vector result without coercion", {
  data <- make_filter_data()
  invalid_results <- list(
    NULL,
    1L,
    c("TRUE", "FALSE", "TRUE", "FALSE"),
    list(TRUE, FALSE, TRUE, FALSE),
    matrix(c(TRUE, FALSE, TRUE, FALSE), ncol = 1L)
  )

  for (value in invalid_results) {
    expect_error(
      filter(data, function(d) value),
      "`fun` must return a logical vector",
      fixed = TRUE
    )
  }
})

test_that("filter rejects every result length other than the row count", {
  data <- make_filter_data()
  invalid_results <- list(logical(), TRUE, rep(TRUE, 3), rep(TRUE, 5))

  for (value in invalid_results) {
    expect_error(
      filter(data, function(d) value),
      "`fun` must return a vector with one value for each row",
      fixed = TRUE
    )
  }
})

test_that("filter rejects missing predicate values and reports their count", {
  data <- make_filter_data()

  expect_error(
    filter(data, function(d) c(TRUE, NA, FALSE, FALSE)),
    "`fun` returned 1 missing values",
    fixed = TRUE
  )
  expect_error(
    filter(data, function(d) c(NA, TRUE, FALSE, NA)),
    "`fun` returned 2 missing values",
    fixed = TRUE
  )
})

test_that("filter propagates errors raised by the predicate", {
  data <- make_filter_data()

  expect_error(
    filter(data, function(d) stop("predicate failed")),
    "predicate failed",
    fixed = TRUE
  )
})

test_that("filter and keep_rows select the expected IDs for every short mask", {
  data <- data.frame(id = 1:3)
  masks <- expand.grid(rep(list(c(FALSE, TRUE)), nrow(data)))

  for (i in seq_len(nrow(masks))) {
    keep <- unname(as.logical(masks[i, ]))
    expected_ids <- data$id[which(keep)]

    expect_identical(keep_rows(data, keep)$id, expected_ids)
    expect_identical(filter(data, function(d) keep)$id, expected_ids)
  }
})
